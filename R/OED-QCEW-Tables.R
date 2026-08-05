# Quarterly Census of Employment and Wages (QCEW) download and parsing.
#
# The loader uses bundled QualityInfo geography/year choices to construct QCEW
# report-portlet requests, then preserves suppression markers and period
# metadata while standardizing the returned workbook rows. Dynamic service
# helpers remain only as maintenance diagnostics.

qcew_or <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }

  x
}

qcew_clean_names <- function(x) {
  names <- x |>
    as.character() |>
    stringr::str_squish() |>
    stringr::str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")

  blank_names <- !nzchar(names) | is.na(names)
  names[blank_names] <- paste0("blank_", seq_len(sum(blank_names)))
  make.unique(names, sep = "_")
}

qcew_cell <- function(mat, row, col) {
  if (nrow(mat) < row || ncol(mat) < col) {
    return(NA_character_)
  }

  mat[row, col]
}

qcew_is_xlsx <- function(path) {
  oed_xlsx_valid(path)
}

qcew_parse_period <- function(period_line) {
  period_line <- stringr::str_squish(as.character(period_line))
  period_match <- stringr::str_match(
    period_line,
    "^(.*)\\.\\s*([0-9]{4})\\s*(Annual|Q[1-4]|[1-4](?:st|nd|rd|th)\\s+Quarter|Quarter\\s*[1-4])\\s*$"
  )

  if (is.na(period_match[1, 1])) {
    return(tibble::tibble(
      geography = period_line,
      year = NA_integer_,
      quarter = NA_integer_,
      period_type = NA_character_,
      period = NA_character_
    ))
  }

  year <- as.integer(period_match[1, 3])
  period_text <- period_match[1, 4]
  is_annual <- str_detect(stringr::str_to_lower(period_text), "annual")
  quarter <- if (is_annual) NA_integer_ else as.integer(stringr::str_extract(period_text, "[1-4]"))

  tibble::tibble(
    geography = stringr::str_squish(period_match[1, 2]),
    year = year,
    quarter = quarter,
    period_type = if (is_annual) "Annual" else "Quarterly",
    period = if (is_annual) as.character(year) else paste0(year, "Q", quarter)
  )
}

qcew_find_header_row <- function(raw_mat) {
  header_rows <- apply(raw_mat, 1, function(row) {
    row <- stringr::str_to_lower(stringr::str_squish(row))
    any(row == "naics", na.rm = TRUE) && (
      any(row == "industry", na.rm = TRUE) ||
        all(c("units", "employment") %in% row)
    )
  })

  match(TRUE, header_rows)
}

qcew_parse_number <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x <- str_replace_all(x, "[,$]", "")
  x[x %in% c("", "-", "(c)", "(C)")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

qcew_period_date <- function(year, quarter) {
  quarter <- suppressWarnings(as.integer(quarter))
  month <- ifelse(is.na(quarter), 1L, (quarter - 1L) * 3L + 1L)
  as.Date(sprintf("%04d-%02d-01", as.integer(year), month))
}

qcew_normalize_naics <- function(x) {
  str_replace_all(stringr::str_squish(as.character(x)), "\\s+", "")
}

qcew_collapse_unique <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]

  if (length(x) == 0) {
    return(NA_character_)
  }

  paste(sort(unique(x)), collapse = ", ")
}

qcew_filter_naics <- function(data, NAICS = NULL, NAICSMatch = c("exact", "prefix")) {
  NAICSMatch <- match.arg(NAICSMatch)

  if (is.null(NAICS) || length(NAICS) == 0 || !"naics" %in% names(data)) {
    return(data)
  }

  naics_keys <- qcew_normalize_naics(NAICS)

  keep <- vapply(data$naics, function(code) {
    parts <- unlist(stringr::str_split(qcew_normalize_naics(code), ","))

    if (NAICSMatch == "exact") {
      any(parts %in% naics_keys)
    } else {
      any(vapply(naics_keys, function(key) {
        any(stringr::str_starts(parts, key))
      }, logical(1)))
    }
  }, logical(1))

  data[keep, , drop = FALSE]
}

qcew_naics_found <- function(data, naics_key, NAICSMatch = c("exact", "prefix")) {
  NAICSMatch <- match.arg(NAICSMatch)

  if (nrow(data) == 0 || !"naics" %in% names(data)) {
    return(rep(FALSE, nrow(data)))
  }

  naics_key <- qcew_normalize_naics(naics_key)

  vapply(data$naics, function(code) {
    parts <- unlist(stringr::str_split(qcew_normalize_naics(code), ","))

    if (NAICSMatch == "exact") {
      any(parts == naics_key)
    } else {
      any(stringr::str_starts(parts, naics_key))
    }
  }, logical(1))
}

qcew_naics_availability <- function(data, request_plan, NAICS = NULL, NAICSMatch = c("exact", "prefix")) {
  NAICSMatch <- match.arg(NAICSMatch)

  if (is.null(NAICS) || length(NAICS) == 0) {
    return(tibble::tibble())
  }

  expected_grid <- request_plan |>
    dplyr::distinct(
      .data$geography,
      .data$qcew_area,
      .data$year,
      .data$qcew_period_label
    ) |>
    tidyr::crossing(requested_naics = qcew_normalize_naics(NAICS))

  rows_found <- integer(nrow(expected_grid))
  ownerships_found <- character(nrow(expected_grid))

  for (i in seq_len(nrow(expected_grid))) {
    geo_data <- data |>
      dplyr::filter(
        .data$geography == expected_grid$geography[i],
        .data$year == expected_grid$year[i],
        .data$qcew_period_label == expected_grid$qcew_period_label[i]
      )

    matched <- qcew_naics_found(
      geo_data,
      expected_grid$requested_naics[i],
      NAICSMatch = NAICSMatch
    )
    matched_data <- geo_data[matched, , drop = FALSE]
    rows_found[i] <- sum(matched)
    ownerships_found[i] <- if ("ownership" %in% names(matched_data)) {
      qcew_collapse_unique(matched_data$ownership)
    } else {
      NA_character_
    }
  }

  expected_grid <- expected_grid |>
    dplyr::mutate(
      rows_found = rows_found,
      ownerships_found = ownerships_found,
      found_period = .data$rows_found > 0
    )

  expected_grid |>
    dplyr::group_by(.data$geography, .data$qcew_area, .data$requested_naics) |>
    dplyr::summarise(
      rows_found = sum(.data$rows_found),
      period_years_expected = dplyr::n(),
      period_years_found = sum(.data$found_period),
      period_years_missing = dplyr::n() - sum(.data$found_period),
      years_found = qcew_collapse_unique(.data$year[.data$found_period]),
      years_missing = qcew_collapse_unique(.data$year[!.data$found_period]),
      periods_found = qcew_collapse_unique(.data$qcew_period_label[.data$found_period]),
      periods_missing = qcew_collapse_unique(.data$qcew_period_label[!.data$found_period]),
      ownerships_found = qcew_collapse_unique(.data$ownerships_found),
      found = sum(.data$found_period) > 0,
      complete = dplyr::n() == sum(.data$found_period),
      status = dplyr::case_when(
        dplyr::n() == sum(.data$found_period) ~ "found",
        sum(.data$found_period) > 0 ~ "partially found",
        TRUE ~ "not found"
      ),
      .groups = "drop"
    ) |>
    arrange(.data$geography, .data$requested_naics)
}

qcew_message_naics_availability <- function(availability) {
  if (nrow(availability) == 0) {
    return(invisible(NULL))
  }

  if (nrow(availability) > 50) {
    message(
      "NAICS availability: ",
      sum(availability$found),
      " of ",
      nrow(availability),
      " geography/NAICS combinations found. ",
      "Use attr(result, \"naics_availability\") for details."
    )

    missing <- availability |>
      filter(!.data$found)

    if (nrow(missing) > 0) {
      message("First missing combinations:")
      lines <- missing |>
        dplyr::slice_head(n = 50) |>
        mutate(line = paste0(.data$geography, " ", .data$requested_naics, " ", .data$status)) |>
        dplyr::pull(.data$line)
      invisible(lapply(lines, message))
    }

    return(invisible(NULL))
  }

  message("NAICS availability:")

  lines <- availability |>
    mutate(line = paste0(.data$geography, " ", .data$requested_naics, " ", .data$status)) |>
    dplyr::pull(.data$line)
  invisible(lapply(lines, message))

  invisible(NULL)
}

qcew_add_industry_hierarchy <- function(data) {
  if (!"industry" %in% names(data)) {
    return(data)
  }

  industry_raw <- as.character(data$industry)
  industry <- str_trim(industry_raw, side = "left")
  indent_spaces <- nchar(industry_raw) - nchar(industry)
  parent_industry <- character(length(industry))
  stack_levels <- integer(0)
  stack_labels <- character(0)

  for (row_id in seq_along(industry)) {
    if (is.na(industry[row_id]) || !nzchar(industry[row_id])) {
      parent_industry[row_id] <- NA_character_
      next
    }

    this_level <- indent_spaces[row_id]

    while (length(stack_levels) > 0 && dplyr::last(stack_levels) >= this_level) {
      stack_levels <- utils::head(stack_levels, -1)
      stack_labels <- utils::head(stack_labels, -1)
    }

    parent_industry[row_id] <- if (length(stack_labels) == 0) {
      NA_character_
    } else {
      dplyr::last(stack_labels)
    }

    stack_levels <- c(stack_levels, this_level)
    stack_labels <- c(stack_labels, industry[row_id])
  }

  data |>
    mutate(
      industry_raw = industry_raw,
      industry = industry,
      indent_spaces = indent_spaces,
      parent_industry = parent_industry
    ) |>
    dplyr::relocate(industry_raw, indent_spaces, parent_industry, .after = industry)
}

qcew_read_sheet <- function(path, sheet, keep_raw_values = FALSE) {
  raw <- suppressMessages(read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    .name_repair = "minimal",
    trim_ws = FALSE
  ))

  raw_mat <- as.matrix(raw)
  storage.mode(raw_mat) <- "character"

  header_row <- qcew_find_header_row(raw_mat)
  if (is.na(header_row)) {
    stop(
      "Could not find a QCEW header row in ", path, " / ", sheet, ". ",
      "The QualityInfo workbook layout may have changed; update OEDloadR.",
      call. = FALSE
    )
  }

  header <- raw_mat[header_row, ]
  last_col <- max(which(!is.na(header) & nzchar(stringr::str_squish(header))))
  header <- header[seq_len(last_col)]

  data_mat <- if (header_row >= nrow(raw_mat)) {
    matrix(character(), nrow = 0, ncol = last_col)
  } else {
    raw_mat[
      seq.int(header_row + 1, nrow(raw_mat)),
      seq_len(last_col),
      drop = FALSE
    ]
  }
  data <- tibble::as_tibble(data_mat, .name_repair = "minimal")
  names(data) <- qcew_clean_names(header)

  data <- data |>
    mutate(across(dplyr::everything(), as.character)) |>
    filter(!dplyr::if_all(dplyr::everything(), ~ is.na(.x) | !nzchar(stringr::str_squish(.x))))

  if ("naics" %in% names(data)) {
    data <- data |>
      filter(stringr::str_to_lower(stringr::str_squish(.data$naics)) != "naics")
  }

  period <- qcew_parse_period(qcew_cell(raw_mat, 2, 1))
  downloaded <- stringr::str_remove(qcew_or(qcew_cell(raw_mat, 3, 1), NA_character_), "^Downloaded:\\s*")
  source <- stringr::str_remove(qcew_or(qcew_cell(raw_mat, 4, 1), NA_character_), "^Source:\\s*")

  data <- data |>
    mutate(
      workbook = basename(path),
      path = path,
      sheet = sheet,
      title = qcew_cell(raw_mat, 1, 1),
      downloaded = downloaded,
      source = source,
      geography = period$geography,
      year = period$year,
      quarter = period$quarter,
      period_type = period$period_type,
      period = period$period,
      .before = 1
    ) |>
    qcew_add_industry_hierarchy()

  metric_cols <- intersect(
    c(
      "units",
      "employment",
      "wages",
      "annual_average_wage",
      "quarterly_average_wage",
      "average_weekly_wage",
      "weeks_in_a_q"
    ),
    names(data)
  )

  for (metric_col in metric_cols) {
    raw_values <- data[[metric_col]]

    if (isTRUE(keep_raw_values)) {
      data[[paste0(metric_col, "_raw")]] <- raw_values
    }

    data[[paste0(metric_col, "_suppressed")]] <- str_detect(
      raw_values,
      stringr::regex("^\\s*\\(c\\)\\s*$", ignore_case = TRUE)
    )
    data[[metric_col]] <- qcew_parse_number(raw_values)
  }

  data
}

qcew_read_workbooks <- function(Paths, Sheets = NULL, keep_raw_values = FALSE) {
  workbook_results <- lapply(Paths, function(path) {
    if (!file.exists(path)) {
      stop("File not found: ", path, call. = FALSE)
    }

    if (!qcew_is_xlsx(path)) {
      stop(
        "File is not an xlsx workbook: ", path,
        ". QualityInfo may have returned an HTML/error page instead of Excel.",
        call. = FALSE
      )
    }

    workbook_sheets <- excel_sheets(path)
    qcew_sheets <- workbook_sheets[str_detect(workbook_sheets, stringr::regex("^QCEW", ignore_case = TRUE))]
    sheets <- qcew_or(Sheets, qcew_or(qcew_sheets, workbook_sheets[1]))

    sheet_results <- lapply(sheets, function(sheet) {
      qcew_read_sheet(path, sheet, keep_raw_values = keep_raw_values)
    })

    bind_rows(sheet_results)
  })

  bind_rows(workbook_results)
}

qcew_period_lookup <- tibble::tibble(
  period_input = c("Annual", "00", "Q1", "1", "01", "Q2", "2", "02", "Q3", "3", "03", "Q4", "4", "04"),
  qcew_period_code = c("00", "00", "01", "01", "01", "02", "02", "02", "03", "03", "03", "04", "04", "04"),
  qcew_period_label = c(
    "Annual", "Annual",
    "1st Quarter", "1st Quarter", "1st Quarter",
    "2nd Quarter", "2nd Quarter", "2nd Quarter",
    "3rd Quarter", "3rd Quarter", "3rd Quarter",
    "4th Quarter", "4th Quarter", "4th Quarter"
  )
)

qcew_ownership_lookup <- tibble::tibble(
  ownership_input = c("All Ownerships", "All", "00", "Private", "50", "All Government", "90", "Federal Government", "10", "State Government", "20", "Local Government", "30"),
  qcew_ownership = c("00", "00", "00", "50", "50", "90", "90", "10", "10", "20", "20", "30", "30"),
  qcew_ownership_label = c(
    "All Ownerships", "All Ownerships", "All Ownerships",
    "Private", "Private",
    "All Government", "All Government",
    "Federal Government", "Federal Government",
    "State Government", "State Government",
    "Local Government", "Local Government"
  )
)

qcew_report_type_lookup <- tibble::tibble(
  report_input = c("Summary", "0", "All Industries", "1", "Detailed Industry", "Detailed", "2"),
  qcew_report_type = c("0", "0", "1", "1", "2", "2", "2"),
  qcew_report_type_label = c("Summary", "Summary", "All Industries", "All Industries", "Detailed Industry", "Detailed Industry", "Detailed Industry")
)

qcew_industry_level_lookup <- tibble::tibble(
  level_input = c("Total, all industries", "Total", "0", "Supersector", "1", "Sector", "2", "Subsector", "3", "Industry", "5"),
  qcew_industry_level = c("0", "0", "0", "1", "1", "2", "2", "3", "3", "5", "5")
)

qcew_match_lookup <- function(value, lookup, input_col, value_col, label_col = NULL, arg_name = "value") {
  value <- as.character(value)[1]
  hit <- lookup |>
    filter(stringr::str_to_lower(.data[[input_col]]) == stringr::str_to_lower(value))

  if (nrow(hit) == 0) {
    stop("No matching ", arg_name, ": ", value, call. = FALSE)
  }

  out <- hit[[value_col]][1]
  label <- if (is.null(label_col)) out else hit[[label_col]][1]
  list(value = out, label = label)
}

qcew_match_lookup_vec <- function(values, lookup, input_col, value_col, label_col = NULL, arg_name = "value") {
  lapply(values, function(value) {
    qcew_match_lookup(
      value = value,
      lookup = lookup,
      input_col = input_col,
      value_col = value_col,
      label_col = label_col,
      arg_name = arg_name
    )
  })
}

qcew_get_years <- function() {
  body <- request("https://www.qualityinfo.org/lmiservice/service/industry/qcewyears") |>
    req_url_query(indcodty = 10) |>
    req_user_agent("Mozilla/5.0") |>
    oed_request_perform() |>
    httr2::resp_body_string()

  years <- stringr::str_match_all(
    body,
    "<(?:[^:<>]+:)?periodyear>([0-9]{4})</(?:[^:<>]+:)?periodyear>"
  )[[1]][, 2]

  as.integer(years)
}

qcew_get_latest_year <- function(Periods = "Annual") {
  years <- qcew_get_years()
  if (length(years) == 0) {
    stop("QualityInfo returned no QCEW years.", call. = FALSE)
  }

  period_keys <- stringr::str_to_lower(as.character(Periods))
  annual_requested <- any(period_keys %in% c("annual", "00", "all"))

  if (annual_requested) {
    prior_years <- years[years < as.integer(format(Sys.Date(), "%Y"))]
    if (length(prior_years) > 0) {
      return(max(prior_years))
    }
  }

  max(years)
}

qcew_resolve_years <- function(Years, Periods = "Annual") {
  if (is.null(Years)) {
    annual_requested <- any(
      stringr::str_to_lower(as.character(Periods)) %in% c("annual", "00", "all")
    )
    if (annual_requested) {
      prior_years <- oed_static_qcew_years()[
        oed_static_qcew_years() < as.integer(format(Sys.Date(), "%Y"))
      ]
      if (length(prior_years) > 0) return(max(prior_years))
    }
    return(oed_static_qcew_latest_year())
  }

  if (length(Years) == 1 && stringr::str_to_lower(as.character(Years)) %in% c("all", "history", "timeseries", "time_series")) {
    return(oed_static_qcew_years())
  }

  years <- suppressWarnings(as.integer(Years))

  if (any(is.na(years))) {
    stop("Years must be numeric years or \"all\".", call. = FALSE)
  }

  sort(unique(years), decreasing = TRUE)
}

qcew_resolve_periods <- function(Periods) {
  if (is.null(Periods) || length(Periods) == 0) {
    stop(
      "Periods must include Annual, Q1, Q2, Q3, Q4, quarterly, or all.",
      call. = FALSE
    )
  }

  if (length(Periods) == 1 && stringr::str_to_lower(as.character(Periods)) == "all") {
    Periods <- c("Annual", "Q1", "Q2", "Q3", "Q4")
  }

  if (length(Periods) == 1 && stringr::str_to_lower(as.character(Periods)) %in% c("quarterly", "quarters")) {
    Periods <- c("Q1", "Q2", "Q3", "Q4")
  }

  qcew_match_lookup_vec(
    values = Periods,
    lookup = qcew_period_lookup,
    input_col = "period_input",
    value_col = "qcew_period_code",
    label_col = "qcew_period_label",
    arg_name = "Period"
  )
}

qcew_build_request_plan <- function(geo_selected, years, periods) {
  request_rows <- list()

  for (geo_id in seq_len(nrow(geo_selected))) {
    for (year in years) {
      for (period in periods) {
        request_rows[[length(request_rows) + 1]] <- tibble::tibble(
          geo_id = geo_id,
          geography = geo_selected$geography[geo_id],
          qcew_area = geo_selected$qcew_area[geo_id],
          year = year,
          qcew_period_code = period$value,
          qcew_period_label = period$label
        )
      }
    }
  }

  bind_rows(request_rows)
}

qcew_get_geography_lookup <- function() {
  body <- request("https://www.qualityinfo.org/lmiservice/service/geog/areas/41/qcew") |>
    req_url_query(includestatewide = "y") |>
    req_user_agent("Mozilla/5.0") |>
    oed_request_perform() |>
    httr2::resp_body_string()

  json <- jsonlite::fromJSON(body, flatten = TRUE)

  tibble::as_tibble(json$Geog) |>
    dplyr::transmute(
      geo_type = dplyr::case_when(
        .data$areatype == "01" ~ "State",
        .data$areatype == "04" ~ "County",
        .data$areatype %in% c("02", "21", "56") ~ "MSA",
        TRUE ~ paste0("Area type ", .data$areatype)
      ),
      area_type = .data$areatype,
      geo_code = .data$area,
      qcew_area = paste0(.data$stfips, .data$areatype, .data$area),
      geography = .data$areaname,
      geography_short = stringr::str_remove(.data$areaname, "\\s+County$")
    )
}

qcew_select_geographies <- function(Geographies) {
  geography_lookup <- oed_static_qcew_geographies()

  if (is.null(Geographies)) {
    return(geography_lookup)
  }

  geography_keys <- stringr::str_to_lower(stringr::str_squish(as.character(Geographies)))

  selected <- geography_lookup |>
    filter(
      stringr::str_to_lower(.data$geography) %in% geography_keys |
        stringr::str_to_lower(.data$geography_short) %in% geography_keys |
        stringr::str_to_lower(.data$qcew_area) %in% geography_keys
    )

  if (nrow(selected) == 0) {
    stop(
      "No matching QCEW geographies found. Use OED_QCEW_Options()$geographies " ,
      "to inspect valid names and area codes.",
      call. = FALSE
    )
  }

  selected
}

# Return the bundled QualityInfo choices used by OED_QCEW_Table(). This helper
# is deliberately offline; the values are updated when the package's
# QualityInfo contract is updated.
OED_QCEW_Options <- function() {
  list(
    geographies = oed_static_qcew_geographies(),
    years = oed_static_qcew_years(),
    periods = qcew_period_lookup |>
      dplyr::distinct(.data$qcew_period_code, .data$qcew_period_label),
    ownerships = qcew_ownership_lookup |>
      dplyr::distinct(.data$qcew_ownership, .data$qcew_ownership_label),
    report_types = qcew_report_type_lookup |>
      dplyr::distinct(.data$qcew_report_type, .data$qcew_report_type_label),
    industry_levels = qcew_industry_level_lookup |>
      dplyr::distinct(.data$qcew_industry_level)
  )
}

qcew_destination_path <- function(geography_row,
                                   year,
                                   period,
                                   ownership,
                                   report_type,
                                   industry_level,
                                   industry_supersector,
                                   industry_sector,
                                   industry,
                                   DownloadDir) {
  title <- paste(
    "QCEW",
    geography_row$geography,
    year,
    period$label,
    paste0("ownership", ownership$value),
    paste0("report", report_type$value),
    paste0("level", industry_level$value),
    paste0("supersector", industry_supersector),
    paste0("sector", industry_sector),
    paste0("industry", industry),
    sep = "_"
  )

  data_destination_path(title, DownloadDir)
}

qcew_download_one <- function(geography_row,
                              year,
                              period,
                              ownership,
                              report_type,
                              industry_level,
                              industry_supersector,
                              industry_sector,
                              industry,
                              destination_path,
                              Overwrite = FALSE,
                              Refresh = "auto",
                              MaxAge = NULL,
                              RequestParameters = NULL,
                              LatestAvailablePeriod = NULL) {
  dir.create(dirname(destination_path), recursive = TRUE, showWarnings = FALSE)

  req <- request("https://www.qualityinfo.org/ewind") |>
    req_url_query(
      p_p_id = "QiDatatoolQcew_INSTANCE_1dJUOCoo6aWa",
      p_p_lifecycle = 2,
      p_p_state = "normal",
      p_p_mode = "view",
      p_p_resource_id = "getReportXlsx",
      p_p_cacheability = "cacheLevelPage",
      rt = report_type$value,
      qcewArea = geography_row$qcew_area,
      qcewOwnership = ownership$value,
      qcewIndustryLvl = industry_level$value,
      qcewIndustrySuperSector = industry_supersector,
      qcewIndustrySector = industry_sector,
      qcewIndustry = industry,
      qcewAreaDesc = geography_row$geography,
      qcewIndustrySectorDesc = if (industry_sector == "00") "All" else industry_sector,
      qcewOwnershipDesc = ownership$label,
      qcewIndustrySuperSectorDesc = if (industry_supersector == "0000") "All" else industry_supersector,
      qcewIndustryDesc = if (industry == "0000") "Total, All Industries" else industry,
      qcewPeriodyear = year,
      qcewPeriod = period$value
    ) |>
    req_headers(
      Referer = "https://www.qualityinfo.org/ewind",
      Accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet,*/*"
    ) |>
    req_user_agent("Mozilla/5.0")

  oed_cache_download(
    request = req,
    destination_path = destination_path,
    command = "OED_QCEW_Table",
    dataset = "QCEW",
    source_url = as.character(req$url),
    request_parameters = RequestParameters,
    Refresh = Refresh,
    Overwrite = Overwrite,
    MaxAge = MaxAge,
    latest_available_period = LatestAvailablePeriod,
    validator = qcew_is_xlsx,
    DataUrl = "https://www.qualityinfo.org/ewind"
  )
}

OED_QCEW_Table <- function(Year = NULL,
                           Period = "Annual",
                           Years = Year,
                           Periods = Period,
                           Geographies = "Oregon",
                           NAICS = NULL,
                           NAICSMatch = c("exact", "prefix"),
                           Ownership = "All Ownerships",
                           ReportType = "All Industries",
                           IndustryLevel = "Total, all industries",
                           IndustrySuperSector = "0000",
                           IndustrySector = "00",
                           Industry = "0000",
                           DownloadDir = NULL,
                           Overwrite = FALSE,
                           MaxRequests = 100,
                           PreviewOnly = FALSE,
                           ReportNAICSAvailability = !is.null(NAICS),
                           KeepDownloadMetadata = FALSE,
                           Paths = NULL,
                           Sheets = NULL,
                           keep_raw_values = FALSE,
                           Refresh = c("auto", "always", "never"),
                           MaxAge = NULL) {
  NAICSMatch <- match.arg(NAICSMatch)
  Refresh <- oed_refresh_policy(Refresh, Overwrite = Overwrite)
  DownloadDir <- oed_dataset_download_dir("QCEW", DownloadDir)

  if (!is.null(Paths)) {
    return(
      qcew_read_workbooks(Paths, Sheets = Sheets, keep_raw_values = keep_raw_values) |>
        qcew_filter_naics(NAICS = NAICS, NAICSMatch = NAICSMatch)
    )
  }

  years <- qcew_resolve_years(Years, Periods = Periods)
  periods <- qcew_resolve_periods(Periods)
  ownership <- qcew_match_lookup(Ownership, qcew_ownership_lookup, "ownership_input", "qcew_ownership", "qcew_ownership_label", "Ownership")
  report_type <- qcew_match_lookup(ReportType, qcew_report_type_lookup, "report_input", "qcew_report_type", "qcew_report_type_label", "ReportType")
  industry_level <- qcew_match_lookup(IndustryLevel, qcew_industry_level_lookup, "level_input", "qcew_industry_level", arg_name = "IndustryLevel")

  if (!is.null(NAICS) && ownership$value == "00") {
    message(
      "Note: Ownership = \"All Ownerships\" can return multiple ownership rows for a requested NAICS. ",
      "Use Ownership = \"Private\" for private-industry NAICS series."
    )
  }

  if (industry_level$value == "0") {
    IndustrySuperSector <- "0000"
    IndustrySector <- "00"
    Industry <- "0000"
  }

  geo_selected <- qcew_select_geographies(Geographies)
  request_plan <- qcew_build_request_plan(geo_selected, years, periods)
  request_plan$destination_path <- vapply(
    seq_len(nrow(request_plan)),
    function(i) {
      qcew_destination_path(
        geography_row = geo_selected[request_plan$geo_id[i], ],
        year = request_plan$year[i],
        period = list(
          value = request_plan$qcew_period_code[i],
          label = request_plan$qcew_period_label[i]
        ),
        ownership = ownership,
        report_type = report_type,
        industry_level = industry_level,
        industry_supersector = IndustrySuperSector,
        industry_sector = IndustrySector,
        industry = Industry,
        DownloadDir = DownloadDir
      )
    },
    character(1)
  )
  request_plan$download_status <- "downloaded"
  request_plan$refresh_reason <- NA_character_
  request_plan$cache_age_days <- NA_real_
  request_plan$cache_max_age_days <- NA_real_
  request_plan$metadata_path <- vapply(
    request_plan$destination_path,
    oed_cache_sidecar_path,
    character(1)
  )
  latest_year <- max(years)
  for (i in seq_len(nrow(request_plan))) {
    geography_row <- geo_selected[request_plan$geo_id[i], ]
    request_parameters <- list(
      geography = geography_row$geography,
      qcew_area = geography_row$qcew_area,
      year = request_plan$year[i],
      period = request_plan$qcew_period_label[i],
      ownership = ownership$value,
      report_type = report_type$value,
      industry_level = industry_level$value,
      industry_supersector = IndustrySuperSector,
      industry_sector = IndustrySector,
      industry = Industry
    )
    current_request <- identical(as.integer(request_plan$year[i]), as.integer(latest_year))
    effective_max_age <- oed_cache_max_age(
      "QCEW",
      MaxAge = MaxAge,
      current_request = current_request
    )
    decision <- oed_cache_decision(
      path = request_plan$destination_path[i],
      command = "OED_QCEW_Table",
      dataset = "QCEW",
      source_url = "https://www.qualityinfo.org/ewind",
      request_parameters = request_parameters,
      Refresh = Refresh,
      Overwrite = Overwrite,
      MaxAge = effective_max_age,
      # A valid cache hit must not make a HEAD request. Server revision
      # headers are still saved when the workbook itself is downloaded.
      current_etag = NULL,
      current_last_modified = NULL,
      latest_available_period = if (current_request) as.character(latest_year) else NULL,
      catalog_title = basename(request_plan$destination_path[i]),
      validator = qcew_is_xlsx
    )
    request_plan$download_status[i] <- decision$status
    request_plan$refresh_reason[i] <- decision$reason
    request_plan$cache_age_days[i] <- decision$cache_age_days
    request_plan$cache_max_age_days[i] <- decision$max_age_days
    request_plan$metadata_path[i] <- decision$metadata_path
  }

  if (isTRUE(PreviewOnly)) {
    return(request_plan |>
      mutate(
        naics_filter = if (is.null(NAICS)) NA_character_ else paste(NAICS, collapse = ", "),
        naics_match = NAICSMatch
      ))
  }

  planned_downloads <- sum(request_plan$download_status == "downloaded", na.rm = TRUE)
  if (is.finite(MaxRequests) && planned_downloads > MaxRequests) {
    stop(
      "This call would make ", planned_downloads, " QualityInfo Excel requests. ",
      "Narrow Years, Periods, Geographies, or set PreviewOnly = TRUE to inspect the request plan. ",
      "If the plan is intentional, raise MaxRequests.",
      call. = FALSE
    )
  }

  failed_requests <- tibble::tibble(
    geography = character(),
    qcew_area = character(),
    year = integer(),
    period = character(),
    reason = character()
  )

  all_data <- list()
  download_paths <- rep(NA_character_, nrow(request_plan))

  for (i in seq_len(nrow(request_plan))) {
    plan_row <- request_plan[i, ]
    geography_row <- geo_selected[plan_row$geo_id, ]
    year <- plan_row$year
    period <- list(
      value = plan_row$qcew_period_code,
      label = plan_row$qcew_period_label
    )

    message("Processing: ", geography_row$geography, " | ", year, " ", period$label)

    tf <- NULL
    if (identical(plan_row$download_status, "cached")) {
      tf <- plan_row$destination_path
    } else if (identical(plan_row$download_status, "downloaded")) {
      planned_reason <- plan_row$refresh_reason
      tf <- tryCatch(
        qcew_download_one(
          geography_row = geography_row,
          year = year,
          period = period,
          ownership = ownership,
          report_type = report_type,
          industry_level = industry_level,
          industry_supersector = IndustrySuperSector,
          industry_sector = IndustrySector,
          industry = Industry,
          destination_path = plan_row$destination_path,
          Overwrite = Overwrite,
          # The planning pass already decided that this workbook needs a
          # download, so do not repeat a freshness check before replacing it.
          Refresh = "always",
          MaxAge = plan_row$cache_max_age_days,
          RequestParameters = list(
            geography = geography_row$geography,
            qcew_area = geography_row$qcew_area,
            year = year,
            period = period$label,
            ownership = ownership$value,
            report_type = report_type$value,
            industry_level = industry_level$value,
            industry_supersector = IndustrySuperSector,
            industry_sector = IndustrySector,
            industry = Industry
          ),
          LatestAvailablePeriod = as.character(latest_year)
        ),
        error = function(e) {
          failed_requests <<- bind_rows(
            failed_requests,
            tibble::tibble(
              geography = geography_row$geography,
              qcew_area = geography_row$qcew_area,
              year = year,
              period = period$label,
              reason = conditionMessage(e)
            )
          )
          NULL
        }
      )
    }

    if (is.null(tf)) {
      request_plan$download_status[i] <- "failed"
      next
    }

    if (is.list(tf)) {
      request_plan$download_status[i] <- tf$status
      request_plan$refresh_reason[i] <- if (tf$status %in% c("downloaded", "refreshed")) {
        planned_reason
      } else {
        tf$reason
      }
      request_plan$cache_age_days[i] <- tf$cache_age_days
      request_plan$metadata_path[i] <- tf$metadata_path
      tf <- tf$path
    }
    download_paths[i] <- tf

    result <- tryCatch(
      qcew_read_workbooks(tf, Sheets = Sheets, keep_raw_values = keep_raw_values) |>
        qcew_filter_naics(NAICS = NAICS, NAICSMatch = NAICSMatch) |>
        mutate(
          geo_type = geography_row$geo_type,
          area_type = geography_row$area_type,
          geo_code = geography_row$geo_code,
          qcew_area = geography_row$qcew_area,
          qcew_ownership = .env$ownership$value,
          qcew_ownership_label = .env$ownership$label,
          qcew_report_type = .env$report_type$value,
          qcew_report_type_label = .env$report_type$label,
          qcew_period_code = .env$period$value,
          qcew_period_label = .env$period$label,
          .after = "geography"
        ),
      error = function(e) {
        failed_requests <<- bind_rows(
          failed_requests,
          tibble::tibble(
            geography = geography_row$geography,
            qcew_area = geography_row$qcew_area,
            year = year,
            period = period$label,
            reason = conditionMessage(e)
          )
        )
        NULL
      }
    )

    if (!is.null(result) && nrow(result) > 0) {
      all_data[[length(all_data) + 1]] <- result
    } else if (!is.null(result) && nrow(result) == 0) {
      failed_requests <- bind_rows(
        failed_requests,
        tibble::tibble(
          geography = geography_row$geography,
          qcew_area = geography_row$qcew_area,
          year = year,
          period = period$label,
          reason = if (is.null(NAICS)) "No rows parsed" else "No rows matched NAICS filter"
        )
      )
    }
  }

  final_df <- bind_rows(all_data)

  if (nrow(final_df) == 0) {
    if (nrow(failed_requests) > 0) {
      stop(
        "No QCEW rows were returned. First failed request: ",
        failed_requests$geography[1], " | ", failed_requests$year[1], " ",
        failed_requests$period[1], " | ", failed_requests$reason[1],
        call. = FALSE
      )
    }

    stop(
      "No QCEW rows were returned, and no failed request details were captured.",
      call. = FALSE
    )
  }

  if (all(c("geography", "naics", "year") %in% names(final_df))) {
    final_df <- final_df |>
      mutate(date = qcew_period_date(.data$year, .data$quarter), .after = period) |>
      mutate(qcew_period_sort = if_else(is.na(.data$quarter), 0L, .data$quarter)) |>
      arrange(.data$geography, .data$naics, .data$date, .data$qcew_period_sort) |>
      dplyr::select(-.data$qcew_period_sort)
  }

  if (!isTRUE(KeepDownloadMetadata)) {
    final_df <- final_df |>
      dplyr::select(-dplyr::any_of(c("workbook", "path", "downloaded")))
  }

  request_plan$path <- download_paths
  request_plan$download_status[is.na(download_paths)] <- "failed"
  final_df <- oed_attach_download_diagnostics(
    final_df,
    command = "OED_QCEW_Table",
    plan = request_plan,
    failed_requests = failed_requests
  )
  attr(final_df, "failed_requests") <- failed_requests

  naics_availability <- qcew_naics_availability(
    data = final_df,
    request_plan = request_plan,
    NAICS = NAICS,
    NAICSMatch = NAICSMatch
  )
  attr(final_df, "naics_availability") <- naics_availability

  if (isTRUE(ReportNAICSAvailability)) {
    qcew_message_naics_availability(naics_availability)
  }

  final_df
}
