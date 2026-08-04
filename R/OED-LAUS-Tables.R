# Local Area Unemployment Statistics (LAUS) download and parsing.
#
# Local workbooks are parsed from the QualityInfo LAUS schema. Live mode reads
# the report endpoint and lookup services exposed by /uesti, then downloads
# the returned Excel workbook.

laus_clean_names <- function(x) {
  cleaned <- x |>
    as.character() |>
    stringr::str_squish() |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")

  blank <- is.na(cleaned) | !nzchar(cleaned)
  cleaned[blank] <- paste0("blank_", seq_len(sum(blank)))
  make.unique(cleaned, sep = "_")
}

laus_find_header_row <- function(raw_mat) {
  if (nrow(raw_mat) == 0 || ncol(raw_mat) == 0) return(NA_integer_)

  row_names <- lapply(seq_len(nrow(raw_mat)), function(row_id) {
    laus_clean_names(raw_mat[row_id, ])
  })
  metric_names <- c(
    "unemp_rate", "unemployment_rate", "labor_force", "employed", "unemployed"
  )

  detailed <- vapply(row_names, function(names) {
    "period" %in% names && sum(names %in% metric_names) >= 3
  }, logical(1))
  if (any(detailed)) return(which(detailed)[1])

  summary <- vapply(row_names, function(names) {
    any(names %in% c("period", "year", "date")) &&
      sum(names %in% metric_names) >= 3
  }, logical(1))
  matches <- which(summary)
  if (length(matches) == 0) NA_integer_ else matches[1]
}

laus_find_column <- function(column_names, candidates) {
  hit <- which(column_names %in% candidates)
  if (length(hit) == 0) NA_character_ else column_names[hit[1]]
}

laus_parse_number <- function(x) {
  value <- x |>
    as.character() |>
    stringr::str_squish() |>
    stringr::str_remove_all(stringr::regex("\\(p\\)", ignore_case = TRUE)) |>
    stringr::str_replace_all("[,%]", "")
  value[value %in% c("", "-", "--", "NA", "N/A")] <- NA_character_
  suppressWarnings(as.numeric(value))
}

laus_parse_date <- function(x) {
  value <- stringr::str_squish(as.character(x))
  value <- stringr::str_remove_all(
    value, stringr::regex("\\(p\\)", ignore_case = TRUE)
  )
  value <- stringr::str_squish(value)
  value[!nzchar(value)] <- NA_character_

  out <- rep(as.Date(NA), length(value))
  formats <- c(
    "%d-%b-%Y", "%d-%B-%Y", "%d-%b %Y", "%d-%B %Y",
    "%Y-%m-%d", "%m/%d/%Y"
  )
  for (format in formats) {
    missing <- is.na(out) & !is.na(value)
    if (!any(missing)) break
    candidate <- value[missing]
    if (format %in% c("%d-%b-%Y", "%d-%B-%Y", "%d-%b %Y", "%d-%B %Y")) {
      candidate <- paste0("01-", candidate)
    }
    parsed <- tryCatch(
      suppressWarnings(as.Date(candidate, format = format)),
      error = function(error) rep(as.Date(NA), length(candidate))
    )
    out[missing] <- parsed
  }

  year_only <- is.na(out) & !is.na(value) &
    stringr::str_detect(value, "^[0-9]{4}$")
  if (any(year_only)) {
    out[year_only] <- as.Date(
      paste0(value[year_only], "-01-01"),
      format = "%Y-%m-%d"
    )
  }
  out
}

laus_geography_from_sheet <- function(sheet) {
  sheet |>
    stringr::str_remove(stringr::regex("_col(?:_all)?$", ignore_case = TRUE)) |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_squish()
}

laus_empty_table <- function() {
  tibble::tibble(
    workbook = character(),
    sheet = character(),
    geography = character(),
    date = as.Date(character()),
    period = character(),
    unemployment_rate = numeric(),
    labor_force = numeric(),
    employed = numeric(),
    unemployed = numeric(),
    provisional = logical()
  )
}

laus_read_sheet <- function(path, sheet) {
  raw <- suppressMessages(readxl::read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal",
    trim_ws = FALSE
  ))

  raw_mat <- as.matrix(raw)
  storage.mode(raw_mat) <- "character"
  header_row <- laus_find_header_row(raw_mat)

  if (is.na(header_row)) {
    stop("Could not find a LAUS header row in ", path, " / ", sheet, call. = FALSE)
  }

  header <- raw_mat[header_row, ]
  active <- which(!is.na(header) & nzchar(stringr::str_squish(header)))
  if (length(active) == 0 || header_row >= nrow(raw_mat)) {
    return(laus_empty_table())
  }

  last_col <- max(active)
  data_mat <- raw_mat[
    seq.int(header_row + 1L, nrow(raw_mat)),
    seq_len(last_col),
    drop = FALSE
  ]
  data <- tibble::as_tibble(data_mat, .name_repair = "minimal")
  names(data) <- laus_clean_names(header[seq_len(last_col)])

  period_col <- laus_find_column(names(data), c("period", "year", "date"))
  rate_col <- laus_find_column(names(data), c("unemp_rate", "unemployment_rate"))
  labor_force_col <- laus_find_column(names(data), "labor_force")
  employed_col <- laus_find_column(names(data), "employed")
  unemployed_col <- laus_find_column(names(data), "unemployed")

  required <- c(period_col, rate_col, labor_force_col, employed_col, unemployed_col)
  if (any(is.na(required))) {
    stop(
      "LAUS columns are incomplete in ", path, " / ", sheet,
      ". Expected period, unemployment rate, labor force, employed, and unemployed.",
      call. = FALSE
    )
  }

  provisional <- if (nrow(data) == 0) {
    logical()
  } else {
    apply(data, 1, function(row) {
      any(stringr::str_detect(
        as.character(row),
        stringr::regex("\\(p\\)", ignore_case = TRUE)
      ), na.rm = TRUE)
    })
  }

  result <- tibble::tibble(
    workbook = basename(path),
    sheet = sheet,
    geography = laus_geography_from_sheet(sheet),
    date = laus_parse_date(data[[period_col]]),
    period = stringr::str_squish(as.character(data[[period_col]])),
    unemployment_rate = laus_parse_number(data[[rate_col]]),
    labor_force = laus_parse_number(data[[labor_force_col]]),
    employed = laus_parse_number(data[[employed_col]]),
    unemployed = laus_parse_number(data[[unemployed_col]]),
    provisional = provisional
  )

  valid_date <- !is.na(result$date)
  placeholder <- !is.na(result$unemployment_rate) &
    !is.na(result$labor_force) &
    !is.na(result$employed) &
    !is.na(result$unemployed) &
    result$unemployment_rate == 0 &
    result$labor_force == 0 &
    result$employed == 0 &
    result$unemployed == 0

  result[valid_date & !placeholder, , drop = FALSE] |>
    dplyr::arrange(.data$geography, .data$date)
}

laus_select_sheets <- function(path, Sheets = NULL) {
  workbook_sheets <- readxl::excel_sheets(path)
  if (!is.null(Sheets)) {
    missing <- setdiff(Sheets, workbook_sheets)
    if (length(missing) > 0) {
      stop(
        "LAUS sheet(s) not found in ", path, ": ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    return(Sheets)
  }

  preferred <- workbook_sheets[stringr::str_detect(
    workbook_sheets,
    stringr::regex("_col_all$", ignore_case = TRUE)
  )]
  if (length(preferred) == 0) {
    preferred <- workbook_sheets[stringr::str_detect(
      workbook_sheets,
      stringr::regex("_col$", ignore_case = TRUE)
    )]
  }
  if (length(preferred) == 0) workbook_sheets else preferred
}

laus_filter_geographies <- function(data, Geographies = NULL) {
  if (is.null(Geographies) || length(Geographies) == 0 || nrow(data) == 0) {
    return(data)
  }

  keys <- stringr::str_to_lower(as.character(Geographies))
  keep <- stringr::str_to_lower(data$geography) %in% keys |
    stringr::str_to_lower(data$sheet) %in% keys

  if (!any(keep)) {
    stop(
      "No matching LAUS geographies or sheets. Available sheets: ",
      paste(unique(data$sheet), collapse = ", "),
      call. = FALSE
    )
  }

  data[keep, , drop = FALSE]
}

laus_read_workbooks <- function(Paths, Sheets = NULL, Geographies = NULL) {
  workbook_results <- lapply(Paths, function(path) {
    if (!file.exists(path)) {
      stop("File not found: ", path, call. = FALSE)
    }
    if (!data_is_xlsx(path)) {
      stop("File is not an xlsx workbook: ", path, call. = FALSE)
    }

    sheets <- laus_select_sheets(path, Sheets = Sheets)
    sheet_results <- lapply(sheets, function(sheet) {
      tryCatch(
        laus_read_sheet(path, sheet),
        error = function(e) {
          if (!is.null(Sheets)) {
            stop(e)
          }
          NULL
        }
      )
    })
    sheet_results <- Filter(Negate(is.null), sheet_results)
    if (length(sheet_results) == 0) {
      stop("No parseable LAUS sheets found in: ", path, call. = FALSE)
    }
    dplyr::bind_rows(sheet_results)
  })

  dplyr::bind_rows(workbook_results) |>
    laus_filter_geographies(Geographies = Geographies) |>
    dplyr::arrange(.data$geography, .data$date)
}

# Live mode is metadata-driven: /uesti exposes the report URL, while the
# QualityInfo services provide current geography codes and available years.
# This avoids relying on static links that the interactive page does not have.
laus_live_absolute_url <- function(href, base_url) {
  href <- stringr::str_squish(as.character(href))
  if (stringr::str_detect(href, "^https?://")) return(href)
  if (stringr::str_starts(href, "//")) return(paste0("https:", href))

  origin <- stringr::str_match(base_url, "^(https?://[^/]+)")[, 2]
  if (stringr::str_starts(href, "/")) return(paste0(origin, href))
  paste0(stringr::str_replace(base_url, "/[^/]*$", "/"), href)
}

laus_live_input_value <- function(html, id) {
  patterns <- c(
    paste0(
      "<input\\b[^>]*\\bid=[\\\"']", id,
      "[\\\"'][^>]*\\bvalue=[\\\"']([^\\\"']+)[\\\"']"
    ),
    paste0(
      "<input\\b[^>]*\\bvalue=[\\\"']([^\\\"']+)[\\\"'][^>]*\\bid=[\\\"']",
      id, "[\\\"']"
    )
  )

  for (pattern in patterns) {
    match <- stringr::str_match(
      html,
      stringr::regex(pattern, ignore_case = TRUE, dotall = TRUE)
    )
    if (nrow(match) > 0 && !is.na(match[1, 2])) {
      return(data_html_decode(match[1, 2]))
    }
  }

  stop(
    "QualityInfo LAUS page did not expose the expected ", id,
    " input. The page layout or report endpoint may have changed.",
    call. = FALSE
  )
}

laus_live_page_config <- function(PageUrl) {
  response <- tryCatch(
    httr2::request(PageUrl) |>
      httr2::req_user_agent("Mozilla/5.0") |>
      oed_request_perform(),
    error = function(error) {
      stop(
        "Could not load the QualityInfo LAUS page: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )

  html <- httr2::resp_body_string(response)
  origin <- stringr::str_match(PageUrl, "^(https?://[^/]+)")[, 2]
  if (is.na(origin) || !nzchar(origin)) {
    stop("PageUrl must be an absolute HTTP(S) URL.", call. = FALSE)
  }

  list(
    page_url = PageUrl,
    report_xlsx_url = laus_live_absolute_url(
      laus_live_input_value(html, "jsGetReportXlsxUrl"),
      PageUrl
    ),
    service_url = paste0(origin, "/lmiservice/service")
  )
}

laus_live_service_json <- function(url) {
  response <- tryCatch(
    httr2::request(url) |>
      httr2::req_user_agent("Mozilla/5.0") |>
      oed_request_perform(),
    error = function(error) {
      stop(
        "Could not load the QualityInfo LAUS service: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )

  tryCatch(
    jsonlite::fromJSON(
      httr2::resp_body_string(response),
      simplifyVector = TRUE
    ),
    error = function(error) {
      stop(
        "QualityInfo returned invalid JSON for the LAUS service: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
}

laus_live_key <- function(x) {
  stringr::str_replace_all(
    stringr::str_to_lower(stringr::str_squish(as.character(x))),
    "[^a-z0-9]+",
    ""
  )
}

laus_live_areas <- function(config) {
  payload <- laus_live_service_json(
    paste0(config$service_url, "/geog/areas/41/laus")
  )
  areas <- payload$Geog

  if (is.null(areas) || nrow(areas) == 0) {
    stop("QualityInfo returned no LAUS geographies.", call. = FALSE)
  }

  tibble::as_tibble(areas) |>
    dplyr::transmute(
      name = as.character(.data$areaname),
      code = paste0(
        as.character(.data$stfips),
        as.character(.data$areatype),
        as.character(.data$area)
      )
    ) |>
    dplyr::mutate(name_key = laus_live_key(.data$name))
}

laus_live_resolve_areas <- function(config, Geographies = NULL) {
  areas <- laus_live_areas(config)
  requested <- if (is.null(Geographies) || length(Geographies) == 0) {
    "Oregon"
  } else {
    as.character(Geographies)
  }

  requested_keys <- laus_live_key(requested)
  matched <- integer(length(requested_keys))

  for (i in seq_along(requested_keys)) {
    exact <- which(
      areas$name_key == requested_keys[i] |
        laus_live_key(areas$code) == requested_keys[i]
    )
    if (length(exact) == 0 && !stringr::str_ends(requested_keys[i], "county")) {
      exact <- which(areas$name_key == paste0(requested_keys[i], "county"))
    }
    if (length(exact) == 0 && requested_keys[i] == "statewide") {
      exact <- which(areas$name_key == "oregon")
    }

    if (length(exact) == 0) {
      stop(
        "No matching QualityInfo LAUS geography for: ", requested[i],
        ". Available examples include: ",
        paste(utils::head(areas$name, 10), collapse = ", "), ".",
        call. = FALSE
      )
    }
    matched[i] <- exact[1]
  }

  areas[unique(matched), c("name", "code"), drop = FALSE]
}

laus_live_available_years <- function(config) {
  payload <- laus_live_service_json(
    paste0(config$service_url, "/employment/labforceyear?laussort=desc")
  )
  years <- suppressWarnings(as.integer(payload$PeriodYear$periodyear))
  years[!is.na(years)]
}

laus_live_years <- function(config, StartYear = NULL, EndYear = NULL) {
  if (xor(is.null(StartYear), is.null(EndYear))) {
    stop("Provide both StartYear and EndYear, or neither.", call. = FALSE)
  }

  if (is.null(StartYear)) {
    years <- laus_live_available_years(config)
    if (length(years) == 0) {
      stop("QualityInfo returned no LAUS years.", call. = FALSE)
    }
    StartYear <- EndYear <- max(years)
  }

  if (length(StartYear) != 1 || length(EndYear) != 1) {
    stop("StartYear and EndYear must each contain one year.", call. = FALSE)
  }

  start <- suppressWarnings(as.integer(StartYear))
  end <- suppressWarnings(as.integer(EndYear))
  if (is.na(start) || is.na(end) || start < 1900 || end < 1900) {
    stop("StartYear and EndYear must be valid four-digit years.", call. = FALSE)
  }
  if (start > end) {
    stop("StartYear must be less than or equal to EndYear.", call. = FALSE)
  }

  list(start = as.character(start), end = as.character(end))
}

# Return the current QualityInfo LAUS geography codes and available years so
# users can discover valid inputs before requesting a workbook.
OED_LAUS_Options <- function(PageUrl = "https://www.qualityinfo.org/uesti") {
  config <- laus_live_page_config(PageUrl)

  list(
    geographies = laus_live_areas(config) |>
      dplyr::select(.data$name, .data$code),
    years = laus_live_available_years(config),
    page_url = PageUrl
  )
}

laus_live_catalog <- function(PageUrl,
                              Geographies = NULL,
                              StartYear = NULL,
                              EndYear = NULL,
                              DownloadDir) {
  config <- laus_live_page_config(PageUrl)
  areas <- laus_live_resolve_areas(config, Geographies = Geographies)
  years <- laus_live_years(
    config,
    StartYear = StartYear,
    EndYear = EndYear
  )

  request <- httr2::request(config$report_xlsx_url) |>
    httr2::req_url_query(
      lf_areanames = paste(areas$name, collapse = ","),
      lf_areacode = paste(areas$code, collapse = ","),
      lf_measure = "all",
      lf_adjusted = "1",
      lf_syear = years$start,
      lf_eyear = years$end
    )

  title <- if (nrow(areas) == 1) areas$name else "selected_areas"
  title <- paste0("LAUS_", title, "_", years$start, "-", years$end)

  tibble::tibble(
    file_title = title,
    download_url = request$url,
    data_link_label = title,
    geography = paste(areas$name, collapse = ", "),
    start_year = as.integer(years$start),
    end_year = as.integer(years$end),
    destination_path = data_destination_path(title, DownloadDir)
  )
}

# Public LAUS loader. Paths reads local workbooks; Urls accepts explicit
# workbook links; otherwise the function builds a live portlet request.
OED_LAUS_Table <- function(Geographies = NULL,
                           Paths = NULL,
                           Urls = NULL,
                           DownloadDir = file.path("output", "qualityinfo_laus"),
                           Overwrite = FALSE,
                           PreviewOnly = FALSE,
                           MaxDownloads = 10,
                           Sheets = NULL,
                           PageUrl = "https://www.qualityinfo.org/uesti",
                           StartYear = NULL,
                           EndYear = NULL) {
  if (!is.null(Paths)) {
    if (isTRUE(PreviewOnly)) {
      return(tibble::tibble(
        command = "OED_LAUS_Table",
        path = as.character(Paths),
        exists = file.exists(Paths)
      ))
    }
    return(laus_read_workbooks(
      Paths = Paths,
      Sheets = Sheets,
      Geographies = Geographies
    ))
  }

  selected <- if (!is.null(Urls)) {
    data_page_workbook_catalog(PageUrl = PageUrl, Urls = Urls) |>
      dplyr::mutate(
        geography = NA_character_,
        start_year = NA_integer_,
        end_year = NA_integer_,
        destination_path = vapply(
          .data$file_title,
          data_destination_path,
          character(1),
          DownloadDir = DownloadDir
        )
      )
  } else {
    laus_live_catalog(
      PageUrl = PageUrl,
      Geographies = Geographies,
      StartYear = StartYear,
      EndYear = EndYear,
      DownloadDir = DownloadDir
    )
  }
  selected$download_status <- ifelse(
    file.exists(selected$destination_path) && !isTRUE(Overwrite),
    "cached",
    "downloaded"
  )
  selected$command <- "OED_LAUS_Table"
  selected$page_url <- PageUrl
  selected <- dplyr::relocate(selected, .data$command, .data$page_url)

  if (isTRUE(PreviewOnly)) return(selected)

  if (is.finite(MaxDownloads) && nrow(selected) > MaxDownloads) {
    stop(
      "OED_LAUS_Table would download ", nrow(selected),
      " QualityInfo Excel files. Provide fewer Urls, set PreviewOnly = TRUE, ",
      "or raise MaxDownloads.",
      call. = FALSE
    )
  }

  paths <- character(nrow(selected))
  for (i in seq_len(nrow(selected))) {
    paths[i] <- data_download_one(
      selected$download_url[i],
      selected$destination_path[i],
      Overwrite = Overwrite,
      DataUrl = PageUrl
    )
  }
  downloaded <- dplyr::mutate(selected, path = paths)
  read_geographies <- Geographies
  if (is.null(Urls) && "geography" %in% names(downloaded)) {
    read_geographies <- unique(unlist(
      strsplit(downloaded$geography, ", ", fixed = TRUE),
      use.names = FALSE
    ))
  }
  result <- laus_read_workbooks(
    Paths = downloaded$path,
    Sheets = Sheets,
    Geographies = read_geographies
  )
  oed_attach_download_diagnostics(
    result,
    command = "OED_LAUS_Table",
    plan = downloaded
  )
}
