# Local Area Unemployment Statistics (LAUS) download and parsing.
#
# Local workbooks are parsed from the QualityInfo LAUS schema. The standard
# live path uses a bundled selected-area endpoint and lookup table, then
# downloads the requested Excel workbooks. Dynamic metadata discovery is an
# explicit fallback, not part of ordinary calls.

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

laus_adjustment_from_text <- function(x) {
  text <- stringr::str_to_lower(stringr::str_squish(as.character(x)))

  if (stringr::str_detect(text, "not seasonally adjusted")) {
    return("not seasonally adjusted")
  }
  if (stringr::str_detect(text, "seasonally adjusted")) {
    return("seasonally adjusted")
  }
  if (stringr::str_detect(text, "annual averages?")) {
    return("annual average")
  }

  NA_character_
}

laus_measure_from_text <- function(x) {
  text <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  text <- stringr::str_replace_all(text, "[^a-z0-9]+", " ")

  if (stringr::str_detect(text, "unemployment rate|unemp rate")) {
    return("unemployment_rate")
  }
  if (stringr::str_detect(text, "civilian labor force|labor force")) {
    return("labor_force")
  }
  if (stringr::str_detect(text, "number of unemployed|unemployed")) {
    return("unemployed")
  }
  if (stringr::str_detect(text, "number of employed|employed")) {
    return("employed")
  }

  NA_character_
}

laus_find_wide_header_row <- function(raw_mat) {
  if (nrow(raw_mat) == 0 || ncol(raw_mat) == 0) {
    return(NA_integer_)
  }

  first_col <- laus_clean_names(raw_mat[, 1])
  hit <- which(first_col == "area")
  if (length(hit) == 0) NA_integer_ else hit[1]
}

laus_wide_sheet_metadata <- function(raw_mat, sheet) {
  header_row <- laus_find_wide_header_row(raw_mat)
  if (is.na(header_row) || nrow(raw_mat) < 3) {
    return(NULL)
  }

  first_values <- as.character(raw_mat[seq_len(min(5, nrow(raw_mat))), 1])
  frequency <- if (any(stringr::str_detect(
    stringr::str_to_lower(first_values), "annual averages?"
  ), na.rm = TRUE)) {
    "annual"
  } else {
    "monthly"
  }

  measure <- laus_measure_from_text(raw_mat[2, 1])
  if (is.na(measure)) {
    return(NULL)
  }

  adjustment_text <- paste(first_values, collapse = " ")
  seasonal_adjustment <- if (frequency == "annual") {
    "annual average"
  } else {
    laus_adjustment_from_text(adjustment_text)
  }

  list(
    header_row = header_row,
    frequency = frequency,
    measure = measure,
    seasonal_adjustment = seasonal_adjustment
  )
}

laus_selected_sheet_metadata <- function(raw_mat, sheet) {
  if (nrow(raw_mat) < 3 || ncol(raw_mat) < 2) return(NULL)

  first_col <- laus_clean_names(raw_mat[, 1])
  header_row <- which(first_col == "year")
  if (length(header_row) == 0) return(NULL)
  header_row <- header_row[1]

  measure_text <- stringr::str_squish(as.character(raw_mat[2, 1]))
  is_annual <- stringr::str_detect(
    stringr::str_to_lower(measure_text),
    "all laus measures"
  )
  measure <- if (is_annual) NA_character_ else laus_measure_from_text(measure_text)
  if (!is_annual && is.na(measure)) return(NULL)

  list(
    header_row = header_row,
    frequency = if (is_annual) "annual" else "monthly",
    measure = measure,
    seasonal_adjustment = if (is_annual) {
      "annual average"
    } else {
      laus_adjustment_from_text(raw_mat[3, 1])
    },
    geography = stringr::str_squish(as.character(raw_mat[1, 1]))
  )
}

laus_read_wide_matrix <- function(raw_mat, path, sheet, metadata) {
  header_row <- metadata$header_row
  last_col <- ncol(raw_mat)
  data_start <- which(
    seq_len(nrow(raw_mat)) > header_row &
      !is.na(raw_mat[, 1]) &
      nzchar(stringr::str_squish(as.character(raw_mat[, 1])))
  )

  if (length(data_start) == 0 || last_col < 2) {
    return(laus_empty_table())
  }

  data_start <- data_start[1]
  data_rows <- seq.int(data_start, nrow(raw_mat))
  geography <- stringr::str_squish(as.character(raw_mat[data_rows, 1]))
  keep_rows <- !is.na(geography) & nzchar(geography)
  geography <- geography[keep_rows]
  data_rows <- data_rows[keep_rows]

  if (metadata$frequency == "monthly") {
    month_values <- raw_mat[header_row, -1, drop = TRUE]
    year_values <- raw_mat[header_row + 1L, -1, drop = TRUE]
    period_values <- paste0(
      stringr::str_squish(as.character(month_values)),
      "-",
      stringr::str_squish(as.character(year_values))
    )
  } else {
    period_values <- stringr::str_squish(as.character(raw_mat[header_row, -1, drop = TRUE]))
  }

  dates <- laus_parse_date(period_values)
  values <- raw_mat[data_rows, -1, drop = FALSE]
  numeric_values <- matrix(
    laus_parse_number(as.vector(values)),
    nrow = nrow(values),
    ncol = ncol(values),
    byrow = FALSE
  )
  n_areas <- length(geography)
  n_periods <- length(dates)

  result <- tibble::tibble(
    workbook = basename(path),
    sheet = sheet,
    geography = rep(geography, each = n_periods),
    area_code = NA_character_,
    date = rep(dates, times = n_areas),
    period = rep(period_values, times = n_areas),
    frequency = metadata$frequency,
    seasonal_adjustment = metadata$seasonal_adjustment,
    measure = as.vector(t(matrix(
      rep(metadata$measure, nrow(numeric_values) * ncol(numeric_values)),
      nrow = nrow(numeric_values),
      ncol = ncol(numeric_values)
    ))),
    value = as.vector(t(numeric_values))
  )

  result |>
    dplyr::filter(!is.na(.data$date), !is.na(.data$value))
}

laus_read_selected_matrix <- function(raw_mat, path, sheet, metadata) {
  header_row <- metadata$header_row
  active <- which(
    !is.na(raw_mat[header_row, ]) &
      nzchar(stringr::str_squish(as.character(raw_mat[header_row, ])))
  )
  if (length(active) < 2 || header_row >= nrow(raw_mat)) {
    return(laus_empty_table())
  }

  data_rows <- seq.int(header_row + 1L, nrow(raw_mat))
  years <- suppressWarnings(as.integer(raw_mat[data_rows, 1]))
  keep_rows <- !is.na(years)
  years <- years[keep_rows]
  data_rows <- data_rows[keep_rows]

  if (length(years) == 0) return(laus_empty_table())

  if (metadata$frequency == "monthly") {
    month_labels <- stringr::str_squish(as.character(raw_mat[header_row, active[-1]]))
    values <- raw_mat[data_rows, active[-1], drop = FALSE]
    numeric_values <- matrix(
      laus_parse_number(as.vector(values)),
      nrow = nrow(values),
      ncol = ncol(values),
      byrow = FALSE
    )
    periods <- paste(
      rep(month_labels, times = length(years)),
      rep(years, each = length(month_labels)),
      sep = "-"
    )
    measures <- rep(metadata$measure, length(periods))
  } else {
    measure_names <- vapply(
      raw_mat[header_row, active[-1]],
      laus_measure_from_text,
      character(1)
    )
    keep_columns <- !is.na(measure_names)
    measure_names <- measure_names[keep_columns]
    values <- raw_mat[data_rows, active[-1][keep_columns], drop = FALSE]
    numeric_values <- matrix(
      laus_parse_number(as.vector(values)),
      nrow = nrow(values),
      ncol = ncol(values),
      byrow = FALSE
    )
    periods <- rep(as.character(years), each = length(measure_names))
    measures <- rep(measure_names, times = length(years))
  }

  result <- tibble::tibble(
    workbook = basename(path),
    sheet = sheet,
    geography = metadata$geography,
    area_code = NA_character_,
    date = laus_parse_date(periods),
    period = periods,
    frequency = metadata$frequency,
    seasonal_adjustment = metadata$seasonal_adjustment,
    measure = measures,
    value = as.vector(t(numeric_values))
  )

  result |>
    dplyr::filter(!is.na(.data$date), !is.na(.data$value))
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
    area_code = character(),
    date = as.Date(character()),
    period = character(),
    frequency = character(),
    seasonal_adjustment = character(),
    measure = character(),
    value = numeric(),
    unemployment_rate = numeric(),
    labor_force = numeric(),
    employed = numeric(),
    unemployed = numeric()
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

  wide_metadata <- laus_wide_sheet_metadata(raw_mat, sheet)
  if (!is.null(wide_metadata)) {
    return(laus_read_wide_matrix(
      raw_mat = raw_mat,
      path = path,
      sheet = sheet,
      metadata = wide_metadata
    ))
  }

  selected_metadata <- laus_selected_sheet_metadata(raw_mat, sheet)
  if (!is.null(selected_metadata)) {
    return(laus_read_selected_matrix(
      raw_mat = raw_mat,
      path = path,
      sheet = sheet,
      metadata = selected_metadata
    ))
  }

  header_row <- laus_find_header_row(raw_mat)

  if (is.na(header_row)) {
    stop(
      "Could not find a LAUS header row in ", path, " / ", sheet, ". ",
      "The QualityInfo workbook layout may have changed; update OEDloadR.",
      call. = FALSE
    )
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

  period <- stringr::str_squish(as.character(data[[period_col]]))

  result <- tibble::tibble(
    workbook = basename(path),
    sheet = sheet,
    geography = laus_geography_from_sheet(sheet),
    area_code = NA_character_,
    date = laus_parse_date(data[[period_col]]),
    period = period,
    frequency = ifelse(
      stringr::str_detect(period, "^[0-9]{4}$"),
      "annual",
      "monthly"
    ),
    seasonal_adjustment = laus_adjustment_from_text(sheet),
    unemployment_rate = laus_parse_number(data[[rate_col]]),
    labor_force = laus_parse_number(data[[labor_force_col]]),
    employed = laus_parse_number(data[[employed_col]]),
    unemployed = laus_parse_number(data[[unemployed_col]])
  )

  valid_date <- !is.na(result$date)
  result[valid_date, , drop = FALSE] |>
    dplyr::arrange(.data$geography, .data$date)
}

laus_select_sheets <- function(path,
                               Sheets = NULL,
                               SeasonalAdjustment = NULL,
                               Frequency = NULL) {
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

  if (!is.null(SeasonalAdjustment) && !is.null(Frequency)) {
    adjustments <- laus_resolve_adjustments(SeasonalAdjustment)
    frequencies <- laus_resolve_frequencies(Frequency)
    monthly_sheets <- workbook_sheets[
      stringr::str_detect(workbook_sheets, stringr::regex("^Monthly", ignore_case = TRUE))
    ]
    annual_sheets <- workbook_sheets[
      stringr::str_detect(workbook_sheets, stringr::regex("^Annual", ignore_case = TRUE))
    ]
    if (length(monthly_sheets) == 0 && length(annual_sheets) == 0) {
      monthly_sheets <- workbook_sheets[
        !stringr::str_detect(
          workbook_sheets,
          stringr::regex("_(?:col_all|col)$", ignore_case = TRUE)
        )
      ]
      annual_sheets <- workbook_sheets[
        stringr::str_detect(
          workbook_sheets,
          stringr::regex("_(?:col_all|col)$", ignore_case = TRUE)
        )
      ]
    }
    selected <- character()
    if ("monthly" %in% frequencies) {
      selected <- c(
        selected,
        monthly_sheets[
          vapply(monthly_sheets, function(sheet) {
            if ("not seasonally adjusted" %in% adjustments) {
              stringr::str_detect(
                stringr::str_to_lower(sheet),
                "not seas"
              )
            } else {
              FALSE
            }
          }, logical(1)) |
            ("seasonally adjusted" %in% adjustments &
              !stringr::str_detect(
                stringr::str_to_lower(monthly_sheets),
                "not seas"
              ))
        ]
      )
    }
    if ("annual" %in% frequencies) selected <- c(selected, annual_sheets)
    if (length(selected) > 0) return(unique(selected))
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

laus_area_lookup_table <- function(AreaLookup) {
  if (is.null(AreaLookup) || nrow(AreaLookup) == 0) {
    return(tibble::tibble(area_key = character(), lookup_area_code = character()))
  }

  required <- c("name", "code")
  if (!all(required %in% names(AreaLookup))) {
    stop(
      "LAUS area metadata must contain name and code columns.",
      call. = FALSE
    )
  }

  AreaLookup |>
    dplyr::transmute(
      area_key = laus_live_key(.data$name),
      lookup_area_code = as.character(.data$code)
    ) |>
    dplyr::distinct(.data$area_key, .keep_all = TRUE)
}

laus_attach_area_codes <- function(data, AreaLookup = NULL) {
  if (nrow(data) == 0 || is.null(AreaLookup)) {
    return(data)
  }

  lookup <- laus_area_lookup_table(AreaLookup)

  data |>
    dplyr::mutate(area_key = laus_live_key(.data$geography)) |>
    dplyr::left_join(lookup, by = "area_key") |>
    dplyr::mutate(
      area_code = dplyr::coalesce(
        as.character(.data$area_code),
        as.character(.data$lookup_area_code)
      )
    ) |>
    dplyr::select(-.data$area_key, -.data$lookup_area_code)
}

laus_first_numeric <- function(x) {
  values <- x[!is.na(x)]
  if (length(values) == 0) NA_real_ else values[1]
}

laus_standardize_legacy <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }

  data |>
    dplyr::mutate(
      area_code = as.character(.data$area_code),
      frequency = dplyr::if_else(
        stringr::str_detect(.data$period, "^[0-9]{4}$"),
        "annual",
        "monthly"
      ),
      seasonal_adjustment = as.character(.data$seasonal_adjustment)
    ) |>
    dplyr::select(
      .data$workbook,
      .data$sheet,
      .data$geography,
      .data$area_code,
      .data$date,
      .data$period,
      .data$frequency,
      .data$seasonal_adjustment,
      .data$unemployment_rate,
      .data$labor_force,
      .data$employed,
      .data$unemployed
    )
}

laus_pivot_measures <- function(data) {
  if (nrow(data) == 0 || !"measure" %in% names(data) ||
      all(is.na(data$measure))) {
    return(laus_standardize_legacy(data))
  }

  data |>
    dplyr::filter(!is.na(.data$measure)) |>
    dplyr::group_by(
      .data$geography,
      .data$area_code,
      .data$date,
      .data$frequency,
      .data$seasonal_adjustment
    ) |>
    dplyr::summarise(
      workbook = paste(sort(unique(.data$workbook)), collapse = "; "),
      sheet = paste(sort(unique(.data$sheet)), collapse = "; "),
      unemployment_rate = laus_first_numeric(
        .data$value[.data$measure == "unemployment_rate"]
      ),
      labor_force = laus_first_numeric(
        .data$value[.data$measure == "labor_force"]
      ),
      employed = laus_first_numeric(
        .data$value[.data$measure == "employed"]
      ),
      unemployed = laus_first_numeric(
        .data$value[.data$measure == "unemployed"]
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      period = ifelse(
        .data$frequency == "annual",
        format(.data$date, "%Y"),
        format(.data$date, "%b-%Y")
      )
    ) |>
    dplyr::select(
      .data$workbook,
      .data$sheet,
      .data$geography,
      .data$area_code,
      .data$date,
      .data$period,
      .data$frequency,
      .data$seasonal_adjustment,
      .data$unemployment_rate,
      .data$labor_force,
      .data$employed,
      .data$unemployed
    ) |>
    dplyr::arrange(.data$geography, .data$date, .data$frequency)
}

laus_resolve_adjustments <- function(SeasonalAdjustment) {
  aliases <- c(
    sa = "seasonally adjusted",
    seasonally_adjusted = "seasonally adjusted",
    nsa = "not seasonally adjusted",
    not_seasonally_adjusted = "not seasonally adjusted"
  )
  values <- stringr::str_to_lower(stringr::str_squish(as.character(SeasonalAdjustment)))
  values <- unname(ifelse(values %in% names(aliases), aliases[values], values))
  if (any(values == "both")) {
    values <- c(values[values != "both"], "seasonally adjusted", "not seasonally adjusted")
  }
  allowed <- c("seasonally adjusted", "not seasonally adjusted")
  if (length(values) == 0 || any(!values %in% allowed)) {
    stop(
      "SeasonalAdjustment must be 'seasonally adjusted', 'not seasonally adjusted', or 'both'.",
      call. = FALSE
    )
  }
  unique(values)
}

laus_resolve_frequencies <- function(Frequency) {
  values <- stringr::str_to_lower(stringr::str_squish(as.character(Frequency)))
  if (any(values == "both")) {
    values <- c(values[values != "both"], "monthly", "annual")
  }
  allowed <- c("monthly", "annual")
  if (length(values) == 0 || any(!values %in% allowed)) {
    stop("Frequency must be 'monthly', 'annual', or 'both'.", call. = FALSE)
  }
  unique(values)
}

laus_resolve_measures <- function(Measures) {
  aliases <- c(
    ur = "unemployment_rate",
    unemp_rate = "unemployment_rate",
    lf = "labor_force",
    unemp = "unemployed",
    emp = "employed"
  )
  values <- stringr::str_to_lower(stringr::str_squish(as.character(Measures)))
  values <- unname(ifelse(values %in% names(aliases), aliases[values], values))
  allowed <- c("unemployment_rate", "labor_force", "unemployed", "employed")
  if (length(values) == 0 || any(!values %in% allowed)) {
    stop(
      "Measures must use unemployment_rate, labor_force, unemployed, or employed (or their short names).",
      call. = FALSE
    )
  }
  unique(values)
}

laus_series_prefix <- function(adjustment) {
  switch(
    adjustment,
    "seasonally adjusted" = "sa",
    "not seasonally adjusted" = "nsa",
    "annual average" = "annual",
    stop("Unknown LAUS adjustment: ", adjustment, call. = FALSE)
  )
}

laus_compact_table <- function(data,
                               SeasonalAdjustment = "seasonally adjusted",
                               Frequency = "monthly",
                               Measures = c(
                                 "unemployment_rate",
                                 "labor_force",
                                 "unemployed",
                                 "employed"
                               ),
                               Metadata = FALSE) {
  adjustments <- laus_resolve_adjustments(SeasonalAdjustment)
  frequencies <- laus_resolve_frequencies(Frequency)
  measures <- laus_resolve_measures(Measures)
  if (nrow(data) == 0) return(data)

  keep <- data$frequency %in% frequencies & (
    data$frequency == "annual" |
      data$seasonal_adjustment %in% adjustments
  )
  data <- data[keep, , drop = FALSE]
  if (nrow(data) == 0) {
    stop(
      "No LAUS observations match the requested Frequency and SeasonalAdjustment.",
      call. = FALSE
    )
  }

  keys <- c("geography", "area_code", "date", "frequency")
  output <- data |>
    dplyr::distinct(dplyr::across(dplyr::all_of(keys)))

  available_adjustments <- unique(data$seasonal_adjustment)
  for (adjustment in available_adjustments) {
    prefix <- laus_series_prefix(adjustment)
    part <- data[data$seasonal_adjustment == adjustment, , drop = FALSE]
    part <- part |>
      dplyr::select(
        dplyr::all_of(keys),
        dplyr::all_of(measures),
        dplyr::any_of(c("workbook", "sheet", "period"))
      ) |>
      dplyr::distinct(dplyr::across(dplyr::all_of(keys)), .keep_all = TRUE)

    names(part)[match(measures, names(part))] <- paste0(
      prefix,
      "_",
      c("ur", "lf", "unemployed", "employed")[match(measures, c(
        "unemployment_rate", "labor_force", "unemployed", "employed"
      ))]
    )
    if (isTRUE(Metadata)) {
      metadata_names <- intersect(c("workbook", "sheet", "period"), names(part))
      names(part)[match(metadata_names, names(part))] <- paste0(
        prefix, "_", metadata_names
      )
    } else {
      part <- part |>
        dplyr::select(-dplyr::any_of(c("workbook", "sheet", "period")))
    }
    output <- dplyr::full_join(output, part, by = keys)
  }

  output <- output |>
    dplyr::mutate(
      year = as.integer(format(.data$date, "%Y")),
      month = ifelse(
        .data$frequency == "monthly",
        as.integer(format(.data$date, "%m")),
        NA_integer_
      )
    ) |>
    dplyr::arrange(.data$year, .data$month, .data$geography)

  measure_columns <- as.vector(outer(
    unique(vapply(available_adjustments, laus_series_prefix, character(1))),
    c("ur", "lf", "unemployed", "employed"),
    paste,
    sep = "_"
  ))
  measure_columns <- intersect(measure_columns, names(output))
  base_columns <- c("year", "month", "area_code", "geography")
  if (isTRUE(Metadata)) {
    base_columns <- c(base_columns, "date", "frequency")
  } else {
    output <- output |>
      dplyr::select(-dplyr::any_of(c("date", "period", "frequency")))
  }
  output |>
    dplyr::select(
      dplyr::all_of(base_columns),
      dplyr::all_of(measure_columns),
      dplyr::everything()
    )
}

laus_filter_geographies <- function(data, Geographies = NULL) {
  if (is.null(Geographies) || length(Geographies) == 0 || nrow(data) == 0) {
    return(data)
  }

  keys <- laus_live_key(Geographies)
  keep <- laus_live_key(data$geography) %in% keys |
    laus_live_key(data$sheet) %in% keys |
    as.character(data$area_code) %in% as.character(Geographies)

  if (!any(keep)) {
    stop(
      "No matching LAUS geographies or sheets. Available sheets: ",
      paste(unique(data$sheet), collapse = ", "),
      call. = FALSE
    )
  }

  data[keep, , drop = FALSE]
}

laus_read_workbooks <- function(Paths,
                                Sheets = NULL,
                                Geographies = NULL,
                                AreaLookup = NULL,
                                SeasonalAdjustment = NULL,
                                Frequency = NULL) {
  workbook_results <- lapply(Paths, function(path) {
    if (!file.exists(path)) {
      stop("File not found: ", path, call. = FALSE)
    }
    if (!data_is_xlsx(path)) {
      stop("File is not an xlsx workbook: ", path, call. = FALSE)
    }

    sheets <- laus_select_sheets(
      path,
      Sheets = Sheets,
      SeasonalAdjustment = SeasonalAdjustment,
      Frequency = Frequency
    )
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
      stop(
        "No parseable LAUS sheets found in: ", path, ". ",
        "The QualityInfo workbook layout may have changed; update OEDloadR.",
        call. = FALSE
      )
    }
    dplyr::bind_rows(sheet_results)
  })

  parsed <- dplyr::bind_rows(workbook_results)
  parsed <- laus_attach_area_codes(parsed, AreaLookup = AreaLookup)
  parsed <- laus_pivot_measures(parsed)

  parsed |>
    laus_filter_geographies(Geographies = Geographies) |>
    dplyr::arrange(.data$geography, .data$date, .data$frequency)
}

# Live mode is static-first for the standard /uesti page. Dynamic discovery
# remains available for an explicitly supplied custom PageUrl or when
# RefreshMetadata = TRUE, but ordinary calls do not reload stable metadata.
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

laus_live_resolve_areas <- function(config,
                                    Geographies = NULL,
                                    Areas = NULL) {
  areas <- if (is.null(Areas)) laus_live_areas(config) else Areas
  requested <- as.character(Geographies)

  if (is.null(Geographies) || length(Geographies) == 0) {
    return(areas[, c("name", "code"), drop = FALSE])
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

laus_live_years <- function(config,
                            StartYear = NULL,
                            EndYear = NULL,
                            AvailableYears = NULL) {
  if (xor(is.null(StartYear), is.null(EndYear))) {
    stop("Provide both StartYear and EndYear, or neither.", call. = FALSE)
  }

  if (is.null(StartYear)) {
    years <- data_or(AvailableYears, config$years)
    if (is.null(years)) years <- laus_live_available_years(config)
    if (length(years) == 0) {
      stop("QualityInfo returned no LAUS years.", call. = FALSE)
    }
    StartYear <- 2000
    EndYear <- max(years)
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

laus_metadata_cache_path <- function(DownloadDir, PageUrl) {
  file.path(
    DownloadDir,
    paste0("LAUS_metadata_", data_title_key(PageUrl), ".rds")
  )
}

laus_load_live_metadata <- function(PageUrl,
                                    DownloadDir,
                                    MetadataCacheDays = 7,
                                    RefreshMetadata = FALSE) {
  static_config <- oed_static_laus_config(PageUrl)
  if (!is.null(static_config) && !isTRUE(RefreshMetadata)) {
    return(list(
      config = static_config,
      areas = oed_static_laus_geographies(),
      years = static_config$years,
      fetched_at = NA,
      metadata_cache_path = NA_character_,
      metadata_cache_status = "static"
    ))
  }

  cache_path <- laus_metadata_cache_path(DownloadDir, PageUrl)
  cache_valid <- FALSE
  cached <- NULL

  if (!isTRUE(RefreshMetadata) && file.exists(cache_path)) {
    age_days <- as.numeric(
      difftime(Sys.time(), file.info(cache_path)$mtime, units = "days")
    )
    cache_valid <- is.finite(age_days) && age_days <= MetadataCacheDays
    if (cache_valid) {
      cached <- tryCatch(readRDS(cache_path), error = function(error) NULL)
      cache_valid <- is.list(cached) &&
        !is.null(cached$config) &&
        !is.null(cached$areas) &&
        !is.null(cached$years)
    }
  }

  if (cache_valid) {
    cached$metadata_cache_path <- cache_path
    cached$metadata_cache_status <- "cached"
    return(cached)
  }

  config <- laus_live_page_config(PageUrl)
  areas <- laus_live_areas(config)
  years <- laus_live_available_years(config)
  if (length(years) == 0) {
    stop("QualityInfo returned no LAUS years.", call. = FALSE)
  }

  metadata <- list(
    config = config,
    areas = areas,
    years = years,
    fetched_at = Sys.time(),
    metadata_cache_path = cache_path,
    metadata_cache_status = "downloaded"
  )
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(metadata, cache_path)
  metadata
}

laus_request_key <- function(scope,
                             area_codes,
                             adjustment_code,
                             start_year,
                             end_year,
                             measure = "all") {
  paste(
    scope,
    paste(sort(as.character(area_codes)), collapse = ","),
    adjustment_code,
    measure,
    start_year,
    end_year,
    sep = "|"
  )
}

laus_cache_status <- function(path,
                              RollingMonthly = FALSE,
                              CacheMaxAgeDays = 1,
                              Overwrite = FALSE) {
  if (isTRUE(Overwrite) || !file.exists(path)) {
    return("downloaded")
  }
  if (!data_is_xlsx(path)) {
    return("stale")
  }
  if (!isTRUE(RollingMonthly)) {
    return("cached")
  }

  age_days <- as.numeric(
    difftime(Sys.time(), file.info(path)$mtime, units = "days")
  )
  if (is.finite(age_days) && age_days <= CacheMaxAgeDays) {
    "cached"
  } else {
    "stale"
  }
}

laus_apply_cache_decisions <- function(plan,
                                       Refresh = "auto",
                                       Overwrite = FALSE,
                                       MaxAge = NULL,
                                       CacheMaxAgeDays = 1) {
  plan$download_status <- "downloaded"
  plan$refresh_reason <- NA_character_
  plan$cache_age_days <- NA_real_
  plan$cache_max_age_days <- NA_real_
  plan$metadata_path <- vapply(
    plan$destination_path,
    oed_cache_sidecar_path,
    character(1)
  )

  effective_max_age <- if (is.null(MaxAge)) CacheMaxAgeDays else MaxAge
  for (i in seq_len(nrow(plan))) {
    decision <- oed_cache_decision(
      path = plan$destination_path[i],
      command = "OED_LAUS_Table",
      dataset = "LAUS",
      source_url = plan$download_url[i],
      request_parameters = as.list(plan[i, , drop = FALSE]),
      Refresh = Refresh,
      Overwrite = Overwrite,
      MaxAge = effective_max_age,
      # Do not turn a valid cache read into HTTP. Revision headers are saved
      # when the workbook itself is downloaded.
      current_etag = NULL,
      current_last_modified = NULL,
      latest_available_period = if (isTRUE(plan$rolling_monthly[i])) {
        as.character(plan$end_year[i])
      } else {
        NULL
      },
      catalog_title = plan$file_title[i],
      catalog_url = plan$download_url[i],
      validator = data_is_xlsx
    )
    plan$download_status[i] <- decision$status
    plan$refresh_reason[i] <- decision$reason
    plan$cache_age_days[i] <- decision$cache_age_days
    plan$cache_max_age_days[i] <- decision$max_age_days
    plan$metadata_path[i] <- decision$metadata_path
  }

  oed_cache_plan_columns(plan)
}

laus_build_request_plan <- function(config,
                                    Areas,
                                    Geographies = NULL,
                                    StartYear,
                                    EndYear,
                                    DownloadDir,
                                    SeasonalAdjustment = "seasonally adjusted",
                                    Frequency = "monthly",
                                    Measures = c(
                                      "unemployment_rate",
                                      "labor_force",
                                      "unemployed",
                                      "employed"
                                    ),
                                    RollingMonthly = FALSE,
                                    CacheMaxAgeDays = 1,
                                    Overwrite = FALSE) {
  selected_areas <- laus_live_resolve_areas(
    config,
    Geographies = Geographies,
    Areas = Areas
  )
  scope <- "selected_areas"
  endpoint <- config$report_xlsx_url
  frequencies <- laus_resolve_frequencies(Frequency)
  measures <- laus_resolve_measures(Measures)
  request_measures <- if ("monthly" %in% frequencies) measures else "all"
  adjustment_names <- laus_resolve_adjustments(SeasonalAdjustment)
  adjustments <- tibble::tibble(
    adjustment = adjustment_names,
    adjustment_code = ifelse(
      adjustment_names == "seasonally adjusted", "1", "0"
    ),
    short = ifelse(
      adjustment_names == "seasonally adjusted", "sa", "nsa"
    )
  )
  area_codes <- selected_areas$code
  area_names <- selected_areas$name
  geography_label <- paste(area_names, collapse = ", ")
  measure_codes <- c(
    all = "all",
    unemployment_rate = "unemprate",
    labor_force = "laborforce",
    employed = "emplab",
    unemployed = "unemp"
  )
  measure_labels <- c(
    all = "all",
    unemployment_rate = "ur",
    labor_force = "lf",
    employed = "employed",
    unemployed = "unemployed"
  )
  request_grid <- expand.grid(
    adjustment_index = seq_len(nrow(adjustments)),
    measure = request_measures,
    stringsAsFactors = FALSE
  )

  plan <- lapply(seq_len(nrow(request_grid)), function(i) {
    adjustment_name <- adjustments$adjustment[request_grid$adjustment_index[i]]
    adjustment_code <- adjustments$adjustment_code[request_grid$adjustment_index[i]]
    adjustment_short <- adjustments$short[request_grid$adjustment_index[i]]
    measure_name <- request_grid$measure[i]
    measure_code <- unname(measure_codes[measure_name])
    measure_label <- unname(measure_labels[measure_name])
    request <- httr2::request(endpoint) |>
      httr2::req_url_query(
        lf_measure = measure_code,
        lf_adjusted = adjustment_code,
        lf_syear = as.character(StartYear),
        lf_eyear = as.character(EndYear),
        lf_areanames = paste(area_names, collapse = ";"),
        lf_areacode = paste(area_codes, collapse = ",")
      )

    cache_key <- laus_request_key(
      scope = scope,
      area_codes = area_codes,
      adjustment_code = adjustment_code,
      start_year = StartYear,
      end_year = EndYear,
      measure = measure_code
    )
    title <- paste0(
      "LAUS_",
      data_title_key(geography_label),
      "_",
      adjustment_short,
      "_",
      measure_label,
      "_",
      StartYear,
      "-",
      EndYear
    )
    destination_path <- data_destination_path(title, DownloadDir)

    tibble::tibble(
      file_title = title,
      download_url = as.character(request$url),
      data_link_label = title,
      geography = geography_label,
      area_codes = paste(area_codes, collapse = ","),
      scope = scope,
      adjustment = adjustment_name,
      adjustment_code = adjustment_code,
      measure = measure_name,
      measure_code = measure_code,
      frequency = paste(frequencies, collapse = ","),
      start_year = as.integer(StartYear),
      end_year = as.integer(EndYear),
      rolling_monthly = isTRUE(RollingMonthly),
      cache_key = cache_key,
      destination_path = destination_path,
      download_status = laus_cache_status(
        destination_path,
        RollingMonthly = RollingMonthly,
        CacheMaxAgeDays = CacheMaxAgeDays,
        Overwrite = Overwrite
      )
    )
  }) |>
    dplyr::bind_rows()

  attr(plan, "area_lookup") <- selected_areas
  plan
}

# Return the bundled QualityInfo LAUS geography codes and years without HTTP.
# A custom page still uses the explicit dynamic discovery helpers.
OED_LAUS_Options <- function(PageUrl = "https://www.qualityinfo.org/uesti") {
  static_config <- oed_static_laus_config(PageUrl)
  if (!is.null(static_config)) {
    return(list(
      geographies = oed_static_laus_geographies() |>
        dplyr::select(.data$name, .data$code),
      years = static_config$years,
      page_url = PageUrl
    ))
  }

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
                              DownloadDir,
                              SeasonalAdjustment = "seasonally adjusted",
                              Frequency = "monthly",
                              Measures = c(
                                "unemployment_rate",
                                "labor_force",
                                "unemployed",
                                "employed"
                              ),
                              CacheMaxAgeDays = 1,
                              Overwrite = FALSE,
                              MetadataCacheDays = 7,
                              RefreshMetadata = FALSE,
                              Refresh = "auto",
                              MaxAge = NULL) {
  metadata <- laus_load_live_metadata(
    PageUrl = PageUrl,
    DownloadDir = DownloadDir,
    MetadataCacheDays = MetadataCacheDays,
    RefreshMetadata = RefreshMetadata
  )
  years <- laus_live_years(
    metadata$config,
    StartYear = StartYear,
    EndYear = EndYear,
    AvailableYears = metadata$years
  )
  plan <- laus_build_request_plan(
    config = metadata$config,
    Areas = metadata$areas,
    Geographies = Geographies,
    StartYear = years$start,
    EndYear = years$end,
    DownloadDir = DownloadDir,
    SeasonalAdjustment = SeasonalAdjustment,
    Frequency = Frequency,
    Measures = Measures,
    RollingMonthly = is.null(EndYear),
    CacheMaxAgeDays = CacheMaxAgeDays,
    Overwrite = Overwrite
  )
  plan <- laus_apply_cache_decisions(
    plan,
    Refresh = Refresh,
    Overwrite = Overwrite,
    MaxAge = MaxAge,
    CacheMaxAgeDays = CacheMaxAgeDays
  )
  attr(plan, "metadata") <- metadata
  attr(plan, "years_source") <- if (is.null(StartYear)) {
    if (identical(metadata$metadata_cache_status, "static")) {
      "bundled QualityInfo years"
    } else {
      "live available years"
    }
  } else "user supplied"
  plan
}

laus_explicit_catalog <- function(PageUrl,
                                  Urls,
                                  DownloadDir,
                                  Overwrite = FALSE,
                                  Refresh = "auto",
                                  MaxAge = NULL,
                                  CacheMaxAgeDays = 1) {
  plan <- data_page_workbook_catalog(PageUrl = PageUrl, Urls = Urls) |>
    dplyr::mutate(
      geography = NA_character_,
      area_codes = NA_character_,
      scope = "explicit_url",
      adjustment = NA_character_,
      adjustment_code = NA_character_,
      measure = "all",
      start_year = NA_integer_,
      end_year = NA_integer_,
      rolling_monthly = FALSE,
      cache_key = paste0("explicit_url|", .data$download_url),
      destination_path = vapply(
        .data$file_title,
        data_destination_path,
        character(1),
        DownloadDir = DownloadDir
      ),
      download_status = "downloaded"
    )
  laus_apply_cache_decisions(
    plan,
    Refresh = Refresh,
    Overwrite = Overwrite,
    MaxAge = MaxAge,
    CacheMaxAgeDays = CacheMaxAgeDays
  )
}

laus_workbook_sidecar_path <- function(path) {
  paste0(path, ".metadata.rds")
}

laus_read_area_sidecars <- function(Paths) {
  sidecars <- vapply(Paths, laus_workbook_sidecar_path, character(1))
  sidecars <- sidecars[file.exists(sidecars)]
  if (length(sidecars) == 0) return(NULL)

  metadata <- lapply(sidecars, function(path) {
    tryCatch(readRDS(path), error = function(error) NULL)
  })
  metadata <- Filter(Negate(is.null), metadata)
  lookups <- lapply(metadata, function(item) item$area_lookup)
  lookups <- Filter(function(x) !is.null(x) && nrow(x) > 0, lookups)
  if (length(lookups) == 0) NULL else dplyr::bind_rows(lookups) |> dplyr::distinct()
}

laus_write_workbook_sidecar <- function(path, AreaLookup, Request = NULL) {
  if (is.null(AreaLookup) || nrow(AreaLookup) == 0) return(invisible(NULL))

  saveRDS(
    list(
      area_lookup = AreaLookup,
      request = Request,
      saved_at = Sys.time()
    ),
    laus_workbook_sidecar_path(path)
  )
  invisible(NULL)
}

laus_attach_laus_diagnostics <- function(data,
                                         plan,
                                         metadata = NULL,
                                         parsing = NULL) {
  diagnostics <- attr(data, "download_diagnostics")
  diagnostics <- data_or(diagnostics, list())
  diagnostics$planned_requests <- nrow(plan)
  diagnostics$uncached_requests <- sum(plan$download_status != "cached", na.rm = TRUE)
  diagnostics$cache_hits <- sum(plan$download_status == "cached", na.rm = TRUE)
  diagnostics$metadata_cache_status <- if (is.null(metadata)) {
    "not used"
  } else {
    metadata$metadata_cache_status
  }
  diagnostics$metadata_cache_path <- if (is.null(metadata)) {
    NA_character_
  } else {
    metadata$metadata_cache_path
  }
  diagnostics$parsing <- parsing
  attr(data, "download_diagnostics") <- diagnostics
  attr(data, "laus_diagnostics") <- list(
    planned_requests = nrow(plan),
    uncached_requests = sum(plan$download_status != "cached", na.rm = TRUE),
    cache_hits = sum(plan$download_status == "cached", na.rm = TRUE),
    metadata_requests = if (
      is.null(metadata) || metadata$metadata_cache_status %in% c("cached", "static")
    ) 0L else 3L,
    parsing = parsing
  )
  data
}

# Public LAUS loader. Paths reads local workbooks; Urls accepts explicit
# workbook links; otherwise the function builds a live portlet request.
OED_LAUS_Table <- function(Geographies = NULL,
                           Paths = NULL,
                           Urls = NULL,
                           DownloadDir = NULL,
                           Overwrite = FALSE,
                           PreviewOnly = FALSE,
                           MaxDownloads = 10,
                           Sheets = NULL,
                           PageUrl = "https://www.qualityinfo.org/uesti",
                           StartYear = NULL,
                           EndYear = NULL,
                           CacheMaxAgeDays = 1,
                           ThrottleSeconds = 0.25,
                           MetadataCacheDays = 7,
                           RefreshMetadata = FALSE,
                           AreaLookup = NULL,
                           SeasonalAdjustment = "seasonally adjusted",
                           Frequency = "monthly",
                           Measures = c(
                             "unemployment_rate",
                             "labor_force",
                             "unemployed",
                             "employed"
                           ),
                           metadata = FALSE,
                           Refresh = c("auto", "always", "never"),
                           MaxAge = NULL) {
  Refresh <- oed_refresh_policy(Refresh, Overwrite = Overwrite)
  DownloadDir <- oed_dataset_download_dir("LAUS", DownloadDir)
  if (!is.null(Paths)) {
    if (isTRUE(PreviewOnly)) {
      return(tibble::tibble(
        command = "OED_LAUS_Table",
        path = as.character(Paths),
        exists = file.exists(Paths)
      ))
    }
    area_lookup <- data_or(AreaLookup, laus_read_area_sidecars(Paths))
    raw_result <- laus_read_workbooks(
      Paths = Paths,
      Sheets = Sheets,
      Geographies = Geographies,
      AreaLookup = area_lookup,
      SeasonalAdjustment = SeasonalAdjustment,
      Frequency = Frequency
    )
    result <- laus_compact_table(
      raw_result,
      SeasonalAdjustment = SeasonalAdjustment,
      Frequency = Frequency,
      Measures = Measures,
      Metadata = metadata
    )
    plan <- tibble::tibble(
      command = "OED_LAUS_Table",
      path = as.character(Paths),
      download_status = "cached"
    )
    result <- oed_attach_download_diagnostics(
      result,
      command = "OED_LAUS_Table",
      plan = plan
    )
    return(laus_attach_laus_diagnostics(
      result,
      plan = plan,
      parsing = list(
        workbooks = length(Paths),
        rows = nrow(raw_result),
        area_codes_recovered = sum(!is.na(raw_result$area_code)),
        frequencies = unique(raw_result$frequency),
        seasonal_adjustments = unique(raw_result$seasonal_adjustment)
      )
    ))
  }

  if (isTRUE(PreviewOnly) && is.null(Urls) &&
      (isTRUE(RefreshMetadata) || is.null(oed_static_laus_config(PageUrl)))) {
    stop(
      "PreviewOnly cannot discover a custom or refreshed LAUS endpoint. Use " ,
      "the standard PageUrl with bundled metadata, or provide explicit Urls.",
      call. = FALSE
    )
  }

  selected <- if (!is.null(Urls)) {
    laus_explicit_catalog(
      PageUrl = PageUrl,
      Urls = Urls,
      DownloadDir = DownloadDir,
      Overwrite = Overwrite,
      Refresh = Refresh,
      MaxAge = MaxAge,
      CacheMaxAgeDays = CacheMaxAgeDays
    )
  } else {
    laus_live_catalog(
      PageUrl = PageUrl,
      Geographies = Geographies,
      StartYear = StartYear,
      EndYear = EndYear,
      DownloadDir = DownloadDir,
      SeasonalAdjustment = SeasonalAdjustment,
      Frequency = Frequency,
      Measures = Measures,
      CacheMaxAgeDays = CacheMaxAgeDays,
      Overwrite = Overwrite,
      MetadataCacheDays = MetadataCacheDays,
      RefreshMetadata = RefreshMetadata,
      Refresh = Refresh,
      MaxAge = MaxAge
    )
  }
  live_metadata <- attr(selected, "metadata")
  area_lookup <- if (is.null(Urls)) {
    attr(selected, "area_lookup")
  } else {
    NULL
  }
  selected$command <- "OED_LAUS_Table"
  selected$page_url <- PageUrl
  selected <- dplyr::relocate(selected, .data$command, .data$page_url)

  if (isTRUE(PreviewOnly)) return(selected)

  needs_download <- selected$download_status == "downloaded"
  if (is.finite(MaxDownloads) && sum(needs_download) > MaxDownloads) {
    stop(
      "OED_LAUS_Table would download ", sum(needs_download),
      " QualityInfo Excel files. Provide fewer Urls, set PreviewOnly = TRUE, ",
      "or raise MaxDownloads.",
      call. = FALSE
    )
  }

  paths <- selected$destination_path
  failed_requests <- tibble::tibble(
    file_title = character(),
    download_url = character(),
    reason = character()
  )
  last_download <- NULL
  for (i in which(needs_download)) {
    if (!is.null(last_download) && ThrottleSeconds > 0) {
      elapsed <- as.numeric(difftime(Sys.time(), last_download, units = "secs"))
      if (elapsed < ThrottleSeconds) Sys.sleep(ThrottleSeconds - elapsed)
    }
    planned_reason <- selected$refresh_reason[i]
    outcome <- data_download_one(
      selected$download_url[i],
      selected$destination_path[i],
      Overwrite = Overwrite,
      DataUrl = PageUrl,
      Command = "OED_LAUS_Table",
      Dataset = "LAUS",
      # Planning already decided that this workbook must be fetched. Force
      # the execution pass so a changed revision or period cannot be lost.
      Refresh = "always",
      MaxAge = if (is.null(MaxAge)) CacheMaxAgeDays else MaxAge,
      RequestParameters = as.list(selected[i, , drop = FALSE]),
      LatestAvailablePeriod = if (isTRUE(selected$rolling_monthly[i])) {
        as.character(selected$end_year[i])
      } else {
        NULL
      },
      CatalogTitle = selected$file_title[i],
      CatalogUrl = selected$download_url[i]
    )
    selected$download_status[i] <- outcome$status
    selected$refresh_reason[i] <- if (outcome$status %in% c("downloaded", "refreshed")) {
      planned_reason
    } else {
      outcome$reason
    }
    selected$cache_age_days[i] <- outcome$cache_age_days
    selected$metadata_path[i] <- outcome$metadata_path
    if (identical(outcome$status, "failed")) {
      paths[i] <- NA_character_
      failed_requests <- dplyr::bind_rows(
        failed_requests,
        tibble::tibble(
          file_title = selected$file_title[i],
          download_url = selected$download_url[i],
          reason = outcome$error
        )
      )
    } else {
      paths[i] <- outcome$path
    }
    last_download <- Sys.time()
  }
  downloaded <- dplyr::mutate(selected, path = paths)
  readable_indices <- which(!is.na(paths) & file.exists(paths))
  if (length(readable_indices) == 0) {
    stop(
      "No valid LAUS workbooks were available after download attempts.",
      call. = FALSE
    )
  }
  if (!is.null(area_lookup)) {
    for (i in readable_indices) {
      laus_write_workbook_sidecar(
        downloaded$path[i],
        AreaLookup = area_lookup,
        Request = downloaded[i, , drop = FALSE]
      )
    }
  }
  read_geographies <- Geographies
  raw_result <- laus_read_workbooks(
    Paths = downloaded$path[readable_indices],
    Sheets = Sheets,
    Geographies = read_geographies,
    AreaLookup = area_lookup,
    SeasonalAdjustment = SeasonalAdjustment,
    Frequency = Frequency
  )
  result <- laus_compact_table(
    raw_result,
    SeasonalAdjustment = SeasonalAdjustment,
    Frequency = Frequency,
    Measures = Measures,
    Metadata = metadata
  )
  result <- oed_attach_download_diagnostics(
    result,
    command = "OED_LAUS_Table",
    plan = downloaded,
    failed_requests = failed_requests
  )
  laus_attach_laus_diagnostics(
    result,
    plan = downloaded,
    metadata = live_metadata,
    parsing = list(
      workbooks = length(unique(downloaded$path)),
      rows = nrow(raw_result),
      geographies = dplyr::n_distinct(raw_result$geography),
      area_codes_recovered = sum(!is.na(raw_result$area_code)),
      frequencies = sort(unique(raw_result$frequency)),
      seasonal_adjustments = sort(unique(raw_result$seasonal_adjustment)),
      latest_month = if (any(raw_result$frequency == "monthly")) {
        max(raw_result$date[raw_result$frequency == "monthly"], na.rm = TRUE)
      } else {
        as.Date(NA)
      }
    )
  )
}
