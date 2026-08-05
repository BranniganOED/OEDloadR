# Shared data-root, cache, refresh, and safe workbook replacement helpers.
#
# Loaders use these helpers so that path resolution, freshness decisions,
# metadata sidecars, and diagnostics behave consistently across datasets.

oed_xlsx_valid <- function(path) {
  if (!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size < 4) {
    return(FALSE)
  }
  if (!identical(readBin(path, what = "raw", n = 2), charToRaw("PK"))) {
    return(FALSE)
  }

  entries <- tryCatch(
    utils::unzip(path, list = TRUE)$Name,
    error = function(error) character()
  )
  any(entries == "[Content_Types].xml") && any(entries == "xl/workbook.xml")
}

oed_data_root <- function() {
  configured <- getOption("OEDloadR.data_dir", NULL)
  if (is.null(configured) || !nzchar(stringr::str_squish(as.character(configured)))) {
    configured <- Sys.getenv("OEDLOADR_DATA_DIR", unset = "")
  }
  if (is.null(configured) || !nzchar(stringr::str_squish(as.character(configured)))) {
    configured <- file.path(getwd(), "OEDloadR data")
  }

  normalizePath(as.character(configured)[1], mustWork = FALSE)
}

oed_dataset_download_dir <- function(dataset, DownloadDir = NULL) {
  if (!is.null(DownloadDir) && length(DownloadDir) > 0 &&
      nzchar(stringr::str_squish(as.character(DownloadDir)[1]))) {
    return(as.character(DownloadDir)[1])
  }

  dataset <- match.arg(
    dataset,
    c("QCEW", "LAUS", "Data", "Industry Profiles", "Businesses")
  )
  file.path(oed_data_root(), dataset)
}

oed_refresh_policy <- function(Refresh = c("auto", "always", "never"), Overwrite = FALSE) {
  if (isTRUE(Overwrite)) return("always")
  match.arg(Refresh)
}

oed_cache_max_age <- function(dataset,
                              MaxAge = NULL,
                              current_request = TRUE) {
  if (!is.null(MaxAge)) {
    if (length(MaxAge) != 1 || is.na(MaxAge) || !is.numeric(MaxAge) || MaxAge < 0) {
      stop("MaxAge must be NULL or one non-negative number of days.", call. = FALSE)
    }
    return(as.numeric(MaxAge))
  }

  dataset <- match.arg(
    dataset,
    c("QCEW", "LAUS", "Data", "Industry Profiles", "Businesses")
  )
  if (dataset == "QCEW" && !isTRUE(current_request)) return(90)
  if (dataset == "QCEW") return(7)
  if (dataset == "LAUS") return(1)
  30
}

oed_cache_sidecar_path <- function(path) {
  paste0(path, ".metadata.json")
}

oed_cache_age_days <- function(path, now = Sys.time()) {
  if (!file.exists(path)) return(NA_real_)
  as.numeric(difftime(now, file.info(path)$mtime, units = "days"))
}

oed_cache_valid <- function(path, validator = data_is_xlsx) {
  isTRUE(file.exists(path)) && isTRUE(tryCatch(
    validator(path),
    error = function(error) FALSE
  ))
}

oed_cache_read_metadata <- function(path) {
  sidecar <- oed_cache_sidecar_path(path)
  if (!file.exists(sidecar)) return(NULL)

  tryCatch(
    jsonlite::fromJSON(sidecar, simplifyVector = FALSE),
    error = function(error) NULL
  )
}

oed_cache_hash <- function(path) {
  unname(as.character(tools::md5sum(path)[[1]]))
}

oed_cache_package_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("OEDloadR")),
    error = function(error) NA_character_
  )
}

oed_cache_scalar <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  value <- as.character(x[[1]])
  if (length(value) == 0 || is.na(value) || !nzchar(value)) return(NULL)
  value
}

oed_cache_period_newer <- function(current, cached) {
  current <- oed_cache_scalar(current)
  cached <- oed_cache_scalar(cached)
  if (is.null(current) || is.null(cached) || identical(current, cached)) return(FALSE)

  current_number <- suppressWarnings(as.numeric(current))
  cached_number <- suppressWarnings(as.numeric(cached))
  if (!is.na(current_number) && !is.na(cached_number)) {
    return(current_number > cached_number)
  }

  current_date <- suppressWarnings(as.Date(current))
  cached_date <- suppressWarnings(as.Date(cached))
  if (!is.na(current_date) && !is.na(cached_date)) {
    return(current_date > cached_date)
  }

  FALSE
}

oed_cache_normalize_parameters <- function(parameters) {
  if (is.null(parameters)) return(list())
  if (is.data.frame(parameters)) {
    if (nrow(parameters) == 0) return(list())
    parameters <- as.list(parameters[1, , drop = FALSE])
  }
  if (!is.list(parameters)) return(list(value = as.character(parameters)))

  lapply(parameters, function(value) {
    if (length(value) <= 1) return(as.character(value))
    as.character(value)
  })
}

oed_cache_decision <- function(path,
                               command,
                               dataset,
                               source_url = NULL,
                               request_parameters = NULL,
                               Refresh = c("auto", "always", "never"),
                               Overwrite = FALSE,
                               MaxAge = NULL,
                               current_etag = NULL,
                               current_last_modified = NULL,
                               latest_available_period = NULL,
                               catalog_title = NULL,
                               catalog_url = NULL,
                               validator = data_is_xlsx,
                               now = Sys.time()) {
  policy <- oed_refresh_policy(Refresh, Overwrite = Overwrite)
  valid <- oed_cache_valid(path, validator = validator)
  metadata <- oed_cache_read_metadata(path)
  age <- oed_cache_age_days(path, now = now)
  max_age <- oed_cache_max_age(dataset, MaxAge = MaxAge)
  metadata_path <- oed_cache_sidecar_path(path)

  base <- list(
    status = if (valid) "cached" else "downloaded",
    reason = if (valid) "freshness_check_pending" else if (file.exists(path)) "invalid_workbook" else "missing_workbook",
    cache_age_days = age,
    max_age_days = max_age,
    metadata_path = metadata_path,
    metadata = metadata,
    existing_valid = valid,
    policy = policy,
    source_url = source_url,
    request_parameters = oed_cache_normalize_parameters(request_parameters)
  )

  if (!valid) return(base)
  if (policy == "always") {
    base$status <- "downloaded"
    base$reason <- if (isTRUE(Overwrite)) "overwrite" else "refresh_always"
    return(base)
  }
  if (policy == "never") {
    base$reason <- "refresh_never"
    return(base)
  }

  current_etag_value <- oed_cache_scalar(current_etag)
  cached_etag_value <- oed_cache_scalar(metadata$etag)
  if (!is.null(current_etag_value) && !is.null(cached_etag_value) &&
      !identical(current_etag_value, cached_etag_value)) {
    base$status <- "downloaded"
    base$reason <- "etag_changed"
    return(base)
  }
  current_last_modified_value <- oed_cache_scalar(current_last_modified)
  cached_last_modified_value <- oed_cache_scalar(metadata$last_modified)
  if (!is.null(current_last_modified_value) && !is.null(cached_last_modified_value) &&
      !identical(current_last_modified_value, cached_last_modified_value)) {
    base$status <- "downloaded"
    base$reason <- "last_modified_changed"
    return(base)
  }
  if (oed_cache_period_newer(latest_available_period, metadata$latest_available_period)) {
    base$status <- "downloaded"
    base$reason <- "latest_period_changed"
    return(base)
  }
  if (!is.null(catalog_title) && !is.null(metadata$catalog_title) &&
      !identical(oed_cache_scalar(catalog_title), oed_cache_scalar(metadata$catalog_title))) {
    base$status <- "downloaded"
    base$reason <- "catalog_title_changed"
    return(base)
  }
  if (!is.null(catalog_url) && !is.null(metadata$catalog_url) &&
      !identical(oed_cache_scalar(catalog_url), oed_cache_scalar(metadata$catalog_url))) {
    base$status <- "downloaded"
    base$reason <- "catalog_url_changed"
    return(base)
  }
  if (!is.na(age) && age > max_age) {
    base$status <- "downloaded"
    base$reason <- "max_age_exceeded"
    return(base)
  }

  base$reason <- "within_max_age"
  base
}

oed_cache_response_header <- function(response, name) {
  headers <- tryCatch(httr2::resp_headers(response), error = function(error) NULL)
  if (is.null(headers)) return(NULL)
  hit <- headers[tolower(names(headers)) == tolower(name)]
  if (length(hit) == 0) NULL else as.character(hit[[1]])
}

oed_cache_atomic_replace <- function(temp_path, destination_path) {
  backup_path <- NULL
  if (file.exists(destination_path)) {
    backup_path <- tempfile(
      pattern = paste0(".", basename(destination_path), ".backup-"),
      tmpdir = dirname(destination_path),
      fileext = ".xlsx"
    )
    if (!file.rename(destination_path, backup_path)) {
      stop("Could not move the existing workbook to a temporary backup.", call. = FALSE)
    }
  }

  replaced <- file.rename(temp_path, destination_path)
  if (!replaced) {
    if (!is.null(backup_path)) file.rename(backup_path, destination_path)
    stop("Could not atomically replace the workbook.", call. = FALSE)
  }
  if (!is.null(backup_path)) unlink(backup_path)
  invisible(destination_path)
}

oed_cache_write_metadata <- function(path,
                                     command,
                                     dataset,
                                     source_url,
                                     request_parameters,
                                     response = NULL,
                                     latest_available_period = NULL,
                                     catalog_title = NULL,
                                     catalog_url = NULL) {
  info <- file.info(path)
  metadata <- list(
    command = command,
    dataset = dataset,
    source_url = source_url,
    request_parameters = oed_cache_normalize_parameters(request_parameters),
    destination_path = normalizePath(path, mustWork = FALSE),
    downloaded_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    package_version = oed_cache_package_version(),
    etag = if (is.null(response)) NULL else oed_cache_response_header(response, "etag"),
    last_modified = if (is.null(response)) NULL else oed_cache_response_header(response, "last-modified"),
    file_size = unname(info$size),
    hash = oed_cache_hash(path),
    latest_available_period = oed_cache_scalar(latest_available_period),
    catalog_title = oed_cache_scalar(catalog_title),
    catalog_url = oed_cache_scalar(catalog_url)
  )
  jsonlite::write_json(
    metadata,
    path = oed_cache_sidecar_path(path),
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )
  invisible(metadata)
}

oed_cache_download <- function(request,
                               destination_path,
                               command,
                               dataset,
                               source_url,
                               request_parameters = NULL,
                               Refresh = c("auto", "always", "never"),
                               Overwrite = FALSE,
                               MaxAge = NULL,
                               current_etag = NULL,
                               current_last_modified = NULL,
                               latest_available_period = NULL,
                               catalog_title = NULL,
                               catalog_url = NULL,
                               validator = data_is_xlsx,
                               DataUrl = NULL,
                               perform = oed_request_perform) {
  dir.create(dirname(destination_path), recursive = TRUE, showWarnings = FALSE)
  decision <- oed_cache_decision(
    path = destination_path,
    command = command,
    dataset = dataset,
    source_url = source_url,
    request_parameters = request_parameters,
    Refresh = Refresh,
    Overwrite = Overwrite,
    MaxAge = MaxAge,
    current_etag = current_etag,
    current_last_modified = current_last_modified,
    latest_available_period = latest_available_period,
    catalog_title = catalog_title,
    catalog_url = catalog_url,
    validator = validator
  )
  if (decision$status == "cached") {
    decision$path <- destination_path
    return(decision)
  }

  temp_path <- tempfile(
    pattern = paste0(".", basename(destination_path), ".download-"),
    tmpdir = dirname(destination_path),
    fileext = ".xlsx"
  )
  on.exit(unlink(temp_path), add = TRUE)

  tryCatch({
    if (!is.null(DataUrl)) {
      request <- request |>
        httr2::req_headers(
          Referer = DataUrl,
          Accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet,*/*"
        )
    }
    request <- request |> httr2::req_user_agent("Mozilla/5.0")
    response <- perform(request, path = temp_path)
    if (!oed_cache_valid(temp_path, validator = validator)) {
      stop("QualityInfo did not return a valid XLSX workbook.", call. = FALSE)
    }

    oed_cache_atomic_replace(temp_path, destination_path)
    oed_cache_write_metadata(
      path = destination_path,
      command = command,
      dataset = dataset,
      source_url = source_url,
      request_parameters = request_parameters,
      response = response,
      latest_available_period = latest_available_period,
      catalog_title = catalog_title,
      catalog_url = catalog_url
    )
    decision$path <- destination_path
    decision$status <- if (isTRUE(decision$existing_valid)) "refreshed" else "downloaded"
    decision$metadata <- oed_cache_read_metadata(destination_path)
    decision
  }, error = function(error) {
    if (isTRUE(decision$existing_valid) && oed_cache_valid(destination_path, validator)) {
      warning(
        "Could not refresh ", basename(destination_path), "; using stale cached data. ",
        conditionMessage(error),
        call. = FALSE
      )
      decision$path <- destination_path
      decision$status <- "stale_cache_used"
      decision$reason <- "refresh_failed"
      decision$error <- conditionMessage(error)
      return(decision)
    }

    decision$path <- NA_character_
    decision$status <- "failed"
    decision$reason <- "download_failed"
    decision$error <- conditionMessage(error)
    decision
  })
}

oed_cache_plan_columns <- function(plan) {
  if (!"cache_age_days" %in% names(plan)) plan$cache_age_days <- NA_real_
  if (!"cache_max_age_days" %in% names(plan)) plan$cache_max_age_days <- NA_real_
  if (!"refresh_reason" %in% names(plan)) plan$refresh_reason <- NA_character_
  if (!"metadata_path" %in% names(plan)) plan$metadata_path <- NA_character_
  plan
}
