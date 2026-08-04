# Shared helpers for QualityInfo's static workbook catalogs and downloads.
#
# OED_Data selects workbooks from the public /data page, downloads them with
# caching and overwrite controls, and optionally performs generic spreadsheet
# cleanup. OED_IProfile and OED_Businesses reuse the same workflow for pages
# that expose direct workbook links.

data_or <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }

  x
}

data_clean_names <- function(x) {
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

data_is_xlsx <- function(path) {
  if (!file.exists(path) || file.info(path)$size < 4) {
    return(FALSE)
  }

  identical(readBin(path, what = "raw", n = 2), charToRaw("PK"))
}

# QualityInfo occasionally responds slowly or transiently with a server error.
# Retry idempotent page, service, and workbook requests a small number of times
# while leaving the source-specific request construction to each loader.
oed_request_perform <- function(req, path = NULL, max_tries = 3L) {
  req <- httr2::req_retry(req, max_tries = max_tries)

  if (is.null(path)) {
    return(httr2::req_perform(req))
  }

  httr2::req_perform(req, path = path)
}

oed_download_diagnostics <- function(command, plan, failed_requests = NULL) {
  status <- if ("download_status" %in% names(plan)) {
    as.character(plan$download_status)
  } else {
    character()
  }

  list(
    command = command,
    files_requested = nrow(plan),
    files_downloaded = sum(status == "downloaded", na.rm = TRUE),
    files_reused = sum(status == "cached", na.rm = TRUE),
    files_failed = if (is.null(failed_requests)) 0L else nrow(failed_requests),
    failed_requests = failed_requests,
    generated_at = Sys.time()
  )
}

oed_attach_download_diagnostics <- function(data,
                                            command,
                                            plan,
                                            failed_requests = NULL) {
  attr(data, "download_plan") <- plan
  attr(data, "download_diagnostics") <- oed_download_diagnostics(
    command = command,
    plan = plan,
    failed_requests = failed_requests
  )
  data
}

# Extract the common diagnostics list without requiring users to know the
# attribute name used on a loader result.
OED_Diagnostics <- function(x) {
  attr(x, "download_diagnostics")
}

data_title_key <- function(x) {
  x |>
    as.character() |>
    str_replace_all("&amp;", " and ") |>
    str_replace_all("&", " and ") |>
    stringr::str_to_lower() |>
    str_replace_all("\\.xlsx$", "") |>
    str_replace_all("[^a-z0-9]+", "") |>
    stringr::str_replace("xlsx$", "")
}

data_html_decode <- function(x) {
  x |>
    str_replace_all("&amp;", "&") |>
    str_replace_all("&nbsp;", " ") |>
    str_replace_all("&#39;", "'") |>
    str_replace_all("&quot;", "\"") |>
    str_replace_all("\\s+", " ") |>
    stringr::str_squish()
}

oed_data_category_lookup <- function() {
  tibble::tibble(
    category_key = c(
      "industry_projections",
      "occupational_wage_information",
      "agricultural_employment",
      "occupational_projections",
      "occupations_in_demand",
      "high_wage_high_demand_high_skill",
      "stem_occupations"
    ),
    category = c(
      "Industry Employment Projections",
      "Occupational Wage Information",
      "Oregon Agricultural Employment",
      "Occupational Employment Projections",
      "Occupations in Demand",
      "High-Wage High-Demand High-Skill Occupations",
      "STEM occupations"
    ),
    aliases = list(
      c("industry projections", "industry employment projections"),
      c("wage information", "occupational wage information", "occupation wage information"),
      c("agricultural employment", "oregon agricultural employment", "ag employment"),
      c("occupational projections", "occupational employment projections"),
      c("occupations in demand", "demand occupations"),
      c("high wage high demand high skill", "hwhdhs", "hwhd", "high demand high wage high skill"),
      c("stem", "stem occupations", "stem employment projections")
    )
  )
}

# These aliases keep the public interface tolerant of the geography labels
# used in different QualityInfo workbook releases.
oed_data_geography_aliases <- function() {
  tibble::tibble(
    geography = c(
      "Oregon",
      "Clackamas",
      "East Cascades",
      "Eastern Oregon",
      "Lane",
      "Mid-Valley",
      "Northwest Oregon",
      "Portland Tri-County",
      "Portland-Metro",
      "Rogue Valley",
      "Southwestern Oregon"
    ),
    aliases = list(
      c("Oregon", "Statewide"),
      c("Clackamas", "Clackamas County"),
      c("East Cascades"),
      c("Eastern Oregon"),
      c("Lane", "Lane County"),
      c("Mid-Valley", "Mid Valley"),
      c("Northwest Oregon", "Northwest"),
      c("Portland Tri-County", "Portland-Metro", "Portland Metro", "PMSA"),
      c("Portland-Metro", "Portland Metro", "Portland Tri-County", "PMSA"),
      c("Rogue Valley"),
      c("Southwestern Oregon", "Southwest Oregon")
    )
  )
}

# The /data catalog is based on workbook titles rather than stable file IDs.
# Keep the current titles here so callers can select files by human-readable
# category and geography names.
oed_data_projection_titles <- function() {
  tibble::tibble(
    geography = c(
      "Oregon",
      "East Cascades",
      "Eastern Oregon",
      "Lane",
      "Mid-Valley",
      "Northwest Oregon",
      "Portland Tri-County",
      "Rogue Valley",
      "Southwestern Oregon"
    ),
    industry_title = c(
      "Oregon Industry Employment Projections 2024-2034",
      "East Cascades Industry Employment Projections 2024-2034",
      "Eastern Oregon Industry Employment Projections, 2024-2034",
      "Lane Industry Employment Projections 2024-2034",
      "Mid-Valley Industry Employment Projections 2024-2034",
      "Northwest Oregon Industry Employment Projections 2024-2034",
      "Portland Tri-County Industry Projections 2024-2034",
      "Rogue Valley Industry Employment Projections 2024-2034",
      "Southwestern Oregon Industry Employment Projections 2024-2034"
    ),
    occupational_title = c(
      "Oregon Occupational Employment Projections 2024-2034",
      "East Cascades Occupational Employment Projections 2024-2034",
      "Eastern Oregon Occupational Employment Projections 2024-2034",
      "Lane Occupational Employment Projections 2024-2034",
      "Mid-Valley Occupational Employment Projections 2024-2034",
      "Northwest Oregon Occupational Employment Projections 2024-2034",
      "Portland Tri-County Occupational Projections 2024-2034",
      "Rogue Valley Occupational Employment Projections 2024-2034",
      "Southwestern Oregon Occupational Employment Projections 2024-2034"
    ),
    demand_title = c(
      "Oregon Occupations in Demand 2024-2034",
      "East Cascades Occupations in Demand 2024-2034",
      "Eastern Oregon Occupations in Demand 2024-2034",
      "Lane Occupations in Demand 2024-2034",
      "Mid-Valley Occupations in Demand 2024-2034",
      "Northwest Oregon Occupations in Demand 2024-2034",
      "Portland Tri-County Occupations in Demand 2024-2034",
      "Rogue Valley Occupations in Demand 2024-2034",
      "Southwestern Oregon Occupations in Demand 2024-2034"
    ),
    hwhdhs_title = c(
      "Oregon High-Wage, High-Demand, High-Skill Occupations 2024-2034",
      "East Cascades High-Wage, High-Demand, High-Skill Occupations 2024-2034",
      "Eastern Oregon High-Demand, High-Wage, High-Skill Occupations 2024-2034",
      "Lane High-Wage, High-Demand, High-Skill Occupations 2024-2034",
      "Mid-Valley High-Wage, High-Demand, High-Skill Occupations 2024-2034",
      "Northwest Oregon High-Wage, High-Demand, High-Skill Occupations 2024-2034",
      "Portland Tri-County High-Wage, High-Demand, High-Skill Occupations 2024-2034",
      "Rogue Valley High-Wage, High-Demand, High-Skill Occupations 2024-2034",
      "Southwestern Oregon High-Wage, High-Demand, High-Skill Occupations 2024-2034"
    ),
    stem_title = c(
      "Oregon STEM Employment Projections and Wages by Detailed Occupation 2023-2033",
      "East Cascades STEM Employment Projections and Wages by Detailed Occupation 2023-2033",
      "Eastern Oregon STEM Employment Projections and Wages by Detailed Occupation 2023-2033",
      "Lane STEM Employment Projections and Wages by Detailed Occupation 2023-2033",
      "Mid-Valley STEM Employment Projections and Wages by Detailed Occupation 2023-2033",
      "Northwest STEM Employment Projections and Wages by Detailed Occupation 2023-2033",
      "Portland Tri-County STEM Employment Projections and Wages by Detailed Occupation 2023-2033",
      "Rogue Valley STEM Employment Projections and Wages by Detailed Occupation 2023-2033",
      "Southwestern Oregon STEM Employment Projections and Wages by Detailed Occupation 2023-2033"
    )
  )
}

oed_data_catalog <- function() {
  categories <- oed_data_category_lookup() |>
    dplyr::select(.data$category_key, .data$category)

  projection_titles <- oed_data_projection_titles()
  wage_geographies <- c(
    "Oregon",
    "Clackamas",
    "East Cascades",
    "Eastern Oregon",
    "Lane",
    "Mid-Valley",
    "Northwest Oregon",
    "Portland-Metro",
    "Rogue Valley",
    "Southwestern Oregon"
  )
  wage_titles <- tibble::tibble(
    geography = wage_geographies,
    file_title = paste0(wage_geographies, " Wage Information")
  )

  bind_rows(
    projection_titles |>
      dplyr::transmute(
        category_key = "industry_projections",
        geography = .data$geography,
        file_title = .data$industry_title
      ),
    wage_titles |>
      dplyr::transmute(
        category_key = "occupational_wage_information",
        geography = .data$geography,
        file_title = .data$file_title
      ),
    tibble::tibble(
      category_key = "agricultural_employment",
      geography = "Oregon",
      file_title = "Oregon Agricultural Employment"
    ),
    projection_titles |>
      dplyr::transmute(
        category_key = "occupational_projections",
        geography = .data$geography,
        file_title = .data$occupational_title
      ),
    projection_titles |>
      dplyr::transmute(
        category_key = "occupations_in_demand",
        geography = .data$geography,
        file_title = .data$demand_title
      ),
    projection_titles |>
      dplyr::transmute(
        category_key = "high_wage_high_demand_high_skill",
        geography = .data$geography,
        file_title = .data$hwhdhs_title
      ),
    projection_titles |>
      dplyr::transmute(
        category_key = "stem_occupations",
        geography = .data$geography,
        file_title = .data$stem_title
      )
  ) |>
    dplyr::left_join(categories, by = "category_key") |>
    mutate(file_title_key = data_title_key(.data$file_title)) |>
    dplyr::select(
      .data$category_key,
      .data$category,
      .data$geography,
      .data$file_title,
      .data$file_title_key
    ) |>
    arrange(.data$category, .data$geography)
}

data_resolve_categories <- function(Category) {
  if (is.null(Category)) {
    return(oed_data_category_lookup()$category_key)
  }

  lookup <- oed_data_category_lookup()

  resolved <- vapply(Category, function(value) {
    key <- data_title_key(value)
    hits <- lookup$category_key[vapply(seq_len(nrow(lookup)), function(i) {
      key %in% data_title_key(c(lookup$category_key[i], lookup$category[i], lookup$aliases[[i]]))
    }, logical(1))]

    if (length(hits) == 0) {
      stop(
        "No matching OED_Data category for: ", value,
        ". Use OED_Data(List = TRUE) to see available categories.",
        call. = FALSE
      )
    }

    hits[1]
  }, character(1))

  unique(resolved)
}

data_resolve_geographies <- function(Geographies) {
  if (is.null(Geographies)) {
    return(oed_data_geography_aliases()$geography)
  }

  lookup <- oed_data_geography_aliases()

  resolved <- lapply(Geographies, function(value) {
    key <- data_title_key(value)
    hits <- lookup$geography[vapply(seq_len(nrow(lookup)), function(i) {
      key %in% data_title_key(c(lookup$geography[i], lookup$aliases[[i]]))
    }, logical(1))]

    if (length(hits) == 0) {
      stop(
        "No matching OED_Data geography for: ", value,
        ". Use OED_Data(List = TRUE) to see available geographies.",
        call. = FALSE
      )
    }

    hits
  })

  unique(unlist(resolved, use.names = FALSE))
}

data_select_catalog <- function(catalog, Category = NULL, Geographies = NULL) {
  category_keys <- data_resolve_categories(Category)
  geography_keys <- data_resolve_geographies(Geographies)

  selected <- catalog |>
    filter(
      .data$category_key %in% category_keys,
      .data$geography %in% geography_keys
    )

  if (nrow(selected) == 0) {
    stop(
      "No OED_Data files match the requested Category and Geographies. ",
      "Use OED_Data(List = TRUE) to inspect available combinations.",
      call. = FALSE
    )
  }

  selected |>
    arrange(.data$category, .data$geography)
}

data_url_absolute <- function(href, base_url) {
  href <- stringr::str_squish(as.character(href))
  href <- data_html_decode(href)

  if (stringr::str_detect(href, "^https?://")) {
    url <- href
  } else if (stringr::str_starts(href, "//")) {
    url <- paste0("https:", href)
  } else {
    base_root <- stringr::str_match(
      base_url,
      "^(https?://[^/]+)"
    )[, 2]

    if (stringr::str_starts(href, "/")) {
      url <- paste0(base_root, href)
    } else {
      base_dir <- stringr::str_replace(
        base_url,
        "/[^/]*$",
        "/"
      )

      url <- paste0(base_dir, href)
    }
  }

  unname(gsub(" ", "%20", url, fixed = TRUE))
}

data_extract_links <- function(html, base_url) {
  matches <- stringr::str_match_all(
    html,
    stringr::regex("<a\\s+[^>]*href\\s*=\\s*['\"]([^'\"]+)['\"][^>]*>(.*?)</a>", ignore_case = TRUE, dotall = TRUE)
  )[[1]]

  if (nrow(matches) == 0) {
    return(tibble::tibble(download_url = character(), data_link_label = character()))
  }

  tibble::tibble(
    download_url = vapply(matches[, 2], data_url_absolute, character(1), base_url = base_url),
    data_link_label = matches[, 3] |>
      str_replace_all("<[^>]+>", " ") |>
      data_html_decode()
  ) |>
    filter(nzchar(.data$download_url)) |>
    dplyr::distinct(.data$download_url, .data$data_link_label, .keep_all = TRUE)
}

data_page_catalog <- function(DataUrl = "https://www.qualityinfo.org/data") {
  html <- request(DataUrl) |>
    req_user_agent("Mozilla/5.0") |>
    oed_request_perform() |>
    httr2::resp_body_string()

  links <- data_extract_links(html, DataUrl)
  link_keys <- bind_rows(
    links |>
      dplyr::transmute(
        file_title_key = data_title_key(.data$data_link_label),
        download_url = .data$download_url,
        data_link_label = .data$data_link_label
      ),
    links |>
      mutate(
        url_file_name = .data$download_url |>
          stringr::str_remove("[?#].*$") |>
          basename() |>
          utils::URLdecode()
      ) |>
      dplyr::transmute(
        file_title_key = data_title_key(.data$url_file_name),
        download_url = .data$download_url,
        data_link_label = .data$data_link_label
      )
  ) |>
    filter(nzchar(.data$file_title_key)) |>
    dplyr::distinct(.data$file_title_key, .keep_all = TRUE)

  oed_data_catalog() |>
    dplyr::left_join(link_keys, by = "file_title_key")
}

# Sanitize source titles before using them as local filenames. This prevents
# URL or workbook labels from creating invalid paths on Windows.
data_safe_filename <- function(file_title) {
  file_name <- file_title |>
    str_replace_all("[\\\\/:*?\"<>|]+", " ") |>
    str_replace_all("\\s+", " ") |>
    stringr::str_squish()

  if (!str_detect(stringr::str_to_lower(file_name), "\\.xlsx$")) {
    file_name <- paste0(file_name, ".xlsx")
  }

  file_name
}

data_destination_path <- function(file_title, DownloadDir) {
  file.path(DownloadDir, data_safe_filename(file_title))
}

data_download_one <- function(download_url, destination_path, Overwrite = FALSE, DataUrl = "https://www.qualityinfo.org/data") {
  dir.create(dirname(destination_path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(destination_path) && !isTRUE(Overwrite)) {
    if (!data_is_xlsx(destination_path)) {
      stop(
        "Existing file is not an xlsx workbook: ", destination_path,
        ". Remove it or set Overwrite = TRUE.",
        call. = FALSE
      )
    }

    return(destination_path)
  }

  if (file.exists(destination_path) && isTRUE(Overwrite)) {
    unlink(destination_path)
  }

  request(download_url) |>
    req_headers(
      Referer = DataUrl,
      Accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet,*/*"
    ) |>
    req_user_agent("Mozilla/5.0") |>
    oed_request_perform(path = destination_path)

  if (!data_is_xlsx(destination_path)) {
    stop(
      "QualityInfo did not return an xlsx workbook for: ", download_url,
      call. = FALSE
    )
  }

  destination_path
}

# Generic workbook cleaning starts by locating the first dense header area.
# Specialized loaders use stricter schema-aware header detection.
data_find_header_row <- function(raw_mat) {
  if (nrow(raw_mat) == 0 || ncol(raw_mat) == 0) {
    return(1L)
  }

  non_empty <- apply(raw_mat, 1, function(row) {
    sum(!is.na(row) & nzchar(stringr::str_squish(row)))
  })
  candidates <- which(non_empty >= 2)

  if (length(candidates) == 0) {
    return(1L)
  }

  lookahead <- vapply(candidates, function(row_id) {
    if (row_id < length(non_empty)) {
      min(non_empty[row_id], non_empty[row_id + 1])
    } else {
      0
    }
  }, numeric(1))
  score <- non_empty[candidates] + lookahead - (candidates * 0.01)

  candidates[which.max(score)]
}

data_parse_number <- function(x) {
  x_chr <- stringr::str_squish(as.character(x))
  x_chr[x_chr %in% c("", "-", "--", "NA", "N/A", "(c)", "(C)")] <- NA_character_

  is_negative <- str_detect(x_chr, "^\\(.*\\)$")
  is_negative[is.na(is_negative)] <- FALSE
  cleaned <- x_chr |>
    str_replace_all("[,$%]", "") |>
    stringr::str_replace("^\\((.*)\\)$", "\\1")

  out <- suppressWarnings(as.numeric(cleaned))
  out[is_negative & !is.na(out)] <- -abs(out[is_negative & !is.na(out)])
  out
}

data_maybe_parse_number <- function(x, col_name, threshold = 0.9) {
  if (str_detect(col_name, stringr::regex("soc|naics|code|id|title|occupation|industry|name|area|region|geography|county", ignore_case = TRUE))) {
    return(x)
  }

  values <- stringr::str_squish(as.character(x))
  values <- values[!is.na(values) & nzchar(values)]

  if (length(values) == 0) {
    return(x)
  }

  parsed <- data_parse_number(values)

  if (mean(!is.na(parsed)) >= threshold) {
    return(data_parse_number(x))
  }

  x
}

data_meta_value <- function(meta, col) {
  if (is.null(meta) || nrow(meta) == 0 || !col %in% names(meta)) {
    return(NA_character_)
  }

  as.character(meta[[col]][1])
}

data_add_metadata <- function(data, path, sheet, meta, header_row = NA_integer_) {
  data |>
    mutate(
      source_workbook = basename(path),
      source_path = normalizePath(path, winslash = "/", mustWork = FALSE),
      source_sheet = sheet,
      source_category_key = data_meta_value(meta, "category_key"),
      source_category = data_meta_value(meta, "category"),
      source_geography = data_meta_value(meta, "geography"),
      source_title = data_meta_value(meta, "file_title"),
      source_url = data_meta_value(meta, "download_url"),
      source_header_row = as.integer(header_row),
      .before = 1
    )
}

data_classify_path <- function(path) {
  file_title <- tools::file_path_sans_ext(basename(path))
  key <- data_title_key(file_title)
  catalog <- oed_data_catalog()
  hit <- catalog |>
    filter(vapply(.data$file_title_key, function(file_key) {
      file_key == key ||
        str_detect(key, stringr::fixed(file_key)) ||
        str_detect(file_key, stringr::fixed(key))
    }, logical(1)))

  if (nrow(hit) == 0) {
    return(tibble::tibble(
      category_key = NA_character_,
      category = NA_character_,
      geography = NA_character_,
      file_title = file_title,
      download_url = NA_character_
    ))
  }

  hit |>
    dplyr::slice_head(n = 1) |>
    mutate(download_url = NA_character_)
}

data_metadata_for_path <- function(path, file_catalog = NULL) {
  path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)

  if (!is.null(file_catalog)) {
    for (path_col in intersect(c("path", "destination_path"), names(file_catalog))) {
      catalog_paths <- normalizePath(file_catalog[[path_col]], winslash = "/", mustWork = FALSE)
      hit <- which(catalog_paths == path_norm)

      if (length(hit) > 0) {
        return(file_catalog[hit[1], ])
      }
    }
  }

  data_classify_path(path)
}

data_read_sheet <- function(path, sheet, Clean = TRUE, meta = NULL) {
  raw <- suppressMessages(read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    .name_repair = "minimal",
    trim_ws = FALSE
  ))

  raw_mat <- as.matrix(raw)
  storage.mode(raw_mat) <- "character"

  if (!isTRUE(Clean)) {
    data <- tibble::as_tibble(raw_mat, .name_repair = "minimal")
    names(data) <- paste0("col_", seq_len(ncol(data)))
    return(data_add_metadata(data, path, sheet, meta))
  }

  header_row <- data_find_header_row(raw_mat)
  active_cols <- which(apply(raw_mat, 2, function(col) {
    any(!is.na(col) & nzchar(stringr::str_squish(col)))
  }))

  if (length(active_cols) == 0) {
    data <- tibble::tibble()
    return(data_add_metadata(data, path, sheet, meta, header_row = header_row))
  }

  last_col <- max(active_cols)
  header <- raw_mat[header_row, seq_len(last_col)]

  data_mat <- if (header_row >= nrow(raw_mat)) {
    matrix(character(), nrow = 0, ncol = last_col)
  } else {
    raw_mat[seq.int(header_row + 1, nrow(raw_mat)), seq_len(last_col), drop = FALSE]
  }

  data <- tibble::as_tibble(data_mat, .name_repair = "minimal")
  names(data) <- data_clean_names(header)

  data <- data |>
    mutate(across(dplyr::everything(), as.character)) |>
    filter(!dplyr::if_all(dplyr::everything(), ~ is.na(.x) | !nzchar(stringr::str_squish(.x))))

  for (col in names(data)) {
    data[[col]] <- data_maybe_parse_number(data[[col]], col)
  }

  data_add_metadata(data, path, sheet, meta, header_row = header_row)
}

data_read_workbooks <- function(Paths, Sheets = NULL, Clean = TRUE, file_catalog = NULL) {
  workbook_results <- lapply(Paths, function(path) {
    if (!file.exists(path)) {
      stop("File not found: ", path, call. = FALSE)
    }

    if (!data_is_xlsx(path)) {
      stop(
        "File is not an xlsx workbook: ", path,
        ". QualityInfo may have returned an HTML/error page instead of Excel.",
        call. = FALSE
      )
    }

    workbook_sheets <- excel_sheets(path)
    sheets <- data_or(Sheets, workbook_sheets)
    meta <- data_metadata_for_path(path, file_catalog)

    sheet_results <- lapply(sheets, function(sheet) {
      data_read_sheet(path, sheet, Clean = Clean, meta = meta)
    })

    bind_rows(sheet_results)
  })

  bind_rows(workbook_results)
}

OED_Data <- function(Category = NULL,
                     Geographies = NULL,
                     DownloadDir = file.path("output", "qualityinfo_data"),
                     Overwrite = FALSE,
                     Read = TRUE,
                     Clean = TRUE,
                     PreviewOnly = FALSE,
                     List = FALSE,
                     MaxDownloads = 10,
                     Paths = NULL,
                     Sheets = NULL,
                     DataUrl = "https://www.qualityinfo.org/data") {
  if (isTRUE(List)) {
    return(data_select_catalog(oed_data_catalog(), Category = Category, Geographies = Geographies))
  }

  # A bare OED_Data() call should be useful without attempting the entire
  # Oregon catalog. Explicit NULL values still request all matching entries.
  if (missing(Category) && missing(Geographies)) {
    Category <- "industry_projections"
    Geographies <- "Oregon"
  }

  if (!is.null(Paths)) {
    return(data_read_workbooks(Paths, Sheets = Sheets, Clean = Clean))
  }

  selected <- data_page_catalog(DataUrl) |>
    data_select_catalog(Category = Category, Geographies = Geographies)

  missing_links <- selected |>
    filter(is.na(.data$download_url) | !nzchar(.data$download_url))

  if (nrow(missing_links) > 0) {
    stop(
      "Could not find downloadable QualityInfo links for ",
      nrow(missing_links),
      " selected file(s). Run OED_Data(List = TRUE) to inspect the built-in catalog, ",
      "or check the data page for changed titles.",
      call. = FALSE
    )
  }

  selected <- selected |>
    mutate(destination_path = vapply(
      .data$file_title,
      data_destination_path,
      character(1),
      DownloadDir = DownloadDir
    ))
  selected$download_status <- ifelse(
    file.exists(selected$destination_path) && !isTRUE(Overwrite),
    "cached",
    "downloaded"
  )

  if (isTRUE(PreviewOnly)) {
    return(selected)
  }

  if (is.finite(MaxDownloads) && nrow(selected) > MaxDownloads) {
    stop(
      "This call would download ", nrow(selected), " QualityInfo Excel files. ",
      "Narrow Category or Geographies, set PreviewOnly = TRUE, or raise MaxDownloads.",
      call. = FALSE
    )
  }

  paths <- character(nrow(selected))
  for (i in seq_len(nrow(selected))) {
    paths[i] <- data_download_one(
      download_url = selected$download_url[i],
      destination_path = selected$destination_path[i],
      Overwrite = Overwrite,
      DataUrl = DataUrl
    )
  }
  downloaded <- mutate(selected, path = paths)

  if (!isTRUE(Read)) {
    return(oed_attach_download_diagnostics(
      downloaded,
      command = "OED_Data",
      plan = downloaded
    ))
  }

  result <- data_read_workbooks(
    Paths = downloaded$path,
    Sheets = Sheets,
    Clean = Clean,
    file_catalog = downloaded
  )
  oed_attach_download_diagnostics(
    result,
    command = "OED_Data",
    plan = downloaded
  )
}

data_page_workbook_catalog <- function(PageUrl, Urls = NULL) {
  if (!is.null(Urls)) {
    if (length(Urls) == 0 || anyNA(Urls)) {
      stop("Urls must contain at least one non-missing workbook URL.", call. = FALSE)
    }

    if (any(!stringr::str_detect(as.character(Urls), "^https?://"))) {
      stop("Urls must contain absolute HTTP(S) workbook URLs.", call. = FALSE)
    }

    labels <- names(Urls)
    Urls <- as.character(Urls)
    if (is.null(labels)) {
      labels <- rep("", length(Urls))
    }

    fallback <- Urls |>
      stringr::str_remove("[?#].*$") |>
      basename() |>
      utils::URLdecode() |>
      tools::file_path_sans_ext()

    labels[is.na(labels) | !nzchar(stringr::str_squish(labels))] <- fallback[
      is.na(labels) | !nzchar(stringr::str_squish(labels))
    ]
    blank <- is.na(labels) | !nzchar(stringr::str_squish(labels))
    labels[blank] <- paste("QualityInfo workbook", which(blank))

    return(tibble::tibble(
      file_title = labels,
      download_url = Urls,
      data_link_label = labels
    ))
  }

  html <- request(PageUrl) |>
    req_user_agent("Mozilla/5.0") |>
    oed_request_perform() |>
    httr2::resp_body_string()

  links <- data_extract_links(html, PageUrl)
  keep <- stringr::str_detect(
    links$download_url,
    stringr::regex("\\.xlsx(?:[?#].*)?$|getReportXlsx", ignore_case = TRUE)
  ) | stringr::str_detect(
    links$data_link_label,
    stringr::regex("\\.xlsx$|download.*excel|excel.*download", ignore_case = TRUE)
  )
  links <- links[keep, , drop = FALSE]

  if (nrow(links) == 0) {
    stop(
      "No direct Excel workbook links were found on ", PageUrl, ". ",
      "The page may use an interactive request rather than static links. ",
      "Provide already-downloaded Paths or explicit workbook Urls.",
      call. = FALSE
    )
  }

  fallback <- links$download_url |>
    stringr::str_remove("[?#].*$") |>
    basename() |>
    utils::URLdecode() |>
    tools::file_path_sans_ext()
  titles <- stringr::str_squish(links$data_link_label)
  titles[is.na(titles) | !nzchar(titles)] <- fallback[is.na(titles) | !nzchar(titles)]
  blank <- is.na(titles) | !nzchar(titles)
  titles[blank] <- paste("QualityInfo workbook", which(blank))

  tibble::tibble(
    file_title = titles,
    download_url = links$download_url,
    data_link_label = links$data_link_label
  ) |>
    dplyr::distinct(.data$download_url, .keep_all = TRUE)
}

# Run the shared page-link workflow used by the static IProfile and Business
# List pages. Interactive report pages require a specialized loader instead.
data_page_workbooks <- function(Command,
                                PageUrl,
                                Paths = NULL,
                                Urls = NULL,
                                DownloadDir,
                                Overwrite = FALSE,
                                Read = TRUE,
                                Clean = TRUE,
                                PreviewOnly = FALSE,
                                MaxDownloads = 10,
                                Sheets = NULL) {
  if (!is.null(Paths)) {
    return(data_read_workbooks(Paths, Sheets = Sheets, Clean = Clean))
  }

  selected <- data_page_workbook_catalog(PageUrl = PageUrl, Urls = Urls)
  selected$destination_path <- vapply(
    selected$file_title,
    data_destination_path,
    character(1),
    DownloadDir = DownloadDir
  )
  selected$command <- Command
  selected$page_url <- PageUrl
  selected$download_status <- ifelse(
    file.exists(selected$destination_path) && !isTRUE(Overwrite),
    "cached",
    "downloaded"
  )
  selected <- dplyr::relocate(selected, .data$command, .data$page_url)

  if (isTRUE(PreviewOnly)) {
    return(selected)
  }

  if (is.finite(MaxDownloads) && nrow(selected) > MaxDownloads) {
    stop(
      Command, " would download ", nrow(selected), " QualityInfo Excel files. ",
      "Provide fewer Urls, set PreviewOnly = TRUE, or raise MaxDownloads.",
      call. = FALSE
    )
  }

  paths <- character(nrow(selected))
  for (i in seq_len(nrow(selected))) {
    paths[i] <- data_download_one(
      download_url = selected$download_url[i],
      destination_path = selected$destination_path[i],
      Overwrite = Overwrite,
      DataUrl = PageUrl
    )
  }
  downloaded <- dplyr::mutate(selected, path = paths)

  if (!isTRUE(Read)) {
    return(oed_attach_download_diagnostics(
      downloaded,
      command = Command,
      plan = downloaded
    ))
  }

  result <- data_read_workbooks(
    Paths = downloaded$path,
    Sheets = Sheets,
    Clean = Clean,
    file_catalog = downloaded
  )
  oed_attach_download_diagnostics(
    result,
    command = Command,
    plan = downloaded
  )
}

OED_IProfile <- function(Paths = NULL,
                         Urls = NULL,
                         DownloadDir = file.path("output", "qualityinfo_iprofile"),
                         Overwrite = FALSE,
                         Read = TRUE,
                         Clean = TRUE,
                         PreviewOnly = FALSE,
                         MaxDownloads = 10,
                         Sheets = NULL,
                         PageUrl = "https://www.qualityinfo.org/lipro") {
  data_page_workbooks(
    Command = "OED_IProfile",
    PageUrl = PageUrl,
    Paths = Paths,
    Urls = Urls,
    DownloadDir = DownloadDir,
    Overwrite = Overwrite,
    Read = Read,
    Clean = Clean,
    PreviewOnly = PreviewOnly,
    MaxDownloads = MaxDownloads,
    Sheets = Sheets
  )
}

OED_Businesses <- function(Paths = NULL,
                           Urls = NULL,
                           DownloadDir = file.path("output", "qualityinfo_businesses"),
                           Overwrite = FALSE,
                           Read = TRUE,
                           Clean = TRUE,
                           PreviewOnly = FALSE,
                           MaxDownloads = 10,
                           Sheets = NULL,
                           PageUrl = "https://www.qualityinfo.org/blist") {
  data_page_workbooks(
    Command = "OED_Businesses",
    PageUrl = PageUrl,
    Paths = Paths,
    Urls = Urls,
    DownloadDir = DownloadDir,
    Overwrite = Overwrite,
    Read = Read,
    Clean = Clean,
    PreviewOnly = PreviewOnly,
    MaxDownloads = MaxDownloads,
    Sheets = Sheets
  )
}
