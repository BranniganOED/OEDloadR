testthat::test_that("QualityInfo catalog aliases resolve", {
  testthat::expect_equal(
    OEDloadR:::data_resolve_categories("industry projections"),
    "industry_projections"
  )
  testthat::expect_equal(
    OEDloadR:::data_resolve_geographies("statewide"),
    "Oregon"
  )
})

testthat::test_that("QualityInfo filenames are safe on Windows", {
  filename <- OEDloadR:::data_safe_filename("Portland: Metro / 2026")

  testthat::expect_false(grepl("[\\\\/:*?\"<>|]", filename))
  testthat::expect_true(grepl("\\.xlsx$", filename))
})

testthat::test_that("explicit workbook URLs produce a preview plan", {
  plan <- OED_IProfile(
    Urls = c("Example workbook" = "https://example.org/example.xlsx"),
    PreviewOnly = TRUE
  )

  testthat::expect_s3_class(plan, "data.frame")
  testthat::expect_equal(plan$command, "OED_IProfile")
  testthat::expect_equal(plan$download_url, "https://example.org/example.xlsx")
  testthat::expect_equal(plan$download_status, "downloaded")
})

testthat::test_that("QCEW period and NAICS filters are predictable", {
  periods <- OEDloadR:::qcew_resolve_periods("quarterly")
  testthat::expect_length(periods, 4)
  testthat::expect_true(all(vapply(periods, function(x) x$label, character(1)) %in%
    c("1st Quarter", "2nd Quarter", "3rd Quarter", "4th Quarter")))

  data <- tibble::tibble(
    naics = c("54", "5413", "11"),
    value = 1:3
  )
  exact <- OEDloadR:::qcew_filter_naics(data, "54", "exact")
  prefix <- OEDloadR:::qcew_filter_naics(data, "54", "prefix")

  testthat::expect_equal(exact$value, 1)
  testthat::expect_equal(prefix$value, c(1, 2))
})

testthat::test_that("QCEW recognizes both data and no-data workbook headers", {
  header <- matrix(
    c("NAICS", "Industry", "Ownership", "Units", "Employment", "Wages"),
    nrow = 1
  )
  no_data_header <- matrix(
    c(
      "NAICS",
      "No Summary Data available for search options provided.",
      "Ownership",
      "Units",
      "Employment",
      "Wages"
    ),
    nrow = 1
  )

  testthat::expect_equal(OEDloadR:::qcew_find_header_row(header), 1L)
  testthat::expect_equal(OEDloadR:::qcew_find_header_row(no_data_header), 1L)
})

testthat::test_that("LAUS dates are normalized", {
  parsed <- OEDloadR:::laus_parse_date(c("Jan-2026", "2025", ""))

  testthat::expect_equal(parsed[[1]], as.Date("2026-01-01"))
  testthat::expect_equal(parsed[[2]], as.Date("2025-01-01"))
  testthat::expect_true(is.na(parsed[[3]]))
})

testthat::test_that("LAUS all-area workbooks retain the published schema", {
  fixture_dir <- testthat::test_path("fixtures", "laus")
  paths <- file.path(
    fixture_dir,
    c(
      "laus_all_areas_sa_2000_2026.xlsx",
      "laus_all_areas_nsa_2000_2026.xlsx"
    )
  )
  areas <- utils::read.csv(
    file.path(fixture_dir, "laus_area_lookup.csv"),
    stringsAsFactors = FALSE
  )

  raw_data <- OEDloadR:::laus_read_workbooks(paths, AreaLookup = areas)
  measures <- c("unemployment_rate", "labor_force", "employed", "unemployed")

  testthat::expect_equal(dplyr::n_distinct(raw_data$geography), 46)
  testthat::expect_true(all(!is.na(raw_data$area_code)))
  testthat::expect_setequal(
    unique(raw_data$frequency),
    c("monthly", "annual")
  )
  testthat::expect_setequal(
    unique(raw_data$seasonal_adjustment),
    c("seasonally adjusted", "not seasonally adjusted", "annual average")
  )
  testthat::expect_true(all(vapply(raw_data[measures], function(x) all(!is.na(x)), logical(1))))
  testthat::expect_false(any(grepl("prov", names(raw_data))))
  testthat::expect_true("Multnomah County" %in% raw_data$geography)
  testthat::expect_equal(
    max(raw_data$date[raw_data$frequency == "monthly"]),
    as.Date("2026-06-01")
  )

  annual <- dplyr::filter(raw_data, .data$frequency == "annual")
  testthat::expect_equal(
    nrow(annual),
    nrow(dplyr::distinct(annual, geography, date))
  )
  testthat::expect_equal(nrow(annual), 46 * 26)

  compact <- OED_LAUS_Table(Paths = paths, AreaLookup = areas)
  testthat::expect_identical(
    names(compact),
    c("year", "month", "area_code", "geography", "sa_ur", "sa_lf",
      "sa_unemployed", "sa_employed")
  )
  testthat::expect_equal(
    nrow(compact),
    nrow(dplyr::distinct(compact, geography, year, month))
  )
  testthat::expect_true(all(vapply(compact[5:8], function(x) all(!is.na(x)), logical(1))))

  expanded <- OED_LAUS_Table(
    Paths = paths,
    AreaLookup = areas,
    SeasonalAdjustment = "both",
    Frequency = "both",
    metadata = TRUE
  )
  testthat::expect_true(all(c("sa_ur", "nsa_ur", "annual_ur", "date", "frequency") %in%
    names(expanded)))
  testthat::expect_true(any(expanded$frequency == "annual"))

  selected_paths <- file.path(
    fixture_dir,
    c(
      "selected-all-sa-unemprate.xlsx",
      "selected-all-sa-laborforce.xlsx",
      "selected-all-sa-employed.xlsx",
      "selected-all-sa-unemployed.xlsx"
    )
  )
  selected_default <- OED_LAUS_Table(
    Paths = selected_paths,
    AreaLookup = areas
  )
  testthat::expect_identical(names(selected_default), names(compact))
  testthat::expect_equal(dplyr::n_distinct(selected_default$geography), 46)
  testthat::expect_true(all(vapply(
    selected_default[5:8],
    function(x) all(!is.na(x)),
    logical(1)
  )))
})

testthat::test_that("LAUS request plans use the selected-area endpoint", {
  fixture_dir <- testthat::test_path("fixtures", "laus")
  areas <- utils::read.csv(
    file.path(fixture_dir, "laus_area_lookup.csv"),
    stringsAsFactors = FALSE
  )
  config <- list(
    report_xlsx_url = "https://example.org/getReportXlsx"
  )

  plan <- OEDloadR:::laus_build_request_plan(
    config = config,
    Areas = areas,
    StartYear = 2000,
    EndYear = 2026,
    DownloadDir = tempfile("laus-plan-")
  )

  testthat::expect_equal(nrow(plan), 4)
  testthat::expect_true(all(plan$scope == "selected_areas"))
  testthat::expect_setequal(
    plan$measure_code,
    c("unemprate", "laborforce", "emplab", "unemp")
  )
  testthat::expect_true(all(plan$adjustment_code == "1"))
  testthat::expect_true(all(grepl("getReportXlsx", plan$download_url)))
  testthat::expect_true(all(grepl("lf_areanames|lf_areacode", plan$download_url)))
  testthat::expect_equal(length(unique(plan$cache_key)), 4)
  testthat::expect_true(all(plan$start_year == 2000 & plan$end_year == 2026))

  both <- OEDloadR:::laus_build_request_plan(
    config = config,
    Areas = areas,
    StartYear = 2000,
    EndYear = 2026,
    DownloadDir = tempfile("laus-plan-both-"),
    SeasonalAdjustment = "both",
    Measures = "ur"
  )
  testthat::expect_equal(nrow(both), 2)
  testthat::expect_setequal(both$adjustment_code, c("0", "1"))
})

testthat::test_that("LAUS cache hits do not consume the download limit", {
  fixture_dir <- testthat::test_path("fixtures", "laus")
  download_dir <- tempfile("laus-cache-")
  dir.create(download_dir)
  destination <- file.path(download_dir, "Example SA.xlsx")
  file.copy(
    file.path(fixture_dir, "laus_all_areas_sa_2000_2026.xlsx"),
    destination
  )

  result <- OED_LAUS_Table(
    Urls = c("Example SA" = "https://example.org/example.xlsx"),
    DownloadDir = download_dir,
    MaxDownloads = 0
  )

  testthat::expect_true(nrow(result) > 0)
  testthat::expect_equal(OED_Diagnostics(result)$uncached_requests, 0)
  testthat::expect_equal(OED_Diagnostics(result)$cache_hits, 1)
})

testthat::test_that("data listing remains discoverable", {
  catalog <- OED_Data(List = TRUE)

  testthat::expect_s3_class(catalog, "data.frame")
  testthat::expect_true(all(c("category", "geography", "file_title") %in% names(catalog)))
  testthat::expect_true(any(catalog$geography == "Oregon"))
})

testthat::test_that("download diagnostics have a common shape", {
  result <- OEDloadR:::oed_attach_download_diagnostics(
    tibble::tibble(value = 1),
    command = "test",
    plan = tibble::tibble(download_status = c("cached", "downloaded"))
  )
  diagnostics <- OED_Diagnostics(result)

  testthat::expect_equal(diagnostics$files_requested, 2)
  testthat::expect_equal(diagnostics$files_downloaded, 1)
  testthat::expect_equal(diagnostics$files_reused, 1)
})

testthat::test_that("QualityInfo URLs encode spaces", {
  href <- paste0(
    "/documents/20118/37537/",
    "Portland Tri-County Industry Projections 2024-2034/",
    "8dd44f95-dd35-5255-0e17-86b562938864?version=1.0"
  )

  url <- OEDloadR:::data_url_absolute(
    href,
    "https://www.qualityinfo.org/data"
  )

  expected <- paste0(
    "https://www.qualityinfo.org/documents/20118/37537/",
    "Portland%20Tri-County%20Industry%20Projections%202024-2034/",
    "8dd44f95-dd35-5255-0e17-86b562938864?version=1.0"
  )

  testthat::expect_equal(url, expected)
  testthat::expect_false(grepl(" ", url, fixed = TRUE))
})
