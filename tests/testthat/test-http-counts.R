new_http_counter <- function(fixture = NULL, fail_first = 0L) {
  state <- new.env(parent = emptyenv())
  state$calls <- character()
  state$attempts <- integer()

  handler <- function(req, path = NULL, attempt = 1L) {
    url <- as.character(req$url)
    state$calls <- c(state$calls, url)
    state$attempts <- c(state$attempts, attempt)

    if (length(state$calls) <= fail_first) {
      stop("simulated transient failure")
    }
    if (is.null(path)) {
      stop("unexpected non-download HTTP request")
    }

    if (is.null(fixture)) {
      writeBin(charToRaw("not an xlsx"), path)
    } else {
      file.copy(fixture, path, overwrite = TRUE)
    }
    invisible(NULL)
  }

  list(state = state, handler = handler)
}

with_mock_http <- function(counter, code) {
  old <- getOption("OEDloadR.http_perform", NULL)
  options(OEDloadR.http_perform = counter$handler)
  on.exit(options(OEDloadR.http_perform = old), add = TRUE)
  force(code)
}

testthat::test_that("static preview plans make no HTTP requests", {
  counter <- new_http_counter()
  result <- with_mock_http(counter, {
    list(
      data = OED_Data(PreviewOnly = TRUE),
      laus = OED_LAUS_Table(PreviewOnly = TRUE),
      qcew = OED_QCEW_Table(PreviewOnly = TRUE),
      iprofile = OED_IProfile(
        Urls = c(example = "https://example.org/example.xlsx"),
        PreviewOnly = TRUE
      ),
      businesses = OED_Businesses(
        Urls = c(example = "https://example.org/example.xlsx"),
        PreviewOnly = TRUE
      ),
      qcew_options = OED_QCEW_Options(),
      laus_options = OED_LAUS_Options()
    )
  })

  testthat::expect_length(counter$state$calls, 0)
  testthat::expect_equal(nrow(result$data), 1)
  testthat::expect_equal(nrow(result$laus), 4)
  testthat::expect_equal(nrow(result$qcew), 1)
  testthat::expect_true(nrow(result$qcew_options$geographies) > 0)
  testthat::expect_true(nrow(result$laus_options$geographies) > 0)
})

testthat::test_that("ordinary uncached pulls make only report requests", {
  fixture_dir <- testthat::test_path("fixtures")
  laus_fixture <- file.path(
    fixture_dir, "laus", "selected-all-sa-unemprate.xlsx"
  )
  qcew_fixture <- file.path(
    fixture_dir, "qcew", "qcew_oregon_2025_annual.xlsx"
  )

  data_counter <- new_http_counter(laus_fixture)
  data_result <- with_mock_http(data_counter, OED_Data(
    Category = "industry projections",
    Geographies = "Oregon",
    DownloadDir = tempfile("data-http-count-"),
    Read = FALSE
  ))
  testthat::expect_length(data_counter$state$calls, 1)
  testthat::expect_true(grepl("/documents/", data_counter$state$calls[1]))
  testthat::expect_identical(data_result$download_status, "downloaded")

  laus_counter <- new_http_counter(laus_fixture)
  laus_result <- with_mock_http(laus_counter, OED_LAUS_Table(
    Geographies = "Oregon",
    StartYear = 2025,
    EndYear = 2025,
    Measures = "ur",
    DownloadDir = tempfile("laus-http-count-"),
    MaxDownloads = 1
  ))
  testthat::expect_length(laus_counter$state$calls, 1)
  testthat::expect_true(grepl("getReportXlsx", laus_counter$state$calls[1]))
  testthat::expect_true(nrow(laus_result) > 0)

  qcew_counter <- new_http_counter(qcew_fixture)
  qcew_result <- with_mock_http(qcew_counter, OED_QCEW_Table(
    Years = 2025,
    Geographies = "Oregon",
    MaxRequests = 1
  ))
  testthat::expect_length(qcew_counter$state$calls, 1)
  testthat::expect_true(grepl("getReportXlsx", qcew_counter$state$calls[1]))
  testthat::expect_true(nrow(qcew_result) > 0)

  iprofile_counter <- new_http_counter(laus_fixture)
  iprofile_result <- with_mock_http(iprofile_counter, OED_IProfile(
    Urls = c(example = "https://example.org/iprofile.xlsx"),
    DownloadDir = tempfile("iprofile-http-count-"),
    Read = FALSE
  ))
  testthat::expect_length(iprofile_counter$state$calls, 1)
  testthat::expect_identical(iprofile_result$download_status, "downloaded")

  businesses_counter <- new_http_counter(laus_fixture)
  businesses_result <- with_mock_http(businesses_counter, OED_Businesses(
    Urls = c(example = "https://example.org/businesses.xlsx"),
    DownloadDir = tempfile("businesses-http-count-"),
    Read = FALSE
  ))
  testthat::expect_length(businesses_counter$state$calls, 1)
  testthat::expect_identical(businesses_result$download_status, "downloaded")
})

testthat::test_that("paths and valid caches are entirely offline", {
  fixture_dir <- testthat::test_path("fixtures")
  laus_fixture <- file.path(
    fixture_dir, "laus", "laus_all_areas_sa_2000_2026.xlsx"
  )
  qcew_fixture <- file.path(
    fixture_dir, "qcew", "qcew_oregon_2025_annual.xlsx"
  )
  selected_laus <- file.path(
    fixture_dir, "laus", "selected-all-sa-unemprate.xlsx"
  )
  counter <- new_http_counter()
  result <- with_mock_http(counter, {
    data <- OED_Data(Paths = laus_fixture)
    laus <- OED_LAUS_Table(Paths = selected_laus)
    qcew <- OED_QCEW_Table(Paths = qcew_fixture)
    iprofile <- OED_IProfile(Paths = laus_fixture)
    businesses <- OED_Businesses(Paths = laus_fixture)

    cache_dir <- tempfile("cache-http-count-")
    dir.create(cache_dir)
    cache_path <- file.path(cache_dir, "Example SA.xlsx")
    file.copy(laus_fixture, cache_path)
    cached <- OED_LAUS_Table(
      Urls = c("Example SA" = "https://example.org/example.xlsx"),
      DownloadDir = cache_dir,
      MaxDownloads = 0
    )

    data_cache_dir <- tempfile("data-cache-http-count-")
    dir.create(data_cache_dir)
    data_cache_path <- file.path(
      data_cache_dir,
      "Oregon Industry Employment Projections 2024-2034.xlsx"
    )
    file.copy(laus_fixture, data_cache_path)
    data_cached <- OED_Data(
      Category = "industry projections",
      Geographies = "Oregon",
      DownloadDir = data_cache_dir,
      Read = FALSE,
      MaxDownloads = 0
    )

    iprofile_cache_dir <- tempfile("iprofile-cache-http-count-")
    dir.create(iprofile_cache_dir)
    file.copy(laus_fixture, file.path(iprofile_cache_dir, "Example.xlsx"))
    iprofile_cached <- OED_IProfile(
      Urls = c("Example" = "https://example.org/example.xlsx"),
      DownloadDir = iprofile_cache_dir,
      Read = FALSE,
      MaxDownloads = 0
    )
    businesses_cached <- OED_Businesses(
      Urls = c("Example" = "https://example.org/example.xlsx"),
      DownloadDir = iprofile_cache_dir,
      Read = FALSE,
      MaxDownloads = 0
    )

    qcew_cache_dir <- tempfile("qcew-cache-http-count-")
    qcew_preview <- OED_QCEW_Table(
      Years = 2025,
      Geographies = "Oregon",
      DownloadDir = qcew_cache_dir,
      PreviewOnly = TRUE
    )
    dir.create(qcew_cache_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(qcew_fixture, qcew_preview$destination_path[1])
    qcew_cached <- OED_QCEW_Table(
      Years = 2025,
      Geographies = "Oregon",
      DownloadDir = qcew_cache_dir,
      MaxRequests = 0
    )

    list(data = data, laus = laus, qcew = qcew, iprofile = iprofile,
         businesses = businesses, cached = cached, data_cached = data_cached,
         iprofile_cached = iprofile_cached,
         businesses_cached = businesses_cached,
         qcew_cached = qcew_cached)
  })

  testthat::expect_length(counter$state$calls, 0)
  testthat::expect_true(nrow(result$laus) > 0)
  testthat::expect_true(nrow(result$qcew) > 0)
  testthat::expect_true(nrow(result$cached) > 0)
  testthat::expect_identical(result$data_cached$download_status, "cached")
  testthat::expect_identical(result$iprofile_cached$download_status, "cached")
  testthat::expect_identical(result$businesses_cached$download_status, "cached")
  testthat::expect_true(nrow(result$qcew_cached) > 0)
})

testthat::test_that("stable invalid selections fail before HTTP", {
  counter <- new_http_counter()
  with_mock_http(counter, {
    testthat::expect_error(
      OED_Data(Category = "not a category", PreviewOnly = TRUE),
      "No matching OED_Data category"
    )
    testthat::expect_error(
      OED_LAUS_Table(Geographies = "not a geography", PreviewOnly = TRUE),
      "No matching QualityInfo LAUS geography"
    )
    testthat::expect_error(
      OED_QCEW_Table(Geographies = "not a geography", PreviewOnly = TRUE),
      "No matching QCEW geographies"
    )
    testthat::expect_error(
      OED_IProfile(PreviewOnly = TRUE),
      "interactive QualityInfo report page"
    )
  })
  testthat::expect_length(counter$state$calls, 0)
})

testthat::test_that("invalid server data produces a clear error", {
  counter <- new_http_counter()
  with_mock_http(counter, {
    outcome <- OEDloadR:::data_download_one(
      download_url = "https://example.org/report.xlsx",
      destination_path = file.path(tempfile("invalid-xlsx-"), "report.xlsx"),
      DataUrl = "https://example.org/data",
      Refresh = "always"
    )
    testthat::expect_identical(outcome$status, "failed")
    testthat::expect_match(outcome$error, "valid XLSX workbook")
  })
  testthat::expect_length(counter$state$calls, 1)
})

testthat::test_that("incompatible workbook layouts explain how to recover", {
  laus_fixture <- testthat::test_path(
    "fixtures", "laus", "selected-all-sa-unemprate.xlsx"
  )
  testthat::expect_error(
    OED_QCEW_Table(Paths = laus_fixture),
    "layout may have changed; update OEDloadR"
  )
})

testthat::test_that("retries repeat only the necessary report request", {
  fixture <- testthat::test_path(
    "fixtures", "laus", "selected-all-sa-unemprate.xlsx"
  )
  counter <- new_http_counter(fixture, fail_first = 1L)
  result <- with_mock_http(counter, OED_Data(
    Category = "industry projections",
    Geographies = "Oregon",
    DownloadDir = tempfile("retry-http-count-"),
    Read = FALSE
  ))

  testthat::expect_identical(result$download_status, "downloaded")
  testthat::expect_length(counter$state$calls, 2)
  testthat::expect_identical(counter$state$calls[1], counter$state$calls[2])
  testthat::expect_identical(counter$state$attempts, c(1L, 2L))
})

testthat::test_that("bulk plans use report requests rather than lookup loops", {
  laus <- OED_LAUS_Table(
    Geographies = NULL,
    StartYear = 2025,
    EndYear = 2025,
    PreviewOnly = TRUE
  )
  qcew <- OED_QCEW_Table(
    Years = 2025,
    Geographies = c("Oregon", "Benton"),
    PreviewOnly = TRUE
  )

  testthat::expect_equal(nrow(laus), 4)
  testthat::expect_true(all(laus$scope == "selected_areas"))
  testthat::expect_equal(nrow(qcew), 2)
  testthat::expect_setequal(qcew$qcew_area, c("4101000000", "4104000003"))
})
