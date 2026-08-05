testthat::test_that("shared data roots honor option, environment, and dataset folders", {
  old_option <- getOption("OEDloadR.data_dir", NULL)
  old_env <- Sys.getenv("OEDLOADR_DATA_DIR", unset = NA_character_)
  on.exit({
    options(OEDloadR.data_dir = old_option)
    if (is.na(old_env)) Sys.unsetenv("OEDLOADR_DATA_DIR") else Sys.setenv(OEDLOADR_DATA_DIR = old_env)
  }, add = TRUE)

  options(OEDloadR.data_dir = NULL)
  Sys.unsetenv("OEDLOADR_DATA_DIR")
  expected_default <- normalizePath(file.path(getwd(), "OEDloadR data"), mustWork = FALSE)
  testthat::expect_equal(OEDloadR:::oed_data_root(), expected_default)

  env_root <- file.path(tempdir(), "oed-env-root")
  Sys.setenv(OEDLOADR_DATA_DIR = env_root)
  testthat::expect_equal(
    OEDloadR:::oed_dataset_download_dir("LAUS"),
    file.path(normalizePath(env_root, mustWork = FALSE), "LAUS")
  )

  option_root <- file.path(tempdir(), "oed-option-root")
  options(OEDloadR.data_dir = option_root)
  testthat::expect_equal(
    OEDloadR:::oed_dataset_download_dir("QCEW"),
    file.path(normalizePath(option_root, mustWork = FALSE), "QCEW")
  )
  testthat::expect_equal(
    OEDloadR:::oed_dataset_download_dir("Data"),
    file.path(normalizePath(option_root, mustWork = FALSE), "Data")
  )
  testthat::expect_equal(
    OEDloadR:::oed_dataset_download_dir("Industry Profiles"),
    file.path(normalizePath(option_root, mustWork = FALSE), "Industry Profiles")
  )
  testthat::expect_equal(
    OEDloadR:::oed_dataset_download_dir("Businesses"),
    file.path(normalizePath(option_root, mustWork = FALSE), "Businesses")
  )

  explicit <- file.path(tempdir(), "explicit-download")
  testthat::expect_identical(
    OEDloadR:::oed_dataset_download_dir("LAUS", explicit),
    explicit
  )
})

testthat::test_that("cache decisions apply refresh policies and freshness signals", {
  fixture <- testthat::test_path("fixtures", "laus", "selected-all-sa-unemprate.xlsx")
  cache <- tempfile(fileext = ".xlsx")
  testthat::expect_true(file.copy(fixture, cache))

  fresh <- OEDloadR:::oed_cache_decision(
    cache,
    command = "test",
    dataset = "LAUS",
    Refresh = "auto",
    MaxAge = 30
  )
  testthat::expect_identical(fresh$status, "cached")
  testthat::expect_identical(fresh$reason, "within_max_age")

  old_time <- Sys.time() - as.difftime(3, units = "days")
  Sys.setFileTime(cache, old_time)
  expired <- OEDloadR:::oed_cache_decision(
    cache,
    command = "test",
    dataset = "LAUS",
    Refresh = "auto",
    MaxAge = 1
  )
  testthat::expect_identical(expired$status, "downloaded")
  testthat::expect_identical(expired$reason, "max_age_exceeded")

  never <- OEDloadR:::oed_cache_decision(
    cache,
    command = "test",
    dataset = "LAUS",
    Refresh = "never",
    MaxAge = 1
  )
  testthat::expect_identical(never$status, "cached")
  testthat::expect_identical(never$reason, "refresh_never")

  always <- OEDloadR:::oed_cache_decision(
    cache,
    command = "test",
    dataset = "LAUS",
    Refresh = "always",
    MaxAge = 30
  )
  testthat::expect_identical(always$status, "downloaded")
  testthat::expect_identical(always$reason, "refresh_always")

  overwrite <- OEDloadR:::oed_cache_decision(
    cache,
    command = "test",
    dataset = "LAUS",
    Refresh = "never",
    Overwrite = TRUE,
    MaxAge = 30
  )
  testthat::expect_identical(overwrite$status, "downloaded")
  testthat::expect_identical(overwrite$reason, "overwrite")

  metadata <- OEDloadR:::oed_cache_write_metadata(
    cache,
    command = "test",
    dataset = "LAUS",
    source_url = "https://example.org/laus.xlsx",
    request_parameters = list(area = "Oregon"),
    latest_available_period = "2026"
  )
  metadata$etag <- "old-etag"
  metadata$last_modified <- "old-modified"
  jsonlite::write_json(
    metadata,
    OEDloadR:::oed_cache_sidecar_path(cache),
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )

  etag_changed <- OEDloadR:::oed_cache_decision(
    cache,
    command = "test",
    dataset = "LAUS",
    current_etag = "new-etag",
    MaxAge = 30
  )
  testthat::expect_identical(etag_changed$reason, "etag_changed")

  modified_changed <- OEDloadR:::oed_cache_decision(
    cache,
    command = "test",
    dataset = "LAUS",
    current_last_modified = "new-modified",
    MaxAge = 30
  )
  testthat::expect_identical(modified_changed$reason, "last_modified_changed")

  period_changed <- OEDloadR:::oed_cache_decision(
    cache,
    command = "test",
    dataset = "LAUS",
    latest_available_period = "2027",
    MaxAge = 30
  )
  testthat::expect_identical(period_changed$reason, "latest_period_changed")

  title_changed <- OEDloadR:::oed_cache_decision(
    cache,
    command = "test",
    dataset = "Data",
    catalog_title = "New title",
    MaxAge = 30
  )
  testthat::expect_identical(title_changed$reason, "catalog_title_changed")

  url_changed <- OEDloadR:::oed_cache_decision(
    cache,
    command = "test",
    dataset = "Data",
    catalog_url = "https://example.org/new.xlsx",
    MaxAge = 30
  )
  testthat::expect_identical(url_changed$reason, "catalog_url_changed")

  read_back <- OEDloadR:::oed_cache_read_metadata(cache)
  testthat::expect_true(file.exists(OEDloadR:::oed_cache_sidecar_path(cache)))
  testthat::expect_identical(read_back$dataset, "LAUS")
  testthat::expect_identical(read_back$request_parameters$area, "Oregon")
  testthat::expect_true(nzchar(read_back$hash))
})

testthat::test_that("corrupt caches are rejected and safe refreshes preserve valid data", {
  fixture <- testthat::test_path("fixtures", "laus", "selected-all-sa-unemprate.xlsx")
  replacement <- testthat::test_path("fixtures", "laus", "selected-all-sa-laborforce.xlsx")
  cache_dir <- tempfile("oed-cache-safe-")
  dir.create(cache_dir)
  cache <- file.path(cache_dir, "LAUS.xlsx")
  testthat::expect_true(file.copy(fixture, cache))

  corrupt <- file.path(cache_dir, "corrupt.xlsx")
  writeBin(charToRaw("not an xlsx"), corrupt)
  corrupt_decision <- OEDloadR:::oed_cache_decision(
    corrupt,
    command = "test",
    dataset = "LAUS"
  )
  testthat::expect_false(corrupt_decision$existing_valid)
  testthat::expect_identical(corrupt_decision$reason, "invalid_workbook")

  mock_download <- function(request, path) {
    file.copy(replacement, path, overwrite = TRUE)
    invisible(NULL)
  }
  refreshed <- OEDloadR:::oed_cache_download(
    request = httr2::request("https://example.org/laus.xlsx"),
    destination_path = cache,
    command = "test",
    dataset = "LAUS",
    source_url = "https://example.org/laus.xlsx",
    request_parameters = list(area = "Oregon"),
    Refresh = "always",
    perform = mock_download
  )
  testthat::expect_identical(refreshed$status, "refreshed")
  testthat::expect_true(OEDloadR:::data_is_xlsx(cache))
  testthat::expect_true(file.exists(OEDloadR:::oed_cache_sidecar_path(cache)))
  testthat::expect_identical(
    OEDloadR:::oed_cache_read_metadata(cache)$destination_path,
    normalizePath(cache, mustWork = FALSE)
  )
  testthat::expect_false(any(grepl("download-|backup-", list.files(cache_dir, all.files = TRUE))))

  before_failure <- OEDloadR:::oed_cache_hash(cache)
  warning_seen <- FALSE
  failed <- withCallingHandlers(
    OEDloadR:::oed_cache_download(
      request = httr2::request("https://example.org/laus.xlsx"),
      destination_path = cache,
      command = "test",
      dataset = "LAUS",
      source_url = "https://example.org/laus.xlsx",
      Refresh = "always",
      perform = function(request, path) stop("simulated network failure")
    ),
    warning = function(condition) {
      warning_seen <<- grepl("using stale cached data", conditionMessage(condition), fixed = TRUE)
      invokeRestart("muffleWarning")
    }
  )
  testthat::expect_true(warning_seen)
  testthat::expect_identical(failed$status, "stale_cache_used")
  testthat::expect_identical(OEDloadR:::oed_cache_hash(cache), before_failure)
})

testthat::test_that("download diagnostics distinguish cache outcomes", {
  plan <- tibble::tibble(
    download_status = c("downloaded", "refreshed", "cached", "stale_cache_used", "failed"),
    cache_age_days = c(NA_real_, 1, 2, 8, NA_real_),
    refresh_reason = c("missing_workbook", "max_age_exceeded", "within_max_age", "refresh_failed", "download_failed"),
    metadata_path = paste0("metadata-", seq_len(5), ".json")
  )
  result <- OEDloadR:::oed_attach_download_diagnostics(
    tibble::tibble(value = 1),
    command = "test",
    plan = plan
  )
  diagnostics <- OED_Diagnostics(result)
  testthat::expect_equal(diagnostics$files_downloaded, 1)
  testthat::expect_equal(diagnostics$files_refreshed, 1)
  testthat::expect_equal(diagnostics$files_reused, 1)
  testthat::expect_equal(diagnostics$files_stale_cache_used, 1)
  testthat::expect_equal(diagnostics$files_failed, 1)
  testthat::expect_equal(diagnostics$refresh_reasons, plan$refresh_reason)
  testthat::expect_equal(diagnostics$metadata_paths, plan$metadata_path)
})
