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
