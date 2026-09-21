test_that(".gexpipe_geo_series_folder uses nnn suffix", {
  fn <- getFromNamespace(".gexpipe_geo_series_folder", "GExPipe")
  expect_equal(fn("GSE89076"), "GSE89nnn")
  expect_equal(fn("GSE50760"), "GSE50nnn")
})

test_that(".gexpipe_classify_geo_error keeps network detail", {
  skip("Aspirational: .gexpipe_classify_geo_error was never implemented as a named helper; equivalent inline logic lives in server_download.R's network-error grepl check.")
  fn <- getFromNamespace(".gexpipe_classify_geo_error", "GExPipe")
  expect_match(fn("cannot open URL: HTTP status was '403 Forbidden'"), "network/HTTP")
  expect_match(fn("destfile 'x.gz' not found"), "destfile")
  expect_equal(fn("unexpected SOFT parser crash"), "unexpected SOFT parser crash")
})

test_that(".gexpipe_clear_stale_geo_cache removes truncated GSE files", {
  skip("Aspirational: .gexpipe_clear_stale_geo_cache was never implemented as a named helper; equivalent truncated-file detection lives in .gexpipe_downloaded_file_ok().")
  fn <- getFromNamespace(".gexpipe_clear_stale_geo_cache", "GExPipe")
  td <- tempfile("gexp_geo_cache_")
  dir.create(td, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  stale <- file.path(td, "GSE89076_series_matrix.txt.gz")
  keep <- file.path(td, "GSE89076_keep.txt.gz")
  writeBin(raw(100), stale)
  writeBin(raw(4096), keep)
  fn("GSE89076", td)
  expect_false(file.exists(stale))
  expect_true(file.exists(keep))
})

test_that(".gexpipe_fetch_series_matrix_files reuses a local cache file", {
  skip("Aspirational: .gexpipe_fetch_series_matrix_files was never implemented as a named helper; series-matrix URL/caching logic lives in .gexpipe_series_matrix_urls() and gexp_fetch_geo_series_matrix_metadata().")
  fn <- getFromNamespace(".gexpipe_fetch_series_matrix_files", "GExPipe")
  td <- tempfile("gexp_matrix_")
  dir.create(td, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  cached <- file.path(td, "GSE89076_series_matrix.txt.gz")
  writeBin(raw(4096), cached)
  got <- fn("GSE89076", td)
  expect_true(cached %in% got)
})

test_that("download helper parsers return expected shapes", {
  parsed <- gexp_parse_gse_inputs(
    analysis_type = "merged",
    rnaseq_gses = "GSE1, GSE2",
    microarray_gses = "GSE3",
    dataset_mode = "single"
  )
  expect_equal(parsed$rnaseq_ids, c("GSE1", "GSE2"))
  expect_equal(parsed$micro_ids, "GSE3")
  expect_equal(parsed$dataset_mode, "multi")
})

test_that("runGExPipe app directory exists in installed package", {
  app_dir <- system.file("shinyapp", package = "GExPipe")
  expect_true(nzchar(app_dir))
  expect_true(dir.exists(app_dir))
  expect_true(file.exists(file.path(app_dir, "app.R")))
})

test_that("shiny app object can be created from app directory", {
  skip_if_not_installed("shiny")
  app_dir <- system.file("shinyapp", package = "GExPipe")
  app_obj <- shiny::shinyAppDir(app_dir)
  expect_s3_class(app_obj, "shiny.appobj")
})

test_that("runGExPipe returns a shiny app object (does not launch)", {
  skip_if_not_installed("shiny")
  app <- runGExPipe(launch.browser = FALSE, host = "127.0.0.1", port = 0)
  expect_s3_class(app, "shiny.appobj")
})
