test_that("download helper parsers return expected shapes", {
  parsed <- gexp_parse_gse_inputs(
    analysis_type = "merged",
    rnaseq_gses = "GSE1, GSE2",
    microarray_gses = "GSE3",
    dataset_mode = "single"
  )
  expect_equal(length(parsed$rnaseq_ids), 1)
  expect_equal(length(parsed$micro_ids), 1)
  expect_true(parsed$dataset_mode %in% c("single", "multi"))
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
