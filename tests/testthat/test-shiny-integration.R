test_that("shinytest2: AppDriver connects to runGExPipe app", {
  app <- .gexpipe_shinytest2_driver("launch-smoke")
  on.exit(app$stop(), add = TRUE)
  expect_true(inherits(app, "AppDriver"))
  expect_true(nzchar(app$get_html(selector = "body")))
})

test_that("shinytest2: welcome -> dashboard -> QC navigation", {
  app <- .gexpipe_shinytest2_driver("workflow")
  on.exit(app$stop(), add = TRUE)

  html <- app$get_html(selector = "body")
  expect_true(grepl("go_to_analysis|Start Analyzing|GExPipe", html, ignore.case = TRUE))

  .gexpipe_shinytest2_enter_analysis(app)
  expect_equal(app$get_value(input = "analysis_type"), "rnaseq")
  expect_true(app$exists(selector = "#start_processing"))
  expect_true(app$exists(selector = "#skip_load_btn"))

  app$click("next_page_download")
  app$wait_for_value(input = "sidebar_menu", value = "qc", timeout = 90000L)
  expect_equal(app$get_value(input = "sidebar_menu"), "qc")
})

test_that("shinytest2: start processing warns when no GSE IDs", {
  app <- .gexpipe_shinytest2_driver("geo-empty")
  on.exit(app$stop(), add = TRUE)

  .gexpipe_shinytest2_enter_analysis(app)
  app$set_inputs(analysis_type = "rnaseq")
  app$set_inputs(rnaseq_gses = "")
  app$click("start_processing")

  log <- .gexpipe_shinytest2_poll_output(
    app, "download_log", "Please specify at least one GSE ID",
    timeout_ms = 60000L
  )
  expect_true(grepl("Please specify at least one GSE ID", log, fixed = TRUE))
})

test_that("shinytest2: start processing with GSE ID updates download log", {
  .gexpipe_shinytest2_skip_geo()
  gse <- Sys.getenv("GEXPIPE_SHINYTEST2_GSE", "GSE62646")
  app <- .gexpipe_shinytest2_driver("geo-start")
  on.exit(app$stop(), add = TRUE)

  .gexpipe_shinytest2_enter_analysis(app)
  .gexpipe_shinytest2_start_geo_download(
    app,
    gse_id = gse,
    disease = "shinytest2 validation"
  )

  gse_pat <- gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", gse, perl = TRUE)
  log <- .gexpipe_shinytest2_poll_output(
    app,
    "download_log",
    paste0("RNA-seq:\\s*1|Downloading RNA-seq|", gse_pat, "|STEP 1|Error|error|failed"),
    timeout_ms = .gexpipe_shinytest2_geo_ms()
  )
  expect_true(nchar(log) > 20L)
  expect_true(grepl("GExPipe", log, fixed = TRUE))
  expect_false(grepl("Please specify at least one GSE ID", log, fixed = TRUE))
})
