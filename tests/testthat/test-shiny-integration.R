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
