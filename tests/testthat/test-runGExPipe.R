test_that("runGExPipe returns a Shiny app object without calling runApp", {
  skip_if_not_installed("shiny")

  op <- options(
    shiny.testmode = TRUE,
    gexpipe.minimal_attach_in_testmode = TRUE,
    gexpipe.wgcna_threads = 0L
  )
  on.exit(options(op), add = TRUE)

  app <- runGExPipe(launch.browser = FALSE, port = 0L)
  expect_true(inherits(app, "shiny.appobj"))
  # shiny.appobj layout (Shiny >= 1.13): httpHandler + serverFuncSource, not top-level ui/server.
  expect_type(app$httpHandler, "closure")
  expect_type(app$serverFuncSource, "closure")
})
