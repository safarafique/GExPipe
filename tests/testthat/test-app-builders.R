test_that("internal app UI and server builders construct without error", {
  skip_if_not_installed("shiny")
  skip_on_cran()

  op <- options(
    shiny.testmode = TRUE,
    gexpipe.minimal_attach_in_testmode = TRUE,
    gexpipe.wgcna_threads = 0L
  )
  on.exit(options(op), add = TRUE)

  gexp_app_ui <- getFromNamespace("gexp_app_ui", "GExPipe")
  gexp_app_server <- getFromNamespace("gexp_app_server", "GExPipe")

  expect_no_error(gexp_app_ui())
  expect_no_error({
    shiny::shinyApp(ui = gexp_app_ui, server = gexp_app_server)
  })
})

