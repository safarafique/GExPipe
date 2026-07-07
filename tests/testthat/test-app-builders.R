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

test_that("gexp_ui_plot_download_bar resolves when loading tab UI", {
  skip_if_not_installed("shiny")
  skip_on_cran()

  op <- options(
    shiny.testmode = TRUE,
    gexpipe.minimal_attach_in_testmode = TRUE,
    gexpipe.wgcna_threads = 0L
  )
  on.exit(options(op), add = TRUE)

  gexp_ui_nomogram <- getFromNamespace("gexp_ui_nomogram", "GExPipe")
  gexp_ui_batch <- getFromNamespace("gexp_ui_batch", "GExPipe")
  gexp_ui_results_summary <- getFromNamespace("gexp_ui_results_summary", "GExPipe")

  expect_no_error(ui_nom <- gexp_ui_nomogram())
  expect_no_error(ui_batch <- gexp_ui_batch())
  expect_no_error(ui_rs <- gexp_ui_results_summary())

  expect_no_error(ui_nom <- gexp_ui_nomogram())
  expect_no_error(ui_batch <- gexp_ui_batch())
  expect_no_error(ui_rs <- gexp_ui_results_summary())

  expect_true(inherits(ui_nom, "shiny.tag"))
  expect_true(inherits(ui_batch, "shiny.tag"))
  expect_true(inherits(ui_rs, "shiny.tag"))
  expect_no_error(nom_ui <- ui_nom)
  expect_no_error(batch_ui <- ui_batch)
  expect_no_error(rs_ui <- ui_rs)
  expect_true(inherits(nom_ui, "shiny.tag"))
  expect_true(inherits(batch_ui, "shiny.tag"))
  expect_true(inherits(rs_ui, "shiny.tag"))
})

