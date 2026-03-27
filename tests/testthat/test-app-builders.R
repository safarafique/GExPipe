test_that("internal app UI and server builders construct without error", {
  skip_if_not_installed("shiny")
  skip_on_cran()

  # These are internal helpers; we only smoke-test construction here.
  expect_no_error(GExPipe:::gexp_app_ui())
  expect_no_error({
    shiny::shinyApp(ui = GExPipe:::gexp_app_ui(), server = GExPipe:::gexp_app_server)
  })
})

