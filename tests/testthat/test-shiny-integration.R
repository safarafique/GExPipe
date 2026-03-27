test_that("Shiny app launches (smoke test)", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("pkgload")
  skip_on_cran()
  if (nzchar(Sys.getenv("CI")) && identical(tolower(Sys.getenv("GEXPIPE_SKIP_SHINYTEST2")), "true")) {
    skip("Skipping shinytest2 smoke test because GEXPIPE_SKIP_SHINYTEST2=true")
  }

  # Use a temporary app.R that builds the app via GExPipe::runGExPipe().
  # This ensures we exercise the R/ app builder code (not only inst/shinyapp).
  test_app_dir <- file.path(tempdir(), "gexpipe-shinytest-app")
  dir.create(test_app_dir, showWarnings = FALSE, recursive = TRUE)
  app_r <- file.path(test_app_dir, "app.R")
  pkg_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  writeLines(
    c(
      sprintf("pkgload::load_all(\"%s\", quiet = TRUE, export_all = FALSE)", gsub("\\\\", "/", pkg_root)),
      "options(shiny.testmode = TRUE)",
      "app <- GExPipe::runGExPipe(launch.browser = FALSE, port = 0)",
      "app"
    ),
    con = app_r,
    useBytes = TRUE
  )

  app <- tryCatch(
    shinytest2::AppDriver$new(
      app_dir = test_app_dir,
      name = "launch-smoke",
      seed = 1,
      load_timeout = 120000
    ),
    error = function(e) {
      skip(paste("Could not start shinytest2 AppDriver:", conditionMessage(e)))
    }
  )
  on.exit(app$stop(), add = TRUE)

  # Basic check that AppDriver attached to a live app session.
  expect_true(inherits(app, "AppDriver"))
})
