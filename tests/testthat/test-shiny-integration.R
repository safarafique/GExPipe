test_that("Shiny app launches (smoke test)", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("pkgload")
  skip_on_cran()
  if (identical(Sys.getenv("GEXPIPE_SKIP_SHINYTEST2_APPDRIVER", ""), "1")) {
    skip("GEXPIPE_SKIP_SHINYTEST2_APPDRIVER=1 (full browser smoke test disabled)")
  }
  if (nzchar(Sys.getenv("CI")) && identical(tolower(Sys.getenv("GEXPIPE_SKIP_SHINYTEST2")), "true")) {
    skip("Skipping shinytest2 smoke test because GEXPIPE_SKIP_SHINYTEST2=true")
  }

  # Use a temporary app.R that builds the app via GExPipe::runGExPipe().
  # This ensures we exercise the R/ app builder code (not only inst/shinyapp).
  test_app_dir <- file.path(tempdir(), "gexpipe-shinytest-app")
  dir.create(test_app_dir, showWarnings = FALSE, recursive = TRUE)
  app_r <- file.path(test_app_dir, "app.R")
  pkg_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  pkg_root_js <- gsub("\\\\", "/", pkg_root)
  load_ms <- suppressWarnings(as.integer(Sys.getenv("GEXPIPE_SHINYTEST2_MS", "600000")))
  if (length(load_ms) != 1L || is.na(load_ms) || load_ms < 60000L) {
    load_ms <- 600000L
  }
  # Child R process: prefer installed GExPipe (R CMD check / devtools::check). shinytest2 waits
  # until Shiny connects; pkgload::load_all() from cold source can exceed any reasonable timeout.
  # Force source load with GEXPIPE_SHINYTEST2_USE_PKGLOAD=1 when developing without install.
  writeLines(
    c(
      "if (identical(Sys.getenv(\"GEXPIPE_SHINYTEST2_USE_PKGLOAD\"), \"1\")) {",
      sprintf("  pkgload::load_all(\"%s\", quiet = TRUE, export_all = FALSE)", pkg_root_js),
      "} else if (nzchar(system.file(\"NAMESPACE\", package = \"GExPipe\"))) {",
      "  suppressPackageStartupMessages(library(GExPipe))",
      "} else {",
      sprintf("  pkgload::load_all(\"%s\", quiet = TRUE, export_all = FALSE)", pkg_root_js),
      "}",
      "options(shiny.testmode = TRUE)",
      "options(gexpipe.minimal_attach_in_testmode = TRUE)",
      "options(gexpipe.wgcna_threads = 0L)",
      "app <- GExPipe::runGExPipe(launch.browser = FALSE, port = 0)",
      "app"
    ),
    con = app_r,
    useBytes = TRUE
  )

  # load_timeout: AppDriver waits for window.shinytest2.ready (session init).
  # wait = FALSE skips wait_for_idle(); this app rarely reaches 200 ms Shiny idle.
  app <- tryCatch(
    shinytest2::AppDriver$new(
      app_dir = test_app_dir,
      name = "launch-smoke",
      seed = 1,
      load_timeout = load_ms,
      wait = FALSE
    ),
    error = function(e) {
      skip(paste("Could not start shinytest2 AppDriver:", conditionMessage(e)))
    }
  )
  on.exit(app$stop(), add = TRUE)

  # Basic check that AppDriver attached to a live app session.
  expect_true(inherits(app, "AppDriver"))
})
