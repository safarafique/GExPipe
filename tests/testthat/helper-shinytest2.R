## Shared shinytest2 helpers for GExPipe integration tests.

.gexpipe_shinytest2_skip <- function() {
  if (!requireNamespace("shinytest2", quietly = TRUE)) {
    testthat::skip("shinytest2 not installed")
  }
  if (!requireNamespace("chromote", quietly = TRUE)) {
    testthat::skip("chromote not installed (required by shinytest2 AppDriver)")
  }
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    testthat::skip("pkgload not installed")
  }
  if (identical(Sys.getenv("GEXPIPE_SKIP_SHINYTEST2", ""), "1")) {
    testthat::skip("GEXPIPE_SKIP_SHINYTEST2=1")
  }
  if (nzchar(Sys.getenv("CI")) && identical(tolower(Sys.getenv("GEXPIPE_SKIP_SHINYTEST2")), "true")) {
    testthat::skip("GEXPIPE_SKIP_SHINYTEST2=true in CI")
  }
  invisible(TRUE)
}

.gexpipe_shinytest2_load_ms <- function() {
  ms <- suppressWarnings(as.integer(Sys.getenv("GEXPIPE_SHINYTEST2_MS", "300000")))
  if (length(ms) != 1L || is.na(ms) || ms < 60000L) {
    300000L
  } else {
    ms
  }
}

.gexpipe_shinytest2_pkg_root <- function() {
  normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
}

.gexpipe_shinytest2_write_app_r <- function(app_r) {
  pkg_root_js <- gsub("\\\\", "/", .gexpipe_shinytest2_pkg_root())
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
      "options(gexpipe.prelaunch_install_done = TRUE)",
      "app <- GExPipe::runGExPipe(launch.browser = FALSE, port = 0)",
      "app"
    ),
    con = app_r,
    useBytes = TRUE
  )
}

.gexpipe_shinytest2_driver <- function(name = "gexpipe") {
  .gexpipe_shinytest2_skip()
  test_app_dir <- file.path(tempdir(), "gexpipe-shinytest2", name)
  dir.create(test_app_dir, recursive = TRUE, showWarnings = FALSE)
  app_r <- file.path(test_app_dir, "app.R")
  .gexpipe_shinytest2_write_app_r(app_r)
  tryCatch(
    shinytest2::AppDriver$new(
      app_dir = test_app_dir,
      name = name,
      seed = 42L,
      load_timeout = .gexpipe_shinytest2_load_ms(),
      wait = FALSE
    ),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("Chrome|chromote|debugging port", msg, ignore.case = TRUE)) {
        testthat::skip(paste("chromote/Chrome unavailable:", msg))
      }
      testthat::skip(paste("Could not start shinytest2 AppDriver:", msg))
    }
  )
}

.gexpipe_shinytest2_enter_analysis <- function(app) {
  app$click("go_to_analysis")
  app$wait_for_value(input = "analysis_type", timeout = 120000L)
  invisible(app)
}
