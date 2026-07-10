#!/usr/bin/env Rscript
# Record or debug a GEO download shinytest2 scenario locally.
# Usage (from package root):
#   Rscript inst/scripts/record-shinytest2-geo.R
#
# Requires: shinytest2, chromote, pkgload

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Install pkgload first.", call. = FALSE)
}
if (!requireNamespace("shinytest2", quietly = TRUE)) {
  stop("Install shinytest2 first.", call. = FALSE)
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
  script <- sub("^--file=", "", file_arg[1L])
  root <- normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE)
} else {
  root <- normalizePath(getwd(), mustWork = TRUE)
}
setwd(root)
pkgload::load_all(root, quiet = TRUE)

options(shiny.testmode = TRUE)
options(gexpipe.minimal_attach_in_testmode = TRUE)
options(gexpipe.wgcna_threads = 0L)
options(gexpipe.prelaunch_install_done = TRUE)

gse <- Sys.getenv("GEXPIPE_SHINYTEST2_GSE", "GSE62646")
message("Recording shinytest2 GEO scenario for ", gse, " ...")

app <- GExPipe::runGExPipe(launch.browser = FALSE, port = 0L)
on.exit(if (inherits(app, "shiny.appobj")) invisible(NULL), add = TRUE)

shinytest2::record_test(
  app = app,
  name = "geo-download",
  record = c("input", "output", "screenshot")
)

# After recording, move generated files from ./_shinytest/ into
# tests/testthat/_shinytest/ if you want version-controlled snapshots.
