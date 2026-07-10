# ==============================================================================
# GLOBAL.R — thin entry for runGitHub / RStudio Run App on inst/shinyapp
# ==============================================================================
# All bootstrap logic lives in R/gexpipe_shinyapp_bootstrap.R (package namespace).
# Recommended entry: GExPipe::runGExPipe() then shiny::runApp().
# ==============================================================================

if (!exists(".gexpipe_shinyapp_ensure_package", mode = "function")) {
  .gexpipe_shinyapp_ensure_package <- function() {
    if (requireNamespace("GExPipe", quietly = TRUE)) {
      return(invisible(TRUE))
    }
    wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    for (d in unique(c(
      normalizePath(file.path(wd, "..", ".."), winslash = "/", mustWork = FALSE),
      wd
    ))) {
      desc <- file.path(d, "DESCRIPTION")
      if (!file.exists(desc)) next
      hdr <- tryCatch(readLines(desc, n = 12L, warn = FALSE), error = function(e) character(0))
      if (!any(grepl("^Package:\\s*GExPipe\\s*$", hdr))) next
      if (requireNamespace("pkgload", quietly = TRUE)) {
        if (isTRUE(tryCatch({
          pkgload::load_all(d, quiet = TRUE, export_all = FALSE)
          requireNamespace("GExPipe", quietly = TRUE)
        }, error = function(e) FALSE))) {
          return(invisible(TRUE))
        }
      }
    }
    invisible(FALSE)
  }
}

.gexpipe_shinyapp_ensure_package()

if (!requireNamespace("GExPipe", quietly = TRUE)) {
  stop(
    "GExPipe could not be loaded. Install pkgload, then run:\n",
    "  shiny::runGitHub('safarafique/GExPipe', ref = 'main', subdir = 'inst/shinyapp', destdir = tempfile())\n",
    "Or: BiocManager::install('GExPipe', dependencies = TRUE); GExPipe::runGExPipe()",
    call. = FALSE
  )
}

.gexpipe_bootstrap_result <- getFromNamespace("gexpipe_shinyapp_bootstrap", "GExPipe")(verbose = TRUE)

if (identical(.gexpipe_bootstrap_result$status, "blocked")) {
  ui <- .gexpipe_bootstrap_result$ui
  server <- .gexpipe_bootstrap_result$server
}
