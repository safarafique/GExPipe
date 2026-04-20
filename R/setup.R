#' Set up GExPipe dependencies and optionally launch the app
#'
#' Detects your R version, installs or updates all required packages via
#' `BiocManager` (which automatically selects the correct Bioconductor release
#' for your R version — 3.19 for R 4.4, 3.21 for R 4.5, 3.22 for R 4.6),
#' and optionally installs recommended optional packages and launches the app.
#'
#' Run this once on a new system or after upgrading R to ensure all packages
#' are at the correct versions.
#'
#' @param update Logical. If `TRUE` (default), update already-installed
#'   packages to the versions matching your current Bioconductor release.
#'   Set to `FALSE` to only install missing packages without updating existing ones.
#' @param optional Logical. If `TRUE` (default), also install optional packages
#'   (`Boruta`, `xgboost`, `SHAPforxgboost`, `mixOmics`, `rms`, `rmda`,
#'   `cicerone`, `biomaRt`). These unlock additional ML methods, the guided
#'   tour, the nomogram, and Ensembl ID mapping.
#' @param launch Logical. If `TRUE`, launch the GExPipe Shiny app after
#'   setup completes. Default `FALSE`.
#' @param port Integer. Port for the Shiny app when `launch = TRUE`.
#' @param ... Additional arguments passed to [shiny::runApp()] when
#'   `launch = TRUE`.
#'
#' @return Invisibly returns a named logical vector indicating which packages
#'   were successfully installed/available.
#'
#' @examples
#' # Check and install missing packages only (no updates, no launch)
#' if (interactive()) {
#'   gexpipe_setup(update = FALSE, optional = FALSE, launch = FALSE)
#' }
#'
#' @export
gexpipe_setup <- function(update   = TRUE,
                           optional = TRUE,
                           launch   = FALSE,
                           port     = 3838L,
                           ...) {

  ## ── 1. R version check ────────────────────────────────────────────────────
  r_ver <- getRversion()
  if (r_ver < "4.4.0") {
    stop(
      "GExPipe requires R >= 4.4.0. Your version is ", r_ver, ".\n",
      "Please update R from https://cran.r-project.org and re-run gexpipe_setup()."
    )
  }

  ## ── 2. Detect and report Bioconductor version ─────────────────────────────
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    message("Installing BiocManager...")
    utils::install.packages("BiocManager", quiet = TRUE)
  }

  bioc_ver <- tryCatch(BiocManager::version(), error = function(e) NA)
  message(
    "\n── GExPipe setup ──────────────────────────────────────────────────────\n",
    "  R version          : ", r_ver, "\n",
    "  Bioconductor       : ", if (is.na(bioc_ver)) "unknown" else bioc_ver, "\n",
    "  update existing    : ", update, "\n",
    "  install optional   : ", optional, "\n",
    "──────────────────────────────────────────────────────────────────────"
  )

  ## ── 3. Required packages ──────────────────────────────────────────────────
  # BiocManager::install() automatically selects versions matching your
  # Bioconductor release, so no hardcoded version numbers are needed here.
  bioc_required <- c(
    "AnnotationDbi", "Biobase", "clusterProfiler", "DESeq2", "edgeR",
    "enrichplot", "GEOquery", "limma", "msigdbr", "org.Hs.eg.db",
    "STRINGdb", "sva", "WGCNA"
  )

  cran_required <- c(
    "shiny", "shinydashboard", "shinyjs", "DT",
    "caret", "circlize", "corrplot", "data.table", "dplyr", "dynamicTreeCut",
    "e1071", "ggplot2", "ggpubr", "ggraph", "ggrepel", "glmnet",
    "gridExtra", "igraph", "kernlab", "parallel", "pheatmap", "pROC",
    "R.utils", "randomForest", "RColorBrewer", "reshape2", "scales",
    "tibble", "tidygraph", "tidyr", "UpSetR", "VennDiagram"
  )

  ## ── 4. Optional packages ──────────────────────────────────────────────────
  bioc_optional  <- c("biomaRt", "mixOmics")
  cran_optional  <- c("Boruta", "cicerone", "rms", "rmda",
                      "SHAPforxgboost", "xgboost")

  ## ── 5. Install / update via BiocManager ───────────────────────────────────
  all_bioc <- if (optional) c(bioc_required, bioc_optional) else bioc_required
  all_cran <- if (optional) c(cran_required, cran_optional) else cran_required

  message("\nInstalling/updating Bioconductor packages (this may take several minutes)...")
  tryCatch(
    BiocManager::install(all_bioc, update = update, ask = FALSE, quiet = FALSE),
    error = function(e) {
      message("  Warning: some Bioconductor packages could not be installed: ",
              conditionMessage(e))
    }
  )

  message("\nInstalling/updating CRAN packages...")
  to_install_cran <- all_cran[!vapply(all_cran, requireNamespace,
                                       logical(1L), quietly = TRUE)]
  if (length(to_install_cran) > 0L) {
    tryCatch(
      utils::install.packages(to_install_cran,
                               repos = "https://cloud.r-project.org",
                               quiet = TRUE),
      error = function(e) {
        message("  Warning: some CRAN packages could not be installed: ",
                conditionMessage(e))
      }
    )
  } else if (!update) {
    message("  All CRAN packages already present.")
  }

  ## ── 6. Verify ─────────────────────────────────────────────────────────────
  all_pkgs <- unique(c(all_bioc, all_cran))
  status   <- vapply(all_pkgs, requireNamespace, logical(1L), quietly = TRUE)

  missing  <- names(status)[!status]
  if (length(missing) > 0L) {
    message(
      "\n  Could not install: ", paste(missing, collapse = ", "),
      "\n  Re-run gexpipe_setup() or install manually:\n",
      "    BiocManager::install(c(", paste0('"', missing, '"', collapse = ", "), "))"
    )
  } else {
    message("\n  All packages installed successfully.")
  }

  ## ── 7. Summary ────────────────────────────────────────────────────────────
  message(
    "\n── Setup complete ─────────────────────────────────────────────────────\n",
    "  Packages checked   : ", length(all_pkgs), "\n",
    "  Ready              : ", sum(status), "\n",
    "  Missing            : ", length(missing), "\n",
    "──────────────────────────────────────────────────────────────────────"
  )

  ## ── 8. Optionally launch ──────────────────────────────────────────────────
  if (isTRUE(launch) && length(missing) == 0L) {
    message("Launching GExPipe app on port ", port, "...")
    app <- runGExPipe(launch.browser = TRUE, port = as.integer(port))
    shiny::runApp(app, port = as.integer(port), ...)
  } else if (isTRUE(launch) && length(missing) > 0L) {
    message("App not launched: fix missing packages above first, then run:\n",
            "  shiny::runApp(GExPipe::runGExPipe(), port = ", port, ")")
  }

  invisible(status)
}
