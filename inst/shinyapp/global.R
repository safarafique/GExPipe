# ==============================================================================
# GLOBAL.R  —  GExPipe auto-setup: installs all packages, loads libraries
# ==============================================================================
# Users never need to manually install anything.
# Just run: shiny::runGitHub("GExPipe", "safarafique", destdir = tempfile())
# ==============================================================================

cat("\n")
cat("======================================================================\n")
cat("  GExPipe — Gene Expression Pipeline\n")
cat("  R", R.version.string, "\n")
cat("======================================================================\n")

options(timeout = 600)   # allow long downloads during install

# ==============================================================================
# STEP 1  —  Ensure BiocManager is present
# ==============================================================================
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  cat("  [1/4] Installing BiocManager...\n")
  install.packages("BiocManager", repos = "https://cloud.r-project.org", quiet = TRUE)
}

bioc_ver <- tryCatch(as.character(BiocManager::version()), error = function(e) "?")
cat("  Bioconductor :", bioc_ver, "\n\n")

# ==============================================================================
# STEP 2  —  Define all required and optional packages
# ==============================================================================

# All packages GExPipe needs — BiocManager::install() handles both
# Bioconductor and CRAN packages and selects versions matching your R version
# automatically (Bioc 3.19 for R 4.4, Bioc 3.21 for R 4.5, Bioc 3.22 for R 4.6).
.gexpipe_all_required <- c(
  # Shiny UI
  "shiny", "shinydashboard", "shinyjs", "DT",
  # Bioconductor core
  "Biobase", "GEOquery", "limma", "AnnotationDbi", "org.Hs.eg.db",
  "edgeR", "DESeq2", "sva", "clusterProfiler", "enrichplot", "STRINGdb",
  "WGCNA",
  # CRAN analysis
  "dplyr", "data.table", "tibble", "tidyr", "tidygraph", "reshape2",
  "ggplot2", "ggpubr", "ggrepel", "ggraph", "gridExtra", "RColorBrewer",
  "pheatmap", "circlize", "corrplot", "scales",
  "VennDiagram", "UpSetR", "igraph",
  "randomForest", "caret", "e1071", "glmnet", "pROC", "kernlab",
  "msigdbr", "R.utils", "dynamicTreeCut", "parallel",
  # Feature packages (formerly optional — now required for full functionality)
  "biomaRt", "Boruta", "cicerone", "mixOmics",
  "xgboost", "SHAPforxgboost", "rms", "rmda"
)

.gexpipe_all_optional <- character(0)

# ==============================================================================
# STEP 3  —  Install missing packages via a background Rscript subprocess
# ==============================================================================
# Running in a subprocess means zero packages are loaded in that process —
# no Windows DLL locks — so every package can be installed regardless of
# what is already loaded in the parent (RStudio) session.
.gexpipe_batch_install <- function(pkgs, label) {
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(missing_pkgs) == 0L) {
    cat("  ", label, ": all present\n", sep = "")
    return(invisible(character(0)))
  }
  cat("  ", label, ": installing ", length(missing_pkgs), " package(s):\n", sep = "")
  cat("    ", paste(missing_pkgs, collapse = ", "), "\n")
  cat("  Running in a background R process to avoid Windows DLL locks...\n")

  lib_path    <- .libPaths()[1L]
  parent_libs <- paste0('"', gsub("\\\\", "/", .libPaths()), '"', collapse = ", ")
  pkg_vec     <- paste0('"', missing_pkgs, '"', collapse = ", ")

  # Remove stale 00LOCK dirs in parent before spawning subprocess
  lock_dirs <- list.files(lib_path, pattern = "^00LOCK-", full.names = TRUE)
  if (length(lock_dirs) > 0L)
    unlink(lock_dirs, recursive = TRUE, force = TRUE)

  script <- c(
    paste0('.libPaths(c(', parent_libs, '))'),
    'options(',
    '  repos               = c(CRAN = "https://cloud.r-project.org"),',
    '  timeout             = 2400L,',
    '  download.file.method = "libcurl"',
    ')',
    paste0('.lib  <- "', gsub("\\\\", "/", lib_path), '"'),
    paste0('.pkgs <- c(', pkg_vec, ')'),
    '.locks <- list.files(.lib, pattern = "^00LOCK-", full.names = TRUE)',
    'if (length(.locks) > 0L) unlink(.locks, recursive = TRUE, force = TRUE)',
    'if (!requireNamespace("BiocManager", quietly = TRUE))',
    '  install.packages("BiocManager", lib = .lib, quiet = FALSE)',
    'cat("GExPipe subprocess: installing", length(.pkgs), "package(s)\\n")',
    'BiocManager::install(.pkgs, lib = .lib, ask = FALSE, update = FALSE,',
    '                     force = TRUE, quiet = FALSE)'
  )

  tmp_script <- tempfile(pattern = "gexpipe_install_", fileext = ".R")
  on.exit(unlink(tmp_script), add = TRUE)
  writeLines(script, tmp_script)

  rscript   <- file.path(R.home("bin"), "Rscript")
  exit_code <- tryCatch(
    system2(rscript,
            args    = c("--vanilla", "--no-save", shQuote(tmp_script)),
            stdout  = "", stderr  = "",
            timeout = 2400L),
    error = function(e) { cat("  Could not launch subprocess:", conditionMessage(e), "\n"); 1L }
  )

  if (!identical(exit_code, 0L))
    cat("  Subprocess exited with code", exit_code,
        "— some packages may not have installed. Try running again.\n")

  still_missing <- missing_pkgs[
    !vapply(missing_pkgs, requireNamespace, logical(1L), quietly = TRUE)
  ]
  if (length(still_missing) > 0L)
    cat("  Still missing:", paste(still_missing, collapse = ", "), "\n",
        "  Run: BiocManager::install(c(",
        paste0('"', still_missing, '"', collapse = ", "), "))\n")

  invisible(still_missing)
}

cat("  [2/4] Checking and installing required packages...\n")
failed_required <- .gexpipe_batch_install(.gexpipe_all_required, "Required")

cat("\n  [3/4] Checking and installing optional packages...\n")
.gexpipe_batch_install(.gexpipe_all_optional, "Optional")

# ==============================================================================
# STEP 4  —  Load all libraries
# ==============================================================================
cat("\n  [4/4] Loading libraries...\n")

.gexpipe_load_quietly <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) return(FALSE)
  if (pkg == "parallel") { library(parallel); return(TRUE) }
  tryCatch(
    { suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE)); TRUE },
    error = function(e) FALSE
  )
}

loaded   <- character(0)
failed_load <- character(0)
for (p in c(.gexpipe_all_required, .gexpipe_all_optional)) {
  if (.gexpipe_load_quietly(p)) loaded <- c(loaded, p)
  else                          failed_load <- c(failed_load, p)
}

# ==============================================================================
# Runtime options
# ==============================================================================
options(shiny.maxRequestSize = 500 * 1024^2)
options(timeout = 3600)
options(stringsAsFactors = FALSE)

if (isNamespaceLoaded("WGCNA")) {
  tryCatch({
    if (exists("enableWGCNAThreads", mode = "function", where = asNamespace("WGCNA")))
      WGCNA::enableWGCNAThreads(nThreads = max(1L, parallel::detectCores() - 1L))
  }, error = function(e) NULL)
}

# ==============================================================================
# Summary
# ==============================================================================
cat("\n----------------------------------------------------------------------\n")
cat("  Loaded       :", length(loaded), "packages\n")
if (length(failed_load) > 0L)
  cat("  Not loaded   :", paste(failed_load, collapse = ", "), "\n")
if (length(failed_required) > 0L) {
  cat("  STILL MISSING:", paste(failed_required, collapse = ", "), "\n")
  cat("  Some features may not work. Try running again or check your internet connection.\n")
} else {
  cat("  Status       : Ready\n")
}
cat("======================================================================\n\n")

# ==============================================================================
# HELPER LOADING (prefer package namespace; fallback to R/ source checkout)
# ==============================================================================
if (!requireNamespace("GExPipe", quietly = TRUE)) {
  helper_candidates <- c(
    file.path(getwd(), "..", "..", "R", "gexpipe_shiny_helpers.R"),
    file.path(getwd(), "R", "gexpipe_shiny_helpers.R")
  )
  helper_file <- helper_candidates[file.exists(helper_candidates)][1]
  if (is.na(helper_file) || !nzchar(helper_file)) {
    stop("GExPipe helper code not found. Run via GExPipe::runGExPipe() or from package root.")
  }
  source(helper_file, local = TRUE)
}

tryCatch({
  if (requireNamespace("GExPipe", quietly = TRUE)) {
    ns <- asNamespace("GExPipe")
    to_pull <- c(
      "theme_publication", "palette_primary", ".gexpipe_log_file", "app_log", "safe_run",
      "IMAGE_DPI", "CSV_EXPORT_DIR",
      "detect_gene_id_format",
      "probe_ids_to_symbol_hugene_db", "probe_ids_to_symbol_gpl", "probe_ids_to_symbol_biomart",
      "map_microarray_ids", "entrez_to_symbol_biomart", "any_id_to_symbol",
      "get_platform_for_gse", "run_gse_annotation_and_download",
      "convert_ids_to_symbols_simple", "convert_rnaseq_ids",
      "download_ncbi_raw_counts", "download_ncbi_raw_counts_best",
      "read_count_matrix", "classify_groups",
      "normalize_microarray", "GPL_USE_OLIGO", "normalize_microarray_rma", "normalize_rnaseq",
      "gexp_parse_gse_inputs", "gexp_prepare_download_dirs", "gexp_download_finalize_common_genes",
      "gexp_fetch_geo_series_matrix_metadata", "gexp_no_common_genes_diagnostic_log",
      "gexp_rebuild_all_genes_list", "gexp_download_normalize_ids_for_overlap",
      "gexp_download_one_microarray_gse", "gexp_download_one_rnaseq_gse",
      "gexp_qc_detect_outliers", "gexp_qc_exclude_samples", "gexp_qc_gene_overlap_summary",
      "gexp_qc_prepare_venn_sets", "gexp_qc_prepare_upset_data",
      "gexp_qc_prepare_boxplot_data", "gexp_qc_prepare_density_data",
      "gexp_register_pipeline_observers",
      "gexp_register_navigation_observers",
      "gexp_register_workspace_observers",
      "gexp_register_help_observers",
      "gexp_user_guideline_modal_ui"
    )
    for (nm in to_pull) {
      if (exists(nm, envir = ns, inherits = FALSE))
        assign(nm, get(nm, envir = ns, inherits = FALSE), envir = environment())
    }
  }
}, error = function(e) NULL)

# Source any local R/ pipeline files (covers runGitHub checkout path)
local_r_files <- c(
  file.path(getwd(), "..", "..", "R", "gexpipe_shiny_helpers.R"),
  file.path(getwd(), "..", "..", "R", "utils_shiny_app.R"),
  file.path(getwd(), "..", "..", "R", "gexp_download_pipeline.R"),
  file.path(getwd(), "..", "..", "R", "gexp_qc_pipeline.R"),
  file.path(getwd(), "..", "..", "R", "gexp_normalize_pipeline.R"),
  file.path(getwd(), "..", "..", "R", "gexp_batch_pipeline.R"),
  file.path(getwd(), "..", "..", "R", "gexp_de_pipeline.R"),
  file.path(getwd(), "..", "..", "R", "gexp_wgcna_pipeline.R"),
  file.path(getwd(), "..", "..", "R", "observers_pipeline.R"),
  file.path(getwd(), "..", "..", "R", "observers_navigation.R"),
  file.path(getwd(), "..", "..", "R", "observers_workspace.R"),
  file.path(getwd(), "..", "..", "R", "observers_help.R"),
  file.path(getwd(), "R", "gexpipe_shiny_helpers.R"),
  file.path(getwd(), "R", "utils_shiny_app.R"),
  file.path(getwd(), "R", "gexp_download_pipeline.R"),
  file.path(getwd(), "R", "gexp_qc_pipeline.R"),
  file.path(getwd(), "R", "gexp_normalize_pipeline.R"),
  file.path(getwd(), "R", "gexp_batch_pipeline.R"),
  file.path(getwd(), "R", "gexp_de_pipeline.R"),
  file.path(getwd(), "R", "gexp_wgcna_pipeline.R"),
  file.path(getwd(), "R", "observers_pipeline.R"),
  file.path(getwd(), "R", "observers_navigation.R"),
  file.path(getwd(), "R", "observers_workspace.R"),
  file.path(getwd(), "R", "observers_help.R")
)
for (rf in unique(local_r_files[file.exists(local_r_files)]))
  tryCatch(source(rf, local = TRUE), error = function(e) NULL)

# Verify core functions are present
.gexpipe_has_core_fn <- function(nm) {
  if (exists(nm, mode = "function", inherits = TRUE)) return(TRUE)
  if (requireNamespace("GExPipe", quietly = TRUE)) {
    ns <- asNamespace("GExPipe")
    if (exists(nm, envir = ns, inherits = FALSE, mode = "function")) return(TRUE)
  }
  FALSE
}

core_needed <- c(
  "gexp_parse_gse_inputs", "gexp_prepare_download_dirs", "gexp_download_finalize_common_genes",
  "gexp_download_normalize_ids_for_overlap", "gexp_download_one_microarray_gse",
  "gexp_download_one_rnaseq_gse", "gexp_qc_detect_outliers", "gexp_qc_exclude_samples",
  "gexp_register_pipeline_observers", "gexp_register_navigation_observers",
  "gexp_register_workspace_observers", "gexp_register_help_observers"
)
missing_core <- core_needed[!vapply(core_needed, .gexpipe_has_core_fn, logical(1))]
if (length(missing_core) > 0) {
  stop(
    "Missing core GExPipe functions: ", paste(missing_core, collapse = ", "),
    ". Reinstall/update the package or run from the package root."
  )
}
