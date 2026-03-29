# ==============================================================================
# GLOBAL.R - Libraries, startup options, and helper loading
# ==============================================================================

# Timeout for GEO/downloads (user data), not for package installation
options(timeout = 600)

# ==============================================================================
# LOAD PACKAGES (auto-install missing packages on first run)
# ==============================================================================
cat("Loading packages (R ", R.version.string, ")...\n")

.gexpipe_ensure_pkg <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) return(TRUE)
  cat("  Installing missing package:", pkg, "...\n")
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager", quiet = TRUE)
  }
  tryCatch({
    BiocManager::install(pkg, ask = FALSE, update = FALSE, quiet = TRUE)
    requireNamespace(pkg, quietly = TRUE)
  }, error = function(e) {
    tryCatch({
      install.packages(pkg, quiet = TRUE)
      requireNamespace(pkg, quietly = TRUE)
    }, error = function(e2) FALSE)
  })
}

required_pkgs <- c(
  "shiny", "shinydashboard", "shinyjs", "DT",
  "Biobase", "GEOquery", "limma", "AnnotationDbi", "org.Hs.eg.db",
  "dplyr", "data.table", "edgeR", "sva", "ggplot2", "gridExtra", "RColorBrewer", "pheatmap", "ggrepel",
  "VennDiagram", "UpSetR", "WGCNA", "parallel", "clusterProfiler", "enrichplot", "circlize", "STRINGdb", "DESeq2",
  "igraph", "ggraph", "tidygraph", "tidyr", "randomForest", "caret", "e1071", "glmnet", "pROC", "kernlab",
  "tibble", "msigdbr", "ggpubr", "reshape2", "corrplot", "R.utils", "dynamicTreeCut", "scales"
)

gexpipe_missing <- character(0)
for (p in required_pkgs) {
  if (!.gexpipe_ensure_pkg(p)) gexpipe_missing <- c(gexpipe_missing, p)
}

for (p in required_pkgs) {
  if (p == "parallel") next
  tryCatch(
    suppressPackageStartupMessages(library(p, character.only = TRUE, quietly = TRUE)),
    error = function(e) NULL
  )
}
if (requireNamespace("parallel", quietly = TRUE)) library(parallel)

if (length(gexpipe_missing) > 0L) {
  warning("GExPipe: could not install: ", paste(gexpipe_missing, collapse = ", "),
          ". Some features may not work.")
}

optional_pkgs <- c("Boruta", "mixOmics", "xgboost", "SHAPforxgboost",
                   "rms", "rmda", "cicerone", "biomaRt")
for (p in optional_pkgs) {
  if (.gexpipe_ensure_pkg(p)) {
    tryCatch(suppressPackageStartupMessages(library(p, character.only = TRUE, quietly = TRUE)),
             error = function(e) NULL)
  }
}

# Runtime options
options(shiny.maxRequestSize = 500 * 1024^2)  # 500 MB
options(timeout = 3600)                        # allow long GEO downloads
options(stringsAsFactors = FALSE)
if (isNamespaceLoaded("WGCNA")) {
  tryCatch({
    if (exists("enableWGCNAThreads", mode = "function", where = asNamespace("WGCNA"))) {
      WGCNA::enableWGCNAThreads(nThreads = max(1L, parallel::detectCores() - 1L))
    }
  }, error = function(e) NULL)
}

cat("✓ All packages loaded\n")

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

cat("✓ Helper functions loaded\n")

# Pull helper implementations from installed package namespace when available.
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
      if (exists(nm, envir = ns, inherits = FALSE)) {
        assign(nm, get(nm, envir = ns, inherits = FALSE), envir = environment())
      }
    }
  }
    }, error = function(e) NULL)

# If running from source checkout with an older installed namespace, ensure
# refactored pipeline functions are available by sourcing local R/ files.
core_needed <- c(
  "gexp_parse_gse_inputs", "gexp_prepare_download_dirs", "gexp_download_finalize_common_genes",
  "gexp_download_normalize_ids_for_overlap", "gexp_download_one_microarray_gse",
  "gexp_download_one_rnaseq_gse", "gexp_qc_detect_outliers", "gexp_qc_exclude_samples",
  "gexp_register_pipeline_observers", "gexp_register_navigation_observers",
  "gexp_register_workspace_observers",
  "gexp_register_help_observers"
)

# exists() must not be passed bare to vapply() — the default env is wrong, so internal
# (non-exported) functions in GExPipe:: would look "missing" and trigger stop() + wrapup errors.
.gexpipe_has_core_fn <- function(nm) {
  if (exists(nm, mode = "function", inherits = TRUE)) {
    return(TRUE)
  }
  if (requireNamespace("GExPipe", quietly = TRUE)) {
    ns <- asNamespace("GExPipe")
    if (exists(nm, envir = ns, inherits = FALSE, mode = "function")) {
      return(TRUE)
    }
  }
  FALSE
}

missing_core <- core_needed[!vapply(core_needed, .gexpipe_has_core_fn, logical(1))]
if (length(missing_core) > 0) {
  local_r_files <- c(
    file.path(getwd(), "..", "..", "R", "gexpipe_shiny_helpers.R"),
    file.path(getwd(), "..", "..", "R", "gexp_download_pipeline.R"),
    file.path(getwd(), "..", "..", "R", "gexp_qc_pipeline.R"),
    file.path(getwd(), "..", "..", "R", "gexp_batch_pipeline.R"),
    file.path(getwd(), "..", "..", "R", "gexp_de_pipeline.R"),
    file.path(getwd(), "..", "..", "R", "observers_pipeline.R"),
    file.path(getwd(), "..", "..", "R", "observers_navigation.R"),
    file.path(getwd(), "..", "..", "R", "observers_workspace.R"),
    file.path(getwd(), "..", "..", "R", "observers_help.R"),
    file.path(getwd(), "R", "gexpipe_shiny_helpers.R"),
    file.path(getwd(), "R", "gexp_download_pipeline.R"),
    file.path(getwd(), "R", "gexp_qc_pipeline.R"),
    file.path(getwd(), "R", "gexp_batch_pipeline.R"),
    file.path(getwd(), "R", "gexp_de_pipeline.R"),
    file.path(getwd(), "R", "observers_pipeline.R"),
    file.path(getwd(), "R", "observers_navigation.R"),
    file.path(getwd(), "R", "observers_workspace.R"),
    file.path(getwd(), "R", "observers_help.R")
  )
  local_r_files <- unique(local_r_files[file.exists(local_r_files)])
  for (rf in local_r_files) {
    tryCatch(source(rf, local = TRUE), error = function(e) NULL)
  }
}

missing_core <- core_needed[!vapply(core_needed, .gexpipe_has_core_fn, logical(1))]
if (length(missing_core) > 0) {
  stop(
    "Missing core GExPipe functions: ", paste(missing_core, collapse = ", "),
    ". Reinstall/update the package (devtools::install_local()) or run from the package root."
  )
}

