# ==============================================================================
# gexp_run_manifest.R - provenance record for one analysis run
# ==============================================================================
# Collects, in one two-column table (Field, Value), what is needed to reproduce
# or audit a run: package and R versions, seeds, training datasets and samples,
# the WGCNA settings and modules that fed Step 8, the STRING release used for
# PPI, the ML methods and genes, and (via `extra`) the nomogram settings and
# the training/validation sample IDs. Written as Run_Manifest_<time>.csv.
# ==============================================================================

#' Deterministic gene ranking by an association score
#'
#' Highest score first, scores compared at `digits` decimal places, ties broken
#' alphabetically by gene name, so the order never depends on the order genes
#' arrived in from earlier steps.
#' @param assoc named numeric vector (names = genes)
#' @param digits decimal places used for the comparison
#' @return character vector of gene names
#' @noRd
gexp_rank_genes_det <- function(assoc, digits = 4L) {
  names(assoc)[order(-round(assoc, digits), names(assoc))]
}

#' Build a run manifest (Field / Value table)
#' @param rv reactiveValues (or list) with the app state
#' @param extra named list of additional fields (scalars or vectors; vectors are
#'   collapsed with ";")
#' @return data.frame with columns Field and Value
#' @noRd
gexp_run_manifest <- function(rv, extra = list()) {
  get <- function(name) tryCatch(rv[[name]], error = function(e) NULL)
  flat <- function(x) {
    if (is.null(x) || length(x) == 0L) return(NA_character_)
    if (is.data.frame(x)) return(paste(names(x), vapply(x, function(v) paste(v, collapse = ","), ""), sep = "=", collapse = "; "))
    paste(as.character(unlist(x)), collapse = ";")
  }
  pkg_ver <- function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) "not installed")
  meta <- get("wgcna_sample_info")
  if (is.null(meta)) meta <- get("unified_metadata")
  datasets <- if (!is.null(meta) && "Dataset" %in% names(meta)) sort(unique(as.character(meta$Dataset))) else names(get("platform_per_gse"))
  pkgs <- c("GExPipe", "limma", "edgeR", "DESeq2", "sva", "WGCNA", "STRINGdb", "igraph", "glmnet",
            "randomForest", "kernlab", "caret", "Boruta", "mixOmics", "xgboost", "rms", "pROC", "clusterProfiler")
  fields <- list(
    Run_Time = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    R_Version = R.version.string,
    Platform = R.version$platform,
    Analysis_Type = flat(get("analysis_type")),
    Training_Datasets = flat(datasets),
    Training_N_Samples = if (is.null(meta)) NA_integer_ else nrow(meta),
    ML_Seed = 123L,
    ML_glmnet_Folds = "10, fixed foldid from seed 123",
    ML_Methods_Run = flat(get("ml_methods_run")),
    ML_Common_Genes = flat(get("ml_common_genes")),
    ROC_Selected_Genes = flat(get("roc_selected_genes")),
    WGCNA_Settings = flat(get("wgcna_settings_used")),
    Step8_WGCNA_Scope = flat(get("common_genes_wgcna_scope")),
    Step8_Common_Genes_N = length(get("common_genes_de_wgcna")),
    PPI_STRING = flat(get("ppi_string_info")),
    PPI_Consensus_Hubs = flat(get("ppi_consensus_hubs"))
  )
  fields <- c(fields, lapply(extra, flat),
              stats::setNames(lapply(pkgs, pkg_ver), paste0("Package_", pkgs)))
  data.frame(Field = names(fields), Value = vapply(fields, function(v) as.character(v)[1L], ""),
             stringsAsFactors = FALSE, row.names = NULL)
}
