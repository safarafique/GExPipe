## GExPipe batch correction helpers
##
## This file mirrors the main batch-correction pipeline currently implemented
## inside inst/shinyapp/server/server_batch.R, but as reusable functions
## under R/. Existing Shiny code still uses the inline version; you can
## gradually switch server_batch.R to call these helpers.

#' @importFrom stats var
#' @importFrom utils download.file head
utils::globalVariables(c("."))

#' Variance-based gene filtering and batch correction
#'
#' This helper wraps the Step 5 logic:
#' - filters genes by a variance percentile,
#' - applies one of several batch-correction methods,
#' - reports gene reduction statistics.
#'
#' @param expr Combined expression matrix (genes x samples), after normalization.
#' @param metadata Data.frame with at least columns `Dataset` and `Condition`.
#' @param variance_percentile Numeric between 0 and 50; bottom percentile to remove.
#' @param method Batch method: one of
#'   "limma", "combat", "combat_ref", "quantile_limma", "hybrid", "sva".
#'
#' @return A list with elements:
#'   \item{expr_filtered}{filtered expression matrix before batch correction}
#'   \item{batch_corrected}{batch-corrected matrix}
#'   \item{genes_before}{integer, genes before filtering}
#'   \item{genes_after}{integer, genes after filtering}
#'   \item{filter_percent}{numeric, percent genes removed}
#'   \item{log_text}{character, human-readable log string}
#'
#' @examples
#' expr <- matrix(rnorm(200), nrow = 20)
#' rownames(expr) <- paste0("Gene", seq_len(nrow(expr)))
#' colnames(expr) <- paste0("S", seq_len(ncol(expr)))
#' metadata <- data.frame(
#'   Dataset = rep(c("D1", "D2"), each = 5),
#'   Condition = rep(c("Normal", "Disease"), times = 5),
#'   row.names = colnames(expr),
#'   stringsAsFactors = FALSE
#' )
#' out <- gexp_batch_correct(expr, metadata, variance_percentile = 10, method = "limma")
#' dim(out$batch_corrected)
#' @export
gexp_batch_correct <- function(
  expr,
  metadata,
  variance_percentile = 25,
  method = c("combat_ref", "sva", "limma", "combat", "quantile_limma", "hybrid")
) {
  if (is.null(expr) || !is.matrix(expr)) {
    stop("expr must be a non-null matrix (genes x samples).")
  }
  if (is.null(metadata) || !is.data.frame(metadata)) {
    stop("metadata must be a data.frame with Dataset and Condition columns.")
  }
  if (!all(c("Dataset", "Condition") %in% colnames(metadata))) {
    stop("metadata must contain 'Dataset' and 'Condition' columns.")
  }
  metadata <- .gexpipe_align_metadata_to_expr(expr, metadata)
  method <- match.arg(method)
  variance_percentile <- max(0, min(50, as.numeric(variance_percentile)))

  log_text <- ""

  # ---- Variance-based gene filtering ----
  # Remove rows that are entirely NA/NaN before variance calculation to avoid
  # "missing values not allowed if na.rm = FALSE" errors in quantile().
  all_na_rows <- apply(expr, 1, function(x) all(is.na(x)))
  if (any(all_na_rows)) {
    expr <- expr[!all_na_rows, , drop = FALSE]
  }
  if (variance_percentile <= 0) {
    expr_filtered <- expr
  } else {
    gene_vars <- apply(expr, 1, var, na.rm = TRUE)
    percentile <- variance_percentile / 100
    cutoff <- stats::quantile(gene_vars, percentile, na.rm = TRUE)
    high_var <- !is.na(gene_vars) & gene_vars > cutoff
    expr_filtered <- expr[high_var, , drop = FALSE]
  }

  genes_before <- nrow(expr)
  genes_after <- nrow(expr_filtered)
  genes_filtered <- genes_before - genes_after
  filter_percent <- round(100 * genes_filtered / max(1, genes_before), 1)

  log_text <- paste0(
    log_text,
    "Variance filtering:\n",
    "  Percentile cutoff: ", variance_percentile, "%\n",
    "  Before filter: ", format(genes_before, big.mark = ","), " genes\n",
    "  After filter:  ", format(genes_after, big.mark = ","), " genes\n",
    "  Filtered out:  ", format(genes_filtered, big.mark = ","), " genes (",
    filter_percent, "%)\n\n"
  )

  # ---- Design matrix: preserve Condition (+ Platform when estimable) ----
  plat_info <- gexpipe_batch_covariate_info(metadata)
  mod <- gexpipe_build_batch_mod(metadata)
  if (plat_info$mixed_platforms) {
    log_text <- paste0(
      log_text,
      "Mixed platforms detected (Microarray + RNA-seq).\n",
      if (plat_info$include_platform_covariate) {
        "  Platform included as covariate in batch model (not confounded with Dataset).\n"
      } else {
        "  Platform is confounded with Dataset - Dataset batch term absorbs platform effect.\n"
      }
    )
  }

  # ---- Apply batch method ----
  batch_corrected <- expr_filtered

  if (method == "limma") {
    batch_corrected <- limma::removeBatchEffect(
      expr_filtered,
      batch = metadata$Dataset,
      design = mod
    )
    log_text <- paste0(log_text, "Batch method: limma removeBatchEffect (Condition",
                       if (plat_info$include_platform_covariate) " + Platform" else "",
                       " protected)\n")
  } else if (method == "combat") {
    batch_corrected <- sva::ComBat(
      expr_filtered,
      batch = metadata$Dataset,
      mod = mod,
      par.prior = TRUE,
      prior.plots = FALSE
    )
    log_text <- paste0(log_text, "Batch method: ComBat (Condition",
                       if (plat_info$include_platform_covariate) " + Platform" else "",
                       " in mod)\n")
  } else if (method == "quantile_limma") {
    expr_q <- limma::normalizeBetweenArrays(expr_filtered, method = "quantile")
    batch_corrected <- limma::removeBatchEffect(
      expr_q,
      batch = metadata$Dataset,
      design = mod
    )
    log_text <- paste0(log_text, "Batch method: Quantile + limma removeBatchEffect\n")
  } else if (method == "hybrid") {
    expr_q <- limma::normalizeBetweenArrays(expr_filtered, method = "quantile")
    batch_corrected <- sva::ComBat(
      expr_q,
      batch = metadata$Dataset,
      mod = mod,
      par.prior = TRUE,
      prior.plots = FALSE
    )
    log_text <- paste0(log_text, "Batch method: Hybrid (Quantile + ComBat, Condition",
                       if (plat_info$include_platform_covariate) " + Platform" else "",
                       " in mod)\n")
  } else if (method == "combat_ref") {
    sizes <- table(metadata$Dataset)
    ref <- names(sizes)[which.max(sizes)]
    batch_corrected <- sva::ComBat(
      expr_filtered,
      batch = metadata$Dataset,
      mod = mod,
      par.prior = TRUE,
      prior.plots = FALSE,
      ref.batch = ref
    )
    log_text <- paste0(
      log_text,
      "Batch method: ComBat-ref (ref.batch = ", ref, ", Condition",
      if (plat_info$include_platform_covariate) " + Platform" else "",
      " in mod)\n"
    )
  } else if (method == "sva") {
    # Surrogate variable analysis: estimate hidden confounders, then ComBat with mod + SVs
    mod0 <- stats::model.matrix(~1, data = metadata)
    n_sv <- tryCatch(
      sva::num.sv(expr_filtered, mod, method = "be"),
      error = function(e) 0L
    )
    n_sv <- max(0L, min(n_sv, 10L))
    svobj <- if (n_sv > 0) {
      tryCatch(
        sva::sva(as.matrix(expr_filtered), mod, mod0, n.sv = n_sv),
        error = function(e) NULL
      )
    } else {
      NULL
    }
    sv_ok <- !is.null(svobj) && ncol(svobj$sv) > 0
    mod_use <- if (sv_ok) cbind(mod, svobj$sv) else mod
    batch_corrected <- sva::ComBat(
      expr_filtered,
      batch = metadata$Dataset,
      mod = mod_use,
      par.prior = TRUE,
      prior.plots = FALSE
    )
    log_text <- paste0(
      log_text,
      if (sv_ok) {
        paste0("Batch method: SVA + ComBat (", n_sv, " SVs)\n")
      } else if (n_sv > 0) {
        "Batch method: SVA requested, but SV computation failed; used ComBat with Condition in mod\n"
      } else {
        "Batch method: SVA requested, but num.sv returned 0; used ComBat with Condition in mod\n"
      }
    )
  }

  list(
    expr_filtered = expr_filtered,
    batch_corrected = batch_corrected,
    genes_before = genes_before,
    genes_after = genes_after,
    filter_percent = filter_percent,
    log_text = log_text
  )
}

#' Variance-filter one platform block (no cross-platform merge)
#' @noRd
.gexpipe_filter_var_block <- function(expr, variance_percentile = 25) {
  if (is.null(expr) || !is.matrix(expr) || ncol(expr) < 1L) {
    return(expr)
  }
  all_na_rows <- apply(expr, 1, function(x) all(is.na(x)))
  if (any(all_na_rows)) {
    expr <- expr[!all_na_rows, , drop = FALSE]
  }
  gene_vars <- apply(expr, 1, stats::var, na.rm = TRUE)
  percentile <- max(0, min(50, as.numeric(variance_percentile))) / 100
  cutoff <- stats::quantile(gene_vars, percentile, na.rm = TRUE)
  keep <- !is.na(gene_vars) & gene_vars > cutoff
  expr[keep, , drop = FALSE]
}

#' Default Parallel batch methods (scientifically matched per platform)
#'
#' Microarray: ComBat-ref (log intensities, largest GSE as reference).
#' RNA-seq count DE (DESeq2/edgeR/voom): limma on the continuous matrix;
#' those engines use raw counts + batch at DE. RNA-seq limma DE: ComBat-ref
#' on TMM log-CPM. Confounded Dataset×Condition: limma on both.
#' @noRd
gexpipe_parallel_batch_defaults <- function(de_method_rna = "deseq2", confounded = FALSE) {
  if (isTRUE(confounded)) {
    return(list(rna = "limma", micro = "limma"))
  }
  rna <- if (!is.null(de_method_rna) && de_method_rna %in% c("deseq2", "edger", "limma_voom")) {
    "limma"
  } else {
    "combat_ref"
  }
  list(rna = rna, micro = "combat_ref")
}

#' Batch-correct RNA-seq and microarray separately (no joint ComBat)
#'
#' Used by Parallel DE. Each platform is filtered and corrected on its own
#' samples and gene set. Single-GSE platforms are only variance-filtered.
#' Platforms are not gene-intersected or jointly ComBat-corrected.
#'
#' @param expr Numeric matrix (genes x samples); used when platform matrices
#'   are not supplied.
#' @param metadata Sample metadata with `Dataset`, `Condition`, `Platform`.
#' @param variance_percentile Bottom variance percentile to drop, used for
#'   both platforms when `rna_variance_percentile`/`micro_variance_percentile`
#'   are not given.
#' @param rna_method Batch method for RNA-seq when it has 2+ datasets.
#' @param micro_method Batch method for microarray when it has 2+ datasets.
#' @param expr_rna Optional RNA-seq-only matrix (genes x RNA samples).
#' @param expr_micro Optional microarray-only matrix (genes x array samples).
#' @param rna_variance_percentile Optional RNA-seq-specific percentile
#'   (overrides `variance_percentile` for the RNA-seq block only).
#' @param micro_variance_percentile Optional microarray-specific percentile
#'   (overrides `variance_percentile` for the microarray block only).
#' @return Same list shape as [gexp_batch_correct()], plus platform matrices.
#' @noRd
gexp_batch_correct_by_platform <- function(
  expr,
  metadata,
  variance_percentile = 25,
  rna_method = "combat_ref",
  micro_method = "combat_ref",
  expr_rna = NULL,
  expr_micro = NULL,
  rna_variance_percentile = NULL,
  micro_variance_percentile = NULL
) {
  rna_variance_percentile <- if (!is.null(rna_variance_percentile)) rna_variance_percentile else variance_percentile
  micro_variance_percentile <- if (!is.null(micro_variance_percentile)) micro_variance_percentile else variance_percentile
  if ((is.null(expr) || !is.matrix(expr)) && is.null(expr_rna) && is.null(expr_micro)) {
    stop("expr must be a non-null matrix (genes x samples).")
  }
  if (is.null(expr) || !is.matrix(expr)) {
    expr <- .gexpipe_cbind_union_na(expr_micro, expr_rna)
  }
  metadata <- .gexpipe_align_metadata_to_expr(expr, metadata)
  rna_ids <- intersect(gexpipe_platform_sample_ids(metadata, "RNAseq"), colnames(expr))
  micro_ids <- intersect(gexpipe_platform_sample_ids(metadata, "Microarray"), colnames(expr))

  block_input <- function(ids, plat_expr) {
    if (!is.null(plat_expr) && is.matrix(plat_expr) && ncol(plat_expr) > 0L) {
      keep <- intersect(ids, colnames(plat_expr))
      if (length(keep) >= 2L) {
        return(plat_expr[, keep, drop = FALSE])
      }
    }
    if (length(ids) < 2L) {
      return(NULL)
    }
    expr[, ids, drop = FALSE]
  }

  correct_block <- function(sub, method, label, block_variance_percentile) {
    if (is.null(sub) || !is.matrix(sub) || ncol(sub) < 2L) {
      return(list(
        expr = NULL,
        genes_before = 0L,
        genes_after = 0L,
        n_samples = 0L,
        n_datasets = 0L,
        log = paste0(label, ": no samples; skipped.\n")
      ))
    }
    ids <- colnames(sub)
    meta <- metadata[intersect(ids, rownames(metadata)), , drop = FALSE]
    meta <- meta[ids, , drop = FALSE]
    n_ds <- length(unique(as.character(meta$Dataset)))
    n_before <- nrow(sub)
    filtered <- .gexpipe_filter_var_block(sub, block_variance_percentile)
    if (n_ds < 2L) {
      return(list(
        expr = filtered,
        genes_before = n_before,
        genes_after = nrow(filtered),
        n_samples = ncol(filtered),
        n_datasets = n_ds,
        log = paste0(
          label, ": 1 dataset (", unique(as.character(meta$Dataset)), "). ",
          "Standard: no between-study batch. Variance filter only (",
          format(n_before, big.mark = ","), " \u2192 ",
          format(nrow(filtered), big.mark = ","), " genes).\n"
        )
      ))
    }
    out <- gexp_batch_correct(
      filtered,
      meta,
      variance_percentile = 0,
      method = method
    )
    list(
      expr = out$batch_corrected,
      genes_before = n_before,
      genes_after = nrow(out$batch_corrected),
      n_samples = ncol(out$batch_corrected),
      n_datasets = n_ds,
      log = paste0(label, " (", method, ", ", n_ds, " datasets):\n", out$log_text)
    )
  }

  rna_out <- correct_block(block_input(rna_ids, expr_rna), rna_method, "RNA-seq", rna_variance_percentile)
  micro_out <- correct_block(block_input(micro_ids, expr_micro), micro_method, "Microarray", micro_variance_percentile)

  if (is.null(rna_out$expr) && is.null(micro_out$expr)) {
    stop("Per-platform batch correction produced no expression blocks.")
  }

  batch_corrected <- .gexpipe_cbind_union_na(rna_out$expr, micro_out$expr)
  expr_filtered <- batch_corrected
  n_rna <- if (is.null(rna_out$expr)) 0L else nrow(rna_out$expr)
  n_micro <- if (is.null(micro_out$expr)) 0L else nrow(micro_out$expr)

  log_text <- paste0(
    "Parallel batch: each platform corrected separately (not one joint ComBat).\n",
    "Gene sets stay platform-specific (not intersected).\n",
    rna_out$log,
    micro_out$log,
    "RNA-seq genes after platform batch: ", format(n_rna, big.mark = ","), "\n",
    "Microarray genes after platform batch: ", format(n_micro, big.mark = ","), "\n"
  )

  list(
    expr_filtered = expr_filtered,
    batch_corrected = batch_corrected,
    batch_corrected_rna = rna_out$expr,
    batch_corrected_micro = micro_out$expr,
    genes_before = n_rna + n_micro,
    genes_after = n_rna + n_micro,
    genes_before_rna = rna_out$genes_before,
    genes_after_rna = rna_out$genes_after,
    n_samples_rna = rna_out$n_samples,
    genes_before_micro = micro_out$genes_before,
    genes_after_micro = micro_out$genes_after,
    n_samples_micro = micro_out$n_samples,
    n_datasets_rna = rna_out$n_datasets,
    n_datasets_micro = micro_out$n_datasets,
    filter_percent = NA_real_,
    log_text = log_text
  )
}
