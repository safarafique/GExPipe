## GExPipe WGCNA helpers
##
## This file starts moving Step 7 (WGCNA) into R/ as reusable functions.
## The Shiny app still uses inst/shinyapp/server/server_wgcna.R; later you
## can wire that server code to call these helpers.

utils::globalVariables(c("."))

#' Prepare expression and sample data for WGCNA
#'
#' This helper mirrors the "Prepare WGCNA Data" step:
#' - uses batch-corrected expression (or combined expression if needed),
#' - drops samples/genes with too many missing values,
#' - optionally filters to top variable genes,
#' - enforces a minimum fraction of non-missing samples per gene,
#' - returns a WGCNA-ready `datExpr` matrix (samples x genes) and
#'   companion sample info and gene-variance table.
#'
#' @param expr Matrix of expression values (genes x samples), typically
#'   batch-corrected.
#' @param metadata Data.frame with sample metadata; should contain
#'   either rownames matching `colnames(expr)` or a `SampleID` column.
#' @param gene_mode "all_common" or "top_variable" (default).
#' @param top_genes If `gene_mode = "top_variable"`, the number of
#'   most variable genes to select (default 5000).
#' @param min_samples_frac Minimum fraction of samples with non-missing
#'   expression required for a gene to be kept (default 0.5).
#'
#' @return A list with elements:
#'   \item{datExpr}{numeric matrix (samples x genes) for WGCNA}
#'   \item{sample_info}{data.frame with sample metadata aligned to datExpr}
#'   \item{gene_variance_table}{data.frame with gene variance and rank}
#' @export
gexp_wgcna_prepare <- function(
  expr,
  metadata,
  gene_mode = c("top_variable", "all_common"),
  top_genes = 5000L,
  min_samples_frac = 0.5
) {
  if (!requireNamespace("WGCNA", quietly = TRUE)) {
    stop("Package 'WGCNA' is required. Install via BiocManager::install('WGCNA').")
  }
  if (is.null(expr) || !is.matrix(expr)) {
    stop("expr must be a non-null matrix (genes x samples).")
  }
  if (is.null(metadata) || !is.data.frame(metadata)) {
    stop("metadata must be a data.frame.")
  }

  gene_mode <- match.arg(gene_mode)
  top_genes <- as.integer(top_genes)
  if (is.na(top_genes) || top_genes < 100L) {
    top_genes <- 5000L
  }
  min_samples_frac <- max(0.1, min(1, as.numeric(min_samples_frac)))

  # Align samples between expr and metadata
  samp_ids <- if ("SampleID" %in% names(metadata)) {
    metadata$SampleID
  } else {
    rownames(metadata)
  }
  if (is.null(samp_ids)) samp_ids <- rownames(metadata)
  common_samples <- intersect(colnames(expr), as.character(samp_ids))
  if (length(common_samples) == 0) {
    stop("No matching samples between expression data and metadata.")
  }
  expr_mat <- expr[, common_samples, drop = FALSE]

  if (all(common_samples %in% rownames(metadata))) {
    sample_info <- metadata[common_samples, , drop = FALSE]
  } else if ("SampleID" %in% names(metadata)) {
    sample_info <- metadata[match(common_samples, metadata$SampleID), , drop = FALSE]
    rownames(sample_info) <- common_samples
  } else {
    sample_info <- metadata[match(common_samples, rownames(metadata)), , drop = FALSE]
    rownames(sample_info) <- common_samples
  }

  # Remove samples with too many NAs (>=50% genes missing)
  good_samples <- colSums(is.na(expr_mat)) < nrow(expr_mat) * 0.5
  expr_mat <- expr_mat[, good_samples, drop = FALSE]
  sample_info <- sample_info[good_samples, , drop = FALSE]

  # Use WGCNA's goodSamplesGenes check
  gsg <- WGCNA::goodSamplesGenes(t(expr_mat), verbose = 3)
  if (!gsg$allOK) {
    expr_mat <- t(expr_mat)[gsg$goodSamples, gsg$goodGenes]
    expr_mat <- t(expr_mat)
  }

  # Gene variance and selection
  vars <- apply(expr_mat, 1, var, na.rm = TRUE)
  if (gene_mode == "all_common") {
    expr_top <- expr_mat
  } else {
    top_n <- min(top_genes, length(vars))
    keep_genes <- names(sort(vars, decreasing = TRUE))[seq_len(top_n)]
    expr_top <- expr_mat[keep_genes, , drop = FALSE]
  }

  # Filter by minimum non-missing fraction
  min_samples <- ceiling(min_samples_frac * ncol(expr_top))
  keep <- rowSums(!is.na(expr_top)) >= min_samples
  expr_top <- expr_top[keep, , drop = FALSE]

  if (nrow(expr_top) < 20L) {
    stop(
      "Too few genes (", nrow(expr_top),
      ") after filtering. Use 'all_common' or increase 'top_genes'/decrease 'min_samples_frac'."
    )
  }
  if (ncol(expr_top) < 3L) {
    stop("Too few samples (", ncol(expr_top), ") for WGCNA (need >= 3).")
  }

  # Transpose for WGCNA: samples as rows, genes as columns
  datExpr <- t(expr_top)

  # Build gene variance table (on selected genes)
  final_genes <- colnames(datExpr)
  gene_var <- vars[final_genes]
  gene_variance_table <- data.frame(
    Rank = seq_along(final_genes),
    Gene = final_genes,
    Variance = round(as.numeric(gene_var), 6),
    stringsAsFactors = FALSE
  )

  list(
    datExpr = datExpr,
    sample_info = sample_info[rownames(datExpr), , drop = FALSE],
    gene_variance_table = gene_variance_table
  )
}

