## GExPipe differential expression helpers
##
## This file begins moving Step 6 (DE analysis) into R/ as reusable
## functions. The Shiny app still uses inst/shinyapp/server/server_results.R;
## later we can wire that server code to call these helpers.

utils::globalVariables(c("."))

#' Run differential expression analysis
#'
#' Wrapper for the limma-based DE pipeline used in Step 6. This version
#' currently implements the limma path; other methods can be added later.
#'
#' @param expr Matrix of (batch-corrected, normalized) expression values
#'   with genes in rows and samples in columns.
#' @param metadata Data.frame with at least a `Condition` column taking
#'   values "Normal" and "Disease", and optionally `Dataset` for batches.
#' @param method Character string, one of "limma", "limma_voom",
#'   "deseq2", or "edger". Currently only "limma" is implemented here.
#' @param logfc_cutoff Numeric log2 fold-change cutoff (e.g. 0.5).
#' @param padj_cutoff Numeric adjusted P-value cutoff (e.g. 0.05).
#'
#' @return A list with elements:
#'   \item{de_results}{data.frame with columns Gene, logFC, AveExpr,
#'     P.Value, adj.P.Val, Significance}
#'   \item{sig_genes}{subset of de_results with Significance != "Not Significant"}
#'
#' @examples
#' expr <- matrix(rnorm(240), nrow = 24)
#' rownames(expr) <- paste0("Gene", seq_len(nrow(expr)))
#' colnames(expr) <- paste0("S", seq_len(ncol(expr)))
#' metadata <- data.frame(
#'   Dataset = rep(c("D1", "D2"), each = 5),
#'   Condition = rep(c("Normal", "Disease"), times = 5),
#'   row.names = colnames(expr),
#'   stringsAsFactors = FALSE
#' )
#' de <- gexp_run_de(expr, metadata, method = "limma", logfc_cutoff = 0.1, padj_cutoff = 0.5)
#' head(de$de_results$Gene)
#' @export
gexp_run_de <- function(
  expr,
  metadata,
  method = c("limma", "limma_voom", "deseq2", "edger"),
  logfc_cutoff = 0.5,
  padj_cutoff = 0.05
) {
  if (is.null(expr) || !is.matrix(expr)) {
    stop("expr must be a non-null matrix (genes x samples).")
  }
  if (is.null(metadata) || !is.data.frame(metadata)) {
    stop("metadata must be a data.frame with at least a 'Condition' column.")
  }
  if (!"Condition" %in% colnames(metadata)) {
    stop("metadata must contain a 'Condition' column.")
  }

  method <- match.arg(method)

  if (method != "limma") {
    stop("gexp_run_de currently implements only 'limma'. Other methods are handled in the Shiny server code.")
  }

  # Ensure Condition is a factor with Normal as reference
  metadata$Condition <- factor(metadata$Condition, levels = c("Normal", "Disease"))

  # Align samples between expr and metadata
  common_samples <- intersect(colnames(expr), rownames(metadata))
  if (length(common_samples) < 3) {
    stop("Need at least 3 samples with matching expression and metadata for DE.")
  }
  expr <- expr[, common_samples, drop = FALSE]
  metadata <- metadata[common_samples, , drop = FALSE]

  # Design matrix with optional batch covariate
  if ("Dataset" %in% colnames(metadata) && length(unique(metadata$Dataset)) > 1) {
    metadata$Dataset <- factor(metadata$Dataset)
    design <- stats::model.matrix(~ Dataset + Condition, data = metadata)
  } else {
    design <- stats::model.matrix(~Condition, data = metadata)
  }

  # Fit limma model
  fit <- limma::lmFit(expr, design)
  fit <- limma::eBayes(fit)

  # Last coefficient corresponds to Condition effect (Disease vs Normal)
  coef_idx <- ncol(design)
  tt <- limma::topTable(
    fit,
    coef = coef_idx,
    number = Inf,
    adjust.method = "BH",
    sort.by = "P"
  )

  tt$Gene <- rownames(tt)
  de_results <- tt[, c("Gene", "logFC", "AveExpr", "P.Value", "adj.P.Val")]

  # Classify significance
  de_results$Significance <- "Not Significant"
  de_results$Significance[
    de_results$adj.P.Val < padj_cutoff & de_results$logFC > logfc_cutoff
  ] <- "Up-regulated"
  de_results$Significance[
    de_results$adj.P.Val < padj_cutoff & de_results$logFC < -logfc_cutoff
  ] <- "Down-regulated"
  de_results$Significance <- as.character(de_results$Significance)

  rownames(de_results) <- de_results$Gene
  sig_genes <- de_results[de_results$Significance != "Not Significant", , drop = FALSE]

  list(
    de_results = de_results,
    sig_genes = sig_genes
  )
}
