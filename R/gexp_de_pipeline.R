## GExPipe differential expression helpers
##
## Reusable Step 6 (DE) logic for the Shiny app and programmatic use.

#' @importFrom edgeR filterByExpr
utils::globalVariables(c("."))

#' Independent filtering for DE (limma filterByExpr)
#'
#' Removes lowly expressed genes using a design-aware filter so filtering is
#' not tied to differential expression statistics (avoids FDR bias from
#' variance-percentile pre-filtering).
#'
#' @param expr Numeric matrix (genes x samples) or integer counts.
#' @param design Model matrix for the DE analysis.
#' @param group Optional factor when design is NULL (single-factor designs).
#' @return list with expr (filtered), keep (logical vector), n_before, n_after, note
#' @export
gexpipe_independent_filter <- function(expr, design = NULL, group = NULL) {
  if (is.null(expr) || !is.matrix(expr) || nrow(expr) == 0L || ncol(expr) == 0L) {
    stop("expr must be a non-empty matrix for independent filtering.")
  }
  n_before <- nrow(expr)
  keep <- tryCatch({
    if (!is.null(design)) {
      edgeR::filterByExpr(expr, design = design)
    } else if (!is.null(group)) {
      edgeR::filterByExpr(expr, group = group)
    } else {
      rowSums(expr > 0, na.rm = TRUE) >= max(2L, ceiling(ncol(expr) * 0.1))
    }
  }, error = function(e) {
    rowSums(expr > 0, na.rm = TRUE) >= max(2L, ceiling(ncol(expr) * 0.1))
  })
  if (length(keep) != n_before) {
    keep <- rep(TRUE, n_before)
  }
  n_after <- sum(keep)
  if (n_after < 1L) {
    stop("Independent filtering removed all genes; check expression/count data.")
  }
  note <- paste0(
    "Independent filtering (filterByExpr): ",
    format(n_before, big.mark = ","), " \u2192 ",
    format(n_after, big.mark = ","), " genes"
  )
  list(
    expr = expr[keep, , drop = FALSE],
    keep = keep,
    n_before = n_before,
    n_after = n_after,
    note = note
  )
}

#' Summarise samples used in a DE run (transparency for mixed-platform runs)
#'
#' @param meta_used Metadata rows actually used in the DE fit.
#' @param total_meta Full unified metadata before subsetting (optional).
#' @param method DE method name.
#' @return list with human-readable fields for the Shiny UI.
#' @export
gexpipe_de_sample_info <- function(meta_used, total_meta = NULL, method = "limma") {
  n_used <- nrow(meta_used)
  n_total <- if (is.null(total_meta)) n_used else nrow(total_meta)
  plat_tab <- if ("Platform" %in% colnames(meta_used)) table(meta_used$Platform) else NULL
  cond_tab <- if ("Condition" %in% colnames(meta_used)) table(meta_used$Condition) else NULL
  plat_str <- if (!is.null(plat_tab)) {
    paste(paste0(names(plat_tab), "=", as.integer(plat_tab)), collapse = ", ")
  } else {
    "n/a"
  }
  cond_str <- if (!is.null(cond_tab)) {
    paste(paste0(names(cond_tab), "=", as.integer(cond_tab)), collapse = ", ")
  } else {
    "n/a"
  }
  excluded <- max(0L, n_total - n_used)
  count_methods <- method %in% c("deseq2", "edger", "limma_voom")
  note <- if (excluded > 0L && count_methods) {
    paste0(
      n_used, " of ", n_total, " samples used in ", method,
      " (", excluded, " excluded \u2014 count-based DE uses RNA-seq samples only)."
    )
  } else if (excluded > 0L) {
    paste0(n_used, " of ", n_total, " samples used (", excluded, " excluded after alignment).")
  } else {
    paste0(n_used, " samples used.")
  }
  list(
    n_used = n_used,
    n_total = n_total,
    n_excluded = excluded,
    platform_summary = plat_str,
    condition_summary = cond_str,
    method = method,
    note = note
  )
}

#' Build reproducibility report text for export
#'
#' @param params Named list of scalar analysis parameters (character or numeric).
#' @param include_session Include \code{sessionInfo()} block (default TRUE).
#' @return Character vector of report lines.
#' @export
gexpipe_analysis_report_text <- function(params = list(), include_session = TRUE) {
  lines <- c(
    paste("GExPipe analysis report —", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    strrep("=", 60L),
    ""
  )
  if (length(params) > 0L) {
    lines <- c(lines, "Parameters:", "")
    for (nm in names(params)) {
      val <- params[[nm]]
      if (is.null(val)) val <- "n/a"
      if (length(val) > 1L) val <- paste(val, collapse = ", ")
      lines <- c(lines, paste0("  ", nm, ": ", as.character(val)))
    }
    lines <- c(lines, "")
  }
  if (isTRUE(include_session)) {
    lines <- c(lines, "sessionInfo():", "", capture.output(utils::sessionInfo()))
  }
  lines
}

#' Run differential expression analysis
#'
#' @param expr Matrix of (batch-corrected, normalized) expression values
#'   with genes in rows and samples in columns.
#' @param metadata Data.frame with at least a `Condition` column.
#' @param method Currently only `"limma"` is implemented.
#' @param logfc_cutoff Numeric log2 fold-change cutoff.
#' @param padj_cutoff Numeric adjusted P-value cutoff.
#' @return list with de_results, sig_genes, filter_note, sample_info, formula_desc
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

  metadata <- .gexpipe_align_metadata_to_expr(expr, metadata)
  metadata$Condition <- factor(metadata$Condition, levels = c("Normal", "Disease"))

  de_design <- gexpipe_build_de_design(metadata)
  design <- de_design$design
  coef_idx <- de_design$coef_condition

  filt <- gexpipe_independent_filter(expr, design = design)
  expr_f <- filt$expr

  fit <- limma::lmFit(expr_f, design)
  fit <- limma::eBayes(fit)

  tt <- limma::topTable(
    fit,
    coef = coef_idx,
    number = Inf,
    adjust.method = "BH",
    sort.by = "P"
  )

  tt$Gene <- rownames(tt)
  de_results <- tt[, c("Gene", "logFC", "AveExpr", "P.Value", "adj.P.Val")]

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
    sig_genes = sig_genes,
    filter_note = filt$note,
    sample_info = gexpipe_de_sample_info(metadata, method = "limma"),
    formula_desc = de_design$formula_desc
  )
}
