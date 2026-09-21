## GExPipe differential expression helpers
##
## Reusable Step 6 (DE) logic for the Shiny app and programmatic use.

#' @importFrom edgeR filterByExpr
utils::globalVariables(c("."))

#' Does a matrix contain negative values?
#'
#' Count-based DE engines (DESeq2, edgeR, limma-voom) require non-negative
#' values. Several GEO series only publish normalized or log-scale tables named
#' like counts (for example `*_normalized.counts.txt.gz`); those contain
#' negatives and make DESeq2 abort with "some values in assay are negative".
#'
#' @param m Matrix or data frame.
#' @return TRUE when at least one finite value is below zero.
#' @keywords internal
.gexpipe_matrix_has_negative <- function(m) {
  if (is.null(m) || length(m) == 0L) {
    return(FALSE)
  }
  v <- suppressWarnings(as.numeric(as.matrix(m)))
  v <- v[is.finite(v)]
  length(v) > 0L && min(v) < 0
}

#' Datasets whose stored "counts" are not usable as counts
#'
#' @param counts_list Named list of count matrices.
#' @param combined Optional combined count matrix used as a fallback check.
#' @return Character vector of offending dataset names (empty when all valid).
#' @keywords internal
.gexpipe_negative_count_datasets <- function(counts_list, combined = NULL) {
  bad <- character(0)
  if (length(counts_list) > 0) {
    flags <- vapply(counts_list, .gexpipe_matrix_has_negative, logical(1))
    bad <- names(counts_list)[flags]
  }
  if (length(bad) == 0L && .gexpipe_matrix_has_negative(combined)) {
    bad <- "combined count matrix"
  }
  bad
}

#' Does a matrix look like raw integer counts (vs normalized/continuous values)?
#'
#' Real RNA-seq raw counts are non-negative integers. Some GEO series publish
#' FPKM/TPM/CPM-normalized tables under a "counts" filename; those are
#' non-negative but non-integer, and feeding them into DESeq2/edgeR's count
#' model does not raise a clean error - instead, \code{edgeR::filterByExpr()}
#' quietly removes every gene ("Independent filtering removed all genes"),
#' because CPM computed from already-normalized values is meaningless.
#'
#' @param m Matrix or data frame.
#' @param tol Numeric tolerance for "close enough to an integer".
#' @return TRUE when finite values look like raw counts.
#' @keywords internal
.gexpipe_matrix_looks_like_counts <- function(m, tol = 1e-6) {
  if (is.null(m) || length(m) == 0L) {
    return(TRUE) # nothing to judge; don't block on missing data here
  }
  v <- suppressWarnings(as.numeric(as.matrix(m)))
  v <- v[is.finite(v)]
  if (length(v) == 0L) {
    return(TRUE)
  }
  frac_noninteger <- mean(abs(v - round(v)) > tol)
  frac_noninteger < 0.01
}

#' Datasets whose stored "counts" do not look like raw integer counts
#'
#' @param counts_list Named list of count matrices.
#' @param combined Optional combined count matrix used as a fallback check.
#' @return Character vector of offending dataset names (empty when all valid).
#' @keywords internal
.gexpipe_noncount_datasets <- function(counts_list, combined = NULL) {
  bad <- character(0)
  if (length(counts_list) > 0) {
    flags <- vapply(counts_list, function(x) !.gexpipe_matrix_looks_like_counts(x), logical(1))
    bad <- names(counts_list)[flags]
  }
  if (length(bad) == 0L && !is.null(combined) && !.gexpipe_matrix_looks_like_counts(combined)) {
    bad <- "combined count matrix"
  }
  bad
}

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
#' @examples
#' expr <- matrix(stats::rpois(5000, lambda = 50), nrow = 500, ncol = 10)
#' design <- stats::model.matrix(~ factor(rep(c("A", "B"), each = 5)))
#' gexpipe_independent_filter(expr, design = design)
#' @export
gexpipe_independent_filter <- function(expr, design = NULL, group = NULL) {
  if (is.null(expr) || !is.matrix(expr) || nrow(expr) == 0L || ncol(expr) == 0L) {
    stop("expr must be a non-empty matrix for independent filtering.")
  }
  n_before <- nrow(expr)
  # edgeR::filterByExpr() assumes raw integer counts (it computes CPM from
  # library sizes); applying it to already-normalized continuous data
  # (microarray log-intensities, or batch-corrected/log expression used with
  # limma) produces meaningless thresholds and can filter out every gene.
  # Only use it when expr genuinely looks like counts; otherwise fall back to
  # a simple detectable-expression filter appropriate for continuous data.
  looks_like_counts <- .gexpipe_matrix_looks_like_counts(expr)
  used_filter_by_expr <- FALSE
  keep <- tryCatch({
    if (looks_like_counts && !is.null(design)) {
      used_filter_by_expr <- TRUE
      edgeR::filterByExpr(expr, design = design)
    } else if (looks_like_counts && !is.null(group)) {
      used_filter_by_expr <- TRUE
      edgeR::filterByExpr(expr, group = group)
    } else {
      threshold <- if (looks_like_counts) 0 else stats::median(expr, na.rm = TRUE)
      rowSums(expr > threshold, na.rm = TRUE) >= max(2L, ceiling(ncol(expr) * 0.1))
    }
  }, error = function(e) {
    threshold <- if (looks_like_counts) 0 else stats::median(expr, na.rm = TRUE)
    rowSums(expr > threshold, na.rm = TRUE) >= max(2L, ceiling(ncol(expr) * 0.1))
  })
  if (length(keep) != n_before) {
    keep <- rep(TRUE, n_before)
  }
  n_after <- sum(keep)
  if (n_after < 1L) {
    # Filter was too strict for this data (e.g. very low expression overall) -
    # keep everything rather than crash; the earlier variance-percentile
    # filter (Step 5) already did the primary gene-level filtering.
    keep <- rep(TRUE, n_before)
    n_after <- n_before
  }
  note <- paste0(
    "Independent filtering (", if (used_filter_by_expr) "filterByExpr" else "detectable-expression", "): ",
    format(n_before, big.mark = ","), " -> ",
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
#' @examples
#' meta <- data.frame(
#'   Condition = rep(c("Normal", "Disease"), each = 3),
#'   Platform = "RNAseq",
#'   row.names = paste0("S", 1:6),
#'   stringsAsFactors = FALSE
#' )
#' gexpipe_de_sample_info(meta)
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
      " (", excluded, " excluded - count-based DE uses RNA-seq samples only)."
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
#' @examples
#' gexpipe_analysis_report_text(list(method = "limma"), include_session = FALSE)
#' @export
gexpipe_analysis_report_text <- function(params = list(), include_session = TRUE) {
  lines <- c(
    paste("GExPipe analysis report -", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
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
#' @examples
#' expr <- matrix(rnorm(200), nrow = 20)
#' rownames(expr) <- paste0("G", seq_len(nrow(expr)))
#' colnames(expr) <- paste0("S", seq_len(ncol(expr)))
#' meta <- data.frame(
#'   Condition = rep(c("Normal", "Disease"), each = 5),
#'   row.names = colnames(expr),
#'   stringsAsFactors = FALSE
#' )
#' gexp_run_de(expr, meta)
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
  cond_chr <- as.character(metadata$Condition)
  cond_lvls <- unique(cond_chr[!is.na(cond_chr) & nzchar(cond_chr)])
  if (setequal(cond_lvls, c("Normal", "Disease"))) {
    cond_lvls <- c("Normal", "Disease")
  }
  metadata$Condition <- factor(cond_chr, levels = cond_lvls)

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

#' Label DE rows by logFC and adjusted p-value cutoffs
#'
#' @param de_results data.frame with `logFC` and `adj.P.Val`.
#' @param logfc_cutoff Numeric log2 fold-change cutoff.
#' @param padj_cutoff Numeric adjusted P-value cutoff.
#' @return The same data.frame with a `Significance` column.
#' @noRd
gexpipe_classify_de_significance <- function(de_results, logfc_cutoff = 0.5, padj_cutoff = 0.05) {
  if (is.null(de_results) || !is.data.frame(de_results) || nrow(de_results) < 1L) {
    return(de_results)
  }
  de_results$Significance <- "Not Significant"
  ok <- !is.na(de_results$adj.P.Val) & !is.na(de_results$logFC)
  de_results$Significance[ok & de_results$adj.P.Val < padj_cutoff &
    de_results$logFC > logfc_cutoff] <- "Up-regulated"
  de_results$Significance[ok & de_results$adj.P.Val < padj_cutoff &
    de_results$logFC < -logfc_cutoff] <- "Down-regulated"
  de_results$Significance <- as.character(de_results$Significance)
  de_results
}

#' Run limma DE on one platform (or one study) subset
#'
#' Uses per-platform normalized expression (not the cross-platform
#' batch-corrected matrix). Single-dataset subsets use a Condition contrast;
#' multiple datasets on the same platform use [gexpipe_build_de_design()].
#'
#' @param expr Numeric matrix (genes x samples).
#' @param metadata Sample metadata with `Condition` and optional `Dataset`.
#' @param logfc_cutoff Numeric log2 fold-change cutoff.
#' @param padj_cutoff Numeric adjusted P-value cutoff.
#' @param ref_lab Reference condition label (default `Normal`).
#' @param alt_lab Alternate condition label (default `Disease`).
#' @return list with `de_results`, `sig_genes`, `filter_note`, `sample_info`,
#'   `formula_desc`.
#' @examples
#' expr <- matrix(rnorm(200), nrow = 20)
#' rownames(expr) <- paste0("G", seq_len(nrow(expr)))
#' colnames(expr) <- paste0("S", seq_len(ncol(expr)))
#' meta <- data.frame(
#'   Condition = rep(c("Normal", "Disease"), each = 5),
#'   row.names = colnames(expr),
#'   stringsAsFactors = FALSE
#' )
#' gexpipe_run_limma_on_subset(expr, meta)
#' @export
gexpipe_run_limma_on_subset <- function(
  expr,
  metadata,
  logfc_cutoff = 0.5,
  padj_cutoff = 0.05,
  ref_lab = "Normal",
  alt_lab = "Disease"
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

  metadata <- .gexpipe_align_metadata_to_expr(expr, metadata)
  metadata$Condition <- factor(
    as.character(metadata$Condition),
    levels = c(ref_lab, alt_lab)
  )
  n_ref <- sum(metadata$Condition == ref_lab, na.rm = TRUE)
  n_alt <- sum(metadata$Condition == alt_lab, na.rm = TRUE)
  if (n_ref < 2L || n_alt < 2L) {
    stop(
      "Need at least 2 ", ref_lab, " and 2 ", alt_lab,
      " samples on this platform (have ", n_ref, " / ", n_alt, ")."
    )
  }

  n_ds <- if ("Dataset" %in% colnames(metadata)) {
    length(unique(as.character(metadata$Dataset)))
  } else {
    1L
  }

  if (n_ds <= 1L) {
    design <- stats::model.matrix(~ 0 + Condition, data = metadata)
    colnames(design) <- levels(metadata$Condition)
    filt <- gexpipe_independent_filter(expr, design = design)
    expr_f <- filt$expr
    contrast_expr <- paste0(alt_lab, " - ", ref_lab)
    contrast <- limma::makeContrasts(contrasts = contrast_expr, levels = design)
    fit <- limma::lmFit(expr_f, design)
    fit2 <- limma::contrasts.fit(fit, contrast)
    fit2 <- limma::eBayes(fit2)
    tt <- limma::topTable(fit2, number = Inf, adjust.method = "BH")
    formula_desc <- paste0("~ Condition (contrast: ", alt_lab, " vs ", ref_lab, ")")
  } else {
    de_design <- gexpipe_build_de_design(metadata)
    filt <- gexpipe_independent_filter(expr, design = de_design$design)
    expr_f <- filt$expr
    fit <- limma::lmFit(expr_f, de_design$design)
    fit2 <- limma::eBayes(fit)
    tt <- limma::topTable(
      fit2,
      coef = de_design$coef_condition,
      number = Inf,
      adjust.method = "BH"
    )
    formula_desc <- de_design$formula_desc
  }

  tt$Gene <- rownames(tt)
  de_results <- tt[, c("Gene", "logFC", "AveExpr", "P.Value", "adj.P.Val")]
  de_results <- gexpipe_classify_de_significance(de_results, logfc_cutoff, padj_cutoff)
  rownames(de_results) <- de_results$Gene
  sig_genes <- de_results[de_results$Significance != "Not Significant", , drop = FALSE]

  list(
    de_results = de_results,
    sig_genes = sig_genes,
    filter_note = filt$note,
    sample_info = gexpipe_de_sample_info(metadata, method = "limma"),
    formula_desc = formula_desc
  )
}

#' Default Parallel DE methods (scientifically matched per platform)
#'
#' Microarray: always limma on the array matrix. RNA-seq: the user's count
#' engine (DESeq2 / edgeR / voom) on raw counts + Dataset when 2+ GSEs, or
#' limma on TMM log-CPM when that is the chosen RNA method.
#' @noRd
gexpipe_parallel_de_defaults <- function(de_method_rna = "deseq2") {
  rna <- if (!is.null(de_method_rna) && nzchar(de_method_rna) &&
      de_method_rna %in% c("deseq2", "edger", "limma_voom", "limma")) {
    de_method_rna
  } else {
    "deseq2"
  }
  list(rna = rna, micro = "limma")
}

#' Bind RNA-seq count matrices on shared RNA genes (no microarray intersection)
#' @noRd
gexpipe_bind_rna_counts <- function(rna_counts_list) {
  if (is.null(rna_counts_list) || length(rna_counts_list) < 1L) {
    return(NULL)
  }
  mats <- lapply(rna_counts_list, function(m) as.matrix(m))
  gene_sets <- lapply(mats, rownames)
  genes <- if (length(gene_sets) == 1L) {
    gene_sets[[1]]
  } else {
    Reduce(intersect, gene_sets)
  }
  if (length(genes) < 1L) {
    return(NULL)
  }
  raw_list <- list()
  nms <- names(mats)
  if (is.null(nms)) nms <- paste0("RNA", seq_along(mats))
  for (i in seq_along(mats)) {
    keep <- intersect(genes, rownames(mats[[i]]))
    if (length(keep) > 0L) {
      raw_list[[nms[[i]]]] <- mats[[i]][keep, , drop = FALSE]
    }
  }
  if (length(raw_list) < 1L) {
    return(NULL)
  }
  built <- do.call(cbind, raw_list)
  built <- round(built)
  storage.mode(built) <- "integer"
  built
}

#' Count-based DE on one RNA-seq matrix (DESeq2 / edgeR / limma-voom)
#'
#' Uses raw integer counts and includes Dataset in the design when 2+ GSEs
#' are present. Does not mix microarray samples or genes.
#' @noRd
gexpipe_run_count_de <- function(
  counts,
  metadata,
  method = c("deseq2", "edger", "limma_voom"),
  logfc_cutoff = 0.5,
  padj_cutoff = 0.05,
  ref_lab = "Normal",
  alt_lab = "Disease"
) {
  method <- match.arg(method)
  if (is.null(counts) || !is.matrix(counts) || ncol(counts) < 3L) {
    stop("Count DE needs an integer count matrix with at least 3 samples.")
  }
  if (is.null(metadata) || !is.data.frame(metadata) ||
      !"Condition" %in% colnames(metadata)) {
    stop("metadata must contain a Condition column.")
  }
  metadata <- .gexpipe_align_metadata_to_expr(counts, metadata)
  metadata$Condition <- factor(
    as.character(metadata$Condition),
    levels = c(ref_lab, alt_lab)
  )
  n_ref <- sum(metadata$Condition == ref_lab, na.rm = TRUE)
  n_alt <- sum(metadata$Condition == alt_lab, na.rm = TRUE)
  if (n_ref < 2L || n_alt < 2L) {
    stop(
      "Need at least 2 ", ref_lab, " and 2 ", alt_lab,
      " RNA-seq samples (have ", n_ref, " / ", n_alt, ")."
    )
  }

  format_out <- function(de_results, filter_note, formula_desc) {
    de_results <- de_results[!is.na(de_results$adj.P.Val), , drop = FALSE]
    de_results <- gexpipe_classify_de_significance(de_results, logfc_cutoff, padj_cutoff)
    rownames(de_results) <- de_results$Gene
    sig_genes <- de_results[de_results$Significance != "Not Significant", , drop = FALSE]
    list(
      de_results = de_results,
      sig_genes = sig_genes,
      filter_note = filter_note,
      sample_info = gexpipe_de_sample_info(metadata, method = method),
      formula_desc = formula_desc
    )
  }

  if (identical(method, "deseq2")) {
    if (!requireNamespace("DESeq2", quietly = TRUE)) {
      stop("DESeq2 is not installed.")
    }
    ds_design <- gexpipe_deseq2_design(metadata)
    design_mm <- stats::model.matrix(ds_design$formula, data = metadata)
    filt <- gexpipe_independent_filter(counts, design = design_mm)
    counts_f <- filt$expr
    dds <- DESeq2::DESeqDataSetFromMatrix(
      countData = counts_f,
      colData = metadata,
      design = ds_design$formula
    )
    dds <- DESeq2::DESeq(dds, quiet = TRUE)
    res <- DESeq2::results(
      dds,
      contrast = c("Condition", alt_lab, ref_lab),
      alpha = padj_cutoff
    )
    res_df <- as.data.frame(res)
    de_results <- data.frame(
      Gene = rownames(res_df),
      logFC = res_df$log2FoldChange,
      AveExpr = res_df$baseMean,
      P.Value = res_df$pvalue,
      adj.P.Val = res_df$padj,
      stringsAsFactors = FALSE
    )
    return(format_out(de_results, filt$note, ds_design$formula_desc))
  }

  de_design <- gexpipe_build_de_design(metadata)
  filt <- gexpipe_independent_filter(counts, design = de_design$design)
  counts_f <- filt$expr

  if (identical(method, "edger")) {
    if (!requireNamespace("edgeR", quietly = TRUE)) {
      stop("edgeR is not installed.")
    }
    dge <- edgeR::DGEList(counts = counts_f, group = metadata$Condition)
    dge <- edgeR::calcNormFactors(dge, method = "TMM")
    dge <- edgeR::estimateDisp(dge, de_design$design)
    fit <- edgeR::glmQLFit(dge, de_design$design)
    qlf <- edgeR::glmQLFTest(fit, coef = de_design$coef_condition)
    res <- edgeR::topTags(qlf, n = Inf, sort.by = "PValue")$table
    de_results <- data.frame(
      Gene = rownames(res),
      logFC = res$logFC,
      AveExpr = res$logCPM,
      P.Value = res$PValue,
      adj.P.Val = res$FDR,
      stringsAsFactors = FALSE
    )
    return(format_out(de_results, filt$note, de_design$formula_desc))
  }

  v <- limma::voom(counts_f, design = de_design$design, plot = FALSE)
  fit <- limma::lmFit(v, de_design$design)
  fit <- limma::eBayes(fit)
  tt <- limma::topTable(
    fit,
    coef = de_design$coef_condition,
    number = Inf,
    adjust.method = "BH",
    sort.by = "P"
  )
  tt$Gene <- rownames(tt)
  de_results <- tt[, c("Gene", "logFC", "AveExpr", "P.Value", "adj.P.Val")]
  format_out(de_results, filt$note, de_design$formula_desc)
}
