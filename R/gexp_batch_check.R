# ==============================================================================
# GEXP_BATCH_CHECK.R - Quantitative batch-effect check (Step 5)
# ==============================================================================
# Measures how strongly samples still separate by batch (Dataset) before and
# after batch correction, and whether the biological signal (Condition) was
# preserved, so the user can verify the correction numerically and not only
# from the PCA plots. Used for every analysis type (single-track and Parallel).

#' Put a matrix on a log-like scale for PCA (counts / linear values -> log2)
#' @keywords internal
.gexpipe_bc_log_scale <- function(m) {
  mx <- suppressWarnings(max(m, na.rm = TRUE))
  if (is.finite(mx) && mx > 50) log2(pmax(m, 0) + 1) else m
}

#' Mean silhouette width of samples grouped by a factor (Euclidean on PCs)
#' @keywords internal
.gexpipe_bc_silhouette <- function(x, grp) {
  grp <- as.character(grp)
  if (length(unique(grp)) < 2L || nrow(x) < 3L) return(NA_real_)
  d <- as.matrix(stats::dist(x))
  s <- vapply(seq_len(nrow(x)), function(i) {
    own <- grp == grp[i]
    own[i] <- FALSE
    if (!any(own)) return(0)
    a <- mean(d[i, own])
    b <- min(vapply(setdiff(unique(grp), grp[i]), function(g) mean(d[i, grp == g]), numeric(1)))
    if (max(a, b) == 0) 0 else (b - a) / max(a, b)
  }, numeric(1))
  mean(s)
}

#' R-squared of a PC score vector on a factor (one-way ANOVA)
#' @keywords internal
.gexpipe_bc_r2 <- function(y, grp) {
  grp <- as.character(grp)
  ok <- !is.na(grp) & nzchar(grp) & is.finite(y)
  if (sum(ok) < 3L || length(unique(grp[ok])) < 2L) return(NA_real_)
  y <- y[ok]
  grp <- factor(grp[ok])
  tss <- sum((y - mean(y))^2)
  if (tss == 0) return(NA_real_)
  fitted <- stats::ave(y, grp)
  1 - sum((y - fitted)^2) / tss
}

#' Batch-effect metrics for one expression matrix
#'
#' @param mat numeric matrix (genes x samples).
#' @param metadata data.frame with \code{SampleID}, \code{Dataset} and
#'   optionally \code{Condition}.
#' @param n_pcs number of principal components to summarise.
#' @param max_genes most-variable genes used for PCA / silhouette.
#' @return one-row data.frame of metrics.
#' @keywords internal
gexp_batch_effect_metrics <- function(mat, metadata, n_pcs = 5L, max_genes = 5000L) {
  m <- .gexpipe_norm_export_matrix(mat)
  if (is.null(m) || is.null(metadata)) return(NULL)
  md <- as.data.frame(metadata, stringsAsFactors = FALSE)
  sid <- if ("SampleID" %in% names(md)) as.character(md$SampleID) else rownames(md)
  keep <- intersect(colnames(m), sid)
  if (length(keep) < 3L) return(NULL)
  m <- m[, keep, drop = FALSE]
  md <- md[match(keep, sid), , drop = FALSE]
  batch <- as.character(md$Dataset)
  cond <- if ("Condition" %in% names(md)) as.character(md$Condition) else rep(NA_character_, length(keep))
  m <- .gexpipe_bc_log_scale(m)
  m <- m[rowSums(!is.finite(m)) == 0L, , drop = FALSE]
  v <- apply(m, 1L, stats::var)
  m <- m[is.finite(v) & v > 0, , drop = FALSE]
  v <- v[is.finite(v) & v > 0]
  n_batch <- length(unique(batch))
  cond_ok <- sum(!is.na(cond) & nzchar(cond))
  n_cond <- length(unique(cond[!is.na(cond) & nzchar(cond)]))
  out <- data.frame(Genes = nrow(m), Samples = ncol(m), Batches = n_batch,
                    Conditions = n_cond, stringsAsFactors = FALSE)
  if (nrow(m) < 3L) return(out)

  top <- m[order(v, decreasing = TRUE)[seq_len(min(max_genes, nrow(m)))], , drop = FALSE]
  pc <- stats::prcomp(t(top), center = TRUE, scale. = FALSE)
  k <- min(n_pcs, ncol(pc$x))
  ve <- (pc$sdev^2 / sum(pc$sdev^2))[seq_len(k)]
  r2b <- vapply(seq_len(k), function(i) .gexpipe_bc_r2(pc$x[, i], batch), numeric(1))
  r2c <- vapply(seq_len(k), function(i) .gexpipe_bc_r2(pc$x[, i], cond), numeric(1))
  wavg <- function(r2) if (all(is.na(r2))) NA_real_ else 100 * sum(ve * r2, na.rm = TRUE) / sum(ve[!is.na(r2)])
  out$PC1_variance_pct <- 100 * ve[1L]
  out$PC1_batch_R2 <- r2b[1L]
  out$PC1_condition_R2 <- r2c[1L]
  out$TopPC_variance_from_batch_pct <- wavg(r2b)
  out$TopPC_variance_from_condition_pct <- wavg(r2c)
  out$Silhouette_by_batch <- .gexpipe_bc_silhouette(pc$x[, seq_len(k), drop = FALSE], batch)
  out$Silhouette_by_condition <- if (n_cond >= 2L && cond_ok == length(cond)) {
    .gexpipe_bc_silhouette(pc$x[, seq_len(k), drop = FALSE], cond)
  } else {
    NA_real_
  }

  # Share of genes with a significant batch (Dataset) effect, adjusting for
  # Condition when Condition is known and not confounded with batch.
  out$Genes_with_batch_effect_pct <- NA_real_
  if (n_batch >= 2L) {
    b <- factor(batch)
    use_cond <- n_cond >= 2L && cond_ok == length(cond)
    design <- if (use_cond) stats::model.matrix(~ factor(cond) + b) else stats::model.matrix(~ b)
    if (qr(design)$rank < ncol(design)) design <- stats::model.matrix(~ b)
    bcols <- grep("^b", colnames(design))
    if (length(bcols) && nrow(design) > ncol(design)) {
      fit <- tryCatch(limma::eBayes(limma::lmFit(m, design)), error = function(e) NULL)
      if (!is.null(fit)) {
        tt <- limma::topTable(fit, coef = bcols, number = Inf, sort.by = "none")
        out$Genes_with_batch_effect_pct <- 100 * mean(tt$adj.P.Val < 0.05, na.rm = TRUE)
      }
    }
  }
  out
}

#' Before / after batch-effect check table with a plain-language verdict
#'
#' @param pairs named list; each element list(before = matrix, after = matrix).
#'   Names label the matrices (e.g. "Combined", "Microarray", "RNA-seq").
#' @param metadata unified sample metadata (SampleID, Dataset, Condition).
#' @return data.frame with one row per label x stage plus an Assessment column.
#' @keywords internal
gexp_batch_check_table <- function(pairs, metadata) {
  rows <- list()
  for (lab in names(pairs)) {
    p <- pairs[[lab]]
    if (is.null(p$after)) next
    mb <- gexp_batch_effect_metrics(p$before, metadata)
    ma <- gexp_batch_effect_metrics(p$after, metadata)
    if (is.null(ma)) next
    verdict <- .gexpipe_bc_verdict(mb, ma)
    for (st in c("Before", "After")) {
      r <- if (st == "Before") mb else ma
      if (is.null(r)) next
      rows[[length(rows) + 1L]] <- cbind(
        data.frame(Data = lab, Stage = st, stringsAsFactors = FALSE),
        r,
        data.frame(Assessment = if (st == "After") verdict else "", stringsAsFactors = FALSE)
      )
    }
  }
  if (!length(rows)) return(NULL)
  cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(r) {
    for (cn in setdiff(cols, names(r))) r[[cn]] <- NA
    r[, cols, drop = FALSE]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' @keywords internal
.gexpipe_bc_verdict <- function(before, after) {
  if (is.null(after$Batches) || after$Batches < 2L) {
    return("Single dataset - no between-study batch to correct.")
  }
  ba <- after$TopPC_variance_from_batch_pct
  bb <- if (!is.null(before)) before$TopPC_variance_from_batch_pct else NA_real_
  msg <- character(0)
  if (is.na(ba)) {
    msg <- "Batch effect could not be measured."
  } else if (ba < 10) {
    msg <- sprintf("OK: batch explains %.1f%% of top-PC variance after correction", ba)
  } else if (ba < 25) {
    msg <- sprintf("Mild batch effect remains: %.1f%% of top-PC variance", ba)
  } else {
    msg <- sprintf("WARNING: strong batch effect remains: %.1f%% of top-PC variance", ba)
  }
  if (!is.na(bb) && !is.na(ba)) msg <- paste0(msg, sprintf(" (was %.1f%% before)", bb))
  ca <- after$TopPC_variance_from_condition_pct
  cb <- if (!is.null(before)) before$TopPC_variance_from_condition_pct else NA_real_
  if (!is.null(ca) && !is.null(cb) && !is.na(ca) && !is.na(cb) && cb > 5 && ca < 0.5 * cb) {
    msg <- paste0(msg, sprintf(". Check over-correction: condition signal fell from %.1f%% to %.1f%%", cb, ca))
  }
  paste0(msg, ".")
}

#' Write the batch before / after export (matrices, metadata, check, README)
#' @keywords internal
gexp_write_batch_before_after <- function(out_dir, pairs, metadata, check_table,
                                          analysis_type = "", methods = character(0)) {
  dir.create(file.path(out_dir, "1_before_batch_correction"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(out_dir, "2_after_batch_correction"), showWarnings = FALSE, recursive = TRUE)
  safe <- function(x) gsub("[^A-Za-z0-9._-]", "_", x)
  wr <- function(m, f) {
    m <- .gexpipe_norm_export_matrix(m)
    if (!is.null(m)) data.table::fwrite(data.table::as.data.table(m, keep.rownames = "Gene"), f)
  }
  for (lab in names(pairs)) {
    wr(pairs[[lab]]$before, file.path(out_dir, "1_before_batch_correction", paste0(safe(lab), "_before_batch.csv")))
    wr(pairs[[lab]]$after, file.path(out_dir, "2_after_batch_correction", paste0(safe(lab), "_after_batch.csv")))
  }
  if (!is.null(metadata)) utils::write.csv(metadata, file.path(out_dir, "Sample_metadata.csv"), row.names = FALSE)
  if (!is.null(check_table)) utils::write.csv(check_table, file.path(out_dir, "Batch_effect_check.csv"), row.names = FALSE)
  meth <- if (length(methods)) paste0("  ", names(methods), ": ", methods, collapse = "\n") else "  (not recorded)"
  writeLines(c(
    "GExPipe - batch correction before / after export",
    paste0("Created: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Analysis type: ", analysis_type),
    "Methods:", meth,
    "",
    "1_before_batch_correction/  Normalized, variance-filtered matrix BEFORE batch correction (same genes and samples as 'after').",
    "2_after_batch_correction/   Batch-corrected matrix used for differential expression (Step 6).",
    "Sample_metadata.csv         Batch (Dataset), Platform and Condition of every sample.",
    "",
    "How to read Batch_effect_check.csv (batch = Dataset / GSE):",
    "  TopPC_variance_from_batch_pct      % of the variance in the top 5 PCs explained by batch. Should DROP after",
    "                                     correction; < 10% is good, > 25% means a strong batch effect remains.",
    "  PC1_batch_R2                       R-squared of PC1 on batch (0 = PC1 unrelated to batch).",
    "  Silhouette_by_batch                How strongly samples cluster by batch (-1..1). Should fall towards 0 or below.",
    "  Genes_with_batch_effect_pct        % of genes with a significant batch effect (limma F-test, adj. P < 0.05,",
    "                                     adjusted for Condition). Should DROP after correction.",
    "  TopPC_variance_from_condition_pct  Biological signal; should be kept (not collapse) after correction.",
    "                                     A large fall suggests over-correction or batch confounded with condition.",
    "  Assessment                         Plain-language verdict for each corrected matrix.",
    "Counts (e.g. RNA-seq for DESeq2) are log2(x+1)-transformed for these metrics only; exported files are unchanged."
  ), file.path(out_dir, "README.txt"))
  invisible(out_dir)
}
