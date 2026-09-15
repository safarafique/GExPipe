## GExPipe normalization + common-genes helpers
##
## This file mirrors the main normalization pipeline currently implemented
## inside inst/shinyapp/server/server_normalize.R, but as reusable functions
## under R/. Existing Shiny code still uses the inline version; you can
## gradually switch server_normalize.R to call these helpers.

utils::globalVariables(c("."))

#' Detect whether an expression matrix is already on a log2-like scale
#' @keywords internal
gexp_expr_looks_log2 <- function(expr) {
  if (is.null(expr) || !is.matrix(expr) || length(expr) == 0L) {
    return(FALSE)
  }
  vals <- as.numeric(expr[is.finite(expr)])
  if (length(vals) < 10L) {
    return(FALSE)
  }
  mx <- max(vals)
  mn <- min(vals)
  # Raw intensities/counts typically exceed 50; log2 microarray/CPM stay below that.
  mx <= 50 && mn > -20
}

#' Detect FPKM/TPM-like RNA-seq (non-integer, not raw counts)
#' @keywords internal
gexp_matrix_looks_fpkm <- function(expr) {
  if (is.null(expr) || !is.matrix(expr) || length(expr) == 0L) {
    return(FALSE)
  }
  vals <- as.numeric(expr[is.finite(expr)])
  if (length(vals) < 10L) {
    return(FALSE)
  }
  if (any(vals < -0.01)) {
    return(FALSE)
  }
  frac_int <- mean(abs(vals - round(vals)) < 1e-6)
  mx <- max(vals)
  # Integer counts -> TMM. Continuous abundance (FPKM/TPM) -> log2(x+1).
  frac_int < 0.85 && mx > 1 && mx < 1e7
}

#' Column-median boxplots differ enough to need quantile normalization
#' @keywords internal
gexp_boxplots_misaligned <- function(expr) {
  if (is.null(expr) || !is.matrix(expr) || ncol(expr) < 2L) {
    return(FALSE)
  }
  meds <- apply(expr, 2L, stats::median, na.rm = TRUE)
  meds <- meds[is.finite(meds)]
  if (length(meds) < 2L) {
    return(FALSE)
  }
  iqr_med <- stats::IQR(meds)
  if (isTRUE(gexp_expr_looks_log2(expr))) {
    return(iqr_med > 0.5)
  }
  iqr_med > max(20, 0.2 * abs(stats::median(meds)))
}

#' Infer microarray vendor from a GEO GPL id
#' @keywords internal
.gexpipe_micro_vendor <- function(platform_id = "") {
  id <- if (is.null(platform_id) || length(platform_id) == 0L || is.na(platform_id[[1L]])) {
    ""
  } else {
    toupper(trimws(as.character(platform_id[[1L]])))
  }
  if (!nzchar(id)) {
    return("processed")
  }
  affy <- c(
    "GPL96", "GPL97", "GPL201", "GPL570", "GPL571", "GPL8300", "GPL1261",
    "GPL6244", "GPL16686", "GPL11532", "GPL17586", "GPL13158", "GPL15207",
    "GPL3921", "GPL1352"
  )
  illumina <- c(
    "GPL6104", "GPL6947", "GPL10558", "GPL10904", "GPL6883", "GPL6884",
    "GPL8490", "GPL13667", "GPL24676"
  )
  agilent <- c(
    "GPL16699", "GPL17077", "GPL4133", "GPL6480", "GPL13497", "GPL10332",
    "GPL13607", "GPL14550", "GPL21185", "GPL4134", "GPL1708", "GPL887"
  )
  if (id %in% affy || grepl("AFFY|HGU133|HUEX|HTA|PRIMEVIEW", id)) {
    return("affymetrix")
  }
  if (id %in% illumina || grepl("ILLUMINA", id)) {
    return("illumina")
  }
  if (id %in% agilent || grepl("AGILENT", id)) {
    return("agilent")
  }
  "processed"
}

#' Choose microarray normalization method from the GExPipe platform table
#' @keywords internal
gexp_choose_micro_norm_method <- function(expr, platform_id = "", cel_paths = NULL) {
  vendor <- .gexpipe_micro_vendor(platform_id)
  has_cel <- length(cel_paths) > 0L && any(file.exists(as.character(cel_paths)))
  looks_log <- isTRUE(gexp_expr_looks_log2(expr))
  if (identical(vendor, "affymetrix") && isTRUE(has_cel)) {
    return("rma")
  }
  if (identical(vendor, "illumina") && !looks_log) {
    return("neqc")
  }
  if (identical(vendor, "agilent") && !looks_log) {
    return("normexp")
  }
  if (!looks_log) {
    return("log2_quantile")
  }
  if (isTRUE(gexp_boxplots_misaligned(expr))) {
    return("quantile")
  }
  "as_is"
}

#' Choose RNA-seq normalization method from the GExPipe platform table
#' @keywords internal
gexp_choose_rnaseq_norm_method <- function(expr) {
  if (isTRUE(gexp_matrix_looks_fpkm(expr))) {
    return("log2fpkm")
  }
  "TMM"
}

#' Apply a microarray matrix method (series-matrix / processed data)
#' @keywords internal
gexp_normalize_microarray_matrix <- function(expr_matrix, method = "quantile") {
  initial_genes <- nrow(expr_matrix)
  expr_matrix <- as.matrix(expr_matrix)
  storage.mode(expr_matrix) <- "numeric"
  expr_matrix <- expr_matrix[rowSums(is.na(expr_matrix)) < ncol(expr_matrix), , drop = FALSE]
  expr_matrix <- expr_matrix[, colSums(is.na(expr_matrix)) < nrow(expr_matrix), drop = FALSE]
  genes_after_na_removal <- nrow(expr_matrix)
  log2_applied <- FALSE
  used <- method

  apply_log2 <- function(m) {
    mn <- min(m, na.rm = TRUE)
    if (is.finite(mn) && mn < 0) {
      m <- m - mn + 1
    }
    log2(m + 1)
  }

  if (identical(method, "as_is") || identical(method, "quantile_if_needed")) {
    if (identical(method, "quantile_if_needed") && isTRUE(gexp_boxplots_misaligned(expr_matrix))) {
      expr_norm <- limma::normalizeBetweenArrays(expr_matrix, method = "quantile")
      used <- "quantile (boxplots misaligned)"
    } else {
      expr_norm <- expr_matrix
      used <- if (identical(method, "as_is")) "already-log2" else "already-log2 (quantile skipped)"
    }
  } else if (identical(method, "log2_quantile")) {
    if (!isTRUE(gexp_expr_looks_log2(expr_matrix))) {
      expr_matrix <- apply_log2(expr_matrix)
      log2_applied <- TRUE
    }
    expr_norm <- limma::normalizeBetweenArrays(expr_matrix, method = "quantile")
    used <- "log2+quantile"
  } else if (identical(method, "normexp")) {
    if (!isTRUE(gexp_expr_looks_log2(expr_matrix))) {
      bc <- tryCatch({
        el <- methods::new("EList", list(E = expr_matrix))
        el <- limma::backgroundCorrect(el, method = "normexp", offset = 16)
        el <- limma::normalizeBetweenArrays(el, method = "quantile")
        list(ok = TRUE, mat = as.matrix(el$E))
      }, error = function(e) list(ok = FALSE, mat = NULL))
      if (isTRUE(bc$ok)) {
        expr_norm <- bc$mat
        log2_applied <- TRUE
        used <- "normexp+quantile+log2"
      } else {
        expr_norm <- limma::normalizeBetweenArrays(apply_log2(expr_matrix), method = "quantile")
        log2_applied <- TRUE
        used <- "log2+quantile (normexp fallback)"
      }
    } else {
      expr_norm <- limma::normalizeBetweenArrays(expr_matrix, method = "quantile")
      used <- "quantile (Agilent already log2)"
    }
  } else if (identical(method, "neqc")) {
    if (!isTRUE(gexp_expr_looks_log2(expr_matrix))) {
      expr_matrix <- apply_log2(expr_matrix)
      log2_applied <- TRUE
    }
    expr_norm <- limma::normalizeBetweenArrays(expr_matrix, method = "quantile")
    used <- "neqc-fallback (quantile)"
  } else {
    if (!isTRUE(gexp_expr_looks_log2(expr_matrix))) {
      expr_matrix <- apply_log2(expr_matrix)
      log2_applied <- TRUE
    }
    expr_norm <- limma::normalizeBetweenArrays(expr_matrix, method = "quantile")
    used <- if (log2_applied) "log2+quantile" else "quantile"
  }

  attr(expr_norm, "normalization_info") <- list(
    initial_genes = initial_genes,
    genes_after_na_removal = genes_after_na_removal,
    final_genes = nrow(expr_norm),
    log2_applied = log2_applied,
    method = used
  )
  expr_norm
}

#' Normalize RNA-seq FPKM/TPM with log2(x+1)
#' @keywords internal
gexp_normalize_rnaseq_fpkm <- function(expr_matrix, dataset_name = NULL) {
  initial_genes <- nrow(expr_matrix)
  expr_matrix <- as.matrix(expr_matrix)
  storage.mode(expr_matrix) <- "numeric"
  expr_matrix[expr_matrix < 0] <- 0
  keep <- rowSums(is.finite(expr_matrix)) > 0L
  expr_matrix <- expr_matrix[keep, , drop = FALSE]
  expr_norm <- log2(expr_matrix + 1)
  attr(expr_norm, "normalization_info") <- list(
    initial_genes = initial_genes,
    genes_after_filtering = nrow(expr_norm),
    genes_removed = initial_genes - nrow(expr_norm),
    final_genes = nrow(expr_norm),
    method = "log2(x+1)"
  )
  expr_norm
}

#' Drop genes low on either platform after merge
#' @keywords internal
.gexpipe_drop_low_in_either_platform <- function(all_expr_norm, micro_gses, q = 0.10) {
  if (length(all_expr_norm) < 2L) {
    return(all_expr_norm)
  }
  micro_gses <- intersect(micro_gses, names(all_expr_norm))
  rna_gses <- setdiff(names(all_expr_norm), micro_gses)
  if (length(micro_gses) == 0L || length(rna_gses) == 0L) {
    return(all_expr_norm)
  }
  genes <- rownames(all_expr_norm[[1L]])
  micro_mat <- do.call(cbind, all_expr_norm[micro_gses])
  rna_mat <- do.call(cbind, all_expr_norm[rna_gses])
  micro_med <- apply(micro_mat, 1L, stats::median, na.rm = TRUE)
  rna_med <- apply(rna_mat, 1L, stats::median, na.rm = TRUE)
  keep <- micro_med > stats::quantile(micro_med, q, na.rm = TRUE) &
    rna_med > stats::quantile(rna_med, q, na.rm = TRUE)
  keep_genes <- genes[keep]
  if (length(keep_genes) < 50L) {
    return(all_expr_norm)
  }
  lapply(all_expr_norm, function(m) m[keep_genes, , drop = FALSE])
}

#' Column-bind matrices on the union of genes (missing values stay NA)
#' @noRd
.gexpipe_cbind_union_na <- function(...) {
  mats <- list(...)
  if (length(mats) == 1L && is.list(mats[[1L]]) && !is.matrix(mats[[1L]])) {
    mats <- mats[[1L]]
  }
  mats <- Filter(function(m) !is.null(m) && is.matrix(m) && nrow(m) > 0L && ncol(m) > 0L, mats)
  if (length(mats) == 0L) {
    return(NULL)
  }
  if (length(mats) == 1L) {
    return(mats[[1L]])
  }
  genes <- unique(unlist(lapply(mats, rownames), use.names = FALSE))
  samples <- unlist(lapply(mats, colnames), use.names = FALSE)
  out <- matrix(
    NA_real_,
    nrow = length(genes),
    ncol = length(samples),
    dimnames = list(genes, samples)
  )
  for (m in mats) {
    out[rownames(m), colnames(m)] <- m
  }
  out
}

#' Normalize microarray and RNA-seq datasets and compute common genes
#'
#' This is a non-Shiny helper version of the Step 2 (Normalize) logic:
#' - per-dataset normalization (microarray + RNA-seq),
#' - intersection of gene sets (common genes),
#' - global quantile normalization of the combined matrix,
#' - optional extraction of RNA-seq raw counts for count-based DE.
#'
#' @param micro_expr_list named list of microarray expression matrices.
#' @param rna_counts_list named list of RNA-seq count matrices.
#' @param micro_norm_method "quantile" or "rma" (RMA requires CEL paths).
#' @param rnaseq_norm_method "TMM" or "log2cpm_only".
#' @param micro_cel_paths optional named list of CEL paths per GSE (for RMA).
#' @param platform_per_gse optional named vector giving platform IDs per GSE.
#' @param de_method differential expression method (used to decide whether to
#'   save raw counts for DESeq2/edgeR/limma-voom); one of "limma",
#'   "limma_voom", "deseq2", "edger".
#'
#' @return A list with elements:
#'   \item{combined_expr}{globally quantile-normalized matrix (genes x samples)}
#'   \item{combined_expr_before_global}{matrix before global quantile}
#'   \item{all_expr_norm_list}{normalized per-dataset matrices (common genes only)}
#'   \item{common_genes}{character vector of common genes}
#'   \item{normalization_stats}{list of per-dataset normalization info}
#'   \item{normalization_summary_table}{data.frame summarizing gene counts}
#'   \item{raw_counts_for_deseq2}{integer matrix of RNA-seq counts (optional)}
#'   \item{raw_counts_metadata}{data.frame with sample metadata for raw counts (optional)}
#'   \item{unified_metadata}{data.frame with SampleID, Platform, Dataset, Condition=NA}
#'   \item{log_text}{character string with a human-readable log}
#'
#' @examples
#' out <- withr::with_seed(1, {
#'   m1 <- matrix(abs(rnorm(120)), nrow = 20, ncol = 6)
#'   m2 <- matrix(abs(rnorm(120)), nrow = 20, ncol = 6)
#'   rownames(m1) <- rownames(m2) <- paste0("Gene", seq_len(20))
#'   colnames(m1) <- paste0("D1_S", seq_len(6))
#'   colnames(m2) <- paste0("D2_S", seq_len(6))
#'   gexp_normalize_and_intersect(
#'     micro_expr_list = list(D1 = m1, D2 = m2),
#'     rna_counts_list = list(),
#'     de_method = "limma"
#'   )
#' })
#' dim(out$combined_expr)
#' @param micro_eset_list optional named list of ExpressionSet objects per GSE (for RMA ID mapping).
#' @param apply_global_quantile logical; if TRUE, apply limma quantile normalization across all samples.
#' @param keep_platforms_separate logical; if TRUE (Parallel DE), normalize each
#'   platform on its own gene set. Do not intersect RNA-seq with microarray,
#'   do not drop genes low on the other platform, and do not apply global
#'   quantile. Matrices meet only at Step 7 consensus DEGs.
#' @export
gexp_normalize_and_intersect <- function(
  micro_expr_list,
  rna_counts_list,
  micro_norm_method = "quantile",
  rnaseq_norm_method = "TMM",
  micro_cel_paths = NULL,
  platform_per_gse = NULL,
  micro_eset_list = NULL,
  de_method = "limma",
  apply_global_quantile = TRUE,
  keep_platforms_separate = FALSE
) {
  all_expr_norm <- list()
  normalization_stats <- list()
  micro_detail <- character()
  rna_detail <- character()
  log_text <- "Normalizing each dataset with its platform method first...\n\n"

  apply_rma_one <- function(gse) {
    plat <- if (!is.null(platform_per_gse)) platform_per_gse[[gse]] else NULL
    probe_mat <- normalize_microarray_rma(micro_cel_paths[[gse]], plat, dataset_name = gse)
    if (is.null(probe_mat) || nrow(probe_mat) < 1L || ncol(probe_mat) < 1L) {
      return(NULL)
    }
    micro_eset <- if (!is.null(micro_eset_list) && gse %in% names(micro_eset_list)) {
      micro_eset_list[[gse]]
    } else {
      NULL
    }
    fdata <- if (!is.null(micro_eset)) Biobase::fData(micro_eset) else data.frame()
    gene_symbols <- map_microarray_ids(probe_mat, fdata, micro_eset, gse_id = gse)
    if (length(gene_symbols) != nrow(probe_mat)) {
      gene_symbols <- rownames(probe_mat)
    }
    rownames(probe_mat) <- gene_symbols
    valid <- !is.na(gene_symbols) & trimws(gene_symbols) != ""
    expr_norm <- probe_mat[valid, , drop = FALSE]
    if (nrow(expr_norm) > 0 && any(duplicated(rownames(expr_norm)))) {
      expr_norm <- limma::avereps(expr_norm, ID = rownames(expr_norm))
    }
    norm_info <- attr(probe_mat, "normalization_info")
    if (is.null(norm_info)) {
      norm_info <- list(
        initial_genes = nrow(probe_mat),
        final_genes = nrow(expr_norm),
        method = "RMA"
      )
    }
    attr(expr_norm, "normalization_info") <- norm_info
    expr_norm
  }

  # ---- Microarray normalization ----
  if (length(micro_expr_list) > 0) {
    log_text <- paste0(log_text, "Microarray (requested: ", micro_norm_method, "):\n")
    for (gse in names(micro_expr_list)) {
      plat <- ""
      if (!is.null(platform_per_gse) && gse %in% names(platform_per_gse)) {
        plat <- as.character(platform_per_gse[[gse]])[[1L]]
      }
      cels <- if (!is.null(micro_cel_paths)) micro_cel_paths[[gse]] else NULL
      method_i <- micro_norm_method
      if (identical(micro_norm_method, "auto")) {
        method_i <- gexp_choose_micro_norm_method(micro_expr_list[[gse]], plat, cels)
      }
      use_rma <- identical(method_i, "rma") && length(cels) > 0L
      expr_norm <- NULL
      if (use_rma) {
        expr_norm <- apply_rma_one(gse)
        if (is.null(expr_norm)) {
          method_i <- "log2_quantile"
        }
      }
      if (is.null(expr_norm)) {
        expr_norm <- gexp_normalize_microarray_matrix(micro_expr_list[[gse]], method = method_i)
      }
      norm_info <- attr(expr_norm, "normalization_info")
      all_expr_norm[[gse]] <- expr_norm
      normalization_stats[[gse]] <- norm_info
      line <- paste0(
        "  ", gse, ": ",
        if (!is.null(norm_info$method)) paste0(norm_info$method, " | ") else "",
        format(norm_info$initial_genes, big.mark = ","), " \u2192 ",
        format(norm_info$final_genes, big.mark = ","), " genes \u2713\n"
      )
      micro_detail <- c(micro_detail, line)
      log_text <- paste0(log_text, line)
    }
  }

  # ---- RNA-seq normalization ----
  if (length(rna_counts_list) > 0) {
    log_text <- paste0(log_text, "\nRNA-seq (requested: ", rnaseq_norm_method, "):\n")
    for (gse in names(rna_counts_list)) {
      method_i <- rnaseq_norm_method
      if (identical(rnaseq_norm_method, "auto")) {
        method_i <- gexp_choose_rnaseq_norm_method(as.matrix(rna_counts_list[[gse]]))
      }
      if (identical(method_i, "log2fpkm")) {
        expr_norm <- gexp_normalize_rnaseq_fpkm(rna_counts_list[[gse]], dataset_name = gse)
      } else {
        expr_norm <- normalize_rnaseq(rna_counts_list[[gse]], dataset_name = gse, method = method_i)
      }
      norm_info <- attr(expr_norm, "normalization_info")
      all_expr_norm[[gse]] <- expr_norm
      normalization_stats[[gse]] <- norm_info
      line <- paste0(
        "  ", gse, ": ",
        if (!is.null(norm_info$method)) paste0(norm_info$method, " | ") else "",
        format(norm_info$initial_genes, big.mark = ","), " \u2192 ",
        format(if (!is.null(norm_info$genes_after_filtering)) {
          norm_info$genes_after_filtering
        } else {
          norm_info$final_genes
        }, big.mark = ","),
        if (!is.null(norm_info$genes_removed) && norm_info$genes_removed > 0) {
          paste0(" (removed ", format(norm_info$genes_removed, big.mark = ","), " low-expression)")
        } else {
          ""
        },
        " \u2713\n"
      )
      rna_detail <- c(rna_detail, line)
      log_text <- paste0(log_text, line)
    }
  }

  # ---- Gene sets: merged intersects; Parallel keeps platforms separate ----
  gene_lists <- lapply(all_expr_norm, rownames)
  initial_total <- sum(vapply(normalization_stats, function(info) {
    if (!is.null(info)) info$initial_genes else 0L
  }, integer(1)), na.rm = TRUE)

  after_filter_total <- sum(vapply(normalization_stats, function(info) {
    if (!is.null(info)) {
      if (!is.null(info$genes_after_filtering)) {
        info$genes_after_filtering
      } else {
        info$final_genes
      }
    } else {
      0L
    }
  }, integer(1)), na.rm = TRUE)

  rnaseq_removed <- sum(vapply(normalization_stats, function(info) {
    if (!is.null(info) && !is.null(info$genes_removed)) info$genes_removed else 0L
  }, integer(1)), na.rm = TRUE)

  micro_gses <- names(micro_expr_list)
  rna_gses <- names(rna_counts_list)
  micro_names <- intersect(micro_gses, names(all_expr_norm))
  rna_names <- intersect(rna_gses, names(all_expr_norm))
  intersect_named <- function(nm) {
    if (length(nm) == 0L) {
      return(character())
    }
    Reduce(intersect, lapply(all_expr_norm[nm], rownames))
  }
  micro_genes <- intersect_named(micro_names)
  rna_genes <- intersect_named(rna_names)

  if (isTRUE(keep_platforms_separate)) {
    apply_global_quantile <- FALSE
    if (length(micro_names) > 0L && length(micro_genes) == 0L) {
      stop("No genes remain in microarray after per-dataset normalization.")
    }
    if (length(rna_names) > 0L && length(rna_genes) == 0L) {
      stop("No genes remain in RNA-seq after per-dataset normalization.")
    }
    for (gse in micro_names) {
      all_expr_norm[[gse]] <- all_expr_norm[[gse]][micro_genes, , drop = FALSE]
    }
    for (gse in rna_names) {
      all_expr_norm[[gse]] <- all_expr_norm[[gse]][rna_genes, , drop = FALSE]
    }
    common_genes <- if (length(micro_genes) > 0L && length(rna_genes) > 0L) {
      intersect(micro_genes, rna_genes)
    } else if (length(micro_genes) > 0L) {
      micro_genes
    } else {
      rna_genes
    }
    final_count <- length(common_genes)
    filter_method <- "per_platform_separate"
  } else {
    log_text <- paste0(
      log_text,
      "\nAutomatic gene filtering (background process):\n",
      "  Filtering to common genes ensures consistent gene sets across datasets.\n",
      "  This is required for accurate batch correction and differential expression analysis.\n"
    )

    common_genes <- Reduce(intersect, gene_lists)
    if (length(common_genes) == 0L) {
      stop(
        "No common genes across datasets after normalization. ",
        "Re-run Step 1 and confirm gene symbols overlap (see download log for ID mapping)."
      )
    }
    for (i in seq_along(all_expr_norm)) {
      all_expr_norm[[i]] <- all_expr_norm[[i]][common_genes, , drop = FALSE]
    }

    n_before_low <- length(common_genes)
    all_expr_norm <- .gexpipe_drop_low_in_either_platform(all_expr_norm, names(micro_expr_list))
    common_genes <- rownames(all_expr_norm[[1L]])
    if (length(common_genes) < n_before_low) {
      log_text <- paste0(
        log_text,
        "  Removed genes low on either platform: ",
        format(n_before_low, big.mark = ","), " \u2192 ",
        format(length(common_genes), big.mark = ","), "\n"
      )
    }

    final_count <- length(common_genes)
    if (length(gene_lists) > 1L) {
      log_text <- paste0(log_text, "  Per-dataset genes not in intersection:\n")
      for (gse in names(gene_lists)) {
        n_ds <- length(gene_lists[[gse]])
        n_lost <- n_ds - final_count
        log_text <- paste0(
          log_text, "    ", gse, ": ", format(n_ds, big.mark = ","), " \u2192 ",
          format(final_count, big.mark = ","), " shared (", format(n_lost, big.mark = ","), " dropped)\n"
        )
      }
    }
    log_text <- paste0(
      log_text,
      "  \u2713 Common genes identified: ",
      format(final_count, big.mark = ","), "\n"
    )
    filter_method <- "intersection"
    micro_genes <- common_genes
    rna_genes <- common_genes
  }
  if (!isTRUE(apply_global_quantile)) {
    log_text <- paste0(log_text, "  Global quantile normalization: skipped (per-dataset normalization only).\n")
  }

  normalization_stats_global <- list(
    initial_total = initial_total,
    after_filter_total = after_filter_total,
    rnaseq_removed = rnaseq_removed,
    final_count = final_count,
    filter_method = filter_method,
    micro_genes = length(micro_genes),
    rna_genes = length(rna_genes)
  )

  expr_micro <- if (length(micro_names) > 0L) {
    do.call(cbind, all_expr_norm[micro_names])
  } else {
    NULL
  }
  expr_rna <- if (length(rna_names) > 0L) {
    do.call(cbind, all_expr_norm[rna_names])
  } else {
    NULL
  }

  # ---- Combine and optional global quantile normalization ----
  combined_before_global <- if (isTRUE(keep_platforms_separate)) {
    .gexpipe_cbind_union_na(expr_micro, expr_rna)
  } else {
    do.call(cbind, all_expr_norm)
  }
  combined_expr <- if (isTRUE(apply_global_quantile) && ncol(combined_before_global) > 1L) {
    limma::normalizeBetweenArrays(combined_before_global, method = "quantile")
  } else {
    combined_before_global
  }

  # ---- Optional raw counts for count-based DE ----
  raw_counts_for_deseq2 <- NULL
  raw_counts_metadata <- NULL
  save_raw <- length(rna_counts_list) > 0 && (
    isTRUE(keep_platforms_separate) || de_method %in% c("deseq2", "edger", "limma_voom")
  )
  genes_for_raw <- if (isTRUE(keep_platforms_separate)) rna_genes else common_genes
  if (isTRUE(save_raw)) {
    raw_counts_list <- list()
    for (gse in names(rna_counts_list)) {
      raw_mat <- as.matrix(rna_counts_list[[gse]])
      common_in_raw <- intersect(genes_for_raw, rownames(raw_mat))
      if (length(common_in_raw) > 0) {
        raw_counts_list[[gse]] <- raw_mat[common_in_raw, , drop = FALSE]
      }
    }
    if (length(raw_counts_list) > 0) {
      negative_sources <- names(raw_counts_list)[vapply(
        raw_counts_list, .gexpipe_matrix_has_negative, logical(1)
      )]
      raw_counts_for_deseq2 <- do.call(cbind, raw_counts_list)
      raw_counts_for_deseq2 <- round(raw_counts_for_deseq2)
      storage.mode(raw_counts_for_deseq2) <- "integer"
      attr(raw_counts_for_deseq2, "negative_sources") <- negative_sources
      log_text <- paste0(
        log_text,
        "  \u2713 Raw counts saved for DESeq2/edgeR/voom: ",
        format(nrow(raw_counts_for_deseq2), big.mark = ","), " genes \u00d7 ",
        format(ncol(raw_counts_for_deseq2), big.mark = ","), " samples\n"
      )
      if (length(negative_sources) > 0) {
        log_text <- paste0(
          log_text,
          "  \u26a0 Negative values detected in: ", paste(negative_sources, collapse = ", "),
          " - GEO supplied normalized/log-scale values instead of raw counts.\n",
          "    Count-based DE (DESeq2/edgeR/voom) cannot use these; limma will be used instead.\n"
        )
      }
    }
  }

  # ---- Unified metadata (aligned to combined expression columns) ----
  dataset_labels <- rep(names(all_expr_norm), times = vapply(all_expr_norm, ncol, integer(1)))
  micro_gses <- names(micro_expr_list)
  platform_labels <- ifelse(dataset_labels %in% micro_gses, "Microarray", "RNAseq")
  sample_ids <- colnames(combined_expr)
  if (length(platform_labels) != length(sample_ids) || length(dataset_labels) != length(sample_ids)) {
    stop(
      "Sample metadata alignment failed during normalization (",
      length(sample_ids), " expression columns vs ",
      length(platform_labels), " platform labels). ",
      "If using RMA, ensure CEL sample count matches the series matrix."
    )
  }

  unified_metadata <- data.frame(
    SampleID = sample_ids,
    Platform = platform_labels,
    Dataset = dataset_labels,
    Condition = NA_character_,
    row.names = sample_ids,
    stringsAsFactors = FALSE
  )

  # If we have raw counts, align metadata
  if (!is.null(raw_counts_for_deseq2)) {
    raw_samples <- colnames(raw_counts_for_deseq2)
    meta_samples <- unified_metadata$SampleID
    common_samples <- intersect(raw_samples, meta_samples)
    if (length(common_samples) > 0) {
      raw_counts_for_deseq2 <- raw_counts_for_deseq2[, common_samples, drop = FALSE]
      raw_counts_metadata <- unified_metadata[unified_metadata$SampleID %in% common_samples, , drop = FALSE]
    }
  }

  # ---- Summary table ----
  summary_data <- data.frame(
    Dataset = names(normalization_stats),
    Initial_Genes = vapply(normalization_stats, function(x) {
      if (!is.null(x)) x$initial_genes else 0L
    }, integer(1)),
    After_Filtering = vapply(normalization_stats, function(x) {
      if (!is.null(x)) {
        if (!is.null(x$genes_after_filtering)) {
          x$genes_after_filtering
        } else {
          x$final_genes
        }
      } else {
        0L
      }
    }, integer(1)),
    Final_Common_Genes = vapply(names(normalization_stats), function(gse) {
      if (isTRUE(keep_platforms_separate) && gse %in% micro_names) {
        length(micro_genes)
      } else if (isTRUE(keep_platforms_separate) && gse %in% rna_names) {
        length(rna_genes)
      } else {
        final_count
      }
    }, integer(1)),
    stringsAsFactors = FALSE
  )

  summary_data <- rbind(
    summary_data,
    data.frame(
      Dataset = if (isTRUE(keep_platforms_separate)) "OVERLAP (info only)" else "TOTAL/COMMON",
      Initial_Genes = sum(summary_data$Initial_Genes),
      After_Filtering = sum(summary_data$After_Filtering),
      Final_Common_Genes = final_count,
      stringsAsFactors = FALSE
    )
  )

  log_text_micro <- NULL
  log_text_rna <- NULL
  if (isTRUE(keep_platforms_separate)) {
    hr <- paste(rep("\u2501", 56L), collapse = "")
    n_micro_g <- if (is.null(expr_micro)) 0L else nrow(expr_micro)
    n_micro_s <- if (is.null(expr_micro)) 0L else ncol(expr_micro)
    n_rna_g <- if (is.null(expr_rna)) 0L else nrow(expr_rna)
    n_rna_s <- if (is.null(expr_rna)) 0L else ncol(expr_rna)
    log_text_micro <- paste0(
      hr, "\n",
      "RUN 1 \u2014 MICROARRAY (separate pipeline)\n",
      hr, "\n",
      "Requested: ", micro_norm_method, "\n",
      if (length(micro_detail)) paste(micro_detail, collapse = "") else "  (no microarray datasets)\n",
      "Global quantile: off\n",
      "Merged with RNA-seq: no\n",
      "\nOK Microarray run complete.\n",
      "  Genes:   ", format(n_micro_g, big.mark = ","), "\n",
      "  Samples: ", format(n_micro_s, big.mark = ","), "\n"
    )
    raw_line <- if (!is.null(raw_counts_for_deseq2)) {
      paste0(
        "Raw counts kept for DESeq2/edgeR/voom: ",
        format(nrow(raw_counts_for_deseq2), big.mark = ","), " genes \u00d7 ",
        format(ncol(raw_counts_for_deseq2), big.mark = ","), " samples\n"
      )
    } else {
      "Raw counts: not saved\n"
    }
    log_text_rna <- paste0(
      hr, "\n",
      "RUN 2 \u2014 RNA-SEQ (separate pipeline)\n",
      hr, "\n",
      "Requested: ", rnaseq_norm_method, "\n",
      if (length(rna_detail)) paste(rna_detail, collapse = "") else "  (no RNA-seq datasets)\n",
      raw_line,
      "Global quantile: off\n",
      "Merged with microarray: no\n",
      "\nOK RNA-seq run complete.\n",
      "  Genes:   ", format(n_rna_g, big.mark = ","), "\n",
      "  Samples: ", format(n_rna_s, big.mark = ","), "\n"
    )
    log_text <- paste0(
      "Parallel DE: two separate normalization runs (not one shared method).\n\n",
      log_text_micro, "\n",
      log_text_rna, "\n",
      hr, "\n",
      "Not merged. Symbol overlap (information only): ",
      format(length(common_genes), big.mark = ","), "\n",
      "Common DEGs are taken in Step 7.\n"
    )
  }

  list(
    combined_expr = combined_expr,
    combined_expr_before_global = combined_before_global,
    all_expr_norm_list = all_expr_norm,
    expr_micro = expr_micro,
    expr_rna = expr_rna,
    common_genes = common_genes,
    normalization_stats = normalization_stats_global,
    normalization_summary_table = summary_data,
    raw_counts_for_deseq2 = raw_counts_for_deseq2,
    raw_counts_metadata = raw_counts_metadata,
    unified_metadata = unified_metadata,
    log_text = log_text,
    log_text_micro = log_text_micro,
    log_text_rna = log_text_rna
  )
}
