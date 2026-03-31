## GExPipe normalization + common-genes helpers
##
## This file mirrors the main normalization pipeline currently implemented
## inside inst/shinyapp/server/server_normalize.R, but as reusable functions
## under R/. Existing Shiny code still uses the inline version; you can
## gradually switch server_normalize.R to call these helpers.

utils::globalVariables(c("."))

#' Normalize microarray and RNA-seq datasets and compute common genes
#'
#' This is a non-Shiny helper version of the Step 3 logic:
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
#' set.seed(1)
#' m1 <- matrix(abs(rnorm(120)), nrow = 20, ncol = 6)
#' m2 <- matrix(abs(rnorm(120)), nrow = 20, ncol = 6)
#' rownames(m1) <- rownames(m2) <- paste0("Gene", seq_len(20))
#' colnames(m1) <- paste0("D1_S", seq_len(6))
#' colnames(m2) <- paste0("D2_S", seq_len(6))
#' out <- gexp_normalize_and_intersect(
#'   micro_expr_list = list(D1 = m1, D2 = m2),
#'   rna_counts_list = list(),
#'   de_method = "limma"
#' )
#' dim(out$combined_expr)
#' @export
gexp_normalize_and_intersect <- function(
  micro_expr_list,
  rna_counts_list,
  micro_norm_method = "quantile",
  rnaseq_norm_method = "TMM",
  micro_cel_paths = NULL,
  platform_per_gse = NULL,
  de_method = "limma"
) {
  all_expr_norm <- list()
  normalization_stats <- list()
  log_text <- "Normalizing data...\n\n"

  # ---- Microarray normalization ----
  if (length(micro_expr_list) > 0) {
    log_text <- paste0(log_text, "Microarray normalization (method: ", micro_norm_method, "):\n")
    for (gse in names(micro_expr_list)) {
      use_rma <- identical(micro_norm_method, "rma") &&
        !is.null(micro_cel_paths) &&
        length(micro_cel_paths[[gse]]) > 0
      if (use_rma) {
        plat <- if (!is.null(platform_per_gse)) platform_per_gse[[gse]] else NULL
        probe_mat <- normalize_microarray_rma(micro_cel_paths[[gse]], plat, dataset_name = gse)
        if (!is.null(probe_mat) && nrow(probe_mat) > 0 && ncol(probe_mat) > 0) {
          # Map to gene symbols
          micro_eset <- NULL
          fdata <- if (!is.null(micro_eset)) Biobase::fData(micro_eset) else data.frame()
          gene_symbols <- suppressMessages(map_microarray_ids(probe_mat, fdata, micro_eset, gse_id = gse))
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
          all_expr_norm[[gse]] <- expr_norm
          normalization_stats[[gse]] <- norm_info
          log_text <- paste0(
            log_text, "  ", gse, ": RMA \u2192 ",
            format(nrow(expr_norm), big.mark = ","), " genes \u2713\n"
          )
        } else {
          expr_norm <- normalize_microarray(micro_expr_list[[gse]], dataset_name = gse, method = "quantile")
          norm_info <- attr(expr_norm, "normalization_info")
          all_expr_norm[[gse]] <- expr_norm
          normalization_stats[[gse]] <- norm_info
          log_text <- paste0(
            log_text, "  ", gse, ": RMA not available, used Quantile. ",
            format(norm_info$final_genes, big.mark = ","), " genes \u2713\n"
          )
        }
      } else {
        expr_norm <- normalize_microarray(micro_expr_list[[gse]], dataset_name = gse, method = "quantile")
        norm_info <- attr(expr_norm, "normalization_info")
        all_expr_norm[[gse]] <- expr_norm
        normalization_stats[[gse]] <- norm_info
        log_text <- paste0(
          log_text, "  ", gse, ": ",
          format(norm_info$initial_genes, big.mark = ","), " \u2192 ",
          format(norm_info$final_genes, big.mark = ","), " genes \u2713\n"
        )
      }
    }
  }

  # ---- RNA-seq normalization ----
  if (length(rna_counts_list) > 0) {
    log_text <- paste0(log_text, "\nRNA-seq normalization (method: ", rnaseq_norm_method, "):\n")
    for (gse in names(rna_counts_list)) {
      expr_norm <- normalize_rnaseq(rna_counts_list[[gse]], dataset_name = gse, method = rnaseq_norm_method)
      norm_info <- attr(expr_norm, "normalization_info")
      all_expr_norm[[gse]] <- expr_norm
      normalization_stats[[gse]] <- norm_info
      log_text <- paste0(
        log_text, "  ", gse, ": ",
        format(norm_info$initial_genes, big.mark = ","), " \u2192 ",
        format(norm_info$genes_after_filtering, big.mark = ","),
        " (removed ", format(norm_info$genes_removed, big.mark = ","), " low-expression) \u2713\n"
      )
    }
  }

  # ---- Common genes (intersection) ----
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

  log_text <- paste0(
    log_text,
    "\nAutomatic gene filtering (background process):\n",
    "  Filtering to common genes ensures consistent gene sets across datasets.\n",
    "  This is required for accurate batch correction and differential expression analysis.\n"
  )

  common_genes <- Reduce(intersect, gene_lists)
  for (i in seq_along(all_expr_norm)) {
    all_expr_norm[[i]] <- all_expr_norm[[i]][common_genes, , drop = FALSE]
  }

  final_count <- length(common_genes)
  log_text <- paste0(
    log_text,
    "  \u2713 Common genes identified: ",
    format(final_count, big.mark = ","), "\n"
  )

  normalization_stats_global <- list(
    initial_total = initial_total,
    after_filter_total = after_filter_total,
    rnaseq_removed = rnaseq_removed,
    final_count = final_count,
    filter_method = "intersection"
  )

  # ---- Combine and global quantile normalization ----
  combined_before_global <- do.call(cbind, all_expr_norm)
  combined_expr <- limma::normalizeBetweenArrays(combined_before_global, method = "quantile")

  # ---- Optional raw counts for count-based DE ----
  raw_counts_for_deseq2 <- NULL
  raw_counts_metadata <- NULL
  if (length(rna_counts_list) > 0 && de_method %in% c("deseq2", "edger", "limma_voom")) {
    raw_counts_list <- list()
    for (gse in names(rna_counts_list)) {
      raw_mat <- as.matrix(rna_counts_list[[gse]])
      common_in_raw <- intersect(common_genes, rownames(raw_mat))
      if (length(common_in_raw) > 0) {
        raw_counts_list[[gse]] <- raw_mat[common_in_raw, , drop = FALSE]
      }
    }
    if (length(raw_counts_list) > 0) {
      raw_counts_for_deseq2 <- do.call(cbind, raw_counts_list)
      raw_counts_for_deseq2 <- round(raw_counts_for_deseq2)
      storage.mode(raw_counts_for_deseq2) <- "integer"
      log_text <- paste0(
        log_text,
        "  \u2713 Raw counts saved for DESeq2/edgeR/voom: ",
        format(nrow(raw_counts_for_deseq2), big.mark = ","), " genes \u00d7 ",
        format(ncol(raw_counts_for_deseq2), big.mark = ","), " samples\n"
      )
    }
  }

  # ---- Unified metadata (platform + dataset per sample) ----
  micro_n <- if (length(micro_expr_list) > 0) {
    sum(vapply(micro_expr_list, ncol, integer(1)))
  } else {
    0L
  }
  rna_n <- if (length(rna_counts_list) > 0) {
    sum(vapply(rna_counts_list, ncol, integer(1)))
  } else {
    0L
  }
  platform_labels <- c(rep("Microarray", micro_n), rep("RNAseq", rna_n))
  dataset_labels <- rep(names(all_expr_norm), times = vapply(all_expr_norm, ncol, integer(1)))

  unified_metadata <- data.frame(
    SampleID = colnames(combined_expr),
    Platform = platform_labels,
    Dataset = dataset_labels,
    Condition = NA_character_,
    row.names = colnames(combined_expr),
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
    Final_Common_Genes = final_count,
    stringsAsFactors = FALSE
  )

  summary_data <- rbind(
    summary_data,
    data.frame(
      Dataset = "TOTAL/COMMON",
      Initial_Genes = sum(summary_data$Initial_Genes),
      After_Filtering = sum(summary_data$After_Filtering),
      Final_Common_Genes = final_count,
      stringsAsFactors = FALSE
    )
  )

  list(
    combined_expr = combined_expr,
    combined_expr_before_global = combined_before_global,
    all_expr_norm_list = all_expr_norm,
    common_genes = common_genes,
    normalization_stats = normalization_stats_global,
    normalization_summary_table = summary_data,
    raw_counts_for_deseq2 = raw_counts_for_deseq2,
    raw_counts_metadata = raw_counts_metadata,
    unified_metadata = unified_metadata,
    log_text = log_text
  )
}
