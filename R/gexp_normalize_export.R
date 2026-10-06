# ==============================================================================
# GEXP_NORMALIZE_EXPORT.R - Before / after normalization export (Step 2)
# ==============================================================================
# Writes the expression matrices as they were BEFORE normalization (Step 1
# input) and AFTER normalization (per dataset, per platform and combined),
# plus a per-sample and per-dataset statistics table, so the user can check
# outside the app that normalization did what it should. Works for every
# analysis type: RNA-seq only, microarray only, Merged and Parallel.

#' Coerce an expression object to a numeric matrix (NULL if not possible)
#' @keywords internal
.gexpipe_norm_export_matrix <- function(x) {
  if (is.null(x)) return(NULL)
  m <- tryCatch(as.matrix(x), error = function(e) NULL)
  if (is.null(m) || length(dim(m)) != 2L || nrow(m) == 0L || ncol(m) == 0L) return(NULL)
  if (!is.numeric(m)) {
    rn <- rownames(m)
    cn <- colnames(m)
    m <- suppressWarnings(matrix(as.numeric(m), nrow = nrow(m), dimnames = list(rn, cn)))
  }
  m
}

#' Per-sample distribution statistics of an expression matrix
#'
#' @param mat numeric matrix (genes x samples).
#' @param dataset dataset label (e.g. a GSE ID).
#' @param platform "Microarray", "RNA-seq" or "Combined".
#' @param stage "Before" or "After".
#' @return data.frame with one row per sample.
#' @keywords internal
gexp_norm_sample_stats <- function(mat, dataset, platform, stage) {
  m <- .gexpipe_norm_export_matrix(mat)
  if (is.null(m)) return(NULL)
  q <- apply(m, 2L, function(v) {
    v <- v[is.finite(v)]
    if (length(v) == 0L) return(rep(NA_real_, 7L))
    c(stats::quantile(v, c(0, 0.25, 0.5, 0.75, 1), names = FALSE), mean(v), stats::sd(v))
  })
  q <- matrix(q, nrow = 7L)
  sid <- colnames(m)
  if (is.null(sid)) sid <- paste0("Sample", seq_len(ncol(m)))
  data.frame(
    Stage = stage, Dataset = dataset, Platform = platform, Sample = sid,
    Genes = nrow(m),
    Missing = colSums(!is.finite(m)),
    Min = q[1L, ], Q1 = q[2L, ], Median = q[3L, ], Mean = q[6L, ],
    Q3 = q[4L, ], Max = q[5L, ], SD = q[7L, ],
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#' One-row-per-dataset normalization check from per-sample statistics
#'
#' "Median_SD_across_samples" near 0 means the samples were aligned (e.g.
#' quantile normalization); "Likely_log_scale" flags whether values look
#' log-transformed (max <= 50), which limma expects.
#' @keywords internal
gexp_norm_dataset_check <- function(sample_stats) {
  if (is.null(sample_stats) || nrow(sample_stats) == 0L) return(NULL)
  key <- paste(sample_stats$Stage, sample_stats$Dataset, sample_stats$Platform, sep = "\r")
  rows <- lapply(split(sample_stats, factor(key, levels = unique(key))), function(d) {
    data.frame(
      Stage = d$Stage[1L], Dataset = d$Dataset[1L], Platform = d$Platform[1L],
      Genes = d$Genes[1L], Samples = nrow(d),
      Missing_values = sum(d$Missing),
      Min = suppressWarnings(min(d$Min, na.rm = TRUE)),
      Max = suppressWarnings(max(d$Max, na.rm = TRUE)),
      Median_of_sample_medians = stats::median(d$Median, na.rm = TRUE),
      Median_SD_across_samples = if (nrow(d) > 1L) stats::sd(d$Median, na.rm = TRUE) else NA_real_,
      Median_range_across_samples = diff(suppressWarnings(range(d$Median, na.rm = TRUE))),
      IQR_SD_across_samples = if (nrow(d) > 1L) stats::sd(d$Q3 - d$Q1, na.rm = TRUE) else NA_real_,
      Likely_log_scale = isTRUE(suppressWarnings(max(d$Max, na.rm = TRUE)) <= 50),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Collect the before / after matrices to export
#'
#' @param micro_expr_list,rna_counts_list Step 1 input lists (before).
#' @param all_expr_norm_list per-dataset normalized matrices (after).
#' @param expr_micro,expr_rna per-platform normalized matrices (after).
#' @param combined_expr_before_global,combined_expr combined matrices
#'   (Merged / single-platform; before and after the global quantile step).
#' @param raw_counts_for_deseq2 RNA-seq counts kept for count-based DE.
#' @param keep_separate TRUE for Parallel (per-platform matrices are the
#'   analysis matrices; the combined one is storage only).
#' @return list of entries, each list(path, mat, dataset, platform, stage).
#' @keywords internal
gexp_norm_export_entries <- function(micro_expr_list = NULL, rna_counts_list = NULL,
                                     all_expr_norm_list = NULL, expr_micro = NULL, expr_rna = NULL,
                                     combined_expr_before_global = NULL, combined_expr = NULL,
                                     raw_counts_for_deseq2 = NULL, keep_separate = FALSE) {
  safe <- function(x) gsub("[^A-Za-z0-9._-]", "_", x)
  ent <- list()
  add <- function(path, mat, dataset, platform, stage, in_stats = TRUE) {
    m <- .gexpipe_norm_export_matrix(mat)
    if (!is.null(m)) {
      ent[[length(ent) + 1L]] <<- list(path = path, mat = m, dataset = dataset,
                                       platform = platform, stage = stage, in_stats = in_stats)
    }
  }
  plat_of <- function(nm) {
    if (nm %in% names(rna_counts_list)) "RNA-seq" else if (nm %in% names(micro_expr_list)) "Microarray" else "Unknown"
  }
  for (nm in names(micro_expr_list)) {
    add(file.path("1_before_normalization", paste0(safe(nm), "_Microarray_before.csv")),
        micro_expr_list[[nm]], nm, "Microarray", "Before")
  }
  for (nm in names(rna_counts_list)) {
    add(file.path("1_before_normalization", paste0(safe(nm), "_RNAseq_before.csv")),
        rna_counts_list[[nm]], nm, "RNA-seq", "Before")
  }
  for (nm in names(all_expr_norm_list)) {
    p <- plat_of(nm)
    add(file.path("2_after_normalization", "per_dataset",
                  paste0(safe(nm), "_", gsub("-", "", p), "_after.csv")),
        all_expr_norm_list[[nm]], nm, p, "After")
  }
  if (isTRUE(keep_separate)) {
    add(file.path("2_after_normalization", "Microarray_all_datasets_after.csv"),
        expr_micro, "All microarray", "Microarray", "After")
    add(file.path("2_after_normalization", "RNAseq_all_datasets_after.csv"),
        expr_rna, "All RNA-seq", "RNA-seq", "After")
  } else {
    add(file.path("2_after_normalization", "Combined_before_global_quantile.csv"),
        combined_expr_before_global, "Combined (before global quantile)", "Combined", "After")
    add(file.path("2_after_normalization", "Combined_final_normalized.csv"),
        combined_expr, "Combined (final)", "Combined", "After")
  }
  add(file.path("2_after_normalization", "RNAseq_raw_counts_used_for_DE.csv"),
      raw_counts_for_deseq2, "RNA-seq raw counts for DE", "RNA-seq", "After", in_stats = FALSE)
  ent
}

#' Write the before / after normalization export into a folder
#'
#' @param out_dir destination folder (created if needed).
#' @param entries output of \code{gexp_norm_export_entries()}.
#' @param analysis_type analysis type label for the README.
#' @param methods named character vector of method labels for the README.
#' @param include_matrices FALSE writes only the statistics tables.
#' @return Character vector of written file paths (relative to out_dir).
#' @keywords internal
gexp_write_norm_before_after <- function(out_dir, entries, analysis_type = "",
                                         methods = character(0), include_matrices = TRUE) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  written <- character(0)
  stats_list <- list()
  for (e in entries) {
    if (isTRUE(include_matrices)) {
      f <- file.path(out_dir, e$path)
      dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
      dt <- data.table::as.data.table(e$mat, keep.rownames = "Gene")
      data.table::fwrite(dt, f)
      written <- c(written, e$path)
    }
    if (isTRUE(e$in_stats)) {
      stats_list[[length(stats_list) + 1L]] <- gexp_norm_sample_stats(e$mat, e$dataset, e$platform, e$stage)
    }
  }
  sample_stats <- do.call(rbind, stats_list)
  if (!is.null(sample_stats) && nrow(sample_stats) > 0L) {
    utils::write.csv(gexp_norm_dataset_check(sample_stats),
                     file.path(out_dir, "Normalization_check_per_dataset.csv"), row.names = FALSE)
    utils::write.csv(sample_stats,
                     file.path(out_dir, "Normalization_check_per_sample.csv"), row.names = FALSE)
    written <- c(written, "Normalization_check_per_dataset.csv", "Normalization_check_per_sample.csv")
  }
  meth <- if (length(methods)) paste0("  ", names(methods), ": ", methods, collapse = "\n") else "  (not recorded)"
  readme <- c(
    "GExPipe - normalization before / after export",
    paste0("Created: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Analysis type: ", analysis_type),
    "Methods:", meth,
    "",
    "1_before_normalization/  Step 1 input matrices, exactly as downloaded / uploaded (genes x samples).",
    "2_after_normalization/   Normalized matrices: per dataset, and per platform (Parallel) or combined (Merged / single platform).",
    "  Merged: per-dataset files hold common genes only; Combined_before_global_quantile is before the",
    "  global quantile step, Combined_final_normalized is the matrix used downstream.",
    "  RNAseq_raw_counts_used_for_DE: raw counts used by DESeq2 / edgeR / limma-voom (they normalize internally).",
    "  Affymetrix CEL + RMA: the 'before' file is the series matrix; RMA itself is computed from the CEL files.",
    "",
    "How to check normalization (Normalization_check_per_dataset.csv):",
    "  - Likely_log_scale should be TRUE after normalization for limma (values roughly 0-20).",
    "  - Median_SD_across_samples / Median_range_across_samples should shrink from Before to After;",
    "    after quantile normalization they are ~0 (every sample has the same distribution).",
    "  - IQR_SD_across_samples should also shrink (sample spreads aligned).",
    "  - Missing_values should not increase.",
    "Normalization_check_per_sample.csv has min / quartiles / median / mean / max / SD for every sample."
  )
  writeLines(readme, file.path(out_dir, "README.txt"))
  c(written, "README.txt")
}

#' Bundle a folder into an archive; zip when a zip tool exists, else tar.gz
#' @keywords internal
gexp_norm_archive_ext <- function() {
  if (nzchar(Sys.getenv("R_ZIPCMD")) || nzchar(Sys.which("zip"))) "zip" else "tar.gz"
}

#' @keywords internal
gexp_norm_archive_dir <- function(src_dir, dest_file) {
  # zip.exe (Rtools / msys) mis-parses backslash Windows temp paths and fails
  # with "Temporary file failure"; give it an absolute forward-slash path.
  dest_file <- normalizePath(dest_file, winslash = "/", mustWork = FALSE)
  old <- setwd(src_dir)
  on.exit(setwd(old), add = TRUE)
  files <- list.files(".", recursive = TRUE)
  if (identical(gexp_norm_archive_ext(), "zip")) {
    utils::zip(dest_file, files, flags = "-r9Xq")
  } else {
    utils::tar(dest_file, files, compression = "gzip", tar = "internal")
  }
  invisible(dest_file)
}
