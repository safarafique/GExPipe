## GExPipe download-step helpers
##
## Small reusable helpers extracted from Shiny download module.

#' @importFrom utils capture.output
NULL

#' Split a comma/whitespace GSE text field into IDs
#' @keywords internal
.gexpipe_split_gse_ids <- function(text) {
 if (is.null(text) || length(text) == 0L || all(is.na(text))) {
 return(character(0))
 }
 text <- paste(as.character(text), collapse = ",")
 ids <- trimws(unlist(strsplit(gsub("\\s+", ",", text), ",", fixed = FALSE)))
 ids[nzchar(ids)]
}

#' Parse GSE IDs from Shiny text inputs
#'
#' RNA-seq and microarray boxes are always read. If both contain GSE IDs and
#' the user did not choose Parallel, the run is treated as Merged. Parallel
#' keeps both platforms separate through DE and accepts several GSEs per box.
#'
#' @param analysis_type One of "rnaseq", "microarray", "merged", or "parallel".
#' @param rnaseq_gses Text field for RNA-seq IDs.
#' @param microarray_gses Text field for microarray IDs.
#' @param dataset_mode "single" or "multi". Single truncates only for
#'   RNA-seq-only or microarray-only. Merged and Parallel keep every GSE
#'   in each platform box.
#' @return List with rnaseq_ids, micro_ids, dataset_mode, analysis_type, and messages.
#'
#' @examples
#' x <- gexp_parse_gse_inputs(
#'   analysis_type = "parallel",
#'   rnaseq_gses = "GSE1, GSE2",
#'   microarray_gses = "GSE3, GSE4",
#'   dataset_mode = "single"
#' )
#' x$rnaseq_ids
#' @export
gexp_parse_gse_inputs <- function(
  analysis_type,
  rnaseq_gses = "",
  microarray_gses = "",
  dataset_mode = "multi"
) {
  messages <- character(0)
  rnaseq_ids <- unique(.gexpipe_split_gse_ids(rnaseq_gses))
  micro_ids <- unique(.gexpipe_split_gse_ids(microarray_gses))

  requested <- if (is.null(analysis_type) || !nzchar(as.character(analysis_type)[[1L]])) {
    "merged"
  } else {
    as.character(analysis_type)[[1L]]
  }

  if (identical(requested, "parallel")) {
    inferred <- "parallel"
  } else if (length(rnaseq_ids) > 0L && length(micro_ids) > 0L) {
    inferred <- "merged"
    if (!identical(requested, "merged")) {
      messages <- c(
        messages,
        "Both RNA-seq and microarray GSE IDs were entered; running merged analysis."
      )
    }
  } else if (length(micro_ids) > 0L) {
    inferred <- "microarray"
  } else if (length(rnaseq_ids) > 0L) {
    inferred <- "rnaseq"
  } else {
    inferred <- requested
  }

  mode <- if (is.null(dataset_mode) || !nzchar(dataset_mode)) "multi" else dataset_mode
  two_platform <- inferred %in% c("merged", "parallel")
  if (isTRUE(two_platform)) {
    if (length(rnaseq_ids) + length(micro_ids) > 1L) {
      mode <- "multi"
    }
  } else if (identical(mode, "single")) {
    if (length(rnaseq_ids) > 1) {
      messages <- c(messages, "Single dataset mode: using only the first RNA-seq GSE ID.")
      rnaseq_ids <- rnaseq_ids[1]
    }
    if (length(micro_ids) > 1) {
      messages <- c(messages, "Single dataset mode: using only the first microarray GSE ID.")
      micro_ids <- micro_ids[1]
    }
  }

  list(
    rnaseq_ids = rnaseq_ids,
    micro_ids = micro_ids,
    dataset_mode = mode,
    analysis_type = inferred,
    messages = messages
  )
}

#' Prepare clean download directories for current run
#'
#' @param base_dir Base working directory.
#' @param has_micro Logical; whether microarray IDs exist.
#' @param has_rna Logical; whether RNA-seq IDs exist.
#' @param clear_cache If `TRUE`, delete existing `micro_data` / `rna_data`
#'   before download. Default follows `options(gexpipe.clear_download_cache)`
#'   (FALSE) so GEOquery can reuse cached matrix files between runs.
#' @return Character vector log lines describing cleanup.
#'
#' @examples
#' td <- tempdir()
#' gexp_prepare_download_dirs(td, has_micro = TRUE, has_rna = TRUE)
#' @export
gexp_prepare_download_dirs <- function(
 base_dir = getwd(),
 has_micro = FALSE,
 has_rna = FALSE,
 clear_cache = NULL
) {
 if (is.null(clear_cache)) {
 clear_cache <- isTRUE(getOption("gexpipe.clear_download_cache", FALSE))
 }
 logs <- character(0)
 if (isTRUE(has_micro)) {
 micro_dir <- file.path(base_dir, "micro_data")
 if (isTRUE(clear_cache) && dir.exists(micro_dir)) {
 gpl_backup <- list.files(
 micro_dir, pattern = "^GPL[0-9]+\\.soft(\\.gz)?$",
 full.names = TRUE
 )
 tryCatch(unlink(micro_dir, recursive = TRUE, force = TRUE), error = function(e) NULL)
 logs <- c(logs, "Cleared previous microarray cache (micro_data).")
 dir.create(micro_dir, showWarnings = FALSE, recursive = TRUE)
 if (length(gpl_backup) > 0L) {
 tryCatch(file.copy(gpl_backup, micro_dir, overwrite = TRUE), error = function(e) NULL)
 }
 .gexpipe_seed_gpl_cache(micro_dir)
 } else {
 dir.create(micro_dir, showWarnings = FALSE, recursive = TRUE)
 if (length(list.files(micro_dir, all.files = FALSE)) == 0L) {
 .gexpipe_seed_gpl_cache(micro_dir)
 }
 }
 }
 if (isTRUE(has_rna)) {
 rna_dir <- file.path(base_dir, "rna_data")
 if (isTRUE(clear_cache) && dir.exists(rna_dir)) {
 tryCatch(unlink(rna_dir, recursive = TRUE, force = TRUE), error = function(e) NULL)
 logs <- c(logs, "Cleared previous RNA-seq cache (rna_data).")
 }
 dir.create(rna_dir, showWarnings = FALSE, recursive = TRUE)
 }
 logs
}

#' Detect fread-style generic column names (V1, V2, X1, ...)
#'
#' @param nms Character vector of sample/column names.
#' @return Logical scalar.
#' @keywords internal
gexp_is_generic_sample_names <- function(nms) {
 if (length(nms) == 0L) {
 return(FALSE)
 }
 mean(grepl("^(V|X)[0-9]+$", nms, ignore.case = TRUE)) >= 0.5
}

#' Orient an RNA-seq count table to a genes x samples matrix
#'
#' Some GEO supplementary files store samples as rows and genes as columns.
#' This helper transposes when dimensions and optional metadata suggest that layout.
#'
#' @param count_df data.frame read from a count file.
#' @param metadata Optional GEO pData used to hint expected sample count.
#' @return List with `matrix` (genes x samples) and `log` (character).
#' @keywords internal
gexp_orient_count_dataframe <- function(count_df, metadata = NULL) {
 if (is.null(count_df) || ncol(count_df) < 2L || nrow(count_df) < 2L) {
 return(list(matrix = NULL, log = "invalid count table"))
 }

 n_meta <- if (!is.null(metadata) && nrow(metadata) > 0L) nrow(metadata) else NA_integer_
 likely_transposed <- (nrow(count_df) <= 200L && ncol(count_df) >= 500L) ||
 (!is.na(n_meta) && nrow(count_df) == n_meta && ncol(count_df) > nrow(count_df) * 3L)

 if (isTRUE(likely_transposed)) {
 sample_ids <- as.character(count_df[[1]])
 gene_mat <- as.matrix(count_df[, -1, drop = FALSE])
 mode(gene_mat) <- "numeric"
 rownames(gene_mat) <- sample_ids
 count_matrix <- t(gene_mat)
 return(list(matrix = count_matrix, log = "transposed count table (samples were rows)"))
 }

 gene_ids <- as.character(count_df[[1]])
 count_matrix <- as.matrix(count_df[, -1, drop = FALSE])
 mode(count_matrix) <- "numeric"
 rownames(count_matrix) <- gene_ids
 list(matrix = count_matrix, log = "")
}

#' Align RNA-seq count-matrix column names with GEO sample metadata
#'
#' GExPipe users enter **GSE accessions** in the app; this helper runs
#' automatically during GEO download when a count file has no sample headers
#' (e.g. `data.table::fread` assigns `V2`, `V3`, ...). Columns are renamed
#' to GSM IDs from GEO pData so QC, normalization, and group selection see
#' individual samples (fixes issues such as a single **V2** bar in outlier QC).
#'
#' @param count_matrix Numeric matrix (genes x samples).
#' @param metadata Optional GEO pData with sample IDs as row names.
#' @param gse_id GEO series accession (used for fallback naming).
#' @return Matrix with improved column names.
#' @examples
#' # GSE137136-style: headerless columns after download, GSM IDs in metadata.
#' mat <- matrix(1:4, nrow = 2, ncol = 2,
#' dimnames = list(c("GENE1", "GENE2"), c("V2", "V3")))
#' meta <- data.frame(title = c("sample 1", "sample 2"),
#' row.names = c("GSM111", "GSM222"))
#' out <- gexp_align_rnaseq_sample_names(mat, meta, "GSE137136")
#' colnames(out)
#' @export
gexp_align_rnaseq_sample_names <- function(count_matrix, metadata = NULL, gse_id = NULL) {
 if (is.null(count_matrix) || ncol(count_matrix) < 1L) {
 return(count_matrix)
 }

 nms <- colnames(count_matrix)
 meta_ids <- character(0)
 if (!is.null(metadata) && nrow(metadata) > 0L && !is.null(rownames(metadata))) {
 meta_ids <- rownames(metadata)
 }

 direct <- 0L
 if (length(meta_ids) > 0L) {
 direct <- sum(nms %in% meta_ids)
 if (direct >= max(2L, floor(0.5 * ncol(count_matrix)))) {
 return(count_matrix)
 }
 }

 need_rename <- gexp_is_generic_sample_names(nms) ||
 (length(meta_ids) > 0L && direct < max(2L, floor(0.2 * ncol(count_matrix))))

 if (isTRUE(need_rename) && length(meta_ids) > 0L) {
 n <- min(ncol(count_matrix), length(meta_ids))
 new_nms <- meta_ids[seq_len(n)]
 if (ncol(count_matrix) > n) {
 extra <- seq_len(ncol(count_matrix) - n) + n
 prefix <- if (!is.null(gse_id) && nzchar(gse_id)) gse_id else "Sample"
 new_nms <- c(new_nms, paste0(prefix, "_", extra))
 }
 colnames(count_matrix) <- new_nms
 } else if (gexp_is_generic_sample_names(nms) && !is.null(gse_id) && nzchar(gse_id)) {
 colnames(count_matrix) <- paste0(gse_id, "_", seq_len(ncol(count_matrix)))
 }

 if (any(duplicated(colnames(count_matrix)))) {
 colnames(count_matrix) <- make.unique(colnames(count_matrix), sep = "_")
 }

 count_matrix
}

#' Prefix duplicate sample column names across multiple datasets
#'
#' @param expr_lists Named list of expression/count matrices.
#' @return Updated list with unique column names where needed.
#' @keywords internal
gexp_ensure_unique_colnames_across_datasets <- function(expr_lists) {
 if (length(expr_lists) < 2L) {
 return(expr_lists)
 }
 all_cols <- unlist(lapply(expr_lists, colnames), use.names = FALSE)
 dup <- unique(all_cols[duplicated(all_cols)])
 if (length(dup) == 0L) {
 return(expr_lists)
 }
 for (nm in names(expr_lists)) {
 cn <- colnames(expr_lists[[nm]])
 hit <- cn %in% dup
 if (any(hit)) {
 colnames(expr_lists[[nm]])[hit] <- paste0(nm, "_", cn[hit])
 }
 }
 expr_lists
}

#' Coerce an expression/count object to a numeric genes x samples matrix
#' @keywords internal
.gexpipe_as_expr_matrix <- function(m) {
 if (is.null(m)) {
 return(NULL)
 }
 if (is.matrix(m)) {
 if (nrow(m) == 0L || ncol(m) == 0L) {
 return(NULL)
 }
 return(m)
 }
 if (is.data.frame(m) && nrow(m) > 0L && ncol(m) > 0L) {
 rn <- rownames(m)
 cn <- colnames(m)
 out <- tryCatch(data.matrix(m), error = function(e) NULL)
 if (is.null(out) || nrow(out) == 0L || ncol(out) == 0L) {
 return(NULL)
 }
 if (is.null(rownames(out)) && !is.null(rn)) {
 rownames(out) <- rn
 }
 if (is.null(colnames(out)) && !is.null(cn)) {
 colnames(out) <- cn
 }
 return(out)
 }
 NULL
}

#' Bind per-dataset matrices on a shared gene set without mutating inputs
#'
#' Uses \code{match()} so duplicate gene symbols cannot change the row count
#' of one study relative to another (naive \code{cbind} then errors with
#' "number of rows of matrices must match").
#'
#' @param expr_lists Named list of genes x samples matrices.
#' @param common_genes Character vector of row names to keep (same order).
#' @return Combined matrix, or NULL when nothing can be bound.
#' @keywords internal
.gexpipe_cbind_common_genes <- function(expr_lists, common_genes) {
 common_genes <- unique(as.character(common_genes))
 common_genes <- common_genes[!is.na(common_genes) & nzchar(trimws(common_genes))]
 expr_lists <- lapply(expr_lists, .gexpipe_as_expr_matrix)
 expr_lists <- Filter(function(m) is.matrix(m) && nrow(m) > 0L && ncol(m) > 0L, expr_lists)
 if (length(expr_lists) == 0L || length(common_genes) == 0L) {
 return(NULL)
 }
 aligned <- lapply(expr_lists, function(m) {
 idx <- match(common_genes, rownames(m))
 if (anyNA(idx)) {
 return(NULL)
 }
 out <- m[idx, , drop = FALSE]
 rownames(out) <- common_genes
 out
 })
 aligned <- Filter(Negate(is.null), aligned)
 if (length(aligned) != length(expr_lists) || length(aligned) == 0L) {
 return(NULL)
 }
 n_rows <- vapply(aligned, nrow, integer(1))
 if (length(unique(n_rows)) != 1L) {
 return(NULL)
 }
 combined <- tryCatch(do.call(cbind, aligned), error = function(e) NULL)
 if (is.null(combined)) {
 return(NULL)
 }
 rownames(combined) <- common_genes
 combined
}

#' Finalize common genes and combined matrix after download/mapping
#'
#' Per-dataset matrices keep their full gene sets so Step 3 can normalize
#' **within each study** before merging. `combined_expr_raw` is an overlap
#' view for QC plots only.
#'
#' @param micro_expr_list Named list of microarray matrices (genes x samples).
#' @param rna_counts_list Named list of RNA-seq matrices (genes x samples).
#' @param all_genes_list Named list of rowname vectors per dataset.
#' @param subset_matrices If `TRUE`, also subset stored lists to common genes
#'   (legacy merge-first behaviour). Default `FALSE`.
#' @param keep_platforms_separate If `TRUE` (Parallel DE), do not subset each
#'   platform to the intersecting gene set. Overlap is still reported for QC.
#' @return List with updated lists, common_genes, combined_expr_raw, and status.
#'
#' @examples
#' m1 <- matrix(1:12, nrow = 3, dimnames = list(c("A", "B", "C"), paste0("S", 1:4)))
#' m2 <- matrix(1:12, nrow = 3, dimnames = list(c("B", "C", "D"), paste0("T", 1:4)))
#' out <- gexp_download_finalize_common_genes(
#' micro_expr_list = list(D1 = m1),
#' rna_counts_list = list(D2 = m2),
#' all_genes_list = list(D1 = rownames(m1), D2 = rownames(m2))
#' )
#' out$common_genes
#' @export
gexp_download_finalize_common_genes <- function(
 micro_expr_list,
 rna_counts_list,
 all_genes_list,
 subset_matrices = FALSE,
 keep_platforms_separate = FALSE
) {
 empty <- function(ok = FALSE, common = character(0), combined = NULL) {
 list(
 ok = ok,
 common_genes = common,
 micro_expr_list = micro_expr_list,
 rna_counts_list = rna_counts_list,
 combined_expr_raw = combined
 )
 }
 if (isTRUE(keep_platforms_separate)) {
 subset_matrices <- FALSE
 }
 n_datasets <- length(micro_expr_list) + length(rna_counts_list)
 if (length(all_genes_list) == 0 && n_datasets == 0L) {
 return(empty(ok = FALSE))
 }

 if (n_datasets > 1L) {
 micro_expr_list <- gexp_ensure_unique_colnames_across_datasets(micro_expr_list)
 rna_counts_list <- gexp_ensure_unique_colnames_across_datasets(rna_counts_list)
 }

 rebuilt <- tryCatch(
 gexp_rebuild_all_genes_list(micro_expr_list, rna_counts_list),
 error = function(e) list()
 )
 if (length(rebuilt) > 0L) {
 all_genes_list <- rebuilt
 }
 if (length(all_genes_list) == 0) {
 return(empty(ok = isTRUE(keep_platforms_separate) && n_datasets > 0L))
 }

 common_genes <- Reduce(intersect, all_genes_list)
 common_genes <- unique(as.character(common_genes))
 common_genes <- common_genes[!is.na(common_genes) & nzchar(trimws(common_genes))]
 if (length(common_genes) == 0L) {
 combined_empty <- if (isTRUE(keep_platforms_separate)) {
 matrix(numeric(0), nrow = 0L, ncol = 0L)
 } else {
 NULL
 }
 return(empty(
 ok = isTRUE(keep_platforms_separate) && n_datasets > 0L,
 common = character(0),
 combined = combined_empty
 ))
 }

 .align_list <- function(expr_list, genes) {
 lapply(expr_list, function(m) {
 idx <- match(genes, rownames(m))
 if (anyNA(idx)) {
 return(m)
 }
 out <- m[idx, , drop = FALSE]
 rownames(out) <- genes
 out
 })
 }

 if (isTRUE(subset_matrices)) {
 micro_expr_list <- .align_list(micro_expr_list, common_genes)
 rna_counts_list <- .align_list(rna_counts_list, common_genes)
 }

 combined_expr_raw <- tryCatch(
 .gexpipe_cbind_common_genes(
 c(micro_expr_list, rna_counts_list),
 common_genes
 ),
 error = function(e) NULL
 )
 if (isTRUE(keep_platforms_separate)) {
 if (is.null(combined_expr_raw)) {
 combined_expr_raw <- matrix(numeric(0), nrow = 0L, ncol = 0L)
 }
 return(empty(ok = TRUE, common = common_genes, combined = combined_expr_raw))
 }
 if (is.null(combined_expr_raw)) {
 return(empty(ok = FALSE, common = character(0)))
 }

 list(
 ok = TRUE,
 common_genes = common_genes,
 micro_expr_list = micro_expr_list,
 rna_counts_list = rna_counts_list,
 combined_expr_raw = combined_expr_raw
 )
}

#' GEO series FTP folder name (e.g. GSE50760 -> GSE50nnn)
#' @keywords internal
.gexpipe_geo_series_folder <- function(gse_id) {
 gse_id <- toupper(trimws(as.character(gse_id)[[1L]]))
 if (!nzchar(gse_id) || nchar(gse_id) < 4L) {
 return(gse_id)
 }
 paste0(substr(gse_id, 1L, nchar(gse_id) - 3L), "nnn")
}

#' Strip GEO series-matrix quoting from sample attribute values
#' @keywords internal
.gexpipe_strip_geo_quotes <- function(x) {
 x <- as.character(x)
 x <- gsub("^\"|\"$", "", x)
 x
}

#' Test whether a GEO object is SummarizedExperiment-like
#' @keywords internal
.gexpipe_is_summarized_experiment <- function(obj) {
 inherits(obj, "SummarizedExperiment") || inherits(obj, "RangedSummarizedExperiment")
}

#' Expression matrix from ExpressionSet or SummarizedExperiment
#' @keywords internal
.gexpipe_geo_expr_matrix <- function(obj) {
  if (is.null(obj)) {
    return(NULL)
  }
  if (inherits(obj, "ExpressionSet")) {
    return(tryCatch(Biobase::exprs(obj), error = function(e) NULL))
  }
  if (.gexpipe_is_summarized_experiment(obj)) {
    if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
      warning(
        "GEO returned SummarizedExperiment but package 'SummarizedExperiment' is not installed. ",
        "Run BiocManager::install('SummarizedExperiment').",
        call. = FALSE
      )
      return(NULL)
    }
    mat <- tryCatch(SummarizedExperiment::assay(obj), error = function(e) NULL)
    if (!is.null(mat)) {
      return(as.matrix(mat))
    }
  }
  tryCatch(Biobase::exprs(obj), error = function(e) NULL)
}

#' Sample metadata from ExpressionSet or SummarizedExperiment
#' @keywords internal
.gexpipe_geo_pdata <- function(obj) {
 if (is.null(obj)) {
 return(NULL)
 }
 pd <- NULL
 if (inherits(obj, "ExpressionSet")) {
 pd <- tryCatch(Biobase::pData(obj), error = function(e) NULL)
 } else if (.gexpipe_is_summarized_experiment(obj) && requireNamespace("SummarizedExperiment", quietly = TRUE)) {
 pd <- tryCatch(as.data.frame(SummarizedExperiment::colData(obj)), error = function(e) NULL)
 }
 if (is.null(pd)) {
 pd <- tryCatch(Biobase::pData(obj), error = function(e) NULL)
 }
 if (is.null(pd) && requireNamespace("SummarizedExperiment", quietly = TRUE)) {
 pd <- tryCatch(as.data.frame(SummarizedExperiment::colData(obj)), error = function(e) NULL)
 }
 if (is.null(pd) || nrow(pd) == 0L) {
 return(NULL)
 }
 as.data.frame(pd, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Feature metadata from ExpressionSet or SummarizedExperiment
#' @keywords internal
.gexpipe_geo_fdata <- function(obj) {
 if (is.null(obj)) {
 return(NULL)
 }
 fd <- NULL
 if (inherits(obj, "ExpressionSet")) {
 fd <- tryCatch(Biobase::fData(obj), error = function(e) NULL)
 } else if (.gexpipe_is_summarized_experiment(obj) && requireNamespace("SummarizedExperiment", quietly = TRUE)) {
 fd <- tryCatch(as.data.frame(SummarizedExperiment::rowData(obj)), error = function(e) NULL)
 }
 if (is.null(fd)) {
 fd <- tryCatch(Biobase::fData(obj), error = function(e) NULL)
 }
 if (is.null(fd) || nrow(fd) == 0L) {
 return(NULL)
 }
 as.data.frame(fd, stringsAsFactors = FALSE, check.names = FALSE)
}

#' First non-empty scalar from a possibly zero-/multi-length annotation() result
#' @keywords internal
.gexpipe_first_nonempty_scalar <- function(x) {
 if (is.null(x) || length(x) == 0L) return(NULL)
 x <- as.character(x)
 x <- x[!is.na(x) & nzchar(x)]
 if (length(x) == 0L) return(NULL)
 x[[1L]]
}

#' Platform annotation from ExpressionSet or SummarizedExperiment
#'
#' Tries \code{annotation()} first, then falls back to a \code{platform_id}
#' column in phenodata/colData (always present for GEO series-matrix objects,
#' since GEOquery derives \code{annotation()} from the same \code{!Sample_platform_id}
#' header field) - this keeps GPL lookup working even when \code{annotation()}
#' comes back empty/zero-length for a given object.
#' @keywords internal
.gexpipe_geo_annotation <- function(obj) {
 if (is.null(obj)) {
 return("")
 }
 ann <- tryCatch(Biobase::annotation(obj), error = function(e) NULL)
 got <- .gexpipe_first_nonempty_scalar(ann)
 if (!is.null(got)) return(got)

 if (.gexpipe_is_summarized_experiment(obj) && requireNamespace("BiocGenerics", quietly = TRUE)) {
 ann <- tryCatch(BiocGenerics::annotation(obj), error = function(e) NULL)
 got <- .gexpipe_first_nonempty_scalar(ann)
 if (!is.null(got)) return(got)
 }

 pd <- tryCatch(.gexpipe_geo_pdata(obj), error = function(e) NULL)
 if (!is.null(pd) && nrow(pd) > 0L) {
 plat_col <- grep("^platform_id$", colnames(pd), ignore.case = TRUE, value = TRUE)
 if (length(plat_col) > 0L) {
 got <- .gexpipe_first_nonempty_scalar(pd[[plat_col[[1L]]]])
 if (!is.null(got)) return(got)
 }
 }

 ""
}

#' Extract phenotype table from ExpressionSet or SummarizedExperiment
#' @keywords internal
.gexpipe_extract_geo_pdata <- function(gse) {
 .gexpipe_geo_pdata(gse)
}

#' Stack per-platform sample tables from a multi-platform GEO series
#'
#' A series distributed over several platforms yields one partial sample table
#' per platform. Stack them on the union of columns and drop samples that repeat
#' across platforms, so the series is represented by all of its samples.
#'
#' @param pds List of phenotype data.frames (NULL entries are ignored).
#' @return Single data.frame, or NULL when nothing usable was supplied.
#' @keywords internal
.gexpipe_rbind_pdata_parts <- function(pds) {
 if (!is.list(pds)) {
 pds <- list(pds)
 }
 pds <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L && ncol(x) > 0L, pds)
 if (length(pds) == 0L) {
 return(NULL)
 }
 if (length(pds) == 1L) {
 return(pds[[1L]])
 }
 all_cols <- unique(unlist(lapply(pds, colnames), use.names = FALSE))
 row_ids <- as.character(unlist(lapply(pds, rownames), use.names = FALSE))
 pds <- lapply(pds, function(df) {
 df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
 for (cn in all_cols) {
 df[[cn]] <- if (cn %in% colnames(df)) as.character(df[[cn]]) else NA_character_
 }
 df[, all_cols, drop = FALSE]
 })
 out <- do.call(rbind, pds)
 if (length(row_ids) == nrow(out)) {
 rownames(out) <- make.unique(row_ids, sep = "_")
 }
 key <- if ("geo_accession" %in% colnames(out)) {
 as.character(out[["geo_accession"]])
 } else {
 rownames(out)
 }
 dup <- duplicated(key) & !is.na(key) & nzchar(trimws(key))
 out[!dup, , drop = FALSE]
}

#' Cached series-matrix .gz files GEOquery would reuse for this GSE
#' @keywords internal
.gexpipe_geo_cache_files_for <- function(destdir, gse_id) {
 if (is.null(destdir) || !dir.exists(destdir)) {
 return(character(0))
 }
 pat <- paste0("^", gse_id, "(-GPL[0-9]+)?_series_matrix\\.txt\\.gz$")
 list.files(destdir, pattern = pat, full.names = TRUE, ignore.case = TRUE)
}

#' Test whether a file is actually gzip-compressed, by checking its magic
#' bytes on a plain (non-decompressing) file handle.
#'
#' GEO occasionally serves a plain-text file (e.g. an HTML error page, or an
#' uncompressed SOFT file) that still gets cached with a ".gz" name. Opening
#' such a file with \code{gzfile()}/\code{readBin} to *decompress* it can
#' segfault the R process outright (a crash, not a catchable R condition) -
#' this check never invokes the decompressor, so it is safe to call before
#' any code path that would.
#' @keywords internal
.gexpipe_is_valid_gzip <- function(path) {
 if (is.null(path) || !nzchar(path) || !file.exists(path)) {
 return(FALSE)
 }
 if (file.info(path)$size < 2L) {
 return(FALSE)
 }
 magic <- tryCatch({
 con <- file(path, open = "rb", raw = TRUE)
 on.exit(close(con), add = TRUE)
 readBin(con, "raw", n = 2L)
 }, error = function(e) raw(0))
 length(magic) == 2L && magic[[1L]] == as.raw(0x1f) && magic[[2L]] == as.raw(0x8b)
}

#' Remove any cached series-matrix files for this GSE that fail to
#' decompress, so GEOquery is forced to re-download a fresh copy instead of
#' silently reusing a corrupt/truncated cache (e.g. from an interrupted
#' download) on every subsequent run.
#' @keywords internal
.gexpipe_clean_corrupt_geo_cache <- function(destdir, gse_id) {
 files <- .gexpipe_geo_cache_files_for(destdir, gse_id)
 for (f in files) {
 if (!.gexpipe_is_valid_gzip(f)) {
 try(unlink(f), silent = TRUE)
 }
 }
 invisible(NULL)
}

#' Cached GPL platform-annotation .soft(.gz) files GEOquery would reuse
#' @keywords internal
.gexpipe_gpl_cache_files_for <- function(destdir, gpl_id) {
 if (is.null(destdir) || !dir.exists(destdir) || is.null(gpl_id) || !nzchar(gpl_id)) {
 return(character(0))
 }
 pat <- paste0("^", gpl_id, "\\.soft(\\.gz)?$")
 list.files(destdir, pattern = pat, full.names = TRUE, ignore.case = TRUE)
}

#' Remove cached GPL annotation files that are not actually valid gzip
#' (e.g. an HTML error page or plain-text SOFT file saved with a ".gz" name
#' after a failed download) - reading one with the decompressor can segfault
#' R outright, so this must run before any code path that would do that.
#' @keywords internal
.gexpipe_clean_corrupt_gpl_cache <- function(destdir, gpl_id) {
 files <- .gexpipe_gpl_cache_files_for(destdir, gpl_id)
 for (f in files) {
 if (endsWith(tolower(f), ".gz") && !.gexpipe_is_valid_gzip(f)) {
 try(unlink(f), silent = TRUE)
 }
 }
 invisible(NULL)
}

#' Fetch GEO series matrix via getGEO (ExpressionSet or SummarizedExperiment)
#' @keywords internal
.gexpipe_getgeo_series <- function(gse_id, ...) {
 # Default getGPL=FALSE for speed; caller ... can override.
 dots <- list(...)
 args <- list(GEO = gse_id, GSEMatrix = TRUE, getGPL = FALSE)
 if (length(dots) > 0L) {
 args[names(dots)] <- dots
 }
 if (!is.null(args$destdir)) {
 try(.gexpipe_clean_corrupt_geo_cache(args$destdir, gse_id), silent = TRUE)
 }
 # Some series (e.g. GSE13159, ~2000+ samples) have multi-hundred-MB series
 # matrix files; R's default 60s download timeout is easily exceeded on a
 # perfectly fine connection, which then gets misreported as a connectivity
 # problem. Bump it defensively here regardless of how the app was launched.
 old_timeout <- getOption("timeout", 60L)
 if (old_timeout < 600L) {
 options(timeout = 600L)
 on.exit(options(timeout = old_timeout), add = TRUE)
 }
 .gexpipe_geo_quiet(do.call(GEOquery::getGEO, args))
}

#' Align an enriched phenotype table onto primary sample IDs
#'
#' Matches by rownames, geo_accession, title, then positional fallback when
#' nrow matches (RNA-seq count headers like 7_S13 often differ from GSM IDs).
#' @keywords internal
.gexpipe_align_pdata_to_primary <- function(primary, extra) {
 if (is.null(extra) || !is.data.frame(extra) || nrow(extra) == 0L || ncol(extra) == 0L) {
 return(primary)
 }
 if (is.null(primary) || !is.data.frame(primary) || nrow(primary) == 0L) {
 return(extra)
 }
 primary <- as.data.frame(primary, stringsAsFactors = FALSE, check.names = FALSE)
 extra <- as.data.frame(extra, stringsAsFactors = FALSE, check.names = FALSE)
 prim_ids <- as.character(rownames(primary))
 map_idx <- match(prim_ids, rownames(extra))
 if (all(is.na(map_idx)) && "geo_accession" %in% colnames(extra)) {
 map_idx <- match(prim_ids, as.character(extra[["geo_accession"]]))
 }
 if (all(is.na(map_idx)) && "title" %in% colnames(extra)) {
 map_idx <- match(prim_ids, as.character(extra[["title"]]))
 }
 if (all(is.na(map_idx)) && "title" %in% colnames(primary) && "title" %in% colnames(extra)) {
 map_idx <- match(as.character(primary[["title"]]), as.character(extra[["title"]]))
 }
 if (all(is.na(map_idx)) && nrow(primary) == nrow(extra)) {
 map_idx <- seq_len(nrow(primary))
 }
 if (all(is.na(map_idx))) {
 return(extra)
 }
 out <- extra[map_idx, , drop = FALSE]
 rownames(out) <- prim_ids
 out
}

#' Expand GEO characteristics_ch* "key: value" fields into selectable columns
#'
#' @param pdata Sample phenotype data.frame.
#' @return data.frame with original columns plus one column per characteristic key.
#' @keywords internal
gexp_expand_geo_characteristics <- function(pdata) {
 if (is.null(pdata) || !is.data.frame(pdata) || ncol(pdata) == 0L || nrow(pdata) == 0L) {
 return(pdata)
 }
 char_cols <- grep("^characteristics", colnames(pdata), ignore.case = TRUE, value = TRUE)
 if (length(char_cols) == 0L) {
 return(pdata)
 }
 out <- pdata
 for (cc in char_cols) {
 vals <- .gexpipe_strip_geo_quotes(out[[cc]])
 has_key <- !is.na(vals) & grepl("^[^:]+:\\s*.+", vals)
 if (!any(has_key)) {
 next
 }
 keys <- rep(NA_character_, length(vals))
 keys[has_key] <- trimws(sub("^([^:]+):\\s*.*$", "\\1", vals[has_key]))
 uniq_keys <- unique(keys[!is.na(keys) & nzchar(keys)])
 for (k in uniq_keys) {
 new_col <- make.names(k)
 base <- new_col
 suffix <- 1L
 while (new_col %in% colnames(out)) {
 existing <- out[[new_col]]
 if (all(is.na(existing) | !nzchar(trimws(as.character(existing))))) {
 break
 }
 suffix <- suffix + 1L
 new_col <- paste0(base, ".", suffix)
 }
 col_vals <- rep(NA_character_, nrow(out))
 hit <- which(keys == k)
 col_vals[hit] <- trimws(sub("^[^:]+:\\s*", "", vals[hit]))
 if (new_col %in% colnames(out)) {
 miss <- is.na(out[[new_col]]) | !nzchar(trimws(as.character(out[[new_col]])))
 out[[new_col]][miss] <- col_vals[miss]
 } else {
 out[[new_col]] <- col_vals
 }
 }
 }
 out
}

#' Merge two phenotype tables by sample ID, keeping the union of columns
#' @keywords internal
.gexpipe_merge_pdata_columns <- function(primary, extra) {
 if (is.null(extra) || !is.data.frame(extra) || nrow(extra) == 0L || ncol(extra) == 0L) {
 return(primary)
 }
 if (is.null(primary) || !is.data.frame(primary) || nrow(primary) == 0L) {
 return(extra)
 }
 primary <- as.data.frame(primary, stringsAsFactors = FALSE, check.names = FALSE)
 extra <- as.data.frame(extra, stringsAsFactors = FALSE, check.names = FALSE)
 # When primary is thin (title-only stub), prefer fully aligned enriched table
 if (ncol(primary) <= 1L && ncol(extra) > ncol(primary)) {
 return(.gexpipe_align_pdata_to_primary(primary, extra))
 }
 map_idx <- match(rownames(primary), rownames(extra))
 if (all(is.na(map_idx)) && "geo_accession" %in% colnames(primary) &&
 "geo_accession" %in% colnames(extra)) {
 map_idx <- match(
 as.character(primary[["geo_accession"]]),
 as.character(extra[["geo_accession"]])
 )
 }
 if (all(is.na(map_idx)) && "title" %in% colnames(extra)) {
 map_idx <- match(as.character(rownames(primary)), as.character(extra[["title"]]))
 }
 if (all(is.na(map_idx)) && "title" %in% colnames(primary) && "title" %in% colnames(extra)) {
 map_idx <- match(as.character(primary[["title"]]), as.character(extra[["title"]]))
 }
 if (all(is.na(map_idx)) && nrow(primary) == nrow(extra)) {
 map_idx <- seq_len(nrow(primary))
 }
 for (cn in colnames(extra)) {
 if (!cn %in% colnames(primary)) {
 primary[[cn]] <- NA_character_
 }
 if (all(is.na(map_idx))) {
 next
 }
 hit <- which(!is.na(map_idx))
 prim_vals <- as.character(primary[[cn]])
 fill <- hit[is.na(prim_vals[hit]) | !nzchar(trimws(prim_vals[hit]))]
 if (length(fill) > 0L) {
 primary[[cn]][fill] <- as.character(extra[[cn]])[map_idx[fill]]
 }
 }
 primary
}

#' Fetch full GEO phenotype data for any series
#'
#' Universal path for download + Groups UI: try getGEO pData/colData first
#' (same as a typical GEOquery workflow), expand characteristics_* key:value
#' columns, fall back to series-matrix FTP when still thin/NULL, then optionally
#' align rownames to count/expression sample IDs.
#'
#' @param gse_id GEO series accession.
#' @param sample_ids Optional character vector of primary sample IDs
#' (count/expression colnames) used to align rownames.
#' @return data.frame with all available phenotype columns, or NULL.
#' @keywords internal
gexp_fetch_full_pdata <- function(gse_id, sample_ids = NULL) {
 gse_id <- toupper(trimws(as.character(gse_id)[[1L]]))
 if (!nzchar(gse_id)) {
 return(NULL)
 }

 pd <- tryCatch({
 gse_list <- .gexpipe_getgeo_series(gse_id)
 # getGEO returns one element per platform. Series split across platforms
 # (e.g. GSE114007 on GPL11154 + GPL18573) must contribute every sample,
 # otherwise the expected sample count is capped at one platform's share.
 parts <- if (inherits(gse_list, "list")) gse_list else list(gse_list)
 .gexpipe_rbind_pdata_parts(lapply(parts, .gexpipe_extract_geo_pdata))
 }, error = function(e) NULL)

 if (!is.null(pd) && is.data.frame(pd) && nrow(pd) > 0L) {
 pd <- gexp_expand_geo_characteristics(pd)
 }

 thin <- is.null(pd) || !is.data.frame(pd) || nrow(pd) == 0L || ncol(pd) <= 1L ||
 (ncol(pd) == 1L && grepl("^title$", colnames(pd)[[1L]], ignore.case = TRUE))
 if (isTRUE(thin)) {
 sm <- tryCatch(gexp_fetch_geo_series_matrix_metadata(gse_id), error = function(e) NULL)
 if (!is.null(sm) && is.data.frame(sm) && nrow(sm) > 0L && ncol(sm) > 0L) {
 sm <- gexp_expand_geo_characteristics(sm)
 if (is.null(pd) || !is.data.frame(pd) || nrow(pd) == 0L || ncol(sm) > ncol(pd)) {
 pd <- sm
 } else {
 pd <- .gexpipe_merge_pdata_columns(pd, sm)
 }
 }
 }

 if (is.null(pd) || !is.data.frame(pd) || nrow(pd) == 0L || ncol(pd) == 0L) {
 return(NULL)
 }

 if (!is.null(sample_ids) && length(sample_ids) > 0L) {
 sample_ids <- as.character(sample_ids)
 primary <- data.frame(
 title = sample_ids,
 row.names = sample_ids,
 stringsAsFactors = FALSE
 )
 pd <- .gexpipe_align_pdata_to_primary(primary, pd)
 }

 pd
}

#' Fetch sample metadata from GEO series matrix fallback
#'
#' @param gse_id GEO series ID.
#' @return data.frame or NULL.
#'
#' @examples
#' if (interactive()) {
#' md <- gexp_fetch_geo_series_matrix_metadata("GSE10072")
#' if (!is.null(md)) head(md)
#' }
#' @export
gexp_fetch_geo_series_matrix_metadata <- function(gse_id) {
 gse_id <- toupper(trimws(as.character(gse_id)[[1L]]))
 urls <- .gexpipe_series_matrix_urls(gse_id)
 if (length(urls) == 0L) {
 return(NULL)
 }
 parts <- lapply(urls, .gexpipe_parse_series_matrix_url)
 .gexpipe_rbind_pdata_parts(parts)
}

#' Candidate series-matrix URLs for a GEO series
#'
#' Multi-platform series have no plain \code{GSE_series_matrix.txt.gz}; they ship
#' one \code{GSE-GPLxxxxx_series_matrix.txt.gz} per platform. List the matrix
#' directory so every platform file is picked up.
#'
#' @param gse_id GEO series ID.
#' @return Character vector of URLs (plain file first when no listing is found).
#' @keywords internal
.gexpipe_series_matrix_urls <- function(gse_id) {
 gse_id <- toupper(trimws(as.character(gse_id)[[1L]]))
 base_url <- sprintf(
 "https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/matrix/",
 .gexpipe_geo_series_folder(gse_id), gse_id
 )
 listing <- tryCatch({
 conn <- url(base_url, open = "rb")
 on.exit(try(close(conn), silent = TRUE), add = TRUE)
 readLines(conn, warn = FALSE)
 }, error = function(e) character(0))
 names_found <- character(0)
 if (length(listing) > 0L) {
 hits <- regmatches(
 listing,
 gregexpr(paste0(gse_id, "[A-Za-z0-9_.-]*_series_matrix\\.txt\\.gz"), listing)
 )
 names_found <- unique(unlist(hits, use.names = FALSE))
 }
 if (length(names_found) == 0L) {
 return(paste0(base_url, gse_id, "_series_matrix.txt.gz"))
 }
 paste0(base_url, names_found)
}

#' Download and parse the sample metadata block of one series-matrix file
#'
#' @param url_str URL of a \code{*_series_matrix.txt.gz} file.
#' @return data.frame of sample metadata, or NULL.
#' @keywords internal
.gexpipe_parse_series_matrix_url <- function(url_str) {
 conn <- NULL
 tmp_gz <- NULL
 tryCatch({
 # Series matrix files are gzip-compressed; url()+readLines alone never
 # decompresses, so !sample_ lines are invisible unless we wrap with gzcon.
 raw_lines <- character(0)
 tryCatch({
 conn <- gzcon(url(url_str, open = "rb"))
 raw_lines <- readLines(conn, warn = FALSE, encoding = "UTF-8")
 try(close(conn), silent = TRUE)
 conn <- NULL
 }, error = function(e) {
 if (!is.null(conn)) {
 try(close(conn), silent = TRUE)
 conn <<- NULL
 }
 tmp_gz <<- tempfile(fileext = ".txt.gz")
 utils::download.file(url_str, destfile = tmp_gz, mode = "wb", quiet = TRUE)
 conn <<- gzfile(tmp_gz, open = "rt")
 raw_lines <<- readLines(conn, warn = FALSE, encoding = "UTF-8")
 try(close(conn), silent = TRUE)
 conn <<- NULL
 })
 if (length(raw_lines) == 0) {
 return(NULL)
 }
 # Stop before expression table for speed (sample metadata is above this marker)
 table_begin <- grep("^!series_matrix_table_begin", raw_lines, ignore.case = TRUE)
 if (length(table_begin) > 0L) {
 raw_lines <- raw_lines[seq_len(max(1L, table_begin[[1L]] - 1L))]
 }
 idx <- grep("^!sample_", raw_lines, ignore.case = TRUE)
 if (length(idx) == 0) {
 return(NULL)
 }
 lines <- strsplit(raw_lines[idx], "\t", fixed = TRUE)
 attr_names <- vapply(lines, function(x) sub("^!sample_", "", x[1], ignore.case = TRUE), character(1))
 n_samples <- max(vapply(lines, length, integer(1))) - 1L
 if (n_samples < 1) {
 return(NULL)
 }
 # Prefer GSM accessions as rownames (first !Sample_ line is usually title)
 acc_idx <- which(tolower(attr_names) == "geo_accession")
 if (length(acc_idx) > 0L) {
 sample_ids <- .gexpipe_strip_geo_quotes(lines[[acc_idx[[1L]]]][-1])
 } else {
 sample_ids <- .gexpipe_strip_geo_quotes(lines[[1]][-1])
 }
 sample_ids <- head(sample_ids, n_samples)
 if (length(sample_ids) < n_samples) {
 sample_ids <- c(sample_ids, paste0("sample_", seq_len(n_samples - length(sample_ids))))
 }
 sample_ids <- make.unique(as.character(sample_ids), sep = "_")
 out <- as.data.frame(
 matrix(NA_character_, nrow = length(sample_ids), ncol = length(attr_names)),
 stringsAsFactors = FALSE
 )
 colnames(out) <- make.names(attr_names, unique = TRUE)
 rownames(out) <- sample_ids
 for (j in seq_along(lines)) {
 vals <- .gexpipe_strip_geo_quotes(lines[[j]][-1])
 n <- min(length(vals), nrow(out))
 if (n > 0) out[seq_len(n), j] <- vals[seq_len(n)]
 }
 gexp_expand_geo_characteristics(out)
 }, error = function(e) NULL, finally = {
 if (!is.null(conn)) try(close(conn), silent = TRUE)
 if (!is.null(tmp_gz) && file.exists(tmp_gz)) try(unlink(tmp_gz), silent = TRUE)
 })
}

#' Build diagnostic log text when no common genes are found
#'
#' @param all_genes_list Named list of row IDs per dataset.
#' @return Character string for log appending.
#'
#' @examples
#' txt <- gexp_no_common_genes_diagnostic_log(
#' list(D1 = c("1007_s_at", "1053_at"), D2 = c("ENSG000001", "ENSG000002"))
#' )
#' nchar(txt) > 0
#' @export
gexp_no_common_genes_diagnostic_log <- function(all_genes_list) {
 txt <- "\nNo common genes across datasets after converting to gene symbols.\n"
 txt <- paste0(txt, "Diagnostic: sample row IDs per dataset (to see if mapping to symbols failed):\n")
 for (gse in names(all_genes_list)) {
 rn <- all_genes_list[[gse]]
 sample_rn <- head(rn[!is.na(rn) & nzchar(trimws(rn))], 5)
 txt <- paste0(txt, " ", gse, " (", length(rn), " rows): ", paste(sQuote(sample_rn), collapse = ", "), "\n")
 }
 txt <- paste0(txt, "\n--- Gene ID conversion (probe / Entrez -> symbols) ---\n")
 txt <- paste0(txt, "If you see probe IDs (2824546_st, 200000_s_at) or numeric IDs (Entrez) instead of symbols (e.g. BRCA1):\n")
 txt <- paste0(txt, " 1. Check internet (biomaRt needs Ensembl); install biomaRt: BiocManager::install(\"biomaRt\")\n")
 txt <- paste0(txt, " 2. RNA-seq with Entrez row IDs is converted via org.Hs.eg.db then biomaRt fallback\n")
 txt <- paste0(txt, " 3. Microarray: GEO GPL annotation is used when biomaRt is offline\n")
 txt <- paste0(txt, " 4. For testing merged RNA+microarray, use GSEs that map to symbols (e.g. GSE62646)\n")
 txt
}

#' Rebuild per-dataset gene lists from expression/count lists
#'
#' @param micro_expr_list Named list of microarray matrices.
#' @param rna_counts_list Named list of RNA-seq matrices.
#' @return Named list of gene vectors.
#'
#' @examples
#' m <- matrix(1:6, nrow = 2, dimnames = list(c("A", "B"), paste0("S", 1:3)))
#' r <- matrix(1:6, nrow = 2, dimnames = list(c("B", "C"), paste0("T", 1:3)))
#' gexp_rebuild_all_genes_list(list(M = m), list(R = r))
#' @export
gexp_rebuild_all_genes_list <- function(micro_expr_list, rna_counts_list) {
 out <- list()
 for (gse in names(micro_expr_list)) out[[gse]] <- rownames(micro_expr_list[[gse]])
 for (gse in names(rna_counts_list)) out[[gse]] <- rownames(rna_counts_list[[gse]])
 out
}

#' Normalize dataset row IDs to gene symbols for overlap
#'
#' @param micro_expr_list Named list of microarray matrices.
#' @param rna_counts_list Named list of RNA-seq matrices.
#' @param platform_per_gse Optional named list/vector of GPL IDs.
#' @param all_genes_list Optional prebuilt gene lists.
#' @param micro_eset_list Optional named list of \code{ExpressionSet} objects for
#' \code{fData} fallback when probe-to-symbol mapping needs platform annotation.
#' @return List with updated lists and appended log text.
#'
#' @examples
#' m <- matrix(1:6, nrow = 2, dimnames = list(c("A", "B"), paste0("S", 1:3)))
#' r <- matrix(1:6, nrow = 2, dimnames = list(c("B", "C"), paste0("T", 1:3)))
#' out <- gexp_download_normalize_ids_for_overlap(
#' micro_expr_list = list(M = m),
#' rna_counts_list = list(R = r)
#' )
#' names(out)
#' @export
gexp_download_normalize_ids_for_overlap <- function(
 micro_expr_list,
 rna_counts_list,
 platform_per_gse = NULL,
 all_genes_list = NULL,
 micro_eset_list = NULL # optional: stored ExpressionSets for fData fallback
) {
 normalize_symbol_tokens <- function(ids) {
 ids <- as.character(ids)
 out <- vapply(ids, function(x) {
 if (is.na(x)) return(NA_character_)
 x <- trimws(x)
 if (!nzchar(x)) return(NA_character_)
 toks <- unlist(strsplit(x, "\\s*///\\s*|\\s*//\\s*|\\s*;\\s*|\\s*,\\s*"))
 toks <- trimws(toks)
 toks <- toks[nzchar(toks)]
 if (length(toks) == 0) return(NA_character_)
 toks[1]
 }, character(1))
 out
 }

 if (is.null(all_genes_list)) {
 all_genes_list <- gexp_rebuild_all_genes_list(micro_expr_list, rna_counts_list)
 }

 log_text <- "\nSTEP 2b: Normalize IDs to gene symbols for overlap...\n"

 for (gse in names(micro_expr_list)) {
 .gse_result <- tryCatch({
 micro_expr <- micro_expr_list[[gse]]
 rn <- rownames(micro_expr)
 gpl <- if (!is.null(platform_per_gse)) platform_per_gse[[gse]] else NULL
 micro_eset <- if (!is.null(micro_eset_list)) micro_eset_list[[gse]] else NULL
 if (gexpipe_ids_need_symbol_conversion(rn, gpl_id = gpl)) {
 fmt <- detect_gene_id_format(rn)
 log_text <- paste0(log_text, " ", gse, ": format ", fmt, " -> converting to symbols...\n")
 fdata <- if (!is.null(micro_eset)) {
 tryCatch(.gexpipe_geo_fdata(micro_eset), error = function(e) NULL)
 } else {
 NULL
 }
 sym <- map_microarray_ids(micro_expr, fdata, micro_eset, gse_id = gse)
 if (is.null(sym) || sum(!is.na(sym) & nzchar(trimws(sym))) <= length(rn) * 0.05) {
 sym <- any_id_to_symbol(rn, gpl_id = gpl, gse_id = gse)
 }
 converted <- FALSE
 if (!is.null(sym) && length(sym) == length(rn)) {
 valid <- !is.na(sym) & nzchar(trimws(sym))
 accept <- .gexpipe_accept_overlap_mapping(sym, rn)
 if (accept) {
 rownames(micro_expr) <- sym
 micro_expr <- micro_expr[valid, , drop = FALSE]
 if (any(duplicated(rownames(micro_expr)))) {
 micro_expr <- limma::avereps(micro_expr, ID = rownames(micro_expr))
 }
 micro_expr_list[[gse]] <- micro_expr
 all_genes_list[[gse]] <- rownames(micro_expr)
 log_text <- paste0(log_text, " ", gse, ": converted to ", nrow(micro_expr), " gene symbols\n")
 converted <- TRUE
 }
 }
 # Fallback: use fData from stored ExpressionSet (avoids repeat GPL download)
 if (!converted && !is.null(fdata) && is.data.frame(fdata) && nrow(fdata) == length(rn)) {
 fd_sym <- .gexpipe_extract_fdata_symbols(fdata)
 if (!is.null(fd_sym) && length(fd_sym) == length(rn)) {
 valid_fd <- !is.na(fd_sym) & nzchar(trimws(fd_sym))
 if (sum(valid_fd) > length(rn) * 0.05 &&
 .gexpipe_accept_mapped_symbols(fd_sym, length(rn))) {
 micro_expr_fd <- micro_expr
 rownames(micro_expr_fd) <- fd_sym
 micro_expr_fd <- micro_expr_fd[valid_fd, , drop = FALSE]
 if (any(duplicated(rownames(micro_expr_fd)))) {
 micro_expr_fd <- limma::avereps(micro_expr_fd, ID = rownames(micro_expr_fd))
 }
 micro_expr_list[[gse]] <- micro_expr_fd
 all_genes_list[[gse]] <- rownames(micro_expr_fd)
 log_text <- paste0(log_text, " ", gse, ": fData fallback -> ",
 nrow(micro_expr_fd), " gene symbols\n")
 converted <- TRUE
 }
 }
 }
 if (!converted) {
 log_text <- paste0(
 log_text, " ", gse,
 ": conversion did not yield recognizable symbols; kept original IDs",
 " (probe/custom IDs will not overlap across platforms - need GPL gene symbols)\n"
 )
 }
 } else {
 sym <- normalize_symbol_tokens(rn)
 valid <- !is.na(sym) & trimws(sym) != ""
 if (sum(valid) > 0) {
 rownames(micro_expr) <- sym
 micro_expr <- micro_expr[valid, , drop = FALSE]
 if (any(duplicated(rownames(micro_expr)))) micro_expr <- limma::avereps(micro_expr, ID = rownames(micro_expr))
 micro_expr_list[[gse]] <- micro_expr
 all_genes_list[[gse]] <- rownames(micro_expr)
 }
 }
 list(ok = TRUE, log_text = log_text, micro_expr_list = micro_expr_list, all_genes_list = all_genes_list)
 }, error = function(e) {
 list(ok = FALSE, msg = conditionMessage(e))
 })
 if (isTRUE(.gse_result$ok)) {
 log_text <- .gse_result$log_text
 micro_expr_list <- .gse_result$micro_expr_list
 all_genes_list <- .gse_result$all_genes_list
 } else {
 log_text <- paste0(log_text, " ", gse, ": ID conversion error (", .gse_result$msg, ") - kept original IDs\n")
 }
 }

 for (gse in names(rna_counts_list)) {
 .gse_result <- tryCatch({
 cnt <- rna_counts_list[[gse]]
 rn <- rownames(cnt)
 if (gexpipe_ids_need_symbol_conversion(rn, gpl_id = NULL)) {
 fmt <- detect_gene_id_format(rn)
 log_text <- paste0(log_text, " ", gse, ": format ", fmt, " -> converting to symbols...\n")
 sym <- convert_rnaseq_ids(rn, gse_id = gse)
 if (is.null(sym) || sum(!is.na(sym) & nzchar(trimws(sym))) <= length(rn) * 0.05) {
 sym <- any_id_to_symbol(rn, gpl_id = NULL, gse_id = gse)
 }
 if (!is.null(sym) && length(sym) == length(rn)) {
 valid <- !is.na(sym) & nzchar(trimws(sym))
 accept <- .gexpipe_accept_overlap_mapping(sym, rn)
 if (accept) {
 rownames(cnt) <- sym
 cnt <- cnt[valid, , drop = FALSE]
 if (any(duplicated(rownames(cnt)))) cnt <- limma::avereps(cnt, ID = rownames(cnt))
 rna_counts_list[[gse]] <- cnt
 all_genes_list[[gse]] <- rownames(cnt)
 log_text <- paste0(log_text, " ", gse, ": converted to ", nrow(cnt), " gene symbols\n")
 } else {
 log_text <- paste0(log_text, " ", gse, ": conversion did not yield recognizable symbols; kept original IDs\n")
 }
 } else {
 log_text <- paste0(log_text, " ", gse, ": conversion did not yield recognizable symbols; kept original IDs\n")
 }
 } else {
 sym <- normalize_symbol_tokens(rn)
 valid <- !is.na(sym) & trimws(sym) != ""
 if (sum(valid) > 0) {
 rownames(cnt) <- sym
 cnt <- cnt[valid, , drop = FALSE]
 if (any(duplicated(rownames(cnt)))) cnt <- limma::avereps(cnt, ID = rownames(cnt))
 rna_counts_list[[gse]] <- cnt
 all_genes_list[[gse]] <- rownames(cnt)
 }
 }
 list(ok = TRUE, log_text = log_text, rna_counts_list = rna_counts_list, all_genes_list = all_genes_list)
 }, error = function(e) {
 list(ok = FALSE, msg = conditionMessage(e))
 })
 if (isTRUE(.gse_result$ok)) {
 log_text <- .gse_result$log_text
 rna_counts_list <- .gse_result$rna_counts_list
 all_genes_list <- .gse_result$all_genes_list
 } else {
 log_text <- paste0(log_text, " ", gse, ": ID conversion error (", .gse_result$msg, ") - kept original IDs\n")
 }
 }

 for (gse in names(rna_counts_list)) {
 .gse_result <- tryCatch({
 rn <- rownames(rna_counts_list[[gse]])
 sample_rn <- head(rn[!is.na(rn) & nzchar(trimws(rn))], min(300, length(rn)))
 if (length(sample_rn) > 0 && mean(grepl("^[0-9]+$", sample_rn), na.rm = TRUE) > 0.7) {
 log_text <- paste0(log_text, " ", gse, ": row IDs still Entrez-like -> trying biomaRt Entrez->symbol...\n")
 sym <- entrez_to_symbol_biomart(rn)
 if (!is.null(sym) && length(sym) == length(rn)) {
 valid <- !is.na(sym) & nzchar(trimws(sym))
 mapped_rate <- if (sum(valid) > 0L) mean(sym[valid] != rn[valid], na.rm = TRUE) else 0
 sym_ok <- sum(valid) > 0L && gexpipe_ids_are_verified_symbols(sym[valid])
 if (sum(valid) > length(rn) * 0.05 && (sym_ok || mapped_rate > 0.05)) {
 cnt <- rna_counts_list[[gse]]
 rownames(cnt) <- sym
 cnt <- cnt[valid, , drop = FALSE]
 if (any(duplicated(rownames(cnt)))) cnt <- limma::avereps(cnt, ID = rownames(cnt))
 rna_counts_list[[gse]] <- cnt
 all_genes_list[[gse]] <- rownames(cnt)
 log_text <- paste0(log_text, " ", gse, ": biomaRt converted to ", nrow(cnt), " gene symbols\n")
 }
 }
 }
 list(ok = TRUE, log_text = log_text, rna_counts_list = rna_counts_list, all_genes_list = all_genes_list)
 }, error = function(e) {
 list(ok = FALSE, msg = conditionMessage(e))
 })
 if (isTRUE(.gse_result$ok)) {
 log_text <- .gse_result$log_text
 rna_counts_list <- .gse_result$rna_counts_list
 all_genes_list <- .gse_result$all_genes_list
 } else {
 log_text <- paste0(log_text, " ", gse, ": biomaRt Entrez->symbol error (", .gse_result$msg, ") - kept original IDs\n")
 }
 }

 for (gse in names(all_genes_list)) {
 .gse_result <- tryCatch({
 rn <- all_genes_list[[gse]]
 sample_rn <- head(rn[!is.na(rn) & nzchar(trimws(rn))], min(500, length(rn)))
 if (length(sample_rn) > 0 && mean(grepl("^[0-9]+_st$", sample_rn), na.rm = TRUE) > 0.5) {
 log_text <- paste0(log_text, " ", gse, ": detected Affymetrix HuGene probe (_st) format -> converting...\n")
 gpl <- if (!is.null(platform_per_gse)) platform_per_gse[[gse]] else NULL
 sym <- probe_ids_to_symbol_hugene_db(rn, gpl)
 if (is.null(sym) || length(sym) != length(rn) || sum(!is.na(sym)) <= length(rn) * 0.1) sym <- probe_ids_to_symbol_gpl(rn, gpl)
 if (is.null(sym) || length(sym) != length(rn) || sum(!is.na(sym)) <= length(rn) * 0.1) sym <- probe_ids_to_symbol_biomart(rn, gpl)
 if (!is.null(sym) && length(sym) == length(rn) && sum(!is.na(sym)) > length(rn) * 0.1) {
 valid <- !is.na(sym) & trimws(sym) != ""
 if (gse %in% names(micro_expr_list)) {
 micro_expr <- micro_expr_list[[gse]]
 if (nrow(micro_expr) == length(sym)) {
 rownames(micro_expr) <- sym
 micro_expr <- micro_expr[valid, , drop = FALSE]
 if (any(duplicated(rownames(micro_expr)))) micro_expr <- limma::avereps(micro_expr, ID = rownames(micro_expr))
 micro_expr_list[[gse]] <- micro_expr
 }
 } else if (gse %in% names(rna_counts_list)) {
 cnt <- rna_counts_list[[gse]]
 if (nrow(cnt) == length(sym)) {
 rownames(cnt) <- sym
 cnt <- cnt[valid, , drop = FALSE]
 if (any(duplicated(rownames(cnt)))) cnt <- limma::avereps(cnt, ID = rownames(cnt))
 rna_counts_list[[gse]] <- cnt
 }
 }
 all_genes_list[[gse]] <- if (gse %in% names(micro_expr_list)) rownames(micro_expr_list[[gse]]) else rownames(rna_counts_list[[gse]])
 n_after <- if (gse %in% names(micro_expr_list)) nrow(micro_expr_list[[gse]]) else nrow(rna_counts_list[[gse]])
 log_text <- paste0(log_text, " ", gse, ": _st probe IDs converted to ", n_after, " gene symbols (HuGene/GEO GPL/biomaRt)\n")
 }
 }
 list(ok = TRUE, log_text = log_text, micro_expr_list = micro_expr_list,
 rna_counts_list = rna_counts_list, all_genes_list = all_genes_list)
 }, error = function(e) {
 list(ok = FALSE, msg = conditionMessage(e))
 })
 if (isTRUE(.gse_result$ok)) {
 log_text <- .gse_result$log_text
 micro_expr_list <- .gse_result$micro_expr_list
 rna_counts_list <- .gse_result$rna_counts_list
 all_genes_list <- .gse_result$all_genes_list
 } else {
 log_text <- paste0(log_text, " ", gse, ": _st conversion error (", .gse_result$msg, ") - kept original IDs\n")
 }
 }

 for (gse in names(all_genes_list)) {
 .gse_result <- tryCatch({
 rn <- all_genes_list[[gse]]
 sample_rn <- .gexpipe_stratified_id_sample(rn, 500L)
 if (length(sample_rn) > 0 &&
 mean(grepl("^ENSG.*_at$", sample_rn, ignore.case = TRUE), na.rm = TRUE) > 0.3 &&
 gexpipe_ids_need_symbol_conversion(rn, gpl_id = if (!is.null(platform_per_gse)) platform_per_gse[[gse]] else NULL)) {
 log_text <- paste0(log_text, " ", gse, ": detected Ensembl Affymetrix probe (_at) format -> converting...\n")
 gpl <- if (!is.null(platform_per_gse)) platform_per_gse[[gse]] else NULL
 sym <- .gexpipe_ensembl_ids_to_symbols(rn)
 if (is.null(sym) || length(sym) != length(rn) || sum(!is.na(sym)) <= length(rn) * 0.05) {
 sym <- any_id_to_symbol(rn, gpl_id = gpl, gse_id = gse)
 }
 if (!is.null(sym) && length(sym) == length(rn) && .gexpipe_accept_overlap_mapping(sym, rn)) {
 valid <- !is.na(sym) & nzchar(trimws(sym))
 if (gse %in% names(micro_expr_list)) {
 micro_expr <- micro_expr_list[[gse]]
 if (nrow(micro_expr) == length(sym)) {
 rownames(micro_expr) <- sym
 micro_expr <- micro_expr[valid, , drop = FALSE]
 if (any(duplicated(rownames(micro_expr)))) {
 micro_expr <- limma::avereps(micro_expr, ID = rownames(micro_expr))
 }
 micro_expr_list[[gse]] <- micro_expr
 }
 } else if (gse %in% names(rna_counts_list)) {
 cnt <- rna_counts_list[[gse]]
 if (nrow(cnt) == length(sym)) {
 rownames(cnt) <- sym
 cnt <- cnt[valid, , drop = FALSE]
 if (any(duplicated(rownames(cnt)))) {
 cnt <- limma::avereps(cnt, ID = rownames(cnt))
 }
 rna_counts_list[[gse]] <- cnt
 }
 }
 all_genes_list[[gse]] <- if (gse %in% names(micro_expr_list)) {
 rownames(micro_expr_list[[gse]])
 } else {
 rownames(rna_counts_list[[gse]])
 }
 n_after <- if (gse %in% names(micro_expr_list)) {
 nrow(micro_expr_list[[gse]])
 } else {
 nrow(rna_counts_list[[gse]])
 }
 log_text <- paste0(
 log_text, " ", gse,
 ": Ensembl _at probe IDs converted to ", n_after, " gene symbols\n"
 )
 }
 }
 list(ok = TRUE, log_text = log_text, micro_expr_list = micro_expr_list,
 rna_counts_list = rna_counts_list, all_genes_list = all_genes_list)
 }, error = function(e) {
 list(ok = FALSE, msg = conditionMessage(e))
 })
 if (isTRUE(.gse_result$ok)) {
 log_text <- .gse_result$log_text
 micro_expr_list <- .gse_result$micro_expr_list
 rna_counts_list <- .gse_result$rna_counts_list
 all_genes_list <- .gse_result$all_genes_list
 } else {
 log_text <- paste0(log_text, " ", gse, ": _at conversion error (", .gse_result$msg, ") - kept original IDs\n")
 }
 }

 log_text <- paste0(log_text, "\nGene symbols extracted per GSE (format detected + sample):\n")
 for (gse in names(all_genes_list)) {
 rn <- all_genes_list[[gse]]
 n <- length(rn)
 sample_rn <- head(rn[!is.na(rn) & nzchar(trimws(rn))], 10)
 sample_str <- if (length(sample_rn) > 0) paste(sQuote(sample_rn), collapse = ", ") else "(none)"
 fmt <- detect_gene_id_format(rn)
 log_text <- paste0(log_text, " ", gse, ": ", n, " genes; format: ", fmt, "; sample: ", sample_str, "\n")
 }

 list(
 micro_expr_list = micro_expr_list,
 rna_counts_list = rna_counts_list,
 all_genes_list = all_genes_list,
 log_text = log_text
 )
}

#' Download and parse one microarray GSE
#'
#' @param gse_id GEO series ID.
#' @param micro_dir Directory for supplementary files.
#' @param download_cel Logical; if `TRUE`, also download raw CEL files for
#'   RMA normalization (Affymetrix). Default `NULL` follows
#'   `options(gexpipe.download_cel)` (FALSE).
#' @param fast Logical; if `TRUE` (default from
#'   `options(gexpipe.fast_download)`), skip slower optional lookups.
#' @return List with status, log text, reason, expression, metadata, eset, platform_id, and cel_paths.
#'
#' @examples
#' if (interactive()) {
#' out <- gexp_download_one_microarray_gse("GSE10072", tempdir())
#' out$ok
#' }
#' @export
gexp_download_one_microarray_gse <- function(gse_id, micro_dir, download_cel = NULL, fast = NULL) {
 if (is.null(fast)) {
 fast <- isTRUE(getOption("gexpipe.fast_download", TRUE))
 }
 if (is.null(download_cel)) {
 download_cel <- isTRUE(getOption("gexpipe.download_cel", FALSE))
 }
 out <- list(
 ok = FALSE, reason = NULL, log = "", micro_expr = NULL, metadata = NULL,
 micro_eset = NULL, platform_id = NULL, cel_paths = character(0)
 )

 micro_data <- tryCatch(
 {
 # Skip GPL download: many series (e.g. GSE89076) return SummarizedExperiment
 # and a corrupt/HTML GPL cache breaks getGEO even when the matrix is fine.
 .gexpipe_getgeo_series(gse_id, destdir = micro_dir)
 },
 error = function(e) structure(list(error = conditionMessage(e)), class = "geo_error")
 )

 if (inherits(micro_data, "geo_error")) {
 err_msg <- micro_data$error
 out$reason <- if (grepl("timeout|timed out", err_msg, ignore.case = TRUE)) {
 "download timed out - this GSE may be very large (e.g. GSE13159); your internet connection may be fine, just retry, or it will usually succeed on a second attempt once partial data is cached"
 } else if (grepl("connection|hostname|resolve|HTTP|ssl|could not resolve|Unable to", err_msg, ignore.case = TRUE)) {
 "network/HTTP - check internet connection"
 } else if (grepl("destfile", err_msg, ignore.case = TRUE)) {
 "GEOquery download failed (destfile not found) - retry later or download this GSE manually from GEO"
 } else {
 substr(gsub("\n", " ", err_msg), 1L, 120L)
 }
 return(out)
 }

 eset_parse <- tryCatch({
 if (is.list(micro_data) && length(micro_data) >= 1) {
 platforms <- vapply(micro_data,
 function(x) tryCatch(.gexpipe_geo_annotation(x), error = function(e) ""),
 character(1))

 # ----------------------------------------------------------------
 # GEO sometimes splits one series into several GSEMatrix files
 # (e.g. _series_matrix-1.txt.gz, _series_matrix-2.txt.gz).
 # Each file becomes one ExpressionSet in the returned list, all
 # sharing the same platform. The old code used which.max(n_feat)
 # which picked only the first file -> missing samples.
 #
 # Fix: group ExpressionSets by platform; for groups that share the
 # same probe set, cbind expression matrices and rbind pData so ALL
 # samples are retained. Then pick the platform group with the most
 # combined samples.
 # ----------------------------------------------------------------
 unique_plts <- unique(platforms)
 best_eset <- NULL
 best_expr_mat <- NULL
 best_pdata <- NULL
 best_plt_id <- ""
 best_n_samp <- 0L
 n_files_used <- 0L

 for (plt in unique_plts) {
 grp_idx <- which(platforms == plt)
 grp_mats <- lapply(micro_data[grp_idx],
 function(x) tryCatch(.gexpipe_geo_expr_matrix(x), error = function(e) NULL))
 grp_pds <- lapply(micro_data[grp_idx],
 function(x) tryCatch(.gexpipe_geo_pdata(x), error = function(e) NULL))

 # Drop null / empty matrices
 ok_idx <- which(vapply(grp_mats, function(m) !is.null(m) && nrow(m) > 0 && ncol(m) > 0, logical(1)))
 if (length(ok_idx) == 0) next
 grp_mats <- grp_mats[ok_idx]
 grp_pds <- grp_pds[ok_idx]
 grp_idx <- grp_idx[ok_idx]

 if (length(grp_mats) == 1) {
 comb_mat <- grp_mats[[1]]
 comb_pd <- grp_pds[[1]]
 n_used <- 1L
 } else {
 # Check that all matrices have identical row names (same probes)
 rn_ref <- rownames(grp_mats[[1]])
 all_same_rows <- all(vapply(grp_mats[-1],
 function(m) identical(rownames(m), rn_ref), logical(1)))
 if (all_same_rows) {
 # Safe to cbind: combine all sample columns
 all_cols <- unlist(lapply(grp_mats, colnames))
 if (anyDuplicated(all_cols)) {
 # Remove truly duplicate sample columns (same GSM ID)
 seen <- character(0)
 keep_mats <- list(); keep_pds <- list()
 for (k in seq_along(grp_mats)) {
 new_cols <- setdiff(colnames(grp_mats[[k]]), seen)
 if (length(new_cols) > 0) {
 keep_mats[[length(keep_mats) + 1]] <- grp_mats[[k]][, new_cols, drop = FALSE]
 keep_pds[[length(keep_pds) + 1]] <- grp_pds[[k]][new_cols, , drop = FALSE]
 seen <- c(seen, new_cols)
 }
 }
 grp_mats <- keep_mats; grp_pds <- keep_pds
 }
 comb_mat <- do.call(cbind, grp_mats)
 comb_pd <- do.call(rbind, grp_pds)
 n_used <- length(grp_mats)
 } else {
 # Rows differ - fall back to the single largest matrix
 nf <- vapply(grp_mats, nrow, integer(1))
 best_k <- which.max(nf)
 comb_mat <- grp_mats[[best_k]]
 comb_pd <- grp_pds[[best_k]]
 n_used <- 1L
 }
 }

 n_samp <- ncol(comb_mat)
 if (n_samp > best_n_samp) {
 best_eset <- micro_data[[grp_idx[1]]] # template for rowData / annotation
 best_expr_mat <- comb_mat
 best_pdata <- comb_pd
 best_plt_id <- plt
 best_n_samp <- n_samp
 n_files_used <- n_used
 }
 }

    if (is.null(best_expr_mat)) {
      # Fallback: sometimes GEO does not provide a parseable matrix object
      # but supplies processed series_matrix or other text files as supplementary
      # files. Try to download supplementary files and parse any candidate table.
      try({
        .gexpipe_geo_quiet(
          GEOquery::getGEOSuppFiles(gse_id, baseDir = micro_dir, makeDirectory = TRUE, fetch_files = TRUE)
        )
      }, silent = TRUE)

      search_roots <- unique(c(micro_dir, file.path(micro_dir, gse_id)))
      files <- unique(unlist(lapply(search_roots, function(d) {
        if (dir.exists(d)) list.files(d, full.names = TRUE, recursive = TRUE) else character(0)
      })))
 # Prefer series_matrix files if present
 cand <- files[grepl("series_matrix.*\\.txt|series_matrix.*\\.gz|_matrix.*\\.txt|_matrix.*\\.gz", files, ignore.case = TRUE)]
 # otherwise look for reasonably sized text-like files
 if (length(cand) == 0) {
 txts <- files[grepl("\\.txt$|\\.tsv$|\\.csv$|\\.gz$", files, ignore.case = TRUE)]
 # pick those > 1KB
 cand <- txts[file.info(txts)$size > 1024]
 }
 parsed_ok <- FALSE
 for (cf in cand) {
 try({
 tf <- cf
 if (grepl("\\.gz$", tf, ignore.case = TRUE)) {
 con <- gzfile(tf, open = "rt")
 } else {
 con <- file(tf, open = "rt")
 }
 hdr_lines <- readLines(con, n = 50, warn = FALSE)
 close(con)
 # strip comment lines starting with '!' (GEO series_matrix style)
 content <- paste(hdr_lines, collapse = "\n")
 # try to read with read.table skipping '!' comment lines
            df <- tryCatch({
              utils::read.table(tf, header = TRUE, sep = "\t", comment.char = "!", quote = "\"", fill = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
            }, error = function(e) NULL)
 if (is.null(df) || nrow(df) == 0) next
 # find numeric columns (likely expression data)
 num_cols <- vapply(df, .gexpipe_col_looks_numeric, logical(1))
 if (sum(num_cols) < 1) next
 # assume first column is probe/gene id, numeric columns are samples
 probe_col <- which(!num_cols)[1]
 if (is.na(probe_col)) probe_col <- 1
 probes <- as.character(df[[probe_col]])
 expr_mat <- as.matrix(df[, which(num_cols), drop = FALSE])
 rownames(expr_mat) <- make.names(probes, unique = TRUE)
 if (nrow(expr_mat) > 0 && ncol(expr_mat) > 0) {
 best_eset <- NULL
 best_expr_mat <- expr_mat
 best_pdata <- data.frame(row.names = colnames(expr_mat))
 best_plt_id <- ""
 parsed_ok <- TRUE
 break
 }
 }, silent = TRUE)
 }
 if (!parsed_ok) {
 return(structure(list(ok = FALSE, reason = "No valid expression matrix found in GEO object"), class = "eset_err"))
 }
 }

 eset <- best_eset
 plt_id <- best_plt_id
 expr_mat <- best_expr_mat
 pd <- best_pdata
 log_pfx <- if (length(micro_data) > 1) {
 file_note <- if (n_files_used > 1) paste0(" (", n_files_used, " matrix files combined)") else ""
 paste0("Platforms: ", paste(unique_plts, collapse = ", "),
 ". Using ", plt_id, file_note, ". ")
 } else {
 paste0("Platform ", plt_id, ". ")
 }

 } else if (!is.null(micro_data)) {
 eset <- micro_data
 plt_id <- tryCatch(.gexpipe_geo_annotation(eset), error = function(e) "")
 expr_mat <- tryCatch(.gexpipe_geo_expr_matrix(eset), error = function(e) NULL)
 pd <- tryCatch(.gexpipe_geo_pdata(eset), error = function(e) NULL)
 log_pfx <- paste0("Platform ", plt_id, ". ")
 } else {
 return(structure(list(ok = FALSE, reason = "getGEO returned NULL or empty"), class = "eset_err"))
 }
 if (is.null(expr_mat)) expr_mat <- tryCatch(.gexpipe_geo_expr_matrix(eset), error = function(e) NULL)
 if (is.null(pd)) pd <- tryCatch(.gexpipe_geo_pdata(eset), error = function(e) NULL)
 if (is.null(expr_mat) || nrow(expr_mat) == 0 || ncol(expr_mat) == 0) {
 return(structure(list(ok = FALSE, reason = "GEO object has empty expression matrix"), class = "eset_err"))
 }
 list(ok = TRUE, eset = eset, platform_id = plt_id, log_pfx = log_pfx,
 expr_mat = expr_mat, pdata = pd)
 }, error = function(e) {
 list(ok = FALSE, reason = paste0("GEO object parse error: ", substr(conditionMessage(e), 1L, 120L)))
 })

 if (inherits(eset_parse, "eset_err") || !isTRUE(eset_parse$ok)) {
 out$reason <- if (inherits(eset_parse, "eset_err")) eset_parse$reason else eset_parse$reason
 return(out)
 }

 micro_eset <- eset_parse$eset
 platform_id <- eset_parse$platform_id
 out$log <- eset_parse$log_pfx
 micro_expr <- eset_parse$expr_mat
 pdata <- eset_parse$pdata
 if (is.null(pdata)) pdata <- data.frame(row.names = colnames(micro_expr))
 # Supplementary-only parses can yield empty pData; expand/enrich for group UI.
 if (!is.null(pdata) && is.data.frame(pdata) && ncol(pdata) > 0L) {
 pdata <- gexp_expand_geo_characteristics(
 as.data.frame(pdata, stringsAsFactors = FALSE, check.names = FALSE)
 )
 }
 # Phenotype: expand characteristics from first getGEO result; full NCBI
 # enrichment is deferred to the Groups tab when fast_download is enabled.
 sample_ids <- if (!is.null(micro_expr)) colnames(micro_expr) else NULL
 if (!isTRUE(fast)) {
 if (is.null(pdata) || !is.data.frame(pdata) || ncol(pdata) <= 1L) {
 full <- tryCatch(gexp_fetch_full_pdata(gse_id, sample_ids = sample_ids), error = function(e) NULL)
 if (!is.null(full) && is.data.frame(full) && ncol(full) > 0L) {
 if (is.null(pdata) || !is.data.frame(pdata) || nrow(pdata) == 0L) {
 pdata <- full
 } else {
 pdata <- .gexpipe_align_pdata_to_primary(pdata, full)
 }
 } else {
 pdata <- tryCatch(
 gexp_enrich_pdata_columns(
 if (is.null(pdata) || !is.data.frame(pdata) || nrow(pdata) == 0L) {
 data.frame(row.names = colnames(micro_expr), stringsAsFactors = FALSE)
 } else {
 pdata
 },
 gse_id,
 force = TRUE
 ),
 error = function(e) pdata
 )
 }
 } else {
 pdata <- tryCatch(
 gexp_enrich_pdata_columns(pdata, gse_id),
 error = function(e) pdata
 )
 }
 } else if (is.null(pdata) || !is.data.frame(pdata) || nrow(pdata) == 0L) {
 pdata <- data.frame(
 title = sample_ids,
 row.names = sample_ids,
 stringsAsFactors = FALSE
 )
 }
 if (!is.null(pdata) && is.data.frame(pdata) && ncol(pdata) > 0L) {
 out$log <- paste0(out$log, "(phenodata columns: ", ncol(pdata), ") ")
 }

 # ---- NA-sample detection and removal ----
 # Some GEO samples have all-NA or mostly-NA expression values (failed
 # hybridisations, placeholder columns, corrupt matrix files). Detect
 # them, remove them, and report to the user so sample counts stay clear.
 n_total_samp <- ncol(micro_expr)
 na_frac_per_sample <- colMeans(is.na(micro_expr) | !is.finite(micro_expr))
 # Flag samples that are >90% NA/NaN/Inf
 bad_samples <- names(which(na_frac_per_sample > 0.90))
 na_log <- ""
 if (length(bad_samples) > 0) {
 na_log <- paste0(
 " | ", length(bad_samples), "/", n_total_samp,
 " sample(s) removed (>90% NA): ",
 paste(head(bad_samples, 5), collapse = ", "),
 if (length(bad_samples) > 5) paste0(" (+", length(bad_samples) - 5, " more)") else ""
 )
 good_samples <- setdiff(colnames(micro_expr), bad_samples)
 micro_expr <- micro_expr[, good_samples, drop = FALSE]
 pdata <- pdata[intersect(rownames(pdata), good_samples), , drop = FALSE]
 }

 # Also count & report partially-NA samples (10-90% NA) for user awareness
 if (ncol(micro_expr) > 0) {
 partial_na <- colMeans(is.na(micro_expr) | !is.finite(micro_expr))
 partial_bad <- names(which(partial_na > 0.10 & partial_na <= 0.90))
 if (length(partial_bad) > 0) {
 na_log <- paste0(na_log,
 " | ", length(partial_bad), " sample(s) have 10-90% NA values (",
 paste(head(partial_bad, 3), collapse = ", "),
 if (length(partial_bad) > 3) paste0(" +", length(partial_bad) - 3, " more") else "",
 ")")
 }
 }

 if (ncol(micro_expr) == 0) {
 out$reason <- "all samples were NA - no usable data"
 return(out)
 }

 out$log <- paste0(out$log, "Downloaded: ", nrow(micro_expr), " genes x ",
 ncol(micro_expr), " samples",
 if (length(bad_samples) > 0) paste0(" (of ", n_total_samp, " total)") else "",
 na_log)

 cel <- character(0)
 if (isTRUE(download_cel)) {
 tryCatch(
 {
 .gexpipe_geo_quiet(
 GEOquery::getGEOSuppFiles(gse_id, baseDir = micro_dir, makeDirectory = TRUE, fetch_files = TRUE)
 )
 supp_dir <- file.path(micro_dir, gse_id)
 if (!dir.exists(supp_dir)) supp_dir <- micro_dir
 files <- list.files(supp_dir, full.names = TRUE, recursive = TRUE)
 tar_files <- files[grepl("\\.tar$|\\.zip$", files, ignore.case = TRUE)]
 for (tf in tar_files) {
 tryCatch(
 {
 if (grepl("\\.zip$", tf, ignore.case = TRUE)) utils::unzip(tf, exdir = supp_dir) else utils::untar(tf, exdir = supp_dir, tar = "internal")
 },
 error = function(e) NULL
 )
 }
 files <- list.files(supp_dir, full.names = TRUE, recursive = TRUE)
 cel <- files[grepl("\\.cel$", files, ignore.case = TRUE)]
 },
 error = function(e) NULL
 )
 }

 if (length(cel) > 0) {
 out$log <- paste0(out$log, ". CEL: ", length(cel), " files (RMA available)")
 }

 out$ok <- TRUE
 out$micro_expr <- micro_expr
 out$metadata <- pdata
 out$micro_eset <- micro_eset
 out$platform_id <- platform_id
 out$cel_paths <- cel
 out
}

#' Quick sample count from getGEO (no phenodata enrichment)
#' @keywords internal
.gexpipe_getgeo_sample_count <- function(gse_id) {
 tryCatch({
 gse_list <- .gexpipe_getgeo_series(gse_id)
 parts <- if (inherits(gse_list, "list")) gse_list else list(gse_list)
 ns <- vapply(parts, function(x) {
 pd <- .gexpipe_geo_pdata(x)
 if (is.null(pd)) 0L else nrow(pd)
 }, integer(1))
 n <- sum(ns)
 if (n > 0L) n else NA_integer_
 }, error = function(e) NA_integer_)
}

#' Minimum gene rows for a plausible RNA-seq count matrix
#' @keywords internal
.gexpipe_rnaseq_min_genes <- function() {
 as.integer(getOption("gexpipe.rnaseq_min_genes", 500L))
}

#' Is a tabular count candidate too small to be a gene x sample matrix?
#' @keywords internal
.gexpipe_rnaseq_count_df_invalid <- function(count_df) {
 min_genes <- .gexpipe_rnaseq_min_genes()
 is.null(count_df) || !is.data.frame(count_df) ||
 ncol(count_df) < 2L || nrow(count_df) < min_genes
}

#' Load a count table from file path
#' @keywords internal
.gexpipe_rnaseq_read_count_df <- function(path) {
 if (is.null(path) || !nzchar(path) || !file.exists(path)) {
 return(NULL)
 }
 if (.gexpipe_file_looks_like_html(path)) {
 return(NULL)
 }
 tryCatch(read_count_matrix(path), error = function(e) .gexpipe_fread_counts(path))
}

#' Preview nrow/ncol of a tabular count file
#' @param path File path.
#' @param min_genes Minimum gene rows required (default: RNA-seq threshold).
#' @keywords internal
.gexpipe_preview_count_file <- function(path, min_genes = NULL) {
 if (is.null(min_genes)) {
 min_genes <- .gexpipe_rnaseq_min_genes()
 }
 tryCatch(
 {
 if (.gexpipe_file_looks_like_html(path)) {
 return(NULL)
 }
 df <- .gexpipe_fread_counts(path, nrows = 1e6)
 if (is.null(df) || ncol(df) < 2L || nrow(df) < min_genes) {
 return(NULL)
 }
 list(
 path = path,
 nrow = nrow(df),
 ncol = ncol(df),
 nsamp = ncol(df) - 1L
 )
 },
 error = function(e) NULL
 )
}

#' True if the first two bytes are gzip magic (1f 8b)
#' @keywords internal
.gexpipe_file_has_gzip_magic <- function(path) {
 if (is.null(path) || !nzchar(path) || !file.exists(path)) {
 return(FALSE)
 }
 isTRUE(tryCatch({
 con <- file(path, "rb")
 on.exit(close(con), add = TRUE)
 b <- readBin(con, what = "raw", n = 2L)
 length(b) >= 2L && identical(b[1:2], as.raw(c(0x1f, 0x8b)))
 }, error = function(e) FALSE))
}

#' Detect HTML/CAPTCHA pages saved instead of GEO tabular data
#' @keywords internal
.gexpipe_file_looks_like_html <- function(path) {
 if (is.null(path) || !nzchar(path) || !file.exists(path)) {
 return(FALSE)
 }
 is_gz_name <- grepl("\\.gz$", path, ignore.case = TRUE)
 use_gz <- is_gz_name && .gexpipe_file_has_gzip_magic(path)
 head <- tryCatch({
 con <- if (use_gz) gzfile(path, "r") else file(path, "r")
 on.exit(close(con), add = TRUE)
 readLines(con, n = 3L, warn = FALSE)
 }, error = function(e) character(0))
 if (length(head) == 0L) {
 return(FALSE)
 }
 any(grepl("^<!doctype html>|^<html|recaptcha|challengepage", head, ignore.case = TRUE))
}

#' Score a count-file candidate (higher is better)
#' @keywords internal
.gexpipe_score_count_candidate <- function(info, n_meta = NA_integer_) {
 if (is.null(info)) {
 return(-Inf)
 }
 ngenes <- info$nrow
 if (!is.finite(ngenes) || ngenes < .gexpipe_rnaseq_min_genes()) {
 return(-Inf)
 }
 nsamp <- info$nsamp
 base_name <- if (length(info$path) == 0L) "" else basename(info$path[[1]])
 is_matrix_name <- grepl("matrix", base_name, ignore.case = TRUE)
 samp_match <- if (!is.na(n_meta) && n_meta > 0L) {
 -abs(nsamp - n_meta) * 1e6 + nsamp * 1e3
 } else {
 nsamp * 1e3
 }
 matrix_bonus <- if (is_matrix_name) 1e4 else 0L
 # Files named "normalized"/FPKM/TPM/rlog hold continuous (often log-scale)
 # values, which count-based DE engines cannot use. Break ties towards the
 # raw table whenever a series ships both.
 scale_bonus <- if (.gexpipe_is_normalized_count_name(base_name)) {
 -5e4
 } else if (grepl("raw", base_name, ignore.case = TRUE)) {
 5e4
 } else {
 0L
 }
 samp_match + matrix_bonus + scale_bonus + ngenes
}

#' Does a count-file name indicate normalized (non-count) values?
#' @keywords internal
.gexpipe_is_normalized_count_name <- function(x) {
 grepl(
 "normali[sz]ed|fpkm|tpm|rpkm|[^a-z]cpm|vst|rlog|log2|z[-_]?score",
 basename(x), ignore.case = TRUE
 )
}

#' Pick the best count file from candidates using sample count then gene rows
#' @keywords internal
.gexpipe_pick_best_count_file <- function(paths, n_meta = NA_integer_) {
 paths <- unique(paths[nzchar(paths)])
 if (length(paths) == 0L) {
 return(NULL)
 }
 best_path <- NULL
 best_score <- -Inf
 for (p in paths) {
 info <- .gexpipe_preview_count_file(p)
 score <- .gexpipe_score_count_candidate(info, n_meta)
 if (score > best_score) {
 best_score <- score
 best_path <- p
 }
 }
 best_path
}

#' Collect supplementary count-file candidates for one GSE directory
#' @keywords internal
.gexpipe_collect_supp_count_candidates <- function(files) {
 files <- files[!grepl("\\.tar$", files, ignore.case = TRUE)]
 patterns <- c(
 "count", "raw", "matrix", "htseq", "htcount", "reads",
 "gene", "expr", "rna", "featurecount", "read.count", "counts_mat"
 )
 candidates <- character(0)
 for (pattern in patterns) {
 matches <- files[grepl(pattern, basename(files), ignore.case = TRUE)]
 matches <- matches[!grepl("series_matrix", basename(matches), ignore.case = TRUE)]
 candidates <- c(candidates, matches)
 }
 unique(candidates)
}

#' List all files related to one GSE from both cache locations
#' @keywords internal
.gexpipe_list_gse_related_files <- function(gse_dir, rna_dir, gse_id) {
 files_gse <- list.files(gse_dir, full.names = TRUE, recursive = TRUE)
 files_rna <- list.files(rna_dir, full.names = TRUE, recursive = TRUE)
 files_rna <- files_rna[grepl(gse_id, basename(files_rna), ignore.case = TRUE)]
 unique(c(files_gse, files_rna))
}

#' Detect per-sample HTSeq/count files that can be merged
#' @keywords internal
.gexpipe_find_per_sample_count_files <- function(files) {
 candidates <- .gexpipe_collect_supp_count_candidates(files)
 candidates <- candidates[
 !grepl("matrix", basename(candidates), ignore.case = TRUE)
 ]
 per_sample <- character(0)
 for (cand in candidates) {
 info <- .gexpipe_preview_count_file(cand)
 if (!is.null(info) && info$nsamp == 1L && info$nrow >= .gexpipe_rnaseq_min_genes()) {
 per_sample <- c(per_sample, cand)
 }
 }
 unique(per_sample)
}

#' Broad scan for per-sample two-column count files (any basename)
#' @keywords internal
.gexpipe_find_per_sample_count_files_broad <- function(files, min_genes = 100L) {
 files <- files[!grepl(
 "\\.tar$|series_matrix|\\.cel$|\\.bed$|\\.bw$|\\.bam$|\\.bai$|\\.fq|\\.fastq",
 files, ignore.case = TRUE
 )]
 files <- files[grepl("\\.(txt|tsv|csv|tab)(\\.gz)?$", files, ignore.case = TRUE)]
 if (length(files) == 0L) {
 return(character(0))
 }
 sizes <- file.info(files)$size
 files <- files[is.finite(sizes) & sizes > 200L & sizes < 5e7]
 per_sample <- character(0)
 max_scan <- as.integer(getOption("gexpipe.rnaseq_max_broad_scan", 80L))
 if (length(files) > max_scan) {
 files <- files[seq_len(max_scan)]
 }
 for (cand in files) {
 info <- .gexpipe_preview_count_file(cand)
 if (!is.null(info) && info$nsamp == 1L && info$nrow >= min_genes) {
 per_sample <- c(per_sample, cand)
 }
 }
 unique(per_sample)
}

#' Detect count files that each cover only part of a series
#'
#' Some series split processed counts by group instead of shipping one table
#' (e.g. GSE114007 provides OA and normal counts separately). Such files each
#' hold several samples, so they are invisible to the per-sample merge.
#'
#' @param files Candidate file paths from the GSE download directory.
#' @param n_meta Expected sample count from GEO metadata.
#' @return Character vector of partial count files.
#' @keywords internal
.gexpipe_find_partial_count_files <- function(files, n_meta = NA_integer_) {
 if (is.na(n_meta) || n_meta <= 1L) {
 return(character(0))
 }
 candidates <- .gexpipe_collect_supp_count_candidates(files)
 candidates <- candidates[!grepl("matrix", basename(candidates), ignore.case = TRUE)]
 partial <- character(0)
 for (cand in candidates) {
 info <- .gexpipe_preview_count_file(cand)
 if (!is.null(info) && info$nsamp >= 1L && info$nsamp < n_meta && info$nrow >= 10L) {
 partial <- c(partial, cand)
 }
 }
 unique(partial)
}

#' Merge count files that each hold a subset of samples
#'
#' Joins on the gene column and keeps genes shared by every file. Columns
#' already present are skipped, so files describing the same samples twice
#' cannot inflate the sample count.
#'
#' @param paths Character vector of count file paths.
#' @return data.frame (gene column first) or NULL.
#' @keywords internal
.gexpipe_merge_count_files <- function(paths) {
 paths <- unique(paths[nzchar(paths)])
 if (length(paths) < 2L) {
 return(NULL)
 }
 merged <- NULL
 for (p in paths) {
 df <- tryCatch(.gexpipe_fread_counts(p), error = function(e) NULL)
 if (is.null(df) || ncol(df) < 2L || nrow(df) < 10L) {
 next
 }
 df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
 piece <- df[, -1L, drop = FALSE]
 if (ncol(piece) == 1L) {
 nm <- sub("\\.[^.]+(\\.gz)?$", "", basename(p))
 colnames(piece) <- sub("\\.htseq-count$", "", nm, ignore.case = TRUE)
 }
 piece <- cbind(gene = as.character(df[[1L]]), piece, stringsAsFactors = FALSE)
 piece <- piece[!duplicated(piece$gene) & !is.na(piece$gene) & nzchar(piece$gene), , drop = FALSE]
 if (is.null(merged)) {
 merged <- piece
 next
 }
 new_cols <- setdiff(colnames(piece)[-1L], colnames(merged))
 if (length(new_cols) == 0L) {
 next
 }
 merged <- merge(
 merged,
 piece[, c("gene", new_cols), drop = FALSE],
 by = "gene", all = FALSE
 )
 }
 if (is.null(merged) || ncol(merged) < 3L || nrow(merged) < 10L) {
 return(NULL)
 }
 as.data.frame(merged, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Merge per-sample count files (genes x one sample each) into one matrix
#' @keywords internal
.gexpipe_merge_per_sample_count_files <- function(paths) {
 paths <- unique(paths)
 if (length(paths) < 2L) {
 return(NULL)
 }
 merged <- NULL
 for (p in paths) {
 df <- tryCatch(.gexpipe_fread_counts(p), error = function(e) NULL)
 if (is.null(df) || ncol(df) < 2L) {
 next
 }
 gene_col <- as.character(df[[1]])
 counts <- as.numeric(df[[2]])
 samp_name <- sub("\\.[^.]+(\\.gz)?$", "", basename(p))
 samp_name <- sub("\\.htseq-count$", "", samp_name, ignore.case = TRUE)
 piece <- data.frame(gene = gene_col, count = counts, stringsAsFactors = FALSE)
 colnames(piece)[2] <- samp_name
 # Duplicate gene keys (e.g. Excel-date-corrupted symbols like MARCH1 -> "01-Mar",
 # repeated across files) make merge(..., all = TRUE) emit one row per key pair
 # (n_x * n_y), which compounds multiplicatively across dozens of per-sample
 # files and can exhaust memory within a few iterations. Drop dup/blank keys
 # per piece first, same as .gexpipe_merge_count_files() does.
 piece <- piece[!duplicated(piece$gene) & !is.na(piece$gene) & nzchar(piece$gene), , drop = FALSE]
 if (is.null(merged)) {
 merged <- piece
 } else {
 merged <- merge(merged, piece, by = "gene", all = TRUE)
 }
 }
 if (is.null(merged) || ncol(merged) < 3L) {
 return(NULL)
 }
 gene_col <- merged$gene
 merged$gene <- NULL
 out <- cbind(gene = gene_col, merged, stringsAsFactors = FALSE)
 as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Fetch GEO metadata early for expected sample count
#' @keywords internal
.gexpipe_fetch_rnaseq_metadata_early <- function(gse_id) {
 # Universal full-pdata path (getGEO -> characteristics expand -> series-matrix).
 pheno <- tryCatch(gexp_fetch_full_pdata(gse_id), error = function(e) NULL)
 if (is.null(pheno) || !is.data.frame(pheno) || nrow(pheno) == 0L) {
 return(NULL)
 }
 pheno
}

#' Choose between GEO supplementary and NCBI count matrices
#' @keywords internal
.gexpipe_choose_supp_or_ncbi <- function(supp_file, ncbi_file, n_meta = NA_integer_) {
 min_genes <- .gexpipe_rnaseq_min_genes()
 supp_info <- if (!is.null(supp_file) && nzchar(supp_file)) {
 .gexpipe_preview_count_file(supp_file, min_genes = 10L)
 } else {
 NULL
 }
 if (!is.null(supp_info) && supp_info$nrow < min_genes) {
 supp_file <- NULL
 supp_info <- NULL
 }
 ncbi_info <- if (!is.null(ncbi_file) && nzchar(ncbi_file) && file.exists(ncbi_file) &&
 !.gexpipe_file_looks_like_html(ncbi_file)) {
 .gexpipe_preview_count_file(ncbi_file, min_genes = min_genes)
 } else {
 NULL
 }
 if (is.null(supp_file) || !nzchar(supp_file)) {
 return(list(file = ncbi_file, source = "NCBI", info = ncbi_info))
 }
 if (is.null(ncbi_file) || !nzchar(ncbi_file) || is.null(ncbi_info)) {
 return(list(file = supp_file, source = "GEO supp", info = supp_info))
 }
 supp_score <- .gexpipe_score_count_candidate(supp_info, n_meta)
 ncbi_score <- .gexpipe_score_count_candidate(ncbi_info, n_meta)
 if (ncbi_score > supp_score) {
 list(file = ncbi_file, source = "NCBI", info = ncbi_info)
 } else {
 list(file = supp_file, source = "GEO supp", info = supp_info)
 }
}

#' Downloaded file is large enough and not an HTML/captcha stand-in
#' @keywords internal
.gexpipe_downloaded_file_ok <- function(path) {
 if (is.null(path) || !nzchar(path) || !file.exists(path)) {
 return(FALSE)
 }
 sz <- file.info(path)$size
 if (!is.finite(sz) || sz < 1000) {
 return(FALSE)
 }
 if (isTRUE(.gexpipe_file_looks_like_html(path))) {
 return(FALSE)
 }
 if (grepl("\\.gz$", path, ignore.case = TRUE) &&
 !isTRUE(.gexpipe_file_has_gzip_magic(path))) {
 return(FALSE)
 }
 TRUE
}

#' Download a URL to dest_file; drop HTML/captcha caches; prefer libcurl.
#' @keywords internal
.gexpipe_download_binary_url <- function(url, dest_file) {
 if (is.null(url) || !nzchar(url) || is.null(dest_file) || !nzchar(dest_file)) {
 return(NULL)
 }
 if (.gexpipe_downloaded_file_ok(dest_file)) {
 return(dest_file)
 }
 if (file.exists(dest_file)) {
 try(unlink(dest_file), silent = TRUE)
 }
 dir.create(dirname(dest_file), showWarnings = FALSE, recursive = TRUE)
 old_to <- getOption("timeout", 60L)
 if (old_to < 180L) {
 options(timeout = 180L)
 }
 on.exit(options(timeout = old_to), add = TRUE)
 methods <- unique(c("libcurl", getOption("download.file.method"), "auto", "wininet"))
 methods <- methods[!is.na(methods) & nzchar(as.character(methods))]
 for (method in methods) {
 tryCatch(
 utils::download.file(url, dest_file, mode = "wb", quiet = TRUE, method = method),
 error = function(e) NULL,
 warning = function(w) invokeRestart("muffleWarning")
 )
 if (.gexpipe_downloaded_file_ok(dest_file)) {
 return(dest_file)
 }
 if (file.exists(dest_file)) {
 try(unlink(dest_file), silent = TRUE)
 }
 }
 NULL
}

#' NCBI uniform RNA-seq counts URL from the GEO download page
#' @keywords internal
.gexpipe_ncbi_rnaseq_counts_url <- function(gse_id) {
 if (!requireNamespace("GEOquery", quietly = TRUE)) {
 return(NA_character_)
 }
 gq <- asNamespace("GEOquery")
 links <- tryCatch(gq$getGSEDownloadPageURLs(gse_id), error = function(e) NULL)
 if (is.null(links) || length(links) == 0L) {
 return(NA_character_)
 }
 if (exists("getRNAQuantRawCountsURL", envir = gq, inherits = FALSE)) {
 u <- tryCatch(gq$getRNAQuantRawCountsURL(links), error = function(e) NULL)
 if (!is.null(u) && length(u) > 0L && nzchar(as.character(u[[1L]]))) {
 return(as.character(u[[1L]]))
 }
 }
 links <- as.character(links)
 hit <- grep("type=rnaseq_counts.*raw_counts", links, ignore.case = TRUE, value = TRUE)
 if (length(hit) > 0L) {
 return(hit[[1L]])
 }
 NA_character_
}

#' Gene x sample numeric matrix from an NCBI counts data.frame
#' @keywords internal
.gexpipe_ncbi_counts_df_to_matrix <- function(df) {
 min_genes <- .gexpipe_rnaseq_min_genes()
 if (is.null(df) || !is.data.frame(df) || ncol(df) < 3L || nrow(df) < min_genes) {
 return(NULL)
 }
 gene_ids <- as.character(df[[1L]])
 mat <- as.matrix(df[, -1L, drop = FALSE])
 storage.mode(mat) <- "numeric"
 rownames(mat) <- gene_ids
 keep <- !is.na(rownames(mat)) & nzchar(trimws(rownames(mat)))
 mat <- mat[keep, , drop = FALSE]
 if (ncol(mat) < 2L || nrow(mat) < min_genes) {
 return(NULL)
 }
 mat
}

#' Fetch NCBI uniform RNA-seq counts (download-page TSV, not GEOquery row.names join)
#' @keywords internal
.gexpipe_fetch_ncbi_rnaseq_counts <- function(gse_id, dest_dir) {
 min_genes <- .gexpipe_rnaseq_min_genes()
 mat <- NULL
 log_pfx <- ""
 dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)

 # Fast path: current NCBI human GRCh38.p13 filename (GSE50760 and most human RNA-seq)
 p13_name <- paste0(gse_id, "_raw_counts_GRCh38.p13_NCBI.tsv.gz")
 p13_url <- paste0(
 "https://www.ncbi.nlm.nih.gov/geo/download/?type=rnaseq_counts&acc=",
 gse_id, "&format=file&file=", p13_name
 )
 p13_path <- tryCatch(
 .gexpipe_download_binary_url(p13_url, file.path(dest_dir, p13_name)),
 error = function(e) NULL
 )
 mat <- .gexpipe_ncbi_counts_df_to_matrix(.gexpipe_rnaseq_read_count_df(p13_path))
 if (!is.null(mat)) {
 log_pfx <- "(NCBI rnaseq_counts GRCh38.p13) "
 }

 ncbi_url <- if (is.null(mat)) {
 tryCatch(.gexpipe_ncbi_rnaseq_counts_url(gse_id), error = function(e) NA_character_)
 } else {
 NA_character_
 }
 if (length(ncbi_url) != 1L || is.null(ncbi_url) || is.na(ncbi_url)) {
 ncbi_url <- NA_character_
 }
 ncbi_url <- as.character(ncbi_url)
 if (!is.na(ncbi_url) && nzchar(ncbi_url)) {
 file_param <- sub(".*[?&]file=", "", ncbi_url)
 file_param <- sub("&.*$", "", file_param)
 file_param <- tryCatch(utils::URLdecode(file_param), error = function(e) file_param)
 file_param <- basename(file_param)
 if (!nzchar(file_param) || grepl("^https?:", file_param)) {
 file_param <- paste0(gse_id, "_raw_counts_NCBI.tsv.gz")
 }
 dest <- file.path(dest_dir, file_param)
 path <- tryCatch(.gexpipe_download_binary_url(ncbi_url, dest), error = function(e) NULL)
 df <- .gexpipe_rnaseq_read_count_df(path)
 mat <- .gexpipe_ncbi_counts_df_to_matrix(df)
 if (!is.null(mat)) {
 log_pfx <- "(NCBI rnaseq_counts URL) "
 }
 }

 if (is.null(mat)) {
 ncbi_path <- tryCatch(download_ncbi_raw_counts_best(gse_id, dest_dir), error = function(e) NULL)
 if (!is.null(ncbi_path)) {
 df <- .gexpipe_rnaseq_read_count_df(ncbi_path)
 mat <- .gexpipe_ncbi_counts_df_to_matrix(df)
 if (is.null(mat) && !.gexpipe_rnaseq_count_df_invalid(df)) {
 oriented <- gexp_orient_count_dataframe(df, metadata = NULL)
 if (!is.null(oriented$matrix) &&
 ncol(oriented$matrix) >= 2L &&
 nrow(oriented$matrix) >= min_genes) {
 mat <- oriented$matrix
 }
 }
 if (!is.null(mat) && !nzchar(log_pfx)) {
 log_pfx <- "(NCBI raw_counts file) "
 }
 }
 }

 if (is.null(mat) && requireNamespace("GEOquery", quietly = TRUE)) {
 gq <- asNamespace("GEOquery")
 if (exists("getRNASeqQuantResults", envir = gq, inherits = FALSE)) {
 get_quants <- get("getRNASeqQuantResults", envir = gq)
 res <- tryCatch(get_quants(gse_id), error = function(e) NULL)
 if (!is.null(res) && !is.null(res$quants)) {
 mat_try <- tryCatch(.gexpipe_ncbi_counts_df_to_matrix(as.data.frame(res$quants)), error = function(e) NULL)
 if (is.null(mat_try)) {
 mat_try <- tryCatch({
 m <- as.matrix(res$quants)
 storage.mode(m) <- "numeric"
 if (ncol(m) >= 2L && nrow(m) >= min_genes) m else NULL
 }, error = function(e) NULL)
 }
 if (!is.null(mat_try)) {
 mat <- mat_try
 log_pfx <- paste0(log_pfx, "(GEOquery NCBI rnaseq_counts) ")
 }
 }
 }
 }

 if (is.null(mat) || ncol(mat) < 2L || nrow(mat) < min_genes) {
 return(NULL)
 }
 list(count_matrix = mat, metadata = NULL, log = log_pfx)
}

#' Finalize RNA-seq download: align samples, QC, optional enrich, return out list
#' @keywords internal
.gexpipe_rnaseq_finish <- function(gse_id, count_matrix, rna_metadata, out, fast = TRUE) {
 count_matrix <- gexp_align_rnaseq_sample_names(count_matrix, rna_metadata, gse_id)
 if (gexp_is_generic_sample_names(colnames(count_matrix))) {
 out$log <- paste0(out$log, "(generic sample names; limited GEO metadata) ")
 }
 n_total_samp_rna <- ncol(count_matrix)
 na_frac_rna <- colMeans(is.na(count_matrix) | !is.finite(count_matrix))
 bad_samp_rna <- names(which(na_frac_rna > 0.90))
 rna_na_log <- ""
 if (length(bad_samp_rna) > 0) {
 rna_na_log <- paste0(
 " | ", length(bad_samp_rna), "/", n_total_samp_rna,
 " sample(s) removed (>90% NA): ",
 paste(head(bad_samp_rna, 5), collapse = ", "),
 if (length(bad_samp_rna) > 5) paste0(" (+", length(bad_samp_rna) - 5, " more)") else ""
 )
 good_samp_rna <- setdiff(colnames(count_matrix), bad_samp_rna)
 count_matrix <- count_matrix[, good_samp_rna, drop = FALSE]
 }
 if (ncol(count_matrix) == 0) {
 out$reason <- "all samples were NA - no usable data"
 return(out)
 }
 out$log <- paste0(out$log, rna_na_log)
 if (is.null(rna_metadata) || nrow(rna_metadata) == 0L) {
 rna_metadata <- data.frame(
 title = colnames(count_matrix),
 row.names = colnames(count_matrix),
 stringsAsFactors = FALSE
 )
 } else {
 count_cols <- colnames(count_matrix)
 if (!all(count_cols %in% rownames(rna_metadata))) {
 outm <- as.data.frame(
 matrix(NA_character_, nrow = length(count_cols), ncol = ncol(rna_metadata)),
 stringsAsFactors = FALSE
 )
 colnames(outm) <- colnames(rna_metadata)
 rownames(outm) <- count_cols
 common_meta <- intersect(count_cols, rownames(rna_metadata))
 if (length(common_meta) > 0L) {
 outm[common_meta, ] <- rna_metadata[common_meta, , drop = FALSE]
 } else {
 n <- min(length(count_cols), nrow(rna_metadata))
 if (n > 0L) outm[seq_len(n), ] <- rna_metadata[seq_len(n), , drop = FALSE]
 }
 rna_metadata <- outm
 }
 common_samples <- intersect(colnames(count_matrix), rownames(rna_metadata))
 if (length(common_samples) > 0L) {
 rna_metadata <- rna_metadata[common_samples, , drop = FALSE]
 }
 }
 if (!isTRUE(fast) && (is.null(rna_metadata) || ncol(rna_metadata) <= 1L)) {
 primary_stub <- data.frame(
 title = colnames(count_matrix),
 row.names = colnames(count_matrix),
 stringsAsFactors = FALSE
 )
 enriched <- tryCatch(
 gexp_fetch_full_pdata(gse_id, sample_ids = rownames(primary_stub)),
 error = function(e) NULL
 )
 if (is.null(enriched) || ncol(enriched) <= 1L) {
 enriched <- tryCatch(
 gexp_enrich_pdata_columns(primary_stub, gse_id, force = TRUE),
 error = function(e) NULL
 )
 }
 if (!is.null(enriched) && ncol(enriched) > 0L) {
 rna_metadata <- .gexpipe_align_pdata_to_primary(primary_stub, enriched)
 out$log <- paste0(out$log, "(phenodata columns: ", ncol(rna_metadata), ") ")
 }
 } else if (!is.null(rna_metadata) && ncol(rna_metadata) > 0L) {
 out$log <- paste0(out$log, "(phenodata columns: ", ncol(rna_metadata), ") ")
 }
 if (!isTRUE(fast)) {
 gene_ids <- rownames(count_matrix)
 gene_symbols <- convert_rnaseq_ids(gene_ids, gse_id)
 rownames(count_matrix) <- gene_symbols
 valid <- !is.na(gene_symbols) & trimws(gene_symbols) != ""
 count_matrix <- count_matrix[valid, , drop = FALSE]
 if (nrow(count_matrix) == 0) {
 out$reason <- "no genes after ID mapping"
 return(out)
 }
 if (any(duplicated(rownames(count_matrix)))) {
 count_matrix <- limma::avereps(count_matrix, ID = rownames(count_matrix))
 }
 }
 out$ok <- TRUE
 out$count_matrix <- count_matrix
 out$metadata <- rna_metadata
 out
}

#' Download and parse one RNA-seq GSE
#'
#' @param gse_id GEO series ID.
#' @param rna_dir Directory containing `rna_data`.
#' @param fast Logical; if `TRUE` (default from
#'   `options(gexpipe.fast_download)`), skip slower optional lookups.
#' @return List with status, reason, log text, count matrix, and metadata.
#'
#' @examples
#' if (interactive()) {
#' out <- gexp_download_one_rnaseq_gse("GSE114007", tempdir())
#' out$ok
#' }
#' @export
gexp_download_one_rnaseq_gse <- function(gse_id, rna_dir, fast = NULL) {
 if (is.null(fast)) {
 fast <- isTRUE(getOption("gexpipe.fast_download", TRUE))
 }
 out <- list(ok = FALSE, reason = NULL, log = "", count_matrix = NULL, metadata = NULL)
 gse_dir <- file.path(rna_dir, gse_id)
 dir.create(gse_dir, showWarnings = FALSE, recursive = TRUE)

 # 1) NCBI uniform counts FIRST (fast; avoids RAW.tar / hundreds of fread calls)
 ncbi_early <- tryCatch(
 .gexpipe_fetch_ncbi_rnaseq_counts(gse_id, gse_dir),
 error = function(e) e
 )
 if (inherits(ncbi_early, "error")) {
 out$log <- paste0(
 out$log, "(NCBI fetch error: ",
 substr(conditionMessage(ncbi_early), 1L, 160L), ") "
 )
 ncbi_early <- NULL
 }
 if (!is.null(ncbi_early)) {
 out$log <- paste0(out$log, ncbi_early$log)
 rna_metadata <- if (!isTRUE(fast)) {
 tryCatch(.gexpipe_fetch_rnaseq_metadata_early(gse_id), error = function(e) NULL)
 } else {
 NULL
 }
 md <- if (!is.null(ncbi_early$metadata)) ncbi_early$metadata else rna_metadata
 return(.gexpipe_rnaseq_finish(gse_id, ncbi_early$count_matrix, md, out, fast))
 }

 # 2) Pre-download NCBI count file before any supplementary / RAW.tar work
 ncbi_best <- tryCatch(download_ncbi_raw_counts_best(gse_id, gse_dir), error = function(e) NULL)
 if (!is.null(ncbi_best) && .gexpipe_file_looks_like_html(ncbi_best)) {
 file.remove(ncbi_best)
 ncbi_best <- tryCatch(download_ncbi_raw_counts_best(gse_id, gse_dir), error = function(e) NULL)
 }
 if (!is.null(ncbi_best) && !.gexpipe_file_looks_like_html(ncbi_best)) {
 ncbi_df <- .gexpipe_rnaseq_read_count_df(ncbi_best)
 if (!.gexpipe_rnaseq_count_df_invalid(ncbi_df)) {
 oriented_ncbi <- gexp_orient_count_dataframe(ncbi_df, metadata = NULL)
 if (!is.null(oriented_ncbi$matrix) &&
 ncol(oriented_ncbi$matrix) >= 2L &&
 nrow(oriented_ncbi$matrix) >= .gexpipe_rnaseq_min_genes()) {
 out$log <- paste0(out$log, "(NCBI raw_counts; skipped RAW.tar supp) ")
 rna_metadata <- if (!isTRUE(fast)) {
 tryCatch(.gexpipe_fetch_rnaseq_metadata_early(gse_id), error = function(e) NULL)
 } else {
 NULL
 }
 return(.gexpipe_rnaseq_finish(gse_id, oriented_ncbi$matrix, rna_metadata, out, fast))
 }
 }
 }

 # 3) Slow path: GEO supplementary (metadata helps pick/merge supp files)
 rna_metadata <- tryCatch(
 .gexpipe_fetch_rnaseq_metadata_early(gse_id),
 error = function(e) NULL
 )
 n_meta <- if (!is.null(rna_metadata) && nrow(rna_metadata) > 0L) {
 nrow(rna_metadata)
 } else {
 NA_integer_
 }
 if (is.na(n_meta)) {
 n_meta <- .gexpipe_getgeo_sample_count(gse_id)
 }

 supp_state <- new.env(parent = emptyenv())
 supp_state$err <- NULL
 count_file <- NULL
 count_df_merged <- NULL
 count_df_merged_src <- NULL
 tryCatch(
 {
 .gexpipe_geo_quiet(
 GEOquery::getGEOSuppFiles(gse_id, baseDir = dirname(gse_dir), makeDirectory = FALSE, fetch_files = TRUE)
 )
 files <- .gexpipe_list_gse_related_files(gse_dir, rna_dir, gse_id)

 tar_files <- files[grepl("\\.tar$", files, ignore.case = TRUE)]
 for (tar_file in tar_files) {
 tryCatch(
 {
 utils::untar(tar_file, exdir = gse_dir, tar = "internal")
 },
 error = function(e) {
 msg <- conditionMessage(e)
 if (grepl("truncated|corrupt|error|invalid", msg, ignore.case = TRUE)) {
 supp_state$err <- paste0("Truncated or corrupted tar archive (", basename(tar_file), "). Re-download or try another GSE.")
 } else {
 supp_state$err <- paste0("Untar failed: ", substr(msg, 1L, 120L))
 }
 },
 warning = function(w) {
 supp_state$err <- paste0("Tar archive problem (", basename(tar_file), "). File may be truncated or corrupted.")
 }
 )
 }
 files <- .gexpipe_list_gse_related_files(gse_dir, rna_dir, gse_id)

 matrix_candidates <- files[
 grepl("matrix\\.htseq-count(\\.txt)?(\\.gz)?$", basename(files), ignore.case = TRUE) |
 grepl("counts?_mat|count[_-]?matrix|raw[_-]?counts", basename(files), ignore.case = TRUE)
 ]
 if (length(matrix_candidates) > 0L) {
 count_file <- .gexpipe_pick_best_count_file(matrix_candidates, n_meta)
 if (!is.null(count_file)) {
 out$log <- paste0(out$log, "(preferred matrix.htseq-count file) ")
 }
 }

 if (is.null(count_file)) {
 candidates <- .gexpipe_collect_supp_count_candidates(files)
 count_file <- .gexpipe_pick_best_count_file(candidates, n_meta)
 }

 best_info <- if (!is.null(count_file)) .gexpipe_preview_count_file(count_file) else NULL
 need_merge <- !is.null(best_info) && (
 (!is.na(n_meta) && best_info$nsamp < n_meta) ||
 best_info$nsamp <= 1L
 )
 if (isTRUE(need_merge) || is.null(count_file)) {
 per_sample <- .gexpipe_find_per_sample_count_files(files)
 if (length(per_sample) < 2L) {
 per_sample <- .gexpipe_find_per_sample_count_files_broad(files)
 }
 if (length(per_sample) >= 2L) {
 merged_df <- .gexpipe_merge_per_sample_count_files(per_sample)
 merged_nsamp <- if (!is.null(merged_df)) ncol(merged_df) - 1L else 0L
 if (!is.null(merged_df) && merged_nsamp >= 2L &&
 (is.null(best_info) || merged_nsamp > best_info$nsamp)) {
 count_df_merged <- merged_df
 count_df_merged_src <- per_sample
 count_file <- NULL
 out$log <- paste0(out$log, "(merged ", merged_nsamp, " per-sample files) ")
 }
 }
 # Counts split by group (e.g. GSE114007 OA + normal) hold several
 # samples each, so combine them into the full series matrix.
 if (is.null(count_df_merged)) {
 partial <- .gexpipe_find_partial_count_files(files, n_meta)
 if (length(partial) >= 2L) {
 merged_df <- .gexpipe_merge_count_files(partial)
 merged_nsamp <- if (!is.null(merged_df)) ncol(merged_df) - 1L else 0L
 if (!is.null(merged_df) && merged_nsamp >= 2L &&
 (is.null(best_info) || merged_nsamp > best_info$nsamp)) {
 count_df_merged <- merged_df
 count_df_merged_src <- partial
 count_file <- NULL
 out$log <- paste0(
 out$log, "(merged ", length(partial), " partial count files -> ",
 merged_nsamp, " samples) "
 )
 }
 }
 }
 }
 },
 error = function(e) {
 supp_state$err <- conditionMessage(e)
 NULL
 }
 )

 if (!is.null(ncbi_best) && !is.null(count_file) && is.null(count_df_merged)) {
 ncbi_info_pre <- .gexpipe_preview_count_file(ncbi_best, min_genes = .gexpipe_rnaseq_min_genes())
 supp_info_pre <- if (!is.null(count_file)) {
 .gexpipe_preview_count_file(count_file, min_genes = 10L)
 } else {
 NULL
 }
 if (!is.null(ncbi_info_pre) && (is.null(supp_info_pre) ||
 supp_info_pre$nrow < .gexpipe_rnaseq_min_genes())) {
 count_file <- ncbi_best
 out$log <- paste0(out$log, "(using NCBI counts over small GEO supp) ")
 }
 }

 chosen <- .gexpipe_choose_supp_or_ncbi(count_file, ncbi_best, n_meta)
 if (!is.null(count_df_merged)) {
 # First column holds gene IDs, so it must not count as a sample
 merged_nsamp <- ncol(count_df_merged) - 1L
 merged_ngenes <- nrow(count_df_merged)
 # Score under a source file name so a merged set of normalized tables does
 # not outrank a genuine raw count matrix with the same sample count.
 merged_path <- if (length(count_df_merged_src) > 0) count_df_merged_src[[1]] else "merged"
 merged_score <- .gexpipe_score_count_candidate(
 list(path = merged_path, nrow = merged_ngenes, ncol = merged_nsamp + 1L, nsamp = merged_nsamp),
 n_meta
 )
 chosen_score <- .gexpipe_score_count_candidate(chosen$info, n_meta)
 if (merged_score >= chosen_score) {
 chosen <- list(file = NULL, source = "GEO supp merged", info = list(nsamp = merged_nsamp, nrow = merged_ngenes))
 } else {
 count_df_merged <- NULL
 }
 }
 if (!is.null(chosen$file)) {
 count_file <- chosen$file
 info <- chosen$info
 if (!is.null(info) && !is.null(info$nrow) && info$nrow < .gexpipe_rnaseq_min_genes()) {
 out$log <- paste0(
 out$log, "(rejected tiny GEO supp: ", info$nrow, " rows; need NCBI counts) "
 )
 count_file <- ncbi_best
 info <- if (!is.null(count_file)) .gexpipe_preview_count_file(count_file) else NULL
 }
 if (!is.null(info) && !is.null(info$nrow)) {
 out$log <- paste0(out$log, "(", chosen$source, " ", info$nrow, " rows, ", info$nsamp, " samples) ")
 }
 } else if (!is.null(count_df_merged)) {
 count_file <- NULL
 } else if (!is.null(ncbi_best)) {
 count_file <- ncbi_best
 }
 if (is.null(count_file) && is.null(count_df_merged)) {
 out$reason <- paste0(
 "no usable RNA-seq count matrix for ", gse_id,
 " (need gene x sample integer counts). Tried GEO supplementary files, ",
 "per-sample merge, and NCBI rnaseq_counts."
 )
 if (!is.null(supp_state$err) && nzchar(supp_state$err)) {
 out$reason <- if (grepl("timeout|timed out", supp_state$err, ignore.case = TRUE)) {
 "download timed out - this GSE's supplementary files may be large; your internet connection may be fine, just retry"
 } else if (grepl("connection|hostname|resolve|HTTP|ssl", supp_state$err, ignore.case = TRUE)) {
 "network/HTTP - check internet connection"
 } else if (grepl("truncated|corrupt|tar archive", supp_state$err, ignore.case = TRUE)) {
 "truncated/corrupted supplementary tar - try re-download or remove this GSE"
 } else {
 supp_state$err
 }
 }
 return(out)
 }

 count_df <- if (!is.null(count_df_merged)) {
 count_df_merged
 } else {
 .gexpipe_rnaseq_read_count_df(count_file)
 }
 # If GEO supplementary parse failed, try NCBI counts before giving up
 if (.gexpipe_rnaseq_count_df_invalid(count_df) && !is.null(ncbi_best) &&
 !identical(count_file, ncbi_best)) {
 ncbi_df <- .gexpipe_rnaseq_read_count_df(ncbi_best)
 if (!.gexpipe_rnaseq_count_df_invalid(ncbi_df)) {
 count_df <- ncbi_df
 count_file <- ncbi_best
 out$log <- paste0(out$log, "(NCBI rnaseq_counts fallback) ")
 }
 }
 # Hard safeguard: if selected table has too few sample columns versus metadata,
 # force-switch to matrix.htseq-count or NCBI candidate with more samples.
 if (!is.null(count_df) && !is.na(n_meta) && n_meta > 1L) {
 current_nsamp <- ncol(count_df) - 1L
 if (is.finite(current_nsamp) && current_nsamp < n_meta) {
 fallback_files <- .gexpipe_list_gse_related_files(gse_dir, rna_dir, gse_id)
 matrix_candidates <- fallback_files[
 grepl("matrix\\.htseq-count(\\.txt)?(\\.gz)?$", basename(fallback_files), ignore.case = TRUE)
 ]
 fallback_pick <- .gexpipe_pick_best_count_file(matrix_candidates, n_meta)
 fallback_source <- "matrix.htseq-count fallback"
 if (is.null(fallback_pick)) {
 fallback_pick <- ncbi_best
 fallback_source <- "NCBI fallback"
 }
 if (!is.null(fallback_pick) && nzchar(fallback_pick)) {
 fallback_df <- tryCatch(read_count_matrix(fallback_pick), error = function(e) .gexpipe_fread_counts(fallback_pick))
 fallback_nsamp <- if (!is.null(fallback_df)) ncol(fallback_df) - 1L else -1L
 if (!is.null(fallback_df) && fallback_nsamp > current_nsamp) {
 count_df <- fallback_df
 count_file <- fallback_pick
 out$log <- paste0(out$log, "(auto-switched: ", fallback_source, ", ", fallback_nsamp, " samples) ")
 }
 }
 }
 }
 if (.gexpipe_rnaseq_count_df_invalid(count_df)) {
 ncbi_retry <- tryCatch(
 .gexpipe_fetch_ncbi_rnaseq_counts(gse_id, gse_dir),
 error = function(e) NULL
 )
 if (!is.null(ncbi_retry)) {
 out$log <- paste0(out$log, "(NCBI fallback) ", ncbi_retry$log)
 md <- if (!is.null(ncbi_retry$metadata)) ncbi_retry$metadata else rna_metadata
 return(.gexpipe_rnaseq_finish(gse_id, ncbi_retry$count_matrix, md, out, fast))
 }
 out$reason <- paste0(
 "count file format invalid or too small for ", gse_id,
 " (need >= ", .gexpipe_rnaseq_min_genes(), " genes). ",
 "Tried NCBI rnaseq_counts and GEO supplementary. ",
 "[GExPipe ", as.character(utils::packageVersion("GExPipe")), "]"
 )
 return(out)
 }

 # Reuse metadata fetched early; refresh only if missing (skip extra getGEO in fast mode)
 if (!isTRUE(fast) && (is.null(rna_metadata) || nrow(rna_metadata) == 0L)) {
 rna_metadata <- tryCatch(
 {
 .gexpipe_fetch_rnaseq_metadata_early(gse_id)
 },
 error = function(e) NULL
 )
 }

 oriented <- gexp_orient_count_dataframe(count_df, metadata = rna_metadata)
 count_matrix <- oriented$matrix
 if (is.null(count_matrix) || ncol(count_matrix) < 2L ||
 nrow(count_matrix) < .gexpipe_rnaseq_min_genes()) {
 ncbi_retry <- tryCatch(
 .gexpipe_fetch_ncbi_rnaseq_counts(gse_id, gse_dir),
 error = function(e) NULL
 )
 if (!is.null(ncbi_retry)) {
 out$log <- paste0(out$log, "(NCBI orientation fallback) ", ncbi_retry$log)
 md <- if (!is.null(ncbi_retry$metadata)) ncbi_retry$metadata else rna_metadata
 return(.gexpipe_rnaseq_finish(gse_id, ncbi_retry$count_matrix, md, out, fast))
 }
 out$reason <- paste0(
 "count file format invalid or too small for ", gse_id,
 " after orientation. Supplementary files may be FPKM/TPM, not raw counts."
 )
 return(out)
 }
 if (nzchar(oriented$log)) {
 out$log <- paste0(out$log, oriented$log, " ")
 }

 return(.gexpipe_rnaseq_finish(gse_id, count_matrix, rna_metadata, out, fast))
}
