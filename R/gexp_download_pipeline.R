## GExPipe download-step helpers
##
## Small reusable helpers extracted from Shiny download module.

#' @importFrom utils capture.output
NULL

#' Parse GSE IDs from Shiny text inputs
#'
#' @param analysis_type One of "rnaseq", "microarray", or "merged".
#' @param rnaseq_gses Text field for RNA-seq IDs.
#' @param microarray_gses Text field for microarray IDs.
#' @param dataset_mode "single" or "multi".
#' @return List with rnaseq_ids, micro_ids, dataset_mode, and messages.
#'
#' @examples
#' x <- gexp_parse_gse_inputs(
#'   analysis_type = "merged",
#'   rnaseq_gses = "GSE1, GSE2",
#'   microarray_gses = "GSE3",
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
  rnaseq_ids <- character(0)
  micro_ids <- character(0)
  messages <- character(0)

  if (analysis_type %in% c("rnaseq", "merged")) {
    rnaseq_text <- gsub("\\s+", ",", rnaseq_gses)
    rnaseq_ids <- trimws(unlist(strsplit(rnaseq_text, ",")))
    rnaseq_ids <- rnaseq_ids[nzchar(rnaseq_ids)]
  }
  if (analysis_type %in% c("microarray", "merged")) {
    micro_text <- gsub("\\s+", ",", microarray_gses)
    micro_ids <- trimws(unlist(strsplit(micro_text, ",")))
    micro_ids <- micro_ids[nzchar(micro_ids)]
  }

  mode <- if (is.null(dataset_mode) || !nzchar(dataset_mode)) "multi" else dataset_mode
  if (identical(mode, "single")) {
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
    messages = messages
  )
}

#' Prepare clean download directories for current run
#'
#' @param base_dir Base working directory.
#' @param has_micro Logical; whether microarray IDs exist.
#' @param has_rna Logical; whether RNA-seq IDs exist.
#' @return Character vector log lines describing cleanup.
#'
#' @examples
#' td <- tempdir()
#' gexp_prepare_download_dirs(td, has_micro = TRUE, has_rna = TRUE)
#' @export
gexp_prepare_download_dirs <- function(base_dir = getwd(), has_micro = FALSE, has_rna = FALSE) {
  logs <- character(0)
  if (isTRUE(has_micro)) {
    micro_dir <- file.path(base_dir, "micro_data")
    if (dir.exists(micro_dir)) {
      tryCatch(unlink(micro_dir, recursive = TRUE, force = TRUE), error = function(e) NULL)
      logs <- c(logs, "Cleared previous microarray cache (micro_data).")
    }
    dir.create(micro_dir, showWarnings = FALSE, recursive = TRUE)
  }
  if (isTRUE(has_rna)) {
    rna_dir <- file.path(base_dir, "rna_data")
    if (dir.exists(rna_dir)) {
      tryCatch(unlink(rna_dir, recursive = TRUE, force = TRUE), error = function(e) NULL)
      logs <- c(logs, "Cleared previous RNA-seq cache (rna_data).")
    }
    dir.create(rna_dir, showWarnings = FALSE, recursive = TRUE)
  }
  logs
}

#' Finalize common genes and combined matrix after download/mapping
#'
#' @param micro_expr_list Named list of microarray matrices (genes x samples).
#' @param rna_counts_list Named list of RNA-seq matrices (genes x samples).
#' @param all_genes_list Named list of rowname vectors per dataset.
#' @return List with updated lists, common_genes, combined_expr_raw, and status.
#'
#' @examples
#' m1 <- matrix(1:12, nrow = 3, dimnames = list(c("A", "B", "C"), paste0("S", 1:4)))
#' m2 <- matrix(1:12, nrow = 3, dimnames = list(c("B", "C", "D"), paste0("T", 1:4)))
#' out <- gexp_download_finalize_common_genes(
#'   micro_expr_list = list(D1 = m1),
#'   rna_counts_list = list(D2 = m2),
#'   all_genes_list = list(D1 = rownames(m1), D2 = rownames(m2))
#' )
#' out$common_genes
#' @export
gexp_download_finalize_common_genes <- function(
  micro_expr_list,
  rna_counts_list,
  all_genes_list
) {
  if (length(all_genes_list) == 0) {
    return(list(
      ok = FALSE,
      common_genes = character(0),
      micro_expr_list = micro_expr_list,
      rna_counts_list = rna_counts_list,
      combined_expr_raw = NULL
    ))
  }

  common_genes <- Reduce(intersect, all_genes_list)

  for (gse in names(micro_expr_list)) {
    keep <- intersect(common_genes, rownames(micro_expr_list[[gse]]))
    micro_expr_list[[gse]] <- micro_expr_list[[gse]][keep, , drop = FALSE]
  }
  for (gse in names(rna_counts_list)) {
    keep <- intersect(common_genes, rownames(rna_counts_list[[gse]]))
    rna_counts_list[[gse]] <- rna_counts_list[[gse]][keep, , drop = FALSE]
  }

  combined_lists <- c(
    if (length(micro_expr_list) > 0) lapply(micro_expr_list, rownames) else list(),
    if (length(rna_counts_list) > 0) lapply(rna_counts_list, rownames) else list()
  )
  common_genes <- if (length(combined_lists) > 0) Reduce(intersect, combined_lists) else character(0)

  if (length(common_genes) == 0) {
    return(list(
      ok = FALSE,
      common_genes = character(0),
      micro_expr_list = micro_expr_list,
      rna_counts_list = rna_counts_list,
      combined_expr_raw = NULL
    ))
  }

  for (gse in names(micro_expr_list)) {
    micro_expr_list[[gse]] <- micro_expr_list[[gse]][common_genes, , drop = FALSE]
  }
  for (gse in names(rna_counts_list)) {
    rna_counts_list[[gse]] <- rna_counts_list[[gse]][common_genes, , drop = FALSE]
  }

  combined_expr_raw <- do.call(cbind, c(micro_expr_list, rna_counts_list))
  rownames(combined_expr_raw) <- common_genes

  list(
    ok = TRUE,
    common_genes = common_genes,
    micro_expr_list = micro_expr_list,
    rna_counts_list = rna_counts_list,
    combined_expr_raw = combined_expr_raw
  )
}

#' Fetch sample metadata from GEO series matrix fallback
#'
#' @param gse_id GEO series ID.
#' @return data.frame or NULL.
#'
#' @examples
#' if (interactive()) {
#'   md <- gexp_fetch_geo_series_matrix_metadata("GSE10072")
#'   if (!is.null(md)) head(md)
#' }
#' @export
gexp_fetch_geo_series_matrix_metadata <- function(gse_id) {
  conn <- NULL
  tryCatch({
    url_str <- sprintf(
      "https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/matrix/%s_series_matrix.txt.gz",
      substr(gse_id, 1, 3), gse_id, gse_id
    )
    conn <- url(url_str, open = "rb")
    raw_lines <- readLines(conn, warn = FALSE, encoding = "UTF-8")
    if (length(raw_lines) == 0) return(NULL)
    idx <- grep("^!sample_", raw_lines, ignore.case = TRUE)
    if (length(idx) == 0) return(NULL)
    lines <- strsplit(raw_lines[idx], "\t", fixed = TRUE)
    attr_names <- vapply(lines, function(x) sub("^!sample_", "", x[1], ignore.case = TRUE), character(1))
    n_samples <- max(vapply(lines, length, integer(1))) - 1L
    if (n_samples < 1) return(NULL)
    sample_ids <- lines[[1]][-1]
    sample_ids <- head(sample_ids, n_samples)
    out <- as.data.frame(
      matrix(NA_character_, nrow = length(sample_ids), ncol = length(attr_names)),
      stringsAsFactors = FALSE
    )
    colnames(out) <- make.names(attr_names, unique = TRUE)
    rownames(out) <- sample_ids
    for (j in seq_along(lines)) {
      vals <- lines[[j]][-1]
      n <- min(length(vals), nrow(out))
      if (n > 0) out[seq_len(n), j] <- vals[seq_len(n)]
    }
    out
  }, error = function(e) NULL, finally = {
    if (!is.null(conn)) try(close(conn), silent = TRUE)
  })
}

#' Build diagnostic log text when no common genes are found
#'
#' @param all_genes_list Named list of row IDs per dataset.
#' @return Character string for log appending.
#'
#' @examples
#' txt <- gexp_no_common_genes_diagnostic_log(
#'   list(D1 = c("1007_s_at", "1053_at"), D2 = c("ENSG000001", "ENSG000002"))
#' )
#' nchar(txt) > 0
#' @export
gexp_no_common_genes_diagnostic_log <- function(all_genes_list) {
  txt <- "\nNo common genes across datasets after converting to gene symbols.\n"
  txt <- paste0(txt, "Diagnostic: sample row IDs per dataset (to see if mapping to symbols failed):\n")
  for (gse in names(all_genes_list)) {
    rn <- all_genes_list[[gse]]
    sample_rn <- head(rn[!is.na(rn) & nzchar(trimws(rn))], 5)
    txt <- paste0(txt, "  ", gse, " (", length(rn), " rows): ", paste(sQuote(sample_rn), collapse = ", "), "\n")
  }
  txt <- paste0(txt, "\n--- Gene ID conversion (probe / Entrez -> symbols) ---\n")
  txt <- paste0(txt, "If you see probe IDs (2824546_st, 200000_s_at) or numeric IDs (Entrez) instead of symbols (e.g. BRCA1):\n")
  txt <- paste0(txt, "  1. Check internet (biomaRt needs Ensembl); install biomaRt: BiocManager::install(\"biomaRt\")\n")
  txt <- paste0(txt, "  2. RNA-seq with Entrez row IDs is converted via org.Hs.eg.db then biomaRt fallback\n")
  txt <- paste0(txt, "  3. Microarray: GEO GPL annotation is used when biomaRt is offline\n")
  txt <- paste0(txt, "  4. For testing merged RNA+microarray, use GSEs that map to symbols (e.g. GSE62646)\n")
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
#' @return List with updated lists and appended log text.
#'
#' @examples
#' m <- matrix(1:6, nrow = 2, dimnames = list(c("A", "B"), paste0("S", 1:3)))
#' r <- matrix(1:6, nrow = 2, dimnames = list(c("B", "C"), paste0("T", 1:3)))
#' out <- gexp_download_normalize_ids_for_overlap(
#'   micro_expr_list = list(M = m),
#'   rna_counts_list = list(R = r)
#' )
#' names(out)
#' @export
gexp_download_normalize_ids_for_overlap <- function(
  micro_expr_list,
  rna_counts_list,
  platform_per_gse = NULL,
  all_genes_list = NULL
) {
  if (is.null(all_genes_list)) {
    all_genes_list <- gexp_rebuild_all_genes_list(micro_expr_list, rna_counts_list)
  }

  log_text <- "\nSTEP 2b: Normalize IDs to gene symbols for overlap...\n"

  for (gse in names(micro_expr_list)) {
    micro_expr <- micro_expr_list[[gse]]
    rn <- rownames(micro_expr)
    sample_rn <- head(rn[!is.na(rn) & rn != ""], min(200, length(rn)))
    looks_like_symbol <- length(sample_rn) > 0 &&
      mean(grepl("^[A-Za-z]", sample_rn), na.rm = TRUE) > 0.6 &&
      mean(grepl("^ENSG|_at$|_st$|^[0-9]+$", sample_rn), na.rm = TRUE) < 0.5
    gpl <- if (!is.null(platform_per_gse)) platform_per_gse[[gse]] else NULL
    if (!looks_like_symbol) {
      fmt <- detect_gene_id_format(rn)
      log_text <- paste0(log_text, "  ", gse, ": format ", fmt, " -> converting to symbols...\n")
      sym <- any_id_to_symbol(rn, gpl_id = gpl)
      valid <- !is.na(sym) & trimws(sym) != ""
      if (sum(valid) > 0) {
        rownames(micro_expr) <- sym
        micro_expr <- micro_expr[valid, , drop = FALSE]
        if (any(duplicated(rownames(micro_expr)))) micro_expr <- limma::avereps(micro_expr, ID = rownames(micro_expr))
        micro_expr_list[[gse]] <- micro_expr
        all_genes_list[[gse]] <- rownames(micro_expr)
        log_text <- paste0(log_text, "  ", gse, ": converted to ", nrow(micro_expr), " gene symbols\n")
      }
    }
  }

  for (gse in names(rna_counts_list)) {
    cnt <- rna_counts_list[[gse]]
    rn <- rownames(cnt)
    sample_rn <- head(rn[!is.na(rn) & rn != ""], min(200, length(rn)))
    looks_like_symbol <- length(sample_rn) > 0 &&
      mean(grepl("^[A-Za-z]", sample_rn), na.rm = TRUE) > 0.6 &&
      mean(grepl("^ENSG|_at$|_st$|^[0-9]+$", sample_rn), na.rm = TRUE) < 0.5
    if (!looks_like_symbol) {
      sym <- any_id_to_symbol(rn, gpl_id = NULL)
      valid <- !is.na(sym) & trimws(sym) != ""
      if (sum(valid) > 0) {
        rownames(cnt) <- sym
        cnt <- cnt[valid, , drop = FALSE]
        if (any(duplicated(rownames(cnt)))) cnt <- limma::avereps(cnt, ID = rownames(cnt))
        rna_counts_list[[gse]] <- cnt
        all_genes_list[[gse]] <- rownames(cnt)
        log_text <- paste0(log_text, "  ", gse, ": converted to ", nrow(cnt), " gene symbols\n")
      }
    }
  }

  for (gse in names(rna_counts_list)) {
    rn <- rownames(rna_counts_list[[gse]])
    sample_rn <- head(rn[!is.na(rn) & nzchar(trimws(rn))], min(300, length(rn)))
    if (length(sample_rn) > 0 && mean(grepl("^[0-9]+$", sample_rn), na.rm = TRUE) > 0.7) {
      log_text <- paste0(log_text, "  ", gse, ": row IDs still Entrez-like -> trying biomaRt Entrez->symbol...\n")
      sym <- entrez_to_symbol_biomart(rn)
      if (!is.null(sym)) {
        valid <- !is.na(sym) & trimws(sym) != ""
        if (sum(valid) > 0) {
          cnt <- rna_counts_list[[gse]]
          rownames(cnt) <- sym
          cnt <- cnt[valid, , drop = FALSE]
          if (any(duplicated(rownames(cnt)))) cnt <- limma::avereps(cnt, ID = rownames(cnt))
          rna_counts_list[[gse]] <- cnt
          all_genes_list[[gse]] <- rownames(cnt)
          log_text <- paste0(log_text, "  ", gse, ": biomaRt converted to ", nrow(cnt), " gene symbols\n")
        }
      }
    }
  }

  for (gse in names(all_genes_list)) {
    rn <- all_genes_list[[gse]]
    sample_rn <- head(rn[!is.na(rn) & nzchar(trimws(rn))], min(500, length(rn)))
    if (length(sample_rn) > 0 && mean(grepl("^[0-9]+_st$", sample_rn), na.rm = TRUE) > 0.5) {
      log_text <- paste0(log_text, "  ", gse, ": detected Affymetrix HuGene probe (_st) format -> converting...\n")
      gpl <- if (!is.null(platform_per_gse)) platform_per_gse[[gse]] else NULL
      sym <- probe_ids_to_symbol_hugene_db(rn, gpl)
      if (is.null(sym) || sum(!is.na(sym)) <= length(rn) * 0.1) sym <- probe_ids_to_symbol_gpl(rn, gpl)
      if (is.null(sym) || sum(!is.na(sym)) <= length(rn) * 0.1) sym <- probe_ids_to_symbol_biomart(rn, gpl)
      if (!is.null(sym) && sum(!is.na(sym)) > length(rn) * 0.1) {
        valid <- !is.na(sym) & trimws(sym) != ""
        if (gse %in% names(micro_expr_list)) {
          micro_expr <- micro_expr_list[[gse]]
          rownames(micro_expr) <- sym
          micro_expr <- micro_expr[valid, , drop = FALSE]
          if (any(duplicated(rownames(micro_expr)))) micro_expr <- limma::avereps(micro_expr, ID = rownames(micro_expr))
          micro_expr_list[[gse]] <- micro_expr
        } else if (gse %in% names(rna_counts_list)) {
          cnt <- rna_counts_list[[gse]]
          rownames(cnt) <- sym
          cnt <- cnt[valid, , drop = FALSE]
          if (any(duplicated(rownames(cnt)))) cnt <- limma::avereps(cnt, ID = rownames(cnt))
          rna_counts_list[[gse]] <- cnt
        }
        all_genes_list[[gse]] <- if (gse %in% names(micro_expr_list)) rownames(micro_expr_list[[gse]]) else rownames(rna_counts_list[[gse]])
        n_after <- if (gse %in% names(micro_expr_list)) nrow(micro_expr_list[[gse]]) else nrow(rna_counts_list[[gse]])
        log_text <- paste0(log_text, "  ", gse, ": _st probe IDs converted to ", n_after, " gene symbols (HuGene/GEO GPL/biomaRt)\n")
      }
    }
  }

  log_text <- paste0(log_text, "\nGene symbols extracted per GSE (format detected + sample):\n")
  for (gse in names(all_genes_list)) {
    rn <- all_genes_list[[gse]]
    n <- length(rn)
    sample_rn <- head(rn[!is.na(rn) & nzchar(trimws(rn))], 10)
    sample_str <- if (length(sample_rn) > 0) paste(sQuote(sample_rn), collapse = ", ") else "(none)"
    fmt <- detect_gene_id_format(rn)
    log_text <- paste0(log_text, "  ", gse, ": ", n, " genes; format: ", fmt, "; sample: ", sample_str, "\n")
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
#' @return List with status, log text, reason, expression, metadata, eset, platform_id, and cel_paths.
#'
#' @examples
#' if (interactive()) {
#'   out <- gexp_download_one_microarray_gse("GSE10072", tempdir())
#'   out$ok
#' }
#' @export
gexp_download_one_microarray_gse <- function(gse_id, micro_dir) {
  out <- list(
    ok = FALSE, reason = NULL, log = "", micro_expr = NULL, metadata = NULL,
    micro_eset = NULL, platform_id = NULL, cel_paths = character(0)
  )

  micro_data <- tryCatch({
    suppressMessages(invisible(capture.output(
      md <- GEOquery::getGEO(gse_id, GSEMatrix = TRUE, getGPL = TRUE),
      file = nullfile()
    )))
    md
  }, error = function(e) structure(list(error = conditionMessage(e)), class = "geo_error"))

  if (inherits(micro_data, "geo_error")) {
    err_msg <- micro_data$error
    out$reason <- if (grepl("connection|timeout|hostname|resolve|HTTP|ssl|could not resolve|Unable to", err_msg, ignore.case = TRUE)) {
      "network/HTTP - check internet connection"
    } else {
      substr(gsub("\n", " ", err_msg), 1L, 80L)
    }
    return(out)
  }

  if (is.list(micro_data) && length(micro_data) >= 1) {
    platforms <- vapply(micro_data, function(x) Biobase::annotation(x), character(1))
    n_feat <- vapply(micro_data, function(x) tryCatch(nrow(Biobase::exprs(x)), error = function(e) 0L), integer(1))
    idx <- which.max(n_feat)
    if (length(idx) == 0 || is.na(idx) || idx < 1) idx <- 1
    micro_eset <- micro_data[[idx]]
    platform_id <- Biobase::annotation(micro_eset)
    out$log <- if (length(micro_data) > 1) {
      paste0("Platforms: ", paste(unique(platforms), collapse = ", "), ". Using ", platform_id, ". ")
    } else {
      paste0("Platform ", platform_id, ". ")
    }
  } else {
    micro_eset <- micro_data
    platform_id <- Biobase::annotation(micro_eset)
    out$log <- paste0("Platform ", platform_id, ". ")
  }

  micro_expr <- Biobase::exprs(micro_eset)
  pdata <- Biobase::pData(micro_eset)
  out$log <- paste0(out$log, "Downloaded: ", nrow(micro_expr), " genes x ", ncol(micro_expr), " samples")

  cel <- character(0)
  tryCatch({
    suppressMessages(invisible(capture.output(
      GEOquery::getGEOSuppFiles(gse_id, baseDir = micro_dir, makeDirectory = TRUE, fetch_files = TRUE),
      file = nullfile()
    )))
    supp_dir <- file.path(micro_dir, gse_id)
    if (!dir.exists(supp_dir)) supp_dir <- micro_dir
    files <- list.files(supp_dir, full.names = TRUE, recursive = TRUE)
    tar_files <- files[grepl("\\.tar$|\\.zip$", files, ignore.case = TRUE)]
    for (tf in tar_files) {
      tryCatch({
        if (grepl("\\.zip$", tf, ignore.case = TRUE)) utils::unzip(tf, exdir = supp_dir) else utils::untar(tf, exdir = supp_dir, tar = "internal")
      }, error = function(e) NULL)
    }
    files <- list.files(supp_dir, full.names = TRUE, recursive = TRUE)
    cel <- files[grepl("\\.cel$", files, ignore.case = TRUE)]
  }, error = function(e) NULL)

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

#' Download and parse one RNA-seq GSE
#'
#' @param gse_id GEO series ID.
#' @param rna_dir Directory containing `rna_data`.
#' @return List with status, reason, log text, count matrix, and metadata.
#'
#' @examples
#' if (interactive()) {
#'   out <- gexp_download_one_rnaseq_gse("GSE50760", tempdir())
#'   out$ok
#' }
#' @export
gexp_download_one_rnaseq_gse <- function(gse_id, rna_dir) {
  out <- list(ok = FALSE, reason = NULL, log = "", count_matrix = NULL, metadata = NULL)
  gse_dir <- file.path(rna_dir, gse_id)
  dir.create(gse_dir, showWarnings = FALSE, recursive = TRUE)

  supp_state <- new.env(parent = emptyenv())
  supp_state$err <- NULL
  count_file <- NULL
  tryCatch({
    suppressMessages(invisible(capture.output(
      GEOquery::getGEOSuppFiles(gse_id, baseDir = dirname(gse_dir), makeDirectory = FALSE, fetch_files = TRUE),
      file = nullfile()
    )))
    files <- list.files(gse_dir, full.names = TRUE, recursive = TRUE)
    if (length(files) == 0) {
      files <- list.files(rna_dir, full.names = TRUE, recursive = TRUE)
      files <- files[grepl(gse_id, basename(files), ignore.case = TRUE)]
    }

    supp_files <- files[!grepl("\\.tar$", files, ignore.case = TRUE)]
    bulk_candidates <- supp_files[
      grepl("bulk", basename(supp_files), ignore.case = TRUE) &
        grepl("count", basename(supp_files), ignore.case = TRUE)
    ]
    if (length(bulk_candidates) > 0) {
      best_nrow <- 0L
      for (cand in bulk_candidates) {
        tryCatch({
          df <- suppressWarnings(data.table::fread(cand, data.table = FALSE, nrows = 1e6))
          if (ncol(df) >= 2 && nrow(df) >= 10 && nrow(df) > best_nrow) {
            best_nrow <- nrow(df)
            count_file <- cand
          }
        }, error = function(e) NULL)
      }
    }

    if (is.null(count_file)) {
      tar_files <- files[grepl("\\.tar$", files, ignore.case = TRUE)]
      for (tar_file in tar_files) {
        tryCatch({
          utils::untar(tar_file, exdir = gse_dir, tar = "internal")
        }, error = function(e) {
          msg <- conditionMessage(e)
          if (grepl("truncated|corrupt|error|invalid", msg, ignore.case = TRUE)) {
            supp_state$err <- paste0("Truncated or corrupted tar archive (", basename(tar_file), "). Re-download or try another GSE.")
          } else {
            supp_state$err <- paste0("Untar failed: ", substr(msg, 1L, 120L))
          }
        }, warning = function(w) {
          supp_state$err <- paste0("Tar archive problem (", basename(tar_file), "). File may be truncated or corrupted.")
        })
      }
      files <- list.files(gse_dir, full.names = TRUE, recursive = TRUE)
      if (length(files) == 0) files <- list.files(rna_dir, full.names = TRUE)
      for (pattern in c("count", "raw", "matrix")) {
        matches <- files[grepl(pattern, basename(files), ignore.case = TRUE)]
        matches <- matches[!grepl("series_matrix", basename(matches), ignore.case = TRUE)]
        if (length(matches) > 0) {
          best_nrow <- 0L
          for (cand in matches) {
            tryCatch({
              df <- suppressWarnings(data.table::fread(cand, data.table = FALSE, nrows = 1e6))
              if (ncol(df) >= 2 && nrow(df) >= 10 && nrow(df) > best_nrow) {
                best_nrow <- nrow(df)
                count_file <- cand
              }
            }, error = function(e) NULL)
          }
          if (!is.null(count_file)) break
          break
        }
      }
    }
  }, error = function(e) {
    supp_state$err <- conditionMessage(e)
    NULL
  })

  ncbi_best <- tryCatch(download_ncbi_raw_counts_best(gse_id, gse_dir), error = function(e) NULL)
  nrow_supp <- if (!is.null(count_file)) tryCatch(nrow(suppressWarnings(data.table::fread(count_file, data.table = FALSE, nrows = 500000L))), error = function(e) 0L) else 0L
  nrow_ncbi <- if (!is.null(ncbi_best)) tryCatch(nrow(suppressWarnings(data.table::fread(ncbi_best, data.table = FALSE, nrows = 500000L))), error = function(e) 0L) else 0L
  if (nrow_ncbi > nrow_supp && !is.null(ncbi_best)) {
    count_file <- ncbi_best
    out$log <- paste0("(NCBI ", nrow_ncbi, " rows) ")
  } else if (!is.null(count_file)) {
    out$log <- paste0("(GEO supp ", nrow_supp, " rows) ")
  }
  if (is.null(count_file) && !is.null(ncbi_best)) count_file <- ncbi_best
  if (is.null(count_file)) {
    out$reason <- "no count file (check internet or GSE may not have GEO supp or NCBI counts)"
    if (!is.null(supp_state$err) && nzchar(supp_state$err)) {
      out$reason <- if (grepl("connection|timeout|hostname|resolve|HTTP|ssl", supp_state$err, ignore.case = TRUE)) {
        "network/HTTP - check internet connection"
      } else if (grepl("truncated|corrupt|tar archive", supp_state$err, ignore.case = TRUE)) {
        "truncated/corrupted supplementary tar - try re-download or remove this GSE"
      } else {
        supp_state$err
      }
    }
    return(out)
  }

  count_df <- tryCatch(read_count_matrix(count_file), error = function(e) suppressWarnings(data.table::fread(count_file, data.table = FALSE)))
  if (is.null(count_df) || ncol(count_df) < 2 || nrow(count_df) < 10) {
    out$reason <- "count file format invalid or too small"
    return(out)
  }

  gene_ids <- as.character(count_df[[1]])
  count_matrix <- as.matrix(count_df[, -1, drop = FALSE])
  mode(count_matrix) <- "numeric"
  rownames(count_matrix) <- gene_ids

  rna_metadata <- tryCatch({
    suppressMessages(invisible(capture.output(
      gse_list <- GEOquery::getGEO(gse_id, GSEMatrix = TRUE),
      file = nullfile()
    )))
    gse <- if (inherits(gse_list, "list") && length(gse_list) > 1) gse_list[[1]] else if (inherits(gse_list, "list")) gse_list[[1]] else gse_list
    pheno <- Biobase::pData(gse)
    if (is.null(pheno) || nrow(pheno) == 0) stop("empty pData")
    pheno
  }, error = function(e) {
    sm <- gexp_fetch_geo_series_matrix_metadata(gse_id)
    count_cols <- colnames(count_matrix)
    if (!is.null(sm) && nrow(sm) > 0 && ncol(sm) > 0) {
      outm <- as.data.frame(matrix(NA_character_, nrow = length(count_cols), ncol = ncol(sm)), stringsAsFactors = FALSE)
      colnames(outm) <- colnames(sm)
      rownames(outm) <- count_cols
      for (sid in count_cols) {
        if (sid %in% rownames(sm)) outm[sid, ] <- sm[sid, , drop = TRUE]
      }
      return(outm)
    }
    data.frame(title = colnames(count_matrix), row.names = colnames(count_matrix), stringsAsFactors = FALSE)
  })

  if (!is.null(rna_metadata) && nrow(rna_metadata) > 0) {
    common_samples <- intersect(colnames(count_matrix), rownames(rna_metadata))
    if (length(common_samples) > 0) {
      rna_metadata <- rna_metadata[common_samples, , drop = FALSE]
    }
  }

  gene_symbols <- suppressMessages(convert_rnaseq_ids(gene_ids, gse_id))
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

  out$ok <- TRUE
  out$count_matrix <- count_matrix
  out$metadata <- rna_metadata
  out
}

