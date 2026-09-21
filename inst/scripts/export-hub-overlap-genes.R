#!/usr/bin/env Rscript
## Export overlapping top-50 hubs (merged vs microarray) for Supplementary Table S3.
## Avoids gexp_download_one_microarray_gse() for GSE89076 (RAW-only cache breaks getGEO).
## Loads GSE89076 from NCBI series_matrix instead.
##
## Run in PowerShell / cmd (NOT inside R):
##   Rscript E:/GExPipe/inst/scripts/export-hub-overlap-genes.R --repo E:/GExPipe

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) args[[i + 1L]] else default
}

repo <- normalizePath(get_arg("--repo", "E:/GExPipe"), winslash = "/", mustWork = TRUE)
micro_gse <- get_arg("--micro", "GSE89076")
rna_gse <- get_arg("--rna", "GSE50760")
outdir <- file.path(repo, "validation_manual", "cross_platform")
work <- file.path(outdir, "work")
reportdir <- file.path(outdir, "report")
dir.create(file.path(work, "micro_data", micro_gse), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(work, "rna_data"), recursive = TRUE, showWarnings = FALSE)
dir.create(reportdir, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  pkgload::load_all(repo, quiet = TRUE)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

assign_gse50760 <- function(meta) {
  txt <- tolower(meta$title)
  cond <- rep(NA_character_, nrow(meta))
  cond[grepl("normal colon", txt)] <- "Normal"
  cond[grepl("primary colorectal", txt)] <- "Disease"
  meta$Condition <- cond
  meta[!is.na(meta$Condition), , drop = FALSE]
}

assign_gse89076 <- function(meta) {
  ttl <- as.character(meta$title)
  cond <- rep(NA_character_, length(ttl))
  cond[grepl("N$", ttl)] <- "Normal"
  cond[grepl("T$", ttl)] <- "Disease"
  if (any(is.na(cond))) {
    txt <- apply(meta, 1L, function(r) paste(tolower(as.character(r)), collapse = " "))
    cond[is.na(cond) & grepl("normal|nontumor|non-tumor|healthy", txt)] <- "Normal"
    cond[is.na(cond) & grepl("tumor|tumour|cancer|adenocarcinoma", txt)] <- "Disease"
  }
  meta$Condition <- cond
  meta[!is.na(meta$Condition), , drop = FALSE]
}

map_micro_to_symbols <- function(micro_expr, micro_eset, gse_id) {
  fdata <- if (!is.null(micro_eset)) Biobase::fData(micro_eset) else data.frame()
  syms <- map_microarray_ids(micro_expr, fdata, micro_eset, gse_id = gse_id)
  rownames(micro_expr) <- syms
  ok <- !is.na(syms) & nzchar(trimws(syms))
  micro_expr <- micro_expr[ok, , drop = FALSE]
  if (any(duplicated(rownames(micro_expr)))) {
    micro_expr <- limma::avereps(micro_expr, ID = rownames(micro_expr))
  }
  micro_expr
}

hub_genes_from_expr <- function(expr, genes, n_hub = 50L) {
  genes <- intersect(genes, rownames(expr))
  if (length(genes) < 10L) return(character())
  use <- head(genes, min(500L, length(genes)))
  mat <- t(expr[use, , drop = FALSE])
  cmat <- suppressWarnings(stats::cor(mat, use = "pairwise.complete.obs"))
  diag(cmat) <- 0
  conn <- rowSums(abs(cmat), na.rm = TRUE)
  names(sort(conn, decreasing = TRUE))[seq_len(min(n_hub, length(conn)))]
}

load_gse89076_series_matrix <- function(gse_id, dest_dir) {
  matrix_url <- sprintf(
    "https://ftp.ncbi.nlm.nih.gov/geo/series/%snnn/%s/matrix/%s_series_matrix.txt.gz",
    substr(gse_id, 1, nchar(gse_id) - 3), gse_id, gse_id
  )
  dest <- file.path(dest_dir, paste0(gse_id, "_series_matrix.txt.gz"))
  if (!file.exists(dest) || file.info(dest)$size < 1000) {
    cat("Downloading series matrix from NCBI FTP...\n  ", matrix_url, "\n", sep = "")
    utils::download.file(matrix_url, destfile = dest, mode = "wb", quiet = FALSE)
  } else {
    cat("Using cached series matrix:\n  ", dest, "\n", sep = "")
  }
  gse <- GEOquery::getGEO(filename = dest, getGPL = TRUE)
  eset <- if (is.list(gse)) gse[[1]] else gse
  list(
    ok = TRUE,
    micro_expr = Biobase::exprs(eset),
    metadata = Biobase::pData(eset),
    micro_eset = eset,
    platform_id = Biobase::annotation(eset)
  )
}

cat("=== Export hub overlap genes ===\n")
cat("Repo:", repo, "\n")

sig_file <- file.path(outdir, "merged_limma_DE_sig.csv")
micro_sig_file <- file.path(outdir, "microarray_limma_DE_sig.csv")
if (!file.exists(sig_file)) stop("Missing: ", sig_file)
if (!file.exists(micro_sig_file)) stop("Missing: ", micro_sig_file)
sig_genes <- utils::read.csv(sig_file, stringsAsFactors = FALSE)[[1]]
sig_m <- utils::read.csv(micro_sig_file, stringsAsFactors = FALSE)[[1]]

# --- Microarray: series matrix (not RAW.tar) ---
cat("Loading microarray via series_matrix...\n")
micro_dl <- tryCatch(
  load_gse89076_series_matrix(micro_gse, file.path(work, "micro_data", micro_gse)),
  error = function(e) stop("Microarray series_matrix load failed: ", conditionMessage(e))
)

# --- RNA-seq: package helper (usually fine) ---
cat("Downloading / loading RNA-seq...\n")
rna_dl <- gexp_download_one_rnaseq_gse(rna_gse, file.path(work, "rna_data"))
if (!isTRUE(rna_dl$ok)) {
  # Fallback message if package helper fails
  stop(rna_dl$reason %||% "RNA-seq download failed")
}

meta_micro <- assign_gse89076(micro_dl$metadata)
meta_rna <- assign_gse50760(rna_dl$metadata)
cat("Micro groups:", paste(names(table(meta_micro$Condition)), table(meta_micro$Condition), sep = "=", collapse = ", "), "\n")
cat("RNA groups:", paste(names(table(meta_rna$Condition)), table(meta_rna$Condition), sep = "=", collapse = ", "), "\n")

micro_expr <- map_micro_to_symbols(
  micro_dl$micro_expr[, rownames(meta_micro), drop = FALSE],
  micro_dl$micro_eset, micro_gse
)
rna_counts <- rna_dl$count_matrix[, rownames(meta_rna), drop = FALSE]

cat("Normalizing and intersecting...\n")
norm <- gexp_normalize_and_intersect(
  micro_expr_list = stats::setNames(list(micro_expr), micro_gse),
  rna_counts_list = stats::setNames(list(rna_counts), rna_gse),
  de_method = "limma"
)
meta <- norm$unified_metadata
meta$Condition <- NA_character_
idx_m <- match(meta$SampleID, rownames(meta_micro))
idx_r <- match(meta$SampleID, rownames(meta_rna))
meta$Condition[!is.na(idx_m)] <- meta_micro$Condition[idx_m[!is.na(idx_m)]]
meta$Condition[!is.na(idx_r)] <- meta_rna$Condition[idx_r[!is.na(idx_r)]]
meta <- meta[!is.na(meta$Condition), , drop = FALSE]
rownames(meta) <- meta$SampleID
expr_before <- norm$combined_expr[, rownames(meta), drop = FALSE]

cat("Batch correcting...\n")
batch <- gexp_batch_correct(expr_before, meta, variance_percentile = 25, method = "limma")
expr_after <- batch$batch_corrected
meta_after <- meta[colnames(expr_after), , drop = FALSE]

common <- intersect(rownames(expr_after), intersect(rownames(micro_expr), rownames(rna_counts)))
hub_merged <- hub_genes_from_expr(expr_after, intersect(sig_genes, common), 50L)
micro_samples <- rownames(meta_after)[meta_after$Platform == "Microarray"]
hub_micro <- hub_genes_from_expr(
  expr_after[, micro_samples, drop = FALSE],
  intersect(sig_m, common),
  50L
)

overlap_genes <- sort(intersect(hub_merged, hub_micro))
hub_j_micro <- length(overlap_genes) / length(union(hub_merged, hub_micro))
mcm_cdk <- overlap_genes[grepl("^(MCM|CDK)", overlap_genes)]

utils::write.csv(
  data.frame(Gene = hub_merged, list = "merged_hubs", stringsAsFactors = FALSE),
  file.path(reportdir, "hubs_merged_top50.csv"), row.names = FALSE
)
utils::write.csv(
  data.frame(Gene = hub_micro, list = "microarray_hubs", stringsAsFactors = FALSE),
  file.path(reportdir, "hubs_microarray_top50.csv"), row.names = FALSE
)
utils::write.csv(
  data.frame(Gene = overlap_genes, list = "overlap_merged_vs_micro", stringsAsFactors = FALSE),
  file.path(reportdir, "hubs_overlap_merged_vs_micro.csv"), row.names = FALSE
)
utils::write.csv(
  data.frame(Gene = mcm_cdk, list = "mcm_cdk_in_overlap", stringsAsFactors = FALSE),
  file.path(reportdir, "hubs_overlap_mcm_cdk.csv"), row.names = FALSE
)

cat(sprintf("\nOverlap n=%d  Jaccard=%.3f\n", length(overlap_genes), hub_j_micro))
cat("All overlapping hubs:\n  ", paste(overlap_genes, collapse = ", "), "\n", sep = "")
cat("MCM/CDK among overlap:\n  ",
    if (length(mcm_cdk)) paste(mcm_cdk, collapse = ", ") else "(none)",
    "\n", sep = "")
cat("\nWrote:\n  ", file.path(reportdir, "hubs_overlap_merged_vs_micro.csv"), "\n", sep = "")
