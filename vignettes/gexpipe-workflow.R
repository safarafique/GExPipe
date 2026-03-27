## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(GExPipe)
set.seed(123)

## -----------------------------------------------------------------------------
## Works after install (system.file) and when knitting from source (inst/extdata fallback).
vignette_extdata_file <- function(name) {
  p <- system.file("extdata", name, package = "GExPipe")
  if (nzchar(p) && file.exists(p)) return(p)
  candidates <- c(
    file.path(getwd(), "..", "inst", "extdata", name),
    file.path(getwd(), "..", "..", "inst", "extdata", name),
    file.path(getwd(), "inst", "extdata", name)
  )
  hits <- candidates[file.exists(candidates)]
  if (length(hits) < 1L) {
    stop("Bundled vignette file '", name, "' not found. Install GExPipe or use package source tree.")
  }
  normalizePath(hits[1], winslash = "/", mustWork = TRUE)
}

expr_path <- vignette_extdata_file("vignette_expression.csv")
meta_path <- vignette_extdata_file("vignette_sample_metadata.csv")

tab <- read.csv(expr_path, check.names = FALSE, stringsAsFactors = FALSE)
meta <- read.csv(meta_path, check.names = FALSE, stringsAsFactors = FALSE)

gene_col <- tab[[1]]
expr <- as.matrix(tab[, -1, drop = FALSE])
storage.mode(expr) <- "numeric"
rownames(expr) <- gene_col
colnames(expr) <- gsub("^X", "", colnames(expr)) # safety if R adds X prefixes

rownames(meta) <- meta$SampleID
stopifnot(all(colnames(expr) %in% rownames(meta)))
meta <- meta[colnames(expr), , drop = FALSE]

dim(expr)
head(meta)

## -----------------------------------------------------------------------------
parsed <- gexp_parse_gse_inputs(
  analysis_type = "merged",
  rnaseq_gses = "GSE123456, GSE789012",
  microarray_gses = "GSE111\nGSE222",
  dataset_mode = "multi"
)
parsed$rnaseq_ids
parsed$micro_ids

## -----------------------------------------------------------------------------
cols1 <- rownames(meta)[meta$Dataset == "D1"]
cols2 <- rownames(meta)[meta$Dataset == "D2"]

micro_expr_list <- list(
  D1 = expr[, cols1, drop = FALSE],
  D2 = expr[, cols2, drop = FALSE]
)
rna_counts_list <- list()

norm <- gexp_normalize_and_intersect(
  micro_expr_list = micro_expr_list,
  rna_counts_list = rna_counts_list,
  micro_norm_method = "quantile",
  rnaseq_norm_method = "TMM",
  de_method = "limma"
)

dim(norm$combined_expr)
length(norm$common_genes)
head(norm$unified_metadata)

## -----------------------------------------------------------------------------
um <- norm$unified_metadata
um$Condition <- meta[um$SampleID, "Condition"]
um$Dataset <- meta[um$SampleID, "Dataset"]
head(um)

## -----------------------------------------------------------------------------
batch_res <- gexp_batch_correct(
  expr = norm$combined_expr,
  metadata = um[, c("Dataset", "Condition")],
  variance_percentile = 25,
  method = "limma"
)

dim(batch_res$expr_filtered)
dim(batch_res$batch_corrected)
substr(batch_res$log_text, 1, 200)

## -----------------------------------------------------------------------------
de_res <- gexp_run_de(
  expr = batch_res$batch_corrected,
  metadata = um[, c("Dataset", "Condition")],
  method = "limma",
  logfc_cutoff = 0.25,
  padj_cutoff = 0.1
)

head(de_res$de_results[, c("Gene", "logFC", "adj.P.Val", "Significance")])

## -----------------------------------------------------------------------------
qc_out <- gexp_qc_detect_outliers(norm$combined_expr, top_n = 100)
length(qc_out$all_outliers)

qc_summary <- gexp_qc_gene_overlap_summary(
  all_genes_list = list(D1 = rownames(micro_expr_list$D1), D2 = rownames(micro_expr_list$D2)),
  common_genes = norm$common_genes
)
qc_summary

## ----message = FALSE, warning = FALSE-----------------------------------------
wgcna_prep <- gexp_wgcna_prepare(
  expr = batch_res$batch_corrected,
  metadata = um,
  gene_mode = "top_variable",
  top_genes = min(100L, nrow(batch_res$batch_corrected)),
  min_samples_frac = 0.5
)

dim(wgcna_prep$datExpr)
head(wgcna_prep$gene_variance_table, 3)

## ----eval = FALSE-------------------------------------------------------------
# app <- runGExPipe()
# runApp(app)

## ----geo_dirs, eval = FALSE---------------------------------------------------
# work_dir <- getwd()
# logs <- gexp_prepare_download_dirs(work_dir, has_micro = TRUE, has_rna = FALSE)
# logs

## ----geo_download_micro, eval = FALSE-----------------------------------------
# library(GEOquery)
# 
# gse_id <- "GSE10000" # example placeholder; substitute a real ID from your project
# micro_dir <- file.path(work_dir, "micro_data")
# dl <- gexp_download_one_microarray_gse(gse_id, micro_dir)
# dl$ok
# if (isTRUE(dl$ok)) dim(dl$micro_expr)

## ----geo_meta, eval = FALSE---------------------------------------------------
# head(gexp_fetch_geo_series_matrix_metadata("GSE10000"))

## ----geo_normalize_pattern, eval = FALSE--------------------------------------
# ## Pseudocode pattern -- adapt names/paths to your download result.
# micro_expr_list <- setNames(list(dl$micro_expr), gse_id)
# rna_counts_list <- list()
# 
# step_ids <- gexp_download_normalize_ids_for_overlap(micro_expr_list, rna_counts_list)
# micro2 <- step_ids$micro_expr_list
# rna2 <- step_ids$rna_counts_list
# genes_list <- step_ids$all_genes_list
# 
# fin <- gexp_download_finalize_common_genes(micro2, rna2, genes_list)
# if (!isTRUE(fin$ok)) stop("No common genes -- check ID mapping and platform.")
# 
# norm_geo <- gexp_normalize_and_intersect(
#   micro_expr_list = fin$micro_expr_list,
#   rna_counts_list = fin$rna_counts_list,
#   de_method = "limma"
# )

## -----------------------------------------------------------------------------
sessionInfo()

