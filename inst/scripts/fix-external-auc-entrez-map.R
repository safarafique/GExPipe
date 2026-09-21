#!/usr/bin/env Rscript
## Fix External AUC: map GSE104836 Entrez IDs -> HGNC symbols, recompute ROC.
## Uses Windows-friendly paths. Run with R-4.6 Rscript.

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) args[i + 1L] else default
}
repo_root <- get_arg("--repo", getwd())
outdir <- file.path(repo_root, "validation_manual")

suppressPackageStartupMessages({
  library(edgeR)
  library(limma)
  library(pROC)
  if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
    stop("Install org.Hs.eg.db: BiocManager::install('org.Hs.eg.db')")
  }
  library(org.Hs.eg.db)
  library(AnnotationDbi)
})

gene_auc <- function(y, x) {
  y <- as.numeric(y); x <- as.numeric(x)
  if (length(unique(y[!is.na(y)])) < 2L) return(NA_real_)
  roc <- tryCatch(pROC::roc(y, x, quiet = TRUE, direction = "auto"), error = function(e) NULL)
  if (is.null(roc)) return(NA_real_)
  as.numeric(pROC::auc(roc))
}

tmm_log_cpm <- function(counts) {
  y <- edgeR::DGEList(counts = counts)
  y <- edgeR::calcNormFactors(y)
  as.matrix(edgeR::cpm(y, log = TRUE, prior.count = 1))
}

assign_gse104836 <- function(meta) {
  txt <- apply(meta, 1L, function(r) paste(tolower(as.character(r)), collapse = " "))
  cond <- rep(NA_character_, length(txt))
  cond[grepl("nontumor|non-tumor", txt)] <- "Normal"
  cond[grepl("colon cancer|cancer tissue|\\bc\\b|_c$", txt)] <- "Disease"
  meta$Condition <- cond
  meta[!is.na(meta$Condition), , drop = FALSE]
}

entrez_to_symbol_matrix <- function(counts) {
  ids <- rownames(counts)
  map <- AnnotationDbi::select(
    org.Hs.eg.db::org.Hs.eg.db,
    keys = ids,
    columns = "SYMBOL",
    keytype = "ENTREZID"
  )
  map <- map[!is.na(map$SYMBOL) & nzchar(map$SYMBOL), , drop = FALSE]
  map <- map[!duplicated(map$ENTREZID), , drop = FALSE]
  keep <- intersect(ids, map$ENTREZID)
  counts <- counts[keep, , drop = FALSE]
  sym <- map$SYMBOL[match(rownames(counts), map$ENTREZID)]
  # collapse duplicate symbols by sum
  split_idx <- split(seq_len(nrow(counts)), sym)
  out <- lapply(split_idx, function(ii) {
    if (length(ii) == 1L) counts[ii, , drop = TRUE] else colSums(counts[ii, , drop = FALSE])
  })
  mat <- do.call(rbind, out)
  rownames(mat) <- names(out)
  storage.mode(mat) <- "numeric"
  mat
}

cat("Loading cached GSE104836...\n")
val <- readRDS(file.path(outdir, "work", "rna_data", "GSE104836_parsed.rds"))
counts_entrez <- val$counts
cat("  Entrez genes:", nrow(counts_entrez), "\n")
counts_sym <- entrez_to_symbol_matrix(counts_entrez)
cat("  Symbol genes:", nrow(counts_sym), "\n")

meta_val <- assign_gse104836(as.data.frame(val$metadata, stringsAsFactors = FALSE))
overlap <- intersect(colnames(counts_sym), rownames(meta_val))
if (!length(overlap) && ncol(counts_sym) == nrow(meta_val)) {
  colnames(counts_sym) <- rownames(meta_val)
  overlap <- rownames(meta_val)
}
counts_sym <- counts_sym[, overlap, drop = FALSE]
meta_val <- meta_val[overlap, , drop = FALSE]
meta_val$Condition <- factor(meta_val$Condition, levels = c("Normal", "Disease"))

expr_va <- limma::normalizeBetweenArrays(tmm_log_cpm(counts_sym), method = "quantile")
dat_va <- t(expr_va)
y_va <- as.numeric(meta_val$Condition[match(rownames(dat_va), rownames(meta_val))] == "Disease")

roc_path <- file.path(outdir, "ROC_AUC_Training_vs_Validation.csv")
roc <- utils::read.csv(roc_path, stringsAsFactors = FALSE)
genes <- roc$Gene
cat("Recomputing external AUC for", length(genes), "genes...\n")
cat("  Overlap in val expr:", length(intersect(genes, colnames(dat_va))), "\n")

roc$AUC_External <- vapply(genes, function(g) {
  if (!g %in% colnames(dat_va)) return(NA_real_)
  gene_auc(y_va, dat_va[, g])
}, numeric(1))
roc$Delta <- round(roc$AUC_External - roc$AUC_Internal, 4)
roc <- roc[order(-roc$AUC_External, na.last = TRUE), , drop = FALSE]
utils::write.csv(roc, roc_path, row.names = FALSE)

# refresh cache with symbol matrix for future runs
val$counts_symbol <- counts_sym
saveRDS(val, file.path(outdir, "work", "rna_data", "GSE104836_parsed.rds"))

cat("Median Train AUC:", round(median(roc$AUC_Internal, na.rm = TRUE), 3), "\n")
cat("Median External AUC:", round(median(roc$AUC_External, na.rm = TRUE), 3), "\n")
print(roc)
