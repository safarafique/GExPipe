#!/usr/bin/env Rscript
## Fair limma-voom / DESeq2 (and optional limma) top-50 external AUCs on GSE104836.
## Primary RNA-seq baselines for publication: limma_voom + deseq2.
## Writes validation_manual/ROC_AUC_baselines_top50.csv and merges into
## ROC_AUC_Training_vs_Validation.csv style columns for S2 regeneration.

repo <- commandArgs(TRUE)
repo_root <- if (length(repo) >= 2 && repo[1] == "--repo") repo[2] else getwd()
if (.Platform$OS.type == "windows") {
  m <- regexec("^/mnt/([a-zA-Z])(/.*)?$", repo_root)
  r <- regmatches(repo_root, m)[[1]]
  if (length(r) >= 2L) {
    rest <- if (length(r) >= 3L && nzchar(r[3])) r[3] else ""
    repo_root <- paste0(toupper(r[2]), ":", rest)
  }
}
outdir <- file.path(repo_root, "validation_manual")

suppressPackageStartupMessages({
  library(edgeR); library(limma); library(pROC)
  library(org.Hs.eg.db); library(AnnotationDbi)
})

gene_auc <- function(y, x) {
  y <- as.numeric(y); x <- as.numeric(x)
  if (length(unique(y[!is.na(y)])) < 2L) return(NA_real_)
  roc <- tryCatch(pROC::roc(y, x, quiet = TRUE, direction = "auto"), error = function(e) NULL)
  if (is.null(roc)) return(NA_real_)
  as.numeric(pROC::auc(roc))
}

tmm_log <- function(counts) {
  y <- edgeR::DGEList(counts = counts)
  y <- edgeR::calcNormFactors(y)
  limma::normalizeBetweenArrays(as.matrix(edgeR::cpm(y, log = TRUE, prior.count = 1)), method = "quantile")
}

entrez_to_symbol <- function(counts) {
  ids <- rownames(counts)
  if (mean(grepl("^[0-9]+$", head(ids, 200))) <= 0.8) return(counts)
  map <- AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db, keys = ids, columns = "SYMBOL", keytype = "ENTREZID")
  map <- map[!is.na(map$SYMBOL) & nzchar(map$SYMBOL) & !duplicated(map$ENTREZID), ]
  keep <- intersect(ids, map$ENTREZID)
  counts <- counts[keep, , drop = FALSE]
  sym <- map$SYMBOL[match(rownames(counts), map$ENTREZID)]
  split_idx <- split(seq_len(nrow(counts)), sym)
  out <- lapply(split_idx, function(ii) if (length(ii) == 1) counts[ii, ] else colSums(counts[ii, , drop = FALSE]))
  mat <- do.call(rbind, out); rownames(mat) <- names(out); storage.mode(mat) <- "numeric"; mat
}

# Train
counts_tr <- as.matrix(read.csv(file.path(outdir, "competitor_benchmark/upload_pack/GSE50760_counts.csv"),
                                row.names = 1, check.names = FALSE))
pheno <- read.csv(file.path(outdir, "competitor_benchmark/upload_pack/GSE50760_phenotype.csv"),
                  stringsAsFactors = FALSE, check.names = FALSE)
id_col <- if ("SampleID" %in% names(pheno)) "SampleID" else names(pheno)[1]
cond_col <- if ("Condition" %in% names(pheno)) "Condition" else names(pheno)[2]
meta_tr <- data.frame(Condition = as.character(pheno[[cond_col]]), row.names = as.character(pheno[[id_col]]))
meta_tr$Condition[grepl("primary|tumor|disease|cancer", tolower(meta_tr$Condition))] <- "Disease"
meta_tr$Condition[grepl("normal|nontumor|control", tolower(meta_tr$Condition))] <- "Normal"
meta_tr <- meta_tr[meta_tr$Condition %in% c("Normal", "Disease"), , drop = FALSE]
common <- intersect(colnames(counts_tr), rownames(meta_tr))
counts_tr <- counts_tr[, common, drop = FALSE]
meta_tr <- meta_tr[common, , drop = FALSE]
y_tr <- as.numeric(meta_tr$Condition == "Disease")
expr_tr <- tmm_log(counts_tr)

# Val
val <- readRDS(file.path(outdir, "work/rna_data/GSE104836_parsed.rds"))
counts_va <- if (!is.null(val$counts_symbol)) val$counts_symbol else entrez_to_symbol(val$counts)
pd <- as.data.frame(val$metadata, stringsAsFactors = FALSE)
txt <- apply(pd, 1, function(r) paste(tolower(as.character(r)), collapse = " "))
cond <- rep(NA_character_, length(txt))
cond[grepl("nontumor|non-tumor", txt)] <- "Normal"
cond[grepl("colon cancer|cancer tissue|\\bc\\b|_c$", txt)] <- "Disease"
pd$Condition <- cond
pd <- pd[!is.na(pd$Condition), , drop = FALSE]
ov <- intersect(colnames(counts_va), rownames(pd))
if (!length(ov) && ncol(counts_va) == nrow(pd)) {
  colnames(counts_va) <- rownames(pd); ov <- rownames(pd)
}
counts_va <- counts_va[, ov, drop = FALSE]
pd <- pd[ov, , drop = FALSE]
y_va <- as.numeric(pd$Condition == "Disease")
expr_va <- tmm_log(counts_va)
dat_tr <- t(expr_tr); dat_va <- t(expr_va)

top50 <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  df <- df[order(df$adj.P.Val), ]
  head(unique(df$Gene), 50)
}

# Prefer RNA-seq baselines (limma-voom, DESeq2). Include limma only if file exists.
panel_files <- c(
  limma_voom = file.path(outdir, "GSE50760_limma_voom_DE_all.csv"),
  deseq2 = file.path(outdir, "GSE50760_deseq2_DE_all.csv"),
  limma = file.path(outdir, "GSE50760_limma_DE_all.csv")
)
panels <- list()
for (nm in names(panel_files)) {
  if (file.exists(panel_files[[nm]])) {
    panels[[nm]] <- top50(panel_files[[nm]])
  } else {
    cat("Skip missing DE file:", panel_files[[nm]], "\n")
  }
}
if (!length(panels)) stop("No GSE50760_*_DE_all.csv files found in ", outdir)

rows <- list()
for (nm in names(panels)) {
  genes <- panels[[nm]]
  for (g in genes) {
    auc_i <- if (g %in% colnames(dat_tr)) gene_auc(y_tr, dat_tr[, g]) else NA_real_
    auc_e <- if (g %in% colnames(dat_va)) gene_auc(y_va, dat_va[, g]) else NA_real_
    rows[[length(rows) + 1L]] <- data.frame(
      Workflow = nm, Gene = g, AUC_Internal = auc_i, AUC_External = auc_e,
      stringsAsFactors = FALSE
    )
  }
}
base <- do.call(rbind, rows)
write.csv(base, file.path(outdir, "ROC_AUC_baselines_top50.csv"), row.names = FALSE)

# Merge into training-vs-validation file used by S2: keep consensus rows + add baseline genes
cons <- read.csv(file.path(outdir, "ROC_AUC_Training_vs_Validation.csv"), stringsAsFactors = FALSE)
# Ensure consensus Delta
if (!"Delta" %in% names(cons)) cons$Delta <- round(cons$AUC_External - cons$AUC_Internal, 4)

# Write a combined AUC map file for S2 (all genes that have any AUC)
all_auc <- rbind(
  cons[, c("Gene", "AUC_Internal", "AUC_External")],
  base[, c("Gene", "AUC_Internal", "AUC_External")]
)
all_auc <- all_auc[!is.na(all_auc$Gene) & nzchar(all_auc$Gene), ]
# Prefer consensus values when duplicated
all_auc <- all_auc[!duplicated(all_auc$Gene), ]
all_auc$Delta <- round(all_auc$AUC_External - all_auc$AUC_Internal, 4)
write.csv(all_auc, file.path(outdir, "ROC_AUC_Training_vs_Validation.csv"), row.names = FALSE)

for (nm in unique(base$Workflow)) {
  cat(nm, "top-50 median external AUC:",
      round(median(base$AUC_External[base$Workflow == nm], na.rm = TRUE), 3),
      "(n=", sum(base$Workflow == nm & !is.na(base$AUC_External)), ")\n")
}
cons20 <- read.csv(file.path(outdir, "consensus_signature_genes.csv"))$Gene
cat("consensus median external (from prior cons file genes):\n")
csub <- cons[cons$Gene %in% cons20, ]
cat("  ", round(median(csub$AUC_External, na.rm = TRUE), 3), " n=", nrow(csub), "\n")
