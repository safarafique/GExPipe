#!/usr/bin/env Rscript
## Heterogeneous multi-GSE: paper-style ∩ limma vs joint limma vs BIOS-Rank
##
## Paper: Mosharaf et al., BMC Medical Genomics (2023)
##   DOI 10.1186/s12920-023-01488-w
##   Published 11 KGs after multi-GEO limma + PPI.
##
## Heterogeneous merge (this script):
##   Microarray (Affy GPL570): GSE9348, GSE110224, GSE23878
##   RNA-seq:                  GSE50760  (from validation_manual counts)
##   Joint model: limma ~ Condition + Platform
##
## Metrics:
##   A) Overlap with published 11 KGs (top-20 panels)
##   B) Cross-assay gene AUC (min median Micro vs RNA) — heterogeneous transfer
##
## Usage:
##   setwd("E:/GExPipe/GExPipe(original_paper)")
##   source("scripts/compare-paper-heterogeneous-bios.R")
##   Rscript scripts/compare-paper-heterogeneous-bios.R

suppressPackageStartupMessages({
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager", repos = "https://cloud.r-project.org")
  for (p in c("GEOquery", "Biobase", "limma", "edgeR", "org.Hs.eg.db", "AnnotationDbi", "pROC")) {
    if (!requireNamespace(p, quietly = TRUE))
      BiocManager::install(p, ask = FALSE, update = FALSE)
  }
  library(GEOquery)
  library(Biobase)
  library(limma)
  library(edgeR)
  library(pROC)
})

ca <- commandArgs(trailingOnly = FALSE)
fa <- sub("^--file=", "", ca[grep("^--file=", ca)])
root <- if (length(fa)) dirname(dirname(normalizePath(fa[1]))) else getwd()
if (!file.exists(file.path(root, "R", "biosRank.R"))) {
  root <- "E:/GExPipe/GExPipe(original_paper)"
}
source(file.path(root, "R", "biosRank.R"))

gexpipe <- dirname(root)
vm <- file.path(gexpipe, "validation_manual")
outdir <- file.path(root, "results", "paper_heterogeneous_compare")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
destdir <- file.path(root, "results", "cache", "GEO_paper_het")
dir.create(destdir, showWarnings = FALSE, recursive = TRUE)
cache_rds <- file.path(outdir, "merged_heterogeneous.rds")

scale01 <- function(x) {
  x <- as.numeric(x)
  r <- range(x[is.finite(x)], na.rm = TRUE)
  if (!all(is.finite(r)) || diff(r) < .Machine$double.eps) return(rep(0, length(x)))
  out <- (x - r[1]) / (r[2] - r[1])
  out[!is.finite(out)] <- 0
  out
}

paper_KG <- c(
  "CXCL8", "CEMIP", "MMP7", "CA4", "ADH1C", "GUCA2A",
  "GUCA2B", "ZG16", "CLCA4", "MS4A12", "CLDN1"
)

micro_ids <- c("GSE9348", "GSE110224", "GSE23878")
disease_rx <- "tumor|tumour|cancer|carcinoma|crc|adenocarcinoma|disease|case"
normal_rx  <- "normal|healthy|control|nontumor|non-tumor"

gene_auc <- function(y, x) {
  y <- as.numeric(y); x <- as.numeric(x)
  ok <- is.finite(y) & is.finite(x)
  y <- y[ok]; x <- x[ok]
  if (length(unique(y)) < 2L) return(NA_real_)
  roc <- tryCatch(pROC::roc(y, x, quiet = TRUE, direction = "auto"), error = function(e) NULL)
  if (is.null(roc)) return(NA_real_)
  as.numeric(pROC::auc(roc))
}

## ---- Microarray GSEs (Affy) ----
prep_micro <- function(gse_id) {
  cat("Downloading microarray ", gse_id, "...\n", sep = "")
  gset <- getGEO(gse_id, GSEMatrix = TRUE, getGPL = TRUE, destdir = destdir)
  if (length(gset) > 1L) gset <- gset[[which.max(vapply(gset, ncol, 1L))]]
  else gset <- gset[[1]]

  expr <- exprs(gset)
  pdata <- pData(gset)
  pick <- unique(c(
    intersect(c("title", "source_name_ch1"), names(pdata)),
    grep("^characteristics", names(pdata), value = TRUE, ignore.case = TRUE)
  ))
  if (!length(pick)) pick <- names(pdata)[seq_len(min(4L, ncol(pdata)))]
  txt <- apply(pdata[, pick, drop = FALSE], 1, function(r)
    paste(tolower(as.character(r)), collapse = " | "))

  cond <- rep(NA_character_, length(txt))
  cond[grepl(normal_rx, txt)] <- "Normal"
  cond[is.na(cond) & grepl(disease_rx, txt)] <- "Disease"
  ok <- !is.na(cond)
  if (length(unique(na.omit(cond[ok]))) < 2L)
    stop(gse_id, ": could not assign Normal/Disease")

  expr <- expr[, ok, drop = FALSE]
  cond <- factor(cond[ok], levels = c("Normal", "Disease"))

  fd <- fData(gset)
  sym_col <- grep("symbol|gene.?symbol|gene_assignment", names(fd),
                  ignore.case = TRUE, value = TRUE)
  if (!length(sym_col)) stop(gse_id, ": no gene symbol column")
  syms <- trimws(sub("\\s*//.*$", "", as.character(fd[rownames(expr), sym_col[1]])))
  keep <- !is.na(syms) & nzchar(syms) & !syms %in% c("---", "")
  expr <- expr[keep, , drop = FALSE]
  syms <- syms[keep]
  if (anyDuplicated(syms)) expr <- limma::avereps(expr, ID = syms)
  else rownames(expr) <- syms

  if (max(expr, na.rm = TRUE) > 100) expr <- log2(expr + 1)
  expr <- normalizeBetweenArrays(expr, method = "quantile")

  cat("  ", gse_id, ": n=", ncol(expr),
      " Disease=", sum(cond == "Disease"),
      " Normal=", sum(cond == "Normal"),
      " genes=", nrow(expr), "\n", sep = "")
  list(expr = expr, Condition = cond, gse = gse_id, platform = "Microarray")
}

## ---- RNA-seq GSE50760 ----
prep_rnaseq <- function() {
  counts_path <- file.path(vm, "competitor_benchmark/upload_pack/GSE50760_counts.csv")
  pheno_path  <- file.path(vm, "competitor_benchmark/upload_pack/GSE50760_phenotype.csv")
  if (!file.exists(counts_path)) stop("Missing ", counts_path)
  cat("Loading RNA-seq GSE50760 from validation_manual...\n")
  counts <- as.matrix(utils::read.csv(counts_path, row.names = 1, check.names = FALSE))
  storage.mode(counts) <- "numeric"
  pheno <- utils::read.csv(pheno_path, stringsAsFactors = FALSE)
  samp <- intersect(pheno$Sample, colnames(counts))
  counts <- counts[, samp, drop = FALSE]
  cond <- factor(pheno$Condition[match(samp, pheno$Sample)],
                 levels = c("Normal", "Disease"))

  ## Entrez → SYMBOL if needed; else keep gene symbols
  ids <- rownames(counts)
  if (mean(grepl("^[0-9]+$", head(ids, 200))) > 0.8) {
    map <- AnnotationDbi::select(
      org.Hs.eg.db::org.Hs.eg.db, keys = ids,
      columns = "SYMBOL", keytype = "ENTREZID"
    )
    map <- map[!is.na(map$SYMBOL) & nzchar(map$SYMBOL) & !duplicated(map$ENTREZID), ]
    keep <- intersect(ids, map$ENTREZID)
    counts <- counts[keep, , drop = FALSE]
    sym <- map$SYMBOL[match(rownames(counts), map$ENTREZID)]
    split_idx <- split(seq_len(nrow(counts)), sym)
    out <- lapply(split_idx, function(ii) {
      if (length(ii) == 1L) counts[ii, , drop = TRUE] else colSums(counts[ii, , drop = FALSE])
    })
    counts <- do.call(rbind, out)
    rownames(counts) <- names(out)
  }

  y <- edgeR::DGEList(counts = counts)
  y <- edgeR::calcNormFactors(y)
  expr <- limma::normalizeBetweenArrays(
    as.matrix(edgeR::cpm(y, log = TRUE, prior.count = 1)), method = "quantile"
  )

  cat("  GSE50760: n=", ncol(expr),
      " Disease=", sum(cond == "Disease"),
      " Normal=", sum(cond == "Normal"),
      " genes=", nrow(expr), "\n", sep = "")
  list(expr = expr, Condition = cond, gse = "GSE50760", platform = "RNAseq")
}

micro_parts <- lapply(micro_ids, prep_micro)
rna_part <- prep_rnaseq()
parts <- c(micro_parts, list(rna_part))

## ---- A) Paper-style: separate limma per GSE → intersect ----
sep_sig <- list()
lfc_by_study <- list()
for (p in parts) {
  des <- model.matrix(~ p$Condition)
  fit <- eBayes(lmFit(p$expr, des))
  tt <- topTable(fit, coef = 2, number = Inf, sort.by = "P")
  sig <- rownames(tt)[tt$adj.P.Val < 0.05 & abs(tt$logFC) > 0.5]
  sep_sig[[p$gse]] <- sig
  tt0 <- topTable(fit, coef = 2, number = Inf, sort.by = "none")
  lfc_by_study[[p$gse]] <- setNames(tt0$logFC, rownames(tt0))
  cat("Separate limma ", p$gse, " (", p$platform, "): sig DEGs = ",
      length(sig), "\n", sep = "")
}
paper_style_intersect <- Reduce(intersect, sep_sig)
cat("Paper-style ∩ across ", length(parts), " GSEs: ",
    length(paper_style_intersect), " genes\n", sep = "")

avg_abs <- sapply(paper_style_intersect, function(g) {
  mean(abs(sapply(lfc_by_study, function(v) {
    if (g %in% names(v)) v[[g]] else NA_real_
  })), na.rm = TRUE)
})
intersect_ranked <- paper_style_intersect[order(-avg_abs)]
intersect_top20 <- head(intersect_ranked, 20)

## ---- B) Joint limma ~ Condition + Platform ----
## Keep platform scale differences (no within-study z-score wipe-out of Ep).
## Within-study quantile already applied; joint quantile then limma absorbs Platform.
common <- Reduce(intersect, lapply(parts, function(p) rownames(p$expr)))
cat("Common genes for heterogeneous merge: ", length(common), "\n", sep = "")

expr <- do.call(cbind, lapply(parts, function(p) p$expr[common, , drop = FALSE]))
expr <- normalizeBetweenArrays(expr, method = "quantile")
Condition <- factor(unlist(lapply(parts, function(p) as.character(p$Condition))),
                    levels = c("Normal", "Disease"))
Platform <- factor(unlist(lapply(parts, function(p) rep(p$platform, ncol(p$expr)))),
                   levels = c("Microarray", "RNAseq"))

design <- model.matrix(~ Condition + Platform)
fit_joint <- eBayes(lmFit(expr, design))
tt_joint <- topTable(fit_joint, coef = 2, number = Inf, sort.by = "P")
tt_plat  <- topTable(fit_joint, coef = 3, number = Inf, sort.by = "none")
limma_top20 <- head(rownames(tt_joint), 20)

## ---- Cross-assay gene AUC (Microarray pooled vs RNA) ----
idx_m <- which(Platform == "Microarray")
idx_r <- which(Platform == "RNAseq")
ym <- as.numeric(Condition[idx_m] == "Disease")
yr <- as.numeric(Condition[idx_r] == "Disease")
dm <- t(expr[, idx_m, drop = FALSE])
dr <- t(expr[, idx_r, drop = FALSE])

cand <- unique(c(
  rownames(tt_joint)[tt_joint$adj.P.Val < 0.05],
  head(rownames(tt_joint), 1500),
  limma_top20, intersect_top20, paper_KG
))
cand <- intersect(cand, colnames(dm))
cat("Computing cross-assay AUCs for ", length(cand), " candidate genes...\n", sep = "")
auc_m <- setNames(rep(NA_real_, length(cand)), cand)
auc_r <- auc_m
for (g in cand) {
  auc_m[g] <- gene_auc(ym, dm[, g])
  auc_r[g] <- gene_auc(yr, dr[, g])
}

## ---- C) BIOS-Rank ----
## C1) Default API (Ec+Ep+Em_proxy+Es_proxy)
bios <- biosRank(fit_joint, coef_condition = 2, coef_platform = 3)
bios_api_top20 <- topBIOS(bios, n = 20)$Gene

## C2) Heterogeneous BIOS-v2 (non-circular): Ex = microarray AUC only
##     Em/Es = strong DE proxies (no held-out RNA AUC in the rank score)
g_all <- rownames(tt_joint)
beta_c <- abs(tt_joint$logFC)
padj <- tt_joint$adj.P.Val
beta_p <- abs(tt_plat$logFC[match(g_all, rownames(tt_plat))])
beta_p[!is.finite(beta_p)] <- 0
Ec <- scale01((-log10(pmax(padj, 1e-300))) * beta_c)
Ep <- scale01(beta_c / (beta_c + beta_p + 1e-6))
Ex_train <- setNames(rep(0, length(g_all)), g_all)
Ex_train[intersect(names(auc_m), g_all)] <- auc_m[intersect(names(auc_m), g_all)]
Ex_train[!is.finite(Ex_train)] <- 0
Ex_train <- scale01(Ex_train)
sig <- is.finite(padj) & padj < 0.05
thr <- if (any(sig)) stats::quantile(beta_c[sig], 0.8, na.rm = TRUE) else Inf
Em <- as.numeric(sig & beta_c >= thr)
Es <- ifelse(is.finite(padj) & padj < 0.01 & beta_c > 1, 1,
             ifelse(sig, 1 / 3, 0))
## Equal weights on Ec, Ep, Em, Es, Ex_train (v2 correctness: no RNA Ex)
BIOS_v2 <- (Ec + Ep + Em + Es + Ex_train) / 5
names(BIOS_v2) <- g_all
bios_top20 <- head(g_all[order(-BIOS_v2, padj)], 20)

saveRDS(list(expr = expr, Condition = Condition, Platform = Platform,
             tt_joint = tt_joint, BIOS_v2 = BIOS_v2, auc_m = auc_m, auc_r = auc_r),
        cache_rds)

panel_metrics <- function(genes, name) {
  genes <- as.character(genes)
  hit_auc <- intersect(genes, names(auc_m))
  data.frame(
    Panel = name,
    k = length(genes),
    Hits_published_11KG = length(intersect(genes, paper_KG)),
    Genes_shared_KG = paste(sort(intersect(genes, paper_KG)), collapse = ";"),
    Median_AUC_Microarray = if (length(hit_auc)) median(auc_m[hit_auc], na.rm = TRUE) else NA_real_,
    Median_AUC_RNAseq = if (length(hit_auc)) median(auc_r[hit_auc], na.rm = TRUE) else NA_real_,
    MinMedian_cross_assay = if (length(hit_auc)) median(pmin(auc_m[hit_auc], auc_r[hit_auc]), na.rm = TRUE) else NA_real_,
    stringsAsFactors = FALSE
  )
}

metrics <- rbind(
  panel_metrics(intersect_top20, "Paper_style_intersect_top20"),
  panel_metrics(limma_top20, "Joint_limma_Condition_Platform_top20"),
  panel_metrics(bios_api_top20, "BIOS_API_default_top20"),
  panel_metrics(bios_top20, "BIOS_v2_het_ExTrain_top20")
)

## KG recovery map
kg_map <- data.frame(
  Paper_KG = paper_KG,
  In_BIOS_v2_top20 = paper_KG %in% bios_top20,
  In_BIOS_API_top20 = paper_KG %in% bios_api_top20,
  In_JointLimma_top20 = paper_KG %in% limma_top20,
  In_PaperStyle_intersect_top20 = paper_KG %in% intersect_top20,
  In_PaperStyle_intersect_ALL = paper_KG %in% paper_style_intersect,
  AUC_Micro = auc_m[paper_KG],
  AUC_RNA = auc_r[paper_KG],
  stringsAsFactors = FALSE
)

pair <- data.frame(
  Comparison = c(
    "BIOS_v2 ∩ Joint_limma",
    "BIOS_v2 ∩ Paper_style_∩",
    "BIOS_API ∩ Joint_limma",
    "Joint_limma ∩ Paper_style_∩"
  ),
  n_shared = c(
    length(intersect(bios_top20, limma_top20)),
    length(intersect(bios_top20, intersect_top20)),
    length(intersect(bios_api_top20, limma_top20)),
    length(intersect(limma_top20, intersect_top20))
  ),
  genes = c(
    paste(sort(intersect(bios_top20, limma_top20)), collapse = ";"),
    paste(sort(intersect(bios_top20, intersect_top20)), collapse = ";"),
    paste(sort(intersect(bios_api_top20, limma_top20)), collapse = ";"),
    paste(sort(intersect(limma_top20, intersect_top20)), collapse = ";")
  ),
  stringsAsFactors = FALSE
)

## Write outputs
utils::write.csv(metrics, file.path(outdir, "panel_metrics.csv"), row.names = FALSE)
utils::write.csv(pair, file.path(outdir, "pairwise_method_overlap.csv"), row.names = FALSE)
utils::write.csv(kg_map, file.path(outdir, "published_KG_recovery.csv"), row.names = FALSE)
utils::write.csv(data.frame(Gene = bios_top20), file.path(outdir, "BIOS_v2_top20.csv"), row.names = FALSE)
utils::write.csv(data.frame(Gene = bios_api_top20), file.path(outdir, "BIOS_API_top20.csv"), row.names = FALSE)
utils::write.csv(data.frame(Gene = limma_top20), file.path(outdir, "Joint_limma_top20.csv"), row.names = FALSE)
utils::write.csv(data.frame(Gene = intersect_top20),
                 file.path(outdir, "PaperStyle_intersect_top20.csv"), row.names = FALSE)
utils::write.csv(topBIOS(bios, 20), file.path(outdir, "BIOS_API_top20_scores.csv"), row.names = FALSE)
bios_v2_df <- data.frame(
  Gene = bios_top20,
  BIOS_v2 = BIOS_v2[bios_top20],
  Ec = Ec[bios_top20], Ep = Ep[bios_top20], Em = Em[match(bios_top20, g_all)],
  Es = Es[match(bios_top20, g_all)], Ex_train = Ex_train[bios_top20],
  adjP = padj[match(bios_top20, g_all)],
  stringsAsFactors = FALSE
)
utils::write.csv(bios_v2_df, file.path(outdir, "BIOS_v2_top20_scores.csv"), row.names = FALSE)

md <- c(
  "# Heterogeneous multi-GSE: paper ∩ vs joint limma vs BIOS-Rank",
  "",
  "**Reference paper:** Mosharaf et al., *BMC Medical Genomics* (2023).",
  "DOI: [10.1186/s12920-023-01488-w](https://doi.org/10.1186/s12920-023-01488-w)",
  "",
  "Published 11 key genes:",
  paste0("`", paste(paper_KG, collapse = "`, `"), "`"),
  "",
  paste0("**Heterogeneous GSEs:** Microarray ", paste(micro_ids, collapse = ", "),
         " × RNA-seq GSE50760."),
  "Joint model: `limma ~ Condition + Platform` (platform scale retained; no wipe-out z-score).",
  "",
  paste0("Paper-style ∩ significant DEGs: **", length(paper_style_intersect), "** genes."),
  paste0("Common genes after merge: **", length(common), "**."),
  "",
  "## Panel metrics (top-20)",
  "",
  "| Method | Hits/11 KG | MinMedian cross-assay AUC | Med AUC Micro | Med AUC RNA |",
  "|--------|------------:|--------------------------:|--------------:|------------:|"
)
for (i in seq_len(nrow(metrics))) {
  md <- c(md, sprintf(
    "| %s | %d | %.3f | %.3f | %.3f |",
    metrics$Panel[i], metrics$Hits_published_11KG[i],
    metrics$MinMedian_cross_assay[i],
    metrics$Median_AUC_Microarray[i], metrics$Median_AUC_RNAseq[i]
  ))
}
md <- c(
  md, "",
  "## Pairwise top-20 overlaps",
  "",
  "| Comparison | n shared |",
  "|------------|---------:|"
)
for (i in seq_len(nrow(pair))) {
  md <- c(md, paste0("| ", pair$Comparison[i], " | ", pair$n_shared[i], " |"))
}
md <- c(
  md, "",
  "## Verdict",
  "",
  "- **Hits vs published 11 KG:** literature concordance (paper used ∩ + PPI).",
  "- **Med AUC RNA / MinMedian:** heterogeneous transfer (Micro → RNA);",
  "  BIOS_v2 ranks with Ec+Ep+Em+Es+Ex_train (microarray AUC only; RNA held out).",
  "",
  paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE))
)
writeLines(md, file.path(outdir, "comparison_report.md"))

cat("\n===== HETEROGENEOUS PANEL METRICS =====\n")
print(metrics, row.names = FALSE)
cat("\n===== PAIRWISE =====\n")
print(pair[, 1:2], row.names = FALSE)
cat("\n===== PAPER KG RECOVERY =====\n")
print(kg_map[, 1:6], row.names = FALSE)
cat("\nWrote: ", outdir, "\n", sep = "")
