#!/usr/bin/env Rscript
## Reproduce a real multi-GSE limma paper, then BIOS-Rank, then compare.
##
## Paper: Mosharaf et al., BMC Medical Genomics (2023)
##   https://doi.org/10.1186/s12920-023-01488-w
##   Datasets: GSE9348, GSE110224, GSE23878, GSE35279, GSE50760
##   Reported 11 key genes (KGs) after multi-GEO limma + PPI.
##
## This script:
##   A) Paper-style: limma per GSE → common significant DEGs (intersect)
##   B) Joint limma ~ Condition + Batch on merged Affy studies (GPL570 trio)
##   C) BIOS-Rank on the same joint fit
##   D) Compare overlaps with the published 11 KGs
##
## Usage:
##   setwd("E:/GExPipe/GExPipe(original_paper)")
##   source("scripts/compare-paper-multigse-bios.R")
## Or:
##   Rscript scripts/compare-paper-multigse-bios.R

suppressPackageStartupMessages({
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager", repos = "https://cloud.r-project.org")
  for (p in c("GEOquery", "Biobase", "limma")) {
    if (!requireNamespace(p, quietly = TRUE))
      BiocManager::install(p, ask = FALSE, update = FALSE)
  }
  library(GEOquery)
  library(Biobase)
  library(limma)
})

## Resolve paths
ca <- commandArgs(trailingOnly = FALSE)
fa <- sub("^--file=", "", ca[grep("^--file=", ca)])
root <- if (length(fa)) dirname(dirname(normalizePath(fa[1]))) else getwd()
if (!file.exists(file.path(root, "R", "biosRank.R"))) {
  root <- "E:/GExPipe/GExPipe(original_paper)"
}
source(file.path(root, "R", "biosRank.R"))

outdir <- file.path(root, "results", "paper_multigse_compare")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
destdir <- file.path(tempdir(), "GEO_paper_compare")
dir.create(destdir, showWarnings = FALSE, recursive = TRUE)

## Published 11 key genes from Mosharaf et al. 2023
paper_KG <- c(
  "CXCL8", "CEMIP", "MMP7", "CA4", "ADH1C", "GUCA2A",
  "GUCA2B", "ZG16", "CLCA4", "MS4A12", "CLDN1"
)

## Prefer same-platform Affy U133 Plus 2.0 studies from that paper (stable merge).
## GSE35279 (Agilent) + GSE50760 (RNA-seq) omitted here for a clean joint limma;
## they are still cited in the paper KG list for external concordance.
gse_ids <- c("GSE9348", "GSE110224", "GSE23878")

disease_rx <- "tumor|tumour|cancer|carcinoma|crc|adenocarcinoma|disease|case"
normal_rx  <- "normal|healthy|control|nontumor|non-tumor"

prep_gse <- function(gse_id) {
  cat("Downloading ", gse_id, "...\n", sep = "")
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
  if (length(unique(na.omit(cond[ok]))) < 2L) {
    stop(gse_id, ": could not assign Normal/Disease. Check metadata.")
  }
  expr <- expr[, ok, drop = FALSE]
  cond <- factor(cond[ok], levels = c("Normal", "Disease"))

  fd <- fData(gset)
  sym_col <- grep("symbol|gene.?symbol|gene_assignment", names(fd),
                  ignore.case = TRUE, value = TRUE)
  if (!length(sym_col)) stop(gse_id, ": no gene symbol column in fData")
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

  list(expr = expr, Condition = cond, Batch = factor(rep(gse_id, ncol(expr))), gse = gse_id)
}

## ---- Load studies ----
parts <- lapply(gse_ids, prep_gse)

## ---- A) Paper-style: separate limma per GSE → intersect significant DEGs ----
sep_sig <- list()
for (p in parts) {
  des <- model.matrix(~ p$Condition)
  fit <- eBayes(lmFit(p$expr, des))
  tt <- topTable(fit, coef = 2, number = Inf, sort.by = "P")
  sig <- rownames(tt)[tt$adj.P.Val < 0.05 & abs(tt$logFC) > 0.5]
  sep_sig[[p$gse]] <- sig
  cat("Separate limma ", p$gse, ": sig DEGs = ", length(sig), "\n", sep = "")
}
paper_style_intersect <- Reduce(intersect, sep_sig)
cat("Paper-style ∩ significant DEGs across ", length(gse_ids),
    " GSEs: ", length(paper_style_intersect), "\n", sep = "")

## Rank ∩ genes by mean |logFC| across studies
lfc_by_study <- lapply(parts, function(p) {
  des <- model.matrix(~ p$Condition)
  fit <- eBayes(lmFit(p$expr, des))
  tt <- topTable(fit, coef = 2, number = Inf, sort.by = "none")
  setNames(tt$logFC, rownames(tt))
})
avg_abs <- sapply(paper_style_intersect, function(g) {
  mean(abs(sapply(lfc_by_study, function(v) {
    if (g %in% names(v)) v[[g]] else NA_real_
  })), na.rm = TRUE)
})
intersect_ranked <- paper_style_intersect[order(-avg_abs)]
intersect_top20 <- head(intersect_ranked, 20)

## ---- B) Joint limma ~ Condition + Batch (JPCT/BIOS style) ----
common <- Reduce(intersect, lapply(parts, function(p) rownames(p$expr)))
cat("Common genes for merge: ", length(common), "\n", sep = "")

## within-study z-score then merge
expr_list <- lapply(parts, function(p) {
  x <- p$expr[common, , drop = FALSE]
  x <- t(scale(t(x)))
  x[!is.finite(x)] <- 0
  x
})
expr <- do.call(cbind, expr_list)
Condition <- factor(unlist(lapply(parts, function(p) as.character(p$Condition))),
                    levels = c("Normal", "Disease"))
Batch <- factor(unlist(lapply(parts, function(p) as.character(p$Batch))))

design <- model.matrix(~ Condition + Batch)
fit_joint <- eBayes(lmFit(expr, design))
tt_joint <- topTable(fit_joint, coef = 2, number = Inf, sort.by = "P")
limma_top20 <- head(rownames(tt_joint), 20)

## ---- C) BIOS-Rank on same joint fit ----
bios <- biosRank(fit_joint, coef_condition = 2, coef_platform = 3)
bios_top20 <- topBIOS(bios, n = 20)$Gene

## ---- D) Compare to published paper KGs ----
overlap <- function(a, b) {
  a <- unique(as.character(a)); b <- unique(as.character(b))
  hit <- intersect(a, b)
  list(n = length(hit), genes = paste(sort(hit), collapse = ";"),
       frac_a = length(hit) / max(1, length(a)),
       frac_b = length(hit) / max(1, length(b)),
       jaccard = length(hit) / length(union(a, b)))
}

cmp <- rbind(
  data.frame(
    Method = "Paper_style_intersect_top20",
    k = length(intersect_top20),
    Overlap_published_11KG = overlap(intersect_top20, paper_KG)$n,
    Genes_shared_with_paper = overlap(intersect_top20, paper_KG)$genes,
    Frac_of_panel = overlap(intersect_top20, paper_KG)$frac_a,
    Frac_of_paperKG = overlap(intersect_top20, paper_KG)$frac_b,
    Jaccard_vs_paperKG = overlap(intersect_top20, paper_KG)$jaccard,
    stringsAsFactors = FALSE
  ),
  data.frame(
    Method = "Joint_limma_top20",
    k = 20,
    Overlap_published_11KG = overlap(limma_top20, paper_KG)$n,
    Genes_shared_with_paper = overlap(limma_top20, paper_KG)$genes,
    Frac_of_panel = overlap(limma_top20, paper_KG)$frac_a,
    Frac_of_paperKG = overlap(limma_top20, paper_KG)$frac_b,
    Jaccard_vs_paperKG = overlap(limma_top20, paper_KG)$jaccard,
    stringsAsFactors = FALSE
  ),
  data.frame(
    Method = "BIOS_Rank_top20",
    k = 20,
    Overlap_published_11KG = overlap(bios_top20, paper_KG)$n,
    Genes_shared_with_paper = overlap(bios_top20, paper_KG)$genes,
    Frac_of_panel = overlap(bios_top20, paper_KG)$frac_a,
    Frac_of_paperKG = overlap(bios_top20, paper_KG)$frac_b,
    Jaccard_vs_paperKG = overlap(bios_top20, paper_KG)$jaccard,
    stringsAsFactors = FALSE
  )
)

## pairwise method overlaps
pair <- data.frame(
  Comparison = c("BIOS ∩ Joint_limma", "BIOS ∩ Paper_style_∩", "Joint_limma ∩ Paper_style_∩"),
  n_shared = c(
    length(intersect(bios_top20, limma_top20)),
    length(intersect(bios_top20, intersect_top20)),
    length(intersect(limma_top20, intersect_top20))
  ),
  genes = c(
    paste(sort(intersect(bios_top20, limma_top20)), collapse = ";"),
    paste(sort(intersect(bios_top20, intersect_top20)), collapse = ";"),
    paste(sort(intersect(limma_top20, intersect_top20)), collapse = ";")
  ),
  stringsAsFactors = FALSE
)

## Which published KGs each method recovers
kg_map <- data.frame(
  Paper_KG = paper_KG,
  In_BIOS_top20 = paper_KG %in% bios_top20,
  In_JointLimma_top20 = paper_KG %in% limma_top20,
  In_PaperStyle_intersect_top20 = paper_KG %in% intersect_top20,
  In_PaperStyle_intersect_ALL = paper_KG %in% paper_style_intersect,
  stringsAsFactors = FALSE
)

utils::write.csv(cmp, file.path(outdir, "comparison_vs_published_11KG.csv"), row.names = FALSE)
utils::write.csv(pair, file.path(outdir, "pairwise_method_overlap.csv"), row.names = FALSE)
utils::write.csv(kg_map, file.path(outdir, "published_KG_recovery.csv"), row.names = FALSE)
utils::write.csv(data.frame(Gene = bios_top20), file.path(outdir, "BIOS_top20.csv"), row.names = FALSE)
utils::write.csv(data.frame(Gene = limma_top20), file.path(outdir, "Joint_limma_top20.csv"), row.names = FALSE)
utils::write.csv(data.frame(Gene = intersect_top20), file.path(outdir, "PaperStyle_intersect_top20.csv"), row.names = FALSE)
utils::write.csv(topBIOS(bios, 20), file.path(outdir, "BIOS_top20_scores.csv"), row.names = FALSE)

md <- c(
  "# Multi-GSE paper reproduction vs BIOS-Rank",
  "",
  "**Reference paper:** Mosharaf et al., *BMC Medical Genomics* (2023).",
  "DOI: [10.1186/s12920-023-01488-w](https://doi.org/10.1186/s12920-023-01488-w)",
  "",
  "Published 11 key genes:",
  paste0("`", paste(paper_KG, collapse = "`, `"), "`"),
  "",
  paste0("**GSEs used here (Affy GPL570 subset of that paper):** ",
         paste(gse_ids, collapse = ", "), "."),
  "(GSE35279 Agilent + GSE50760 RNA-seq omitted for same-platform joint limma.)",
  "",
  paste0("Paper-style ∩ significant DEGs (all ranks): **",
         length(paper_style_intersect), "** genes."),
  "",
  "## Overlap with published 11 KGs",
  "",
  "| Method | Hits in 11 KG | Genes shared | Frac of paper KG |",
  "|--------|---------------:|--------------|-----------------:|"
)
for (i in seq_len(nrow(cmp))) {
  md <- c(md, paste0(
    "| ", cmp$Method[i], " | ", cmp$Overlap_published_11KG[i], " | ",
    cmp$Genes_shared_with_paper[i], " | ",
    format(round(cmp$Frac_of_paperKG[i], 3), nsmall = 3), " |"
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
  "Compare which method recovers more of the **independent published** 11-gene CRC signature",
  "from the same multi-GSE literature context.",
  "",
  paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE))
)
writeLines(md, file.path(outdir, "comparison_report.md"))

cat("\n===== COMPARISON VS PUBLISHED 11 KEY GENES =====\n")
print(cmp, row.names = FALSE)
cat("\n===== PAIRWISE =====\n")
print(pair[, 1:2], row.names = FALSE)
cat("\n===== PAPER KG RECOVERY =====\n")
print(kg_map, row.names = FALSE)
cat("\nWrote: ", outdir, "\n", sep = "")
)
