#!/usr/bin/env Rscript
## Example: download a GEO series and apply BIOS-Rank (standalone)
##
## Single-platform (default): Ec + optional simple proxies for Em/Es
## Two-platform: set --gse2 and --platform-labels for full Ep + held-out Ex_train
##
## Usage (WSL → Windows R):
##   cd "/mnt/e/GExPipe/GExPipe(original_paper)"   # or your BIOS-Rank clone
##   "/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
##     scripts/example-bios-rank-geo.R \
##     --gse GSE9348 \
##     --k 20 \
##     --outdir "E:/BIOS-Rank-upload/results/example_GSE9348"
##
## Phenotype: edit the rules below or pass --disease-regex / --normal-regex

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) args[[i + 1L]] else default
}

gse_id <- get_arg("--gse", "GSE9348")
k_panel <- as.integer(get_arg("--k", "20"))
outdir <- get_arg("--outdir", file.path(getwd(), "results", paste0("example_", gse_id)))
disease_rx <- get_arg("--disease-regex",
                      "tumor|tumour|cancer|carcinoma|adenocarcinoma|crc|disease|patient")
normal_rx <- get_arg("--normal-regex", "normal|healthy|control|nontumor|non-tumor")
destdir <- get_arg("--destdir", file.path(tempdir(), "GEO_BIOS"))

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
dir.create(destdir, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(GEOquery)
  library(Biobase)
  library(limma)
})

scale01 <- function(x) {
  x <- as.numeric(x)
  r <- range(x, na.rm = TRUE)
  if (!all(is.finite(r)) || diff(r) < .Machine$double.eps) return(rep(0, length(x)))
  (x - r[1]) / (r[2] - r[1])
}

cat("Downloading ", gse_id, " ...\n", sep = "")
gset <- getGEO(gse_id, GSEMatrix = TRUE, getGPL = TRUE, destdir = destdir)
if (length(gset) > 1L) {
  ## pick largest ExpressionSet
  ns <- vapply(gset, ncol, integer(1))
  gset <- gset[[which.max(ns)]]
} else {
  gset <- gset[[1]]
}

expr <- exprs(gset)
pdata <- pData(gset)

## Use title / source_name / characteristics only (avoid data_processing "normalized")
pick <- unique(c(
  intersect(c("title", "source_name_ch1"), names(pdata)),
  grep("^characteristics", names(pdata), value = TRUE, ignore.case = TRUE)
))
if (!length(pick)) pick <- names(pdata)[seq_len(min(3L, ncol(pdata)))]
txt <- apply(pdata[, pick, drop = FALSE], 1, function(r) {
  paste(tolower(as.character(r)), collapse = " | ")
})

cond <- rep(NA_character_, length(txt))
cond[grepl(normal_rx, txt, ignore.case = TRUE)] <- "Normal"
cond[is.na(cond) & grepl(disease_rx, txt, ignore.case = TRUE)] <- "Disease"
## If both match, prefer Normal when "normal" is explicit and not "tumor"
both <- grepl(normal_rx, txt, ignore.case = TRUE) & grepl(disease_rx, txt, ignore.case = TRUE)
cond[both & grepl("\\bnormal\\b|healthy|control", txt)] <- "Normal"

ok <- !is.na(cond)
if (length(unique(cond[ok])) < 2L) {
  stop(
    "Could not find both Normal and Disease. ",
    "Disease n=", sum(cond == "Disease", na.rm = TRUE),
    " Normal n=", sum(cond == "Normal", na.rm = TRUE),
    "\nInspect pdata titles and pass --disease-regex / --normal-regex.\n",
    "Example titles:\n", paste(head(unique(pdata$title), 8), collapse = "\n")
  )
}

expr <- expr[, ok, drop = FALSE]
cond <- factor(cond[ok], levels = c("Normal", "Disease"))
cat("Samples kept: ", ncol(expr),
    " Disease=", sum(cond == "Disease"),
    " Normal=", sum(cond == "Normal"), "\n", sep = "")

## Gene IDs: prefer SYMBOL in fData
fd <- fData(gset)
sym_col <- grep("symbol|gene.?symbol|gene_assignment", names(fd),
                ignore.case = TRUE, value = TRUE)
if (length(sym_col)) {
  syms <- as.character(fd[rownames(expr), sym_col[1]])
  ## Affymetrix gene_assignment often "SYMBOL // ..."
  syms <- sub("\\s*//.*$", "", syms)
  syms <- trimws(syms)
  keep <- !is.na(syms) & nzchar(syms) & syms != "---"
  expr <- expr[keep, , drop = FALSE]
  syms <- syms[keep]
  if (anyDuplicated(syms)) {
    expr <- limma::avereps(expr, ID = syms)
  } else {
    rownames(expr) <- syms
  }
}

## log if looks like raw intensities
if (max(expr, na.rm = TRUE) > 100) {
  cat("Log2-transforming expression...\n")
  expr <- log2(expr + 1)
}

## -------------------------------------------------------------------------
## limma DE  (single platform → ~ Condition only; Ep set to 1)
## -------------------------------------------------------------------------
design <- model.matrix(~ cond)
colnames(design) <- c("Intercept", "ConditionDisease")
fit <- eBayes(lmFit(expr, design))
tt <- topTable(fit, coef = "ConditionDisease", number = Inf, sort.by = "none")
genes <- rownames(tt)

beta_c <- abs(tt$logFC)
padj <- tt$adj.P.Val
Ec <- scale01((-log10(pmax(padj, 1e-300))) * beta_c)
Ep <- rep(1, length(genes))  # no Platform term on single assay

## Simple Em/Es proxies without full WGCNA/ML (optional upgrades below):
## Em: top 20% |logFC| among FDR<0.05 as "programme-ish" proxy (NOT true WGCNA)
## Es: 1 if FDR<0.01 & |logFC|>1 else 0.33 if FDR<0.05 else 0
sig <- padj < 0.05
strong <- padj < 0.01 & beta_c > 1
Em <- as.numeric(sig & beta_c >= stats::quantile(beta_c[sig], 0.8, na.rm = TRUE))
Es <- ifelse(strong, 1, ifelse(sig, 0.33, 0))

BIOS <- (Ec + Ep + Em + Es) / 4

out <- data.frame(
  Gene = genes,
  logFC = tt$logFC,
  adj.P.Val = padj,
  Ec = Ec,
  Ep = Ep,
  Em = Em,
  Es = Es,
  BIOS_Rank = as.numeric(BIOS),
  stringsAsFactors = FALSE
)
out <- out[order(-out$BIOS_Rank, out$adj.P.Val), ]
topk <- head(out, k_panel)

utils::write.csv(out, file.path(outdir, paste0(gse_id, "_BIOS_all_genes.csv")), row.names = FALSE)
utils::write.csv(topk, file.path(outdir, paste0(gse_id, "_BIOS_top", k_panel, ".csv")), row.names = FALSE)
utils::write.csv(tt, file.path(outdir, paste0(gse_id, "_limma_DE_all.csv")), row.names = TRUE)

cat("\nTop ", k_panel, " BIOS genes:\n", sep = "")
print(topk[, c("Gene", "logFC", "adj.P.Val", "BIOS_Rank")], row.names = FALSE)
cat("\nWrote:\n  ", file.path(outdir, paste0(gse_id, "_BIOS_top", k_panel, ".csv")), "\n", sep = "")
cat("\nNOTE: On single-platform data, Ep=1 and Em/Es are simple proxies.\n",
    "For full BIOS (true WGCNA + ML + Ep), use a merged microarray+RNA-seq pair\n",
    "with scripts/bios-rank-filter.R / bios-rank-v2-correctness.R in this repo.\n", sep = "")
)
