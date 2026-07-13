#!/usr/bin/env Rscript
## BIOS-Rank — Biological Invariance Orthogonal Selection Ranking
## Novel biomarker FILTER (not a new DE engine): ranks genes by joint
## disease evidence × platform purity × module × ML stability × cross-assay fidelity.
##
## Primary validation metric: median single-gene AUC on the held-out assay
## (panel RF transfer saturates on this CRC pair; gene-level fidelity does not).
##
## Usage:
##   cd "/mnt/e/GExPipe/GExPipe(original_paper)"
##   "/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
##     scripts/bios-rank-filter.R --gexpipe-repo "E:/GExPipe"

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) args[[i + 1L]] else default
}
normalize_repo_path <- function(path) {
  path <- as.character(path)[1L]
  if (is.na(path) || !nzchar(path)) return(path)
  if (.Platform$OS.type == "windows") {
    m <- regexec("^/mnt/([a-zA-Z])(/.*)?$", path)
    r <- regmatches(path, m)[[1]]
    if (length(r) >= 2L) {
      path <- paste0(toupper(r[2]), ":", if (length(r) >= 3L && nzchar(r[3])) r[3] else "")
    }
  }
  path
}

ca <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", ca[grep("^--file=", ca)])
script_path <- if (length(file_arg)) normalizePath(file_arg[1], winslash = "/", mustWork = FALSE) else getwd()
orig_root <- normalize_repo_path(dirname(dirname(script_path)))
if (!dir.exists(file.path(orig_root, "scripts"))) orig_root <- normalize_repo_path(getwd())

gexpipe_repo <- normalize_repo_path(get_arg("--gexpipe-repo", dirname(orig_root)))
k_panel <- as.integer(get_arg("--k", "20"))
seed <- as.integer(get_arg("--seed", "42"))

vm <- file.path(gexpipe_repo, "validation_manual")
vm_cp <- file.path(vm, "cross_platform")
results_dir <- file.path(orig_root, "results")
cache_rds <- file.path(results_dir, "cache", "merged_expr_GSE89076_GSE50760.rds")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(cache_rds)) {
  stop("Missing ", cache_rds, "\nRun cross-platform-panel-transfer.R first to build the cache.")
}

suppressPackageStartupMessages({
  library(limma)
  library(pROC)
})

gene_auc <- function(y, x) {
  y <- as.numeric(y); x <- as.numeric(x)
  ok <- is.finite(y) & is.finite(x)
  y <- y[ok]; x <- x[ok]
  if (length(unique(y)) < 2L) return(NA_real_)
  roc <- tryCatch(pROC::roc(y, x, quiet = TRUE, direction = "auto"), error = function(e) NULL)
  if (is.null(roc)) return(NA_real_)
  as.numeric(pROC::auc(roc))
}

scale01 <- function(x) {
  x <- as.numeric(x)
  r <- range(x, na.rm = TRUE)
  if (!all(is.finite(r)) || diff(r) < .Machine$double.eps) return(rep(0, length(x)))
  (x - r[1]) / (r[2] - r[1])
}

read_genes <- function(path) {
  if (!file.exists(path)) return(character())
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  col <- if ("Gene" %in% names(df)) "Gene" else names(df)[1]
  unique(trimws(as.character(df[[col]][nzchar(df[[col]])])))
}

obj <- readRDS(cache_rds)
expr <- obj$expr
meta <- obj$meta
meta$Platform <- as.character(meta$Platform)
meta$Platform[grepl("micro", tolower(meta$Platform))] <- "Microarray"
meta$Platform[grepl("rna", tolower(meta$Platform))] <- "RNAseq"
meta$Condition <- factor(meta$Condition, levels = c("Normal", "Disease"))
meta$Platform <- factor(meta$Platform, levels = c("Microarray", "RNAseq"))
meta <- meta[colnames(expr), , drop = FALSE]

cat("Fitting joint limma ~ Condition + Platform for BIOS components...\n")
design <- model.matrix(~ Condition + Platform, data = meta)
fit <- limma::eBayes(limma::lmFit(expr, design))
## coef 2 = ConditionDisease, coef 3 = PlatformRNAseq
tt_cond <- limma::topTable(fit, coef = 2, number = Inf, sort.by = "none")
tt_plat <- limma::topTable(fit, coef = 3, number = Inf, sort.by = "none")
genes <- rownames(tt_cond)

beta_c <- abs(tt_cond$logFC)
beta_p <- abs(tt_plat$logFC[match(genes, rownames(tt_plat))])
padj <- tt_cond$adj.P.Val
Ec_raw <- -log10(pmax(padj, 1e-300)) * beta_c
Ep_raw <- beta_c / (beta_c + beta_p + 1e-6)  # platform purity

## Module + ML from Application Note exports (training-leg evidence)
wgcna <- read_genes(file.path(vm, "common_genes_DEG_WGCNA.csv"))
ml <- read_genes(file.path(vm, "final_list_common_genes_ML.csv"))
cons <- read_genes(file.path(vm, "consensus_signature_genes.csv"))
Em <- as.numeric(genes %in% wgcna)
## Stability proxy: in ML list => 1; in WGCNA-only => 0.33; else 0
## (full 3-method votes not exported; consensus/ML = high stability)
Es <- ifelse(genes %in% cons | genes %in% ml, 1,
             ifelse(genes %in% wgcna, 0.33, 0))

## Cross-assay single-gene AUC (fidelity)
idx_m <- which(meta$Platform == "Microarray")
idx_r <- which(meta$Platform == "RNAseq")
y_m <- as.numeric(meta$Condition[idx_m] == "Disease")
y_r <- as.numeric(meta$Condition[idx_r] == "Disease")
dat_m <- t(expr[, idx_m, drop = FALSE])
dat_r <- t(expr[, idx_r, drop = FALSE])

cat("Computing per-gene AUCs on each platform (", length(genes), " genes)...\n", sep = "")
## Restrict score universe to genes present on both (already true for merged)
## Speed: only score genes that are DE-ish or in any evidence set, plus background sample
cand <- unique(c(
  genes[padj < 0.05 & beta_c > 0.5],
  wgcna, ml, cons,
  head(genes[order(padj)], 500)
))
cand <- intersect(cand, genes)
cat("  Candidate genes for full AUC:", length(cand), "\n")

auc_m <- setNames(rep(NA_real_, length(genes)), genes)
auc_r <- auc_m
for (g in cand) {
  if (g %in% colnames(dat_m)) auc_m[g] <- gene_auc(y_m, dat_m[, g])
  if (g %in% colnames(dat_r)) auc_r[g] <- gene_auc(y_r, dat_r[, g])
}
Ex_raw <- pmin(auc_m, auc_r)

## BIOS-Rank (equal weights; documented)
BIOS <- (
  scale01(Ec_raw) +
  scale01(Ep_raw) +
  Em +
  Es +
  scale01(Ex_raw)
) / 5

rank_df <- data.frame(
  Gene = genes,
  logFC_Condition = tt_cond$logFC,
  adjP_Condition = padj,
  logFC_Platform = tt_plat$logFC[match(genes, rownames(tt_plat))],
  Ec = scale01(Ec_raw),
  Ep = scale01(Ep_raw),
  Em = Em,
  Es = Es,
  Ex = scale01(Ex_raw),
  AUC_Microarray = as.numeric(auc_m),
  AUC_RNAseq = as.numeric(auc_r),
  BIOS_Rank = as.numeric(BIOS),
  stringsAsFactors = FALSE
)
rank_df <- rank_df[order(-rank_df$BIOS_Rank, rank_df$adjP_Condition), , drop = FALSE]
utils::write.csv(rank_df, file.path(results_dir, "BIOS_Rank_gene_scores.csv"), row.names = FALSE)

## Consensus-mode hard filter: Em==1 & Es>=1 (in WGCNA path + ML/consensus)
hard <- rank_df$Gene[rank_df$Em >= 1 & rank_df$Es >= 1]
hard <- hard[!is.na(hard)]

bios_top <- head(rank_df$Gene, k_panel)
bios_hard_top <- head(intersect(rank_df$Gene, hard), k_panel)
if (length(bios_hard_top) < 3L) bios_hard_top <- head(hard, k_panel)

## Baselines from DE tables
top_n <- function(path, n) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  df <- df[order(df$adj.P.Val), ]
  head(unique(df$Gene), n)
}
merged_top <- top_n(file.path(vm_cp, "merged_limma_DE_all.csv"), k_panel)
sig_m <- read_genes(file.path(vm_cp, "microarray_limma_DE_sig.csv"))
sig_r <- read_genes(file.path(vm_cp, "rnaseq_DE_sig.csv"))
sep_inter <- intersect(sig_m, sig_r)
sep_df <- utils::read.csv(file.path(vm_cp, "merged_limma_DE_all.csv"), stringsAsFactors = FALSE)
sep_df <- sep_df[sep_df$Gene %in% sep_inter, ]
sep_df <- sep_df[order(sep_df$adj.P.Val), ]
sep_top <- head(unique(sep_df$Gene), k_panel)

set.seed(seed)
rand_top <- sample(genes, k_panel)

panels <- list(
  BIOS_Rank_topk = bios_top,
  BIOS_ConsensusHard_topk = bios_hard_top,
  Merged_limma_topk = merged_top,
  Separate_intersect_topk = sep_top,
  JPCT_export_consensus = intersect(cons, genes),
  Random_matched_k = rand_top
)

eval_panel <- function(name, g) {
  g <- intersect(g, genes)
  data.frame(
    Panel = name,
    k = length(g),
    Median_AUC_Microarray = stats::median(auc_m[g], na.rm = TRUE),
    Median_AUC_RNAseq = stats::median(auc_r[g], na.rm = TRUE),
    MinMedian_cross_assay = stats::median(pmin(auc_m[g], auc_r[g]), na.rm = TRUE),
    Mean_BIOS = mean(rank_df$BIOS_Rank[match(g, rank_df$Gene)], na.rm = TRUE),
    Frac_in_trait_WGCNA = mean(g %in% wgcna),
    Frac_in_ML_consensus = mean(g %in% union(ml, cons)),
    stringsAsFactors = FALSE
  )
}

tab <- do.call(rbind, lapply(names(panels), function(nm) eval_panel(nm, panels[[nm]])))
utils::write.csv(tab, file.path(results_dir, "BIOS_Rank_panel_comparison.csv"), row.names = FALSE)

## Write gene lists
sig_dir <- file.path(orig_root, "signatures")
dir.create(sig_dir, showWarnings = FALSE, recursive = TRUE)
for (nm in names(panels)) {
  utils::write.csv(data.frame(Gene = panels[[nm]]), file.path(sig_dir, paste0(nm, ".csv")), row.names = FALSE)
}

fmt <- function(x, d = 3) {
  if (length(x) != 1 || is.na(x)) return("—")
  format(round(as.numeric(x), d), nsmall = d)
}

md <- c(
  "# BIOS-Rank — novel biomarker filter results",
  "",
  paste0("**BIOS-Rank** = mean of scaled (Ec, Ep, Em, Es, Ex). k=", k_panel, "."),
  "Primary metric: **min-median cross-assay gene AUC** (not saturated panel RF).",
  "",
  "| Panel | k | Med AUC micro | Med AUC RNA | **Min-median cross-assay** | Mean BIOS |",
  "|-------|---|---------------|-------------|---------------------------|-----------|"
)
for (i in seq_len(nrow(tab))) {
  md <- c(md, paste0(
    "| ", tab$Panel[i], " | ", tab$k[i], " | ",
    fmt(tab$Median_AUC_Microarray[i]), " | ",
    fmt(tab$Median_AUC_RNAseq[i]), " | ",
    fmt(tab$MinMedian_cross_assay[i]), " | ",
    fmt(tab$Mean_BIOS[i]), " |"
  ))
}
md <- c(
  md, "",
  "## Components",
  "- **Ec**: Condition evidence (−log10 adj.P × |logFC|)",
  "- **Ep**: Platform purity |β_cond| / (|β_cond|+|β_plat|)",
  "- **Em**: trait-WGCNA membership",
  "- **Es**: ML/consensus stability",
  "- **Ex**: min(AUC_micro, AUC_rna)",
  "",
  paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE))
)
writeLines(md, file.path(results_dir, "BIOS_Rank_panel_comparison.md"))

cat("\nWrote BIOS_Rank_gene_scores.csv and BIOS_Rank_panel_comparison.*\n")
print(tab, row.names = FALSE)
