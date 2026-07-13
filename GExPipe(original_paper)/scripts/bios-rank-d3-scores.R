#!/usr/bin/env Rscript
## Build D3 (GSE9348 × GSE50760) BIOS channel scores from merged cache,
## then apply D1-locked weights (non-circular Ex_train protocol).
##
## Usage:
##   cd /mnt/e/GExPipe/GExPipe(original_paper)
##   "/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
##     scripts/bios-rank-d3-scores.R --gexpipe-repo "E:/GExPipe"

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

results_dir <- file.path(orig_root, "results")
cache_rds <- file.path(results_dir, "cache", "merged_expr_GSE9348_GSE50760.rds")
lock_path <- file.path(results_dir, "BIOS_v2_locked_weights.csv")
vm <- file.path(gexpipe_repo, "validation_manual")

if (!file.exists(cache_rds)) stop("Missing ", cache_rds)
if (!file.exists(lock_path)) stop("Missing ", lock_path, " — run bios-rank-v2-correctness.R first")

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
meta$Condition <- factor(as.character(meta$Condition), levels = c("Normal", "Disease"))
meta$Platform <- factor(meta$Platform, levels = c("Microarray", "RNAseq"))
meta <- meta[colnames(expr), , drop = FALSE]

cat("D3 limma ~ Condition + Platform...\n")
design <- model.matrix(~ Condition + Platform, data = meta)
fit <- limma::eBayes(limma::lmFit(expr, design))
tt_cond <- limma::topTable(fit, coef = 2, number = Inf, sort.by = "none")
tt_plat <- limma::topTable(fit, coef = 3, number = Inf, sort.by = "none")
genes <- rownames(tt_cond)
beta_c <- abs(tt_cond$logFC)
beta_p <- abs(tt_plat$logFC[match(genes, rownames(tt_plat))])
padj <- tt_cond$adj.P.Val
Ec_raw <- -log10(pmax(padj, 1e-300)) * beta_c
Ep_raw <- beta_c / (beta_c + beta_p + 1e-6)

## Module/ML: reuse Application Note lists as weak prior (D3 may not have its own WGCNA)
wgcna <- read_genes(file.path(vm, "common_genes_DEG_WGCNA.csv"))
ml <- read_genes(file.path(vm, "final_list_common_genes_ML.csv"))
cons <- read_genes(file.path(vm, "consensus_signature_genes.csv"))
Em <- as.numeric(genes %in% wgcna)
Es <- ifelse(genes %in% cons | genes %in% ml, 1, ifelse(genes %in% wgcna, 0.33, 0))

idx_m <- which(meta$Platform == "Microarray")
idx_r <- which(meta$Platform == "RNAseq")
y_m <- as.numeric(meta$Condition[idx_m] == "Disease")
y_r <- as.numeric(meta$Condition[idx_r] == "Disease")
dat_m <- t(expr[, idx_m, drop = FALSE])
dat_r <- t(expr[, idx_r, drop = FALSE])

cand <- unique(c(genes[padj < 0.05 & beta_c > 0.5], wgcna, ml, cons, head(genes[order(padj)], 800)))
cand <- intersect(cand, genes)
cat("AUC candidates:", length(cand), "\n")
auc_m <- setNames(rep(NA_real_, length(genes)), genes)
auc_r <- auc_m
for (g in cand) {
  if (g %in% colnames(dat_m)) auc_m[g] <- gene_auc(y_m, dat_m[, g])
  if (g %in% colnames(dat_r)) auc_r[g] <- gene_auc(y_r, dat_r[, g])
}

rank_df <- data.frame(
  Gene = genes,
  adjP_Condition = padj,
  Ec = scale01(Ec_raw),
  Ep = scale01(Ep_raw),
  Em = Em,
  Es = Es,
  Ex = scale01(pmin(auc_m, auc_r)),
  AUC_Microarray = as.numeric(auc_m),
  AUC_RNAseq = as.numeric(auc_r),
  stringsAsFactors = FALSE
)
ok <- is.finite(rank_df$AUC_Microarray) & is.finite(rank_df$AUC_RNAseq)
rank_df <- rank_df[ok, , drop = FALSE]
rank_df$Ex_train <- scale01(rank_df$AUC_Microarray)
utils::write.csv(rank_df, file.path(results_dir, "BIOS_Rank_D3_gene_scores.csv"), row.names = FALSE)

## Apply locked / equal weights; eval held-out RNA
locks <- utils::read.csv(lock_path, stringsAsFactors = FALSE)
w_ex <- locks$Weight[locks$Protocol == "ExTrain_locked"]
names(w_ex) <- locks$Channel[locks$Protocol == "ExTrain_locked"]
w_nx <- locks$Weight[locks$Protocol == "noEx_locked"]
names(w_nx) <- sub("^noEx_", "", locks$Channel[locks$Protocol == "noEx_locked"])

score <- function(cols, w) {
  m <- as.matrix(rank_df[, cols, drop = FALSE])
  m[!is.finite(m)] <- 0
  w <- as.numeric(w[cols])
  w <- w / sum(w)
  as.numeric(m %*% w)
}
topk <- function(sc) rank_df$Gene[head(order(-sc, rank_df$adjP_Condition), k_panel)]
eval_rna <- function(g) stats::median(rank_df$AUC_RNAseq[match(g, rank_df$Gene)], na.rm = TRUE)
eval_mm <- function(g) {
  i <- match(g, rank_df$Gene)
  stats::median(pmin(rank_df$AUC_Microarray[i], rank_df$AUC_RNAseq[i]), na.rm = TRUE)
}

cols5 <- c("Ec", "Ep", "Em", "Es", "Ex_train")
cols4 <- c("Ec", "Ep", "Em", "Es")
methods <- list(
  D3_ExTrain_equal = topk(score(cols5, setNames(rep(0.2, 5), cols5))),
  D3_ExTrain_locked_from_D1 = topk(score(cols5, w_ex)),
  D3_noEx_equal = topk(score(cols4, setNames(rep(0.25, 4), cols4))),
  D3_noEx_locked_from_D1 = topk(score(cols4, w_nx)),
  D3_limma_proxy = rank_df$Gene[head(order(rank_df$adjP_Condition), k_panel)]
)
set.seed(42)
methods$D3_random <- sample(rank_df$Gene, k_panel)

out <- do.call(rbind, lapply(names(methods), function(nm) {
  g <- methods[[nm]]
  data.frame(
    Method = nm,
    Med_AUC_RNA_heldout = eval_rna(g),
    MinMedian_both = eval_mm(g),
    stringsAsFactors = FALSE
  )
}))
utils::write.csv(out, file.path(results_dir, "BIOS_v2_D3_locked_transfer.csv"), row.names = FALSE)

md <- c(
  "# D3 locked-weight transfer (BIOS v2)",
  "",
  "Weights locked on D1 (maximize RNA held-out AUC); applied to GSE9348 × GSE50760.",
  "",
  "| Method | Med AUC RNA | MinMedian |",
  "|--------|------------:|----------:|"
)
for (i in seq_len(nrow(out))) {
  md <- c(md, paste0(
    "| ", out$Method[i], " | ",
    format(round(out$Med_AUC_RNA_heldout[i], 3), nsmall = 3), " | ",
    format(round(out$MinMedian_both[i], 3), nsmall = 3), " |"
  ))
}
writeLines(md, file.path(results_dir, "BIOS_v2_D3_locked_transfer.md"))
cat("Wrote BIOS_Rank_D3_gene_scores.csv and BIOS_v2_D3_locked_transfer.*\n")
print(out, row.names = FALSE)
