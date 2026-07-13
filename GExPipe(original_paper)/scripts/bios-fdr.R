#!/usr/bin/env Rscript
## BIOS-FDR — permutation null / empirical FDR for BIOS-Rank top-k panels
##
## Novel add-on: calibrated specificity for the BIOS filter.
##   1) Compute observed BIOS (Ec, Ep, Em, Es, Ex)
##   2) Permute Condition labels B times (Platform fixed)
##   3) Refit limma ~ Condition + Platform; recompute Ec, Ep, Ex
##   4) Under null, gene-wise shuffle Em, Es (break module/ML coupling to labels)
##   5) Empirical FDR for top-k: mean_b(# null genes with BIOS >= t_(k)) / k
##      where t_(k) = k-th largest observed BIOS
##
## Also reports the same FDR for limma top-k (by adj.P) for fair comparison.
##
## Usage:
##   cd "/mnt/e/GExPipe/GExPipe(original_paper)"
##   "/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
##     scripts/bios-fdr.R --gexpipe-repo "E:/GExPipe" --n-perm 100

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
n_perm <- as.integer(get_arg("--n-perm", "100"))
seed <- as.integer(get_arg("--seed", "42"))
## Speed: compute Ex (gene AUC) only on this many top candidates by |t| each fit
n_auc <- as.integer(get_arg("--n-auc", "800"))

vm <- file.path(gexpipe_repo, "validation_manual")
results_dir <- file.path(orig_root, "results")
cache_rds <- file.path(results_dir, "cache", "merged_expr_GSE89076_GSE50760.rds")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(cache_rds)) {
  stop("Missing ", cache_rds, "\nRun cross-platform-panel-transfer.R first.")
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
  finite <- is.finite(x)
  if (!any(finite)) return(rep(0, length(x)))
  r <- range(x[finite])
  if (diff(r) < .Machine$double.eps) {
    out <- rep(0, length(x)); out[finite] <- 0; return(out)
  }
  out <- rep(0, length(x))
  out[finite] <- (x[finite] - r[1]) / (r[2] - r[1])
  out
}

read_genes <- function(path) {
  if (!file.exists(path)) return(character())
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  col <- if ("Gene" %in% names(df)) "Gene" else names(df)[1]
  unique(trimws(as.character(df[[col]][nzchar(as.character(df[[col]]))])))
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

wgcna <- read_genes(file.path(vm, "common_genes_DEG_WGCNA.csv"))
ml <- read_genes(file.path(vm, "final_list_common_genes_ML.csv"))
cons <- read_genes(file.path(vm, "consensus_signature_genes.csv"))

genes_all <- rownames(expr)
Em0 <- as.numeric(genes_all %in% wgcna)
Es0 <- ifelse(genes_all %in% union(cons, ml), 1,
              ifelse(genes_all %in% wgcna, 0.33, 0))
names(Em0) <- genes_all
names(Es0) <- genes_all

idx_m <- which(meta$Platform == "Microarray")
idx_r <- which(meta$Platform == "RNAseq")
dat_m <- t(expr[, idx_m, drop = FALSE])
dat_r <- t(expr[, idx_r, drop = FALSE])

## ---- Score engine ----
## Returns list(BIOS, adjP, components, top_genes)
compute_bios <- function(cond_factor, Em, Es, compute_ex = TRUE, auc_genes = NULL) {
  meta_loc <- meta
  meta_loc$Condition <- factor(cond_factor, levels = c("Normal", "Disease"))
  if (length(unique(meta_loc$Condition)) < 2L) {
    return(list(BIOS = setNames(rep(0, nrow(expr)), rownames(expr)), adjP = rep(1, nrow(expr))))
  }
  design <- model.matrix(~ Condition + Platform, data = meta_loc)
  fit <- limma::eBayes(limma::lmFit(expr, design))
  tt_c <- limma::topTable(fit, coef = 2, number = Inf, sort.by = "none")
  tt_p <- limma::topTable(fit, coef = 3, number = Inf, sort.by = "none")
  g <- rownames(tt_c)
  beta_c <- abs(tt_c$logFC)
  beta_p <- abs(tt_p$logFC[match(g, rownames(tt_p))])
  padj <- tt_c$adj.P.Val
  Ec <- scale01(-log10(pmax(padj, 1e-300)) * beta_c)
  Ep <- scale01(beta_c / (beta_c + beta_p + 1e-6))
  Em_g <- Em[g]
  Es_g <- Es[g]
  Em_g[is.na(Em_g)] <- 0
  Es_g[is.na(Es_g)] <- 0

  Ex <- rep(0, length(g))
  names(Ex) <- g
  if (compute_ex) {
    y_m <- as.numeric(meta_loc$Condition[idx_m] == "Disease")
    y_r <- as.numeric(meta_loc$Condition[idx_r] == "Disease")
    if (length(unique(y_m)) >= 2L && length(unique(y_r)) >= 2L) {
      if (is.null(auc_genes)) {
        ## pick strongest Condition |t|
        tt_ord <- order(tt_c$P.Value)
        auc_genes <- head(g[tt_ord], n_auc)
      }
      auc_genes <- intersect(auc_genes, g)
      for (gg in auc_genes) {
        a1 <- gene_auc(y_m, dat_m[, gg])
        a2 <- gene_auc(y_r, dat_r[, gg])
        Ex[gg] <- min(a1, a2, na.rm = TRUE)
      }
      Ex[!is.finite(Ex)] <- 0
    }
  }
  Ex_s <- scale01(Ex)
  BIOS <- (Ec + Ep + Em_g + Es_g + Ex_s) / 5
  names(BIOS) <- g
  list(
    BIOS = BIOS,
    adjP = setNames(padj, g),
    Ec = Ec, Ep = Ep, Em = Em_g, Es = Es_g, Ex = Ex_s
  )
}

cat("Observed BIOS...\n")
set.seed(seed)
obs <- compute_bios(meta$Condition, Em0, Es0, compute_ex = TRUE)
bios_obs <- obs$BIOS
ord <- order(-bios_obs, obs$adjP)
top_obs <- names(bios_obs)[ord][seq_len(min(k_panel, length(bios_obs)))]
thresh_k <- sort(bios_obs, decreasing = TRUE)[k_panel]
mean_top_obs <- mean(bios_obs[top_obs])

## Limma top-k observed
limma_obs <- names(sort(obs$adjP))[seq_len(k_panel)]
thresh_limma <- sort(obs$adjP)[k_panel]  # genes with adjP <= this

cat("Permutations B=", n_perm, " (this may take several minutes)...\n", sep = "")
null_n_ge_thresh <- integer(n_perm)
null_mean_topk <- numeric(n_perm)
null_n_limma <- integer(n_perm)
null_mean_limma_bios <- numeric(n_perm)
## Store max null BIOS for enrichment histogram
null_max <- numeric(n_perm)

cond0 <- as.character(meta$Condition)
auc_focus <- names(sort(obs$adjP))[seq_len(min(n_auc, length(obs$adjP)))]

for (b in seq_len(n_perm)) {
  set.seed(seed + b)
  ## Permute Condition only (Platform structure unchanged)
  cond_p <- sample(cond0)
  ## Shuffle Em/Es across genes under null
  Em_p <- sample(Em0)
  Es_p <- sample(Es0)
  names(Em_p) <- genes_all
  names(Es_p) <- genes_all

  nul <- compute_bios(cond_p, Em_p, Es_p, compute_ex = TRUE, auc_genes = auc_focus)
  nb <- nul$BIOS
  null_n_ge_thresh[b] <- sum(nb >= thresh_k, na.rm = TRUE)
  top_n <- names(sort(nb, decreasing = TRUE))[seq_len(k_panel)]
  null_mean_topk[b] <- mean(nb[top_n], na.rm = TRUE)
  null_max[b] <- max(nb, na.rm = TRUE)

  ## Limma-style null: count genes with adjP <= observed k-th adjP
  null_n_limma[b] <- sum(nul$adjP <= thresh_limma, na.rm = TRUE)
  limma_top_b <- names(sort(nul$adjP))[seq_len(k_panel)]
  null_mean_limma_bios[b] <- mean(nb[limma_top_b], na.rm = TRUE)

  if (b %% 10L == 0L) {
    cat("  perm ", b, "/", n_perm,
        "  null>=thresh:", null_n_ge_thresh[b],
        "  null_mean_topk:", round(null_mean_topk[b], 3), "\n", sep = "")
  }
}

## Empirical FDR for BIOS top-k list
## FDR = E[# null discoveries at observed threshold] / k
fdr_bios <- mean(null_n_ge_thresh) / k_panel
fdr_bios <- min(fdr_bios, 1)

## Empirical p for mean top-k BIOS (higher = better)
p_mean_topk <- (1 + sum(null_mean_topk >= mean_top_obs)) / (1 + n_perm)

## Limma top-k empirical FDR (same permutation engine)
fdr_limma <- mean(null_n_limma) / k_panel
fdr_limma <- min(fdr_limma, 1)

## Per-gene empirical p for observed top-k genes (optional detail)
## Using null max is weak; use fraction of null BIOS_g >= obs for that gene — need gene-aligned nulls
## Approximate with store of null BIOS for top_obs only across perms (recompute light)
cat("Per-gene empirical p for observed top-k (extra pass on stored null maxima style)...\n")
## Re-run short: for each perm we already don't store full vectors — estimate gene p via
## proportion of perms where null_max >= bios_obs[g] is too crude.
## Instead: one more compact loop storing null BIOS only for top_obs genes
gene_null_ge <- matrix(0L, nrow = length(top_obs), ncol = n_perm)
rownames(gene_null_ge) <- top_obs
for (b in seq_len(n_perm)) {
  set.seed(seed + b)
  cond_p <- sample(cond0)
  Em_p <- sample(Em0); Es_p <- sample(Es0)
  names(Em_p) <- genes_all; names(Es_p) <- genes_all
  nul <- compute_bios(cond_p, Em_p, Es_p, compute_ex = TRUE, auc_genes = intersect(auc_focus, top_obs))
  for (g in top_obs) {
    if (!is.na(nul$BIOS[g]) && nul$BIOS[g] >= bios_obs[g]) gene_null_ge[g, b] <- 1L
  }
  if (b %% 20L == 0L) cat("  gene-p perm ", b, "/", n_perm, "\n", sep = "")
}
gene_p <- (1 + rowSums(gene_null_ge)) / (1 + n_perm)

## Outputs
summary_df <- data.frame(
  Method = c("BIOS_Rank_topk", "limma_adjP_topk"),
  k = k_panel,
  n_perm = n_perm,
  Observed_threshold = c(thresh_k, thresh_limma),
  Mean_null_discoveries_at_threshold = c(mean(null_n_ge_thresh), mean(null_n_limma)),
  Empirical_FDR = c(fdr_bios, fdr_limma),
  Observed_mean_score_topk = c(mean_top_obs, mean(bios_obs[limma_obs], na.rm = TRUE)),
  Null_mean_score_topk_mean = c(mean(null_mean_topk), mean(null_mean_limma_bios)),
  Empirical_P_mean_topk = c(p_mean_topk, NA_real_),
  stringsAsFactors = FALSE
)

topk_df <- data.frame(
  Gene = top_obs,
  BIOS_obs = as.numeric(bios_obs[top_obs]),
  adjP_obs = as.numeric(obs$adjP[top_obs]),
  Emp_P = as.numeric(gene_p[top_obs]),
  Em = as.numeric(Em0[top_obs]),
  Es = as.numeric(Es0[top_obs]),
  stringsAsFactors = FALSE
)

null_df <- data.frame(
  Perm = seq_len(n_perm),
  n_genes_BIOS_ge_thresh = null_n_ge_thresh,
  mean_BIOS_topk = null_mean_topk,
  n_genes_limma_ge_thresh = null_n_limma,
  stringsAsFactors = FALSE
)

utils::write.csv(summary_df, file.path(results_dir, "BIOS_FDR_summary.csv"), row.names = FALSE)
utils::write.csv(topk_df, file.path(results_dir, "BIOS_FDR_topk_genes.csv"), row.names = FALSE)
utils::write.csv(null_df, file.path(results_dir, "BIOS_FDR_null_distribution.csv"), row.names = FALSE)

fmt <- function(x, d = 4) {
  if (length(x) != 1L || is.na(x)) return("—")
  format(round(as.numeric(x), d), nsmall = d)
}

md <- c(
  "# BIOS-FDR — permutation empirical FDR for BIOS-Rank",
  "",
  paste0("Cohort: GSE89076 (Microarray) + GSE50760 (RNA-seq), merged cache. k=", k_panel,
         ", B=", n_perm, " Condition permutations. Seed=", seed, "."),
  "",
  "**Null model:** shuffle Condition; keep Platform; refit `~ Condition + Platform`;",
  "recompute Ec, Ep, Ex; gene-wise shuffle Em, Es.",
  "",
  "**Empirical FDR (top-k list):** mean over permutations of",
  "`(# genes with null BIOS ≥ observed k-th BIOS) / k`.",
  "",
  "| Method | Empirical FDR | Mean null discoveries | Observed mean top-k BIOS | Emp. P (mean top-k) |",
  "|--------|--------------:|----------------------:|-------------------------:|--------------------:|"
)
for (i in seq_len(nrow(summary_df))) {
  md <- c(md, paste0(
    "| ", summary_df$Method[i], " | ", fmt(summary_df$Empirical_FDR[i]), " | ",
    fmt(summary_df$Mean_null_discoveries_at_threshold[i], 2), " | ",
    fmt(summary_df$Observed_mean_score_topk[i]), " | ",
    fmt(summary_df$Empirical_P_mean_topk[i]), " |"
  ))
}

md <- c(
  md, "",
  "## Observed BIOS top-k (with per-gene empirical P)",
  "",
  "| Gene | BIOS | adj.P | Emp.P | Em | Es |",
  "|------|-----:|------:|------:|---:|---:|"
)
for (i in seq_len(nrow(topk_df))) {
  md <- c(md, paste0(
    "| ", topk_df$Gene[i], " | ", fmt(topk_df$BIOS_obs[i], 3), " | ",
    format(topk_df$adjP_obs[i], scientific = TRUE, digits = 3), " | ",
    fmt(topk_df$Emp_P[i], 3), " | ", topk_df$Em[i], " | ", topk_df$Es[i], " |"
  ))
}

verdict <- if (fdr_bios < fdr_limma && fdr_bios <= 0.2) {
  paste0("BIOS-Rank top-", k_panel, " shows **lower empirical FDR (", fmt(fdr_bios),
         ")** than limma top-", k_panel, " (", fmt(fdr_limma),
         ") under the same Condition-permutation null — supporting calibrated specificity.")
} else if (fdr_bios <= 0.25) {
  paste0("BIOS-Rank top-", k_panel, " empirical FDR = ", fmt(fdr_bios),
         " (limma = ", fmt(fdr_limma), "). FDR is controlled at a moderate level; report both.")
} else {
  paste0("BIOS-Rank empirical FDR = ", fmt(fdr_bios),
         " is high under this null — tighten k, raise Ex weight, or increase B; do not overclaim specificity.")
}

md <- c(md, "", "## Verdict", "", verdict, "",
        paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)))

out_md <- file.path(results_dir, "BIOS_FDR_summary.md")
writeLines(md, out_md)

## Patch method doc + original paper
method_doc <- file.path(orig_root, "manuscript", "BIOS_RANK_METHOD.md")
if (file.exists(method_doc)) {
  cat("\n## BIOS-FDR (error control)\n",
      file = method_doc, append = TRUE)
  cat("\nSee `results/BIOS_FDR_summary.md` after running `scripts/bios-fdr.R`.\n\n",
      "Phenotype permutation null for Condition; empirical FDR of top-*k* BIOS vs limma top-*k*.\n",
      file = method_doc, append = TRUE)
}

paper <- file.path(orig_root, "manuscript", "GExPipe_Original_Paper.md")
if (file.exists(paper)) {
  txt <- readLines(paper, warn = FALSE)
  marker <- "### 4.7 BIOS-FDR (permutation error control)"
  block <- c(
    marker,
    "",
    paste0("Source: `results/BIOS_FDR_summary.md` (", format(Sys.Date()), "). B=", n_perm, ", k=", k_panel, "."),
    "",
    paste0("- BIOS-Rank top-k empirical FDR = **", fmt(fdr_bios), "**"),
    paste0("- limma top-k empirical FDR = **", fmt(fdr_limma), "**"),
    paste0("- Emp. P (mean top-k BIOS vs null) = **", fmt(p_mean_topk), "**"),
    "",
    verdict,
    ""
  )
  if (any(grepl(marker, txt, fixed = TRUE))) {
    i0 <- grep(marker, txt, fixed = TRUE)[1]
    i1 <- grep("^## 5\\. Discussion", txt)[1]
    if (!is.na(i0) && !is.na(i1) && i1 > i0) {
      txt <- c(txt[seq_len(i0 - 1L)], block, txt[i1:length(txt)])
    }
  } else {
    i1 <- grep("^## 5\\. Discussion", txt)[1]
    if (!is.na(i1)) txt <- c(txt[seq_len(i1 - 1L)], block, txt[i1:length(txt)])
  }
  writeLines(txt, paper)
  cat("Updated paper §4.7\n")
}

cat("\nWrote:\n  ", file.path(results_dir, "BIOS_FDR_summary.csv"), "\n  ", out_md, "\n", sep = "")
cat("\n", verdict, "\n", sep = "")
print(summary_df, row.names = FALSE)
