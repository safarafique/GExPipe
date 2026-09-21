#!/usr/bin/env Rscript
## Run separate vs merged cross-platform DE and quantify differences
## GSE89076 (microarray) + GSE50760 (RNA-seq) — CRC primary/normal style contrast
##
## Usage: Rscript inst/scripts/run-cross-platform-de-comparison.R --repo e:/GExPipe

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) args[i + 1L] else default
}

repo <- get_arg("--repo", getwd())
micro_gse <- get_arg("--micro", "GSE89076")
rna_gse <- get_arg("--rna", "GSE50760")
outdir <- get_arg("--outdir", file.path(repo, "validation_manual", "cross_platform"))
work <- file.path(outdir, "work")
dir.create(work, recursive = TRUE, showWarnings = FALSE)

logfc_cut <- as.numeric(get_arg("--logfc", "0.5"))
padj_cut <- as.numeric(get_arg("--padj", "0.05"))

suppressPackageStartupMessages({
  pkgload::load_all(repo, quiet = TRUE)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

jaccard <- function(a, b) {
  a <- unique(a); b <- unique(b)
  if (!length(union(a, b))) return(NA_real_)
  length(intersect(a, b)) / length(union(a, b))
}

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
  # GSE89076: paired adjacent normal (N) vs tumor (T) in title, e.g. "6N", "6T"
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

safe_batch <- function(expr, meta) {
  meta <- meta[colnames(expr), , drop = FALSE]
  if (length(unique(as.character(meta$Dataset))) < 2L) {
    gv <- apply(expr, 1, stats::var, na.rm = TRUE)
    cutoff <- stats::quantile(gv, 0.25, na.rm = TRUE)
    list(batch_corrected = expr[!is.na(gv) & gv > cutoff, , drop = FALSE])
  } else {
    gexp_batch_correct(expr, meta, variance_percentile = 25, method = "limma")
  }
}

run_limma_de <- function(expr, meta, label) {
  meta <- meta[colnames(expr), , drop = FALSE]
  meta$Condition <- factor(meta$Condition, levels = c("Normal", "Disease"))
  meta$Dataset <- factor(meta$Dataset)
  if ("Platform" %in% colnames(meta)) meta$Platform <- factor(meta$Platform)
  res <- gexp_run_de(expr, meta, method = "limma", logfc_cutoff = logfc_cut, padj_cutoff = padj_cut)
  res$de_results$analysis <- label
  res
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

cat("=== Cross-platform DE comparison ===\n")
cat("Microarray:", micro_gse, "| RNA-seq:", rna_gse, "\n\n")

# ---- Download ----
micro_dir <- file.path(work, "micro_data")
rna_dir <- file.path(work, "rna_data")
dir.create(micro_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(rna_dir, recursive = TRUE, showWarnings = FALSE)

cat("Downloading microarray", micro_gse, "...\n")
micro_dl <- gexp_download_one_microarray_gse(micro_gse, micro_dir)
if (!isTRUE(micro_dl$ok)) stop(micro_gse, " failed: ", micro_dl$reason %||% "unknown")
cat("  ", nrow(micro_dl$micro_expr), "x", ncol(micro_dl$micro_expr), " samples\n")

cat("Downloading RNA-seq", rna_gse, "...\n")
rna_dl <- gexp_download_one_rnaseq_gse(rna_gse, rna_dir)
if (!isTRUE(rna_dl$ok)) stop(rna_gse, " failed: ", rna_dl$reason %||% "unknown")
cat("  ", nrow(rna_dl$count_matrix), "x", ncol(rna_dl$count_matrix), " samples\n")

# ---- Phenotype ----
meta_micro <- assign_gse89076(micro_dl$metadata)
meta_rna <- assign_gse50760(rna_dl$metadata)

cat("\nMicroarray groups:", paste(names(table(meta_micro$Condition)), table(meta_micro$Condition), sep = "=", collapse = ", "), "\n")
cat("RNA-seq groups:", paste(names(table(meta_rna$Condition)), table(meta_rna$Condition), sep = "=", collapse = ", "), "\n")

micro_expr <- micro_dl$micro_expr[, rownames(meta_micro), drop = FALSE]
rna_counts <- rna_dl$count_matrix[, rownames(meta_rna), drop = FALSE]

micro_expr <- map_micro_to_symbols(micro_expr, micro_dl$micro_eset, micro_gse)
cat("  Symbol overlap (micro x rna):", length(intersect(rownames(micro_expr), rownames(rna_counts))), "genes\n")

# ---- Separate: microarray-only ----
cat("\n--- Separate DE: microarray-only ---\n")
norm_micro <- gexp_normalize_and_intersect(
  micro_expr_list = setNames(list(micro_expr), micro_gse),
  rna_counts_list = list(),
  de_method = "limma"
)
meta_m <- norm_micro$unified_metadata
meta_m$Condition <- meta_micro$Condition[match(meta_m$SampleID, rownames(meta_micro))]
meta_m$Dataset <- micro_gse
rownames(meta_m) <- meta_m$SampleID
batch_m <- safe_batch(norm_micro$combined_expr, meta_m)
de_micro <- run_limma_de(batch_m$batch_corrected, meta_m, "microarray_only")

# ---- Separate: RNA-seq-only (limma on log matrix for fair vs merged limma) ----
cat("--- Separate DE: RNA-seq-only ---\n")
norm_rna <- gexp_normalize_and_intersect(
  micro_expr_list = list(),
  rna_counts_list = setNames(list(rna_counts), rna_gse),
  de_method = "limma"
)
meta_r <- norm_rna$unified_metadata
meta_r$Condition <- meta_rna$Condition[match(meta_r$SampleID, rownames(meta_rna))]
meta_r$Dataset <- rna_gse
rownames(meta_r) <- meta_r$SampleID
batch_r <- safe_batch(norm_rna$combined_expr, meta_r)
de_rna <- run_limma_de(batch_r$batch_corrected, meta_r, "rnaseq_only")

# ---- Merged ----
cat("--- Merged DE: microarray + RNA-seq ---\n")
norm_merged <- gexp_normalize_and_intersect(
  micro_expr_list = setNames(list(micro_expr), micro_gse),
  rna_counts_list = setNames(list(rna_counts), rna_gse),
  de_method = "limma"
)
cat("  Common genes after merge:", length(norm_merged$common_genes), "\n")
meta_g <- norm_merged$unified_metadata
meta_g$Condition <- NA_character_
idx_micro <- match(meta_g$SampleID, rownames(meta_micro))
idx_rna <- match(meta_g$SampleID, rownames(meta_rna))
meta_g$Condition[!is.na(idx_micro)] <- meta_micro$Condition[idx_micro[!is.na(idx_micro)]]
meta_g$Condition[!is.na(idx_rna)] <- meta_rna$Condition[idx_rna[!is.na(idx_rna)]]
meta_g <- meta_g[!is.na(meta_g$Condition), , drop = FALSE]
rownames(meta_g) <- meta_g$SampleID
expr_g <- norm_merged$combined_expr[, rownames(meta_g), drop = FALSE]
batch_g <- safe_batch(expr_g, meta_g)
de_merged <- run_limma_de(batch_g$batch_corrected, meta_g, "merged")
cat("  DE design:", de_merged$formula_desc, "\n")

# ---- Compare ----
dm <- de_micro$de_results
dr <- de_rna$de_results
dg <- de_merged$de_results

sig <- function(df) df$Gene[df$adj.P.Val < padj_cut & abs(df$logFC) >= logfc_cut]
sig_m <- sig(dm); sig_r <- sig(dr); sig_g <- sig(dg)

common_genes <- Reduce(intersect, list(dm$Gene, dr$Gene, dg$Gene))
both_sep_sig <- intersect(sig_m, sig_r)

# logFC on all common genes
lm <- dm$logFC[match(common_genes, dm$Gene)]
lr <- dr$logFC[match(common_genes, dr$Gene)]
lg <- dg$logFC[match(common_genes, dg$Gene)]
lavg <- rowMeans(cbind(lm, lr), na.rm = TRUE)

dir_agree_mg <- mean(sign(lg) == sign(lm), na.rm = TRUE)
dir_agree_rg <- mean(sign(lg) == sign(lr), na.rm = TRUE)
dir_agree_sep <- mean(sign(lm) == sign(lr), na.rm = TRUE)
dir_agree_both_sig <- if (length(both_sep_sig) >= 2L) {
  mean(sign(dg$logFC[match(both_sep_sig, dg$Gene)]) == sign(lm[match(both_sep_sig, dm$Gene)]), na.rm = TRUE)
} else NA_real_

cor_mg <- suppressWarnings(stats::cor(lg, lm, use = "complete.obs"))
cor_rg <- suppressWarnings(stats::cor(lg, lr, use = "complete.obs"))
cor_sep <- suppressWarnings(stats::cor(lm, lr, use = "complete.obs"))
cor_g_avg <- suppressWarnings(stats::cor(lg, lavg, use = "complete.obs"))

mad_mg <- mean(abs(lg - lm), na.rm = TRUE)
mad_rg <- mean(abs(lg - lr), na.rm = TRUE)
mad_sep <- mean(abs(lm - lr), na.rm = TRUE)

# adj.P on common sig in merged
padj_m <- dm$adj.P.Val[match(common_genes, dm$Gene)]
padj_r <- dr$adj.P.Val[match(common_genes, dg$Gene)]
padj_g <- dg$adj.P.Val[match(common_genes, dg$Gene)]

summary_df <- data.frame(
  Metric = c(
    "Common genes (all three DE tables)",
    "Significant DEGs: microarray-only",
    "Significant DEGs: RNA-seq-only",
    "Significant DEGs: merged",
    "Significant in BOTH separate (intersection)",
    "Jaccard: merged sig vs (micro sig AND rna sig)",
    "Jaccard: merged sig vs micro sig",
    "Jaccard: merged sig vs rna sig",
    "Jaccard: micro sig vs rna sig (no merge)",
    "Direction agreement: merged vs micro (all common genes)",
    "Direction agreement: merged vs rna (all common genes)",
    "Direction agreement: micro vs rna (all common genes)",
    "Direction agreement: merged vs micro (both-separate-sig genes)",
    "Pearson r logFC: merged vs micro",
    "Pearson r logFC: merged vs rna",
    "Pearson r logFC: micro vs rna",
    "Pearson r logFC: merged vs mean(micro,rna)",
    "Mean |logFC diff|: merged vs micro",
    "Mean |logFC diff|: merged vs rna",
    "Mean |logFC diff|: micro vs rna",
    "Merged-only significant (not sig in either separate)",
    "Sig in both separate but NOT sig in merged",
    "Same/Different verdict (sig gene lists identical?)"
  ),
  Value = c(
    length(common_genes),
    length(sig_m), length(sig_r), length(sig_g), length(both_sep_sig),
    jaccard(sig_g, both_sep_sig),
    jaccard(sig_g, sig_m), jaccard(sig_g, sig_r), jaccard(sig_m, sig_r),
    dir_agree_mg, dir_agree_rg, dir_agree_sep, dir_agree_both_sig,
    cor_mg, cor_rg, cor_sep, cor_g_avg,
    mad_mg, mad_rg, mad_sep,
    length(setdiff(sig_g, union(sig_m, sig_r))),
    length(setdiff(both_sep_sig, sig_g)),
    if (identical(sort(sig_g), sort(both_sep_sig))) "SAME lists" else "DIFFERENT lists"
  ),
  stringsAsFactors = FALSE
)

# Export DE tables
write.csv(dm, file.path(outdir, "microarray_limma_DE_all.csv"), row.names = FALSE)
write.csv(dr, file.path(outdir, "rnaseq_DE_all.csv"), row.names = FALSE)
write.csv(dg, file.path(outdir, "merged_limma_DE_all.csv"), row.names = FALSE)
write.csv(data.frame(Gene = sig_m), file.path(outdir, "microarray_limma_DE_sig.csv"), row.names = FALSE)
write.csv(data.frame(Gene = sig_r), file.path(outdir, "rnaseq_DE_sig.csv"), row.names = FALSE)
write.csv(data.frame(Gene = sig_g), file.path(outdir, "merged_limma_DE_sig.csv"), row.names = FALSE)

# Per-gene comparison on common genes
gene_cmp <- data.frame(
  Gene = common_genes,
  logFC_micro = lm,
  logFC_rna = lr,
  logFC_merged = lg,
  logFC_mean_separate = lavg,
  adjP_micro = padj_m,
  adjP_rna = padj_r,
  adjP_merged = padj_g,
  sig_micro = common_genes %in% sig_m,
  sig_rna = common_genes %in% sig_r,
  sig_merged = common_genes %in% sig_g,
  sig_both_separate = common_genes %in% both_sep_sig,
  direction_merged_vs_micro = sign(lg) == sign(lm),
  direction_merged_vs_rna = sign(lg) == sign(lr),
  abs_diff_merged_micro = abs(lg - lm),
  abs_diff_merged_rna = abs(lg - lr),
  stringsAsFactors = FALSE
)
write.csv(gene_cmp, file.path(outdir, "common_genes_DE_comparison.csv"), row.names = FALSE)
report_dir <- file.path(outdir, "report")
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(summary_df, file.path(report_dir, "merged_vs_separate_summary.csv"), row.names = FALSE)

report <- c(
  "# Merged vs separate cross-platform DE — results",
  "",
  sprintf("**Microarray:** %s | **RNA-seq:** %s", micro_gse, rna_gse),
  sprintf("**Thresholds:** |log2FC| >= %s, adj.P <= %s", logfc_cut, padj_cut),
  sprintf("**Merged design:** %s", de_merged$formula_desc),
  "",
  "## Verdict",
  "",
  if (identical(sort(sig_g), sort(both_sep_sig))) {
    "**Significant gene lists are identical** between merged and intersection of separate analyses."
  } else {
    paste0(
      "**Significant gene lists are DIFFERENT.** ",
      "Merged: ", length(sig_g), " DEGs; micro-only: ", length(sig_m),
      "; rna-only: ", length(sig_r),
      "; intersection of separate sig: ", length(both_sep_sig),
      "; Jaccard(merged, both separate) = ", round(jaccard(sig_g, both_sep_sig), 3), "."
    )
  },
  "",
  "## Summary metrics",
  "",
  capture.output(print(summary_df, row.names = FALSE)),
  "",
  "## Interpretation",
  "",
  "- Merged and separate DE are **not expected to be identical** (different samples in model, joint vs single-cohort estimation).",
  "- **Direction concordance** and **logFC correlation** on common genes measure validity.",
  "- **Low Jaccard** between micro-only and rna-only sig lists shows why integrated analysis is needed.",
  ""
)
writeLines(report, file.path(outdir, "report", "merged_vs_separate_report.md"))

cat("\n========== RESULTS ==========\n")
print(summary_df, row.names = FALSE)
cat("\nReport:", file.path(outdir, "report", "merged_vs_separate_report.md"), "\n")
