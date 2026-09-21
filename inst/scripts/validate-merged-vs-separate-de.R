#!/usr/bin/env Rscript
## Compare merged cross-platform DE vs separate microarray / RNA-seq DE
##
## Required CSV exports (from GExPipe Step 6 or validation_manual/):
##   merged_limma_DE_all.csv      - Merged run (microarray + RNA-seq), limma
##   microarray_limma_DE_all.csv  - Microarray-only GSE, limma
##   rnaseq_DE_all.csv            - RNA-seq-only GSE (DESeq2 or limma_voom)
##
## Optional sig lists (*_DE_sig.csv) for Jaccard on significant genes.
##
## Usage:
##   Rscript inst/scripts/validate-merged-vs-separate-de.R --repo e:/GExPipe
##   Rscript inst/scripts/validate-merged-vs-separate-de.R --dir validation_manual/cross_platform

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) args[i + 1L] else default
}

repo <- get_arg("--repo", getwd())
indir <- get_arg("--dir", file.path(repo, "validation_manual", "cross_platform"))
outdir <- get_arg("--outdir", file.path(repo, "validation_manual", "cross_platform", "report"))
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

logfc_cut <- as.numeric(get_arg("--logfc", "0.5"))
padj_cut <- as.numeric(get_arg("--padj", "0.05"))

read_de <- function(path) {
  if (!file.exists(path)) return(NULL)
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!"Gene" %in% names(df)) df$Gene <- df[[1L]]
  rownames(df) <- df$Gene
  df
}

sig_genes <- function(df) {
  if (is.null(df)) return(character())
  df$Gene[df$adj.P.Val < padj_cut & abs(df$logFC) >= logfc_cut]
}

jaccard <- function(a, b) {
  a <- unique(a); b <- unique(b)
  if (!length(union(a, b))) return(NA_real_)
  length(intersect(a, b)) / length(union(a, b))
}

direction_concordance <- function(df_a, df_b, genes) {
  genes <- intersect(genes, intersect(df_a$Gene, df_b$Gene))
  if (length(genes) < 2L) return(NA_real_)
  la <- df_a[match(genes, df_a$Gene), "logFC"]
  lb <- df_b[match(genes, df_b$Gene), "logFC"]
  mean(sign(la) == sign(lb), na.rm = TRUE)
}

logfc_cor <- function(df_x, df_y, genes) {
  genes <- intersect(genes, intersect(df_x$Gene, df_y$Gene))
  if (length(genes) < 3L) return(NA_real_)
  lx <- df_x[match(genes, df_x$Gene), "logFC"]
  ly <- df_y[match(genes, df_y$Gene), "logFC"]
  suppressWarnings(stats::cor(lx, ly, use = "complete.obs"))
}

paths <- list(
  merged = file.path(indir, "merged_limma_DE_all.csv"),
  micro = file.path(indir, "microarray_limma_DE_all.csv"),
  rna = file.path(indir, "rnaseq_DE_all.csv")
)

merged <- read_de(paths$merged)
micro <- read_de(paths$micro)
rna <- read_de(paths$rna)

missing <- names(paths)[vapply(paths, function(p) !file.exists(p), logical(1))]
if (length(missing)) {
  cat("Missing inputs in", indir, ":\n")
  for (m in missing) cat("  -", basename(paths[[m]]), "\n")
  cat("\nRun three GExPipe analyses and export Step 6 DE tables:\n")
  cat("  1) Merged (GSE89076 + GSE50760), limma\n")
  cat("  2) Microarray-only (GSE89076), limma\n")
  cat("  3) RNA-seq-only (GSE50760), DESeq2 or limma_voom\n")
  cat("\nSee inst/manuscript/CROSS_PLATFORM_VALIDATION.md\n")
  quit(save = "no", status = 1)
}

common_all <- Reduce(intersect, list(merged$Gene, micro$Gene, rna$Gene))
sig_m <- sig_genes(merged)
sig_micro <- sig_genes(micro)
sig_rna <- sig_genes(rna)

both_sep <- intersect(sig_micro, sig_rna)
conc_m_vs_micro <- direction_concordance(merged, micro, common_all)
conc_m_vs_rna <- direction_concordance(merged, rna, common_all)
conc_sep <- direction_concordance(micro, rna, common_all)
cor_m_avg_sep <- logfc_cor(
  merged,
  data.frame(
    Gene = common_all,
    logFC = rowMeans(cbind(
      micro[match(common_all, micro$Gene), "logFC"],
      rna[match(common_all, rna$Gene), "logFC"]
    ), na.rm = TRUE)
  ),
  common_all
)

summary_rows <- data.frame(
  Metric = c(
    "Common genes (all three analyses)",
    "Significant DEGs: merged limma",
    "Significant DEGs: microarray-only",
    "Significant DEGs: RNA-seq-only",
    "Jaccard: merged sig vs (micro sig INTERSECT rna sig)",
    "Jaccard: merged sig vs micro sig",
    "Jaccard: merged sig vs rna sig",
    "Jaccard: micro sig vs rna sig (cross-platform without merge)",
    "Direction concordance: merged vs micro (all common genes)",
    "Direction concordance: merged vs RNA-seq (all common genes)",
    "Direction concordance: micro vs RNA-seq (all common genes)",
    "Direction concordance: merged vs both-separate-sig genes",
    "logFC correlation: merged vs mean(micro, rna) on common genes",
    "Merged-only sig genes (not sig in either separate)",
    "Sig in both separate but not in merged"
  ),
  Value = c(
    length(common_all),
    length(sig_m),
    length(sig_micro),
    length(sig_rna),
    jaccard(sig_m, both_sep),
    jaccard(sig_m, sig_micro),
    jaccard(sig_m, sig_rna),
    jaccard(sig_micro, sig_rna),
    conc_m_vs_micro,
    conc_m_vs_rna,
    conc_sep,
    direction_concordance(merged, micro, intersect(sig_m, both_sep)),
    cor_m_avg_sep,
    length(setdiff(sig_m, union(sig_micro, sig_rna))),
    length(setdiff(both_sep, sig_m))
  ),
  stringsAsFactors = FALSE
)

out_csv <- file.path(outdir, "merged_vs_separate_summary.csv")
utils::write.csv(summary_rows, out_csv, row.names = FALSE)

report <- c(
  "# Cross-platform DE comparison",
  "",
  sprintf("Input directory: `%s`", indir),
  sprintf("Thresholds: |log2FC| >= %s, adj.P <= %s", logfc_cut, padj_cut),
  "",
  "## Interpretation",
  "",
  "- **Merged DE is not expected to equal separate DE** — different models and sample sets.",
  "- **High direction concordance** on common genes supports validity of integration.",
  "- **Moderate Jaccard** between merged and separate sig lists is normal.",
  "- **Low Jaccard between micro-only and RNA-seq-only** sig lists motivates merged analysis.",
  "",
  "## Summary",
  "",
  capture.output(print(summary_rows, row.names = FALSE)),
  "",
  sprintf("Full table: `%s`", out_csv)
)

report_path <- file.path(outdir, "merged_vs_separate_report.md")
writeLines(report, report_path)

cat("Wrote:", out_csv, "\n")
cat("Wrote:", report_path, "\n")
print(summary_rows, row.names = FALSE)
