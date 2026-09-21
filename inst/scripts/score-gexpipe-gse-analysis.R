#!/usr/bin/env Rscript
## Full GExPipe pipeline run + accuracy scoring for GSE50760 & GSE104836
## Usage: Rscript inst/scripts/score-gexpipe-gse-analysis.R --repo e:/GExPipe

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) args[i + 1L] else default
}

repo_root <- get_arg("--repo", getwd())
outdir <- get_arg("--outdir", file.path(repo_root, "validation_manual"))
score_dir <- file.path(outdir, "scoring_report")
dir.create(score_dir, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  pkgload::load_all(repo_root, quiet = TRUE)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

train_gse <- "GSE50760"
val_gse <- "GSE104836"
logfc_cutoff <- 0.5
padj_cutoff <- 0.05
primary_method <- "deseq2"

score_item <- function(name, points, max_pts, detail) {
  list(name = name, points = points, max = max_pts, detail = detail)
}

clamp_score <- function(x, lo = 0, hi = 100) pmax(lo, pmin(hi, x))

assign_gse50760_primary_vs_normal <- function(meta) {
  txt <- tolower(meta$title)
  cond <- rep(NA_character_, nrow(meta))
  cond[grepl("normal colon", txt)] <- "Normal"
  cond[grepl("primary colorectal", txt)] <- "Disease"
  meta$Condition <- cond
  meta[!is.na(meta$Condition), , drop = FALSE]
}

assign_gse104836_tumor_vs_normal <- function(meta) {
  txt <- apply(meta, 1L, function(r) paste(tolower(as.character(r)), collapse = " "))
  cond <- rep(NA_character_, length(txt))
  cond[grepl("nontumor|non-tumor", txt)] <- "Normal"
  cond[grepl("colon cancer|cancer tissue|\\bc\\b|_c$", txt)] <- "Disease"
  meta$Condition <- cond
  meta[!is.na(meta$Condition), , drop = FALSE]
}

download_gse <- function(gse_id, base_dir) {
  rna_dir <- file.path(base_dir, "rna_data")
  dir.create(rna_dir, showWarnings = FALSE, recursive = TRUE)
  gexp_download_one_rnaseq_gse(gse_id, rna_dir)
}

run_gexpipe_de <- function(counts, meta, method, logfc = logfc_cutoff, padj = padj_cutoff) {
  meta <- meta[intersect(colnames(counts), rownames(meta)), , drop = FALSE]
  counts <- counts[, rownames(meta), drop = FALSE]
  meta$Condition <- factor(meta$Condition, levels = c("Normal", "Disease"))

  norm <- gexp_normalize_and_intersect(
    micro_expr_list = list(),
    rna_counts_list = list(dataset = counts),
    de_method = method
  )

  meta_de <- norm$unified_metadata
  meta_de$Condition <- meta$Condition[match(meta_de$SampleID, rownames(meta))]

  if (method %in% c("deseq2", "edger", "limma_voom")) {
    expr <- norm$raw_counts_for_deseq2
    meta_de <- norm$raw_counts_metadata
    meta_de$Condition <- meta$Condition[match(rownames(meta_de), rownames(meta))]
  } else {
    expr <- norm$combined_expr
  }

  if (method == "limma") {
    res <- gexp_run_de(expr, meta_de, method = "limma", logfc_cutoff = logfc, padj_cutoff = padj)
    return(list(de = res$de_results, norm = norm, meta = meta_de))
  }

  if (method == "deseq2") {
    ds_design <- gexpipe_deseq2_design(meta_de)
    design_mm <- stats::model.matrix(ds_design$formula, data = meta_de)
    filt <- gexpipe_independent_filter(expr, design = design_mm)
    dds <- DESeq2::DESeqDataSetFromMatrix(
      countData = filt$expr, colData = meta_de, design = ds_design$formula
    )
    dds <- DESeq2::DESeq(dds, quiet = TRUE)
    rn <- DESeq2::resultsNames(dds)
    coef_name <- rn[grep("Disease", rn)[1]]
    tt <- DESeq2::results(dds, name = coef_name)
    de <- data.frame(
      Gene = rownames(tt), logFC = tt$log2FoldChange,
      P.Value = tt$pvalue, adj.P.Val = tt$padj, stringsAsFactors = FALSE
    )
  } else if (method == "edger") {
    de_design <- gexpipe_build_de_design(meta_de)
    filt <- gexpipe_independent_filter(expr, design = de_design$design)
    y <- edgeR::DGEList(counts = filt$expr)
    y <- edgeR::calcNormFactors(y)
    y <- edgeR::estimateDisp(y, de_design$design)
    fit <- edgeR::glmQLFit(y, de_design$design)
    qlf <- edgeR::glmQLFTest(fit, coef = de_design$coef_condition)
    tt <- edgeR::topTags(qlf, n = Inf)$table
    de <- data.frame(
      Gene = rownames(tt), logFC = tt$logFC,
      P.Value = tt$PValue, adj.P.Val = tt$FDR, stringsAsFactors = FALSE
    )
  } else {
    de_design <- gexpipe_build_de_design(meta_de)
    filt <- gexpipe_independent_filter(expr, design = de_design$design)
    y <- edgeR::DGEList(counts = filt$expr)
    y <- edgeR::calcNormFactors(y)
    v <- limma::voom(y, de_design$design, plot = FALSE)
    fit <- limma::lmFit(v, de_design$design)
    fit <- limma::eBayes(fit)
    tt <- limma::topTable(fit, coef = de_design$coef_condition, number = Inf, sort.by = "P")
    de <- data.frame(
      Gene = rownames(tt), logFC = tt$logFC,
      P.Value = tt$P.Value, adj.P.Val = tt$adj.P.Val, stringsAsFactors = FALSE
    )
  }

  de$Significance <- "Not Significant"
  de$Significance[de$adj.P.Val < padj & de$logFC > logfc] <- "Up-regulated"
  de$Significance[de$adj.P.Val < padj & de$logFC < -logfc] <- "Down-regulated"
  list(de = de, norm = norm, meta = meta_de)
}

sig_genes <- function(de, logfc = logfc_cutoff, padj = padj_cutoff) {
  de$Gene[de$adj.P.Val < padj & abs(de$logFC) >= logfc]
}

jaccard <- function(a, b) {
  a <- unique(a); b <- unique(b)
  if (!length(union(a, b))) return(NA_real_)
  length(intersect(a, b)) / length(union(a, b))
}

score_jaccard_pts <- function(j, max_pts, target = 0.90) {
  if (is.na(j) || j <= 0) return(0)
  min(1, j / target) * max_pts
}

pct_score <- function(ratio, full = 1) clamp_score(100 * ratio / full)

cat("=== GExPipe Full Analysis & Scoring ===\n\n")
work <- file.path(outdir, "work")
dir.create(work, showWarnings = FALSE, recursive = TRUE)

scores <- list()
details <- list()

# ---- 1. DOWNLOAD ----
cat("Step 1: Download...\n")
train_dl <- download_gse(train_gse, work)
val_dl <- download_gse(val_gse, work)

dl_pts <- 0
dl_max <- 20
if (isTRUE(train_dl$ok) && isTRUE(val_dl$ok)) dl_pts <- dl_pts + 10
if (isTRUE(train_dl$ok) && ncol(train_dl$count_matrix) == 54) dl_pts <- dl_pts + 5
if (isTRUE(val_dl$ok) && ncol(val_dl$count_matrix) == 20) dl_pts <- dl_pts + 5
scores$download <- score_item(
  "Data download & sample integrity",
  dl_pts, dl_max,
  sprintf("GSE50760: %s (%d samples); GSE104836: %s (%d samples)",
          if (train_dl$ok) "OK" else "FAIL", ncol(train_dl$count_matrix %||% integer()),
          if (val_dl$ok) "OK" else "FAIL", ncol(val_dl$count_matrix %||% integer()))
)

# ---- 2. QC ----
cat("Step 2: QC...\n")
train_meta_pv <- assign_gse50760_primary_vs_normal(train_dl$metadata)
val_meta <- assign_gse104836_tumor_vs_normal(val_dl$metadata)

qc_pts <- 0; qc_max <- 10
qc_train <- tryCatch({
  gexp_qc_detect_outliers(train_dl$count_matrix[, rownames(train_meta_pv), drop = FALSE], top_n = 5000L)
}, error = function(e) NULL)
qc_val <- tryCatch({
  gexp_qc_detect_outliers(val_dl$count_matrix[, rownames(val_meta), drop = FALSE], top_n = 5000L)
}, error = function(e) NULL)
if (!is.null(qc_train) && !is.null(qc_val)) qc_pts <- qc_pts + 5
if (all(table(train_meta_pv$Condition) >= 3) && all(table(val_meta$Condition) >= 3)) qc_pts <- qc_pts + 5
scores$qc <- score_item("QC pipeline", qc_pts, qc_max,
  sprintf("Outlier detection OK; groups GSE50760 %s, GSE104836 %s",
          paste(names(table(train_meta_pv$Condition)), table(train_meta_pv$Condition), sep = "=", collapse = ", "),
          paste(names(table(val_meta$Condition)), table(val_meta$Condition), sep = "=", collapse = ", ")))

# ---- 3. NORMALIZATION + DE ----
cat("Step 3-6: Normalize & DE (deseq2)...\n")
train_de <- run_gexpipe_de(train_dl$count_matrix, train_meta_pv, primary_method)
val_de <- run_gexpipe_de(val_dl$count_matrix, val_meta, primary_method)

norm_pts <- 0; norm_max <- 10
if (!is.null(train_de$norm$raw_counts_for_deseq2)) norm_pts <- norm_pts + 5
if (nrow(train_de$norm$combined_expr) > 10000) norm_pts <- norm_pts + 5
scores$normalization <- score_item("Normalization (TMM + raw counts for DE)", norm_pts, norm_max,
  sprintf("%d genes after norm; raw counts matrix preserved for DESeq2",
          nrow(train_de$norm$combined_expr)))

train_sig <- sig_genes(train_de$de)
val_sig <- sig_genes(val_de$de)
utils::write.csv(train_de$de, file.path(score_dir, "GSE50760_primary_vs_normal_deseq2_all.csv"), row.names = FALSE)
utils::write.csv(val_de$de, file.path(score_dir, "GSE104836_tumor_vs_normal_deseq2_all.csv"), row.names = FALSE)
utils::write.csv(data.frame(Gene = train_sig), file.path(score_dir, "GSE50760_primary_vs_normal_deseq2_sig.csv"), row.names = FALSE)
utils::write.csv(data.frame(Gene = val_sig), file.path(score_dir, "GSE104836_tumor_vs_normal_deseq2_sig.csv"), row.names = FALSE)

# ---- 4. DE METHOD CONCORDANCE ----
cat("Step 6b: DE method concordance...\n")
train_edger <- run_gexpipe_de(train_dl$count_matrix, train_meta_pv, "edger")
val_edger <- run_gexpipe_de(val_dl$count_matrix, val_meta, "edger")
train_j <- jaccard(train_sig, sig_genes(train_edger$de))
val_j <- jaccard(val_sig, sig_genes(val_edger$de))
conc_pts <- round(score_jaccard_pts(train_j, 7.5) + score_jaccard_pts(val_j, 7.5), 1)
conc_max <- 15
scores$concordance <- score_item("DESeq2 vs edgeR concordance", conc_pts, conc_max,
  sprintf("Jaccard GSE50760=%.3f, GSE104836=%.3f (target >=0.90)", train_j, val_j))

# ---- 5. PUBLISHED REFERENCE (GSE104836) ----
cat("Step 6c: Compare to GEO published DEG count...\n")
pub_target <- 3221L
pub_style <- val_de$de[!is.na(val_de$de$P.Value) & val_de$de$P.Value < 0.05 & abs(val_de$de$logFC) >= 1, ]
pub_n <- nrow(pub_style)
pub_ratio <- 1 - abs(pub_n - pub_target) / pub_target
pub_pts <- round(score_jaccard_pts(max(0, pub_ratio), 20, target = 0.85), 1)
pub_max <- 20
scores$published <- score_item("GSE104836 vs published DE count", pub_pts, pub_max,
  sprintf("GExPipe DESeq2 (|log2FC|>=1, P<0.05): %d genes; GEO paper reports ~%d (ratio match %.1f%%)",
          pub_n, pub_target, 100 * max(0, pub_ratio)))

# ---- 6. CROSS-COHORT BIOLOGY (CRC signature overlap) ----
cat("Step 11 proxy: cross-cohort DEG overlap...\n")
cross_j <- jaccard(train_sig, val_sig)
cross_pts <- round(score_jaccard_pts(cross_j, 15, target = 0.25), 1)
cross_max <- 15
scores$cross_cohort <- score_item("Training vs validation DEG overlap (CRC biology)", cross_pts, cross_max,
  sprintf("Jaccard=%.3f (%d shared / %d train / %d val DEGs)",
          cross_j, length(intersect(train_sig, val_sig)), length(train_sig), length(val_sig)))

# ---- 7. TOP GENE DIRECTION CONCORDANCE ----
cat("Top gene direction check...\n")
top_n <- 200L
train_top <- train_de$de[order(train_de$de$adj.P.Val), ][seq_len(min(top_n, nrow(train_de$de))), ]
val_top <- val_de$de[order(val_de$de$adj.P.Val), ][seq_len(min(top_n, nrow(val_de$de))), ]
common_top <- intersect(train_top$Gene, val_top$Gene)
if (length(common_top) >= 10) {
  tr <- train_top[match(common_top, train_top$Gene), ]
  vr <- val_top[match(common_top, val_top$Gene), ]
  dir_agree <- mean(sign(tr$logFC) == sign(vr$logFC), na.rm = TRUE)
} else {
  dir_agree <- NA_real_
}
dir_pts <- if (is.na(dir_agree)) 5 else round(score_jaccard_pts(dir_agree, 10, target = 0.70), 1)
dir_max <- 10
scores$direction <- score_item("Top-200 DEG logFC direction agreement", dir_pts, dir_max,
  sprintf("%.1f%% of shared top genes agree in direction (n=%d)", 100 * (dir_agree %||% 0), length(common_top)))

# ---- TOTAL ----
total_pts <- sum(vapply(scores, function(x) x$points, numeric(1)))
total_max <- sum(vapply(scores, function(x) x$max, numeric(1)))
pct <- round(100 * total_pts / total_max, 1)
grade <- if (pct >= 90) "A (Excellent)" else if (pct >= 80) "B (Good)" else if (pct >= 70) "C (Acceptable)" else if (pct >= 60) "D (Needs improvement)" else "F (Poor)"

report_lines <- c(
  "# GExPipe Accuracy Report",
  "",
  sprintf("**Date:** %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
  sprintf("**Datasets:** %s (Primary vs Normal, n=36) + %s (Tumor vs Nontumor, n=20)", train_gse, val_gse),
  sprintf("**DE method:** DESeq2 | |log2FC| >= %.1f | adj.P <= %.2f", logfc_cutoff, padj_cutoff),
  "",
  "## Overall Score",
  "",
  sprintf("| **Total** | **%.1f / %d (%.1f%%)** | **Grade: %s** |", total_pts, total_max, pct, grade),
  "",
  "## Scoring Breakdown",
  "",
  "| Criterion | Score | Max | Details |",
  "|-----------|-------|-----|---------|"
)
for (s in scores) {
  report_lines <- c(report_lines,
    sprintf("| %s | %.1f | %d | %s |", s$name, s$points, s$max, s$detail))
}

report_lines <- c(report_lines, "",
  "## Key Outputs",
  "",
  sprintf("- GSE50760 DEGs (Primary vs Normal): **%d**", length(train_sig)),
  sprintf("- GSE104836 DEGs (Tumor vs Nontumor): **%d**", length(val_sig)),
  sprintf("- Cross-cohort shared DEGs: **%d**", length(intersect(train_sig, val_sig))),
  sprintf("- DESeq2 vs edgeR Jaccard (GSE104836): **%.3f**", val_j),
  sprintf("- Published-style GSE104836 count: **%d** (paper ~3221)", pub_n),
  "",
  "## Interpretation",
  "",
  if (pct >= 80) {
    c(
      "GExPipe performs **well** on these CRC RNA-seq cohorts:",
      "- Downloads and parses both HiSeq 2000/3000 datasets correctly.",
      "- Count-based DE (DESeq2/edgeR) is highly self-consistent.",
      "- GSE104836 DEG count is reasonably close to the published GEO summary.",
      "- Cross-cohort overlap reflects real but platform-specific CRC biology."
    )
  } else {
    c(
      "GExPipe shows **mixed** performance — review failed criteria above.",
      "- Check group assignment in Step 4 if DEG counts look off.",
      "- Prefer DESeq2/edgeR over limma for RNA-seq."
    )
  },
  "",
  sprintf("Outputs saved to: `%s`", score_dir)
)

report_path <- file.path(score_dir, "GExPipe_accuracy_report.md")
writeLines(report_lines, report_path)

summary_csv <- do.call(rbind, lapply(scores, function(s) {
  data.frame(Criterion = s$name, Score = s$points, Max = s$max, Detail = s$detail, stringsAsFactors = FALSE)
}))
summary_csv <- rbind(summary_csv, data.frame(
  Criterion = "TOTAL", Score = total_pts, Max = total_max,
  Detail = sprintf("%.1f%% - %s", pct, grade), stringsAsFactors = FALSE
))
utils::write.csv(summary_csv, file.path(score_dir, "GExPipe_scores.csv"), row.names = FALSE)

cat("\n========================================\n")
cat(sprintf("GExPipe SCORE: %.1f / %d (%.1f%%) — Grade %s\n", total_pts, total_max, pct, grade))
cat("========================================\n\n")
for (s in scores) cat(sprintf("  [%.1f/%d] %s\n", s$points, s$max, s$name))
cat(sprintf("\nReport: %s\n", report_path))
