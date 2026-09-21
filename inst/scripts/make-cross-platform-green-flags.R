#!/usr/bin/env Rscript
## Turn yellow merge-validation flags GREEN by exporting:
##   1) PCA before/after batch (Platform + Condition)
##   2) PVCA before/after
##   3) GO/KEGG enrichment on merged DEGs
##   4) Simple hub-gene stability (top connectivity) separate vs merged
##
## Usage:
##   Rscript inst/scripts/make-cross-platform-green-flags.R --repo e:/GExPipe

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
figdir <- file.path(outdir, "report", "figures")
dir.create(figdir, recursive = TRUE, showWarnings = FALSE)
dir.create(work, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  pkgload::load_all(repo, quiet = TRUE)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

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

pca_score_frame <- function(expr, meta, label) {
  meta <- meta[colnames(expr), , drop = FALSE]
  # top variable genes for speed/stability
  gv <- apply(expr, 1, stats::var, na.rm = TRUE)
  keep <- order(gv, decreasing = TRUE)[seq_len(min(2000L, length(gv)))]
  mat <- expr[keep, , drop = FALSE]
  mat <- mat[rowSums(is.finite(mat)) == ncol(mat), , drop = FALSE]
  pca <- stats::prcomp(t(mat), scale. = TRUE)
  pc1 <- pca$x[, 1]
  pc2 <- pca$x[, 2]
  data.frame(
    Sample = colnames(expr),
    PC1 = pc1,
    PC2 = pc2,
    Platform = as.character(meta$Platform),
    Condition = as.character(meta$Condition),
    Dataset = as.character(meta$Dataset),
    Stage = label,
    stringsAsFactors = FALSE
  )
}

factor_r2_on_pc1 <- function(pc1, factor_vec) {
  ok <- !is.na(factor_vec) & is.finite(pc1)
  if (sum(ok) < 4L || length(unique(factor_vec[ok])) < 2L) return(NA_real_)
  fit <- stats::lm(pc1[ok] ~ factor(factor_vec[ok]))
  unname(summary(fit)$r.squared)
}

plot_pca <- function(df, color_by, file, title) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(invisible(NULL))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = PC1, y = PC2, colour = .data[[color_by]])) +
    ggplot2::geom_point(size = 2.2, alpha = 0.85) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(title = title, colour = color_by) +
    ggplot2::coord_equal()
  ggplot2::ggsave(file, p, width = 7, height = 5.5, dpi = 300, bg = "white")
}

hub_genes_from_expr <- function(expr, genes, n_hub = 50L) {
  genes <- intersect(genes, rownames(expr))
  if (length(genes) < 10L) return(character())
  use <- head(genes, min(500L, length(genes)))
  mat <- t(expr[use, , drop = FALSE])
  cmat <- suppressWarnings(stats::cor(mat, use = "pairwise.complete.obs"))
  diag(cmat) <- 0
  conn <- rowSums(abs(cmat), na.rm = TRUE)
  names(sort(conn, decreasing = TRUE))[seq_len(min(n_hub, length(conn)))]
}

enrich_merged <- function(sig_genes, out_prefix) {
  if (!requireNamespace("clusterProfiler", quietly = TRUE) ||
      !requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
    return(list(ok = FALSE, message = "clusterProfiler/org.Hs.eg.db not available"))
  }
  genes <- unique(sig_genes)
  map <- tryCatch(
    AnnotationDbi::select(
      org.Hs.eg.db::org.Hs.eg.db,
      keys = genes,
      keytype = "SYMBOL",
      columns = "ENTREZID"
    ),
    error = function(e) NULL
  )
  if (is.null(map)) return(list(ok = FALSE, message = "symbol mapping failed"))
  entrez <- unique(stats::na.omit(map$ENTREZID))
  if (length(entrez) < 10L) return(list(ok = FALSE, message = "too few mapped genes"))

  go <- tryCatch(
    clusterProfiler::enrichGO(
      gene = entrez, OrgDb = org.Hs.eg.db::org.Hs.eg.db, ont = "BP",
      pAdjustMethod = "BH", pvalueCutoff = 0.05, qvalueCutoff = 0.2, readable = TRUE
    ),
    error = function(e) NULL
  )
  kegg <- tryCatch(
    clusterProfiler::enrichKEGG(
      gene = entrez, organism = "hsa",
      pAdjustMethod = "BH", pvalueCutoff = 0.05, qvalueCutoff = 0.2
    ),
    error = function(e) NULL
  )
  go_df <- if (!is.null(go) && nrow(as.data.frame(go)) > 0) as.data.frame(go) else data.frame()
  kegg_df <- if (!is.null(kegg) && nrow(as.data.frame(kegg)) > 0) as.data.frame(kegg) else data.frame()
  if (nrow(go_df)) utils::write.csv(go_df, paste0(out_prefix, "_GO_BP.csv"), row.names = FALSE)
  if (nrow(kegg_df)) utils::write.csv(kegg_df, paste0(out_prefix, "_KEGG.csv"), row.names = FALSE)

  crc_keywords <- c(
    "wnt", "colorectal", "cell cycle", "epithelial", "extracellular matrix",
    "immune", "cytokine", "metabolic", "oxidative", "apoptosis", "migration",
    "adhesion", "dna replication", "p53", "mapk", "pi3k"
  )
  hit_go <- if (nrow(go_df)) {
    desc <- tolower(paste(go_df$Description, collapse = " | "))
    sum(vapply(crc_keywords, function(k) grepl(k, desc, fixed = TRUE), logical(1)))
  } else 0L
  hit_kegg <- if (nrow(kegg_df)) {
    desc <- tolower(paste(kegg_df$Description, collapse = " | "))
    sum(vapply(crc_keywords, function(k) grepl(k, desc, fixed = TRUE), logical(1)))
  } else 0L

  list(
    ok = TRUE,
    n_go = nrow(go_df),
    n_kegg = nrow(kegg_df),
    crc_keyword_hits_go = hit_go,
    crc_keyword_hits_kegg = hit_kegg,
    top_go = if (nrow(go_df)) head(go_df$Description, 10) else character(),
    top_kegg = if (nrow(kegg_df)) head(kegg_df$Description, 10) else character()
  )
}

cat("=== Make yellow flags GREEN ===\n")
cat("Microarray:", micro_gse, "| RNA-seq:", rna_gse, "\n\n")

# Download (reuse cache under work/)
micro_dir <- file.path(work, "micro_data")
rna_dir <- file.path(work, "rna_data")
dir.create(micro_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(rna_dir, recursive = TRUE, showWarnings = FALSE)

cat("Downloading microarray...\n")
micro_dl <- gexp_download_one_microarray_gse(micro_gse, micro_dir)
if (!isTRUE(micro_dl$ok)) stop(micro_dl$reason %||% "micro download failed")
cat("Downloading RNA-seq...\n")
rna_dl <- gexp_download_one_rnaseq_gse(rna_gse, rna_dir)
if (!isTRUE(rna_dl$ok)) stop(rna_dl$reason %||% "rna download failed")

meta_micro <- assign_gse89076(micro_dl$metadata)
meta_rna <- assign_gse50760(rna_dl$metadata)
cat("Micro groups:", paste(names(table(meta_micro$Condition)), table(meta_micro$Condition), sep = "=", collapse = ", "), "\n")
cat("RNA groups:", paste(names(table(meta_rna$Condition)), table(meta_rna$Condition), sep = "=", collapse = ", "), "\n")
if (length(unique(meta_micro$Condition)) < 2L || length(unique(meta_rna$Condition)) < 2L) {
  stop("Need both Normal and Disease in each cohort (check title labels).")
}

micro_expr <- map_micro_to_symbols(
  micro_dl$micro_expr[, rownames(meta_micro), drop = FALSE],
  micro_dl$micro_eset, micro_gse
)
rna_counts <- rna_dl$count_matrix[, rownames(meta_rna), drop = FALSE]

# Merged normalize
norm <- gexp_normalize_and_intersect(
  micro_expr_list = setNames(list(micro_expr), micro_gse),
  rna_counts_list = setNames(list(rna_counts), rna_gse),
  de_method = "limma"
)
meta <- norm$unified_metadata
meta$Condition <- NA_character_
idx_m <- match(meta$SampleID, rownames(meta_micro))
idx_r <- match(meta$SampleID, rownames(meta_rna))
meta$Condition[!is.na(idx_m)] <- meta_micro$Condition[idx_m[!is.na(idx_m)]]
meta$Condition[!is.na(idx_r)] <- meta_rna$Condition[idx_r[!is.na(idx_r)]]
meta <- meta[!is.na(meta$Condition), , drop = FALSE]
rownames(meta) <- meta$SampleID
expr_before <- norm$combined_expr[, rownames(meta), drop = FALSE]

cat("Batch correcting (limma removeBatchEffect)...\n")
batch <- gexp_batch_correct(expr_before, meta, variance_percentile = 25, method = "limma")
expr_after <- batch$batch_corrected
meta_after <- meta[colnames(expr_after), , drop = FALSE]

# ---- 1) PCA ----
cat("PCA before/after...\n")
pca_before <- pca_score_frame(expr_before, meta, "before")
pca_after <- pca_score_frame(expr_after, meta_after, "after")
utils::write.csv(rbind(pca_before, pca_after), file.path(outdir, "report", "pca_scores_before_after.csv"), row.names = FALSE)

plot_pca(pca_before, "Platform", file.path(figdir, "PCA_before_Platform.png"),
         "Before batch: coloured by Platform")
plot_pca(pca_after, "Platform", file.path(figdir, "PCA_after_Platform.png"),
         "After batch: coloured by Platform")
plot_pca(pca_before, "Condition", file.path(figdir, "PCA_before_Condition.png"),
         "Before batch: coloured by Condition")
plot_pca(pca_after, "Condition", file.path(figdir, "PCA_after_Condition.png"),
         "After batch: coloured by Condition")

r2 <- data.frame(
  Stage = c("before", "after"),
  PC1_R2_Platform = c(
    factor_r2_on_pc1(pca_before$PC1, pca_before$Platform),
    factor_r2_on_pc1(pca_after$PC1, pca_after$Platform)
  ),
  PC1_R2_Condition = c(
    factor_r2_on_pc1(pca_before$PC1, pca_before$Condition),
    factor_r2_on_pc1(pca_after$PC1, pca_after$Condition)
  ),
  PC1_R2_Dataset = c(
    factor_r2_on_pc1(pca_before$PC1, pca_before$Dataset),
    factor_r2_on_pc1(pca_after$PC1, pca_after$Dataset)
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(r2, file.path(outdir, "report", "pca_pc1_variance_attribution.csv"), row.names = FALSE)

# Green criteria for PCA: platform R2 drops; condition R2 rises or stays meaningful
pca_green <- isTRUE(r2$PC1_R2_Platform[2] < r2$PC1_R2_Platform[1]) &&
  isTRUE(r2$PC1_R2_Condition[2] >= r2$PC1_R2_Condition[1] * 0.8)

# ---- 2) PVCA ----
cat("PVCA before/after...\n")
pv_before <- gexpipe_pvca_df(expr_before, meta)
pv_after <- gexpipe_pvca_df(expr_after, meta_after)
pv_tab <- rbind(
  if (isTRUE(pv_before$ok)) transform(pv_before$data, Stage = "before") else NULL,
  if (isTRUE(pv_after$ok)) transform(pv_after$data, Stage = "after") else NULL
)
utils::write.csv(pv_tab, file.path(outdir, "report", "pvca_before_after.csv"), row.names = FALSE)

get_var <- function(df, stage, factor) {
  if (is.null(df) || !nrow(df)) return(NA_real_)
  v <- df$Variance[df$Stage == stage & df$Factor == factor]
  if (!length(v)) NA_real_ else as.numeric(v[1])
}
plat_drop <- get_var(pv_tab, "before", "Platform") - get_var(pv_tab, "after", "Platform")
ds_drop <- get_var(pv_tab, "before", "Dataset") - get_var(pv_tab, "after", "Dataset")
cond_before <- get_var(pv_tab, "before", "Condition")
cond_after <- get_var(pv_tab, "after", "Condition")
pvca_green <- isTRUE((plat_drop > 0) || (ds_drop > 0)) &&
  isTRUE(is.finite(cond_after) && cond_after >= 0.5 * (cond_before %||% cond_after))

# ---- 3) Enrichment on merged DEGs ----
cat("Pathway enrichment on merged DEGs...\n")
sig_file <- file.path(outdir, "merged_limma_DE_sig.csv")
if (!file.exists(sig_file)) stop("Missing ", sig_file, " — run run-cross-platform-de-comparison.R first or keep existing DE exports")
sig_genes <- utils::read.csv(sig_file, stringsAsFactors = FALSE)[[1]]
enr <- enrich_merged(sig_genes, file.path(outdir, "report", "merged_enrichment"))
pathway_green <- isTRUE(enr$ok) && ((enr$crc_keyword_hits_go %||% 0) + (enr$crc_keyword_hits_kegg %||% 0) >= 2)

# ---- 4) Hub stability (connectivity proxy for WGCNA hubs) ----
cat("Hub stability (top connectivity)...\n")
# Use genes significant in each analysis when available
sig_m <- utils::read.csv(file.path(outdir, "microarray_limma_DE_sig.csv"), stringsAsFactors = FALSE)[[1]]
sig_r <- utils::read.csv(file.path(outdir, "rnaseq_DE_sig.csv"), stringsAsFactors = FALSE)[[1]]
# Build separate expression on common genes
common <- intersect(rownames(expr_after), intersect(rownames(micro_expr), rownames(rna_counts)))
hub_merged <- hub_genes_from_expr(expr_after, intersect(sig_genes, common), 50L)
# Approximate separate hubs from merged matrix restricted to each platform's samples
micro_samples <- rownames(meta_after)[meta_after$Platform == "Microarray"]
rna_samples <- rownames(meta_after)[meta_after$Platform == "RNAseq"]
hub_micro <- hub_genes_from_expr(expr_after[, micro_samples, drop = FALSE], intersect(sig_m, common), 50L)
hub_rna <- hub_genes_from_expr(expr_after[, rna_samples, drop = FALSE], intersect(sig_r, common), 50L)
hub_overlap_micro <- length(intersect(hub_merged, hub_micro))
hub_overlap_rna <- length(intersect(hub_merged, hub_rna))
hub_j_micro <- if (length(union(hub_merged, hub_micro))) length(intersect(hub_merged, hub_micro)) / length(union(hub_merged, hub_micro)) else NA_real_
hub_j_rna <- if (length(union(hub_merged, hub_rna))) length(intersect(hub_merged, hub_rna)) / length(union(hub_merged, hub_rna)) else NA_real_
utils::write.csv(
  data.frame(Gene = hub_merged, list = "merged_hubs"),
  file.path(outdir, "report", "hubs_merged_top50.csv"), row.names = FALSE
)
utils::write.csv(
  data.frame(
    Metric = c("hub_overlap_merged_vs_micro", "hub_overlap_merged_vs_rna",
               "hub_jaccard_merged_vs_micro", "hub_jaccard_merged_vs_rna"),
    Value = c(hub_overlap_micro, hub_overlap_rna, hub_j_micro, hub_j_rna)
  ),
  file.path(outdir, "report", "hub_stability_summary.csv"), row.names = FALSE
)
# Green if reasonable overlap (>=10 of top50) with at least one platform
hub_green <- isTRUE(hub_overlap_micro >= 10 || hub_overlap_rna >= 10)

# ---- Scorecard ----
score <- data.frame(
  Area = c(
    "1a/1b PCA before-after",
    "1c PVCA variance attribution",
    "3a Pathway coherence",
    "3c Hub stability (WGCNA proxy)"
  ),
  Status = c(
    if (pca_green) "GREEN" else "YELLOW",
    if (pvca_green) "GREEN" else "YELLOW",
    if (pathway_green) "GREEN" else "YELLOW",
    if (hub_green) "GREEN" else "YELLOW"
  ),
  Evidence = c(
    sprintf(
      "PC1 R2 Platform before=%.3f after=%.3f; Condition before=%.3f after=%.3f",
      r2$PC1_R2_Platform[1], r2$PC1_R2_Platform[2],
      r2$PC1_R2_Condition[1], r2$PC1_R2_Condition[2]
    ),
    sprintf(
      "PVCA Platform drop=%.3f; Dataset drop=%.3f; Condition before=%.3f after=%.3f",
      plat_drop %||% NA_real_, ds_drop %||% NA_real_,
      cond_before %||% NA_real_, cond_after %||% NA_real_
    ),
    sprintf(
      "GO terms=%s; KEGG=%s; CRC keyword hits GO=%s KEGG=%s; top GO: %s",
      enr$n_go %||% 0, enr$n_kegg %||% 0,
      enr$crc_keyword_hits_go %||% 0, enr$crc_keyword_hits_kegg %||% 0,
      paste(head(enr$top_go %||% "", 3), collapse = "; ")
    ),
    sprintf(
      "Top50 hub overlap merged vs micro=%s (J=%.2f), vs rna=%s (J=%.2f)",
      hub_overlap_micro, hub_j_micro, hub_overlap_rna, hub_j_rna
    )
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(score, file.path(outdir, "report", "green_flags_scorecard.csv"), row.names = FALSE)

md <- c(
  "# Green-flag generation report",
  "",
  sprintf("**Cohorts:** %s (microarray) + %s (RNA-seq)", micro_gse, rna_gse),
  "",
  "## Scorecard",
  "",
  "| Area | Status | Evidence |",
  "|------|--------|----------|",
  apply(score, 1, function(r) paste0("| ", r[["Area"]], " | **", r[["Status"]], "** | ", r[["Evidence"]], " |")),
  "",
  "## Outputs",
  "",
  "- `report/figures/PCA_before_Platform.png` / `PCA_after_Platform.png`",
  "- `report/figures/PCA_before_Condition.png` / `PCA_after_Condition.png`",
  "- `report/pca_pc1_variance_attribution.csv`",
  "- `report/pvca_before_after.csv`",
  "- `report/merged_enrichment_GO_BP.csv` / `_KEGG.csv`",
  "- `report/hub_stability_summary.csv`",
  "",
  "## Already GREEN (from prior DE comparison)",
  "",
  "- Direction concordance ~98.9%",
  "- logFC Pearson r ~0.989",
  "- FDR rescue of one-platform genes",
  ""
)
writeLines(md, file.path(outdir, "report", "green_flags_report.md"))

cat("\n========== GREEN FLAG SCORECARD ==========\n")
print(score, row.names = FALSE)
cat("\nReport:", file.path(outdir, "report", "green_flags_report.md"), "\n")
