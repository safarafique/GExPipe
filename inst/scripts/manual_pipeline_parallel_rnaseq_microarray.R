# ==============================================================================
# GExPipe MANUAL PIPELINE - PARALLEL RNA-SEQ + MICROARRAY -> BIOMARKERS
# (plain R, NOT Shiny)
#
# Bulk RNA-seq and microarray are processed as TWO SEPARATE ARMS, side by side
# (same design as the app's "Parallel DE, then merge" mode):
#
#   RNA-seq arm    : download -> TMM/log-CPM (+ raw counts) -> DE (DESeq2/edgeR/voom)
#   Microarray arm : download -> symbols -> quantile norm -> batch -> DE (limma)
#                         \                                        /
#                          +---- consensus DEGs (both platforms, same direction)
#                                          |
#   WGCNA (one network on the larger platform) -> DEG ∩ module genes
#   -> GO/KEGG -> PPI hub genes -> machine learning (LASSO, RF, SVM-RFE,
#   Boruta) -> final biomarkers -> ROC -> cross-platform validation on the
#   OTHER platform -> nomogram + calibration (+ DCA)
#
# The platforms are never merged into one matrix: RNA-seq counts and
# microarray intensities are on different scales. They meet only at the
# consensus DEG list (Step 8) and again at validation (Step 15), where each
# platform is z-scored within its own datasets.
#
# Completely separate from the Shiny app code - does not source or depend on
# inst/shinyapp/ or R/server_*.R / R/ui_*.R. Calls the same underlying
# functions the app uses internally, so results match. Run ONE STEP AT A
# TIME in RStudio (select the block, Ctrl+Enter) and check the output.
# ==============================================================================

library(GExPipe)

# ==============================================================================
# STEP 1: USER INPUT
# One or more GSEs per platform. Each GSE has its own phenodata columns, so
# group_map has one entry PER GSE (both platforms) - edit col/normal/disease.
# ==============================================================================
micro_gse_ids <- c("GSE_MICRO1")            # <-- microarray GSE(s), e.g. c("GSE1","GSE2")
rna_gse_ids   <- c("GSE_RNA1")              # <-- RNA-seq GSE(s)

group_map <- list(
  GSE_MICRO1 = list(col = "disease state:ch1", normal = c("control"), disease = c("disease")),
  GSE_RNA1   = list(col = "tissue:ch1",        normal = c("normal"),  disease = c("tumor"))
  # GSE_X    = list(col = "group:ch1",         normal = c("healthy"), disease = c("case")),
)
# ^ exactly one entry per ID in micro_gse_ids AND rna_gse_ids, named identically.

de_method_choice <- 1   # <-- RNA-seq DE engine: 1 = DESeq2, 2 = edgeR, 3 = limma-voom
de_method <- switch(
  as.character(de_method_choice),
  "1" = "deseq2",
  "2" = "edger",
  "3" = "limma_voom",
  stop("de_method_choice must be 1, 2, or 3")
)

logfc_cutoff <- 0.5     # |log2FC| cutoff, applied on BOTH platforms
padj_cutoff  <- 0.05    # BH-adjusted p cutoff, applied on BOTH platforms

consensus_combine        <- "intersection" # "intersection" (recommended) or "union" (exploratory)
consensus_same_direction <- TRUE           # drop genes whose logFC sign disagrees between platforms

wgcna_platform <- "auto"   # "auto" (platform with more samples), "RNAseq" or "Microarray"
                           # The OTHER platform is then used for independent validation.

ml_min_methods <- 2        # a gene is a final biomarker if >= this many ML methods pick it
set.seed(123)

work_dir <- file.path(getwd(), "manual_pipeline_data",
                      paste0("parallel_", paste(c(micro_gse_ids, rna_gse_ids), collapse = "_")))
dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)
cat("Outputs will be written to:", work_dir, "\n")

missing_map <- setdiff(c(micro_gse_ids, rna_gse_ids), names(group_map))
if (length(missing_map)) stop("group_map has no entry for: ", paste(missing_map, collapse = ", "))

# ==============================================================================
# STEP 2: Download BOTH platforms
# fast = FALSE is required on both: the default fast=TRUE skips gene-ID ->
# symbol conversion, and every later step (consensus matching across
# platforms, WGCNA, GO/KEGG, STRINGdb) assumes real gene symbols.
# ==============================================================================

## ---- 2A: Microarray arm ----
library(org.Hs.eg.db)
# Finishes probe -> symbol conversion when the platform annotation stops at
# an intermediate ID (RefSeq / Ensembl transcript / accession) - see
# manual_pipeline_microarray.R Step 2b for the details.
.gexpipe_finish_to_symbol <- function(ids) {
  ids <- as.character(ids)
  if (isTRUE(GExPipe:::gexpipe_ids_are_verified_symbols(ids))) return(ids)
  best <- rep(NA_character_, length(ids))
  for (kt in c("REFSEQ", "ENSEMBLTRANS", "ACCNUM", "SYMBOL")) {
    m <- tryCatch(
      AnnotationDbi::mapIds(org.Hs.eg.db, keys = ids, keytype = kt, column = "SYMBOL", multiVals = "first"),
      error = function(e) NULL
    )
    if (!is.null(m) && sum(!is.na(m)) > sum(!is.na(best))) best <- unname(m)
  }
  best
}

micro_expr_list <- list()
pdata_list      <- list()
for (gse in micro_gse_ids) {
  dl <- gexp_download_one_microarray_gse(gse, work_dir, fast = FALSE)
  if (!isTRUE(dl$ok)) {
    warning(gse, " (microarray) failed: ", dl$reason)
    next
  }
  m <- dl$micro_expr
  sym <- .gexpipe_finish_to_symbol(rownames(m))
  rownames(m) <- sym
  m <- m[!is.na(sym) & nzchar(sym), , drop = FALSE]
  if (any(duplicated(rownames(m)))) m <- limma::avereps(m, ID = rownames(m))
  micro_expr_list[[gse]] <- m
  pdata_list[[gse]]      <- dl$metadata
  cat("[Microarray]", gse, ":", nrow(m), "genes (symbols) x", ncol(m), "samples\n")
}

## ---- 2B: RNA-seq arm ----
rna_counts_list <- list()
for (gse in rna_gse_ids) {
  dl <- gexp_download_one_rnaseq_gse(gse, work_dir, fast = FALSE)
  if (!isTRUE(dl$ok)) {
    warning(gse, " (RNA-seq) failed: ", dl$reason)
    next
  }
  rna_counts_list[[gse]] <- dl$count_matrix   # raw integer counts, gene symbols
  pdata_list[[gse]]      <- dl$metadata
  cat("[RNA-seq]   ", gse, ":", nrow(dl$count_matrix), "genes x", ncol(dl$count_matrix), "samples\n")
}

if (length(micro_expr_list) < 1L || length(rna_counts_list) < 1L) {
  stop("Parallel mode needs at least 1 microarray AND 1 RNA-seq dataset - check the warnings above.")
}
# View(pdata_list[["GSE_MICRO1"]])   # browse phenodata to fill in group_map (Step 1)

# ==============================================================================
# STEP 3: Normalize - two SEPARATE runs (keep_platforms_separate = TRUE)
# Microarray: quantile. RNA-seq: TMM -> log-CPM, raw counts kept for DE.
# Genes are intersected within each platform only; global quantile is off.
# ==============================================================================
norm_out <- gexp_normalize_and_intersect(
  micro_expr_list         = micro_expr_list,
  rna_counts_list         = rna_counts_list,
  micro_norm_method       = "quantile",
  rnaseq_norm_method      = "TMM",
  keep_platforms_separate = TRUE
)
cat(norm_out$log_text)

expr_micro   <- norm_out$expr_micro             # microarray: normalized log intensities
expr_rna     <- norm_out$expr_rna               # RNA-seq: log-CPM (QC only)
counts_rna   <- norm_out$raw_counts_for_deseq2  # RNA-seq: raw integer counts (DE + VST)
unified_meta <- norm_out$unified_metadata       # SampleID, Platform, Dataset, Condition (NA for now)
table(unified_meta$Platform, unified_meta$Dataset)

# ==============================================================================
# STEP 4: QC - PCA per platform, colored by Dataset
# ==============================================================================
.pca_plot <- function(mat, groups, title) {
  mat <- mat[stats::complete.cases(mat), , drop = FALSE]
  mat <- mat[apply(mat, 1, stats::var) > 0, , drop = FALSE]
  pca <- prcomp(t(mat), scale. = TRUE)
  grp <- factor(groups)
  plot(pca$x[, 1], pca$x[, 2], col = as.numeric(grp), pch = 19,
       main = title, xlab = "PC1", ylab = "PC2")
  text(pca$x[, 1], pca$x[, 2], labels = colnames(mat), pos = 3, cex = 0.5)
  legend("topright", legend = levels(grp), col = seq_along(levels(grp)), pch = 19, cex = 0.8)
}
.pca_plot(expr_micro, unified_meta[colnames(expr_micro), "Dataset"], "Microarray - PCA by Dataset")
.pca_plot(expr_rna,   unified_meta[colnames(expr_rna),   "Dataset"], "RNA-seq (log-CPM) - PCA by Dataset")

# If a sample looks like an outlier, drop it from unified_meta before Step 5, e.g.:
# outliers <- c("GSMxxxxxxx")
# unified_meta <- unified_meta[!rownames(unified_meta) %in% outliers, , drop = FALSE]

# ==============================================================================
# STEP 5: Assign groups (Normal / Disease) PER DATASET using group_map
# ==============================================================================
condition <- setNames(rep(NA_character_, nrow(unified_meta)), rownames(unified_meta))
for (gse in names(pdata_list)) {
  gm <- group_map[[gse]]
  if (!gm$col %in% colnames(pdata_list[[gse]])) {
    stop("Column '", gm$col, "' not in ", gse, " phenodata. Available: ",
         paste(colnames(pdata_list[[gse]]), collapse = ", "))
  }
  gse_samples <- intersect(rownames(unified_meta)[unified_meta$Dataset == gse],
                           rownames(pdata_list[[gse]]))
  raw_vals <- as.character(pdata_list[[gse]][gse_samples, gm$col])
  condition[gse_samples] <- ifelse(
    raw_vals %in% gm$normal, "Normal",
    ifelse(raw_vals %in% gm$disease, "Disease", NA_character_)
  )
}
unified_meta$Condition <- factor(condition[rownames(unified_meta)], levels = c("Normal", "Disease"))
meta <- unified_meta[!is.na(unified_meta$Condition), , drop = FALSE]

# Every dataset should have BOTH groups (otherwise Dataset and Condition are confounded)
print(table(meta$Dataset, meta$Condition))

micro_ids <- intersect(gexpipe_platform_sample_ids(meta, "Microarray"), colnames(expr_micro))
rna_ids   <- intersect(gexpipe_platform_sample_ids(meta, "RNAseq"),     colnames(counts_rna))
meta_micro <- meta[micro_ids, , drop = FALSE]
meta_rna   <- meta[rna_ids,   , drop = FALSE]

for (p in list(list("Microarray", meta_micro), list("RNA-seq", meta_rna))) {
  n <- table(p[[2]]$Condition)
  if (any(n < 2)) {
    stop(p[[1]], " has fewer than 2 samples in a group (", paste(names(n), n, collapse = ", "),
         "). Fix group_map in Step 1: run table(pdata_list[[\"GSE\"]]$`column`) to see real values.")
  }
}
cat("Microarray samples:", length(micro_ids), "| RNA-seq samples:", length(rna_ids), "\n")

# ==============================================================================
# STEP 6: Per-platform batch handling (within each platform only)
# Microarray: gexp_batch_correct() across microarray GSEs (variance filter
#   only when there is a single GSE).
# RNA-seq: DESeq2 VST of raw counts (never raw counts/CPM for WGCNA/ML);
#   with 2+ RNA-seq GSEs, Dataset is removed with limma::removeBatchEffect
#   while protecting Condition. DE in Step 7 still uses RAW counts with
#   Dataset as a covariate, same as the app.
# ==============================================================================
## ---- 6A: Microarray ----
micro_batch_method <- "combat_ref"
if (length(unique(meta_micro$Dataset)) > 1L) {
  conf <- gexpipe_batch_confounding_summary(meta_micro)
  cat("[Microarray]", conf$message, "\n")
  if (isTRUE(conf$confounded)) micro_batch_method <- "limma"
}
micro_batch_out <- gexp_batch_correct(
  expr                = expr_micro[, micro_ids, drop = FALSE],
  metadata            = meta_micro,
  variance_percentile = 25,
  method              = micro_batch_method
)
expr_micro_bc <- micro_batch_out$batch_corrected
cat(micro_batch_out$log_text)

## ---- 6B: RNA-seq ----
expr_rna_vst <- GExPipe:::gexpipe_counts_to_vst(counts_rna, sample_ids = rna_ids)  # not exported
if (length(unique(meta_rna$Dataset)) > 1L) {
  expr_rna_vst <- limma::removeBatchEffect(
    expr_rna_vst,
    batch  = meta_rna[colnames(expr_rna_vst), "Dataset"],
    design = stats::model.matrix(~Condition, data = meta_rna[colnames(expr_rna_vst), , drop = FALSE])
  )
}
cat("[RNA-seq] VST matrix:", nrow(expr_rna_vst), "genes x", ncol(expr_rna_vst), "samples\n")

.pca_plot(expr_micro_bc, meta_micro[colnames(expr_micro_bc), "Condition"], "Microarray (after batch) - by Condition")
.pca_plot(expr_rna_vst,  meta_rna[colnames(expr_rna_vst), "Condition"],    "RNA-seq (VST) - by Condition")

# ==============================================================================
# STEP 7: Differential expression - BOTH ARMS, independently
# ==============================================================================
## ---- 7A: Microarray DE (limma on normalized intensities, Dataset covariate if 2+ GSEs) ----
de_micro <- gexpipe_run_limma_on_subset(
  expr_micro[, micro_ids, drop = FALSE], meta_micro,
  logfc_cutoff = logfc_cutoff, padj_cutoff = padj_cutoff,
  ref_lab = "Normal", alt_lab = "Disease"
)

## ---- 7B: RNA-seq DE (raw counts, chosen engine, Dataset covariate if 2+ GSEs) ----
de_rna <- GExPipe:::gexpipe_run_count_de(   # not exported; accessed via :::
  counts_rna[, rna_ids, drop = FALSE], meta_rna,
  method       = de_method,
  logfc_cutoff = logfc_cutoff, padj_cutoff = padj_cutoff,
  ref_lab = "Normal", alt_lab = "Disease"
)

cat("Microarray (limma):  ", nrow(de_micro$de_results), "tested |", nrow(de_micro$sig_genes), "DEGs\n")
cat("RNA-seq (", de_method, "): ", nrow(de_rna$de_results), "tested |", nrow(de_rna$sig_genes), "DEGs\n", sep = "")

write.csv(de_micro$de_results, file.path(work_dir, "DE_microarray_limma.csv"), row.names = FALSE)
write.csv(de_rna$de_results,   file.path(work_dir, paste0("DE_rnaseq_", de_method, ".csv")), row.names = FALSE)

# Volcano plots, one per arm
.volcano <- function(de, title) {
  col <- ifelse(de$Significance == "Up-regulated", "firebrick",
                ifelse(de$Significance == "Down-regulated", "steelblue", "grey70"))
  plot(de$logFC, -log10(de$adj.P.Val), pch = 20, cex = 0.5, col = col,
       xlab = "log2 fold change (Disease vs Normal)", ylab = "-log10 adj. P", main = title)
  abline(v = c(-logfc_cutoff, logfc_cutoff), h = -log10(padj_cutoff), lty = 2, col = "grey40")
}
.volcano(de_micro$de_results, "Microarray - volcano")
.volcano(de_rna$de_results,   paste0("RNA-seq (", de_method, ") - volcano"))

# ==============================================================================
# STEP 8: Consensus DEGs (RNA-seq ∩ microarray, same direction)
# ==============================================================================
consensus <- gexpipe_consensus_degs(
  de_rna$sig_genes, de_micro$sig_genes,
  require_same_direction = consensus_same_direction,
  combine                = consensus_combine
)
sig_genes <- consensus$table   # Gene, logFC_rna, logFC_micro, logFC (mean), adj.P.Val (max), Direction

cat("RNA-seq DEGs:", consensus$n_rna, "| Microarray DEGs:", consensus$n_micro,
    "| Overlap:", consensus$n_overlap, "| Discordant direction:", consensus$n_discordant,
    "| CONSENSUS:", consensus$n_consensus, "\n")
table(sig_genes$Direction)

if (consensus$n_consensus < 5L) {
  warning("Very few consensus DEGs. Consider relaxing logfc_cutoff/padj_cutoff, or ",
          "consensus_combine = \"union\" (exploratory) in Step 1.")
}

# Agreement of fold changes between platforms
plot(sig_genes$logFC_rna, sig_genes$logFC_micro, pch = 19, cex = 0.6,
     xlab = "log2FC RNA-seq", ylab = "log2FC microarray",
     main = sprintf("Consensus DEGs - cross-platform logFC (r = %.2f)",
                    stats::cor(sig_genes$logFC_rna, sig_genes$logFC_micro, use = "complete.obs")))
abline(h = 0, v = 0, lty = 2, col = "grey50")

write.csv(sig_genes, file.path(work_dir, "consensus_DEGs.csv"), row.names = FALSE)

# ==============================================================================
# STEP 9: WGCNA - ONE network on one platform (top-variable genes, NOT the
# DEG list). "auto" = the platform with more samples. The other platform is
# kept back for independent validation in Step 15.
# ==============================================================================
library(WGCNA)
WGCNA::enableWGCNAThreads()

if (identical(wgcna_platform, "auto")) {
  wgcna_platform <- if (length(rna_ids) >= length(micro_ids)) "RNAseq" else "Microarray"
}
val_platform <- if (identical(wgcna_platform, "RNAseq")) "Microarray" else "RNAseq"
train_expr <- if (identical(wgcna_platform, "RNAseq")) expr_rna_vst else expr_micro_bc
train_meta <- if (identical(wgcna_platform, "RNAseq")) meta_rna     else meta_micro
val_expr   <- if (identical(wgcna_platform, "RNAseq")) expr_micro_bc else expr_rna_vst
val_meta   <- if (identical(wgcna_platform, "RNAseq")) meta_micro    else meta_rna
cat("WGCNA / training platform:", wgcna_platform, "(", ncol(train_expr), "samples )",
    "| validation platform:", val_platform, "(", ncol(val_expr), "samples )\n")

prep <- gexp_wgcna_prepare(
  train_expr, train_meta,
  gene_mode = "top_variable",
  top_genes = 5000L
)
datExpr <- prep$datExpr          # samples x genes (WGCNA convention)
sample_info_wgcna <- prep$sample_info

powers <- c(1:10, seq(12, 20, 2))
sft <- WGCNA::pickSoftThreshold(datExpr, powerVector = powers, networkType = "signed", verbose = 2)
print(sft$fitIndices)
par(mfrow = c(1, 2))
plot(sft$fitIndices[, 1], -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2],
     xlab = "Soft threshold (power)", ylab = "Scale-free fit, signed R^2",
     main = "Scale independence", type = "n")
text(sft$fitIndices[, 1], -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2], labels = powers, col = "red")
abline(h = 0.85, col = "red")
plot(sft$fitIndices[, 1], sft$fitIndices[, 5], xlab = "Soft threshold (power)",
     ylab = "Mean connectivity", main = "Mean connectivity", type = "n")
text(sft$fitIndices[, 1], sft$fitIndices[, 5], labels = powers, col = "red")
par(mfrow = c(1, 1))

soft_power <- sft$powerEstimate
if (is.na(soft_power)) soft_power <- 12   # usual signed-network fallback when no power reaches R^2 0.85
cat("Using soft power:", soft_power, "\n")

net <- WGCNA::blockwiseModules(
  datExpr,
  power             = soft_power,
  TOMType           = "signed",
  minModuleSize     = 30,
  reassignThreshold = 0,
  mergeCutHeight    = 0.25,
  numericLabels     = TRUE,
  pamRespectsDendro = FALSE,
  maxBlockSize      = 6000,
  verbose           = 3
)

module_colors <- WGCNA::labels2colors(net$colors)
names(module_colors) <- colnames(datExpr)
MEs <- WGCNA::orderMEs(WGCNA::moduleEigengenes(datExpr, module_colors)$eigengenes)
table(module_colors)

WGCNA::plotDendroAndColors(
  net$dendrograms[[1]], module_colors[net$blockGenes[[1]]],
  "Module colors", dendroLabels = FALSE, hang = 0.03,
  addGuide = TRUE, guideHang = 0.05
)

# Module-trait relationship (Disease = 1, Normal = 0)
condition_num    <- as.numeric(sample_info_wgcna$Condition == "Disease")
module_trait_cor <- stats::cor(MEs, condition_num, use = "p")
module_trait_p   <- WGCNA::corPvalueStudent(module_trait_cor, nrow(datExpr))
module_trait_table <- data.frame(
  Module      = sub("^ME", "", rownames(module_trait_cor)),
  Correlation = round(module_trait_cor[, 1], 4),
  P_value     = signif(module_trait_p[, 1], 4)
)
module_trait_table <- module_trait_table[module_trait_table$Module != "grey", ]
module_trait_table <- module_trait_table[order(module_trait_table$P_value), ]
print(module_trait_table)

WGCNA::labeledHeatmap(
  Matrix = module_trait_cor, xLabels = "Disease", yLabels = rownames(module_trait_cor),
  ySymbols = rownames(module_trait_cor), colorLabels = FALSE,
  colors = WGCNA::blueWhiteRed(50),
  textMatrix = paste0(signif(module_trait_cor, 2), "\n(", signif(module_trait_p, 1), ")"),
  setStdMargins = FALSE, cex.text = 0.6, zlim = c(-1, 1), main = "Module-trait relationships"
)

# Gene significance (GS) and module membership (MM) for every gene
gene_GS <- as.numeric(stats::cor(datExpr, condition_num, use = "p"))
names(gene_GS) <- colnames(datExpr)
gene_MM <- stats::cor(datExpr, MEs, use = "p")

write.csv(module_trait_table, file.path(work_dir, "WGCNA_module_trait.csv"), row.names = FALSE)
write.csv(data.frame(Gene = names(module_colors), Module = module_colors, GS = gene_GS[names(module_colors)]),
          file.path(work_dir, "WGCNA_gene_modules.csv"), row.names = FALSE)

# ==============================================================================
# STEP 10: Key module genes ∩ consensus DEGs -> candidate genes
# Trait-associated modules: |r| > 0.3 and p < 0.05 (edit if needed).
# Within them, keep hub-like genes: |MM| > 0.6 and |GS| > 0.2.
# ==============================================================================
trait_sig_modules <- module_trait_table$Module[
  abs(module_trait_table$Correlation) > 0.3 & module_trait_table$P_value < 0.05
]
if (length(trait_sig_modules) == 0L) {
  trait_sig_modules <- head(module_trait_table$Module, 1)
  warning("No module passed |r|>0.3 & p<0.05 - using the single most associated module: ", trait_sig_modules)
}
cat("Trait-associated modules:", paste(trait_sig_modules, collapse = ", "), "\n")

wgcna_module_genes <- names(module_colors)[module_colors %in% trait_sig_modules]
key_module_genes <- wgcna_module_genes[vapply(wgcna_module_genes, function(g) {
  abs(gene_MM[g, paste0("ME", module_colors[[g]])]) > 0.6 && abs(gene_GS[[g]]) > 0.2
}, logical(1))]

common_genes <- intersect(sig_genes$Gene, wgcna_module_genes)   # all module genes
key_genes    <- intersect(sig_genes$Gene, key_module_genes)     # hub-like module genes
cat("Consensus DEGs:", nrow(sig_genes),
    "| trait-module genes:", length(wgcna_module_genes),
    "| DEG ∩ module:", length(common_genes),
    "| DEG ∩ key (MM/GS) module genes:", length(key_genes), "\n")

# Use the stricter key-gene list when it is big enough, else all DEG ∩ module genes
candidate_genes <- if (length(key_genes) >= 10L) key_genes else common_genes
if (length(candidate_genes) < 3L) {
  stop("Fewer than 3 candidate genes - relax DE cutoffs (Step 1) or the module thresholds above.")
}

if (requireNamespace("VennDiagram", quietly = TRUE)) {
  grid::grid.newpage()
  grid::grid.draw(VennDiagram::venn.diagram(
    x = list(`RNA-seq DEGs` = de_rna$sig_genes$Gene, `Microarray DEGs` = de_micro$sig_genes$Gene,
             `WGCNA modules` = wgcna_module_genes),
    filename = NULL, fill = c("#E69F00", "#56B4E9", "#009E73"), alpha = 0.5, cex = 1.2, cat.cex = 1
  ))
}
writeLines(candidate_genes, file.path(work_dir, "candidate_genes.txt"))

# ==============================================================================
# STEP 11: GO / KEGG enrichment of the DEG ∩ module genes
# ==============================================================================
library(clusterProfiler)

entrez_ids <- AnnotationDbi::mapIds(org.Hs.eg.db, keys = common_genes, keytype = "SYMBOL", column = "ENTREZID")
entrez_ids <- entrez_ids[!is.na(entrez_ids)]

go_result <- clusterProfiler::enrichGO(
  gene = entrez_ids, OrgDb = org.Hs.eg.db, keyType = "ENTREZID",
  ont = "BP", pAdjustMethod = "BH", pvalueCutoff = 0.05, qvalueCutoff = 0.2, readable = TRUE
)
kegg_result <- tryCatch(
  clusterProfiler::enrichKEGG(gene = entrez_ids, organism = "hsa", pAdjustMethod = "BH", pvalueCutoff = 0.05),
  error = function(e) { message("KEGG failed (needs internet): ", conditionMessage(e)); NULL }
)

if (!is.null(go_result) && nrow(as.data.frame(go_result)) > 0) {
  print(clusterProfiler::dotplot(go_result, showCategory = 15, title = "GO: Biological Process"))
  write.csv(as.data.frame(go_result), file.path(work_dir, "GO_BP.csv"), row.names = FALSE)
}
if (!is.null(kegg_result) && nrow(as.data.frame(kegg_result)) > 0) {
  print(clusterProfiler::dotplot(kegg_result, showCategory = 15, title = "KEGG pathways"))
  write.csv(as.data.frame(kegg_result), file.path(work_dir, "KEGG.csv"), row.names = FALSE)
}

# ==============================================================================
# STEP 12: PPI network (STRINGdb) + hub genes
# ==============================================================================
valid_genes <- common_genes[common_genes %in% AnnotationDbi::keys(org.Hs.eg.db, keytype = "SYMBOL")]

ppi_data <- GExPipe:::gexp_stringdb_get_ppi_data_safe(  # not exported; accessed via :::
  score_threshold = 400,   # 400 = medium confidence
  valid_genes     = valid_genes
)
hub_scores <- NULL
if (is.null(ppi_data$interactions)) {
  warning("STRINGdb failed for all versions tried - skipping PPI: ",
          paste(ppi_data$try_errors, collapse = " | "))
} else {
  mapped       <- ppi_data$mapped
  interactions <- ppi_data$interactions
  id_col       <- ppi_data$id_col
  cat("STRING version used:", ppi_data$version_used, "|", ppi_data$pct_mapped, "% of genes mapped\n")

  sym_col <- if ("SYMBOL" %in% colnames(mapped)) "SYMBOL" else colnames(mapped)[min(2, ncol(mapped))]
  vertices_df <- unique(data.frame(STRING_id = mapped[[id_col]], SYMBOL = mapped[[sym_col]],
                                   stringsAsFactors = FALSE))
  vertices_df <- vertices_df[!is.na(vertices_df$STRING_id) & nzchar(vertices_df$STRING_id), ]
  vertices_df <- vertices_df[!duplicated(vertices_df$STRING_id), ]

  from_col <- if ("from" %in% colnames(interactions)) "from" else "protein1"
  to_col   <- if ("to" %in% colnames(interactions)) "to" else "protein2"
  edge_list <- interactions[
    interactions[[from_col]] %in% vertices_df$STRING_id & interactions[[to_col]] %in% vertices_df$STRING_id,
    c(from_col, to_col)
  ]
  colnames(edge_list) <- c("from", "to")

  library(igraph)
  g <- igraph::simplify(igraph::graph_from_data_frame(edge_list, directed = FALSE, vertices = vertices_df))
  g <- igraph::delete_vertices(g, igraph::degree(g) == 0)

  hub_scores <- data.frame(
    Gene        = igraph::V(g)$SYMBOL,
    Degree      = igraph::degree(g),
    Betweenness = igraph::betweenness(g, normalized = TRUE),
    Closeness   = igraph::closeness(g, normalized = TRUE),
    PageRank    = igraph::page_rank(g)$vector
  )
  hub_scores <- hub_scores[order(-hub_scores$Degree, -hub_scores$PageRank), ]
  cat("PPI network:", igraph::vcount(g), "nodes,", igraph::ecount(g), "edges\n")
  print(head(hub_scores, 15))

  plot(g, vertex.label = igraph::V(g)$SYMBOL, vertex.size = 6 + 2 * sqrt(igraph::degree(g)),
       vertex.label.cex = 0.6, main = "PPI network (DEG ∩ WGCNA module genes)")
  write.csv(hub_scores, file.path(work_dir, "PPI_hub_scores.csv"), row.names = FALSE)
}

# ==============================================================================
# STEP 13: Machine learning feature selection on the TRAINING platform
# Expression is z-scored per gene within each dataset, so the model learns
# relative expression and can be applied to the other platform in Step 15.
# Methods: LASSO, Random Forest, SVM-RFE (kernlab), Boruta (if installed).
# ==============================================================================
.zscore_by_dataset <- function(expr, meta) {
  out <- expr
  for (ds in unique(meta[colnames(expr), "Dataset"])) {
    cols <- colnames(expr)[meta[colnames(expr), "Dataset"] == ds]
    z <- t(scale(t(expr[, cols, drop = FALSE])))
    z[!is.finite(z)] <- 0
    out[, cols] <- z
  }
  out
}

ml_genes <- intersect(intersect(candidate_genes, rownames(train_expr)), rownames(val_expr))
cat("Candidate genes available on both platforms for ML:", length(ml_genes),
    "of", length(candidate_genes), "\n")
if (length(ml_genes) > 200L) {   # keep ML tractable: strongest consensus effects first
  ranked_genes <- sig_genes$Gene[order(-abs(sig_genes$logFC))]
  ml_genes <- head(ranked_genes[ranked_genes %in% ml_genes], 200)
}

X_train <- t(.zscore_by_dataset(train_expr[ml_genes, , drop = FALSE], train_meta))
y_train <- factor(train_meta[rownames(X_train), "Condition"], levels = c("Normal", "Disease"))
table(y_train)

ml_selected <- list()

## ---- 13A: LASSO (glmnet, binomial, cross-validated lambda) ----
nfolds <- max(3L, min(10L, min(table(y_train))))
cv_lasso <- glmnet::cv.glmnet(X_train, y_train, family = "binomial", alpha = 1, nfolds = nfolds)
plot(cv_lasso); title("LASSO - cross-validation", line = 2.5)
plot(cv_lasso$glmnet.fit, xvar = "lambda"); title("LASSO - coefficient paths", line = 2.5)
lasso_coef <- as.matrix(stats::coef(cv_lasso, s = "lambda.min"))
ml_selected$LASSO <- setdiff(rownames(lasso_coef)[lasso_coef[, 1] != 0], "(Intercept)")
cat("LASSO selected:", length(ml_selected$LASSO), "genes\n")

## ---- 13B: Random Forest (top genes by MeanDecreaseGini) ----
rf_model <- randomForest::randomForest(x = X_train, y = y_train, ntree = 500, importance = TRUE)
print(rf_model)
rf_imp <- randomForest::importance(rf_model)
rf_imp <- rf_imp[order(-rf_imp[, "MeanDecreaseGini"]), , drop = FALSE]
randomForest::varImpPlot(rf_model, n.var = min(20, nrow(rf_imp)), main = "Random Forest importance")
ml_selected$RandomForest <- head(rownames(rf_imp), min(15L, nrow(rf_imp)))

## ---- 13C: SVM-RFE (linear SVM, recursive feature elimination) ----
if (requireNamespace("kernlab", quietly = TRUE)) {
  svm_rfe_rank <- function(X, y) {
    remaining <- colnames(X)
    ranked <- character(0)
    while (length(remaining) > 1L) {
      fit <- kernlab::ksvm(X[, remaining, drop = FALSE], y, type = "C-svc",
                           kernel = "vanilladot", C = 1, scaled = FALSE)
      sv <- fit@SVindex
      w <- colSums(fit@coef[[1]] * X[sv, remaining, drop = FALSE])
      worst <- remaining[which.min(w^2)]
      ranked <- c(worst, ranked)
      remaining <- setdiff(remaining, worst)
    }
    c(remaining, ranked)   # best first
  }
  svm_rank <- suppressMessages(svm_rfe_rank(X_train, y_train))
  ml_selected$`SVM-RFE` <- head(svm_rank, min(15L, length(svm_rank)))
} else {
  message("Install 'kernlab' to run SVM-RFE (skipped).")
}

## ---- 13D: Boruta (all-relevant feature selection) ----
if (requireNamespace("Boruta", quietly = TRUE)) {
  boruta_out <- Boruta::Boruta(as.data.frame(X_train), y_train, maxRuns = 200, doTrace = 0)
  print(boruta_out)
  plot(boruta_out, las = 2, cex.axis = 0.6, xlab = "", main = "Boruta")
  ml_selected$Boruta <- Boruta::getSelectedAttributes(Boruta::TentativeRoughFix(boruta_out), withTentative = FALSE)
} else {
  message("Install 'Boruta' to run Boruta (skipped).")
}

## ---- 13E: Vote across methods -> final biomarkers ----
ml_selected <- ml_selected[lengths(ml_selected) > 0]
all_ml <- unique(unlist(ml_selected))
votes <- vapply(all_ml, function(gn) sum(vapply(ml_selected, function(s) gn %in% s, logical(1))), integer(1))
vote_table <- data.frame(
  Gene    = all_ml,
  Votes   = votes,
  Methods = vapply(all_ml, function(gn) paste(names(ml_selected)[vapply(ml_selected, function(s) gn %in% s, logical(1))], collapse = ";"), character(1)),
  RF_Gini = rf_imp[all_ml, "MeanDecreaseGini"],
  stringsAsFactors = FALSE
)
vote_table <- vote_table[order(-vote_table$Votes, -vote_table$RF_Gini), ]
print(vote_table)

biomarkers <- vote_table$Gene[vote_table$Votes >= min(ml_min_methods, length(ml_selected))]
if (length(biomarkers) == 0L) {
  biomarkers <- head(vote_table$Gene, 3)
  warning("No gene reached ", ml_min_methods, " votes - using the top 3 by votes/RF importance.")
}
cat("\n==== FINAL BIOMARKERS (", length(biomarkers), ") ====\n", paste(biomarkers, collapse = ", "), "\n")

if (length(ml_selected) >= 2L && requireNamespace("UpSetR", quietly = TRUE)) {
  print(UpSetR::upset(UpSetR::fromList(ml_selected), order.by = "freq", nsets = length(ml_selected)))
}
write.csv(vote_table, file.path(work_dir, "ML_votes.csv"), row.names = FALSE)
write.csv(sig_genes[sig_genes$Gene %in% biomarkers, ], file.path(work_dir, "FINAL_biomarkers.csv"), row.names = FALSE)

# ==============================================================================
# STEP 14: ROC on the training platform (each biomarker + combined model)
# The combined model is a Firth logistic regression (logistf), which stays
# stable when the groups are perfectly separated.
# ==============================================================================
library(pROC)

X_val <- t(.zscore_by_dataset(val_expr[biomarkers, , drop = FALSE], val_meta))
y_val <- factor(val_meta[rownames(X_val), "Condition"], levels = c("Normal", "Disease"))

safe_names <- make.names(biomarkers)
names(safe_names) <- biomarkers
train_df <- data.frame(Condition = as.numeric(y_train == "Disease"), X_train[, biomarkers, drop = FALSE])
val_df   <- data.frame(Condition = as.numeric(y_val == "Disease"),   X_val[, biomarkers, drop = FALSE])
colnames(train_df) <- colnames(val_df) <- c("Condition", safe_names)

model_formula <- stats::as.formula(paste("Condition ~", paste(safe_names, collapse = " + ")))
combined_fit <- tryCatch(
  logistf::logistf(model_formula, data = train_df),
  error = function(e) { message("logistf failed, using glm: ", conditionMessage(e));
                        suppressWarnings(stats::glm(model_formula, data = train_df, family = binomial)) }
)
.predict_prob <- function(fit, newdata) {
  if (inherits(fit, "logistf")) {
    lp <- stats::model.matrix(stats::delete.response(stats::terms(model_formula)), newdata) %*% stats::coef(fit)
    as.numeric(stats::plogis(lp))
  } else {
    as.numeric(stats::predict(fit, newdata = newdata, type = "response"))
  }
}
train_df$Score <- .predict_prob(combined_fit, train_df)
val_df$Score   <- .predict_prob(combined_fit, val_df)

.auc_table <- function(df, y, label) {
  rows <- lapply(c(safe_names, "Score"), function(v) {
    r <- pROC::roc(y, df[[v]], levels = c("Normal", "Disease"), direction = "<", quiet = TRUE)
    ci <- as.numeric(pROC::ci.auc(r))
    data.frame(Set = label, Feature = if (v == "Score") "Combined model" else names(safe_names)[safe_names == v],
               AUC = round(ci[2], 3), CI_low = round(ci[1], 3), CI_high = round(ci[3], 3))
  })
  do.call(rbind, rows)
}
auc_train <- .auc_table(train_df, y_train, paste0("Training (", wgcna_platform, ")"))
print(auc_train)

roc_cols <- grDevices::hcl.colors(length(safe_names) + 1L, "Dark 3")
.plot_rocs <- function(df, y, title) {
  for (i in seq_along(c(safe_names, "Score"))) {
    v <- c(safe_names, "Score")[i]
    r <- pROC::roc(y, df[[v]], levels = c("Normal", "Disease"), direction = "<", quiet = TRUE)
    pROC::plot.roc(r, add = i > 1, col = roc_cols[i], lwd = if (v == "Score") 3 else 1.5,
                   main = if (i == 1) title else NULL, legacy.axes = TRUE)
  }
  aucs <- vapply(c(safe_names, "Score"), function(v) as.numeric(pROC::auc(pROC::roc(
    y, df[[v]], levels = c("Normal", "Disease"), direction = "<", quiet = TRUE))), numeric(1))
  legend("bottomright", cex = 0.7, col = roc_cols, lwd = 2,
         legend = sprintf("%s (AUC %.3f)", c(biomarkers, "Combined"), aucs))
}
.plot_rocs(train_df, y_train, paste("ROC - training,", wgcna_platform))

# Boxplots of each biomarker on the training platform
op <- par(mfrow = c(1, min(4, length(biomarkers))))
for (gn in head(biomarkers, 4)) {
  boxplot(X_train[, gn] ~ y_train, col = c("#56B4E9", "#E69F00"), main = gn, xlab = "", ylab = "z-score")
}
par(op)

# ==============================================================================
# STEP 15: Independent CROSS-PLATFORM validation
# The model trained on one platform is applied, unchanged, to the other
# platform (never seen during WGCNA or ML).
# ==============================================================================
auc_val <- .auc_table(val_df, y_val, paste0("Validation (", val_platform, ")"))
print(auc_val)
.plot_rocs(val_df, y_val, paste("ROC - independent validation,", val_platform))

auc_all <- rbind(auc_train, auc_val)
write.csv(auc_all, file.path(work_dir, "ROC_AUC_train_validation.csv"), row.names = FALSE)

# Confusion matrix at 0.5
pred_val <- factor(ifelse(val_df$Score > 0.5, "Disease", "Normal"), levels = c("Normal", "Disease"))
print(table(Predicted = pred_val, Actual = y_val))

# ==============================================================================
# STEP 16: Nomogram + calibration (+ decision curve) - rms
# Kept to at most ~1 predictor per 5 events of the smaller class.
# ==============================================================================
library(rms)
max_pred <- max(1L, floor(min(table(y_train)) / 5))
nomo_vars <- head(safe_names, max_pred)
if (length(nomo_vars) < length(safe_names)) {
  message("Nomogram uses the top ", length(nomo_vars), " biomarker(s) (sample size limit): ",
          paste(names(nomo_vars), collapse = ", "))
}

nomo_df <- train_df[, c("Condition", nomo_vars), drop = FALSE]
dd <- rms::datadist(nomo_df)
options(datadist = "dd")
nomo_formula <- stats::as.formula(paste("Condition ~", paste(nomo_vars, collapse = " + ")))
lrm_fit <- tryCatch(rms::lrm(nomo_formula, data = nomo_df, x = TRUE, y = TRUE),
                    error = function(e) { message("lrm failed: ", conditionMessage(e)); NULL })

if (!is.null(lrm_fit)) {
  print(lrm_fit)
  nom <- rms::nomogram(lrm_fit, fun = stats::plogis, funlabel = "Risk of disease",
                       fun.at = c(0.1, 0.3, 0.5, 0.7, 0.9))
  plot(nom, xfrac = 0.3, cex.axis = 0.8)
  title("Biomarker nomogram")

  cal <- rms::calibrate(lrm_fit, method = "boot", B = 200)
  plot(cal, xlab = "Predicted probability", ylab = "Observed probability",
       main = "Calibration (bootstrap, B = 200)")

  if (requireNamespace("dcurves", quietly = TRUE)) {
    dca_df <- data.frame(Condition = nomo_df$Condition, Nomogram = stats::predict(lrm_fit, type = "fitted"))
    print(plot(dcurves::dca(Condition ~ Nomogram, data = dca_df, thresholds = seq(0, 0.9, 0.01))))
  }
}

# ==============================================================================
# STEP 17: Save the summary + session info
# ==============================================================================
summary_lines <- c(
  paste("Microarray GSEs:", paste(names(micro_expr_list), collapse = ", "), "-", length(micro_ids), "samples"),
  paste("RNA-seq GSEs:   ", paste(names(rna_counts_list), collapse = ", "), "-", length(rna_ids), "samples"),
  paste("DE: microarray limma |", "RNA-seq", de_method, "| |logFC| >", logfc_cutoff, "| padj <", padj_cutoff),
  paste("DEGs: RNA-seq", consensus$n_rna, "| microarray", consensus$n_micro, "| consensus", consensus$n_consensus),
  paste("WGCNA on", wgcna_platform, "| power", soft_power, "| trait modules:", paste(trait_sig_modules, collapse = ", ")),
  paste("Candidate genes (DEG ∩ module):", length(candidate_genes)),
  paste("ML methods:", paste(names(ml_selected), collapse = ", ")),
  paste("FINAL BIOMARKERS:", paste(biomarkers, collapse = ", ")),
  paste("Combined-model AUC - training:", auc_train$AUC[auc_train$Feature == "Combined model"],
        "| validation (", val_platform, "):", auc_val$AUC[auc_val$Feature == "Combined model"])
)
cat(summary_lines, sep = "\n")
writeLines(summary_lines, file.path(work_dir, "SUMMARY.txt"))
writeLines(capture.output(sessionInfo()), file.path(work_dir, "sessionInfo.txt"))
cat("\nAll outputs written to:", work_dir, "\n")
