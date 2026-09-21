# ==============================================================================
# GExPipe MANUAL PIPELINE - RNA-SEQ, single dataset (plain R, NOT Shiny)
#
# Completely separate from the Shiny app code (inst/shinyapp/, R/server_*.R,
# R/ui_*.R) - does not source or depend on any of it. It calls the same
# underlying analysis functions the app uses internally, so results match,
# but you run and inspect every step yourself, one at a time.
#
# Open in RStudio and run ONE STEP AT A TIME (select the block, Ctrl+Enter),
# checking the printed output / plots before moving to the next step.
#
# DE method: choose ONE of 3 RNA-seq engines by setting de_method_choice in
# STEP 1 to 1, 2, or 3:
#   1 = DESeq2      (negative binomial GLM on raw counts)
#   2 = edgeR       (quasi-likelihood F-test, TMM normalization)
#   3 = limma-voom  (voom transform + limma empirical Bayes)
# For microarray data, see manual_pipeline_microarray.R instead.
# ==============================================================================

library(GExPipe)

# ==============================================================================
# STEP 1: USER INPUT
# ==============================================================================
gse_id <- "GSE50760"   # RNA-seq GEO series accession

de_method_choice <- 1   # <-- CHOOSE: 1 = DESeq2, 2 = edgeR, 3 = limma-voom
de_method <- switch(
  as.character(de_method_choice),
  "1" = "deseq2",
  "2" = "edger",
  "3" = "limma_voom",
  stop("de_method_choice must be 1, 2, or 3")
)
cat("DE method selected:", de_method, "(choice", de_method_choice, ")\n")

work_dir <- file.path(getwd(), "manual_pipeline_data", gse_id)
dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# STEP 2: Download
# ==============================================================================
dl <- gexp_download_one_rnaseq_gse(gse_id, work_dir)
if (!isTRUE(dl$ok)) stop("Download failed: ", dl$reason)
expr_raw <- dl$count_matrix   # raw integer counts
pdata    <- dl$metadata

cat(gse_id, ":", nrow(expr_raw), "genes x", ncol(expr_raw), "samples downloaded\n")
cat("Phenodata columns:", paste(colnames(pdata), collapse = ", "), "\n")
# View(pdata)   # uncomment to browse phenodata in RStudio's data viewer

# ==============================================================================
# STEP 3: Normalize
# TMM -> log-CPM normalization for the continuous matrix (used by WGCNA/QC
# plots), while the RAW counts are kept separately for count-based DE - same
# split the app uses (same function/defaults as the app's Step 2).
# ==============================================================================
rna_counts_list <- setNames(list(expr_raw), gse_id)

norm_out  <- gexp_normalize_and_intersect(
  micro_expr_list       = list(),
  rna_counts_list       = rna_counts_list,
  rnaseq_norm_method    = "TMM",
  apply_global_quantile = TRUE
)
expr_norm         <- norm_out$combined_expr        # normalized (log-CPM) genes x samples
counts_for_deseq2 <- norm_out$raw_counts_for_deseq2 # raw integer counts, aligned to expr_norm's samples
cat(norm_out$log_text)

# ==============================================================================
# STEP 4: QC - inspect for outlier samples (PCA + hierarchical clustering)
# Uses the normalized log-CPM matrix, not raw counts.
# ==============================================================================
pca <- prcomp(t(expr_norm), scale. = TRUE)
plot(pca$x[, 1], pca$x[, 2], main = paste(gse_id, "- PCA (check for outliers)"),
     xlab = "PC1", ylab = "PC2", pch = 19)
text(pca$x[, 1], pca$x[, 2], labels = colnames(expr_norm), pos = 3, cex = 0.6)

hc <- hclust(as.dist(1 - cor(expr_norm, use = "pairwise.complete.obs")), method = "average")
plot(hc, main = paste(gse_id, "- sample clustering"))

# If a sample looks like an outlier, remove it manually before continuing, e.g.:
# outliers <- c("GSMxxxxxxx")
# expr_norm         <- expr_norm[, !colnames(expr_norm) %in% outliers, drop = FALSE]
# counts_for_deseq2 <- counts_for_deseq2[, !colnames(counts_for_deseq2) %in% outliers, drop = FALSE]
# pdata             <- pdata[!rownames(pdata) %in% outliers, , drop = FALSE]

# ==============================================================================
# STEP 5: Assign groups (Normal / Disease) - EDIT for your dataset
# Look at the phenodata columns printed in Step 2 (or View(pdata)) to find
# the column that holds group/condition info, then set the three lines below.
# ==============================================================================
colnames(pdata)                       # <- run this to see available columns
# table(pdata$`your_column_name`)     # <- uncomment, replace with a real column, inspect its values

group_col      <- "disease state:ch1" # <-- CHANGE to your actual column name
normal_values  <- c("control")        # <-- CHANGE: values in that column meaning "Normal"
disease_values <- c("disease")        # <-- CHANGE: values in that column meaning "Disease"

group_raw <- as.character(pdata[colnames(expr_norm), group_col])
condition <- ifelse(
  group_raw %in% normal_values, "Normal",
  ifelse(group_raw %in% disease_values, "Disease", NA_character_)
)

meta <- data.frame(
  SampleID  = colnames(expr_norm),
  Dataset   = gse_id,
  Platform  = "RNAseq",
  Condition = factor(condition, levels = c("Normal", "Disease")),
  row.names = colnames(expr_norm),
  stringsAsFactors = FALSE
)

keep              <- !is.na(meta$Condition)
expr_norm         <- expr_norm[, keep, drop = FALSE]
meta              <- meta[keep, , drop = FALSE]
common_ids        <- intersect(colnames(counts_for_deseq2), rownames(meta))
counts_for_deseq2 <- counts_for_deseq2[, common_ids, drop = FALSE]
table(meta$Condition)   # confirm you have at least 2 samples in each group

# ==============================================================================
# STEP 6: Batch correction
# Applied to the normalized log-CPM matrix (used later for WGCNA/QC), NOT to
# the raw counts - DESeq2/edgeR/limma-voom keep raw counts and add Dataset
# as a covariate at the DE step instead, same as the app. With a single
# dataset there is no between-study batch to remove, so this just applies
# the variance-percentile gene filter (same default as the app's Step 5).
# ==============================================================================
batch_out <- gexp_batch_correct(
  expr                = expr_norm,
  metadata            = meta,
  variance_percentile = 25,          # same default as the app's Step 5 slider
  method              = "limma"      # limma removeBatchEffect: safe default alongside count-based DE
)
expr_batch <- batch_out$batch_corrected
cat(batch_out$log_text)

# ==============================================================================
# STEP 7: Differential expression - uses de_method chosen in STEP 1
# Always runs on the RAW counts (counts_for_deseq2), not the batch-corrected
# matrix, exactly like the app: DESeq2/edgeR/limma-voom model counts
# directly and add Dataset as a covariate automatically when there are 2+
# datasets in rna_counts_list.
# ==============================================================================
de_out <- GExPipe:::gexpipe_run_count_de(   # not exported; accessed via :::
  counts_for_deseq2, meta,
  method       = de_method,   # set via de_method_choice in STEP 1
  logfc_cutoff = 0.5,
  padj_cutoff  = 0.05,
  ref_lab = "Normal", alt_lab = "Disease"
)

de_results <- de_out$de_results
sig_genes  <- de_out$sig_genes

cat("DE method used:    ", de_method, "\n")
cat("Genes tested:      ", nrow(de_results), "\n")
cat("Significant DEGs:  ", nrow(sig_genes), "\n")
head(sig_genes[order(sig_genes$adj.P.Val), ])

write.csv(de_results, file.path(work_dir, paste0(gse_id, "_", de_method, "_DE_results.csv")), row.names = FALSE)

# ==============================================================================
# STEP 8: WGCNA (weighted gene co-expression network)
# RNA-seq raw counts are variance-stabilized first (VST) before WGCNA -
# never feed raw counts or CPM directly in, same as the app.
# ==============================================================================
library(WGCNA)
WGCNA::enableWGCNAThreads()

wgcna_input <- GExPipe:::gexpipe_counts_to_vst(counts_for_deseq2, sample_ids = rownames(meta)) # not exported

prep <- gexp_wgcna_prepare(
  wgcna_input, meta,
  gene_mode = "top_variable",
  top_genes = 5000L
)
datExpr <- prep$datExpr          # samples x genes (WGCNA convention)
sample_info_wgcna <- prep$sample_info

# Pick soft-thresholding power (look for the first power where the fit
# index curve flattens near/above ~0.85-0.9; adjust manually if needed)
powers <- c(1:10, seq(12, 20, 2))
sft <- WGCNA::pickSoftThreshold(datExpr, powerVector = powers, verbose = 2)
print(sft$fitIndices)
plot(sft$fitIndices[, 1], -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2],
     xlab = "Soft Threshold (power)", ylab = "Scale Free Topology Model Fit, signed R^2",
     main = "Scale independence", type = "n")
text(sft$fitIndices[, 1], -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2],
     labels = powers, col = "red")
abline(h = 0.85, col = "red")

soft_power <- sft$powerEstimate
if (is.na(soft_power)) soft_power <- 6   # WGCNA's usual fallback default
cat("Using soft power:", soft_power, "\n")

net <- WGCNA::blockwiseModules(
  datExpr,
  power              = soft_power,
  TOMType            = "signed",
  minModuleSize      = 30,
  reassignThreshold  = 0,
  mergeCutHeight     = 0.25,
  numericLabels      = TRUE,
  pamRespectsDendro  = FALSE,
  verbose            = 3
)

module_colors <- WGCNA::labels2colors(net$colors)
names(module_colors) <- colnames(datExpr)
MEs <- net$MEs

WGCNA::plotDendroAndColors(
  net$dendrograms[[1]], module_colors[net$blockGenes[[1]]],
  "Module colors", dendroLabels = FALSE, hang = 0.03,
  addGuide = TRUE, guideHang = 0.05
)

# Module-trait correlation against Disease (0/1)
condition_num <- as.numeric(sample_info_wgcna$Condition == "Disease")
module_trait_cor <- stats::cor(MEs, condition_num, use = "p")
module_trait_p    <- WGCNA::corPvalueStudent(module_trait_cor, nrow(datExpr))
module_trait_table <- data.frame(
  Module = sub("^ME", "", rownames(module_trait_cor)),
  Correlation = round(module_trait_cor[, 1], 4),
  P_value = signif(module_trait_p[, 1], 4)
)
print(module_trait_table[order(module_trait_table$P_value), ])

# ==============================================================================
# STEP 9: Common genes (DEG intersect WGCNA) + GO/KEGG enrichment
# ==============================================================================
sig_gene_names <- sig_genes$Gene

# Pick the module(s) most correlated with Disease (e.g. |r| > 0.3, p < 0.05)
trait_sig_modules <- module_trait_table$Module[
  abs(module_trait_table$Correlation) > 0.3 & module_trait_table$P_value < 0.05
]
wgcna_module_genes <- names(module_colors)[module_colors %in% trait_sig_modules]

common_genes <- intersect(sig_gene_names, wgcna_module_genes)
cat("DEGs:", length(sig_gene_names),
    "| WGCNA trait-associated module genes:", length(wgcna_module_genes),
    "| Common genes:", length(common_genes), "\n")

# GO / KEGG enrichment on the common gene set (standard clusterProfiler usage)
library(clusterProfiler)
library(org.Hs.eg.db)

entrez_ids <- AnnotationDbi::mapIds(
  org.Hs.eg.db, keys = common_genes, keytype = "SYMBOL", column = "ENTREZID"
)
entrez_ids <- entrez_ids[!is.na(entrez_ids)]

go_result <- clusterProfiler::enrichGO(
  gene = entrez_ids, OrgDb = org.Hs.eg.db, keyType = "ENTREZID",
  ont = "BP", pAdjustMethod = "BH", pvalueCutoff = 0.05, qvalueCutoff = 0.2
)
kegg_result <- clusterProfiler::enrichKEGG(
  gene = entrez_ids, organism = "hsa",
  pAdjustMethod = "BH", pvalueCutoff = 0.05
)

if (!is.null(go_result) && nrow(as.data.frame(go_result)) > 0) {
  print(clusterProfiler::dotplot(go_result, showCategory = 15, title = "GO: Biological Process"))
}
if (!is.null(kegg_result) && nrow(as.data.frame(kegg_result)) > 0) {
  print(clusterProfiler::dotplot(kegg_result, showCategory = 15, title = "KEGG pathways"))
}

write.csv(as.data.frame(go_result), file.path(work_dir, paste0(gse_id, "_GO_BP.csv")), row.names = FALSE)
write.csv(as.data.frame(kegg_result), file.path(work_dir, paste0(gse_id, "_KEGG.csv")), row.names = FALSE)

# ==============================================================================
# STEP 10: PPI network (STRINGdb) + hub genes
# Uses the same STRINGdb helper the app uses (tries STRING v12.0, then
# v11.5, then v11 if a newer version's server/data has problems).
# ==============================================================================
valid_genes <- common_genes[common_genes %in% AnnotationDbi::keys(org.Hs.eg.db, keytype = "SYMBOL")]

ppi_data <- GExPipe:::gexp_stringdb_get_ppi_data_safe(  # not exported; accessed via :::
  score_threshold = 400,   # STRING combined-score cutoff (0-1000); 400 = medium confidence
  valid_genes     = valid_genes
)
if (is.null(ppi_data$interactions)) {
  stop("STRINGdb failed for all versions tried: ", paste(ppi_data$try_errors, collapse = " | "))
}

mapped       <- ppi_data$mapped
interactions <- ppi_data$interactions
id_col       <- ppi_data$id_col
cat("STRING version used:", ppi_data$version_used,
    "|", ppi_data$pct_mapped, "% of genes mapped\n")

sym_col <- if ("SYMBOL" %in% colnames(mapped)) "SYMBOL" else colnames(mapped)[min(2, ncol(mapped))]
vertices_df <- unique(data.frame(
  STRING_id = mapped[[id_col]], SYMBOL = mapped[[sym_col]], stringsAsFactors = FALSE
))
vertices_df <- vertices_df[!is.na(vertices_df$STRING_id) & nzchar(vertices_df$STRING_id), ]

from_col <- if ("from" %in% colnames(interactions)) "from" else "protein1"
to_col   <- if ("to" %in% colnames(interactions)) "to" else "protein2"
edge_list <- interactions[
  interactions[[from_col]] %in% vertices_df$STRING_id &
    interactions[[to_col]] %in% vertices_df$STRING_id,
  c(from_col, to_col)
]
colnames(edge_list) <- c("from", "to")

library(igraph)
g <- igraph::simplify(igraph::graph_from_data_frame(edge_list, directed = FALSE, vertices = vertices_df))

hub_scores <- data.frame(
  Gene        = igraph::V(g)$SYMBOL,
  Degree      = igraph::degree(g),
  Betweenness = igraph::betweenness(g, normalized = TRUE),
  PageRank    = igraph::page_rank(g)$vector
)
hub_scores <- hub_scores[order(-hub_scores$Degree), ]
cat("PPI network:", igraph::vcount(g), "nodes,", igraph::ecount(g), "edges\n")
head(hub_scores, 10)

plot(g, vertex.label = igraph::V(g)$SYMBOL, vertex.size = 8, vertex.label.cex = 0.7,
     main = paste(gse_id, "- PPI network"))

write.csv(hub_scores, file.path(work_dir, paste0(gse_id, "_PPI_hub_scores.csv")), row.names = FALSE)
