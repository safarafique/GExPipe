# ==============================================================================
# GExPipe MANUAL PIPELINE - MICROARRAY, 2+ DATASETS (plain R, NOT Shiny)
#
# Same as manual_pipeline_microarray.R but for combining any number of
# microarray GSEs (2, 3, 4, 5, ...) into one analysis - download, normalize
# + intersect genes, QC, assign groups per dataset, real batch correction
# across studies, DE, WGCNA, common genes + GO/KEGG, PPI.
#
# Completely separate from the Shiny app code - does not source or depend
# on inst/shinyapp/ or R/server_*.R / R/ui_*.R. Calls the same underlying
# functions the app uses internally, so results match, but you run and
# inspect every step yourself, one at a time (Ctrl+Enter per block).
# ==============================================================================

library(GExPipe)

# ==============================================================================
# STEP 1: USER INPUT - list as many microarray GSEs as you need (2, 3, 4, ...)
# Each dataset almost always uses different phenodata column names/values,
# so group_map has one entry PER GSE - edit col/normal/disease for each.
# ==============================================================================
gse_ids <- c("GSE1", "GSE2")   # <-- add/remove GSE IDs freely, e.g. c("GSE1","GSE2","GSE3")

 group_map <- list(
  GSE1 = list(col = "disease state:ch1", normal = c("control"), disease = c("disease")),
  GSE2 = list(col = "diagnosis:ch1",     normal = c("healthy"), disease = c("tumor"))
  # GSE3 = list(col = "group:ch1",       normal = c("normal"),  disease = c("case")),
)
# ^ group_map must have exactly one entry per ID in gse_ids, named identically.

work_dir <- file.path(getwd(), "manual_pipeline_data", paste(gse_ids, collapse = "_"))
dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)

## ============================================================================
## STEP 2: Download all datasets
## ============================================================================
micro_expr_list <- list()
pdata_list      <- list()
for (gse in gse_ids) {
  dl <- gexp_download_one_microarray_gse(gse, work_dir)
  if (!isTRUE(dl$ok)) {
    warning(gse, " failed: ", dl$reason)
    next
  }
  micro_expr_list[[gse]] <- dl$micro_expr
  pdata_list[[gse]]      <- dl$metadata
  cat(gse, ":", nrow(dl$micro_expr), "genes x", ncol(dl$micro_expr), "samples downloaded\n")
}
if (length(micro_expr_list) < 2L) {
  stop("Need at least 2 successfully downloaded datasets - check the warnings above.")
}
# View(pdata_list[["GSE1"]])   # uncomment (with the real GSE id) to browse a dataset's phenodata

## ============================================================================
## STEP 3: Normalize each dataset, then intersect genes across all of them
## Same function/defaults as the app's Step 2 for a multi-GSE microarray run.
## ============================================================================
norm_out     <- gexp_normalize_and_intersect(
  micro_expr_list       = micro_expr_list,
  rna_counts_list       = list(),
  micro_norm_method     = "quantile",
  apply_global_quantile = TRUE
)
expr_norm    <- norm_out$combined_expr     # normalized genes x samples (all datasets, common genes)
unified_meta <- norm_out$unified_metadata  # SampleID, Platform, Dataset, Condition (NA for now)
cat(norm_out$log_text)
table(unified_meta$Dataset)   # samples contributed by each GSE

## ============================================================================
## STEP 4: QC - PCA colored by Dataset (look for between-study separation
## before correction) and hierarchical clustering.
## ============================================================================
pca <- prcomp(t(expr_norm), scale. = TRUE)
plot(pca$x[, 1], pca$x[, 2], col = as.numeric(factor(unified_meta$Dataset)), pch = 19,
     main = "PCA by Dataset (before batch correction)", xlab = "PC1", ylab = "PC2")
legend("topright", legend = levels(factor(unified_meta$Dataset)),
       col = seq_along(levels(factor(unified_meta$Dataset))), pch = 19, cex = 0.8)

hc <- hclust(as.dist(1 - cor(expr_norm, use = "pairwise.complete.obs")), method = "average")
plot(hc, main = "Sample clustering (before batch correction)")

# If a sample looks like an outlier, remove it manually before continuing, e.g.:
# outliers <- c("GSMxxxxxxx")
# expr_norm    <- expr_norm[, !colnames(expr_norm) %in% outliers, drop = FALSE]
# unified_meta <- unified_meta[!rownames(unified_meta) %in% outliers, , drop = FALSE]

## ============================================================================
## STEP 5: Assign groups (Normal / Disease) PER DATASET using group_map
## ============================================================================
condition <- rep(NA_character_, nrow(unified_meta))
names(condition) <- rownames(unified_meta)

for (gse in gse_ids) {
  if (is.null(group_map[[gse]])) {
    warning("No group_map entry for ", gse, " - its samples will be excluded.")
    next
  }
  gm <- group_map[[gse]]
  gse_samples <- rownames(unified_meta)[unified_meta$Dataset == gse]
  gse_samples <- intersect(gse_samples, rownames(pdata_list[[gse]]))
  raw_vals <- as.character(pdata_list[[gse]][gse_samples, gm$col])
  condition[gse_samples] <- ifelse(
    raw_vals %in% gm$normal, "Normal",
    ifelse(raw_vals %in% gm$disease, "Disease", NA_character_)
  )
}

unified_meta$Condition <- factor(condition[rownames(unified_meta)], levels = c("Normal", "Disease"))

keep         <- !is.na(unified_meta$Condition)
expr_norm    <- expr_norm[, keep, drop = FALSE]
meta         <- unified_meta[keep, , drop = FALSE]

# Check each dataset has both groups (avoids Dataset x Condition confounding)
table(meta$Dataset, meta$Condition)

## ============================================================================
## STEP 6: Batch correction across datasets
## Auto-picks the same way as the app's Step 5 recommendation: ComBat-ref
## normally, or limma if Dataset and Condition look confounded (e.g. one
## GSE is all Disease and another all Normal).
## ============================================================================
conf <- gexpipe_batch_confounding_summary(meta)
cat(conf$message, "\n")
batch_method <- if (isTRUE(conf$confounded)) "limma" else "combat_ref"
cat("Using batch method:", batch_method, "\n")

batch_out  <- gexp_batch_correct(
  expr                = expr_norm,
  metadata            = meta,
  variance_percentile = 25,       # same default as the app's Step 5 slider
  method              = batch_method
)
expr_batch <- batch_out$batch_corrected
cat(batch_out$log_text)

# PCA after correction - datasets should now intermingle
pca_after <- prcomp(t(expr_batch), scale. = TRUE)
plot(pca_after$x[, 1], pca_after$x[, 2], col = as.numeric(factor(meta$Dataset)), pch = 19,
     main = "PCA by Dataset (after batch correction)", xlab = "PC1", ylab = "PC2")
legend("topright", legend = levels(factor(meta$Dataset)),
       col = seq_along(levels(factor(meta$Dataset))), pch = 19, cex = 0.8)

## ============================================================================
## STEP 7: Differential expression (limma)
## With 2+ datasets, gexpipe_run_limma_on_subset() automatically adds
## Dataset as a covariate in the design (same as the app).
## ============================================================================
de_out <- gexpipe_run_limma_on_subset(
  expr_batch, meta,
  logfc_cutoff = 0.5,
  padj_cutoff  = 0.05,
  ref_lab = "Normal", alt_lab = "Disease"
)

de_results <- de_out$de_results
sig_genes  <- de_out$sig_genes

cat("Genes tested:      ", nrow(de_results), "\n")
cat("Significant DEGs:  ", nrow(sig_genes), "\n")
head(sig_genes[order(sig_genes$adj.P.Val), ])

write.csv(de_results, file.path(work_dir, "DE_results.csv"), row.names = FALSE)

## ============================================================================
## STEP 8: WGCNA (weighted gene co-expression network)
## ============================================================================
library(WGCNA)
WGCNA::enableWGCNAThreads()

prep <- gexp_wgcna_prepare(
  expr_batch, meta,
  gene_mode = "top_variable",
  top_genes = 5000L
)
datExpr <- prep$datExpr          # samples x genes (WGCNA convention)
sample_info_wgcna <- prep$sample_info

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
if (is.na(soft_power)) soft_power <- 6
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

condition_num <- as.numeric(sample_info_wgcna$Condition == "Disease")
module_trait_cor <- stats::cor(MEs, condition_num, use = "p")
module_trait_p    <- WGCNA::corPvalueStudent(module_trait_cor, nrow(datExpr))
module_trait_table <- data.frame(
  Module = sub("^ME", "", rownames(module_trait_cor)),
  Correlation = round(module_trait_cor[, 1], 4),
  P_value = signif(module_trait_p[, 1], 4)
)
print(module_trait_table[order(module_trait_table$P_value), ])

## ============================================================================
## STEP 9: Common genes (DEG intersect WGCNA) + GO/KEGG enrichment
## ============================================================================
sig_gene_names <- sig_genes$Gene

trait_sig_modules <- module_trait_table$Module[
  abs(module_trait_table$Correlation) > 0.3 & module_trait_table$P_value < 0.05
]
wgcna_module_genes <- names(module_colors)[module_colors %in% trait_sig_modules]

common_genes <- intersect(sig_gene_names, wgcna_module_genes)
cat("DEGs:", length(sig_gene_names),
    "| WGCNA trait-associated module genes:", length(wgcna_module_genes),
    "| Common genes:", length(common_genes), "\n")

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

write.csv(as.data.frame(go_result), file.path(work_dir, "GO_BP.csv"), row.names = FALSE)
write.csv(as.data.frame(kegg_result), file.path(work_dir, "KEGG.csv"), row.names = FALSE)

## ============================================================================
## STEP 10: PPI network (STRINGdb) + hub genes
## ============================================================================
valid_genes <- common_genes[common_genes %in% AnnotationDbi::keys(org.Hs.eg.db, keytype = "SYMBOL")]

ppi_data <- GExPipe:::gexp_stringdb_get_ppi_data_safe(  # not exported; accessed via :::
  score_threshold = 400,
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
     main = "PPI network")

write.csv(hub_scores, file.path(work_dir, "PPI_hub_scores.csv"), row.names = FALSE)
