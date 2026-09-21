#!/usr/bin/env Rscript
## Manual validation helper: GSE50760 (training) + GSE104836 (external validation)
## Compare DE method variants against GExPipe Shiny exports for accuracy checks.
##
## Usage (from repo root):
##   Rscript inst/scripts/validate-rnaseq-gse50760-gse104836.R
##   Rscript inst/scripts/validate-rnaseq-gse50760-gse104836.R --gse GSE50760 --outdir validation_out
##
## Optional env:
##   GEXPIPE_VALIDATE_GSE_TRAIN=GSE50760
##   GEXPIPE_VALIDATE_GSE_VAL=GSE104836

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) args[i + 1L] else default
}

train_gse <- get_arg("--train", Sys.getenv("GEXPIPE_VALIDATE_GSE_TRAIN", "GSE50760"))
val_gse <- get_arg("--val", Sys.getenv("GEXPIPE_VALIDATE_GSE_VAL", "GSE104836"))
outdir <- get_arg("--outdir", file.path(getwd(), "validation_manual"))
repo_root <- get_arg("--repo", getwd())
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(file.path(repo_root, "DESCRIPTION"))) {
  stop("Repo root not found (no DESCRIPTION): ", repo_root)
}

suppressPackageStartupMessages({
  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists(file.path(repo_root, "DESCRIPTION"))) {
    pkgload::load_all(repo_root, quiet = TRUE)
    message("Loaded GExPipe from: ", repo_root)
  } else {
    library(GExPipe)
    message("Loaded installed GExPipe")
  }
})

de_methods <- c("deseq2", "edger", "limma_voom", "limma")
logfc_cutoff <- 0.5
padj_cutoff <- 0.05

`%||%` <- function(a, b) if (!is.null(a)) a else b

summarize_pheno <- function(meta, gse_id) {
  cols <- intersect(
    c("title", "source_name_ch1", "characteristics_ch1", "characteristics_ch1.1",
      "characteristics_ch1.2", "tissue", "disease state:ch1"),
    colnames(meta)
  )
  cat("\n--- ", gse_id, " metadata preview (", nrow(meta), " samples) ---\n", sep = "")
  for (col in head(cols, 6L)) {
    tab <- table(meta[[col]], useNA = "ifany")
    cat("  ", col, ": ", paste(names(tab), tab, sep = "=", collapse = ", "), "\n", sep = "")
  }
  invisible(meta)
}

assign_crc_binary <- function(meta) {
  txt <- apply(meta, 1L, function(row) paste(as.character(row), collapse = " "))
  txt <- tolower(txt)
  cond <- rep(NA_character_, length(txt))
  cond[grepl("nontumor|non-tumor|normal colon|normal-looking|healthy|_n$|\\bn\\b", txt)] <- "Normal"
  cond[grepl("primary|metastas|metastatic|\\bc\\b|_c$|colon cancer|cancer tissue|tumor tissue|tumour", txt)] <- "Disease"
  meta$Condition <- cond
  meta
}

download_gse <- function(gse_id, base_dir) {
  rna_dir <- file.path(base_dir, "rna_data")
  dir.create(rna_dir, showWarnings = FALSE, recursive = TRUE)
  cat("\nDownloading ", gse_id, " ...\n", sep = "")
  out <- gexp_download_one_rnaseq_gse(gse_id, rna_dir)
  if (!isTRUE(out$ok)) {
    stop(gse_id, " download failed: ", out$reason %||% "unknown")
  }
  cat("  OK: ", nrow(out$count_matrix), " genes x ", ncol(out$count_matrix), " samples\n", sep = "")
  cat("  Log: ", out$log, "\n", sep = "")
  out
}

run_de_variant <- function(counts, meta, method, logfc = logfc_cutoff, padj = padj_cutoff) {
  meta <- meta[intersect(colnames(counts), rownames(meta)), , drop = FALSE]
  counts <- counts[, rownames(meta), drop = FALSE]
  meta <- meta[!is.na(meta$Condition) & meta$Condition %in% c("Normal", "Disease"), , drop = FALSE]
  counts <- counts[, rownames(meta), drop = FALSE]
  if (length(unique(meta$Condition)) < 2L) {
    stop("Need both Normal and Disease samples after group assignment.")
  }
  norm <- gexp_normalize_and_intersect(
    micro_expr_list = list(),
    rna_counts_list = list(train = counts),
    de_method = method
  )
  meta$Condition <- factor(meta$Condition, levels = c("Normal", "Disease"))
  norm$unified_metadata$Condition <- meta$Condition[match(norm$unified_metadata$SampleID, rownames(meta))]

  if (method %in% c("deseq2", "edger", "limma_voom")) {
    expr <- norm$raw_counts_for_deseq2
    meta_de <- norm$raw_counts_metadata
    meta_de$Condition <- norm$unified_metadata$Condition[match(rownames(meta_de), norm$unified_metadata$SampleID)]
  } else {
    expr <- norm$combined_expr
    meta_de <- norm$unified_metadata
  }

  if (method == "limma") {
    res <- gexp_run_de(expr, meta_de, method = "limma", logfc_cutoff = logfc, padj_cutoff = padj)
    de <- res$de_results
  } else if (method == "deseq2") {
    if (!requireNamespace("DESeq2", quietly = TRUE)) stop("DESeq2 not installed")
    ds_design <- gexpipe_deseq2_design(meta_de)
    design_mm <- stats::model.matrix(ds_design$formula, data = meta_de)
    filt <- gexpipe_independent_filter(expr, design = design_mm)
    count_mat <- filt$expr
    dds <- DESeq2::DESeqDataSetFromMatrix(
      countData = count_mat, colData = meta_de, design = ds_design$formula
    )
    dds <- DESeq2::DESeq(dds, quiet = TRUE)
    coef_name <- DESeq2::resultsNames(dds)[grep("Disease", DESeq2::resultsNames(dds))[1]]
    tt <- DESeq2::results(dds, name = coef_name)
    de <- data.frame(
      Gene = rownames(tt),
      logFC = tt$log2FoldChange,
      adj.P.Val = tt$padj,
      P.Value = tt$pvalue,
      stringsAsFactors = FALSE
    )
    de$Significance <- "Not Significant"
    de$Significance[de$adj.P.Val < padj & de$logFC > logfc] <- "Up-regulated"
    de$Significance[de$adj.P.Val < padj & de$logFC < -logfc] <- "Down-regulated"
  } else if (method == "edger") {
    if (!requireNamespace("edgeR", quietly = TRUE)) stop("edgeR not installed")
    de_design <- gexpipe_build_de_design(meta_de)
    filt <- gexpipe_independent_filter(expr, design = de_design$design)
    count_mat <- filt$expr
    y <- edgeR::DGEList(counts = count_mat)
    y <- edgeR::calcNormFactors(y)
    y <- edgeR::estimateDisp(y, de_design$design)
    fit <- edgeR::glmQLFit(y, de_design$design)
    qlf <- edgeR::glmQLFTest(fit, coef = de_design$coef_condition)
    tt <- edgeR::topTags(qlf, n = Inf)$table
    de <- data.frame(
      Gene = rownames(tt),
      logFC = tt$logFC,
      adj.P.Val = tt$FDR,
      P.Value = tt$PValue,
      stringsAsFactors = FALSE
    )
    de$Significance <- "Not Significant"
    de$Significance[de$adj.P.Val < padj & de$logFC > logfc] <- "Up-regulated"
    de$Significance[de$adj.P.Val < padj & de$logFC < -logfc] <- "Down-regulated"
  } else if (method == "limma_voom") {
    de_design <- gexpipe_build_de_design(meta_de)
    filt <- gexpipe_independent_filter(expr, design = de_design$design)
    count_mat <- filt$expr
    y <- edgeR::DGEList(counts = count_mat)
    y <- edgeR::calcNormFactors(y)
    v <- limma::voom(y, de_design$design, plot = FALSE)
    fit <- limma::lmFit(v, de_design$design)
    fit <- limma::eBayes(fit)
    tt <- limma::topTable(fit, coef = de_design$coef_condition, number = Inf, sort.by = "P")
    de <- data.frame(
      Gene = rownames(tt),
      logFC = tt$logFC,
      adj.P.Val = tt$adj.P.Val,
      P.Value = tt$P.Value,
      stringsAsFactors = FALSE
    )
    de$Significance <- "Not Significant"
    de$Significance[de$adj.P.Val < padj & de$logFC > logfc] <- "Up-regulated"
    de$Significance[de$adj.P.Val < padj & de$logFC < -logfc] <- "Down-regulated"
  }

  sig <- de[de$Significance != "Not Significant", , drop = FALSE]
  list(de = de, sig = sig, n_sig = nrow(sig))
}

compare_de_lists <- function(manual_sig, app_csv_path) {
  if (!file.exists(app_csv_path)) {
    return(list(found = FALSE, message = paste("App export not found:", app_csv_path)))
  }
  app <- utils::read.csv(app_csv_path, stringsAsFactors = FALSE)
  gene_col <- intersect(c("Gene", "gene", "SYMBOL"), colnames(app))[1]
  if (is.na(gene_col)) return(list(found = TRUE, error = "No Gene column in app CSV"))
  app_genes <- unique(app[[gene_col]])
  man_genes <- unique(manual_sig$Gene)
  both <- intersect(man_genes, app_genes)
  union_genes <- union(man_genes, app_genes)
  list(
    found = TRUE,
    manual_n = length(man_genes),
    app_n = length(app_genes),
    overlap = length(both),
    jaccard = if (length(union_genes)) length(both) / length(union_genes) else NA_real_,
    precision = if (length(man_genes)) length(both) / length(man_genes) else NA_real_,
    recall = if (length(app_genes)) length(both) / length(app_genes) else NA_real_
  )
}

cat("GExPipe manual validation\n")
cat("  Training GSE:   ", train_gse, " (GPL11154 HiSeq 2000)\n", sep = "")
cat("  Validation GSE: ", val_gse, " (GPL21290 HiSeq 3000)\n", sep = "")
cat("  Output dir:     ", outdir, "\n", sep = "")
cat("  DE thresholds:  |log2FC| >= ", logfc_cutoff, ", adj.P <= ", padj_cutoff, "\n", sep = "")

work <- file.path(outdir, "work")
dir.create(work, showWarnings = FALSE, recursive = TRUE)

train <- download_gse(train_gse, work)
val <- download_gse(val_gse, work)

summarize_pheno(train$metadata, train_gse)
summarize_pheno(val$metadata, val_gse)

train_meta <- assign_crc_binary(train$metadata)
val_meta <- assign_crc_binary(val$metadata)

cat("\nTraining group counts:\n")
print(table(train_meta$Condition, useNA = "ifany"))
cat("\nValidation group counts:\n")
print(table(val_meta$Condition, useNA = "ifany"))

datasets <- list(
  list(id = train_gse, counts = train$count_matrix, meta = train_meta),
  list(id = val_gse, counts = val$count_matrix, meta = val_meta)
)

summary_rows <- list()
for (ds in datasets) {
  gse_id <- ds$id
  counts <- ds$counts
  meta <- ds$meta
  for (method in de_methods) {
    cat("\nRunning DE:", gse_id, "-", method, "...\n")
    ok <- tryCatch({
      res <- run_de_variant(counts, meta, method)
      out_csv <- file.path(outdir, paste0(gse_id, "_", method, "_DE_all.csv"))
      sig_csv <- file.path(outdir, paste0(gse_id, "_", method, "_DE_sig.csv"))
      utils::write.csv(res$de, out_csv, row.names = FALSE)
      utils::write.csv(res$sig, sig_csv, row.names = FALSE)
      app_guess <- file.path(outdir, paste0("app_", gse_id, "_", method, "_DE_sig.csv"))
      cmp <- compare_de_lists(res$sig, app_guess)
      summary_rows[[paste(gse_id, method, sep = "_")]] <- data.frame(
        GSE = gse_id,
        Method = method,
        DEGs = res$n_sig,
        App_csv = app_guess,
        stringsAsFactors = FALSE
      )
      if (isTRUE(cmp$found) && is.null(cmp$error)) {
        summary_rows[[paste(gse_id, method, sep = "_")]]$Overlap <- cmp$overlap
        summary_rows[[paste(gse_id, method, sep = "_")]]$Jaccard <- round(cmp$jaccard, 4)
        summary_rows[[paste(gse_id, method, sep = "_")]]$Precision_vs_app <- round(cmp$precision, 4)
        summary_rows[[paste(gse_id, method, sep = "_")]]$Recall_vs_app <- round(cmp$recall, 4)
        cat("  DEGs:", res$n_sig,
            " | vs app overlap:", cmp$overlap,
            " (Jaccard=", round(cmp$jaccard, 3), ")\n", sep = "")
      } else {
        cat("  DEGs:", res$n_sig, " (export app CSV to ", app_guess, " to compare)\n", sep = "")
      }
      TRUE
    }, error = function(e) {
      cat("  FAILED:", conditionMessage(e), "\n")
      summary_rows[[paste(gse_id, method, sep = "_")]] <<- data.frame(
        GSE = gse_id, Method = method, DEGs = NA_integer_, Error = conditionMessage(e),
        stringsAsFactors = FALSE
      )
      FALSE
    })
  }
}

summary_df <- do.call(rbind, summary_rows)
summary_path <- file.path(outdir, "validation_summary.csv")
utils::write.csv(summary_df, summary_path, row.names = FALSE)

cat("\n=== Recommended GExPipe Shiny settings ===\n")
cat("Step 1: Platform = RNA-seq; DE method = deseq2 (primary) or edger/limma_voom\n")
cat("Step 4: GSE50760 -> compare Primary CRC vs Normal (exclude metastasis OR label as Disease)\n")
cat("Step 4: GSE104836 -> Tumor vs Nontumor (paired)\n")
cat("Step 11: External validation = GSE104836 when training on GSE50760\n")
cat("\nTo compute accuracy vs app:\n")
cat("  1. Run the Shiny app through Step 6 for each GSE + DE method\n")
cat("  2. Download DE results CSV from the app\n")
cat("  3. Save as: ", outdir, "/app_<GSE>_<method>_DE_sig.csv\n", sep = "")
cat("  4. Re-run this script\n")
cat("\nSummary written to: ", summary_path, "\n", sep = "")
