#!/usr/bin/env Rscript
## Empirical competitor-style efficiency + DE comparison on one public RNA-seq cohort
##
## What this script does AUTOMATICALLY:
##   - Downloads GSE50760 (or uses cache)
##   - Times GExPipe DE engines: limma, limma-voom, DESeq2, edgeR
##   - Times equivalent "standard Bioconductor script" DE (same engines, minimal wrappers)
##   - Writes DEG counts, Jaccard overlaps, and a fillable competitor table
##
## What you fill MANUALLY (GUI tools cannot be fully scripted fairly):
##   - GEOexplorer, Shiny-Seq, iGEAK, GeneTrailExpress wall-clock + n DEGs
##   See: validation_manual/competitor_benchmark/MANUAL_TIMING_PROTOCOL.md
##
## Usage:
##   Rscript inst/scripts/benchmark-competitor-efficiency-de.R --repo e:/GExPipe
##   Rscript inst/scripts/benchmark-competitor-efficiency-de.R --repo e:/GExPipe --gse GSE50760

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) args[i + 1L] else default
}

repo <- get_arg("--repo", getwd())
gse_id <- get_arg("--gse", "GSE50760")
outdir <- get_arg("--outdir", file.path(repo, "validation_manual", "competitor_benchmark"))
logfc_cut <- as.numeric(get_arg("--logfc", "0.5"))
padj_cut <- as.numeric(get_arg("--padj", "0.05"))
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
work <- file.path(outdir, "work")
dir.create(work, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("Install pkgload: install.packages('pkgload')")
  }
  pkgload::load_all(repo, quiet = TRUE)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

assign_gse50760 <- function(meta) {
  txt <- tolower(as.character(meta$title))
  cond <- rep(NA_character_, length(txt))
  cond[grepl("normal colon", txt)] <- "Normal"
  cond[grepl("primary colorectal", txt)] <- "Disease"
  meta$Condition <- cond
  meta[!is.na(meta$Condition), , drop = FALSE]
}

assign_generic <- function(meta) {
  # Fallback for other GSEs: scan all columns for normal/tumor keywords
  txt <- apply(meta, 1L, function(r) paste(tolower(as.character(r)), collapse = " "))
  cond <- rep(NA_character_, length(txt))
  cond[grepl("normal|nontumor|non-tumor|healthy|control", txt)] <- "Normal"
  cond[grepl("tumor|tumour|cancer|disease|primary colorectal", txt)] <- "Disease"
  meta$Condition <- cond
  meta[!is.na(meta$Condition), , drop = FALSE]
}

jaccard <- function(a, b) {
  a <- unique(a); b <- unique(b)
  u <- union(a, b)
  if (!length(u)) return(NA_real_)
  length(intersect(a, b)) / length(u)
}

sig_genes <- function(de) {
  de$Gene[!is.na(de$adj.P.Val) & de$adj.P.Val < padj_cut & abs(de$logFC) >= logfc_cut]
}

time_call <- function(expr) {
  start <- proc.time()[["elapsed"]]
  mem0 <- sum(gc()[, 2], na.rm = TRUE)
  out <- force(expr)
  elapsed <- proc.time()[["elapsed"]] - start
  mem1 <- sum(gc()[, 2], na.rm = TRUE)
  list(result = out, seconds = as.numeric(elapsed), mem_mb_delta = as.numeric(mem1 - mem0))
}

# ---- Download ----
cat("=== Competitor efficiency / DE benchmark ===\n")
cat("GSE:", gse_id, "\n")
cat("Thresholds: |log2FC| >=", logfc_cut, ", adj.P <=", padj_cut, "\n\n")

hw <- data.frame(
  Item = c("Date", "R_version", "OS", "GSE", "logFC_cutoff", "padj_cutoff"),
  Value = c(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    paste(R.version$major, R.version$minor, sep = "."),
    paste(Sys.info()[["sysname"]], Sys.info()[["release"]]),
    gse_id, logfc_cut, padj_cut
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(hw, file.path(outdir, "hardware_session.csv"), row.names = FALSE)

cat("Downloading / loading", gse_id, "...\n")
t_dl <- time_call({
  gexp_download_one_rnaseq_gse(gse_id, file.path(work, "rna_data"))
})
dl <- t_dl$result
if (!isTRUE(dl$ok)) stop("Download failed: ", dl$reason %||% "unknown")

meta <- if (identical(gse_id, "GSE50760")) assign_gse50760(dl$metadata) else assign_generic(dl$metadata)
if (nrow(meta) < 6L || length(unique(meta$Condition)) < 2L) {
  stop("Could not assign Normal/Disease labels. Check phenotype for ", gse_id)
}
counts <- as.matrix(dl$count_matrix[, rownames(meta), drop = FALSE])
storage.mode(counts) <- "integer"
cat("Samples:", ncol(counts),
    " Groups:", paste(names(table(meta$Condition)), table(meta$Condition), sep = "=", collapse = ", "),
    " Genes:", nrow(counts), "\n")
cat(sprintf("Download elapsed: %.1f s\n\n", t_dl$seconds))

# ---- Standard Bioconductor script baselines (what many GUI tools wrap) ----
run_standard_limma_voom <- function(counts, meta) {
  meta$Condition <- factor(meta$Condition, levels = c("Normal", "Disease"))
  design <- stats::model.matrix(~ Condition, data = meta)
  y <- edgeR::DGEList(counts = counts)
  keep <- edgeR::filterByExpr(y, design = design)
  y <- y[keep, , keep.lib.sizes = FALSE]
  y <- edgeR::calcNormFactors(y)
  v <- limma::voom(y, design, plot = FALSE)
  fit <- limma::lmFit(v, design)
  fit <- limma::eBayes(fit)
  tt <- limma::topTable(fit, coef = "ConditionDisease", number = Inf, sort.by = "P")
  data.frame(
    Gene = rownames(tt), logFC = tt$logFC, AveExpr = tt$AveExpr,
    P.Value = tt$P.Value, adj.P.Val = tt$adj.P.Val, stringsAsFactors = FALSE
  )
}

run_standard_deseq2 <- function(counts, meta) {
  meta$Condition <- factor(meta$Condition, levels = c("Normal", "Disease"))
  dds <- DESeq2::DESeqDataSetFromMatrix(countData = counts, colData = meta, design = ~ Condition)
  keep <- rowSums(DESeq2::counts(dds)) >= 10
  dds <- dds[keep, ]
  dds <- DESeq2::DESeq(dds, quiet = TRUE)
  res <- DESeq2::results(dds, contrast = c("Condition", "Disease", "Normal"))
  data.frame(
    Gene = rownames(res),
    logFC = res$log2FoldChange,
    AveExpr = res$baseMean,
    P.Value = res$pvalue,
    adj.P.Val = res$padj,
    stringsAsFactors = FALSE
  )
}

run_standard_edger <- function(counts, meta) {
  meta$Condition <- factor(meta$Condition, levels = c("Normal", "Disease"))
  design <- stats::model.matrix(~ Condition, data = meta)
  y <- edgeR::DGEList(counts = counts)
  keep <- edgeR::filterByExpr(y, design = design)
  y <- y[keep, , keep.lib.sizes = FALSE]
  y <- edgeR::calcNormFactors(y)
  y <- edgeR::estimateDisp(y, design)
  fit <- edgeR::glmQLFit(y, design)
  qlf <- edgeR::glmQLFTest(fit, coef = 2)
  tt <- edgeR::topTags(qlf, n = Inf)$table
  data.frame(
    Gene = rownames(tt), logFC = tt$logFC, AveExpr = tt$logCPM,
    P.Value = tt$PValue, adj.P.Val = tt$FDR, stringsAsFactors = FALSE
  )
}

run_gexpipe_method <- function(method) {
  # Reuse scoring helper pattern
  norm <- gexp_normalize_and_intersect(
    micro_expr_list = list(),
    rna_counts_list = list(dataset = counts),
    de_method = method
  )
  if (method %in% c("deseq2", "edger", "limma_voom")) {
    expr <- norm$raw_counts_for_deseq2
    meta_de <- norm$raw_counts_metadata
  } else {
    expr <- norm$combined_expr
    meta_de <- norm$unified_metadata
  }
  meta_de$Condition <- meta$Condition[match(rownames(meta_de), rownames(meta))]
  meta_de <- meta_de[!is.na(meta_de$Condition), , drop = FALSE]
  expr <- expr[, rownames(meta_de), drop = FALSE]
  meta_de$Condition <- factor(meta_de$Condition, levels = c("Normal", "Disease"))

  if (method == "limma") {
    return(gexp_run_de(expr, meta_de, method = "limma",
                       logfc_cutoff = logfc_cut, padj_cutoff = padj_cut)$de_results)
  }
  if (method == "deseq2") {
    ds <- gexpipe_deseq2_design(meta_de)
    design_mm <- stats::model.matrix(ds$formula, data = meta_de)
    filt <- gexpipe_independent_filter(expr, design = design_mm)
    dds <- DESeq2::DESeqDataSetFromMatrix(filt$expr, meta_de, design = ds$formula)
    dds <- DESeq2::DESeq(dds, quiet = TRUE)
    rn <- DESeq2::resultsNames(dds)
    coef_name <- rn[grep("Disease", rn)[1]]
    tt <- DESeq2::results(dds, name = coef_name)
    return(data.frame(
      Gene = rownames(tt), logFC = tt$log2FoldChange, AveExpr = tt$baseMean,
      P.Value = tt$pvalue, adj.P.Val = tt$padj, stringsAsFactors = FALSE
    ))
  }
  if (method == "edger") {
    de_design <- gexpipe_build_de_design(meta_de)
    filt <- gexpipe_independent_filter(expr, design = de_design$design)
    y <- edgeR::DGEList(counts = filt$expr)
    y <- edgeR::calcNormFactors(y)
    y <- edgeR::estimateDisp(y, de_design$design)
    fit <- edgeR::glmQLFit(y, de_design$design)
    qlf <- edgeR::glmQLFTest(fit, coef = de_design$coef_condition)
    tt <- edgeR::topTags(qlf, n = Inf)$table
    return(data.frame(
      Gene = rownames(tt), logFC = tt$logFC, AveExpr = tt$logCPM,
      P.Value = tt$PValue, adj.P.Val = tt$FDR, stringsAsFactors = FALSE
    ))
  }
  # limma_voom
  de_design <- gexpipe_build_de_design(meta_de)
  filt <- gexpipe_independent_filter(expr, design = de_design$design)
  y <- edgeR::DGEList(counts = filt$expr)
  y <- edgeR::calcNormFactors(y)
  v <- limma::voom(y, de_design$design, plot = FALSE)
  fit <- limma::lmFit(v, de_design$design)
  fit <- limma::eBayes(fit)
  tt <- limma::topTable(fit, coef = de_design$coef_condition, number = Inf, sort.by = "P")
  data.frame(
    Gene = rownames(tt), logFC = tt$logFC, AveExpr = tt$AveExpr,
    P.Value = tt$P.Value, adj.P.Val = tt$adj.P.Val, stringsAsFactors = FALSE
  )
}

jobs <- list(
  list(tool = "GExPipe", method = "limma",       runner = function() run_gexpipe_method("limma")),
  list(tool = "GExPipe", method = "limma_voom",  runner = function() run_gexpipe_method("limma_voom")),
  list(tool = "GExPipe", method = "deseq2",      runner = function() run_gexpipe_method("deseq2")),
  list(tool = "GExPipe", method = "edger",       runner = function() run_gexpipe_method("edger")),
  list(tool = "Standard_Bioc_script", method = "limma_voom", runner = function() run_standard_limma_voom(counts, meta)),
  list(tool = "Standard_Bioc_script", method = "deseq2",     runner = function() run_standard_deseq2(counts, meta)),
  list(tool = "Standard_Bioc_script", method = "edger",      runner = function() run_standard_edger(counts, meta))
)

rows <- list()
sig_lists <- list()

for (job in jobs) {
  label <- paste(job$tool, job$method, sep = "/")
  cat("Running", label, "...\n")
  timed <- tryCatch(time_call(job$runner()), error = function(e) {
    list(result = NULL, seconds = NA_real_, mem_mb_delta = NA_real_, error = conditionMessage(e))
  })
  if (is.null(timed$result)) {
    cat("  FAILED:", timed$error %||% "unknown", "\n")
    rows[[length(rows) + 1L]] <- data.frame(
      Tool = job$tool, Method = job$method, Status = "FAIL",
      Seconds = NA_real_, Mem_MB_delta = NA_real_,
      n_genes_tested = NA_integer_, n_DEGs = NA_integer_,
      Note = timed$error %||% "error", stringsAsFactors = FALSE
    )
    next
  }
  de <- timed$result
  de <- de[!is.na(de$adj.P.Val), , drop = FALSE]
  sg <- sig_genes(de)
  sig_lists[[label]] <- sg
  utils::write.csv(de, file.path(outdir, paste0(job$tool, "_", job$method, "_DE_all.csv")), row.names = FALSE)
  utils::write.csv(data.frame(Gene = sg), file.path(outdir, paste0(job$tool, "_", job$method, "_DE_sig.csv")), row.names = FALSE)
  cat(sprintf("  %.1f s | DEGs = %d\n", timed$seconds, length(sg)))
  rows[[length(rows) + 1L]] <- data.frame(
    Tool = job$tool, Method = job$method, Status = "OK",
    Seconds = round(timed$seconds, 2),
    Mem_MB_delta = round(timed$mem_mb_delta, 1),
    n_genes_tested = nrow(de),
    n_DEGs = length(sg),
    Note = "automated",
    stringsAsFactors = FALSE
  )
}

auto_df <- do.call(rbind, rows)
utils::write.csv(auto_df, file.path(outdir, "automated_timing_de_results.csv"), row.names = FALSE)

# Jaccard among automated methods
lab <- names(sig_lists)
jac <- expand.grid(A = lab, B = lab, stringsAsFactors = FALSE)
jac$Jaccard <- mapply(function(a, b) jaccard(sig_lists[[a]], sig_lists[[b]]), jac$A, jac$B)
utils::write.csv(jac, file.path(outdir, "de_jaccard_matrix_long.csv"), row.names = FALSE)

# ---- Fillable competitor efficiency table for the paper ----
paper_tbl <- data.frame(
  Tool = c(
    "GExPipe (limma-voom)",
    "GExPipe (DESeq2)",
    "GExPipe (edgeR)",
    "Standard Bioconductor script (limma-voom)",
    "Standard Bioconductor script (DESeq2)",
    "Standard Bioconductor script (edgeR)",
    "GEOexplorer",
    "Shiny-Seq",
    "iGEAK",
    "GeneTrailExpress"
  ),
  Interface = c(
    "Shiny + R helpers", "Shiny + R helpers", "Shiny + R helpers",
    "Command line / R script", "Command line / R script", "Command line / R script",
    "Shiny / web", "Shiny / web / Docker", "Desktop Shiny", "Web"
  ),
  Input = c(
    "GEO GSE / counts", "GEO GSE / counts", "GEO GSE / counts",
    "Count matrix", "Count matrix", "Count matrix",
    "GEO GSE / CSV", "Uploaded RNA-seq", "Uploaded matrix", "Uploaded lists / expression"
  ),
  Seconds_to_DEG_table = NA_real_,
  n_DEGs = NA_integer_,
  Jaccard_vs_GExPipe_deseq2 = NA_real_,
  Cross_platform_merge = c("Yes", "Yes", "Yes", "No", "No", "No", "No", "No", "No", "No"),
  Ensemble_ML_ROC = c("Yes", "Yes", "Yes", "No", "No", "No", "No", "No", "No", "No"),
  Notes = c(
    "Fill from automated row limma_voom",
    "Fill from automated row deseq2",
    "Fill from automated row edger",
    "Proxy for GUI engines under the hood",
    "Proxy for GUI engines under the hood",
    "Proxy for GUI engines under the hood",
    "MANUAL: see MANUAL_TIMING_PROTOCOL.md",
    "MANUAL: RNA-seq only; see protocol",
    "MANUAL: upload counts+phenotype; see protocol",
    "N/A for DE — enrichment tool; record enrichment runtime only"
  ),
  stringsAsFactors = FALSE
)

# Auto-fill GExPipe + standard script rows
fill_row <- function(tool_label, tool_key, method_key) {
  hit <- auto_df$Tool == tool_key & auto_df$Method == method_key & auto_df$Status == "OK"
  if (!any(hit)) return(invisible(NULL))
  i <- which(paper_tbl$Tool == tool_label)[1]
  paper_tbl$Seconds_to_DEG_table[i] <<- auto_df$Seconds[hit][1]
  paper_tbl$n_DEGs[i] <<- auto_df$n_DEGs[hit][1]
  ref <- sig_lists[["GExPipe/deseq2"]]
  lab <- paste(tool_key, method_key, sep = "/")
  if (!is.null(ref) && !is.null(sig_lists[[lab]])) {
    paper_tbl$Jaccard_vs_GExPipe_deseq2[i] <<- round(jaccard(sig_lists[[lab]], ref), 3)
  }
}

fill_row("GExPipe (limma-voom)", "GExPipe", "limma_voom")
fill_row("GExPipe (DESeq2)", "GExPipe", "deseq2")
fill_row("GExPipe (edgeR)", "GExPipe", "edger")
fill_row("Standard Bioconductor script (limma-voom)", "Standard_Bioc_script", "limma_voom")
fill_row("Standard Bioconductor script (DESeq2)", "Standard_Bioc_script", "deseq2")
fill_row("Standard Bioconductor script (edgeR)", "Standard_Bioc_script", "edger")

utils::write.csv(paper_tbl, file.path(outdir, "Table_competitor_efficiency_DE.csv"), row.names = FALSE)

# Manual entry template
manual <- data.frame(
  Tool = c("GEOexplorer", "Shiny-Seq", "iGEAK", "GeneTrailExpress"),
  Seconds_to_DEG_or_main_result = NA_real_,
  n_DEGs_or_NA = NA_character_,
  Same_thresholds_used = "Yes/No (|log2FC|>=0.5, FDR<=0.05)",
  Hardware_notes = "",
  Operator = "",
  Date = "",
  stringsAsFactors = FALSE
)
utils::write.csv(manual, file.path(outdir, "manual_competitor_timing_TEMPLATE.csv"), row.names = FALSE)

cat("\n========== AUTOMATED RESULTS ==========\n")
print(auto_df, row.names = FALSE)
cat("\nWrote:\n")
cat(" ", file.path(outdir, "Table_competitor_efficiency_DE.csv"), "\n")
cat(" ", file.path(outdir, "automated_timing_de_results.csv"), "\n")
cat(" ", file.path(outdir, "manual_competitor_timing_TEMPLATE.csv"), "\n")
cat("\nNext: follow MANUAL_TIMING_PROTOCOL.md to fill GUI tool rows.\n")
