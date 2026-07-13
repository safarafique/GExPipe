#!/usr/bin/env Rscript
## JPCT M3 — Cross-platform panel transfer
## Train classifier on one assay; test on the other (micro ↔ RNA-seq).
## Writes ONLY under GExPipe(original_paper)/; does not modify Application Note package code.
##
## Usage (WSL):
##   cd "/mnt/e/GExPipe/GExPipe(original_paper)"
##   "/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
##     scripts/cross-platform-panel-transfer.R \
##     --gexpipe-repo "E:/GExPipe"
##
## First run downloads/rebuilds the merged matrix and caches:
##   results/cache/merged_expr_GSE89076_GSE50760.rds

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
      drive <- toupper(r[2])
      rest <- if (length(r) >= 3L && nzchar(r[3])) r[3] else ""
      path <- paste0(drive, ":", rest)
    }
  }
  path
}

ca <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", ca[grep("^--file=", ca)])
script_path <- if (length(file_arg)) normalizePath(file_arg[1], winslash = "/", mustWork = FALSE) else getwd()
orig_root <- normalize_repo_path(dirname(dirname(script_path)))
if (!dir.exists(file.path(orig_root, "scripts"))) {
  orig_root <- normalize_repo_path(getwd())
}

gexpipe_repo <- normalize_repo_path(get_arg("--gexpipe-repo", dirname(orig_root)))
seed <- as.integer(get_arg("--seed", "42"))
k_panel <- as.integer(get_arg("--k", "20"))
force_rebuild <- identical(tolower(get_arg("--rebuild", "false")), "true")

vm_cp <- file.path(gexpipe_repo, "validation_manual", "cross_platform")
results_dir <- file.path(orig_root, "results")
cache_dir <- file.path(results_dir, "cache")
sig_dir <- file.path(orig_root, "signatures")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(sig_dir, showWarnings = FALSE, recursive = TRUE)

cache_rds <- file.path(cache_dir, "merged_expr_GSE89076_GSE50760.rds")

suppressPackageStartupMessages({
  if (!requireNamespace("pkgload", quietly = TRUE)) stop("Need pkgload")
  if (!requireNamespace("randomForest", quietly = TRUE)) stop("Need randomForest")
  if (!requireNamespace("pROC", quietly = TRUE)) stop("Need pROC")
  if (!requireNamespace("glmnet", quietly = TRUE)) stop("Need glmnet")
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

read_genes <- function(path) {
  if (!file.exists(path)) return(character())
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  col <- if ("Gene" %in% names(df)) "Gene" else names(df)[1L]
  g <- unique(trimws(as.character(df[[col]])))
  g[nzchar(g)]
}

top_n_from_de <- function(path, n, genes_restrict = NULL) {
  if (!file.exists(path)) return(character())
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!is.null(genes_restrict)) df <- df[df$Gene %in% genes_restrict, , drop = FALSE]
  df <- df[order(df$adj.P.Val), , drop = FALSE]
  head(unique(as.character(df$Gene)), n)
}

panel_auc_rf <- function(X_tr, y_tr, X_va, y_va, seed = 42L) {
  genes <- intersect(colnames(X_tr), colnames(X_va))
  if (length(genes) < 1L) return(list(auc = NA_real_, k = 0L))
  X_tr <- as.matrix(X_tr[, genes, drop = FALSE])
  X_va <- as.matrix(X_va[, genes, drop = FALSE])
  set.seed(seed)
  yf <- factor(y_tr, levels = c(0, 1))
  rf <- tryCatch(randomForest::randomForest(x = X_tr, y = yf, ntree = 500), error = function(e) NULL)
  if (is.null(rf)) return(list(auc = NA_real_, k = length(genes)))
  p <- stats::predict(rf, newdata = X_va, type = "prob")
  pr <- if ("1" %in% colnames(p)) as.numeric(p[, "1"]) else as.numeric(p[, 2])
  list(auc = gene_auc(y_va, pr), k = length(genes), score = pr)
}

panel_auc_glmnet <- function(X_tr, y_tr, X_va, y_va, seed = 42L) {
  genes <- intersect(colnames(X_tr), colnames(X_va))
  if (length(genes) < 1L) return(list(auc = NA_real_, k = 0L))
  X_tr <- as.matrix(X_tr[, genes, drop = FALSE])
  X_va <- as.matrix(X_va[, genes, drop = FALSE])
  set.seed(seed)
  cv <- tryCatch(
    glmnet::cv.glmnet(X_tr, y_tr, family = "binomial", alpha = 1,
                      nfolds = min(5L, length(y_tr))),
    error = function(e) NULL
  )
  if (is.null(cv)) return(list(auc = NA_real_, k = length(genes)))
  pr <- as.numeric(stats::predict(cv, newx = X_va, s = "lambda.min", type = "response"))
  list(auc = gene_auc(y_va, pr), k = length(genes))
}

median_gene_auc <- function(dat, y, genes) {
  genes <- intersect(genes, colnames(dat))
  if (!length(genes)) return(NA_real_)
  stats::median(vapply(genes, function(g) gene_auc(y, dat[, g]), numeric(1)), na.rm = TRUE)
}

## ---- Build or load merged expression ----
build_merged <- function(repo, work_dir) {
  cat("Loading GExPipe via pkgload (read-only)...\n")
  pkgload::load_all(repo, quiet = TRUE)

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

  micro_gse <- "GSE89076"
  rna_gse <- "GSE50760"
  micro_dir <- file.path(work_dir, "micro_data")
  rna_dir <- file.path(work_dir, "rna_data")
  dir.create(micro_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(rna_dir, recursive = TRUE, showWarnings = FALSE)

  cat("Downloading / loading microarray", micro_gse, "...\n")
  micro_dl <- gexp_download_one_microarray_gse(micro_gse, micro_dir)
  if (!isTRUE(micro_dl$ok)) stop(micro_gse, " failed: ", micro_dl$reason %||% "unknown")
  cat("Downloading / loading RNA-seq", rna_gse, "...\n")
  rna_dl <- gexp_download_one_rnaseq_gse(rna_gse, rna_dir)
  if (!isTRUE(rna_dl$ok)) stop(rna_gse, " failed: ", rna_dl$reason %||% "unknown")

  meta_micro <- assign_gse89076(micro_dl$metadata)
  meta_rna <- assign_gse50760(rna_dl$metadata)
  micro_expr <- micro_dl$micro_expr[, rownames(meta_micro), drop = FALSE]
  rna_counts <- rna_dl$count_matrix[, rownames(meta_rna), drop = FALSE]
  micro_expr <- map_micro_to_symbols(micro_expr, micro_dl$micro_eset, micro_gse)

  cat("Normalizing and intersecting...\n")
  norm_merged <- gexp_normalize_and_intersect(
    micro_expr_list = setNames(list(micro_expr), micro_gse),
    rna_counts_list = setNames(list(rna_counts), rna_gse),
    de_method = "limma"
  )
  meta_g <- norm_merged$unified_metadata
  meta_g$Condition <- NA_character_
  idx_micro <- match(meta_g$SampleID, rownames(meta_micro))
  idx_rna <- match(meta_g$SampleID, rownames(meta_rna))
  meta_g$Condition[!is.na(idx_micro)] <- meta_micro$Condition[idx_micro[!is.na(idx_micro)]]
  meta_g$Condition[!is.na(idx_rna)] <- meta_rna$Condition[idx_rna[!is.na(idx_rna)]]
  meta_g <- meta_g[!is.na(meta_g$Condition), , drop = FALSE]
  rownames(meta_g) <- meta_g$SampleID
  expr_g <- norm_merged$combined_expr[, rownames(meta_g), drop = FALSE]
  cat("Batch-correcting (limma removeBatchEffect)...\n")
  batch_g <- safe_batch(expr_g, meta_g)
  expr_bc <- batch_g$batch_corrected[, rownames(meta_g), drop = FALSE]

  list(
    expr = expr_bc,
    meta = meta_g,
    common_genes = rownames(expr_bc),
    micro_gse = micro_gse,
    rna_gse = rna_gse
  )
}

if (file.exists(cache_rds) && !force_rebuild) {
  cat("Loading cached merged matrix:", cache_rds, "\n")
  obj <- readRDS(cache_rds)
} else {
  work <- file.path(vm_cp, "work")
  obj <- build_merged(gexpipe_repo, work)
  saveRDS(obj, cache_rds)
  cat("Cached:", cache_rds, "\n")
}

expr <- obj$expr  # genes x samples
meta <- obj$meta
stopifnot(all(c("Condition", "Platform") %in% names(meta)))
meta$Platform <- as.character(meta$Platform)
## Harmonize platform labels
meta$Platform[grepl("micro", tolower(meta$Platform))] <- "Microarray"
meta$Platform[grepl("rna", tolower(meta$Platform))] <- "RNAseq"

idx_m <- which(meta$Platform == "Microarray")
idx_r <- which(meta$Platform == "RNAseq")
cat("Samples: Microarray=", length(idx_m), " RNAseq=", length(idx_r), "\n", sep = "")
cat("Genes:", nrow(expr), "\n")

dat_m <- t(expr[, idx_m, drop = FALSE])
dat_r <- t(expr[, idx_r, drop = FALSE])
y_m <- as.numeric(meta$Condition[idx_m] == "Disease")
y_r <- as.numeric(meta$Condition[idx_r] == "Disease")

## ---- Panels from existing DE exports (JPCT gene sources) ----
merged_all <- file.path(vm_cp, "merged_limma_DE_all.csv")
micro_all <- file.path(vm_cp, "microarray_limma_DE_all.csv")
rna_all <- file.path(vm_cp, "rnaseq_DE_all.csv")
same_both <- file.path(vm_cp, "report", "same_in_both.csv")
only_merged <- file.path(vm_cp, "report", "only_in_merged.csv")
only_sep <- file.path(vm_cp, "report", "only_in_separate_common.csv")
cons_path <- file.path(gexpipe_repo, "validation_manual", "consensus_signature_genes.csv")

## Separate∩ = genes significant on BOTH platforms
sig_m <- read_genes(file.path(vm_cp, "microarray_limma_DE_sig.csv"))
sig_r <- read_genes(file.path(vm_cp, "rnaseq_DE_sig.csv"))
sep_inter <- intersect(sig_m, sig_r)

rank_in_list <- function(genes, de_path, n) {
  if (!length(genes) || !file.exists(de_path)) return(character())
  df <- utils::read.csv(de_path, stringsAsFactors = FALSE)
  df <- df[df$Gene %in% genes, , drop = FALSE]
  df <- df[order(df$adj.P.Val), , drop = FALSE]
  head(unique(as.character(df$Gene)), n)
}

panels <- list(
  Merged_limma_topk = top_n_from_de(merged_all, k_panel),
  Separate_intersect_topk = rank_in_list(sep_inter, merged_all, k_panel),
  Same_in_both_topk = rank_in_list(read_genes(same_both), merged_all, k_panel),
  Only_in_merged_topk = rank_in_list(read_genes(only_merged), merged_all, k_panel),
  Only_in_separate_topk = rank_in_list(read_genes(only_sep), merged_all, k_panel),
  JPCT_consensus_overlap = intersect(read_genes(cons_path), rownames(expr))
)

## If consensus overlap empty/small, keep as-is; add random control matched to k
set.seed(seed)
universe <- setdiff(rownames(expr), panels$Merged_limma_topk)
panels$Random_matched_k <- sample(universe, size = min(k_panel, length(universe)))

## Write signature files
for (nm in names(panels)) {
  utils::write.csv(
    data.frame(Gene = panels[[nm]], stringsAsFactors = FALSE),
    file.path(sig_dir, paste0("transfer_", nm, ".csv")),
    row.names = FALSE
  )
}

cat("Evaluating transfer (k=", k_panel, ")...\n", sep = "")
rows <- list()
for (nm in names(panels)) {
  genes <- panels[[nm]]
  genes <- intersect(genes, colnames(dat_m))
  genes <- intersect(genes, colnames(dat_r))
  cat("  ", nm, " usable genes=", length(genes), "\n", sep = "")

  ## Direction 1: train Microarray → test RNAseq
  rf_mr <- panel_auc_rf(dat_m, y_m, dat_r, y_r, seed = seed)
  gl_mr <- panel_auc_glmnet(dat_m, y_m, dat_r, y_r, seed = seed)
  ## Direction 2: train RNAseq → test Microarray
  rf_rm <- panel_auc_rf(dat_r, y_r, dat_m, y_m, seed = seed)
  gl_rm <- panel_auc_glmnet(dat_r, y_r, dat_m, y_m, seed = seed)

  rows[[length(rows) + 1L]] <- data.frame(
    Panel = nm,
    k_requested = length(panels[[nm]]),
    k_used = length(genes),
    Median_gene_AUC_on_RNAseq = median_gene_auc(dat_r, y_r, genes),
    Median_gene_AUC_on_Microarray = median_gene_auc(dat_m, y_m, genes),
    Transfer_RF_Micro_to_RNA = rf_mr$auc,
    Transfer_glmnet_Micro_to_RNA = gl_mr$auc,
    Transfer_RF_RNA_to_Micro = rf_rm$auc,
    Transfer_glmnet_RNA_to_Micro = gl_rm$auc,
    Transfer_RF_mean = mean(c(rf_mr$auc, rf_rm$auc), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

tab <- do.call(rbind, rows)
out_csv <- file.path(results_dir, "cross_platform_panel_transfer.csv")
utils::write.csv(tab, out_csv, row.names = FALSE)

fmt <- function(x, d = 3) {
  if (length(x) != 1L || is.na(x)) return("—")
  format(round(as.numeric(x), d), nsmall = d)
}

md <- c(
  "# Table 5. Cross-platform panel transfer (JPCT M3)",
  "",
  paste0("Train on one assay, test on the other after joint harmonization + limma batch correction."),
  paste0("Cohorts: GSE89076 (Microarray, n=", length(idx_m), ") ↔ GSE50760 (RNA-seq, n=", length(idx_r), ")."),
  paste0("Matched panel size target k=", k_panel, ". Seed=", seed, "."),
  "",
  "**Primary:** mean of RF transfer AUCs (Micro→RNA and RNA→Micro).",
  "",
  "| Panel | k | Med. gene AUC (RNA) | RF Micro→RNA | RF RNA→Micro | **RF mean** | glmnet mean* |",
  "|-------|---|---------------------|--------------|--------------|-------------|--------------|"
)

for (i in seq_len(nrow(tab))) {
  gmean <- mean(c(tab$Transfer_glmnet_Micro_to_RNA[i], tab$Transfer_glmnet_RNA_to_Micro[i]), na.rm = TRUE)
  md <- c(md, paste0(
    "| ", tab$Panel[i], " | ", tab$k_used[i], " | ",
    fmt(tab$Median_gene_AUC_on_RNAseq[i]), " | ",
    fmt(tab$Transfer_RF_Micro_to_RNA[i]), " | ",
    fmt(tab$Transfer_RF_RNA_to_Micro[i]), " | ",
    fmt(tab$Transfer_RF_mean[i]), " | ",
    fmt(gmean), " |"
  ))
}

## Headline contrasts
getv <- function(panel, col) tab[[col]][tab$Panel == panel][1]
md <- c(
  md,
  "",
  "## Headline (biological transfer)",
  "",
  paste0("- **Merged limma top-", k_panel, "** RF mean transfer = ",
         fmt(getv("Merged_limma_topk", "Transfer_RF_mean")), "."),
  paste0("- **Separate∩ top-", k_panel, "** RF mean transfer = ",
         fmt(getv("Separate_intersect_topk", "Transfer_RF_mean")), "."),
  paste0("- **Only-in-merged top-", k_panel, "** RF mean transfer = ",
         fmt(getv("Only_in_merged_topk", "Transfer_RF_mean")),
         " (genes ∩ would discard)."),
  paste0("- **Random** RF mean transfer = ",
         fmt(getv("Random_matched_k", "Transfer_RF_mean")), "."),
  "",
  "JPCT prediction: merged / same-in-both / only-in-merged should transfer better than random;",
  "merged should match or beat separate∩ at matched k if joint estimand captures shared biology.",
  "",
  paste0("\\* glmnet mean = average of both transfer directions."),
  paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE))
)

out_md <- file.path(results_dir, "cross_platform_panel_transfer.md")
writeLines(md, out_md)

## Patch Original Paper section 4.4 if present
paper <- file.path(orig_root, "manuscript", "GExPipe_Original_Paper.md")
if (file.exists(paper)) {
  txt <- readLines(paper, warn = FALSE)
  marker <- "### 4.4 Cross-platform transfer"
  i0 <- grep(marker, txt, fixed = TRUE)[1]
  if (!is.na(i0)) {
    ## find next ### at same level or ## 
    rest <- txt[(i0 + 1L):length(txt)]
    i1_rel <- grep("^## ", rest)[1]
    i1 <- if (is.na(i1_rel)) length(txt) + 1L else i0 + i1_rel
    new_sec <- c(
      "### 4.4 Cross-platform transfer (JPCT M3) — results",
      "",
      paste0("Source: `results/cross_platform_panel_transfer.md` (", format(Sys.Date()), ")."),
      paste0("Microarray n=", length(idx_m), "; RNA-seq n=", length(idx_r), "; matched k=", k_panel, "."),
      "",
      md[grep("^\\|", md)],
      "",
      paste0(
        "- Merged limma top-k RF mean transfer **",
        fmt(getv("Merged_limma_topk", "Transfer_RF_mean")),
        "** vs separate∩ top-k **",
        fmt(getv("Separate_intersect_topk", "Transfer_RF_mean")),
        "** vs random **",
        fmt(getv("Random_matched_k", "Transfer_RF_mean")),
        "**."
      ),
      paste0(
        "- Only-in-merged top-k (genes ∩ misses) RF mean **",
        fmt(getv("Only_in_merged_topk", "Transfer_RF_mean")),
        "**."
      ),
      ""
    )
    txt <- c(txt[seq_len(i0 - 1L)], new_sec, txt[i1:length(txt)])
    writeLines(txt, paper)
    cat("Updated", paper, "\n")
  }
}

cat("\nWrote:\n  ", out_csv, "\n  ", out_md, "\n", sep = "")
