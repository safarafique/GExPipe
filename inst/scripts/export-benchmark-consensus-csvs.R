#!/usr/bin/env Rscript
## Headless export of Step 8/10/12 CSVs — NO pkgload / GExPipe install required.
## Needs: GEOquery, edgeR, limma, WGCNA, glmnet, randomForest, pROC
## (optional: kernlab, caret for SVM-RFE)
##
## Usage:
##   cd /mnt/e/GExPipe
##   Rscript inst/scripts/export-benchmark-consensus-csvs.R --repo /mnt/e/GExPipe

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) args[i + 1L] else default
}

# Windows Rscript launched from WSL often gets --repo /mnt/e/... which is invalid on Win32.
# Translate /mnt/<drive>/... -> <DRIVE>:/...
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
    path <- chartr("/", "\\", path)
  }
  path
}

repo_root <- normalize_repo_path(get_arg("--repo", getwd()))
outdir <- normalize_repo_path(get_arg("--outdir", file.path(repo_root, "validation_manual")))
# If outdir was built with forward slashes before normalize of default, rebuild from repo
if (!dir.exists(outdir) && dir.exists(file.path(repo_root, "validation_manual"))) {
  outdir <- file.path(repo_root, "validation_manual")
}
work <- file.path(outdir, "work", "rna_data")
dir.create(work, showWarnings = FALSE, recursive = TRUE)
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

set.seed(123)
`%||%` <- function(a, b) if (!is.null(a)) a else b

need_pkgs <- c("GEOquery", "Biobase", "edgeR", "limma", "WGCNA", "glmnet", "randomForest", "pROC")
missing <- need_pkgs[!vapply(need_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop(
    "Missing R packages: ", paste(missing, collapse = ", "), "\n",
    "Install with BiocManager / install.packages, then re-run."
  )
}
suppressPackageStartupMessages({
  library(GEOquery)
  library(Biobase)
  library(edgeR)
  library(limma)
  library(WGCNA)
  library(glmnet)
  library(randomForest)
  library(pROC)
})
options(stringsAsFactors = FALSE)
try(allowWGCNAThreads(), silent = TRUE)

gene_auc <- function(y, x) {
  y <- as.numeric(y); x <- as.numeric(x)
  if (length(unique(y[!is.na(y)])) < 2L) return(NA_real_)
  roc <- tryCatch(pROC::roc(y, x, quiet = TRUE, direction = "auto"), error = function(e) NULL)
  if (is.null(roc)) return(NA_real_)
  as.numeric(pROC::auc(roc))
}

tmm_log_cpm <- function(counts) {
  y <- edgeR::DGEList(counts = counts)
  y <- edgeR::calcNormFactors(y)
  logcpm <- edgeR::cpm(y, log = TRUE, prior.count = 1)
  as.matrix(logcpm)
}

assign_gse104836 <- function(meta) {
  txt <- apply(meta, 1L, function(r) paste(tolower(as.character(r)), collapse = " "))
  cond <- rep(NA_character_, length(txt))
  cond[grepl("nontumor|non-tumor", txt)] <- "Normal"
  cond[grepl("colon cancer|cancer tissue|\\bc\\b|_c$", txt)] <- "Disease"
  meta$Condition <- cond
  meta[!is.na(meta$Condition), , drop = FALSE]
}

# NCBI matrices often use Entrez IDs; training uses HGNC symbols.
ensure_symbol_counts <- function(counts) {
  ids <- rownames(counts)
  if (!length(ids)) return(counts)
  if (mean(grepl("^[0-9]+$", head(ids, min(200L, length(ids))))) <= 0.8) {
    return(counts)  # already symbols
  }
  if (!requireNamespace("org.Hs.eg.db", quietly = TRUE) ||
      !requireNamespace("AnnotationDbi", quietly = TRUE)) {
    stop("Validation counts are Entrez IDs; install org.Hs.eg.db to map to symbols")
  }
  cat("  Mapping Entrez -> SYMBOL (", length(ids), "IDs)...\n")
  map <- AnnotationDbi::select(
    org.Hs.eg.db::org.Hs.eg.db,
    keys = ids,
    columns = "SYMBOL",
    keytype = "ENTREZID"
  )
  map <- map[!is.na(map$SYMBOL) & nzchar(map$SYMBOL), , drop = FALSE]
  map <- map[!duplicated(map$ENTREZID), , drop = FALSE]
  keep <- intersect(ids, map$ENTREZID)
  counts <- counts[keep, , drop = FALSE]
  sym <- map$SYMBOL[match(rownames(counts), map$ENTREZID)]
  split_idx <- split(seq_len(nrow(counts)), sym)
  out <- lapply(split_idx, function(ii) {
    if (length(ii) == 1L) counts[ii, , drop = TRUE] else colSums(counts[ii, , drop = FALSE])
  })
  mat <- do.call(rbind, out)
  rownames(mat) <- names(out)
  storage.mode(mat) <- "numeric"
  cat("  Symbol genes after map:", nrow(mat), "\n")
  mat
}

download_gse104836_counts <- function(gse_id = "GSE104836", dest = work) {
  gse_dir <- file.path(dest, gse_id)
  dir.create(gse_dir, showWarnings = FALSE, recursive = TRUE)
  cat("Downloading GEO supplementary for", gse_id, "...\n")
  suppressMessages(GEOquery::getGEOSuppFiles(gse_id, baseDir = dest, makeDirectory = TRUE, fetch_files = TRUE))
  files <- list.files(gse_dir, recursive = TRUE, full.names = TRUE)
  tar_files <- files[grepl("\\.tar$", files, ignore.case = TRUE)]
  for (tf in tar_files) {
    try(utils::untar(tf, exdir = gse_dir), silent = TRUE)
  }
  files <- list.files(gse_dir, recursive = TRUE, full.names = TRUE)
  # Prefer count-like tables
  cand <- files[grepl("count|raw|htseq|matrix", basename(files), ignore.case = TRUE)]
  cand <- cand[!grepl("\\.tar$", cand, ignore.case = TRUE)]
  if (!length(cand)) cand <- files[grepl("\\.txt(\\.gz)?$|\\.csv(\\.gz)?$", files, ignore.case = TRUE)]
  if (!length(cand)) stop("No count files found under ", gse_dir)

  read_one <- function(path) {
    df <- tryCatch({
      if (grepl("\\.gz$", path, ignore.case = TRUE)) {
        utils::read.delim(gzfile(path), check.names = FALSE, stringsAsFactors = FALSE)
      } else {
        utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)
      }
    }, error = function(e) NULL)
    if (is.null(df) || ncol(df) < 2L) return(NULL)
    rn <- as.character(df[[1]])
    mat <- as.matrix(df[, -1, drop = FALSE])
    storage.mode(mat) <- "numeric"
    rownames(mat) <- rn
    mat <- mat[!grepl("^__", rownames(mat)), , drop = FALSE]
    mat
  }

  best <- NULL
  best_n <- 0L
  for (p in cand) {
    m <- read_one(p)
    if (is.null(m)) next
    if (ncol(m) > best_n) {
      best <- m
      best_n <- ncol(m)
      cat("  candidate", basename(p), "->", nrow(m), "x", ncol(m), "\n")
    }
  }
  if (is.null(best)) stop("Could not parse any count matrix for ", gse_id)

  # NCBI matrices often use Entrez IDs; map to HGNC symbols for cross-cohort ROC
  if (mean(grepl("^[0-9]+$", head(rownames(best), 200))) > 0.8) {
    if (!requireNamespace("org.Hs.eg.db", quietly = TRUE) ||
        !requireNamespace("AnnotationDbi", quietly = TRUE)) {
      stop("Validation counts are Entrez IDs; install org.Hs.eg.db to map to symbols")
    }
    cat("  Mapping Entrez -> SYMBOL for", gse_id, "...\n")
    map <- AnnotationDbi::select(
      org.Hs.eg.db::org.Hs.eg.db,
      keys = rownames(best),
      columns = "SYMBOL",
      keytype = "ENTREZID"
    )
    map <- map[!is.na(map$SYMBOL) & nzchar(map$SYMBOL), , drop = FALSE]
    map <- map[!duplicated(map$ENTREZID), , drop = FALSE]
    keep <- intersect(rownames(best), map$ENTREZID)
    best <- best[keep, , drop = FALSE]
    sym <- map$SYMBOL[match(rownames(best), map$ENTREZID)]
    split_idx <- split(seq_len(nrow(best)), sym)
    out <- lapply(split_idx, function(ii) {
      if (length(ii) == 1L) best[ii, , drop = TRUE] else colSums(best[ii, , drop = FALSE])
    })
    best <- do.call(rbind, out)
    rownames(best) <- names(out)
    storage.mode(best) <- "numeric"
    cat("  Symbol genes after map:", nrow(best), "\n")
  }

  cat("Fetching metadata for", gse_id, "...\n")
  gse <- suppressMessages(GEOquery::getGEO(gse_id, GSEMatrix = TRUE, getGPL = FALSE))
  if (is.list(gse)) gse <- gse[[1]]
  pd <- Biobase::pData(gse)
  list(counts = best, metadata = pd)
}

svm_rank_genes <- function(X, y, max_rfe_genes = 50L) {
  X <- as.matrix(X); mode(X) <- "numeric"
  genes <- colnames(X)
  use_rfe <- ncol(X) <= max_rfe_genes &&
    requireNamespace("kernlab", quietly = TRUE) &&
    requireNamespace("caret", quietly = TRUE)
  if (use_rfe) {
    ranked <- character(); remaining <- seq_len(ncol(X))
    while (length(remaining) > 1L) {
      folds <- caret::createFolds(y, k = min(5L, length(y)))
      scores <- rep(0, length(remaining))
      for (fold in folds) {
        model <- tryCatch(
          kernlab::ksvm(X[-fold, remaining, drop = FALSE], y[-fold],
                        kernel = "vanilladot", C = 1, scaled = FALSE),
          error = function(e) NULL
        )
        if (is.null(model) || is.null(model@coef) || !length(model@coef[[1]])) next
        w <- tryCatch(t(model@coef[[1]]) %*% model@xmatrix[[1]],
                      error = function(e) rep(0, length(remaining)))
        scores <- scores + (as.numeric(w)^2)
      }
      worst <- which.min(scores)
      ranked <- c(genes[remaining[worst]], ranked)
      remaining <- remaining[-worst]
    }
    return(c(genes[remaining], ranked))
  }
  cors <- abs(apply(X, 2L, function(col) suppressWarnings(stats::cor(col, as.numeric(y)))))
  names(sort(cors, decreasing = TRUE, na.last = TRUE))
}

cat("=== Export benchmark consensus CSVs (standalone) ===\n")
cat("Repo:", repo_root, "\n")

# ---- Training: upload_pack ----
upload_counts <- file.path(outdir, "competitor_benchmark", "upload_pack", "GSE50760_counts.csv")
upload_pheno <- file.path(outdir, "competitor_benchmark", "upload_pack", "GSE50760_phenotype.csv")
if (!file.exists(upload_counts) || !file.exists(upload_pheno)) {
  stop("Missing upload_pack GSE50760 counts/phenotype under validation_manual/competitor_benchmark/upload_pack/")
}
cat("Training: upload_pack GSE50760\n")
counts_train <- as.matrix(utils::read.csv(upload_counts, row.names = 1L, check.names = FALSE))
storage.mode(counts_train) <- "numeric"
pheno <- utils::read.csv(upload_pheno, stringsAsFactors = FALSE, check.names = FALSE)
id_col <- if ("SampleID" %in% names(pheno)) "SampleID" else names(pheno)[1L]
cond_col <- if ("Condition" %in% names(pheno)) "Condition" else names(pheno)[2L]
meta_train <- data.frame(
  Condition = as.character(pheno[[cond_col]]),
  row.names = as.character(pheno[[id_col]]),
  stringsAsFactors = FALSE
)
meta_train$Condition[grepl("primary|tumor|disease|cancer", tolower(meta_train$Condition))] <- "Disease"
meta_train$Condition[grepl("normal|nontumor|control", tolower(meta_train$Condition))] <- "Normal"
meta_train <- meta_train[meta_train$Condition %in% c("Normal", "Disease"), , drop = FALSE]
common_s <- intersect(colnames(counts_train), rownames(meta_train))
counts_train <- counts_train[, common_s, drop = FALSE]
meta_train <- meta_train[common_s, , drop = FALSE]
meta_train$Condition <- factor(meta_train$Condition, levels = c("Normal", "Disease"))
cat("  Train:", ncol(counts_train), "samples (",
    paste(names(table(meta_train$Condition)), table(meta_train$Condition), sep = "=", collapse = ", "),
    ")\n")

# ---- Validation ----
cache_rds <- file.path(work, "GSE104836_parsed.rds")
if (file.exists(cache_rds)) {
  cat("Validation: loading cached GSE104836\n")
  val <- readRDS(cache_rds)
} else {
  val <- download_gse104836_counts("GSE104836", work)
  saveRDS(val, cache_rds)
}
counts_val <- if (!is.null(val$counts_symbol)) val$counts_symbol else val$counts
counts_val <- ensure_symbol_counts(counts_val)
# Persist symbol matrix so later cache loads stay mapped
val$counts_symbol <- counts_val
saveRDS(val, cache_rds)
meta_val <- assign_gse104836(as.data.frame(val$metadata, stringsAsFactors = FALSE))
# Align sample names: try GSM / colnames overlap heuristics
cn <- colnames(counts_val)
rn <- rownames(meta_val)
overlap <- intersect(cn, rn)
if (!length(overlap)) {
  # try stripping suffixes
  cn2 <- gsub("\\..*$", "", cn)
  names(cn2) <- cn
  hit <- cn2[cn2 %in% rn]
  if (length(hit)) {
    colnames(counts_val) <- ifelse(cn %in% names(hit), hit[cn], cn)
    overlap <- intersect(colnames(counts_val), rn)
  }
}
if (!length(overlap)) {
  # match by order if dimensions agree with phenotype n
  if (ncol(counts_val) == nrow(meta_val)) {
    colnames(counts_val) <- rownames(meta_val)
    overlap <- rownames(meta_val)
    cat("  Warning: matched val samples by column order\n")
  } else {
    stop("Could not align GSE104836 count colnames with GEO metadata sample IDs")
  }
}
counts_val <- counts_val[, overlap, drop = FALSE]
meta_val <- meta_val[overlap, , drop = FALSE]
meta_val$Condition <- factor(meta_val$Condition, levels = c("Normal", "Disease"))
cat("  Val:", ncol(counts_val), "samples (",
    paste(names(table(meta_val$Condition)), table(meta_val$Condition), sep = "=", collapse = ", "),
    ")\n")

# ---- Normalize ----
cat("TMM log-CPM normalize...\n")
expr_tr <- tmm_log_cpm(counts_train)
expr_va <- tmm_log_cpm(counts_val)
# Quantile normalize within each cohort (genes x samples)
expr_tr <- limma::normalizeBetweenArrays(expr_tr, method = "quantile")
expr_va <- limma::normalizeBetweenArrays(expr_va, method = "quantile")

# ---- DEG list ----
deg_path <- file.path(outdir, "GSE50760_deseq2_DE_sig.csv")
if (!file.exists(deg_path)) stop("Missing ", deg_path)
deg_genes <- unique(trimws(as.character(utils::read.csv(deg_path, stringsAsFactors = FALSE)$Gene)))
deg_genes <- deg_genes[nzchar(deg_genes)]
cat("DEG genes:", length(deg_genes), "\n")

# ---- WGCNA ----
cat("WGCNA prepare (top 5000 variable)...\n")
vars <- apply(expr_tr, 1L, stats::var, na.rm = TRUE)
keep_n <- min(5000L, length(vars))
keep_genes <- names(sort(vars, decreasing = TRUE))[seq_len(keep_n)]
expr_top <- expr_tr[keep_genes, , drop = FALSE]
datExpr <- t(expr_top)  # samples x genes
gsg <- WGCNA::goodSamplesGenes(datExpr, verbose = 0)
if (!gsg$allOK) datExpr <- datExpr[gsg$goodSamples, gsg$goodGenes, drop = FALSE]
sample_info <- meta_train[rownames(datExpr), , drop = FALSE]

cat("pickSoftThreshold...\n")
powers <- c(1:10, seq(12, 20, 2))
sft <- WGCNA::pickSoftThreshold(datExpr, powerVector = powers, verbose = 0, networkType = "signed")
power <- sft$powerEstimate %||% 6L
if (is.na(power)) power <- 6L
cat("  soft power =", power, "\n")

cat("blockwiseModules (this can take several minutes)...\n")
net <- WGCNA::blockwiseModules(
  datExpr, power = power, networkType = "signed", TOMType = "signed",
  minModuleSize = 30L, reassignThreshold = 0, mergeCutHeight = 0.25,
  numericLabels = FALSE, pamRespectsDendro = FALSE, deepSplit = 2L, verbose = 1
)
module_colors <- net$colors
MEs <- WGCNA::orderMEs(net$MEs)
trait_disease <- as.numeric(sample_info$Condition == "Disease")
trait_data <- data.frame(Disease = trait_disease, check.names = FALSE)
rownames(trait_data) <- rownames(sample_info)
moduleTraitCor <- stats::cor(MEs, trait_data, use = "pairwise.complete.obs")
moduleTraitPvalue <- WGCNA::corPvalueStudent(moduleTraitCor, nrow(datExpr))

sig_idx <- which(moduleTraitPvalue[, 1] < 0.05 & abs(moduleTraitCor[, 1]) > 0.2)
sig_colors <- unique(sub("^ME", "", rownames(moduleTraitCor)[sig_idx]))
cat("  Sig modules:", paste(sig_colors, collapse = ", "), "\n")
wgcna_genes <- names(module_colors)[module_colors %in% sig_colors]
common_deg_wgcna <- intersect(deg_genes, wgcna_genes)
cat("  DEG∩WGCNA:", length(common_deg_wgcna), "\n")
if (length(common_deg_wgcna) < 5L) {
  sig_idx <- which(moduleTraitPvalue[, 1] < 0.1 & abs(moduleTraitCor[, 1]) > 0.15)
  sig_colors <- unique(sub("^ME", "", rownames(moduleTraitCor)[sig_idx]))
  wgcna_genes <- names(module_colors)[module_colors %in% sig_colors]
  common_deg_wgcna <- intersect(deg_genes, wgcna_genes)
  cat("  DEG∩WGCNA (relaxed):", length(common_deg_wgcna), "\n")
}
if (length(common_deg_wgcna) < 3L) stop("Too few DEG∩WGCNA genes")

utils::write.csv(
  data.frame(Gene = common_deg_wgcna),
  file.path(outdir, "common_genes_DEG_WGCNA.csv"),
  row.names = FALSE
)

# ---- ML ≥2 ----
ml_genes <- intersect(common_deg_wgcna, colnames(datExpr))
if (length(ml_genes) > 100L) {
  v2 <- apply(datExpr[, ml_genes, drop = FALSE], 2L, stats::var, na.rm = TRUE)
  ml_genes <- names(sort(v2, decreasing = TRUE))[seq_len(100L)]
  cat("  ML input capped to 100 genes\n")
}
X <- datExpr[, ml_genes, drop = FALSE]
y <- factor(as.numeric(sample_info$Condition == "Disease"))
cat("ML on", ncol(X), "genes...\n")

gene_lists <- list()
cv_fit <- tryCatch(glmnet::cv.glmnet(as.matrix(X), y, family = "binomial", alpha = 1), error = function(e) NULL)
if (!is.null(cv_fit)) {
  cf <- as.matrix(stats::coef(cv_fit, s = "lambda.min"))
  gene_lists$LASSO <- setdiff(rownames(cf)[cf[, 1] != 0], "(Intercept)")
  cat("  LASSO:", length(gene_lists$LASSO), "\n")
}
rf <- randomForest::randomForest(x = as.matrix(X), y = y, ntree = 500, importance = TRUE)
imp <- randomForest::importance(rf)
imp_col <- if ("MeanDecreaseGini" %in% colnames(imp)) "MeanDecreaseGini" else colnames(imp)[1]
ord <- order(imp[, imp_col], decreasing = TRUE)
gene_lists$RF <- rownames(imp)[ord[seq_len(min(50L, length(ord)))]]
cat("  RF:", length(gene_lists$RF), "\n")
gene_lists$SVM <- head(svm_rank_genes(X, y, max_rfe_genes = 50L), 20L)
cat("  SVM:", length(gene_lists$SVM), "\n")

freq <- table(unlist(gene_lists, use.names = FALSE))
ml_final <- names(freq)[freq >= 2L]
cat("  ≥2 methods:", length(ml_final), "\n")
if (length(ml_final) < 3L) {
  ml_final <- head(gene_lists$RF %||% ml_genes, 15L)
  cat("  Fallback RF top-15\n")
}

utils::write.csv(data.frame(Gene = ml_final), file.path(outdir, "final_list_common_genes_ML.csv"), row.names = FALSE)
utils::write.csv(data.frame(Gene = ml_final), file.path(outdir, "consensus_signature_genes.csv"), row.names = FALSE)

# ---- ROC ----
cat("ROC...\n")
y_tr <- as.numeric(sample_info$Condition == "Disease")
auc_int <- vapply(ml_final, function(g) {
  if (!g %in% colnames(datExpr)) return(NA_real_)
  gene_auc(y_tr, datExpr[, g])
}, numeric(1))

dat_va <- t(expr_va)
y_va <- as.numeric(meta_val$Condition[match(rownames(dat_va), rownames(meta_val))] == "Disease")
auc_ext <- vapply(ml_final, function(g) {
  if (!g %in% colnames(dat_va)) return(NA_real_)
  gene_auc(y_va, dat_va[, g])
}, numeric(1))

roc_df <- data.frame(
  Gene = ml_final,
  AUC_Internal = as.numeric(auc_int),
  AUC_External = as.numeric(auc_ext),
  stringsAsFactors = FALSE
)
roc_df$Delta <- round(roc_df$AUC_External - roc_df$AUC_Internal, 4)
roc_df <- roc_df[order(-roc_df$AUC_External, na.last = TRUE), , drop = FALSE]
utils::write.csv(roc_df, file.path(outdir, "ROC_AUC_Training_vs_Validation.csv"), row.names = FALSE)

cat("\nWrote CSVs to", outdir, "\n")
cat("Panel size:", nrow(roc_df), "\n")
cat("Median Train AUC:", round(stats::median(roc_df$AUC_Internal, na.rm = TRUE), 3), "\n")
cat("Median External AUC:", round(stats::median(roc_df$AUC_External, na.rm = TRUE), 3), "\n")
cat("\nNext:\n")
cat("  python inst/scripts/make-supplementary-table-s2.py --repo", repo_root, "\n")
cat("  Rscript inst/scripts/benchmark-consensus-vs-standard.R --repo", repo_root, "\n")
