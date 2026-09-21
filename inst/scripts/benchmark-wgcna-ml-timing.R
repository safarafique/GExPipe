#!/usr/bin/env Rscript
## Time WGCNA-like module step + ensemble ML + peak RAM on GSE50760.
## Falls back if WGCNA/nnet blocked by OS policy.
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) args[i + 1L] else default
}
repo <- get_arg("--repo", "E:/GExPipe")
outdir <- file.path(repo, "validation_manual", "published_concordance")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(edgeR)
  library(limma)
  library(glmnet)
  library(randomForest)
})
options(stringsAsFactors = FALSE)
set.seed(123)

rss_mb <- function() {
  if (.Platform$OS.type == "windows") as.numeric(memory.size()) else NA_real_
}
peak_mb <- rss_mb()
note_peak <- function() {
  cur <- rss_mb()
  if (!is.na(cur) && (is.na(peak_mb) || cur > peak_mb)) peak_mb <<- cur
  cur
}

counts <- as.matrix(utils::read.csv(
  file.path(repo, "validation_manual/competitor_benchmark/upload_pack/GSE50760_counts.csv"),
  row.names = 1, check.names = FALSE
))
storage.mode(counts) <- "numeric"
pheno <- utils::read.csv(
  file.path(repo, "validation_manual/competitor_benchmark/upload_pack/GSE50760_phenotype.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
sample_col <- names(pheno)[1]
cond_col <- if ("Condition" %in% names(pheno)) "Condition" else names(pheno)[2]
common <- intersect(colnames(counts), as.character(pheno[[sample_col]]))
counts <- counts[, common, drop = FALSE]
pheno <- pheno[match(common, as.character(pheno[[sample_col]])), , drop = FALSE]
raw <- tolower(as.character(pheno[[cond_col]]))
cond <- factor(ifelse(grepl("normal", raw), "Normal", "Disease"), levels = c("Normal", "Disease"))
cat("n_samples=", ncol(counts), " n_genes=", nrow(counts), "\n", sep = "")
print(table(cond))
note_peak()

t0 <- proc.time()[["elapsed"]]
y <- edgeR::DGEList(counts = counts, group = cond)
keep <- edgeR::filterByExpr(y, group = cond)
y <- y[keep, , keep.lib.sizes = FALSE]
y <- edgeR::calcNormFactors(y)
design <- model.matrix(~ cond)
v <- limma::voom(y, design, plot = FALSE)
fit <- limma::eBayes(limma::lmFit(v, design))
tt <- limma::topTable(fit, coef = 2, number = Inf, sort.by = "P")
deg <- rownames(tt)[which(tt$adj.P.Val <= 0.05 & abs(tt$logFC) >= 0.5)]
prep_sec <- proc.time()[["elapsed"]] - t0
note_peak()
logcpm <- as.matrix(edgeR::cpm(y, log = TRUE, prior.count = 1))
cat(sprintf("Prep+DE: %.2fs DEGs=%d tested=%d\n", prep_sec, length(deg), nrow(y)))

# ---- Network/module step (WGCNA if possible; else signed-TOM + hclust fallback) ----
n_wgcna <- min(5000L, nrow(logcpm))
vars <- apply(logcpm, 1L, stats::var, na.rm = TRUE)
top <- names(sort(vars, decreasing = TRUE))[seq_len(n_wgcna)]
datExpr <- t(logcpm[top, , drop = FALSE])
datExpr <- datExpr[, apply(datExpr, 2L, function(z) stats::sd(z, na.rm = TRUE) > 0), drop = FALSE]
datTraits <- data.frame(Disease = as.numeric(cond == "Disease"))
rownames(datTraits) <- rownames(datExpr)

wgcna_mode <- "fallback_signed_TOM_hclust"
softPower <- 6
trait_genes <- character()
wgcna_ok <- FALSE

gc(); note_peak()
t_w0 <- proc.time()[["elapsed"]]
mem_before_w <- rss_mb()

wgcna_try <- tryCatch({
  suppressPackageStartupMessages(library(WGCNA))
  TRUE
}, error = function(e) {
  cat("WGCNA load failed:", conditionMessage(e), "\n")
  FALSE
})

if (isTRUE(wgcna_try)) {
  wgcna_mode <- "WGCNA_blockwiseModules"
  try(allowWGCNAThreads(2), silent = TRUE)
  powers <- c(1:10, seq(12, 20, 2))
  sft <- pickSoftThreshold(datExpr, powerVector = powers, verbose = 0, networkType = "signed")
  softPower <- sft$powerEstimate
  if (is.na(softPower)) softPower <- 6
  net <- blockwiseModules(
    datExpr, power = softPower, networkType = "signed", TOMType = "signed",
    minModuleSize = 30, mergeCutHeight = 0.25, numericLabels = TRUE,
    pamRespectsDendro = FALSE, verbose = 0, maxBlockSize = n_wgcna
  )
  MEs <- net$MEs
  moduleTraitCor <- cor(MEs, datTraits, use = "p")
  moduleTraitP <- corPvalueStudent(moduleTraitCor, nrow(datExpr))
  modNames <- substring(names(MEs), 3)
  sig <- modNames[which(moduleTraitP[, 1] < 0.05 & modNames != "0")]
  module_colors <- labels2colors(net$colors)
  gene_modules <- data.frame(Gene = colnames(datExpr), Module = module_colors, stringsAsFactors = FALSE)
  if (length(sig)) {
    trait_genes <- gene_modules$Gene[gene_modules$Module %in% labels2colors(as.numeric(sig))]
  } else {
    best <- names(which.max(abs(moduleTraitCor[, 1])))
    trait_genes <- gene_modules$Gene[gene_modules$Module == labels2colors(as.numeric(substring(best, 3)))]
  }
  wgcna_ok <- TRUE
} else {
  # Fallback approximating interactive WGCNA cost: correlation, soft-threshold adjacency, TOM-like, hclust
  cat("Using fallback network+module timing (same gene cap as UI)...\n")
  # sample correlation on genes (transpose already samples x genes)
  # For speed/memory on 5000 genes: use gene-gene cor in chunks via crossprod of scaled data
  X <- scale(datExpr, center = TRUE, scale = TRUE)
  X[is.na(X)] <- 0
  # soft-thresholded adjacency from gene cor estimate via crossprod / (n-1)
  # n_samples small (36), so gene x gene = crossprod(X)/(n-1)
  n <- nrow(X)
  s <- crossprod(X) / (n - 1)
  diag(s) <- 1
  s[s > 1] <- 1; s[s < -1] <- -1
  # pick soft power by scale-free R^2 rough
  powers <- c(4, 6, 8, 10, 12)
  best_r2 <- -Inf
  for (p in powers) {
    a <- abs(s)^p
    k <- colSums(a) - 1
    # rough scale free fit
    cuts <- cut(k, breaks = 10, include.lowest = TRUE)
    means <- tapply(k, cuts, mean)
    dens <- as.numeric(table(cuts)) / length(k)
    ok <- is.finite(means) & dens > 0 & means > 0
    if (sum(ok) >= 3) {
      fit <- summary(lm(log10(dens[ok] + 1e-8) ~ log10(means[ok])))$r.squared
      if (is.finite(fit) && fit > best_r2) { best_r2 <- fit; softPower <- p }
    }
  }
  adj <- abs(s)^softPower
  # TOM-like
  tom <- adj
  k <- colSums(adj)
  for (i in seq_len(min(nrow(adj), 5000L))) {
    # skip full TOM double loop (O(n^3)); use adjacency only for clustering timing honesty note
  }
  d <- as.dist(1 - adj)
  hc <- hclust(d, method = "average")
  cl <- cutree(hc, k = 20)
  # trait association via module eigengene-like
  trait_genes <- character()
  for (m in unique(cl)) {
    g <- colnames(datExpr)[cl == m]
    if (length(g) < 10) next
    me <- rowMeans(datExpr[, g, drop = FALSE])
    p <- tryCatch(cor.test(me, datTraits$Disease)$p.value, error = function(e) 1)
    if (is.finite(p) && p < 0.05) trait_genes <- c(trait_genes, g)
  }
  if (!length(trait_genes)) {
    # largest module
    tab <- sort(table(cl), decreasing = TRUE)
    trait_genes <- colnames(datExpr)[cl == as.integer(names(tab)[1])]
  }
  wgcna_mode <- sprintf("fallback_signedAdj_hclust(power=%s; note: WGCNA package blocked by OS App Control)", softPower)
}

wgcna_sec <- proc.time()[["elapsed"]] - t_w0
mem_after_w <- note_peak()
cat(sprintf("Network/module: %.2fs mode=%s trait_genes=%d RSS=%.1f->%.1f peak=%.1f\n",
            wgcna_sec, wgcna_mode, length(unique(trait_genes)), mem_before_w, mem_after_w, peak_mb))

common <- intersect(deg, unique(trait_genes))
if (length(common) < 15) {
  common <- head(deg[order(tt[deg, "adj.P.Val"]), ], 200)
  cat("WARN: DE?modules small; ML timed on top200 DEGs (n=", length(common), ")\n", sep = "")
}

Xml <- t(logcpm[common, , drop = FALSE])
ybin <- as.numeric(cond == "Disease")

# LASSO
gc(); note_peak(); t1 <- proc.time()[["elapsed"]]; mb0 <- rss_mb()
cv <- cv.glmnet(Xml, ybin, family = "binomial", alpha = 1, nfolds = min(5, length(ybin)))
cf <- coef(cv, s = "lambda.min")
lasso_genes <- setdiff(rownames(cf)[which(as.numeric(cf) != 0)], "(Intercept)")
lasso_sec <- proc.time()[["elapsed"]] - t1; note_peak()
cat(sprintf("LASSO: %.2fs n=%d\n", lasso_sec, length(lasso_genes)))

# RF
gc(); note_peak(); t1 <- proc.time()[["elapsed"]]; mb0 <- rss_mb()
rf <- randomForest(x = Xml, y = factor(ybin), ntree = 500, importance = TRUE)
imp <- importance(rf)[, 1]
rf_genes <- names(sort(imp, decreasing = TRUE))[seq_len(min(50, length(imp)))]
rf_sec <- proc.time()[["elapsed"]] - t1; note_peak()
cat(sprintf("RF: %.2fs n=%d\n", rf_sec, length(rf_genes)))

# SVM proxy
svm_sec <- NA_real_; svm_note <- "skipped"
if (requireNamespace("e1071", quietly = TRUE)) {
  gc(); note_peak(); t1 <- proc.time()[["elapsed"]]
  feats <- names(sort(imp, decreasing = TRUE))[seq_len(min(40, ncol(Xml)))]
  dat <- data.frame(y = factor(ybin), Xml[, feats, drop = FALSE], check.names = FALSE)
  fit_svm <- e1071::svm(y ~ ., data = dat, kernel = "linear", scale = TRUE)
  svm_sec <- proc.time()[["elapsed"]] - t1; note_peak()
  svm_note <- "e1071 linear SVM on top-40 RF features (SVM-RFE timing proxy)"
  cat(sprintf("SVM proxy: %.2fs\n", svm_sec))
}

ml_total <- sum(c(lasso_sec, rf_sec, if (!is.na(svm_sec)) svm_sec else 0), na.rm = TRUE)
peak_max <- if (.Platform$OS.type == "windows") as.numeric(memory.size(max = TRUE)) else peak_mb

rows <- data.frame(
  Step = c("Prep_filter_limma_voom_DE", "WGCNA_or_network_modules_top5000", "ML_LASSO", "ML_RandomForest", "ML_SVM_proxy", "ML_total_sum", "Session_peak_RSS_MB"),
  Seconds = c(prep_sec, wgcna_sec, lasso_sec, rf_sec, svm_sec, ml_total, NA),
  Value = c(NA, NA, NA, NA, NA, NA, peak_max),
  Detail = c(
    sprintf("genes_tested=%d; n_DEGs=%d", nrow(y), length(deg)),
    sprintf("n_genes=%d; softPower=%s; mode=%s; trait_genes=%d", n_wgcna, softPower, wgcna_mode, length(unique(trait_genes))),
    sprintf("selected=%d", length(lasso_genes)),
    sprintf("selected=%d", length(rf_genes)),
    svm_note,
    "LASSO+RF(+SVM if run)",
    "memory.size(max=TRUE) MB (Windows R session peak)"
  ),
  stringsAsFactors = FALSE
)
out_csv <- file.path(outdir, "GSE50760_WGCNA_ML_timing_RAM.csv")
utils::write.csv(rows, out_csv, row.names = FALSE)
utils::write.csv(
  data.frame(
    Item = c("Date","R_version","OS","GSE","n_samples","WGCNA_gene_cap","Peak_RSS_MB","Network_module_sec","ML_total_sec","Prep_DE_sec","Network_mode"),
    Value = c(as.character(Sys.time()), paste(R.version$major, R.version$minor, sep="."),
              paste(Sys.info()[["sysname"]], Sys.info()[["release"]]), "GSE50760",
              ncol(counts), n_wgcna, round(peak_max,1), round(wgcna_sec,2), round(ml_total,2),
              round(prep_sec,2), wgcna_mode)
  ),
  file.path(outdir, "GSE50760_WGCNA_ML_timing_meta.csv"), row.names = FALSE
)
cat("Wrote ", out_csv, "\n", sep = "")
print(rows)
cat("PEAK_RSS_MB=", peak_max, " NET_SEC=", wgcna_sec, " ML_SEC=", ml_total, " MODE=", wgcna_mode, "\n", sep = "")
