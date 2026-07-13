#!/usr/bin/env Rscript
## Original Paper — panel-level external AUC helpers
## Lives only under GExPipe(original_paper)/; does not modify Application Note code.
##
## Sourced by ablation-consensus-panels.R, or run alone after signatures exist:
##   Rscript scripts/panel-external-auc.R --gexpipe-repo E:/GExPipe

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

# This file's directory → original_paper root
ca_all <- commandArgs(trailingOnly = FALSE)
file_flag <- sub("^--file=", "", ca_all[grep("^--file=", ca_all)])
this_file <- if (length(file_flag)) {
  normalizePath(file_flag[1], winslash = "/", mustWork = FALSE)
} else {
  NA_character_
}
## When sourced from ablation-consensus-panels.R, ofile points at this helper
if (is.na(this_file) || !grepl("panel-external-auc\\.R$", this_file)) {
  of <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(of) && nzchar(of)) {
    this_file <- normalizePath(of, winslash = "/", mustWork = FALSE)
  }
}
if (is.na(this_file) || !nzchar(this_file)) this_file <- file.path(getwd(), "scripts", "panel-external-auc.R")
orig_root <- normalize_repo_path(dirname(dirname(this_file)))
if (!dir.exists(file.path(orig_root, "scripts"))) {
  orig_root <- normalize_repo_path(get_arg("--outdir", getwd()))
}

gexpipe_repo <- normalize_repo_path(get_arg("--gexpipe-repo", dirname(orig_root)))
outdir_vm <- file.path(gexpipe_repo, "validation_manual")
results_dir <- file.path(orig_root, "results")
sig_dir <- file.path(orig_root, "signatures")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(sig_dir, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(edgeR)
  library(limma)
  library(pROC)
  library(glmnet)
  library(randomForest)
  library(org.Hs.eg.db)
  library(AnnotationDbi)
})

gene_auc <- function(y, x) {
  y <- as.numeric(y)
  x <- as.numeric(x)
  ok <- is.finite(y) & is.finite(x)
  y <- y[ok]
  x <- x[ok]
  if (length(unique(y)) < 2L) return(NA_real_)
  roc <- tryCatch(pROC::roc(y, x, quiet = TRUE, direction = "auto"), error = function(e) NULL)
  if (is.null(roc)) return(NA_real_)
  as.numeric(pROC::auc(roc))
}

tmm_log <- function(counts) {
  y <- edgeR::DGEList(counts = counts)
  y <- edgeR::calcNormFactors(y)
  limma::normalizeBetweenArrays(
    as.matrix(edgeR::cpm(y, log = TRUE, prior.count = 1)),
    method = "quantile"
  )
}

entrez_to_symbol <- function(counts) {
  ids <- rownames(counts)
  if (mean(grepl("^[0-9]+$", head(ids, 200))) <= 0.8) return(counts)
  map <- AnnotationDbi::select(
    org.Hs.eg.db::org.Hs.eg.db,
    keys = ids, columns = "SYMBOL", keytype = "ENTREZID"
  )
  map <- map[!is.na(map$SYMBOL) & nzchar(map$SYMBOL) & !duplicated(map$ENTREZID), ]
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
  mat
}

read_gene_list <- function(path) {
  if (!file.exists(path)) return(character())
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  col <- if ("Gene" %in% names(df)) "Gene" else names(df)[1L]
  genes <- unique(trimws(as.character(df[[col]])))
  genes[nzchar(genes) & !grepl("^#", genes)]
}

top_n_by_adjp <- function(path, n = 50L) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!"adj.P.Val" %in% names(df) || !"Gene" %in% names(df)) {
    stop("Expected Gene + adj.P.Val in ", path)
  }
  df <- df[order(df$adj.P.Val), , drop = FALSE]
  head(unique(as.character(df$Gene)), n)
}

load_train_val <- function(vm_dir) {
  counts_tr <- as.matrix(utils::read.csv(
    file.path(vm_dir, "competitor_benchmark/upload_pack/GSE50760_counts.csv"),
    row.names = 1, check.names = FALSE
  ))
  pheno <- utils::read.csv(
    file.path(vm_dir, "competitor_benchmark/upload_pack/GSE50760_phenotype.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  id_col <- if ("Sample" %in% names(pheno)) "Sample" else if ("SampleID" %in% names(pheno)) "SampleID" else names(pheno)[1]
  cond_col <- if ("Condition" %in% names(pheno)) "Condition" else names(pheno)[2]
  meta_tr <- data.frame(
    Condition = as.character(pheno[[cond_col]]),
    row.names = as.character(pheno[[id_col]]),
    stringsAsFactors = FALSE
  )
  meta_tr$Condition[grepl("primary|tumor|disease|cancer", tolower(meta_tr$Condition))] <- "Disease"
  meta_tr$Condition[grepl("normal|nontumor|control", tolower(meta_tr$Condition))] <- "Normal"
  meta_tr <- meta_tr[meta_tr$Condition %in% c("Normal", "Disease"), , drop = FALSE]
  common <- intersect(colnames(counts_tr), rownames(meta_tr))
  counts_tr <- counts_tr[, common, drop = FALSE]
  meta_tr <- meta_tr[common, , drop = FALSE]
  y_tr <- as.numeric(meta_tr$Condition == "Disease")
  expr_tr <- tmm_log(counts_tr)

  rds <- file.path(vm_dir, "work/rna_data/GSE104836_parsed.rds")
  if (!file.exists(rds)) stop("Missing validation RDS: ", rds)
  val <- readRDS(rds)
  counts_va <- if (!is.null(val$counts_symbol)) val$counts_symbol else entrez_to_symbol(val$counts)
  pd <- as.data.frame(val$metadata, stringsAsFactors = FALSE)
  txt <- apply(pd, 1, function(r) paste(tolower(as.character(r)), collapse = " "))
  cond <- rep(NA_character_, length(txt))
  cond[grepl("nontumor|non-tumor", txt)] <- "Normal"
  cond[grepl("colon cancer|cancer tissue|\\bc\\b|_c$", txt)] <- "Disease"
  pd$Condition <- cond
  pd <- pd[!is.na(pd$Condition), , drop = FALSE]
  ov <- intersect(colnames(counts_va), rownames(pd))
  if (!length(ov) && ncol(counts_va) == nrow(pd)) {
    colnames(counts_va) <- rownames(pd)
    ov <- rownames(pd)
  }
  counts_va <- counts_va[, ov, drop = FALSE]
  pd <- pd[ov, , drop = FALSE]
  y_va <- as.numeric(pd$Condition == "Disease")
  expr_va <- tmm_log(counts_va)

  list(
    dat_tr = t(expr_tr),
    dat_va = t(expr_va),
    y_tr = y_tr,
    y_va = y_va,
    n_train = length(y_tr),
    n_val = length(y_va)
  )
}

align_panel_matrix <- function(dat, genes) {
  genes <- intersect(genes, colnames(dat))
  if (!length(genes)) return(NULL)
  as.matrix(dat[, genes, drop = FALSE])
}

panel_auc_glmnet <- function(X_tr, y_tr, X_va, y_va, seed = 42L) {
  if (is.null(X_tr) || is.null(X_va) || ncol(X_tr) < 1L) {
    return(list(auc = NA_real_, auprc = NA_real_, n_genes_used = 0L, model = "glmnet"))
  }
  genes <- intersect(colnames(X_tr), colnames(X_va))
  if (!length(genes)) {
    return(list(auc = NA_real_, auprc = NA_real_, n_genes_used = 0L, model = "glmnet"))
  }
  X_tr <- X_tr[, genes, drop = FALSE]
  X_va <- X_va[, genes, drop = FALSE]
  set.seed(seed)
  cv <- tryCatch(
    glmnet::cv.glmnet(X_tr, y_tr, family = "binomial", alpha = 1, nfolds = min(5L, length(y_tr))),
    error = function(e) NULL
  )
  if (is.null(cv)) {
    return(list(auc = NA_real_, auprc = NA_real_, n_genes_used = length(genes), model = "glmnet"))
  }
  pr <- as.numeric(stats::predict(cv, newx = X_va, s = "lambda.min", type = "response"))
  auc <- gene_auc(y_va, pr)
  ## AUPRC via PR curve if pROC supports; else manual
  auprc <- tryCatch({
    if (requireNamespace("PRROC", quietly = TRUE)) {
      PRROC::pr.curve(scores.class0 = pr[y_va == 1], scores.class1 = pr[y_va == 0])$auc.integral
    } else {
      ## simple average precision fallback
      ord <- order(pr, decreasing = TRUE)
      y_o <- y_va[ord]
      tp <- cumsum(y_o == 1)
      fp <- cumsum(y_o == 0)
      prec <- tp / pmax(tp + fp, 1)
      rec <- tp / max(sum(y_va == 1), 1)
      sum(diff(c(0, rec)) * prec)
    }
  }, error = function(e) NA_real_)
  list(auc = auc, auprc = auprc, n_genes_used = length(genes), model = "glmnet_lasso")
}

panel_auc_rf <- function(X_tr, y_tr, X_va, y_va, seed = 42L) {
  if (is.null(X_tr) || is.null(X_va) || ncol(X_tr) < 1L) {
    return(list(auc = NA_real_, score = numeric(), n_genes_used = 0L, model = "rf"))
  }
  genes <- intersect(colnames(X_tr), colnames(X_va))
  if (!length(genes)) {
    return(list(auc = NA_real_, score = numeric(), n_genes_used = 0L, model = "rf"))
  }
  X_tr <- X_tr[, genes, drop = FALSE]
  X_va <- X_va[, genes, drop = FALSE]
  set.seed(seed)
  yf <- factor(y_tr, levels = c(0, 1))
  rf <- tryCatch(
    randomForest::randomForest(x = X_tr, y = yf, ntree = 500),
    error = function(e) NULL
  )
  if (is.null(rf)) {
    return(list(auc = NA_real_, score = numeric(), n_genes_used = length(genes), model = "rf"))
  }
  pr <- tryCatch({
    p <- stats::predict(rf, newdata = X_va, type = "prob")
    if ("1" %in% colnames(p)) as.numeric(p[, "1"]) else as.numeric(p[, 2])
  }, error = function(e) rep(NA_real_, nrow(X_va)))
  list(auc = gene_auc(y_va, pr), score = pr, n_genes_used = length(genes), model = "rf")
}

## Nested CV on train only (optimism check; never uses external labels)
nested_cv_auc <- function(X, y, method = c("rf", "glmnet"), seed = 42L, nfolds = 5L) {
  method <- match.arg(method)
  X <- as.matrix(X)
  y <- as.numeric(y)
  if (nrow(X) < 6L || length(unique(y)) < 2L || ncol(X) < 1L) return(NA_real_)
  set.seed(seed)
  ## stratified-ish folds
  idx0 <- which(y == 0L)
  idx1 <- which(y == 1L)
  fold0 <- sample(rep(seq_len(nfolds), length.out = length(idx0)))
  fold1 <- sample(rep(seq_len(nfolds), length.out = length(idx1)))
  fold <- integer(length(y))
  fold[idx0] <- fold0
  fold[idx1] <- fold1
  score <- rep(NA_real_, length(y))
  for (f in seq_len(nfolds)) {
    te <- which(fold == f)
    tr <- which(fold != f)
    if (length(unique(y[tr])) < 2L || length(te) < 1L) next
    if (method == "rf") {
      yf <- factor(y[tr], levels = c(0, 1))
      rf <- tryCatch(
        randomForest::randomForest(x = X[tr, , drop = FALSE], y = yf, ntree = 300),
        error = function(e) NULL
      )
      if (is.null(rf)) next
      p <- stats::predict(rf, newdata = X[te, , drop = FALSE], type = "prob")
      score[te] <- if ("1" %in% colnames(p)) as.numeric(p[, "1"]) else as.numeric(p[, 2])
    } else {
      cv <- tryCatch(
        glmnet::cv.glmnet(
          X[tr, , drop = FALSE], y[tr], family = "binomial", alpha = 1,
          nfolds = min(5L, length(tr))
        ),
        error = function(e) NULL
      )
      if (is.null(cv)) next
      score[te] <- as.numeric(stats::predict(
        cv, newx = X[te, , drop = FALSE], s = "lambda.min", type = "response"
      ))
    }
  }
  gene_auc(y, score)
}

## ML ≥2 selectors on a DE gene universe (no WGCNA) — for true B4
select_ml_ge2 <- function(dat_tr, y_tr, candidate_genes, seed = 42L,
                          max_input = 200L, rf_top = 50L, svm_top = 20L) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  genes <- intersect(candidate_genes, colnames(dat_tr))
  if (length(genes) < 3L) return(character())
  ## Cap by train variance among candidates
  if (length(genes) > max_input) {
    v <- apply(dat_tr[, genes, drop = FALSE], 2L, stats::var, na.rm = TRUE)
    genes <- names(sort(v, decreasing = TRUE))[seq_len(max_input)]
  }
  X <- as.matrix(dat_tr[, genes, drop = FALSE])
  yf <- factor(as.numeric(y_tr), levels = c(0, 1))
  set.seed(seed)
  gene_lists <- list()

  cv_fit <- tryCatch(
    glmnet::cv.glmnet(X, yf, family = "binomial", alpha = 1),
    error = function(e) NULL
  )
  if (!is.null(cv_fit)) {
    cf <- as.matrix(stats::coef(cv_fit, s = "lambda.min"))
    gene_lists$LASSO <- setdiff(rownames(cf)[cf[, 1] != 0], "(Intercept)")
  }

  rf <- tryCatch(
    randomForest::randomForest(x = X, y = yf, ntree = 500, importance = TRUE),
    error = function(e) NULL
  )
  if (!is.null(rf)) {
    imp <- randomForest::importance(rf)
    imp_col <- if ("MeanDecreaseGini" %in% colnames(imp)) "MeanDecreaseGini" else colnames(imp)[1]
    ord <- order(imp[, imp_col], decreasing = TRUE)
    gene_lists$RF <- rownames(imp)[ord[seq_len(min(rf_top, length(ord)))]]
  }

  ## SVM-RFE if available; else |cor| ranking
  svm_rank <- function(X, y, max_rfe_genes = 50L) {
    genes <- colnames(X)
    use_rfe <- ncol(X) <= max_rfe_genes &&
      requireNamespace("kernlab", quietly = TRUE) &&
      requireNamespace("caret", quietly = TRUE)
    if (use_rfe) {
      ranked <- character()
      remaining <- seq_len(ncol(X))
      while (length(remaining) > 1L) {
        folds <- caret::createFolds(y, k = min(5L, length(y)))
        scores <- rep(0, length(remaining))
        for (fold in folds) {
          model <- tryCatch(
            kernlab::ksvm(
              X[-fold, remaining, drop = FALSE], y[-fold],
              kernel = "vanilladot", C = 1, scaled = FALSE
            ),
            error = function(e) NULL
          )
          if (is.null(model) || is.null(model@coef) || !length(model@coef[[1]])) next
          w <- tryCatch(
            t(model@coef[[1]]) %*% model@xmatrix[[1]],
            error = function(e) rep(0, length(remaining))
          )
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
  gene_lists$SVM <- head(svm_rank(X, yf, max_rfe_genes = 50L), svm_top)

  freq <- table(unlist(gene_lists, use.names = FALSE))
  ml_final <- names(freq)[freq >= 2L]
  if (length(ml_final) < 3L) {
    ml_final <- head(gene_lists$RF %||% genes, 15L)
  }
  list(
    genes = ml_final,
    per_method = gene_lists,
    n_input = length(genes)
  )
}

bootstrap_auc_ci <- function(y, score, n_boot = 1000L, seed = 42L) {
  ok <- is.finite(y) & is.finite(score)
  y <- y[ok]
  score <- score[ok]
  if (length(unique(y)) < 2L) return(c(lo = NA_real_, hi = NA_real_))
  set.seed(seed)
  n <- length(y)
  aucs <- replicate(n_boot, {
    ii <- sample.int(n, n, replace = TRUE)
    if (length(unique(y[ii])) < 2L) return(NA_real_)
    gene_auc(y[ii], score[ii])
  })
  qs <- stats::quantile(aucs, probs = c(0.025, 0.975), na.rm = TRUE)
  c(lo = unname(qs[1]), hi = unname(qs[2]))
}

evaluate_signature <- function(name, genes, data, seed = 42L, n_boot = 500L,
                                do_nested = TRUE) {
  genes <- unique(as.character(genes))
  genes <- genes[nzchar(genes)]
  X_tr <- align_panel_matrix(data$dat_tr, genes)
  X_va <- align_panel_matrix(data$dat_va, genes)
  used <- if (is.null(X_tr) || is.null(X_va)) character() else intersect(colnames(X_tr), colnames(X_va))

  ## secondary: median single-gene external AUC
  gene_ext <- vapply(used, function(g) gene_auc(data$y_va, data$dat_va[, g]), numeric(1))
  gene_tr <- vapply(used, function(g) gene_auc(data$y_tr, data$dat_tr[, g]), numeric(1))

  Xtr_u <- if (length(used)) as.matrix(data$dat_tr[, used, drop = FALSE]) else NULL
  Xva_u <- if (length(used)) as.matrix(data$dat_va[, used, drop = FALSE]) else NULL

  ## Primary for small external n: RF; glmnet kept as secondary
  rf <- panel_auc_rf(Xtr_u, data$y_tr, Xva_u, data$y_va, seed = seed)
  gnet <- panel_auc_glmnet(Xtr_u, data$y_tr, Xva_u, data$y_va, seed = seed)

  ci_rf <- if (length(rf$score)) {
    bootstrap_auc_ci(data$y_va, rf$score, n_boot = n_boot, seed = seed)
  } else {
    c(lo = NA_real_, hi = NA_real_)
  }

  ci_glm <- c(lo = NA_real_, hi = NA_real_)
  if (length(used) >= 1L) {
    set.seed(seed)
    cv <- tryCatch(
      glmnet::cv.glmnet(
        Xtr_u, data$y_tr, family = "binomial", alpha = 1,
        nfolds = min(5L, length(data$y_tr))
      ),
      error = function(e) NULL
    )
    if (!is.null(cv)) {
      pr <- as.numeric(stats::predict(cv, newx = Xva_u, s = "lambda.min", type = "response"))
      ci_glm <- bootstrap_auc_ci(data$y_va, pr, n_boot = n_boot, seed = seed)
    }
  }

  nested_rf <- if (do_nested && length(used) >= 1L) {
    nested_cv_auc(Xtr_u, data$y_tr, method = "rf", seed = seed)
  } else {
    NA_real_
  }
  nested_glm <- if (do_nested && length(used) >= 1L) {
    nested_cv_auc(Xtr_u, data$y_tr, method = "glmnet", seed = seed)
  } else {
    NA_real_
  }

  data.frame(
    Signature = name,
    Panel_size_requested = length(genes),
    Panel_size_used = length(used),
    Median_gene_AUC_train = stats::median(gene_tr, na.rm = TRUE),
    Median_gene_AUC_external = stats::median(gene_ext, na.rm = TRUE),
    ## Primary endpoint (small-n robust)
    Panel_AUC_RF_external = rf$auc,
    Panel_AUC_RF_CI95_lo = ci_rf[["lo"]],
    Panel_AUC_RF_CI95_hi = ci_rf[["hi"]],
    NestedCV_AUC_RF_train = nested_rf,
    ## Secondary
    Panel_AUC_glmnet_external = gnet$auc,
    Panel_AUPRC_glmnet_external = gnet$auprc,
    Panel_AUC_glmnet_CI95_lo = ci_glm[["lo"]],
    Panel_AUC_glmnet_CI95_hi = ci_glm[["hi"]],
    NestedCV_AUC_glmnet_train = nested_glm,
    stringsAsFactors = FALSE
  )
}

## If executed directly (not sourced with ORIGINAL_PAPER_HELPERS_ONLY), evaluate signatures
.run_panel_auc_main <- !exists("ORIGINAL_PAPER_HELPERS_ONLY") || !isTRUE(ORIGINAL_PAPER_HELPERS_ONLY)
if (.run_panel_auc_main) {
  is_main <- length(grep("panel-external-auc\\.R", commandArgs(trailingOnly = FALSE))) > 0L
  if (is_main || !interactive()) {
    cat("Loading train/val from", outdir_vm, "\n")
    data <- load_train_val(outdir_vm)
    cat("Train n=", data$n_train, " Val n=", data$n_val, "\n", sep = "")
    sig_files <- sort(list.files(sig_dir, pattern = "^B[0-9].*\\.csv$", full.names = TRUE))
    if (!length(sig_files)) {
      stop("No signatures in ", sig_dir, " — run ablation-consensus-panels.R first")
    }
    rows <- lapply(sig_files, function(f) {
      nm <- sub("\\.csv$", "", basename(f))
      cat("Evaluating", nm, "...\n")
      evaluate_signature(nm, read_gene_list(f), data)
    })
    tab <- do.call(rbind, rows)
    out_csv <- file.path(results_dir, "ablation_panel_auc.csv")
    utils::write.csv(tab, out_csv, row.names = FALSE)
    cat("Wrote", out_csv, "\n")
  }
}
