## Helpers for mixed microarray + RNA-seq integration (platform covariates & diagnostics)

#' @importFrom limma is.fullrank
utils::globalVariables(c("."))

.gexpipe_align_metadata_to_expr <- function(expr, metadata) {
  samp <- colnames(expr)
  if (is.null(samp)) {
    stop("Expression matrix must have colnames (sample IDs) for metadata alignment.")
  }
  idx <- match(samp, rownames(metadata))
  if (any(is.na(idx)) && "SampleID" %in% colnames(metadata)) {
    idx <- match(samp, as.character(metadata$SampleID))
  }
  if (any(is.na(idx))) {
    missing <- samp[is.na(idx)]
    stop(
      "metadata not aligned to expression columns: missing sample(s): ",
      paste(head(missing, 10L), collapse = ", "),
      if (length(missing) > 10L) paste0(" (and ", length(missing) - 10L, " more)") else ""
    )
  }
  metadata <- metadata[idx, , drop = FALSE]
  rownames(metadata) <- samp
  metadata
}

#' Detect microarray + RNA-seq in the same analysis
#' @param metadata data.frame with optional `Platform` column.
#' @return logical
#' @examples
#' meta <- data.frame(Platform = c("Microarray", "RNAseq"))
#' gexpipe_has_mixed_platforms(meta)
#' @export
gexpipe_has_mixed_platforms <- function(metadata) {
  if (is.null(metadata) || !"Platform" %in% colnames(metadata)) return(FALSE)
  plats <- unique(as.character(metadata$Platform))
  plats <- plats[!is.na(plats) & nzchar(plats)]
  "Microarray" %in% plats && "RNAseq" %in% plats
}

#' Test whether Platform is not estimable alongside Dataset
#'
#' Returns TRUE when Platform is nested within Dataset (each GSE has one technology)
#' or when a tentative \code{~ Dataset + Platform (+ Condition)} design is rank-deficient.
#' @param metadata data.frame with `Dataset` and `Platform` columns.
#' @return logical
#' @examples
#' meta <- data.frame(
#'   Dataset = c("GSE1", "GSE1", "GSE2", "GSE2"),
#'   Platform = c("Microarray", "Microarray", "RNAseq", "RNAseq")
#' )
#' gexpipe_platform_dataset_confounded(meta)
#' @export
gexpipe_platform_dataset_confounded <- function(metadata) {
  if (!gexpipe_has_mixed_platforms(metadata)) return(FALSE)
  if (!all(c("Dataset", "Platform") %in% colnames(metadata))) return(FALSE)

  combos <- unique(metadata[, c("Dataset", "Platform"), drop = FALSE])
  n_ds <- length(unique(as.character(metadata$Dataset)))
  n_combo <- nrow(combos)

  # Each dataset maps to exactly one platform -> Platform is a function of Dataset
  if (n_combo == n_ds) return(TRUE)

  # Platform varies within at least one dataset - check estimability with Condition
  meta <- .gexpipe_factor_meta(metadata)
  design <- tryCatch(
    if ("Condition" %in% colnames(meta)) {
      stats::model.matrix(~ Dataset + Platform + Condition, data = meta)
    } else {
      stats::model.matrix(~ Dataset + Platform, data = meta)
    },
    error = function(e) NULL
  )
  if (is.null(design) || ncol(design) < 2L) return(FALSE)

  full_rank <- if (requireNamespace("limma", quietly = TRUE)) {
    limma::is.fullrank(design)
  } else {
    qr(design)$rank == ncol(design)
  }
  !full_rank
}

#' Summarise how Platform should enter batch/DE models
#' @param metadata sample metadata data.frame.
#' @return list with mixed_platforms, platform_dataset_confounded, include_platform_covariate
#' @examples
#' meta <- data.frame(
#'   Dataset = rep(c("GSE1", "GSE2"), each = 2),
#'   Platform = rep(c("Microarray", "RNAseq"), each = 2)
#' )
#' gexpipe_batch_covariate_info(meta)
#' @export
gexpipe_batch_covariate_info <- function(metadata) {
  mixed <- gexpipe_has_mixed_platforms(metadata)
  confounded <- if (mixed) gexpipe_platform_dataset_confounded(metadata) else FALSE
  list(
    mixed_platforms = mixed,
    platform_dataset_confounded = confounded,
    include_platform_covariate = isTRUE(mixed && !confounded)
  )
}

.gexpipe_factor_meta <- function(meta, cols = c("Condition", "Platform", "Dataset")) {
  if ("Condition" %in% cols && "Condition" %in% colnames(meta)) {
    meta$Condition <- factor(meta$Condition, levels = c("Normal", "Disease"))
  }
  if ("Platform" %in% cols && "Platform" %in% colnames(meta)) {
    meta$Platform <- factor(meta$Platform)
  }
  if ("Dataset" %in% cols && "Dataset" %in% colnames(meta)) {
    meta$Dataset <- factor(meta$Dataset)
  }
  meta
}

.gexpipe_de_formula_info <- function(meta) {
  info <- gexpipe_batch_covariate_info(meta)
  multi_ds <- "Dataset" %in% colnames(meta) &&
    length(unique(as.character(meta$Dataset))) > 1L

  if (multi_ds && info$include_platform_covariate) {
    list(
      formula_str = "~ Dataset + Platform + Condition",
      formula_desc = "~ Dataset + Platform + Condition",
      info = info,
      multi_ds = multi_ds
    )
  } else if (multi_ds) {
    desc <- "~ Dataset + Condition"
    if (info$mixed_platforms && info$platform_dataset_confounded) {
      desc <- paste0(
        desc,
        " (Platform confounded with Dataset - platform effect absorbed by Dataset)"
      )
    }
    list(formula_str = "~ Dataset + Condition", formula_desc = desc, info = info, multi_ds = multi_ds)
  } else if (info$include_platform_covariate) {
    list(
      formula_str = "~ Platform + Condition",
      formula_desc = "~ Platform + Condition",
      info = info,
      multi_ds = multi_ds
    )
  } else {
    list(
      formula_str = "~ Condition",
      formula_desc = "~ Condition",
      info = info,
      multi_ds = multi_ds
    )
  }
}

#' Build a model matrix for DE (limma / edgeR / voom)
#' @param metadata data.frame with Dataset, Platform, Condition columns.
#' @return list with design, coef_condition, formula_desc, info
#' @examples
#' meta <- data.frame(
#'   Dataset = rep(c("GSE1", "GSE2"), each = 3),
#'   Condition = rep(c("Normal", "Disease"), times = 3),
#'   row.names = paste0("S", 1:6),
#'   stringsAsFactors = FALSE
#' )
#' gexpipe_build_de_design(meta)
#' @export
gexpipe_build_de_design <- function(metadata) {
  meta <- .gexpipe_factor_meta(metadata)
  fi <- .gexpipe_de_formula_info(meta)
  design <- stats::model.matrix(stats::as.formula(fi$formula_str), data = meta)

  cond_cols <- grep("^Condition", colnames(design), value = TRUE)
  coef_condition <- if (length(cond_cols) > 0L) {
    which(colnames(design) == cond_cols[length(cond_cols)])
  } else {
    ncol(design)
  }

  list(
    design = design,
    coef_condition = coef_condition,
    formula_desc = fi$formula_desc,
    info = fi$info
  )
}

#' Build ComBat / removeBatchEffect model matrix (biology to preserve)
#' @param metadata sample metadata data.frame.
#' @return model matrix
#' @examples
#' meta <- data.frame(
#'   Condition = rep(c("Normal", "Disease"), each = 3),
#'   row.names = paste0("S", 1:6),
#'   stringsAsFactors = FALSE
#' )
#' gexpipe_build_batch_mod(meta)
#' @export
gexpipe_build_batch_mod <- function(metadata) {
  meta <- .gexpipe_factor_meta(metadata, cols = c("Condition", "Platform"))
  info <- gexpipe_batch_covariate_info(meta)
  if (info$include_platform_covariate) {
    stats::model.matrix(~ Platform + Condition, data = meta)
  } else {
    stats::model.matrix(~ Condition, data = meta)
  }
}

#' DESeq2 design formula for count-based DE
#' @param metadata sample metadata data.frame.
#' @return list with formula (formula object) and formula_desc (character)
#' @examples
#' meta <- data.frame(
#'   Condition = rep(c("Normal", "Disease"), each = 3),
#'   row.names = paste0("S", 1:6),
#'   stringsAsFactors = FALSE
#' )
#' gexpipe_deseq2_design(meta)
#' @export
gexpipe_deseq2_design <- function(metadata) {
  meta <- .gexpipe_factor_meta(metadata)
  fi <- .gexpipe_de_formula_info(meta)
  list(
    formula = stats::as.formula(fi$formula_str),
    formula_desc = fi$formula_desc
  )
}

#' Polar PCA coordinates for batch/platform diagnostic plots
#' @param expr genes x samples matrix.
#' @param metadata sample metadata aligned to expr columns.
#' @param color_by column name in metadata to colour points.
#' @return data.frame with theta, r, and colour column.
#' @examples
#' expr <- matrix(rnorm(120), 20, 6, dimnames = list(paste0("G", 1:20), paste0("S", 1:6)))
#' meta <- data.frame(Dataset = rep(c("A", "B"), each = 3), row.names = colnames(expr))
#' gexpipe_pca_polar_df(expr, meta)
#' @export
gexpipe_pca_polar_df <- function(expr, metadata, color_by = "Dataset") {
  metadata <- .gexpipe_align_metadata_to_expr(expr, metadata)
  pca <- stats::prcomp(t(expr), scale. = TRUE)
  pc1 <- pca$x[, 1]
  pc2 <- pca$x[, 2]
  theta <- atan2(pc2, pc1)
  r <- sqrt(pc1^2 + pc2^2)
  if (max(r) > 0) r <- r / max(r)
  df <- data.frame(theta = theta, r = r, stringsAsFactors = FALSE)
  if (color_by %in% colnames(metadata)) {
    df[[color_by]] <- metadata[[color_by]]
  }
  df
}

#' Simplified PVCA variance bar-chart data (PCA + per-factor R^2 on top PCs)
#'
#' Aligns metadata to expression columns, runs PCA on samples, and estimates
#' the mean fraction of variance in the top PCs explained by Dataset, Platform,
#' and Condition. Residual is \code{1 - sum(factor fractions)} (approximate).
#'
#' @param expr Numeric matrix, genes x samples.
#' @param metadata Sample metadata with \code{Dataset} and optional \code{Platform},
#'   \code{Condition}.
#' @param max_samples Subsample at most this many columns for speed (default 100).
#' @param max_pcs Use at most this many principal components (default 10).
#' @return A list with \code{ok} (logical), \code{data} (data.frame or \code{NULL}),
#'   and \code{message} (character, empty on success).
#' @examples
#' expr <- matrix(rnorm(120), 20, 6, dimnames = list(paste0("G", 1:20), paste0("S", 1:6)))
#' meta <- data.frame(
#'   Dataset = rep(c("GSE1", "GSE2"), each = 3),
#'   Condition = rep(c("Normal", "Disease"), times = 3),
#'   row.names = colnames(expr),
#'   stringsAsFactors = FALSE
#' )
#' gexpipe_pvca_df(expr, meta)
#' @export
gexpipe_pvca_df <- function(expr, metadata, max_samples = 100L, max_pcs = 10L) {
  if (is.null(expr) || is.null(metadata)) {
    return(list(ok = FALSE, data = NULL, message = "Expression or metadata missing."))
  }
  expr <- as.matrix(expr)
  storage.mode(expr) <- "double"
  if (nrow(expr) < 2L || ncol(expr) < 2L) {
    return(list(
      ok = FALSE,
      data = NULL,
      message = "Need at least 2 genes and 2 samples for PVCA."
    ))
  }
  metadata <- .gexpipe_align_metadata_to_expr(expr, metadata)

  gene_ok <- rowSums(is.finite(expr)) >= 2L
  expr <- expr[gene_ok, , drop = FALSE]
  if (nrow(expr) < 2L) {
    return(list(
      ok = FALSE,
      data = NULL,
      message = "Too few genes with finite values for PVCA."
    ))
  }

  n_samples <- min(as.integer(max_samples), ncol(expr))
  if (ncol(expr) > n_samples) {
    sample_idx <- seq_len(n_samples)
    expr <- expr[, sample_idx, drop = FALSE]
    metadata <- metadata[colnames(expr), , drop = FALSE]
  }

  pca <- tryCatch(stats::prcomp(t(expr), scale. = TRUE), error = function(e) NULL)
  if (is.null(pca) || is.null(pca$x) || ncol(pca$x) < 1L) {
    return(list(
      ok = FALSE,
      data = NULL,
      message = "PCA failed - expression may be constant or non-numeric."
    ))
  }

  cum_var <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
  n_pcs <- which(cum_var >= 0.8)[1]
  if (length(n_pcs) == 0L || is.na(n_pcs)) n_pcs <- ncol(pca$x)
  n_pcs <- max(1L, min(as.integer(max_pcs), as.integer(n_pcs)))
  pca_scores <- pca$x[, seq_len(n_pcs), drop = FALSE]

  .pvca_factor_var <- function(scores, factor_vec) {
    ok <- !is.na(factor_vec)
    if (sum(ok) < 2L || length(unique(factor_vec[ok])) < 2L) {
      return(NA_real_)
    }
    vals <- apply(scores[ok, , drop = FALSE], 2, function(pc) {
      fit <- tryCatch(stats::aov(pc ~ factor_vec[ok]), error = function(e) NULL)
      if (is.null(fit)) return(0)
      ss <- summary(fit)[[1]]$`Sum Sq`
      if (length(ss) < 1L || sum(ss, na.rm = TRUE) <= 0) return(0)
      ss[1] / sum(ss, na.rm = TRUE)
    })
    mean(vals, na.rm = TRUE)
  }

  pvca_results <- data.frame(
    Factor = character(),
    Variance = numeric(),
    stringsAsFactors = FALSE
  )

  if (length(unique(metadata$Dataset)) > 1L) {
    dataset_var <- .pvca_factor_var(pca_scores, metadata$Dataset)
    if (is.finite(dataset_var)) {
      pvca_results <- rbind(
        pvca_results,
        data.frame(Factor = "Dataset", Variance = dataset_var, stringsAsFactors = FALSE)
      )
    }
  }

  if (gexpipe_has_mixed_platforms(metadata)) {
    platform_var <- .pvca_factor_var(pca_scores, metadata$Platform)
    if (is.finite(platform_var)) {
      pvca_results <- rbind(
        pvca_results,
        data.frame(Factor = "Platform", Variance = platform_var, stringsAsFactors = FALSE)
      )
    }
  }

  if ("Condition" %in% colnames(metadata) &&
      !all(is.na(metadata$Condition)) &&
      length(unique(metadata$Condition[!is.na(metadata$Condition)])) > 1L) {
    condition_var <- .pvca_factor_var(pca_scores, metadata$Condition)
    if (is.finite(condition_var)) {
      pvca_results <- rbind(
        pvca_results,
        data.frame(Factor = "Condition", Variance = condition_var, stringsAsFactors = FALSE)
      )
    }
  }

  if (nrow(pvca_results) == 0L) {
    return(list(
      ok = FALSE,
      data = NULL,
      message = "No estimable batch or condition factors for PVCA."
    ))
  }

  residual_var <- max(0, 1 - sum(pvca_results$Variance))
  pvca_results <- rbind(
    pvca_results,
    data.frame(Factor = "Residual", Variance = residual_var, stringsAsFactors = FALSE)
  )
  pvca_levels <- c("Dataset", "Platform", "Condition", "Residual")
  present <- intersect(pvca_levels, unique(as.character(pvca_results$Factor)))
  pvca_results$Factor <- factor(pvca_results$Factor, levels = present)

  list(ok = TRUE, data = pvca_results, message = "")
}

#' Summarise Dataset x Condition confounding for batch/DE guidance
#' @param metadata data.frame with Dataset and Condition columns.
#' @return list with confounded (logical), table (matrix), message (character)
#' @examples
#' meta <- data.frame(
#'   Dataset = rep(c("GSE1", "GSE2"), each = 3),
#'   Condition = rep(c("Normal", "Disease"), times = 3)
#' )
#' gexpipe_batch_confounding_summary(meta)
#' @export
gexpipe_batch_confounding_summary <- function(metadata) {
  if (is.null(metadata) || !is.data.frame(metadata)) {
    return(list(confounded = FALSE, table = NULL, message = "No metadata available."))
  }
  if (!all(c("Dataset", "Condition") %in% colnames(metadata))) {
    return(list(confounded = FALSE, table = NULL, message = "Assign Dataset and Condition to assess confounding."))
  }
  meta <- metadata[
    !is.na(metadata$Condition) & metadata$Condition %in% c("Normal", "Disease"),
    , drop = FALSE
  ]
  if (nrow(meta) < 2L) {
    return(list(confounded = FALSE, table = NULL, message = "Label Normal/Disease samples in Step 4 first."))
  }
  tbl <- table(meta$Dataset, meta$Condition)
  confounded <- nrow(tbl) > 1L && ncol(tbl) > 1L && any(tbl == 0L)
  msg <- if (confounded) {
    "Dataset and Condition are confounded (empty cells in the table below). DE and batch correction may not separate batch from biology."
  } else {
    "Dataset and Condition are crossed - each dataset has both groups (no perfect confounding)."
  }
  list(confounded = confounded, table = tbl, message = msg)
}
