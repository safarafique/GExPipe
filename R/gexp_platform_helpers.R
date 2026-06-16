## Helpers for mixed microarray + RNA-seq integration (platform covariates & diagnostics)

utils::globalVariables(c("."))

#' Detect microarray + RNA-seq in the same analysis
#' @param metadata data.frame with optional `Platform` column.
#' @return logical
#' @export
gexpipe_has_mixed_platforms <- function(metadata) {
  if (is.null(metadata) || !"Platform" %in% colnames(metadata)) return(FALSE)
  plats <- unique(as.character(metadata$Platform))
  plats <- plats[!is.na(plats) & nzchar(plats)]
  "Microarray" %in% plats && "RNAseq" %in% plats
}

#' Test whether Platform and Dataset are perfectly confounded (1:1 mapping)
#' @param metadata data.frame with `Dataset` and `Platform` columns.
#' @return logical
#' @export
gexpipe_platform_dataset_confounded <- function(metadata) {
  if (!gexpipe_has_mixed_platforms(metadata)) return(FALSE)
  if (!all(c("Dataset", "Platform") %in% colnames(metadata))) return(FALSE)
  n_ds <- length(unique(as.character(metadata$Dataset)))
  n_plat <- length(unique(as.character(metadata$Platform)))
  n_combo <- nrow(unique(metadata[, c("Dataset", "Platform"), drop = FALSE]))
  isTRUE(n_combo == n_ds && n_ds == n_plat)
}

#' Summarise how Platform should enter batch/DE models
#' @param metadata sample metadata data.frame.
#' @return list with mixed_platforms, platform_dataset_confounded, include_platform_covariate
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

#' Build a model matrix for DE (limma / edgeR / voom)
#' @param metadata data.frame with Dataset, Platform, Condition columns.
#' @return list with design, coef_condition, formula_desc, info
#' @export
gexpipe_build_de_design <- function(metadata) {
  meta <- metadata
  if ("Condition" %in% colnames(meta)) {
    meta$Condition <- factor(meta$Condition, levels = c("Normal", "Disease"))
  }
  if ("Platform" %in% colnames(meta)) {
    meta$Platform <- factor(meta$Platform)
  }
  if ("Dataset" %in% colnames(meta)) {
    meta$Dataset <- factor(meta$Dataset)
  }

  info <- gexpipe_batch_covariate_info(meta)
  multi_ds <- length(unique(as.character(meta$Dataset))) > 1L

  if (multi_ds && info$include_platform_covariate) {
    design <- stats::model.matrix(~ Dataset + Platform + Condition, data = meta)
    formula_desc <- "~ Dataset + Platform + Condition"
  } else if (multi_ds) {
    design <- stats::model.matrix(~ Dataset + Condition, data = meta)
    formula_desc <- "~ Dataset + Condition"
    if (info$mixed_platforms && info$platform_dataset_confounded) {
      formula_desc <- paste0(
        formula_desc,
        " (Platform confounded with Dataset — platform effect absorbed by Dataset)"
      )
    }
  } else if (info$include_platform_covariate) {
    design <- stats::model.matrix(~ Platform + Condition, data = meta)
    formula_desc <- "~ Platform + Condition"
  } else {
    design <- stats::model.matrix(~ Condition, data = meta)
    formula_desc <- "~ Condition"
  }

  cond_cols <- grep("^Condition", colnames(design), value = TRUE)
  coef_condition <- if (length(cond_cols) > 0L) {
    which(colnames(design) == cond_cols[length(cond_cols)])
  } else {
    ncol(design)
  }

  list(
    design = design,
    coef_condition = coef_condition,
    formula_desc = formula_desc,
    info = info
  )
}

#' Build ComBat / removeBatchEffect model matrix (biology to preserve)
#' @param metadata sample metadata data.frame.
#' @return model matrix
#' @export
gexpipe_build_batch_mod <- function(metadata) {
  meta <- metadata
  if ("Condition" %in% colnames(meta)) {
    meta$Condition <- factor(meta$Condition, levels = c("Normal", "Disease"))
  }
  if ("Platform" %in% colnames(meta)) {
    meta$Platform <- factor(meta$Platform)
  }
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
#' @export
gexpipe_deseq2_design <- function(metadata) {
  meta <- metadata
  if ("Condition" %in% colnames(meta)) {
    meta$Condition <- factor(meta$Condition, levels = c("Normal", "Disease"))
  }
  if ("Platform" %in% colnames(meta)) {
    meta$Platform <- factor(meta$Platform)
  }
  if ("Dataset" %in% colnames(meta)) {
    meta$Dataset <- factor(meta$Dataset)
  }

  info <- gexpipe_batch_covariate_info(meta)
  multi_ds <- length(unique(as.character(meta$Dataset))) > 1L

  if (multi_ds && info$include_platform_covariate) {
    list(formula = stats::as.formula("~ Dataset + Platform + Condition"),
         formula_desc = "~ Dataset + Platform + Condition")
  } else if (multi_ds) {
    desc <- "~ Dataset + Condition"
    if (info$mixed_platforms && info$platform_dataset_confounded) {
      desc <- paste0(desc, " (Platform confounded with Dataset)")
    }
    list(formula = stats::as.formula("~ Dataset + Condition"), formula_desc = desc)
  } else if (info$include_platform_covariate) {
    list(formula = stats::as.formula("~ Platform + Condition"),
         formula_desc = "~ Platform + Condition")
  } else {
    list(formula = stats::as.formula("~ Condition"), formula_desc = "~ Condition")
  }
}

#' Polar PCA coordinates for batch/platform diagnostic plots
#' @param expr genes x samples matrix.
#' @param metadata sample metadata aligned to expr columns.
#' @param color_by column name in metadata to colour points.
#' @return data.frame with theta, r, and colour column.
#' @export
gexpipe_pca_polar_df <- function(expr, metadata, color_by = "Dataset") {
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
