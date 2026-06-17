test_that("gexpipe_has_mixed_platforms detects microarray + RNA-seq", {
  meta <- data.frame(
    Platform = c("Microarray", "Microarray", "RNAseq", "RNAseq"),
    stringsAsFactors = FALSE
  )
  expect_true(gexpipe_has_mixed_platforms(meta))
  meta2 <- data.frame(Platform = c("RNAseq", "RNAseq"), stringsAsFactors = FALSE)
  expect_false(gexpipe_has_mixed_platforms(meta2))
})

test_that("gexpipe_platform_dataset_confounded detects nested platform in dataset", {
  meta <- data.frame(
    Dataset = c("GSE1", "GSE1", "GSE2", "GSE2"),
    Platform = c("Microarray", "Microarray", "RNAseq", "RNAseq"),
    Condition = rep(c("Normal", "Disease"), 2),
    stringsAsFactors = FALSE
  )
  expect_true(gexpipe_platform_dataset_confounded(meta))
  info <- gexpipe_batch_covariate_info(meta)
  expect_false(info$include_platform_covariate)
})

test_that("gexpipe_build_de_design omits Platform when nested in Dataset", {
  meta <- data.frame(
    Dataset = factor(c("GSE1", "GSE1", "GSE2", "GSE2")),
    Platform = factor(c("Microarray", "Microarray", "RNAseq", "RNAseq")),
    Condition = factor(c("Normal", "Disease", "Normal", "Disease"),
                       levels = c("Normal", "Disease")),
    row.names = paste0("S", 1:4),
    stringsAsFactors = FALSE
  )
  out <- gexpipe_build_de_design(meta)
  expect_false(any(grepl("Platform", colnames(out$design), fixed = TRUE)))
  expect_match(out$formula_desc, "Dataset")
})

test_that("gexpipe_pca_polar_df aligns metadata to expression column order", {
  expr <- matrix(1:12, nrow = 3, ncol = 4)
  rownames(expr) <- c("G1", "G2", "G3")
  colnames(expr) <- c("S2", "S1", "S4", "S3")
  meta <- data.frame(
    Dataset = c("D1", "D1", "D2", "D2"),
    Condition = c("Normal", "Disease", "Normal", "Disease"),
    row.names = c("S1", "S2", "S3", "S4"),
    stringsAsFactors = FALSE
  )
  df <- gexpipe_pca_polar_df(expr, meta, "Dataset")
  expect_equal(nrow(df), ncol(expr))
  expect_equal(df$Dataset, c("D1", "D1", "D2", "D2"))
})

test_that("gexpipe_independent_filter reduces low-expression genes", {
  expr <- gexpipe_test_expr(100L, 10L)
  meta <- gexpipe_test_metadata(colnames(expr))
  design <- gexpipe_build_de_design(meta)$design
  filt <- gexpipe_independent_filter(expr, design = design)
  expect_lte(filt$n_after, filt$n_before)
  expect_equal(nrow(filt$expr), filt$n_after)
})

test_that("gexpipe_wgcna_heatmap_cor drops redundant combined column", {
  cor_mat <- matrix(runif(6), nrow = 2,
                    dimnames = list(c("M1", "M2"), c("Normal", "Disease", "Disease vs Normal")))
  out <- gexpipe_wgcna_heatmap_cor(cor_mat, "Disease vs Normal")
  expect_false("Disease vs Normal" %in% colnames(out))
  expect_equal(ncol(out), 2L)
})

test_that("gexpipe_de_sample_info reports excluded RNA-seq-only samples on mixed meta", {
  meta_used <- data.frame(
    Platform = c("RNAseq", "RNAseq"),
    Condition = c("Normal", "Disease"),
    stringsAsFactors = FALSE
  )
  total <- data.frame(
    Platform = c("Microarray", "Microarray", "RNAseq", "RNAseq"),
    Condition = rep(c("Normal", "Disease"), 2),
    stringsAsFactors = FALSE
  )
  info <- gexpipe_de_sample_info(meta_used, total_meta = total, method = "deseq2")
  expect_equal(info$n_used, 2L)
  expect_equal(info$n_excluded, 2L)
  expect_match(info$note, "RNA-seq")
})

test_that("gexpipe_batch_confounding_summary detects empty cells", {
  meta <- data.frame(
    Dataset = c("GSE1", "GSE1", "GSE2", "GSE2"),
    Condition = c("Normal", "Disease", "Normal", "Disease"),
    stringsAsFactors = FALSE
  )
  ok <- gexpipe_batch_confounding_summary(meta)
  expect_false(ok$confounded)
  meta_bad <- data.frame(
    Dataset = c("GSE1", "GSE1", "GSE2", "GSE2"),
    Condition = c("Normal", "Normal", "Disease", "Disease"),
    stringsAsFactors = FALSE
  )
  bad <- gexpipe_batch_confounding_summary(meta_bad)
  expect_true(bad$confounded)
})

test_that("gexp_normalize_and_intersect builds aligned unified metadata", {
  set.seed(1)
  genes <- paste0("Gene", seq_len(20))
  m1 <- matrix(abs(rnorm(120)), nrow = 20, ncol = 6, dimnames = list(genes, paste0("M1_S", 1:6)))
  m2 <- matrix(round(abs(rnorm(80, 50, 10))), nrow = 20, ncol = 4, dimnames = list(genes, paste0("R1_S", 1:4)))
  storage.mode(m2) <- "integer"
  out <- gexp_normalize_and_intersect(
    micro_expr_list = list(GSEmicro = m1),
    rna_counts_list = list(GSErna = m2),
    de_method = "limma",
    apply_global_quantile = FALSE
  )
  expect_equal(ncol(out$combined_expr), nrow(out$unified_metadata))
  expect_equal(colnames(out$combined_expr), out$unified_metadata$SampleID)
  expect_equal(sum(out$unified_metadata$Platform == "Microarray"), 6L)
  expect_equal(sum(out$unified_metadata$Platform == "RNAseq"), 4L)
})

test_that("gexpipe_pvca_df returns aligned variance components", {
  set.seed(2)
  genes <- paste0("G", seq_len(50))
  expr <- matrix(rnorm(500), nrow = 50, ncol = 10, dimnames = list(genes, paste0("S", 1:10)))
  meta <- data.frame(
    SampleID = colnames(expr),
    Dataset = rep(c("GSE1", "GSE2"), each = 5),
    Platform = rep(c("Microarray", "RNAseq"), each = 5),
    Condition = rep(c("Normal", "Disease"), 5),
    row.names = colnames(expr),
    stringsAsFactors = FALSE
  )
  meta <- meta[sample(nrow(meta)), , drop = FALSE]
  pv <- gexpipe_pvca_df(expr, meta)
  expect_true(pv$ok)
  expect_true(all(c("Dataset", "Platform", "Condition", "Residual") %in% pv$data$Factor))
  expect_gte(sum(pv$data$Variance), 0.99)
  expect_lte(sum(pv$data$Variance), 1.01)
})
