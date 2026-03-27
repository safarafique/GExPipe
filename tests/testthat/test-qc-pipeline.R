test_that("gexp_qc_detect_outliers returns expected structure", {
  set.seed(1)
  expr <- matrix(rnorm(2000), nrow = 200, ncol = 10)
  rownames(expr) <- paste0("Gene", seq_len(nrow(expr)))
  colnames(expr) <- paste0("S", seq_len(ncol(expr)))

  res <- gexp_qc_detect_outliers(expr, top_n = 100)

  expect_true(is.list(res))
  expect_true(all(c(
    "scores", "distances", "pca_threshold", "pca_outliers",
    "pca_var_explained", "connectivity", "conn_threshold",
    "conn_outliers", "all_outliers"
  ) %in% names(res)))
  expect_equal(length(res$distances), ncol(expr))
})

test_that("gexp_qc_exclude_samples excludes samples consistently", {
  set.seed(2)
  expr <- matrix(rnorm(1200), nrow = 120, ncol = 10)
  rownames(expr) <- paste0("Gene", seq_len(nrow(expr)))
  colnames(expr) <- paste0("S", seq_len(ncol(expr)))

  micro <- list(GSE1 = expr[, 1:5, drop = FALSE])
  rna <- list(GSE2 = expr[, 6:10, drop = FALSE])
  meta <- data.frame(
    SampleID = colnames(expr),
    Dataset = rep(c("GSE1", "GSE2"), each = 5),
    stringsAsFactors = FALSE
  )

  out <- gexp_qc_exclude_samples(
    combined_expr_raw = expr,
    micro_expr_list = micro,
    rna_counts_list = rna,
    unified_metadata = meta,
    samples_to_exclude = c("S1", "S7")
  )

  expect_false("S1" %in% colnames(out$combined_expr_raw))
  expect_false("S7" %in% colnames(out$combined_expr_raw))
  expect_false("S1" %in% out$unified_metadata$SampleID)
  expect_false("S7" %in% out$unified_metadata$SampleID)
})
