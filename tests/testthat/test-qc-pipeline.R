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

test_that("gexp_align_rnaseq_sample_names replaces fread V2/V3 headers", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("A", "B"), c("V2", "V3")))
  meta <- data.frame(title = c("s1", "s2"), row.names = c("GSM1", "GSM2"))
  out <- gexp_align_rnaseq_sample_names(mat, meta, "GSE137136")
  expect_equal(colnames(out), c("GSM1", "GSM2"))
})

test_that("gexp_suggest_group_column prefers condition then title", {
  skip_if_not_installed("GExPipe")
  suggest <- getFromNamespace("gexp_suggest_group_column", "GExPipe")
  expect_equal(suggest(c("geo_accession", "title", "supplementary_file")), "title")
  expect_equal(suggest(c("geo_accession", "disease_state", "title")), "disease_state")
  expect_equal(suggest(c("characteristics_ch1", "title")), "characteristics_ch1")
})

test_that("gexp_normalize_group_labels parses GEO title conditions", {
  skip_if_not_installed("GExPipe")
  norm <- getFromNamespace("gexp_normalize_group_labels", "GExPipe")
  titles <- c(
    "islet preparation 1 under control condition",
    "islet preparation 2 under control condition",
    "islet preparation 1 under cytokine treatment",
    "islet preparation 2 under cytokine treatment"
  )
  out <- norm(titles, col_name = "title")
  expect_equal(unique(out), c("control condition", "cytokine treatment"))
})

test_that("gexp_suggest_group_category detects control and cytokine labels", {
  skip_if_not_installed("GExPipe")
  suggest_cat <- getFromNamespace("gexp_suggest_group_category", "GExPipe")
  expect_equal(suggest_cat("control condition"), "Normal")
  expect_equal(suggest_cat("cytokine treatment"), "Disease")
})

test_that("gexp_qc_build_sample_dataset_map maps samples to GSE IDs", {
  m1 <- matrix(1:4, nrow = 2, dimnames = list(c("A", "B"), c("S1", "S2")))
  m2 <- matrix(1:4, nrow = 2, dimnames = list(c("A", "B"), c("S3", "S4")))
  mp <- gexp_qc_build_sample_dataset_map(list(GSE1 = m1), list(GSE2 = m2))
  expect_equal(unname(mp), c("GSE1", "GSE1", "GSE2", "GSE2"))
  expect_equal(names(mp), c("S1", "S2", "S3", "S4"))
})

test_that("gexp_orient_count_dataframe transposes samples-as-rows layout", {
  df <- data.frame(
    sample = c("GSM1", "GSM2"),
    G1 = c(10L, 12L),
    G2 = c(20L, 22L),
    G3 = c(1L, 2L),
    G4 = c(3L, 4L),
    G5 = c(5L, 6L),
    G6 = c(7L, 8L),
    G7 = c(9L, 10L),
    stringsAsFactors = FALSE
  )
  meta <- data.frame(title = c("s1", "s2"), row.names = c("GSM1", "GSM2"))
  out <- gexp_orient_count_dataframe(df, metadata = meta)
  expect_equal(dim(out$matrix), c(7L, 2L))
  expect_equal(colnames(out$matrix), c("GSM1", "GSM2"))
  expect_equal(rownames(out$matrix)[1:2], c("G1", "G2"))
})
