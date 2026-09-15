test_that("gexpipe_parallel_batch_defaults matches DE method and confounding", {
  d1 <- GExPipe:::gexpipe_parallel_batch_defaults("deseq2", FALSE)
  expect_equal(d1$rna, "limma")
  expect_equal(d1$micro, "combat_ref")
  d2 <- GExPipe:::gexpipe_parallel_batch_defaults("limma", FALSE)
  expect_equal(d2$rna, "combat_ref")
  expect_equal(d2$micro, "combat_ref")
  d3 <- GExPipe:::gexpipe_parallel_batch_defaults("deseq2", TRUE)
  expect_equal(d3$rna, "limma")
  expect_equal(d3$micro, "limma")
  expect_equal(GExPipe:::gexpipe_parallel_batch_defaults("edger", FALSE)$rna, "limma")
  expect_equal(GExPipe:::gexpipe_parallel_batch_defaults("limma_voom", FALSE)$rna, "limma")
})

test_that("gexp_batch_correct runs and returns matrix outputs", {
  expr <- gexpipe_test_expr_small
  metadata <- gexpipe_test_meta_small

  res <- gexp_batch_correct(
    expr = expr,
    metadata = metadata,
    variance_percentile = 25,
    method = "limma"
  )

  expect_true(is.matrix(res$expr_filtered))
  expect_true(is.matrix(res$batch_corrected))
  expect_true(nrow(res$batch_corrected) <= nrow(expr))
})

test_that("gexp_batch_correct_by_platform does not jointly ComBat platforms", {
  set.seed(3)
  expr <- matrix(rnorm(80 * 12), nrow = 80, ncol = 12)
  rownames(expr) <- paste0("G", seq_len(80))
  colnames(expr) <- paste0("S", seq_len(12))
  meta <- data.frame(
    Dataset = c(rep("GSE_M", 6), rep("GSE_R", 6)),
    Platform = c(rep("Microarray", 6), rep("RNAseq", 6)),
    Condition = rep(c("Normal", "Disease"), 6),
    row.names = colnames(expr),
    stringsAsFactors = FALSE
  )
  out <- GExPipe:::gexp_batch_correct_by_platform(
    expr, meta, variance_percentile = 10,
    rna_method = "limma", micro_method = "limma"
  )
  expect_true(is.matrix(out$batch_corrected))
  expect_equal(ncol(out$batch_corrected), 12L)
  expect_true(grepl("1 dataset", out$log_text, fixed = TRUE))
  expect_true(grepl("not one joint ComBat", out$log_text, fixed = TRUE))
  expect_true(grepl("not intersected", out$log_text, fixed = TRUE))
  expect_true(is.matrix(out$batch_corrected_rna))
  expect_true(is.matrix(out$batch_corrected_micro))
  expect_equal(ncol(out$batch_corrected_rna), 6L)
  expect_equal(ncol(out$batch_corrected_micro), 6L)
  expect_true(!is.null(out$genes_after_rna))
  expect_true(!is.null(out$genes_after_micro))
})

test_that("gexp_batch_correct_by_platform keeps platform-specific genes", {
  set.seed(6)
  micro <- matrix(rnorm(50 * 6), nrow = 50, ncol = 6)
  rownames(micro) <- paste0("M", seq_len(50))
  colnames(micro) <- paste0("S", 1:6)
  rna <- matrix(rnorm(40 * 6), nrow = 40, ncol = 6)
  rownames(rna) <- c(paste0("M", 1:10), paste0("R", 1:30))
  colnames(rna) <- paste0("S", 7:12)
  meta <- data.frame(
    Dataset = c(rep("GSE_M", 6), rep("GSE_R", 6)),
    Platform = c(rep("Microarray", 6), rep("RNAseq", 6)),
    Condition = rep(c("Normal", "Disease"), 6),
    row.names = c(colnames(micro), colnames(rna)),
    stringsAsFactors = FALSE
  )
  out <- GExPipe:::gexp_batch_correct_by_platform(
    expr = NULL,
    metadata = meta,
    variance_percentile = 10,
    rna_method = "limma",
    micro_method = "limma",
    expr_rna = rna,
    expr_micro = micro
  )
  expect_true(any(grepl("^R", rownames(out$batch_corrected_rna))))
  expect_false(any(grepl("^R", rownames(out$batch_corrected_micro))))
  expect_equal(ncol(out$batch_corrected), 12L)
})

test_that("gexp_batch_correct_by_platform applies a different method per platform", {
  set.seed(7)
  expr <- matrix(rnorm(60 * 16), nrow = 60, ncol = 16)
  rownames(expr) <- paste0("G", seq_len(60))
  colnames(expr) <- paste0("S", seq_len(16))
  meta <- data.frame(
    Dataset = c(rep("GSE_M1", 4), rep("GSE_M2", 4), rep("GSE_R1", 4), rep("GSE_R2", 4)),
    Platform = c(rep("Microarray", 8), rep("RNAseq", 8)),
    Condition = rep(c("Normal", "Disease"), 8),
    row.names = colnames(expr),
    stringsAsFactors = FALSE
  )
  out <- GExPipe:::gexp_batch_correct_by_platform(
    expr, meta, variance_percentile = 10,
    rna_method = "limma", micro_method = "quantile_limma"
  )
  expect_true(grepl("RNA-seq (limma", out$log_text, fixed = TRUE))
  expect_true(grepl("Microarray (quantile_limma", out$log_text, fixed = TRUE))
  expect_equal(out$n_datasets_rna, 2L)
  expect_equal(out$n_datasets_micro, 2L)
  expect_false(grepl("1 dataset", out$log_text, fixed = TRUE))
})

test_that("gexpipe_parallel_de_defaults keeps microarray as limma", {
  d1 <- GExPipe:::gexpipe_parallel_de_defaults("deseq2")
  expect_equal(d1$rna, "deseq2")
  expect_equal(d1$micro, "limma")
  expect_equal(GExPipe:::gexpipe_parallel_de_defaults("limma")$rna, "limma")
  expect_equal(GExPipe:::gexpipe_parallel_de_defaults(NULL)$rna, "deseq2")
})

test_that("gexpipe_bind_rna_counts intersects RNA genes only", {
  a <- matrix(1:20, nrow = 5, ncol = 4)
  rownames(a) <- paste0("G", 1:5)
  colnames(a) <- paste0("R", 1:4)
  b <- matrix(21:36, nrow = 4, ncol = 4)
  rownames(b) <- paste0("G", 2:5)
  colnames(b) <- paste0("R", 5:8)
  out <- GExPipe:::gexpipe_bind_rna_counts(list(GSE1 = a, GSE2 = b))
  expect_equal(nrow(out), 4L)
  expect_equal(ncol(out), 8L)
  expect_false("G1" %in% rownames(out))
})

test_that("gexpipe_run_count_de voom returns DE columns", {
  set.seed(8)
  counts <- matrix(rnbinom(80 * 8, mu = 30, size = 5), nrow = 80, ncol = 8)
  rownames(counts) <- paste0("G", seq_len(80))
  colnames(counts) <- paste0("S", seq_len(8))
  meta <- data.frame(
    Condition = rep(c("Normal", "Disease"), each = 4),
    Dataset = "GSE1",
    row.names = colnames(counts),
    stringsAsFactors = FALSE
  )
  out <- GExPipe:::gexpipe_run_count_de(counts, meta, method = "limma_voom")
  expect_true(all(c("Gene", "logFC", "adj.P.Val", "Significance") %in% names(out$de_results)))
  expect_equal(ncol(counts), 8L)
})

test_that("gexp_run_de returns expected columns", {
  expr <- gexpipe_test_expr(200L, 12L)
  metadata <- gexpipe_test_metadata(colnames(expr))

  res <- gexp_run_de(expr = expr, metadata = metadata, method = "limma")
  expect_true(is.data.frame(res$de_results))
  expect_true(all(c("Gene", "logFC", "adj.P.Val", "Significance") %in% colnames(res$de_results)))
  expect_true(!is.null(res$filter_note))
  expect_true(!is.null(res$formula_desc))
  expect_true(!is.null(res$sample_info))
})

test_that("gexpipe_parallel_wgcna_defaults picks the side with more samples", {
  meta <- data.frame(
    Platform = c(rep("Microarray", 6), rep("RNAseq", 4)),
    row.names = paste0("S", 1:10),
    stringsAsFactors = FALSE
  )
  d <- GExPipe:::gexpipe_parallel_wgcna_defaults(meta)
  expect_equal(d$platform, "microarray")
  expect_equal(d$top_genes, 5000L)
  expect_equal(d$gene_mode, "top_variable")
})

test_that("gexpipe_wgcna_input_expr errors if RNA is requested without RNA data", {
  rv <- list(
    merge_after_de = TRUE,
    unified_metadata = data.frame(
      Dataset = c(rep("GSE_M", 6), rep("GSE_R", 4)),
      Platform = c(rep("Microarray", 6), rep("RNAseq", 4)),
      row.names = paste0("S", 1:10),
      stringsAsFactors = FALSE
    ),
    batch_corrected = matrix(
      rnorm(5 * 6), 5, 6,
      dimnames = list(paste0("G", 1:5), paste0("S", 1:6))
    ),
    raw_counts_for_deseq2 = NULL,
    batch_corrected_rna = NULL,
    batch_corrected_micro = NULL
  )
  expect_error(
    GExPipe:::gexpipe_wgcna_input_expr(rv, parallel_platform = "rnaseq"),
    "RNA-seq WGCNA"
  )
})

test_that("gexpipe_wgcna_input_expr does not subset to DEGs in parallel", {
  set.seed(4)
  expr <- matrix(rnorm(40 * 10), nrow = 40, ncol = 10)
  rownames(expr) <- paste0("G", seq_len(40))
  colnames(expr) <- paste0("S", seq_len(10))
  meta <- data.frame(
    Dataset = c(rep("GSE_M", 6), rep("GSE_R", 4)),
    Platform = c(rep("Microarray", 6), rep("RNAseq", 4)),
    Condition = rep(c("Normal", "Disease"), 5),
    row.names = colnames(expr),
    stringsAsFactors = FALSE
  )
  rv <- list(
    merge_after_de = TRUE,
    unified_metadata = meta,
    batch_corrected = expr,
    combined_expr = expr,
    raw_counts_for_deseq2 = NULL,
    sig_genes = data.frame(Gene = c("G1", "G2"), stringsAsFactors = FALSE)
  )
  out <- GExPipe:::gexpipe_wgcna_input_expr(rv, parallel_platform = "microarray")
  expect_true(nrow(out$expr) >= 30L)
  expect_false(setequal(rownames(out$expr), c("G1", "G2")))
  expect_equal(out$platform, "Microarray")
})

test_that("gexpipe_counts_to_vst returns a continuous matrix", {
  skip_if_not_installed("DESeq2")
  set.seed(5)
  counts <- matrix(rnbinom(30 * 8, mu = 40, size = 5), nrow = 30, ncol = 8)
  rownames(counts) <- paste0("G", seq_len(30))
  colnames(counts) <- paste0("S", seq_len(8))
  vs <- GExPipe:::gexpipe_counts_to_vst(counts)
  expect_true(is.matrix(vs))
  expect_equal(ncol(vs), 8L)
  expect_true(all(is.finite(vs)))
})
