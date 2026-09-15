test_that("gexpipe_platform_sample_ids splits by Platform", {
  meta <- data.frame(
    Platform = c("Microarray", "Microarray", "RNAseq"),
    row.names = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
  expect_equal(gexpipe_platform_sample_ids(meta, "Microarray"), c("A", "B"))
  expect_equal(gexpipe_platform_sample_ids(meta, "RNAseq"), "C")
})

test_that("gexpipe_parallel_consensus_defaults requires same direction", {
  d <- GExPipe:::gexpipe_parallel_consensus_defaults()
  expect_true(isTRUE(d$same_direction))
})

test_that("gexpipe_consensus_degs keeps same-direction overlap", {
  rna <- data.frame(
    Gene = c("A", "B", "C"),
    logFC = c(1.2, -0.8, 1.1),
    adj.P.Val = c(0.01, 0.02, 0.03),
    Significance = c("Up-regulated", "Down-regulated", "Up-regulated"),
    stringsAsFactors = FALSE
  )
  micro <- data.frame(
    Gene = c("A", "B", "D"),
    logFC = c(0.9, -1.0, 1.4),
    adj.P.Val = c(0.02, 0.01, 0.04),
    Significance = c("Up-regulated", "Down-regulated", "Up-regulated"),
    stringsAsFactors = FALSE
  )
  out <- gexpipe_consensus_degs(rna, micro, require_same_direction = TRUE)
  expect_equal(sort(out$genes), c("A", "B"))
  expect_equal(out$n_rna, 3L)
  expect_equal(out$n_micro, 3L)
  expect_equal(out$n_consensus, 2L)
  expect_equal(out$rna_only, "C")
  expect_equal(out$micro_only, "D")
  expect_equal(out$table["A", "Significance"], "Up-regulated")
  expect_equal(out$table["B", "Significance"], "Down-regulated")
})

test_that("gexpipe_consensus_degs drops discordant directions", {
  rna <- data.frame(
    Gene = c("A", "B"),
    logFC = c(1.2, 0.8),
    adj.P.Val = c(0.01, 0.02),
    Significance = c("Up-regulated", "Up-regulated"),
    stringsAsFactors = FALSE
  )
  micro <- data.frame(
    Gene = c("A", "B"),
    logFC = c(0.9, -1.0),
    adj.P.Val = c(0.02, 0.01),
    Significance = c("Up-regulated", "Down-regulated"),
    stringsAsFactors = FALSE
  )
  out <- gexpipe_consensus_degs(rna, micro, require_same_direction = TRUE)
  expect_equal(out$genes, "A")
  expect_equal(out$n_discordant, 1L)
  out2 <- gexpipe_consensus_degs(rna, micro, require_same_direction = FALSE)
  expect_equal(sort(out2$genes), c("A", "B"))
})

test_that("gexpipe_run_limma_on_subset finds DE genes in a simple contrast", {
  set.seed(1)
  expr <- rbind(
    matrix(c(rnorm(6, 0), rnorm(6, 3)), nrow = 1),
    matrix(rnorm(60), nrow = 5)
  )
  rownames(expr) <- paste0("G", seq_len(nrow(expr)))
  colnames(expr) <- paste0("S", seq_len(ncol(expr)))
  meta <- data.frame(
    Condition = rep(c("Normal", "Disease"), each = 6),
    row.names = colnames(expr),
    stringsAsFactors = FALSE
  )
  out <- gexpipe_run_limma_on_subset(expr, meta, logfc_cutoff = 0.5, padj_cutoff = 0.1)
  expect_true("G1" %in% out$sig_genes$Gene)
  expect_true(out$sig_genes["G1", "logFC"] > 0)
})
