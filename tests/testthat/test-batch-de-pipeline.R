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

test_that("gexp_run_de returns expected columns", {
  expr <- gexpipe_test_expr(200L, 12L)
  metadata <- gexpipe_test_metadata(colnames(expr))

  res <- gexp_run_de(expr = expr, metadata = metadata, method = "limma")
  expect_true(is.data.frame(res$de_results))
  expect_true(all(c("Gene", "logFC", "adj.P.Val", "Significance") %in% colnames(res$de_results)))
})
