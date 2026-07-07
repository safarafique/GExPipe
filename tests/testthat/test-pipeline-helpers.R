test_that("gexp_parse_gse_inputs respects single-dataset mode", {
  parsed <- gexp_parse_gse_inputs(
    analysis_type = "merged",
    rnaseq_gses = "GSE1, GSE2, GSE3",
    microarray_gses = "GSE4, GSE5",
    dataset_mode = "single"
  )
  expect_equal(parsed$rnaseq_ids, "GSE1")
  expect_equal(parsed$micro_ids, "GSE4")
})

test_that("gexp_download_finalize_common_genes intersects and cbinds", {
  m1 <- matrix(1:12, nrow = 3, dimnames = list(c("A", "B", "C"), paste0("S", 1:4)))
  m2 <- matrix(1:12, nrow = 3, dimnames = list(c("B", "C", "D"), paste0("T", 1:4)))
  out <- gexp_download_finalize_common_genes(
    micro_expr_list = list(GSE1 = m1),
    rna_counts_list = list(GSE2 = m2),
    all_genes_list = list(GSE1 = rownames(m1), GSE2 = rownames(m2))
  )
  expect_true(out$ok)
  expect_equal(out$common_genes, c("B", "C"))
  expect_equal(ncol(out$combined_expr_raw), 8L)
})

test_that("gexp_ensure_unique_colnames_across_datasets prefixes duplicates", {
  fn <- getFromNamespace("gexp_ensure_unique_colnames_across_datasets", "GExPipe")
  m1 <- matrix(1:4, nrow = 2, dimnames = list(c("A", "B"), c("S1", "S2")))
  m2 <- matrix(5:8, nrow = 2, dimnames = list(c("A", "B"), c("S1", "S2")))
  out <- fn(list(GSE1 = m1, GSE2 = m2))
  expect_equal(colnames(out$GSE1), c("GSE1_S1", "GSE1_S2"))
  expect_equal(colnames(out$GSE2), c("GSE2_S1", "GSE2_S2"))
})

test_that("gexp_qc_prepare_venn_sets validates minimum datasets", {
  bad <- gexp_qc_prepare_venn_sets(list(D1 = c("A", "B")))
  expect_false(bad$ok)
  ok <- gexp_qc_prepare_venn_sets(list(D1 = c("A", "B"), D2 = c("B", "C")))
  expect_true(ok$ok)
  expect_length(ok$sets, 2L)
})

test_that("gexp_qc_exclude_samples keeps at least three samples", {
  expr <- gexpipe_test_expr_small
  expect_error(
    gexp_qc_exclude_samples(
      combined_expr_raw = expr,
      micro_expr_list = list(),
      rna_counts_list = list(),
      unified_metadata = NULL,
      samples_to_exclude = colnames(expr)[1:8]
    ),
    "fewer than 3 samples"
  )
})

test_that("classify_groups maps keywords to Normal and Disease", {
  groups <- c("healthy", "control", "tumor", "cancer")
  out <- classify_groups(
    groups,
    normal_keywords = c("healthy", "control"),
    disease_keywords = c("tumor", "cancer")
  )
  expect_equal(out$groups, c("Normal", "Normal", "Disease", "Disease"))
  expect_true(all(out$keep))
})

test_that("gexp_prepare_download_dirs creates expected folders", {
  td <- tempfile("gexp_dl_")
  dir.create(td, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  dir.create(file.path(td, "micro_data"), showWarnings = FALSE)
  dir.create(file.path(td, "rna_data"), showWarnings = FALSE)
  logs <- gexp_prepare_download_dirs(td, has_micro = TRUE, has_rna = TRUE)
  expect_true(dir.exists(file.path(td, "micro_data")))
  expect_true(dir.exists(file.path(td, "rna_data")))
  expect_gt(length(logs), 0L)
})

test_that("primary Shiny server wires modules from namespace not inst source", {
  skip_if_not_installed("GExPipe")
  srv <- getFromNamespace("gexp_app_server", "GExPipe")
  srv_file <- utils::getSrcFilename(srv)
  expect_false(grepl("inst/shinyapp/server/", srv_file, fixed = TRUE))
})
