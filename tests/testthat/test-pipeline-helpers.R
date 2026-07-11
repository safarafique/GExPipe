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
  ns <- asNamespace("GExPipe")
  expect_true(exists("gexp_app_server", envir = ns, inherits = FALSE, mode = "function"))
  expect_true(exists("server_download", envir = ns, inherits = FALSE, mode = "function"))
  srv_file <- utils::getSrcFilename(getFromNamespace("gexp_app_server", "GExPipe"))
  skip_if_not(nzchar(srv_file), "getSrcFilename unavailable for installed bytecode")
  expect_false(grepl("inst/shinyapp/server/", srv_file, fixed = TRUE))
  expect_true(grepl("server_app", basename(srv_file), fixed = TRUE))
})

test_that("count-file scoring prefers multi-sample matrix over single-sample HTSeq", {
  skip_if_not_installed("GExPipe")
  score_fn <- getFromNamespace(".gexpipe_score_count_candidate", "GExPipe")
  matrix_info <- list(path = "GSE_matrix.htseq-count.txt.gz", nrow = 39000L, ncol = 11L, nsamp = 10L)
  single_info <- list(path = "GSM1_sample.htseq-count.txt.gz", nrow = 57450L, ncol = 2L, nsamp = 1L)
  expect_gt(score_fn(matrix_info, 10L), score_fn(single_info, 10L))
})

test_that(".gexpipe_pick_best_count_file selects combined matrix over per-sample files", {
  skip_if_not_installed("GExPipe")
  skip_if_not_installed("data.table")
  pick_fn <- getFromNamespace(".gexpipe_pick_best_count_file", "GExPipe")
  td <- tempfile("gexp_counts_")
  dir.create(td, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  matrix_path <- file.path(td, "GSE1_matrix.htseq-count.txt.gz")
  single1 <- file.path(td, "GSM1_sample.htseq-count.txt.gz")
  single2 <- file.path(td, "GSM2_sample.htseq-count.txt.gz")
  genes <- paste0("G", seq_len(50))
  mat_df <- data.frame(
    gene = genes,
    S1 = seq_len(50),
    S2 = seq_len(50) + 50,
    S3 = seq_len(50) + 100,
    stringsAsFactors = FALSE
  )
  data.table::fwrite(mat_df, matrix_path)
  data.table::fwrite(data.frame(gene = genes, count = seq_len(50), stringsAsFactors = FALSE), single1)
  data.table::fwrite(data.frame(gene = genes, count = rev(seq_len(50)), stringsAsFactors = FALSE), single2)

  picked <- pick_fn(c(matrix_path, single1, single2), n_meta = 3L)
  expect_equal(picked, matrix_path)
})

test_that(".gexpipe_merge_per_sample_count_files builds genes x samples table", {
  skip_if_not_installed("GExPipe")
  skip_if_not_installed("data.table")
  merge_fn <- getFromNamespace(".gexpipe_merge_per_sample_count_files", "GExPipe")
  td <- tempfile("gexp_merge_")
  dir.create(td, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  genes <- paste0("G", seq_len(20))
  f1 <- file.path(td, "GSM1_tumor.htseq-count.txt.gz")
  f2 <- file.path(td, "GSM2_normal.htseq-count.txt.gz")
  data.table::fwrite(data.frame(gene = genes, count = seq_len(20), stringsAsFactors = FALSE), f1)
  data.table::fwrite(data.frame(gene = genes, count = rev(seq_len(20)), stringsAsFactors = FALSE), f2)

  merged <- merge_fn(c(f1, f2))
  expect_equal(ncol(merged), 3L)
  expect_equal(nrow(merged), 20L)
  expect_equal(colnames(merged)[1], "gene")
})

test_that(".gexpipe_choose_supp_or_ncbi prefers NCBI when supp has fewer samples", {
  skip_if_not_installed("GExPipe")
  skip_if_not_installed("data.table")
  choose_fn <- getFromNamespace(".gexpipe_choose_supp_or_ncbi", "GExPipe")
  td <- tempfile("gexp_choose_")
  dir.create(td, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  genes <- paste0("G", seq_len(30))
  supp_path <- file.path(td, "GSM1.htseq-count.txt.gz")
  ncbi_path <- file.path(td, "GSE1_raw_counts_GRCh38_NCBI.tsv.gz")
  data.table::fwrite(data.frame(gene = genes, count = seq_len(30), stringsAsFactors = FALSE), supp_path)
  ncbi_df <- data.frame(
    gene = genes,
    S1 = seq_len(30),
    S2 = seq_len(30) + 30,
    S3 = seq_len(30) + 60,
    stringsAsFactors = FALSE
  )
  data.table::fwrite(ncbi_df, ncbi_path)

  chosen <- choose_fn(supp_path, ncbi_path, n_meta = 3L)
  expect_equal(chosen$file, ncbi_path)
  expect_equal(chosen$source, "NCBI")
})

test_that(".gexpipe_list_gse_related_files includes top-level matrix file", {
  skip_if_not_installed("GExPipe")
  list_fn <- getFromNamespace(".gexpipe_list_gse_related_files", "GExPipe")
  td <- tempfile("gexp_files_")
  dir.create(td, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  gse <- "GSE137136"
  gse_dir <- file.path(td, gse)
  dir.create(gse_dir, showWarnings = FALSE)
  top_matrix <- file.path(td, "GSE137136_matrix.htseq-count.txt.gz")
  nested_single <- file.path(gse_dir, "GSM123.htseq-count.txt.gz")
  writeLines("x", top_matrix)
  writeLines("x", nested_single)

  files <- list_fn(gse_dir, td, gse)
  expect_true(top_matrix %in% files)
  expect_true(nested_single %in% files)
})
