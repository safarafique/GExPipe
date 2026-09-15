test_that("pipeline DE label never crashes on a missing method", {
  fn <- getFromNamespace(".gexp_de_progress_label", "GExPipe")
  expect_equal(fn(NULL), "DE (limma)")
  expect_equal(fn(character(0)), "DE (limma)")
  expect_equal(fn(NA_character_), "DE (limma)")
  expect_equal(fn("deseq2"), "DE (DESeq2)")
  expect_equal(fn("edger"), "DE (edgeR)")
  expect_equal(fn("limma_voom"), "DE (limma-voom)")
  expect_equal(fn("limma"), "DE (limma)")
})

test_that("workspace skip helper rejects functions and keeps matrices", {
  fn <- getFromNamespace(".gexp_workspace_value_ok", "GExPipe")
  expect_true(fn(NULL))
  expect_true(fn(matrix(1:4, 2)))
  expect_false(fn(identity))
  expect_false(fn(new.env()))
})

test_that("Parallel Apply Normalization is not a click on the hidden legacy button", {
  r_dir <- if (dir.exists("R")) "R" else file.path("..", "..", "R")
  norm <- paste(readLines(file.path(r_dir, "server_normalize.R"), warn = FALSE), collapse = "\n")
  expect_false(grepl('shinyjs::click("apply_normalization")', norm, fixed = TRUE))
  expect_true(grepl("apply_normalization_parallel", norm, fixed = TRUE))
  pipe <- paste(readLines(file.path(r_dir, "observers_pipeline.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl('toggleState("apply_normalization_parallel"', pipe, fixed = TRUE))
  groups <- paste(readLines(file.path(r_dir, "server_groups.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl('identical(input$sidebar_menu, "groups")', groups, fixed = TRUE))
})

test_that("Parallel UI keeps RNA-seq left and a separate legacy track", {
  r_dir <- if (dir.exists("R")) "R" else file.path("..", "..", "R")
  wrap <- paste(readLines(file.path(r_dir, "interface_tabs_wrappers.R"), warn = FALSE), collapse = "\n")
  rna_pos <- regexpr("RNA-seq run log", wrap, fixed = TRUE)[1]
  micro_pos <- regexpr("Microarray run log", wrap, fixed = TRUE)[1]
  expect_true(rna_pos > 0)
  expect_true(micro_pos > rna_pos)

  ui_names <- c("ui_normalize.R", "ui_qc.R", "ui_groups.R", "ui_batch.R", "ui_results.R")
  for (nm in ui_names) {
    f <- file.path(r_dir, nm)
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    expect_true(grepl("analysis_type != 'parallel'", txt, fixed = TRUE), info = nm)
    expect_true(grepl("analysis_type == 'parallel'", txt, fixed = TRUE), info = nm)
  }
  results <- paste(readLines(file.path(r_dir, "ui_results.R"), warn = FALSE), collapse = "\n")
  expect_false(grepl("de_view_platform", results, fixed = TRUE))
  expect_true(grepl("volcano_plot_rna", results, fixed = TRUE))
  expect_true(grepl("volcano_plot_micro", results, fixed = TRUE))
  expect_true(grepl("de_mode_parallel", results, fixed = TRUE))
  expect_true(grepl("next_page_results_parallel", results, fixed = TRUE))
  expect_true(grepl("de_method_rna_step6", results, fixed = TRUE))
  expect_true(grepl("logfc_cutoff_rna", results, fixed = TRUE))
  expect_true(grepl("logfc_cutoff_micro", results, fixed = TRUE))
  expect_true(grepl("padj_cutoff_rna", results, fixed = TRUE))
  expect_true(grepl("padj_cutoff_micro", results, fixed = TRUE))
  cons_ui <- paste(readLines(file.path(r_dir, "ui_consensus.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl("consensus_mode_parallel", cons_ui, fixed = TRUE))
  wgcna_ui <- paste(readLines(file.path(r_dir, "ui_wgcna.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl("wgcna_mode", wgcna_ui, fixed = TRUE))
  expect_true(grepl("wgcna_setup_guide_ui", wgcna_ui, fixed = TRUE))
  expect_true(grepl("wgcna_parallel_platform", wgcna_ui, fixed = TRUE))
  expect_true(grepl("RNA-seq (VST of counts)", wgcna_ui, fixed = TRUE))
  norm_ui <- paste(readLines(file.path(r_dir, "ui_normalize.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl("normalization_median_range_rna", norm_ui, fixed = TRUE))
  expect_true(grepl("normalization_median_range_micro", norm_ui, fixed = TRUE))
  expect_true(grepl("normalization_distribution_overlap_rna", norm_ui, fixed = TRUE))
  expect_true(grepl("normalization_distribution_overlap_micro", norm_ui, fixed = TRUE))
  expect_true(grepl("norm_manual_guide_ui", norm_ui, fixed = TRUE))
  expect_true(grepl("norm_auto_guide_parallel_ui", norm_ui, fixed = TRUE))
  batch_ui <- paste(readLines(file.path(r_dir, "ui_batch.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl("batch_method_rna", batch_ui, fixed = TRUE))
  expect_true(grepl("batch_method_micro", batch_ui, fixed = TRUE))
  expect_true(grepl("batch_mode_parallel", batch_ui, fixed = TRUE))
  expect_true(grepl("next_page_batch", batch_ui, fixed = TRUE))
  expect_true(grepl("next_page_batch_end", batch_ui, fixed = TRUE))
})

test_that("four-type later-step About boxes name the DEG source", {
  rna <- GExPipe:::gexpipe_ui_later_step_about("common_genes", "rnaseq")
  par <- GExPipe:::gexpipe_ui_later_step_about("common_genes", "parallel")
  expect_true(inherits(rna, "shiny.tag") || inherits(rna, "shiny.tag.list"))
  expect_true(grepl("RNA-seq", paste(rna, collapse = " ")))
  expect_true(grepl("Step 7", paste(par, collapse = " ")))
  expect_equal(GExPipe:::gexpipe_analysis_type_label("merged"), "Merged (joint DE)")
  expect_true(inherits(GExPipe:::gexpipe_pub_theme(), "theme"))
})

test_that("normalization Auto/Manual guides mention count DE skip and limma TMM", {
  auto_count <- gexpipe_ui_norm_auto_guide("rnaseq", "deseq2")
  auto_limma <- gexpipe_ui_norm_auto_guide("parallel", "limma")
  man_count <- gexpipe_ui_norm_manual_guide("rnaseq", "edger")
  man_limma <- gexpipe_ui_norm_manual_guide("parallel", "limma")
  expect_true(inherits(auto_count, "shiny.tag"))
  expect_true(grepl("not", paste(auto_count, collapse = " "), ignore.case = TRUE))
  expect_true(grepl("TMM", paste(auto_limma, collapse = " ")))
  expect_true(grepl("raw counts", paste(man_count, collapse = " "), ignore.case = TRUE))
  expect_true(grepl("limma", paste(man_limma, collapse = " "), ignore.case = TRUE))
  merged_auto <- gexpipe_ui_norm_auto_guide("merged", "deseq2")
  expect_true(grepl("global quantile", paste(merged_auto, collapse = " "), ignore.case = TRUE))
  expect_true(grepl("TMM", paste(merged_auto, collapse = " ")))
})

test_that("gexp_parse_gse_inputs accepts parallel DE then merge", {
  parsed <- gexp_parse_gse_inputs(
    analysis_type = "parallel",
    rnaseq_gses = "GSE50760",
    microarray_gses = "GSE89076",
    dataset_mode = "multi"
  )
  expect_equal(parsed$rnaseq_ids, "GSE50760")
  expect_equal(parsed$micro_ids, "GSE89076")
  expect_equal(parsed$analysis_type, "parallel")
})

test_that("Merged and Parallel keep every GSE on each platform", {
  par <- gexp_parse_gse_inputs(
    analysis_type = "parallel",
    rnaseq_gses = "GSE1, GSE2",
    microarray_gses = "GSE3, GSE4, GSE5",
    dataset_mode = "single"
  )
  expect_equal(par$rnaseq_ids, c("GSE1", "GSE2"))
  expect_equal(par$micro_ids, c("GSE3", "GSE4", "GSE5"))
  expect_equal(par$dataset_mode, "multi")
  expect_equal(par$analysis_type, "parallel")
  mer <- gexp_parse_gse_inputs(
    analysis_type = "merged",
    rnaseq_gses = "GSE1, GSE2, GSE3",
    microarray_gses = "GSE4, GSE5",
    dataset_mode = "single"
  )
  expect_equal(mer$rnaseq_ids, c("GSE1", "GSE2", "GSE3"))
  expect_equal(mer$micro_ids, c("GSE4", "GSE5"))
  expect_equal(mer$dataset_mode, "multi")
  expect_equal(mer$analysis_type, "merged")
})

test_that("both GSE boxes with rnaseq radio infer merged; parallel stays parallel", {
  coerced <- gexp_parse_gse_inputs(
    analysis_type = "rnaseq",
    rnaseq_gses = "GSE1",
    microarray_gses = "GSE2",
    dataset_mode = "multi"
  )
  expect_equal(coerced$analysis_type, "merged")
  par <- gexp_parse_gse_inputs(
    analysis_type = "parallel",
    rnaseq_gses = "GSE1",
    microarray_gses = "GSE2",
    dataset_mode = "multi"
  )
  expect_equal(par$analysis_type, "parallel")
})

test_that("gexp_parse_gse_inputs respects single-dataset mode for one platform", {
  parsed <- gexp_parse_gse_inputs(
    analysis_type = "rnaseq",
    rnaseq_gses = "GSE1, GSE2, GSE3",
    microarray_gses = "",
    dataset_mode = "single"
  )
  expect_equal(parsed$rnaseq_ids, "GSE1")
  expect_equal(parsed$micro_ids, character(0))
})

test_that("gexp_download_finalize_common_genes keep_platforms_separate does not subset", {
  m1 <- matrix(1:12, nrow = 3, dimnames = list(c("A", "B", "C"), paste0("S", 1:4)))
  m2 <- matrix(1:16, nrow = 4, dimnames = list(c("B", "C", "D", "E"), paste0("T", 1:4)))
  out <- gexp_download_finalize_common_genes(
    micro_expr_list = list(GSE1 = m1),
    rna_counts_list = list(GSE2 = m2),
    all_genes_list = list(GSE1 = rownames(m1), GSE2 = rownames(m2)),
    keep_platforms_separate = TRUE
  )
  expect_true(out$ok)
  expect_equal(sort(rownames(out$micro_expr_list$GSE1)), c("A", "B", "C"))
  expect_equal(sort(rownames(out$rna_counts_list$GSE2)), c("B", "C", "D", "E"))
  expect_equal(sort(out$common_genes), c("B", "C"))
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

test_that("gexp_download_finalize_common_genes cbinds mixed platforms with different gene counts", {
  m1 <- matrix(1, nrow = 5, ncol = 3, dimnames = list(paste0("G", 1:5), paste0("S", 1:3)))
  m2 <- matrix(1, nrow = 8, ncol = 4, dimnames = list(paste0("G", 3:10), paste0("T", 1:4)))
  r1 <- matrix(1, nrow = 6, ncol = 2, dimnames = list(paste0("G", 2:7), paste0("U", 1:2)))
  r2 <- matrix(1, nrow = 6, ncol = 2, dimnames = list(paste0("G", 2:7), paste0("V", 1:2)))
  expect_error(do.call(cbind, list(m1, m2, r1, r2)), "number of rows")
  out <- gexp_download_finalize_common_genes(
    micro_expr_list = list(GSE89076 = m1, GSE44076 = m2),
    rna_counts_list = list(GSE50760 = r1, GSE104836 = r2),
    all_genes_list = list(
      GSE89076 = rownames(m1),
      GSE44076 = rownames(m2),
      GSE50760 = rownames(r1),
      GSE104836 = rownames(r2)
    ),
    keep_platforms_separate = TRUE
  )
  expect_true(out$ok)
  expect_equal(nrow(out$micro_expr_list$GSE89076), 5L)
  expect_equal(nrow(out$micro_expr_list$GSE44076), 8L)
  expect_equal(sort(out$common_genes), c("G3", "G4", "G5"))
  expect_equal(nrow(out$combined_expr_raw), 3L)
  expect_equal(ncol(out$combined_expr_raw), 11L)

  merged <- gexp_download_finalize_common_genes(
    micro_expr_list = list(GSE89076 = m1, GSE44076 = m2),
    rna_counts_list = list(GSE50760 = r1, GSE104836 = as.data.frame(r2)),
    all_genes_list = list(
      GSE89076 = rownames(m1),
      GSE44076 = rownames(m2),
      GSE50760 = rownames(r1),
      GSE104836 = rownames(r2)
    )
  )
  expect_true(merged$ok)
  expect_equal(nrow(merged$combined_expr_raw), 3L)
  expect_equal(ncol(merged$combined_expr_raw), 11L)
})

test_that("gexp_download_finalize_common_genes aligns duplicate gene symbols", {
  m1 <- matrix(1:8, nrow = 4, dimnames = list(c("A", "B", "A", "C"), paste0("S", 1:2)))
  m2 <- matrix(1:6, nrow = 3, dimnames = list(c("A", "B", "C"), paste0("T", 1:2)))
  out <- gexp_download_finalize_common_genes(
    micro_expr_list = list(GSE1 = m1),
    rna_counts_list = list(GSE2 = m2),
    all_genes_list = list(GSE1 = rownames(m1), GSE2 = rownames(m2))
  )
  expect_true(out$ok)
  expect_equal(nrow(out$combined_expr_raw), length(out$common_genes))
  expect_equal(sort(out$common_genes), c("A", "B", "C"))
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
