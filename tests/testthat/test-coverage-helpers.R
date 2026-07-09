test_that("detect_gene_id_format recognizes common ID types", {
  skip_if_not_installed("GExPipe")
  detect <- getFromNamespace("detect_gene_id_format", "GExPipe")
  expect_equal(detect(c("BRCA1", "TP53", "EGFR")), "Gene symbol (HGNC)")
  expect_equal(detect(c("1007_s_at", "1053_at")), "Microarray probe-like ID")
  expect_equal(detect(c("12345", "67890", "11111")), "Entrez ID")
  expect_equal(detect(c("ENSG00000141510", "ENSG00000012048")), "Ensembl ID")
  expect_equal(detect(c("AB000409", "AB000463", "AB000781")), "GenBank/EMBL accession")
  expect_equal(detect(c("ASHG19AP1B100000016V5", "ASHG19AP1B100000050V5")), "Microarray probe-like ID")
  expect_false(identical(
    detect(c("(+)E1A_r60_1", "(+)E1A_r60_3", "(+)E1A_r60_a104")),
    "Gene symbol (HGNC)"
  ))
})

test_that("gexpipe_ids_need_symbol_conversion flags non-symbol IDs", {
  skip_if_not_installed("GExPipe")
  need_conv <- getFromNamespace("gexpipe_ids_need_symbol_conversion", "GExPipe")
  expect_false(need_conv(c("TP53", "BRCA1", "EGFR", "MYC", "GAPDH")))
  expect_true(need_conv(c("AB000409", "AB000463", "AB000781")))
  expect_true(need_conv(c("1007_s_at", "1053_at", "117_at")))
  expect_true(need_conv(c("ENSG00000141510", "ENSG00000012048")))
  expect_true(need_conv(c("ASHG19AP1B100000016V5", "ASHG19AP1B100000050V5")))
  expect_true(need_conv(c("(+)E1A_r60_1", "(+)E1A_r60_3", "(+)E1A_r60_a104")))
})

test_that(".gexpipe_clean_ensembl_keys strips Affymetrix _at suffix", {
  skip_if_not_installed("GExPipe")
  clean <- getFromNamespace(".gexpipe_clean_ensembl_keys", "GExPipe")
  expect_equal(clean("ENSG00000000003_at"), "ENSG00000000003")
  expect_equal(clean("ENSG00000000005.14_at"), "ENSG00000000005")
})

test_that("detect_gene_id_format flags custom probe IDs from user GSEs", {
  skip_if_not_installed("GExPipe")
  detect <- getFromNamespace("detect_gene_id_format", "GExPipe")
  expect_equal(detect(c("(+)E1A_r60_1", "(+)E1A_r60_3")), "Microarray probe-like ID")
  expect_equal(detect(c("ASHG19AP1B100000016V5", "ASHG19AP1B100000050V5")), "Microarray probe-like ID")
  expect_equal(detect(c("ENSG00000000003_at", "ENSG00000000005_at")), "Ensembl ID")
})

test_that("normalize_microarray and normalize_rnaseq run on small matrices", {
  skip_if_not_installed("GExPipe")
  skip_if_not_installed("limma")
  norm_ma <- getFromNamespace("normalize_microarray", "GExPipe")
  norm_rna <- getFromNamespace("normalize_rnaseq", "GExPipe")
  ma <- matrix(abs(rnorm(200, 5, 2)), nrow = 20, ncol = 10)
  rownames(ma) <- paste0("G", seq_len(nrow(ma)))
  colnames(ma) <- paste0("S", seq_len(ncol(ma)))
  ma_out <- norm_ma(ma)
  expect_equal(dim(ma_out), dim(ma))
  rna <- matrix(round(abs(rnorm(200, 50, 10))), nrow = 20, ncol = 10)
  storage.mode(rna) <- "integer"
  rownames(rna) <- rownames(ma)
  colnames(rna) <- colnames(ma)
  rna_out <- norm_rna(rna, method = "TMM")
  expect_equal(dim(rna_out), dim(rna))
})

test_that("read_count_matrix reads a plain CSV count file", {
  skip_if_not_installed("GExPipe")
  skip_if_not_installed("data.table")
  read_cm <- getFromNamespace("read_count_matrix", "GExPipe")
  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf), add = TRUE)
  writeLines(
    c("gene,S1,S2,S3", "G1,10,12,11", "G2,20,22,21", "G3,5,6,7"),
    tf
  )
  df <- read_cm(tf)
  expect_equal(ncol(df), 4L)
  expect_equal(nrow(df), 3L)
})

test_that("Arraystar probe IDs never classified as HGNC", {
  skip_if_not_installed("GExPipe")
  verified <- getFromNamespace("gexpipe_ids_are_verified_symbols", "GExPipe")
  detect <- getFromNamespace("detect_gene_id_format", "GExPipe")
  need <- getFromNamespace("gexpipe_ids_need_symbol_conversion", "GExPipe")
  e1a <- c("(+)E1A_r60_1", "(+)E1A_r60_3", "(+)E1A_r60_a104")
  ashg <- c("ASHG19AP1B100000016V5", "ASHG19AP1B100000050V5")
  expect_false(verified(e1a))
  expect_false(verified(ashg))
  expect_true(need(e1a))
  expect_true(need(ashg))
  expect_equal(detect(e1a), "Microarray probe-like ID")
  expect_equal(detect(ashg), "Microarray probe-like ID")
  expect_false(identical(detect(e1a), "Gene symbol (HGNC)"))
})

test_that(".gexpipe_accept_mapped_symbols accepts plausible gene names", {
  skip_if_not_installed("GExPipe")
  accept <- getFromNamespace(".gexpipe_accept_mapped_symbols", "GExPipe")
  sym <- c("TP53", "BRCA1", "MALAT1", "GAPDH", "ACTB")
  expect_true(accept(sym, length(sym)))
  expect_false(accept(c("(+)E1A_r60_1", "ASHG19AP1B100000016V5"), 2L))
})

test_that("gexpipe_ids_are_verified_symbols rejects probe-like rownames", {
  skip_if_not_installed("GExPipe")
  verified <- getFromNamespace("gexpipe_ids_are_verified_symbols", "GExPipe")
  expect_false(verified(c("1007_s_at", "1053_at", "117_at")))
  expect_false(verified(c("(+)E1A_r60_1", "(+)E1A_r60_3", "(+)E1A_r60_a104")))
  expect_false(verified(c("ENSG00000141510", "ENSG00000012048")))
  expect_true(verified(c("TP53", "BRCA1", "EGFR", "MYC", "GAPDH")))
})

test_that(".gexpipe_is_probe_like_ids detects custom and Affymetrix probes", {
  skip_if_not_installed("GExPipe")
  probe_like <- getFromNamespace(".gexpipe_is_probe_like_ids", "GExPipe")
  expect_true(probe_like(c("1007_s_at", "1053_at")))
  expect_true(probe_like(c("(+)E1A_r60_1", "ASHG19AP1B100000016V5")))
  expect_false(probe_like(c("TP53", "BRCA1", "EGFR")))
})

test_that("gexp_download_normalize_ids_for_overlap keeps HGNC symbol matrices", {
  skip_if_not_installed("GExPipe")
  genes <- paste0("SYM", seq_len(15))
  m1 <- matrix(abs(rnorm(150)), nrow = 15, dimnames = list(genes, paste0("A", 1:10)))
  m2 <- matrix(abs(rnorm(90)), nrow = 15, dimnames = list(genes, paste0("B", 1:6)))
  out <- gexp_download_normalize_ids_for_overlap(
    micro_expr_list = list(GSE1 = m1),
    rna_counts_list = list(GSE2 = m2),
    all_genes_list = list(GSE1 = genes, GSE2 = genes)
  )
  expect_true(grepl("STEP 2b", out$log_text, fixed = TRUE))
  expect_equal(nrow(out$micro_expr_list$GSE1), 15L)
})

test_that("gexp_no_common_genes_diagnostic_log and gexp_rebuild_all_genes_list", {
  skip_if_not_installed("GExPipe")
  m1 <- matrix(1:6, nrow = 2, dimnames = list(c("A", "B"), c("S1", "S2", "S3")))
  m2 <- matrix(1:6, nrow = 2, dimnames = list(c("B", "C"), c("T1", "T2", "T3")))
  rebuilt <- gexp_rebuild_all_genes_list(list(G1 = m1), list(G2 = m2))
  expect_equal(rebuilt$G1, c("A", "B"))
  log_txt <- gexp_no_common_genes_diagnostic_log(rebuilt)
  expect_true(nchar(log_txt) > 50L)
})

test_that("gexp_wgcna_prepare returns datExpr for small synthetic data", {
  skip_if_not_installed("GExPipe")
  skip_if_not_installed("WGCNA")
  set.seed(3)
  expr <- matrix(rnorm(1200), nrow = 120)
  rownames(expr) <- paste0("Gene", seq_len(nrow(expr)))
  colnames(expr) <- paste0("S", seq_len(ncol(expr)))
  meta <- data.frame(
    SampleID = colnames(expr),
    Condition = rep(c("Normal", "Disease"), each = 5),
    Dataset = rep(c("D1", "D2"), each = 5),
    stringsAsFactors = FALSE
  )
  prep <- gexp_wgcna_prepare(expr, meta, gene_mode = "top_variable", top_genes = 100L)
  expect_true(is.matrix(prep$datExpr))
  expect_equal(nrow(prep$datExpr), ncol(expr))
  expect_lte(ncol(prep$datExpr), 100L)
})

test_that("ML and UI helper functions return expected structures", {
  skip_if_not_installed("GExPipe")
  theme_publication <- getFromNamespace("theme_publication", "GExPipe")
  expect_s3_class(theme_publication(), "theme")

  names <- gexp_ml_method_display_names()
  expect_true(length(names) >= 4L)

  sets <- gexp_ml_venn_sets_for_selected(
    list(LASSO = c("A", "B"), `Random Forest` = c("B", "C")),
    c("lasso", "rf")
  )
  expect_equal(gexp_ml_common_gene_count(sets), 1L)

  bar <- gexp_ui_plot_download_bar("p1", "p2", "p3")
  expect_true(inherits(bar, "shiny.tag"))
})

test_that("gexpipe_spearman_cor handles constant columns without warnings", {
  skip_if_not_installed("GExPipe")
  cor_fn <- getFromNamespace("gexpipe_spearman_cor", "GExPipe")
  x <- matrix(c(1, 2, 3, 1, 1, 1, 4, 5, 6), nrow = 3)
  colnames(x) <- c("A", "B", "C")
  expect_warning(
    cor_fn(x),
    NA
  )
  expect_equal(dim(cor_fn(x)), c(3L, 3L))
})

test_that("gexp_app_ui builds welcome page in test mode", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("GExPipe")
  skip_on_cran()
  op <- options(
    shiny.testmode = TRUE,
    gexpipe.minimal_attach_in_testmode = TRUE,
    gexpipe.wgcna_threads = 0L
  )
  on.exit(options(op), add = TRUE)
  gexp_app_ui <- getFromNamespace("gexp_app_ui", "GExPipe")
  ui_obj <- gexp_app_ui()
  expect_true(inherits(ui_obj, "shiny.tag") || inherits(ui_obj, "shiny.tag.list"))
})

test_that(".gexpipe_all_pkgs returns required shiny stack", {
  skip_if_not_installed("GExPipe")
  all_pkgs <- getFromNamespace(".gexpipe_all_pkgs", "GExPipe")
  pkgs <- all_pkgs(include_optional = FALSE)
  expect_true(all(c("shiny", "limma", "DESeq2") %in% pkgs))
})
