test_that("dummy_imports satisfies R CMD check import references", {
  skip_if_not_installed("GExPipe")
  expect_no_error(getFromNamespace("dummy_imports", "GExPipe")())
})

test_that("utils_shiny_app helpers return expected values", {
  skip_if_not_installed("GExPipe")
  all_pkgs <- getFromNamespace(".gexpipe_all_pkgs", "GExPipe")
  pkgs <- all_pkgs(include_optional = FALSE)
  expect_true(all(c("shiny", "limma", "DESeq2", "GEOquery") %in% pkgs))
  expect_type(getFromNamespace(".gexpipe_rv_str", "GExPipe")(), "character")
  expect_true(dir.exists(getFromNamespace(".gexpipe_get_lib", "GExPipe")()))
  best <- getFromNamespace(".gexpipe_best_version", "GExPipe")("shiny")
  expect_s3_class(best, "package_version")
  inst <- getFromNamespace(".gexp_inst_file", "GExPipe")("shinyapp")
  expect_true(nzchar(inst))
})

test_that(".gexpipe_call resolves exported pipeline helpers", {
  skip_if_not_installed("GExPipe")
  call_fn <- getFromNamespace(".gexpipe_call", "GExPipe")
  out <- call_fn("gexp_parse_gse_inputs",
    analysis_type = "rnaseq",
    rnaseq_gses = "GSE1",
    microarray_gses = "",
    dataset_mode = "single"
  )
  expect_equal(out$rnaseq_ids, "GSE1")
})

test_that(".gexpipe_missing_required_pkgs detects absent packages", {
  skip_if_not_installed("GExPipe")
  fn <- getFromNamespace(".gexpipe_missing_required_pkgs", "GExPipe")
  lib <- getFromNamespace(".gexpipe_get_lib", "GExPipe")()
  miss <- fn("not_a_real_pkg_xyz", lib)
  expect_equal(miss, "not_a_real_pkg_xyz")
})

test_that("gexp_app_onStart sets shiny options", {
  skip_if_not_installed("GExPipe")
  skip_if_not_installed("shiny")
  on_start <- getFromNamespace("gexp_app_onStart", "GExPipe")
  expect_no_error(on_start())
  expect_false(getOption("stringsAsFactors"))
  expect_equal(getOption("shiny.maxRequestSize"), 500 * 1024^2)
})

test_that("all gexp_ui tab accessors build shiny UI in test mode", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("GExPipe")
  op <- options(
    shiny.testmode = TRUE,
    gexpipe.minimal_attach_in_testmode = TRUE,
    gexpipe.wgcna_threads = 0L,
    gexpipe.attach.done = FALSE,
    gexpipe.attach.shiny_stack_only_done = FALSE,
    gexpipe.prelaunch_install_done = TRUE
  )
  on.exit(options(op), add = TRUE)

  accessors <- c(
    "gexp_ui_download", "gexp_ui_qc", "gexp_ui_normalize", "gexp_ui_groups",
    "gexp_ui_batch", "gexp_ui_results", "gexp_ui_wgcna", "gexp_ui_common_genes",
    "gexp_ui_ppi", "gexp_ui_ml", "gexp_ui_validation", "gexp_ui_roc",
    "gexp_ui_nomogram", "gexp_ui_gsea", "gexp_ui_results_summary", "gexp_ui_welcome"
  )
  for (nm in accessors) {
    fn <- getFromNamespace(nm, "GExPipe")
    ui_obj <- fn()
    expect_true(
      inherits(ui_obj, "shiny.tag") || inherits(ui_obj, "shiny.tag.list") || is.list(ui_obj),
      label = nm
    )
  }
})

test_that("gexp_stringdb_new_safe handles missing STRINGdb gracefully", {
  skip_if_not_installed("GExPipe")
  fn <- getFromNamespace("gexp_stringdb_new_safe", "GExPipe")
  if (requireNamespace("STRINGdb", quietly = TRUE)) {
    expect_no_error(fn(score_threshold = 400))
  } else {
    expect_null(fn(score_threshold = 400))
  }
})
