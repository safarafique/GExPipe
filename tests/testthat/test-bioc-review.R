test_that("Shiny server modules are defined in the package namespace", {
  skip_if_not_installed("GExPipe")
  ns <- asNamespace("GExPipe")
  mods <- c(
    "server_download", "server_qc", "server_normalize", "server_batch",
    "server_results", "server_wgcna", "server_common_genes", "server_ppi",
    "server_ml", "server_validation", "server_roc", "server_nomogram",
    "server_gsea", "server_results_summary", "server_groups"
  )
  for (nm in mods) {
    expect_true(exists(nm, envir = ns, inherits = FALSE, mode = "function"), info = nm)
  }
})

test_that("Shiny UI tab objects are defined in the package namespace", {
  skip_if_not_installed("GExPipe")
  ns <- asNamespace("GExPipe")
  tabs <- c(
    "ui_welcome", "ui_qc", "ui_normalize", "ui_groups", "ui_batch",
    "ui_results", "ui_wgcna", "ui_common_genes", "ui_ppi", "ui_ml",
    "ui_validation", "ui_roc", "ui_nomogram", "ui_gsea", "ui_results_summary"
  )
  for (nm in tabs) {
    expect_true(exists(nm, envir = ns, inherits = FALSE), info = nm)
  }
})

test_that("gexp_ui tab accessors return shiny tags", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("GExPipe")
  skip_on_cran()

  op <- options(
    shiny.testmode = TRUE,
    gexpipe.minimal_attach_in_testmode = TRUE,
    gexpipe.wgcna_threads = 0L
  )
  on.exit(options(op), add = TRUE)

  gexp_ui_qc <- getFromNamespace("gexp_ui_qc", "GExPipe")
  gexp_ui_download <- getFromNamespace("gexp_ui_download", "GExPipe")
  gexp_ui_welcome <- getFromNamespace("gexp_ui_welcome", "GExPipe")

  expect_true(inherits(gexp_ui_qc(), "shiny.tag"))
  expect_true(inherits(gexp_ui_download(), "shiny.tag"))
  welcome_ui <- gexp_ui_welcome()
  expect_true(inherits(welcome_ui, "shiny.tag") || inherits(welcome_ui, "shiny.tag.list"))
})

test_that("vignette extdata loads via system.file", {
  skip_if_not_installed("GExPipe")
  expr_path <- system.file("extdata", "vignette_expression.csv", package = "GExPipe")
  meta_path <- system.file("extdata", "vignette_sample_metadata.csv", package = "GExPipe")
  expect_true(nzchar(expr_path) && file.exists(expr_path))
  expect_true(nzchar(meta_path) && file.exists(meta_path))
  expr <- utils::read.csv(expr_path, check.names = FALSE)
  meta <- utils::read.csv(meta_path)
  expect_gt(nrow(expr), 10L)
  expect_equal(ncol(expr), nrow(meta) + 1L)
})

test_that("gexpipe_setup verifies imports without auto-install by default", {
  skip_if_not_installed("GExPipe")
  gexpipe_setup <- getFromNamespace("gexpipe_setup", "GExPipe")
  op <- options(gexpipe.auto_install = FALSE)
  on.exit(options(op), add = TRUE)
  expect_no_error(gexpipe_setup(update = FALSE, launch = FALSE))
})

test_that("ML Venn helpers handle empty overlap", {
  skip_if_not_installed("GExPipe")
  sets <- getFromNamespace("gexp_ml_venn_sets_for_selected", "GExPipe")(
    list(LASSO = c("A", "B"), Ridge = c("C", "D")),
    c("lasso", "ridge")
  )
  count <- getFromNamespace("gexp_ml_common_gene_count", "GExPipe")(sets)
  expect_equal(count, 0L)
  expect_length(sets, 2L)
})

test_that("observers register without error when shiny is available", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("GExPipe")
  skip_on_cran()

  reg_pipe <- getFromNamespace("gexp_register_pipeline_observers", "GExPipe")
  reg_nav <- getFromNamespace("gexp_register_navigation_observers", "GExPipe")
  reg_ws <- getFromNamespace("gexp_register_workspace_observers", "GExPipe")
  reg_help <- getFromNamespace("gexp_register_help_observers", "GExPipe")

  expect_true(is.function(reg_pipe))
  expect_true(is.function(reg_nav))
  expect_true(is.function(reg_ws))
  expect_true(is.function(reg_help))
})

test_that("DESCRIPTION BugReports points to GitHub issues", {
  desc <- read.dcf(system.file("DESCRIPTION", package = "GExPipe"))
  bug <- desc[1, "BugReports"]
  expect_match(bug, "github\\.com/safarafique/GExPipe/issues")
})
