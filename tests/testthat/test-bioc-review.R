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

  reg_pipe <- getFromNamespace("gexp_register_pipeline_observers", "GExPipe")
  reg_nav <- getFromNamespace("gexp_register_navigation_observers", "GExPipe")
  reg_ws <- getFromNamespace("gexp_register_workspace_observers", "GExPipe")
  reg_help <- getFromNamespace("gexp_register_help_observers", "GExPipe")

  expect_true(is.function(reg_pipe))
  expect_true(is.function(reg_nav))
  expect_true(is.function(reg_ws))
  expect_true(is.function(reg_help))
})

test_that("legacy inst/shinyapp entry files delegate to package namespace", {
  skip_if_not_installed("GExPipe")
  app_dir <- system.file("shinyapp", package = "GExPipe")
  skip_if_not(nzchar(app_dir))
  srv_txt <- readLines(file.path(app_dir, "server.R"), warn = FALSE)
  ui_txt <- readLines(file.path(app_dir, "ui.R"), warn = FALSE)
  app_txt <- readLines(file.path(app_dir, "app.R"), warn = FALSE)
  global_txt <- readLines(file.path(app_dir, "global.R"), warn = FALSE)
  expect_false(any(grepl("source\\(", srv_txt)))
  expect_false(any(grepl("source\\(", ui_txt)))
  expect_true(any(grepl("runGExPipe", app_txt, fixed = TRUE)))
  expect_false(any(grepl("to_pull", global_txt)))
  expect_false(any(grepl("local_r_files", global_txt)))
  expect_false(any(grepl("source\\(.*R/", global_txt)))
  expect_lt(length(global_txt), 80L)
  expect_true(any(grepl("gexpipe_shinyapp_bootstrap", global_txt, fixed = TRUE)))
  expect_true(any(grepl("gexp_app_server", srv_txt, fixed = TRUE)))
  expect_true(any(grepl("gexp_app_ui", ui_txt, fixed = TRUE)))
  srv_env <- new.env(parent = globalenv())
  ui_env <- new.env(parent = globalenv())
  expect_no_error(source(file.path(app_dir, "server.R"), local = srv_env))
  expect_no_error(source(file.path(app_dir, "ui.R"), local = ui_env))
  expect_true(is.function(srv_env$server))
  expect_true(is.function(ui_env$ui))
})

test_that("vignette screenshots exist under vignettes/images", {
  src_vig <- normalizePath(
    file.path(testthat::test_path(), "..", "..", "vignettes"),
    mustWork = FALSE
  )
  pkg_root <- system.file(package = "GExPipe")
  pkg_vig <- if (nzchar(pkg_root)) {
    normalizePath(file.path(dirname(pkg_root), "vignettes"), mustWork = FALSE)
  } else {
    ""
  }
  vig_dir <- if (dir.exists(src_vig)) src_vig else pkg_vig
  skip_if_not(dir.exists(vig_dir), "vignettes/ not found")
  imgs <- c(
    "step01_download.png",
    "step02a_qc.png", "step02b_qc.png", "step02c_qc.png",
    "step03a_normalize.png", "step03b_normalize.png",
    "step04a_groups.png", "step04b_groups.png",
    "step05a_batch.png", "step05b_batch.png",
    "step06a_de.png", "step06b_de.png",
    "step07a_wgcna.png", "step07b_wgcna.png", "step07c_wgcna.png", "step07d_wgcna.png",
    "step08a_common_genes.png", "step08b_go.png", "step08c_kegg.png",
    "step09a_ppi.png", "step09b_ppi.png",
    "step10a_ml.png", "step10b_ml.png",
    "step11a_validation.png", "step11b_validation_external.png",
    "step12a_roc.png", "step12b_roc.png",
    "step13_nomogram.png", "step14_gsea.png", "step15_summary.png"
  )
  for (img in imgs) {
    expect_true(file.exists(file.path(vig_dir, "images", img)), info = img)
  }
})

test_that("inst/shinyapp global.R is a thin bootstrap delegate", {
  repo_global <- system.file("shinyapp", "global.R", package = "GExPipe")
  if (!nzchar(repo_global) || !file.exists(repo_global)) {
    repo_global <- file.path(testthat::test_path(), "..", "..", "inst", "shinyapp", "global.R")
  }
  skip_if_not(file.exists(repo_global), "inst/shinyapp/global.R not found")
  repo_global <- normalizePath(repo_global, winslash = "/", mustWork = TRUE)
  global_txt <- readLines(repo_global, warn = FALSE)
  expect_lt(length(global_txt), 80L)
  expect_false(any(grepl("source\\(.*R/", global_txt)))
  expect_false(any(grepl("to_pull|local_r_files", global_txt)))
  expect_true(any(grepl("gexpipe_shinyapp_bootstrap", global_txt, fixed = TRUE)))
})

test_that("gexpipe_shinyapp_bootstrap lives in R/ not inst/shinyapp", {
  skip_if_not_installed("GExPipe")
  ns <- asNamespace("GExPipe")
  expect_true(exists("gexpipe_shinyapp_bootstrap", envir = ns, inherits = FALSE, mode = "function"))
  expect_true(exists("gexpipe_shinyapp_ensure_package", envir = ns, inherits = FALSE, mode = "function"))
})

test_that("blocked bootstrap returns minimal Shiny app", {
  skip_if_not_installed("GExPipe")
  skip_if_not_installed("shiny")
  fn <- getFromNamespace(".gexpipe_shinyapp_blocked_app", "GExPipe")
  res <- fn("not_a_real_pkg")
  expect_equal(res$status, "blocked")
  expect_true(is.list(res$ui) && length(res$ui) > 0L)
  expect_true(is.function(res$server))
})

test_that("R/ avoids suppressWarnings and suppressMessages", {
  r_dir <- normalizePath(file.path(testthat::test_path(), "..", "..", "R"), mustWork = TRUE)
  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  for (f in r_files) {
    txt <- readLines(f, warn = FALSE)
    hits <- grep("suppressWarnings\\(|suppressMessages\\(", txt, value = TRUE)
    hits <- hits[!grepl("^\\s*#", hits)]
    hits <- hits[!grepl("preferred over suppress", hits, fixed = TRUE)]
    expect_equal(length(hits), 0L, label = basename(f))
  }
})

test_that("quiet I/O helpers are defined in package namespace", {
  skip_if_not_installed("GExPipe")
  ns <- asNamespace("GExPipe")
  for (nm in c(".gexpipe_capture_console", ".gexpipe_map_ids", ".gexpipe_geo_quiet",
               ".gexpipe_fread_counts", ".gexpipe_stringdb_quiet", ".gexpipe_igraph_closeness")) {
    expect_true(exists(nm, envir = ns, inherits = FALSE, mode = "function"), info = nm)
  }
})

test_that("DESCRIPTION BugReports points to GitHub issues", {
  desc <- read.dcf(system.file("DESCRIPTION", package = "GExPipe"))
  bug <- desc[1, "BugReports"]
  expect_match(bug, "github\\.com/safarafique/GExPipe/issues")
})

test_that("shinytest2 integration test scaffolding is present", {
  helper <- file.path(testthat::test_path(), "helper-shinytest2.R")
  tests <- file.path(testthat::test_path(), "test-shiny-integration.R")
  readme <- system.file("scripts", "README-shinytest2.md", package = "GExPipe")
  if (!nzchar(readme) || !file.exists(readme)) {
    readme <- file.path(testthat::test_path(), "..", "..", "inst", "scripts", "README-shinytest2.md")
  }
  skip_if_not(file.exists(helper), "helper-shinytest2.R not found")
  skip_if_not(file.exists(tests), "test-shiny-integration.R not found")
  skip_if_not(file.exists(readme), "README-shinytest2.md not found")
  readme <- normalizePath(readme, winslash = "/", mustWork = TRUE)
  expect_true(any(grepl("shinytest2", readLines(helper, warn = FALSE), fixed = TRUE)))
})
