# Reproducibility helpers added after the SLE case-study review.

test_that("glmnet fold IDs are fixed by the seed and match cv.glmnet's own default draw", {
  f1 <- GExPipe:::.gexpipe_glmnet_foldid(60L)
  f2 <- GExPipe:::.gexpipe_glmnet_foldid(60L)
  expect_identical(f1, f2)
  expect_length(f1, 60L)
  expect_setequal(unique(f1), 1:10)
  # Same draw cv.glmnet makes internally after set.seed(123), so in-session
  # results are unchanged while the subprocess fallback now matches them.
  ref <- withr::with_seed(123L, sample(rep(seq(10L), length = 60L)))
  expect_identical(f1, ref)
  expect_false(identical(f1, GExPipe:::.gexpipe_glmnet_foldid(60L, seed = 124L)))
})

test_that("cv.glmnet gives the same lambda path and selection on repeated runs", {
  skip_if_not_installed("glmnet")
  set.seed(1)
  x <- matrix(stats::rnorm(60 * 8), 60, 8, dimnames = list(NULL, paste0("G", 1:8)))
  y <- factor(rep(0:1, each = 30))
  a <- GExPipe:::.gexpipe_glmnet_cv_fit(x, y, alpha = 1)
  stats::runif(5)  # disturb the global RNG between runs
  b <- GExPipe:::.gexpipe_glmnet_cv_fit(x, y, alpha = 1)
  expect_identical(a$lambda.min, b$lambda.min)
  expect_identical(a$cvm, b$cvm)
})

test_that("gene ranking breaks ties alphabetically and ignores input order", {
  assoc <- c(ISG15 = 0.92314, IFI44L = 0.92307, OAS1 = 0.80)
  expect_identical(GExPipe:::gexp_rank_genes_det(assoc), c("IFI44L", "ISG15", "OAS1"))
  expect_identical(GExPipe:::gexp_rank_genes_det(rev(assoc)), c("IFI44L", "ISG15", "OAS1"))
  # A real difference at 4 d.p. still wins
  expect_identical(GExPipe:::gexp_rank_genes_det(c(B = 0.9235, A = 0.9230)), c("B", "A"))
})

test_that("run manifest is a Field/Value table with extras and package versions", {
  rv <- list(analysis_type = "parallel", ml_common_genes = c("ISG15", "OAS1"),
             wgcna_settings_used = data.frame(Power = 12, Min_Module_Size = 30),
             ppi_string_info = data.frame(STRING_Version = "12.0", Fallback_Used = FALSE))
  m <- GExPipe:::gexp_run_manifest(rv, list(Nomogram_Seed = 123L, Validation_Sample_IDs = c("GSM1", "GSM2")))
  expect_named(m, c("Field", "Value"))
  v <- stats::setNames(m$Value, m$Field)
  expect_identical(v[["ML_Common_Genes"]], "ISG15;OAS1")
  expect_identical(v[["Validation_Sample_IDs"]], "GSM1;GSM2")
  expect_match(v[["PPI_STRING"]], "STRING_Version=12.0")
  expect_match(v[["WGCNA_Settings"]], "Power=12")
  expect_true("Package_GExPipe" %in% m$Field)
})
