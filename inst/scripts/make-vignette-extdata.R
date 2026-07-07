# Generate vignette example data for GExPipe
#
# Source: synthetic demonstration data (not derived from GEO or other restricted
# datasets). Intended for vignette and examples only.
# License: same as the GExPipe package (MIT).
#
# Run from package root:
#   Rscript inst/scripts/make-vignette-extdata.R

set.seed(42)

n_genes <- 120
n_samples <- 12
genes <- sprintf("G%04d", seq_len(n_genes))
samples <- sprintf("S%02d", seq_len(n_samples))

expr <- matrix(stats::rnorm(n_genes * n_samples, sd = 1.5), nrow = n_genes, ncol = n_samples)
expr <- round(expr, 4)
colnames(expr) <- samples
rownames(expr) <- genes

meta <- data.frame(
  SampleID = samples,
  Dataset = rep(c("D1", "D2"), each = n_samples / 2),
  Condition = rep(c("Normal", "Normal", "Disease", "Disease"), times = 3),
  stringsAsFactors = FALSE
)

out_dir <- file.path("inst", "extdata")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

expr_path <- file.path(out_dir, "vignette_expression.csv")
meta_path <- file.path(out_dir, "vignette_sample_metadata.csv")

expr_df <- data.frame(Gene = genes, expr, check.names = FALSE, stringsAsFactors = FALSE)
utils::write.csv(expr_df, expr_path, row.names = FALSE)
utils::write.csv(meta, meta_path, row.names = FALSE)

message("Wrote ", expr_path)
message("Wrote ", meta_path)
