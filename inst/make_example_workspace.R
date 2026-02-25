# Create a small example workspace for OmniVerse vignettes/docs.
# Run from package root: Rscript inst/make_example_workspace.R

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

set.seed(1)

genes <- sprintf("Gene%03d", 1:200)
micro_samples <- sprintf("ExampleMicro_S%02d", 1:6)
rna_samples <- sprintf("ExampleRNA_S%02d", 1:6)
samples <- c(micro_samples, rna_samples)

# A toy expression matrix (genes x samples)
expr <- matrix(
  rnorm(length(genes) * length(samples), mean = 6, sd = 1),
  nrow = length(genes),
  dimnames = list(genes, samples)
)

# Make the RNA-seq half look count-like (integers)
expr[, 7:12] <- round(pmax(0, exp(expr[, 7:12])), 0)

micro_expr <- expr[, 1:6, drop = FALSE]
rna_counts <- expr[, 7:12, drop = FALSE]

all_genes_list <- list(ExampleMicro = genes, ExampleRNA = genes)

unified_metadata <- data.frame(
  SampleID = samples,
  Condition = rep(c("Normal", "Disease"), each = 3, times = 2),
  Dataset = rep(c("ExampleMicro", "ExampleRNA"), c(6, 6)),
  stringsAsFactors = FALSE
)
rownames(unified_metadata) <- samples

# Minimal state that enables the QC tab when loaded
state <- list(
  saved_step = "qc",
  disease_name = "ExampleDisease",
  all_genes_list = all_genes_list,
  common_genes = genes,
  micro_expr_list = list(ExampleMicro = micro_expr),
  rna_counts_list = list(ExampleRNA = rna_counts),
  combined_expr_raw = expr,
  combined_expr = expr,
  unified_metadata = unified_metadata
)

out_file <- "inst/extdata/omniverse_example_workspace.rds"
saveRDS(state, out_file)
cat("Wrote ", out_file, "\n", sep = "")

