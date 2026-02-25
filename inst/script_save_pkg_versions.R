# Run this script AFTER loading OmniVerse (or after installing and loading it)
# to save the list of package versions used by the app.
# Usage: source("inst/script_save_pkg_versions.R")  (from package root)

pkgs_imports <- c(
  "shiny", "shinydashboard", "shinyjs", "DT", "Biobase", "GEOquery", "limma",
  "AnnotationDbi", "org.Hs.eg.db", "dplyr", "data.table", "edgeR", "sva",
  "ggplot2", "gridExtra", "RColorBrewer", "pheatmap", "ggrepel", "VennDiagram",
  "UpSetR", "WGCNA", "parallel", "clusterProfiler", "enrichplot", "circlize",
  "STRINGdb", "DESeq2", "igraph", "ggraph", "tidygraph", "tidyr", "randomForest",
  "caret", "e1071", "glmnet", "pROC", "kernlab", "tibble", "msigdbr", "ggpubr",
  "reshape2", "corrplot", "R.utils", "dynamicTreeCut", "scales"
)
pkgs_suggests <- c(
  "Boruta", "mixOmics", "xgboost", "SHAPforxgboost", "rms", "rmda", "cicerone",
  "biomaRt", "oligo", "affy", "knitr", "rmarkdown", "BiocCheck"
)
pkgs_all <- c(pkgs_imports, pkgs_suggests)

vers <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    as.character(packageVersion(pkg))
  } else {
    "not installed"
  }
}
out <- data.frame(
  package = pkgs_all,
  version = vapply(pkgs_all, vers, character(1)),
  stringsAsFactors = FALSE
)
out <- out[order(out$package), ]
out_file <- "inst/pkg_versions.txt"
write.table(out, out_file, sep = "\t", row.names = FALSE, quote = FALSE)
message("Saved package versions to ", out_file)
print(out, row.names = FALSE)
