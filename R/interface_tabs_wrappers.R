## Shiny UI tab modules live in R/ui_*.R (installed with the package).
## Thin accessors attach dependencies then return the tab UI object.

.gexp_ui_tab <- function(obj_name) {
  gexp_app_attach_packages()
  obj <- get(obj_name, envir = asNamespace("GExPipe"), inherits = FALSE)
  force(obj)
}

gexp_ui_qc <- function() .gexp_ui_tab("ui_qc")
gexp_ui_normalize <- function() .gexp_ui_tab("ui_normalize")
gexp_ui_groups <- function() .gexp_ui_tab("ui_groups")
gexp_ui_batch <- function() .gexp_ui_tab("ui_batch")
gexp_ui_results <- function() .gexp_ui_tab("ui_results")
gexp_ui_wgcna <- function() .gexp_ui_tab("ui_wgcna")
gexp_ui_common_genes <- function() .gexp_ui_tab("ui_common_genes")
gexp_ui_ppi <- function() .gexp_ui_tab("ui_ppi")
gexp_ui_ml <- function() .gexp_ui_tab("ui_ml")
gexp_ui_validation <- function() .gexp_ui_tab("ui_validation")
gexp_ui_roc <- function() .gexp_ui_tab("ui_roc")
gexp_ui_nomogram <- function() .gexp_ui_tab("ui_nomogram")
gexp_ui_gsea <- function() .gexp_ui_tab("ui_gsea")
gexp_ui_results_summary <- function() .gexp_ui_tab("ui_results_summary")
