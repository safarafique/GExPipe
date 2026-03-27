## UI tab wrappers
## These functions lazily load the existing Shiny UI tab definitions from
## inst/shinyapp/ui/ into the package session, so `gexp_app_ui()` can rely
## purely on R-level internal functions.

.gexp_ui_cache <- new.env(parent = emptyenv())

.gexp_load_inst_ui_tab <- function(inst_rel_file, object_name) {
  p <- .gexp_inst_file(inst_rel_file)
  env <- new.env(parent = parent.frame())
  source(p, local = env)
  if (!exists(object_name, envir = env, inherits = FALSE)) {
    stop("GExPipe: expected UI object '", object_name, "' not found in ", inst_rel_file)
  }
  env[[object_name]]
}

gexp_ui_qc <- function() {
  if (exists("qc", envir = .gexp_ui_cache, inherits = FALSE)) return(.gexp_ui_cache$qc)
  gexp_app_attach_packages()
  .gexp_ui_cache$qc <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_qc.R", "ui_qc")
}

gexp_ui_normalize <- function() {
  if (exists("normalize", envir = .gexp_ui_cache, inherits = FALSE)) return(.gexp_ui_cache$normalize)
  gexp_app_attach_packages()
  .gexp_ui_cache$normalize <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_normalize.R", "ui_normalize")
}

gexp_ui_groups <- function() {
  if (exists("groups", envir = .gexp_ui_cache, inherits = FALSE)) return(.gexp_ui_cache$groups)
  gexp_app_attach_packages()
  .gexp_ui_cache$groups <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_groups.R", "ui_groups")
}

gexp_ui_batch <- function() {
  if (exists("batch", envir = .gexp_ui_cache, inherits = FALSE)) return(.gexp_ui_cache$batch)
  gexp_app_attach_packages()
  .gexp_ui_cache$batch <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_batch.R", "ui_batch")
}

gexp_ui_results <- function() {
  if (exists("results", envir = .gexp_ui_cache, inherits = FALSE)) return(.gexp_ui_cache$results)
  gexp_app_attach_packages()
  .gexp_ui_cache$results <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_results.R", "ui_results")
}

gexp_ui_wgcna <- function() {
  if (exists("wgcna", envir = .gexp_ui_cache, inherits = FALSE)) return(.gexp_ui_cache$wgcna)
  gexp_app_attach_packages()
  .gexp_ui_cache$wgcna <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_wgcna.R", "ui_wgcna")
}

gexp_ui_common_genes <- function() {
  if (exists("common_genes", envir = .gexp_ui_cache, inherits = FALSE)) return(.gexp_ui_cache$common_genes)
  gexp_app_attach_packages()
  .gexp_ui_cache$common_genes <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_common_genes.R", "ui_common_genes")
}

gexp_ui_ppi <- function() {
  if (exists("ppi", envir = .gexp_ui_cache, inherits = FALSE)) return(.gexp_ui_cache$ppi)
  gexp_app_attach_packages()
  .gexp_ui_cache$ppi <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_ppi.R", "ui_ppi")
}

gexp_ui_ml <- function() {
  if (exists("ml", envir = .gexp_ui_cache, inherits = FALSE)) return(.gexp_ui_cache$ml)
  gexp_app_attach_packages()
  .gexp_ui_cache$ml <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_ml.R", "ui_ml")
}

gexp_ui_validation <- function() {
  if (exists("validation", envir = .gexp_ui_cache, inherits = FALSE)) return(.gexp_ui_cache$validation)
  gexp_app_attach_packages()
  .gexp_ui_cache$validation <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_validation.R", "ui_validation")
}

gexp_ui_roc <- function() {
  if (exists("roc", envir = .gexp_ui_cache, inherits = FALSE)) return(.gexp_ui_cache$roc)
  gexp_app_attach_packages()
  .gexp_ui_cache$roc <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_roc.R", "ui_roc")
}

gexp_ui_nomogram <- function() {
  if (exists("nomogram", envir = .gexp_ui_cache, inherits = FALSE)) return(.gexp_ui_cache$nomogram)
  gexp_app_attach_packages()
  .gexp_ui_cache$nomogram <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_nomogram.R", "ui_nomogram")
}

gexp_ui_gsea <- function() {
  if (exists("gsea", envir = .gexp_ui_cache, inherits = FALSE)) return(.gexp_ui_cache$gsea)
  gexp_app_attach_packages()
  .gexp_ui_cache$gsea <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_gsea.R", "ui_gsea")
}

gexp_ui_results_summary <- function() {
  if (exists("results_summary", envir = .gexp_ui_cache, inherits = FALSE)) return(.gexp_ui_cache$results_summary)
  gexp_app_attach_packages()
  .gexp_ui_cache$results_summary <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_results_summary.R", "ui_results_summary")
}

