## Server module wrappers
## These functions lazily load the existing Shiny server modules from
## inst/shinyapp/server/ and expose them as R-level functions named
## server_* to be called by `gexp_app_server()`.

.gexp_server_cache <- new.env(parent = emptyenv())

.gexp_load_inst_server_module <- function(inst_rel_file, object_name) {
  p <- .gexp_inst_file(inst_rel_file)
  env <- new.env(parent = parent.frame())
  source(p, local = env)
  if (!exists(object_name, envir = env, inherits = FALSE)) {
    stop("GExPipe: expected server object '", object_name, "' not found in ", inst_rel_file)
  }
  env[[object_name]]
}

server_download <- function(input, output, session, rv) {
  if (!exists("download", envir = .gexp_server_cache, inherits = FALSE)) {
    gexp_app_attach_packages()
    .gexp_server_cache$download <- .gexp_load_inst_server_module("shiny_src/server/server_download.R", "server_download")
  }
  .gexp_server_cache$download(input, output, session, rv)
}

server_qc <- function(input, output, session, rv) {
  if (!exists("qc", envir = .gexp_server_cache, inherits = FALSE)) {
    gexp_app_attach_packages()
    .gexp_server_cache$qc <- .gexp_load_inst_server_module("shiny_src/server/server_qc.R", "server_qc")
  }
  .gexp_server_cache$qc(input, output, session, rv)
}

server_normalize <- function(input, output, session, rv) {
  if (!exists("normalize", envir = .gexp_server_cache, inherits = FALSE)) {
    gexp_app_attach_packages()
    .gexp_server_cache$normalize <- .gexp_load_inst_server_module("shiny_src/server/server_normalize.R", "server_normalize")
  }
  .gexp_server_cache$normalize(input, output, session, rv)
}

server_groups <- function(input, output, session, rv) {
  if (!exists("groups", envir = .gexp_server_cache, inherits = FALSE)) {
    gexp_app_attach_packages()
    .gexp_server_cache$groups <- .gexp_load_inst_server_module("shiny_src/server/server_groups.R", "server_groups")
  }
  .gexp_server_cache$groups(input, output, session, rv)
}

server_batch <- function(input, output, session, rv) {
  if (!exists("batch", envir = .gexp_server_cache, inherits = FALSE)) {
    gexp_app_attach_packages()
    .gexp_server_cache$batch <- .gexp_load_inst_server_module("shiny_src/server/server_batch.R", "server_batch")
  }
  .gexp_server_cache$batch(input, output, session, rv)
}

server_results <- function(input, output, session, rv) {
  if (!exists("results", envir = .gexp_server_cache, inherits = FALSE)) {
    gexp_app_attach_packages()
    .gexp_server_cache$results <- .gexp_load_inst_server_module("shiny_src/server/server_results.R", "server_results")
  }
  .gexp_server_cache$results(input, output, session, rv)
}

server_wgcna <- function(input, output, session, rv) {
  if (!exists("wgcna", envir = .gexp_server_cache, inherits = FALSE)) {
    gexp_app_attach_packages()
    .gexp_server_cache$wgcna <- .gexp_load_inst_server_module("shiny_src/server/server_wgcna.R", "server_wgcna")
  }
  .gexp_server_cache$wgcna(input, output, session, rv)
}

server_common_genes <- function(input, output, session, rv) {
  if (!exists("common_genes", envir = .gexp_server_cache, inherits = FALSE)) {
    gexp_app_attach_packages()
    .gexp_server_cache$common_genes <- .gexp_load_inst_server_module("shiny_src/server/server_common_genes.R", "server_common_genes")
  }
  .gexp_server_cache$common_genes(input, output, session, rv)
}

server_ppi <- function(input, output, session, rv) {
  if (!exists("ppi", envir = .gexp_server_cache, inherits = FALSE)) {
    gexp_app_attach_packages()
    .gexp_server_cache$ppi <- .gexp_load_inst_server_module("shiny_src/server/server_ppi.R", "server_ppi")
  }
  .gexp_server_cache$ppi(input, output, session, rv)
}

server_ml <- function(input, output, session, rv) {
  if (!exists("ml", envir = .gexp_server_cache, inherits = FALSE)) {
    gexp_app_attach_packages()
    .gexp_server_cache$ml <- .gexp_load_inst_server_module("shiny_src/server/server_ml.R", "server_ml")
  }
  .gexp_server_cache$ml(input, output, session, rv)
}

server_validation <- function(input, output, session, rv) {
  if (!exists("validation", envir = .gexp_server_cache, inherits = FALSE)) {
    gexp_app_attach_packages()
    .gexp_server_cache$validation <- .gexp_load_inst_server_module("shiny_src/server/server_validation.R", "server_validation")
  }
  .gexp_server_cache$validation(input, output, session, rv)
}

server_roc <- function(input, output, session, rv) {
  if (!exists("roc", envir = .gexp_server_cache, inherits = FALSE)) {
    gexp_app_attach_packages()
    .gexp_server_cache$roc <- .gexp_load_inst_server_module("shiny_src/server/server_roc.R", "server_roc")
  }
  .gexp_server_cache$roc(input, output, session, rv)
}

server_nomogram <- function(input, output, session, rv) {
  if (!exists("nomogram", envir = .gexp_server_cache, inherits = FALSE)) {
    gexp_app_attach_packages()
    .gexp_server_cache$nomogram <- .gexp_load_inst_server_module("shiny_src/server/server_nomogram.R", "server_nomogram")
  }
  .gexp_server_cache$nomogram(input, output, session, rv)
}

server_gsea <- function(input, output, session, rv) {
  if (!exists("gsea", envir = .gexp_server_cache, inherits = FALSE)) {
    gexp_app_attach_packages()
    .gexp_server_cache$gsea <- .gexp_load_inst_server_module("shiny_src/server/server_gsea.R", "server_gsea")
  }
  .gexp_server_cache$gsea(input, output, session, rv)
}

server_results_summary <- function(input, output, session, rv) {
  if (!exists("results_summary", envir = .gexp_server_cache, inherits = FALSE)) {
    gexp_app_attach_packages()
    .gexp_server_cache$results_summary <- .gexp_load_inst_server_module("shiny_src/server/server_results_summary.R", "server_results_summary")
  }
  .gexp_server_cache$results_summary(input, output, session, rv)
}

