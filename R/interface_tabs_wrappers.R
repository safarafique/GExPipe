## UI tab wrappers
## These functions lazily load the existing Shiny UI tab definitions from
## inst/shinyapp/ui/ into the package session, so `gexp_app_ui()` can rely
## purely on R-level internal functions.

.gexp_ui_cache <- new.env(parent = emptyenv())

.gexp_inject_ui_plot_helpers <- function(env) {
  ns_funs <- c(
    "gexp_ui_plot_download_jpg_pdf", "gexp_ui_plot_download_bar",
    "gexp_plot_device_open", "gexp_ggsave_from_file"
  )
  if (requireNamespace("GExPipe", quietly = TRUE)) {
    pkg_ns <- asNamespace("GExPipe")
    ml_funs <- c(
      "gexp_ml_venn_sets_for_selected", "gexp_ml_common_gene_count",
      "gexp_draw_ml_methods_venn", "gexp_ml_method_display_names"
    )
    ns_funs <- c(ns_funs, ml_funs)
    for (nm in ns_funs) {
      if (exists(nm, envir = pkg_ns, inherits = FALSE, mode = "function")) {
        env[[nm]] <- get(nm, envir = pkg_ns, mode = "function")
      }
    }
  }
  for (nm in ns_funs) {
    if (!exists(nm, envir = env, inherits = FALSE) &&
        exists(nm, mode = "function", inherits = TRUE)) {
      env[[nm]] <- get(nm, mode = "function", inherits = TRUE)
    }
  }
  helper_file <- .gexp_inst_file("shiny_src/ui/ui_plot_helpers.R")
  if (file.exists(helper_file)) {
    tryCatch(source(helper_file, local = env), error = function(e) NULL)
  }
  if (!exists("gexp_ui_plot_download_jpg_pdf", envir = env, inherits = FALSE)) {
    assign("gexp_ui_plot_download_jpg_pdf", function(jpg_id, pdf_id, btn_class = "btn-success btn-sm") {
      shiny::tags$div(
        class = "gexp-plot-download-bar",
        style = "margin-top: 8px;",
        shiny::downloadButton(jpg_id, shiny::tagList(shiny::icon("download"), " JPG (300 DPI)"), class = btn_class, style = "margin-right: 6px;"),
        shiny::downloadButton(pdf_id, shiny::tagList(shiny::icon("download"), " PDF"), class = btn_class)
      )
    }, envir = env)
  }
  if (!exists("gexp_ui_plot_download_bar", envir = env, inherits = FALSE)) {
    assign("gexp_ui_plot_download_bar", function(png_id, jpg_id, pdf_id, btn_class = "btn-success btn-sm") {
      shiny::tags$div(
        class = "gexp-plot-download-bar",
        style = "margin-top: 8px;",
        shiny::downloadButton(png_id, shiny::tagList(shiny::icon("download"), " PNG (300 DPI)"), class = btn_class, style = "margin-right: 6px;"),
        shiny::downloadButton(jpg_id, shiny::tagList(shiny::icon("download"), " JPG (300 DPI)"), class = btn_class, style = "margin-right: 6px;"),
        shiny::downloadButton(pdf_id, shiny::tagList(shiny::icon("download"), " PDF"), class = btn_class)
      )
    }, envir = env)
  }
  invisible(env)
}

.gexp_load_inst_ui_tab <- function(inst_rel_file, object_name) {
  p <- .gexp_inst_file(inst_rel_file)
  env <- new.env(parent = parent.frame())
  .gexp_inject_ui_plot_helpers(env)
  source(p, local = env)
  if (!exists(object_name, envir = env, inherits = FALSE)) {
    stop("GExPipe: expected UI object '", object_name, "' not found in ", inst_rel_file)
  }
  env[[object_name]]
}

gexp_ui_qc <- function() {
  if (exists("qc", envir = .gexp_ui_cache, inherits = FALSE)) {
    return(.gexp_ui_cache$qc)
  }
  gexp_app_attach_packages()
  .gexp_ui_cache$qc <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_qc.R", "ui_qc")
}

gexp_ui_normalize <- function() {
  if (exists("normalize", envir = .gexp_ui_cache, inherits = FALSE)) {
    return(.gexp_ui_cache$normalize)
  }
  gexp_app_attach_packages()
  .gexp_ui_cache$normalize <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_normalize.R", "ui_normalize")
}

gexp_ui_groups <- function() {
  if (exists("groups", envir = .gexp_ui_cache, inherits = FALSE)) {
    return(.gexp_ui_cache$groups)
  }
  gexp_app_attach_packages()
  .gexp_ui_cache$groups <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_groups.R", "ui_groups")
}

gexp_ui_batch <- function() {
  if (exists("batch", envir = .gexp_ui_cache, inherits = FALSE)) {
    return(.gexp_ui_cache$batch)
  }
  gexp_app_attach_packages()
  .gexp_ui_cache$batch <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_batch.R", "ui_batch")
}

gexp_ui_results <- function() {
  if (exists("results", envir = .gexp_ui_cache, inherits = FALSE)) {
    return(.gexp_ui_cache$results)
  }
  gexp_app_attach_packages()
  .gexp_ui_cache$results <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_results.R", "ui_results")
}

gexp_ui_wgcna <- function() {
  if (exists("wgcna", envir = .gexp_ui_cache, inherits = FALSE)) {
    return(.gexp_ui_cache$wgcna)
  }
  gexp_app_attach_packages()
  .gexp_ui_cache$wgcna <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_wgcna.R", "ui_wgcna")
}

gexp_ui_common_genes <- function() {
  if (exists("common_genes", envir = .gexp_ui_cache, inherits = FALSE)) {
    return(.gexp_ui_cache$common_genes)
  }
  gexp_app_attach_packages()
  .gexp_ui_cache$common_genes <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_common_genes.R", "ui_common_genes")
}

gexp_ui_ppi <- function() {
  if (exists("ppi", envir = .gexp_ui_cache, inherits = FALSE)) {
    return(.gexp_ui_cache$ppi)
  }
  gexp_app_attach_packages()
  .gexp_ui_cache$ppi <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_ppi.R", "ui_ppi")
}

gexp_ui_ml <- function() {
  if (exists("ml", envir = .gexp_ui_cache, inherits = FALSE)) {
    return(.gexp_ui_cache$ml)
  }
  gexp_app_attach_packages()
  .gexp_ui_cache$ml <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_ml.R", "ui_ml")
}

gexp_ui_validation <- function() {
  if (exists("validation", envir = .gexp_ui_cache, inherits = FALSE)) {
    return(.gexp_ui_cache$validation)
  }
  gexp_app_attach_packages()
  .gexp_ui_cache$validation <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_validation.R", "ui_validation")
}

gexp_ui_roc <- function() {
  if (exists("roc", envir = .gexp_ui_cache, inherits = FALSE)) {
    return(.gexp_ui_cache$roc)
  }
  gexp_app_attach_packages()
  .gexp_ui_cache$roc <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_roc.R", "ui_roc")
}

gexp_ui_nomogram <- function() {
  if (exists("nomogram", envir = .gexp_ui_cache, inherits = FALSE)) {
    return(.gexp_ui_cache$nomogram)
  }
  gexp_app_attach_packages()
  .gexp_ui_cache$nomogram <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_nomogram.R", "ui_nomogram")
}

gexp_ui_gsea <- function() {
  if (exists("gsea", envir = .gexp_ui_cache, inherits = FALSE)) {
    return(.gexp_ui_cache$gsea)
  }
  gexp_app_attach_packages()
  .gexp_ui_cache$gsea <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_gsea.R", "ui_gsea")
}

gexp_ui_results_summary <- function() {
  if (exists("results_summary", envir = .gexp_ui_cache, inherits = FALSE)) {
    return(.gexp_ui_cache$results_summary)
  }
  gexp_app_attach_packages()
  .gexp_ui_cache$results_summary <- .gexp_load_inst_ui_tab("shiny_src/ui/ui_results_summary.R", "ui_results_summary")
}

