## Shiny pipeline observers and outputs
##
## Internal app wiring helpers extracted from inst/shinyapp/server.R so Shiny
## logic is implemented in package R/ code and can be tested/refactored.

gexp_register_pipeline_observers <- function(input, output, session, rv) {
  # nocov start
  output$pipeline_progress <- shiny::renderUI({
    is_count_based <- isTRUE(rv$de_method %in% c("deseq2", "edger", "limma_voom"))

    steps <- list(
      list(
        id = "download", label = "Download", icon = "download", tab = "download",
        done = isTRUE(rv$download_complete), running = isTRUE(rv$download_running)
      )
    )
    steps <- c(steps, list(
      list(
        id = "qc", label = "QC", icon = "chart-bar", tab = "qc",
        done = isTRUE(rv$download_complete), running = FALSE
      )
    ))
    if (!is_count_based) {
      steps <- c(steps, list(
        list(
          id = "normalize", label = "Normalize", icon = "balance-scale", tab = "normalize",
          done = isTRUE(rv$normalization_complete), running = isTRUE(rv$normalize_running)
        )
      ))
    } else {
      steps <- c(steps, list(
        list(
          id = "normalize", label = "Norm (Auto)", icon = "magic", tab = "normalize",
          done = isTRUE(rv$normalization_complete), running = isTRUE(rv$normalize_running)
        )
      ))
    }
    de_label <- if (rv$de_method == "deseq2") {
      "DE (DESeq2)"
    } else if (rv$de_method == "edger") {
      "DE (edgeR)"
    } else if (rv$de_method == "limma_voom") {
      "DE (limma-voom)"
    } else {
      "DE (limma)"
    }
    steps <- c(steps, list(
      list(id = "groups", label = "Groups", icon = "users", tab = "groups", done = isTRUE(rv$groups_applied), running = FALSE),
      list(id = "batch", label = "Batch", icon = "filter", tab = "batch", done = isTRUE(rv$batch_complete), running = isTRUE(rv$batch_running)),
      list(id = "de", label = de_label, icon = "dna", tab = "results", done = !is.null(rv$de_results), running = isTRUE(rv$de_running)),
      list(id = "wgcna", label = "WGCNA", icon = "project-diagram", tab = "wgcna", done = isTRUE(rv$wgcna_complete), running = isTRUE(rv$wgcna_running)),
      list(id = "common", label = "Common Genes", icon = "venus-double", tab = "common_genes", done = length(rv$common_genes_de_wgcna) > 0, running = FALSE),
      list(id = "ppi", label = "PPI", icon = "project-diagram", tab = "ppi", done = isTRUE(rv$ppi_complete), running = FALSE),
      list(id = "ml", label = "ML", icon = "brain", tab = "ml", done = isTRUE(rv$ml_complete), running = FALSE),
      list(
        id = "validation", label = "Validation", icon = "shield-alt", tab = "validation",
        done = !is.null(rv$validation_mode) && (rv$validation_mode == "internal" || !is.null(rv$external_validation_expr)),
        running = FALSE
      ),
      list(id = "roc", label = "ROC", icon = "chart-line", tab = "roc", done = !is.null(rv$ml_common_genes) && length(rv$ml_common_genes) > 0, running = FALSE),
      list(id = "nomogram", label = "Nomogram", icon = "calculator", tab = "nomogram", done = isTRUE(rv$nomogram_complete), running = FALSE),
      list(id = "gsea", label = "GSEA", icon = "project-diagram", tab = "gsea", done = isTRUE(rv$gsea_complete), running = FALSE)
    ))
    # Defensive cleanup for legacy state/code paths where an Immune chip may still be present.
    steps <- Filter(function(s) {
      sid <- tolower(if (is.null(s$id)) "" else as.character(s$id))
      lab <- tolower(if (is.null(s$label)) "" else as.character(s$label))
      !(sid == "immune" || grepl("immune", lab, fixed = TRUE))
    }, steps)

    n_done <- sum(vapply(steps, function(s) isTRUE(s$done), logical(1)))
    n_total <- length(steps)
    pct <- round(n_done / n_total * 100)

    step_chips <- lapply(steps, function(s) {
      if (isTRUE(s$running)) {
        cls <- "pipeline-step running"
        ic <- "spinner fa-spin"
      } else if (isTRUE(s$done)) {
        cls <- "pipeline-step done"
        ic <- "check-circle"
      } else {
        cls <- "pipeline-step pending"
        ic <- s$icon
      }
      shiny::tags$span(
        class = cls,
        `data-tab` = s$tab,
        title = paste0("Go to: ", s$label),
        shiny::tags$i(class = paste0("fa fa-", ic, " step-icon")),
        s$label
      )
    })

    shiny::tags$div(
      class = "pipeline-tracker",
      shiny::tags$div(
        class = "pipeline-tracker-header",
        shiny::tags$span(class = "pipeline-tracker-title", shiny::icon("tasks"), " Pipeline Progress"),
        shiny::tags$span(class = "pipeline-tracker-pct", paste0(n_done, " / ", n_total, " steps - ", pct, "%"))
      ),
      shiny::tags$div(
        class = "progress",
        shiny::tags$div(
          class = "progress-bar",
          role = "progressbar",
          style = paste0("width: ", pct, "%;"),
          `aria-valuenow` = pct,
          `aria-valuemin` = "0",
          `aria-valuemax` = "100"
        )
      ),
      shiny::tags$div(class = "pipeline-steps-row", step_chips)
    )
  })

  shiny::observe({
    shinyjs::toggleState("apply_normalization", condition = isTRUE(rv$download_complete))
    shinyjs::toggleState("extract_groups_btn", condition = isTRUE(rv$download_complete))
    shinyjs::toggleState("apply_groups_btn", condition = isTRUE(rv$download_complete))
    shinyjs::toggleState("apply_batch", condition = isTRUE(rv$groups_applied))
    shinyjs::toggleState("run_de", condition = isTRUE(rv$batch_complete))
    shinyjs::toggleState("prepare_wgcna", condition = isTRUE(rv$batch_complete))
    shinyjs::toggleState("pick_soft_threshold", condition = isTRUE(rv$wgcna_prepared))
    shinyjs::toggleState("run_wgcna", condition = isTRUE(rv$wgcna_prepared))
    shinyjs::toggleState("calculate_module_trait", condition = isTRUE(rv$wgcna_complete))
    shinyjs::toggleState("calculate_me_relationships", condition = isTRUE(rv$wgcna_complete))
    shinyjs::toggleState("identify_significant_modules", condition = isTRUE(rv$wgcna_complete))
    shinyjs::toggleState("compute_common_genes", condition = !is.null(rv$de_results) && isTRUE(rv$wgcna_complete))
    shinyjs::toggleState("run_go_enrichment", condition = length(rv$common_genes_de_wgcna) > 0)
    shinyjs::toggleState("run_kegg_enrichment", condition = length(rv$common_genes_de_wgcna) > 0)
    shinyjs::toggleState("extract_ml_data_common_genes", condition = length(rv$common_genes_de_wgcna) > 0)
    shinyjs::toggleState("run_ppi", condition = length(rv$common_genes_de_wgcna) > 0)
    shinyjs::toggleState("extract_ml_data", condition = isTRUE(rv$ppi_complete))
    shinyjs::toggleState("run_ml", condition = !is.null(rv$extracted_data_ml))
    shinyjs::toggleState(
      "run_nomogram",
      condition = isTRUE(rv$batch_complete) &&
        (length(rv$ml_common_genes) > 0 || length(rv$common_genes_de_wgcna) > 0)
    )
    shinyjs::toggleState(
      "run_gsea",
      condition = isTRUE(rv$batch_complete) &&
        (length(rv$ml_common_genes) > 0 || length(rv$common_genes_de_wgcna) > 0)
    )
  })
  # nocov end
}
