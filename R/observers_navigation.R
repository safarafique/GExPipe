## Shiny tab navigation observers and QC next-button UI
##
## Extracted from inst/shinyapp/server.R for package R/ organization.

gexp_register_navigation_observers <- function(input, output, session, rv) {
  # nocov start
  output$qc_next_button <- shiny::renderUI({
    if (!is.null(input$de_method) && input$de_method %in% c("deseq2", "edger", "limma_voom")) {
      shiny::actionButton(
        "next_to_normalize",
        shiny::tagList(shiny::icon("arrow-right"), " Next: Select Groups (Normalize auto-handled)"),
        class = "btn-success btn-lg",
        style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;"
      )
    } else {
      shiny::actionButton(
        "next_to_normalize", "Next: Normalize Data",
        icon = shiny::icon("arrow-right"), class = "btn-success btn-lg",
        style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;"
      )
    }
  })

  shiny::observeEvent(input$next_page_download, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "qc")
  })
  shiny::observeEvent(input$next_to_normalize, {
    if (!is.null(input$de_method) && input$de_method %in% c("deseq2", "edger", "limma_voom")) {
      shinydashboard::updateTabItems(session, "sidebar_menu", "groups")
    } else {
      shinydashboard::updateTabItems(session, "sidebar_menu", "normalize")
    }
  })
  shiny::observeEvent(input$next_page_normalize, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "groups")
  })
  shiny::observeEvent(input$go_to_groups, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "groups")
  })
  shiny::observeEvent(input$go_to_groups_from_norm, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "groups")
  })
  shiny::observeEvent(input$next_page_groups, {
    if (isTRUE(rv$single_dataset)) {
      shinydashboard::updateTabItems(session, "sidebar_menu", "results")
    } else {
      shinydashboard::updateTabItems(session, "sidebar_menu", "batch")
    }
  })
  shiny::observeEvent(input$next_to_batch_btn, {
    if (isTRUE(rv$single_dataset)) {
      shinydashboard::updateTabItems(session, "sidebar_menu", "results")
    } else {
      shinydashboard::updateTabItems(session, "sidebar_menu", "batch")
    }
  })
  shiny::observeEvent(input$go_to_results, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "results")
  })
  shiny::observeEvent(input$next_page_batch, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "results")
  })
  shiny::observeEvent(input$next_page_results, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "wgcna")
  })
  shiny::observeEvent(input$next_page_wgcna, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "common_genes")
  })
  shiny::observeEvent(input$next_page_common_genes_end, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "ppi")
  })
  shiny::observeEvent(input$next_page_common_genes_to_ml, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "ml")
  })
  shiny::observeEvent(input$next_page_ppi, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "ml")
  })
  shiny::observeEvent(input$next_page_ml, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "download")
  })
  shiny::observeEvent(input$next_page_ml_to_roc, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "validation")
  })
  shiny::observeEvent(input$next_page_ml_to_validation, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "validation")
  })
  shiny::observeEvent(input$next_page_roc, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "download")
  })
  shiny::observeEvent(input$next_page_roc_to_nomogram, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "nomogram")
  })
  shiny::observeEvent(input$next_page_roc_to_gsea, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "gsea")
  })
  shiny::observeEvent(input$next_page_nomogram_to_gsea, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "gsea")
  })
  shiny::observeEvent(input$next_page_nomogram_to_results, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "results_summary")
  })
  shiny::observeEvent(input$next_page_gsea, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "download")
  })
  shiny::observeEvent(input$next_page_gsea_to_results, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "results_summary")
  })
  shiny::observeEvent(input$next_page_roc_to_results, {
    shinydashboard::updateTabItems(session, "sidebar_menu", "results_summary")
  })
  # nocov end
}
