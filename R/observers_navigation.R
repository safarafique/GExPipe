## Shiny tab navigation observers and QC next-button UI
##
## Extracted from inst/shinyapp/server.R for package R/ organization.

gexp_register_navigation_observers <- function(input, output, session, rv) {
  # nocov start
  .gexp_goto_tab <- function(tab) {
    if (is.null(tab) || !nzchar(tab)) return()
    shinydashboard::updateTabItems(session, "sidebar_menu", tab)
    tab <- gsub("[^A-Za-z0-9_]", "", as.character(tab)[[1]])
    js <- sprintf(
      paste(
        "var tab = '%s';",
        "if (typeof gexpClickSidebarTab === 'function') { gexpClickSidebarTab(tab); }",
        "else if (window.Shiny && Shiny.setInputValue) {",
        "  Shiny.setInputValue('sidebar_menu', tab, {priority: 'event'});",
        "  var link = $('a[data-value=\"' + tab + '\"]').first();",
        "  if (link.length && link[0].click) link[0].click();",
        "}",
        "window.scrollTo(0, 0);"
      ),
      tab
    )
    try(shinyjs::runjs(js), silent = TRUE)
  }

  output$qc_next_button <- shiny::renderUI({
    shiny::actionButton(
      "next_to_normalize",
      shiny::tagList(shiny::icon("arrow-right"), " Next: Select Groups"),
      class = "btn-success btn-lg",
      style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;"
    )
  })

  shiny::observeEvent(input$next_page_download, {
    .gexp_goto_tab("normalize")
  })
  shiny::observeEvent(input$next_to_normalize, {
    .gexp_goto_tab("groups")
  })
  shiny::observeEvent(input$next_page_normalize, {
    .gexp_goto_tab("qc")
  })
  shiny::observeEvent(input$next_page_normalize_parallel, {
    .gexp_goto_tab("qc")
  })
  shiny::observeEvent(input$go_to_groups, {
    .gexp_goto_tab("qc")
  })
  shiny::observeEvent(input$go_to_groups_from_norm, {
    .gexp_goto_tab("qc")
  })
  shiny::observeEvent(input$next_page_groups, {
    # Always land on Step 5 - even with a single dataset, there can be a
    # technical batch effect (extraction date, sequencing lane, processing
    # day) worth checking via the diagnostic panel shown there.
    .gexp_goto_tab("batch")
  })
  shiny::observeEvent(input$next_to_batch_btn, {
    .gexp_goto_tab("batch")
  })
  shiny::observeEvent(input$go_to_results, {
    .gexp_goto_tab("results")
  })
  shiny::observeEvent(input$next_page_batch, {
    .gexp_goto_tab("results")
  })
  shiny::observeEvent(input$next_page_batch_end, {
    .gexp_goto_tab("results")
  })
  shiny::observeEvent(input$next_page_results, {
    .gexp_goto_tab(if (isTRUE(rv$merge_after_de)) "consensus" else "wgcna")
  })
  shiny::observeEvent(input$next_page_results_parallel, {
    .gexp_goto_tab("consensus")
  })
  shiny::observeEvent(input$next_page_results_parallel_end, {
    .gexp_goto_tab("consensus")
  })
  shiny::observeEvent(input$next_page_consensus, {
    if (isTRUE(rv$merge_after_de) && !isTRUE(rv$consensus_complete)) {
      shiny::showNotification(
        "Apply Step 7 (RNA-seq \u2229 microarray) first. WGCNA does not use that DEG list; Step 9 does.",
        type = "warning",
        duration = 6
      )
      return()
    }
    .gexp_goto_tab("wgcna")
  })
  shiny::observeEvent(input$next_page_wgcna, {
    .gexp_goto_tab("common_genes")
  })
  shiny::observeEvent(input$next_page_common_genes_end, {
    .gexp_goto_tab("ppi")
  })
  shiny::observeEvent(input$next_page_common_genes_to_ml, {
    .gexp_goto_tab("ml")
  })
  shiny::observeEvent(input$next_page_ppi, {
    .gexp_goto_tab("ml")
  })
  shiny::observeEvent(input$next_page_ml, {
    .gexp_goto_tab("validation")
  })
  shiny::observeEvent(input$next_page_ml_to_roc, {
    .gexp_goto_tab("validation")
  })
  shiny::observeEvent(input$next_page_ml_to_validation, {
    .gexp_goto_tab("validation")
  })
  shiny::observeEvent(input$next_page_roc, {
    .gexp_goto_tab("nomogram")
  })
  shiny::observeEvent(input$next_page_roc_to_nomogram, {
    .gexp_goto_tab("nomogram")
  })
  shiny::observeEvent(input$next_page_roc_to_gsea, {
    .gexp_goto_tab("gsea")
  })
  shiny::observeEvent(input$next_page_nomogram_to_gsea, {
    .gexp_goto_tab("gsea")
  })
  shiny::observeEvent(input$next_page_nomogram_to_results, {
    .gexp_goto_tab("results_summary")
  })
  shiny::observeEvent(input$next_page_gsea, {
    .gexp_goto_tab("results_summary")
  })
  shiny::observeEvent(input$next_page_gsea_to_results, {
    .gexp_goto_tab("results_summary")
  })
  shiny::observeEvent(input$next_page_roc_to_results, {
    .gexp_goto_tab("results_summary")
  })
  shiny::observeEvent(input$next_page_validation_to_roc, {
    .gexp_goto_tab("roc")
  })
  # nocov end
}
