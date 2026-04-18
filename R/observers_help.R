## Help / connectivity observers for the GExPipe Shiny app
##
## Extracted from inst/shinyapp/server.R.

gexp_user_guideline_modal_ui <- function() {
  shiny::tags$div(
    style = "max-height: 75vh; overflow-y: auto; padding-right: 8px;",
    shiny::tags$h4(
      shiny::icon("info-circle"), " About GExPipe",
      style = "color: #1e293b; margin-top: 0; border-bottom: 2px solid #667eea; padding-bottom: 6px;"
    ),
    shiny::tags$p(
      "GExPipe (Gene Expression Pipeline) is a Shiny app for end-to-end analysis of bulk RNA-seq and microarray data. ",
      "You can download data from GEO, run quality control, normalize, correct for batch effects, perform differential expression (limma, DESeq2, edgeR), ",
      "build co-expression networks (WGCNA), enrich pathways (GO/KEGG), analyze protein-protein interactions (PPI), run machine learning, and export validation, ROC, nomogram, GSEA, and a results summary - all without writing code."
    ),
    shiny::tags$h4(
      shiny::icon("list-check"), " Features at a glance",
      style = "color: #1e293b; margin-top: 22px; border-bottom: 2px solid #667eea; padding-bottom: 6px;"
    ),
    shiny::tags$ul(
      style = "padding-left: 22px; line-height: 1.85; color: #334155;",
      shiny::tags$li(shiny::tags$strong("Step 1 - Download:"), " GEO access (GSE IDs), RNA-seq / Microarray / Merged; gene symbol mapping."),
      shiny::tags$li(shiny::tags$strong("Step 2 - QC & Visualization:"), " Gene overlap, PCA, sample connectivity."),
      shiny::tags$li(shiny::tags$strong("Steps 3-5 - Normalize, Groups, Batch:"), " Normalization, group assignment (Normal/Disease), batch correction (ComBat, etc.)."),
      shiny::tags$li(shiny::tags$strong("Step 6 - Differential Expression:"), " limma, DESeq2, edgeR; volcano plot, heatmaps, DEG tables."),
      shiny::tags$li(shiny::tags$strong("Steps 7-8 - WGCNA & Common Genes:"), " Co-expression modules, module-trait links, GO/KEGG enrichment on DEG and WGCNA genes."),
      shiny::tags$li(shiny::tags$strong("Step 9 - PPI:"), " STRINGdb-based protein interaction network, hub genes, network plots."),
      shiny::tags$li(shiny::tags$strong("Step 10 - Machine Learning:"), " LASSO, Random Forest, SVM-RFE, Boruta, sPLS-DA, XGBoost; Venn of selected genes."),
      shiny::tags$li(
        shiny::tags$strong("Steps 11-15 - Validation, ROC, Nomogram, GSEA, Summary:"),
        " Model validation, ROC curves, nomogram, GSEA, and PDF-ready summary."
      )
    ),
    shiny::tags$h4(
      shiny::icon("route"), " Recommended workflow",
      style = "color: #1e293b; margin-top: 22px; border-bottom: 2px solid #667eea; padding-bottom: 6px;"
    ),
    shiny::tags$p("Follow the sidebar steps in order. Each step depends on the previous one.", style = "margin-bottom: 10px; color: #475569;"),
    shiny::tags$ol(
      style = "padding-left: 22px; line-height: 1.9; color: #334155;",
      shiny::tags$li("Download your dataset(s) (Step 1) and run QC (Step 2)."),
      shiny::tags$li("Normalize (Step 3), assign Normal/Disease groups (Step 4), then run batch correction (Step 5)."),
      shiny::tags$li("Run differential expression (Step 6); use the DE method that matches your data (limma for microarray, DESeq2/edgeR for RNA-seq counts)."),
      shiny::tags$li("Run WGCNA (Step 7), then compute common genes between DEGs and WGCNA (Step 8) and run GO/KEGG enrichment."),
      shiny::tags$li("Build the PPI network (Step 9), then run ML (Step 10) on the selected genes."),
      shiny::tags$li(
        "Validate (Step 11), plot ROC (Step 12), build nomogram (Step 13), run GSEA (Step 14), and generate the results summary (Step 15)."
      )
    ),
    shiny::tags$h4(
      shiny::icon("lightbulb"), " Quick example",
      style = "color: #1e293b; margin-top: 22px; border-bottom: 2px solid #667eea; padding-bottom: 6px;"
    ),
    shiny::tags$p("Example: analyze a public RNA-seq study from GEO.", style = "margin-bottom: 8px; color: #475569;"),
    shiny::tags$ul(
      style = "padding-left: 22px; line-height: 1.8; color: #334155;",
      shiny::tags$li("Step 1: Enter a GSE ID (e.g. GSE50760), select \"RNA-seq\", click \"Download\". Wait for processing."),
      shiny::tags$li("Step 2: Check QC plots (PCA, sample clustering)."),
      shiny::tags$li("Step 4: In \"Select Groups\", assign each sample to \"Normal\" or \"Disease\" using the metadata column that contains group labels."),
      shiny::tags$li("Step 5: Run batch correction (e.g. ComBat with reference batch if you have multiple datasets)."),
      shiny::tags$li("Step 6: Run DE (choose DESeq2 or edgeR for RNA-seq). Inspect volcano plot and DEG table."),
      shiny::tags$li("Step 7: Prepare WGCNA data, pick soft threshold, build modules. Step 8: Compute common genes and run GO/KEGG."),
      shiny::tags$li("Step 9: Run PPI on common genes. Step 10: Extract data for ML, run your chosen methods, use the gene list for ROC/Nomogram/GSEA.")
    ),
    shiny::tags$h4(
      shiny::icon("envelope"), " Contact",
      style = "color: #1e293b; margin-top: 22px; border-bottom: 2px solid #667eea; padding-bottom: 6px;"
    ),
    shiny::tags$p("If you have questions, need help, or want to report issues, please contact:", style = "margin-bottom: 4px; color: #475569;"),
    shiny::tags$p(
      shiny::tags$a(href = "mailto:safa.res.sbb@pu.edu.pk", "safa.res.sbb@pu.edu.pk", style = "font-weight: bold; color: #6366f1;"),
      style = "margin-bottom: 0;"
    ),
    shiny::tags$p(
      shiny::tags$a(href = "mailto:safa.sandhu@gmail.com", "safa.sandhu@gmail.com", style = "font-weight: bold; color: #6366f1;"),
      style = "margin-bottom: 0;"
    )
  )
}

gexp_register_help_observers <- function(input, output, session, rv) {
  # nocov start
  shiny::observeEvent(input$online_status,
    {
      if (isFALSE(input$online_status)) {
        shiny::showNotification(
          shiny::tags$div(
            shiny::icon("exclamation-triangle"),
            shiny::tags$strong("Internet connection lost (Offline)."),
            shiny::tags$p("Downloads may fail until you are back online.", style = "margin-top: 6px;")
          ),
          type = "error",
          duration = 8
        )
      }
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(input$start_tour, {
    shiny::showModal(shiny::modalDialog(
      title = shiny::tags$span(shiny::icon("book-open"), " GExPipe User Guideline"),
      size = "l",
      easyClose = TRUE,
      footer = shiny::modalButton("Close"),
      gexp_user_guideline_modal_ui()
    ))
  })
  # nocov end
}
