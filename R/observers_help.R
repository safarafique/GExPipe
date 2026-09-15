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
      shiny::tags$li(shiny::tags$strong("Step 1 - Download:"), " GEO access (GSE IDs). Four types: RNA-seq, microarray, Merged (Both), Parallel DE then merge. Each platform box accepts one or more GSE IDs."),
      shiny::tags$li(shiny::tags$strong("Step 2 - Normalize:"), " Platform-specific methods first (RMA, TMM, quantile, etc.)."),
      shiny::tags$li(shiny::tags$strong("Step 3 - QC & Visualization:"), " Outliers, gene overlap / common genes after normalization."),
      shiny::tags$li(shiny::tags$strong("Steps 4-5 - Groups, Batch:"), " Normal vs Disease labels, then batch correction. Parallel DE corrects each platform separately; Merged (Both) uses one joint correction."),
      shiny::tags$li(shiny::tags$strong("Step 6 - Differential Expression:"), " limma, DESeq2, edgeR; volcano plot, heatmaps, DEG tables. Merged (Both) is one joint limma. Parallel DE runs RNA-seq and microarray separately."),
      shiny::tags$li(shiny::tags$strong("Step 7 - RNA-seq \u2229 microarray:"), " Genes significant on both platforms (same direction). Then one WGCNA on one processed platform matrix (top variable genes, not this DEG list). Step 9 overlaps the two."),
      shiny::tags$li(shiny::tags$strong("Steps 8-9 - WGCNA & Common Genes:"), " WGCNA on top-variable genes (RNA-seq VST or array log values), then overlap modules with DEGs for GO/KEGG."),
      shiny::tags$li(shiny::tags$strong("Step 10 - PPI:"), " STRINGdb-based protein interaction network, hub genes, network plots."),
      shiny::tags$li(shiny::tags$strong("Step 11 - Machine Learning:"), " LASSO, Random Forest, SVM-RFE, Boruta, sPLS-DA, XGBoost; Venn of selected genes."),
      shiny::tags$li(
        shiny::tags$strong("Steps 12-16 - Validation, ROC, Nomogram, GSEA, Summary:"),
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
      shiny::tags$li("Download your dataset(s) (Step 1), then normalize (Step 2)."),
      shiny::tags$li("Run QC (Step 3), assign Normal/Disease groups (Step 4), then batch correction (Step 5)."),
      shiny::tags$li("Run differential expression (Step 6); use the DE method that matches your data (limma for microarray, DESeq2/edgeR for RNA-seq counts). For Parallel DE, then merge, apply Step 7 (RNA-seq intersect microarray). Merged (Both) skips Step 7."),
      shiny::tags$li("Run WGCNA (Step 8), then compute common genes between DEGs and WGCNA (Step 9) and run GO/KEGG enrichment."),
      shiny::tags$li("Build the PPI network (Step 10), then run ML (Step 11) on the selected genes."),
      shiny::tags$li(
        "Validate (Step 12), plot ROC (Step 13), build nomogram (Step 14), run GSEA (Step 15), and generate the results summary (Step 16)."
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
      shiny::tags$li("Step 2: Apply normalization (Auto uses the platform table)."),
      shiny::tags$li("Step 3: Check QC plots (PCA, sample clustering) and common genes."),
      shiny::tags$li("Step 4: In \"Select Groups\", assign each sample to \"Normal\" or \"Disease\" using the metadata column that contains group labels."),
      shiny::tags$li("Step 5: Run batch correction (e.g. ComBat with reference batch if you have multiple datasets). Parallel DE corrects each platform separately before DE; Merged (Both) uses one joint correction."),
      shiny::tags$li("Step 6: Run DE (choose DESeq2 or edgeR for RNA-seq). Inspect volcano plot and DEG table. For Parallel DE, apply Step 7 consensus (same-direction overlap)."),
      shiny::tags$li("Step 8: Prepare WGCNA data, pick soft threshold, build modules. Step 9: Compute common genes and run GO/KEGG."),
      shiny::tags$li("Step 10: Run PPI on common genes. Step 11: Extract data for ML, run your chosen methods, use the gene list for ROC/Nomogram/GSEA.")
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
