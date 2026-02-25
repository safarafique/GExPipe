# ==============================================================================
# UI_NOMOGRAM.R - Step 13: Diagnostic Nomogram
# ==============================================================================
# External mode: train on ALL data, validate on external dataset.
# Internal mode: 70/30 split-sample validation.
# Uses rv$batch_corrected, rv$ml_common_genes (or common_genes_de_wgcna),
# rv$wgcna_sample_info / rv$unified_metadata.
# ==============================================================================

ui_nomogram <- tabItem(
  tabName = "nomogram",
  h2(icon("calculator"), " Step 13: Diagnostic Nomogram Model"),

  fluidRow(
    box(
      title = tags$span(icon("info-circle"), " About this step"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p(tags$strong("Purpose:"), " Build a diagnostic nomogram using common ML genes (or common DEG+WGCNA genes) and batch-corrected expression data.", style = "margin-bottom: 8px;"),
      tags$p(tags$strong("External Validation mode:"), " Model is trained on ALL samples; external dataset (from Step 11) is used for validation. No 70/30 split.", style = "margin-bottom: 4px;"),
      tags$p(tags$strong("Internal Validation mode:"), " 70/30 stratified split-sample validation. EPV >= 10 enforced; optimal threshold by Youden index.", style = "margin-bottom: 4px;"),
      tags$p(tags$strong("Requirements:"), " Batch correction (Step 5), sample/group metadata (WGCNA or unified), and common genes from ML (Step 10) or Step 8.", style = "margin-bottom: 0;")
    )
  ),

  # ---- Validation mode indicator ----
  uiOutput("nomogram_validation_mode_ui"),

  fluidRow(
    box(
      title = tags$span(icon("cogs"), " Run Nomogram Analysis"),
      width = 12, status = "primary", solidHeader = TRUE,
      uiOutput("nomogram_run_info_ui"),
      actionButton("run_nomogram", tagList(icon("play"), " Run Nomogram Analysis"),
        class = "btn-primary btn-lg", style = "min-width: 240px; white-space: nowrap;"),
      uiOutput("nomogram_placeholder_ui"),
      uiOutput("nomogram_status_ui")
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("chart-bar"), " Panel A: Nomogram"),
      width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      plotOutput("nomogram_plot_panel_a", height = "500px"),
      tags$div(style = "margin-top: 8px;",
        downloadButton("download_nomogram_panel_a", tagList(icon("download"), " PNG (300 DPI)"), class = "btn-success btn-sm", style = "margin-right: 6px;"),
        downloadButton("download_nomogram_panel_a_jpg", tagList(icon("download"), " JPG (300 DPI)"), class = "btn-success btn-sm", style = "margin-right: 6px;"),
        downloadButton("download_nomogram_panel_a_pdf", tagList(icon("download"), " PDF"), class = "btn-success btn-sm"))
    )
  ),

  fluidRow(
    column(6,
      box(
        title = "Panel B: Training ROC",
        width = NULL, status = "danger", solidHeader = TRUE, collapsible = TRUE,
        plotOutput("nomogram_plot_roc_train", height = "360px")
      )
    ),
    column(6,
      box(
        title = "Panel B: Validation ROC",
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE,
        plotOutput("nomogram_plot_roc_val", height = "360px")
      )
    )
  ),

  fluidRow(
    column(6,
      box(
        title = "Panel C: Training Calibration",
        width = NULL, status = "warning", solidHeader = TRUE, collapsible = TRUE,
        plotOutput("nomogram_plot_cal_train", height = "320px")
      )
    ),
    column(6,
      box(
        title = "Panel C: Validation Calibration",
        width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE,
        plotOutput("nomogram_plot_cal_val", height = "320px")
      )
    )
  ),

  fluidRow(
    column(6,
      box(
        title = "Panel D: Training DCA",
        width = NULL, status = "warning", solidHeader = TRUE, collapsible = TRUE,
        plotOutput("nomogram_plot_dca_train", height = "320px")
      )
    ),
    column(6,
      box(
        title = "Panel D: Validation DCA",
        width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE,
        plotOutput("nomogram_plot_dca_val", height = "320px")
      )
    )
  ),

  fluidRow(
    column(6,
      box(
        title = "Panel E: Training Clinical Impact",
        width = NULL, status = "warning", solidHeader = TRUE, collapsible = TRUE,
        plotOutput("nomogram_plot_impact_train", height = "320px")
      )
    ),
    column(6,
      box(
        title = "Panel E: Validation Clinical Impact",
        width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE,
        plotOutput("nomogram_plot_impact_val", height = "320px")
      )
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("table"), " Model Diagnostics (VIF, Coefficients)"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
      DT::dataTableOutput("nomogram_diagnostics_table"),
      tags$div(style = "margin-top: 8px;", downloadButton("download_nomogram_diagnostics", tagList(icon("download"), " Diagnostics (CSV)"), class = "btn-info btn-sm"))
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("list-ol"), " Performance Comparison (Training vs Validation)"),
      width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      DT::dataTableOutput("nomogram_performance_table"),
      tags$div(style = "margin-top: 8px;", downloadButton("download_nomogram_performance", tagList(icon("download"), " Performance (CSV)"), class = "btn-success btn-sm"))
    )
  ),

  fluidRow(
    box(width = 12, status = "primary", solidHeader = FALSE,
        tags$div(style = "text-align: center; padding: 20px 0;",
                 actionButton("next_page_nomogram_to_gsea",
                             tagList(icon("project-diagram"), " Continue to GSEA Analysis"),
                             class = "btn-success btn-lg",
                             style = "font-size: 18px; padding: 12px 30px; border-radius: 25px; margin-right: 15px;"),
                 actionButton("next_page_nomogram_to_results",
                             tagList(icon("file-alt"), " View Results Summary"),
                             class = "btn-primary btn-lg",
                             style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;")))
  )
)
