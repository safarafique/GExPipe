# ==============================================================================
# UI_ROC.R - Step 12: ROC Curve Analysis (Common ML Genes)
# ==============================================================================
# Uses rv$ml_common_genes, rv$extracted_data_ml, rv$wgcna_sample_info.
# When external validation is loaded (from Step 11), also shows external ROC.
# ==============================================================================

ui_roc <- tabItem(
  tabName = "roc",
  h2(icon("chart-line"), " Step 12: ROC Curve Analysis"),

  fluidRow(
    box(
      title = tags$span(icon("info-circle"), " About this step"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p(tags$strong("Purpose:"), " For genes common across all ML methods (the final ML product), compute ROC curves and AUC scores to evaluate diagnostic performance (Normal vs Disease).", style = "margin-bottom: 8px;"),
      tags$p(tags$strong("Training ROC (Primary):"), " AUC is first computed on the original training data used in ML (internal performance).", style = "margin-bottom: 4px;"),
      tags$p(tags$strong("Validation ROC (External, when available):"), " When an external validation dataset is loaded (Step 11), per-gene AUC is then computed on the independent cohort and shown below the training results for unbiased biomarker performance.", style = "margin-bottom: 0;")
    )
  ),

  # ---- Validation mode indicator ----
  uiOutput("roc_validation_mode_ui"),

  # ---- Training vs Validation AUC comparison (shown first when validation data exists) ----
  uiOutput("roc_auc_comparison_top_ui"),

  fluidRow(
    box(
      title = tags$span(icon("list"), " AUC Summary -- Training Data"),
      width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      uiOutput("roc_placeholder_ui"),
      uiOutput("roc_filter_message_ui"),
      fluidRow(
        column(6,
               tags$p(tags$strong("AUC scores (biomarkers with AUC >= 0.8)"), style = "margin-bottom: 6px;"),
               DT::dataTableOutput("roc_auc_table"),
               tags$div(style = "margin-top: 8px;",
                        downloadButton("download_roc_auc", tagList(icon("download"), " AUC table (CSV)"), class = "btn-success btn-sm"))),
        column(6,
               tags$p(tags$strong("AUC bar plot (top biomarkers)"), style = "margin-bottom: 6px;"),
               plotOutput("roc_auc_barplot", height = "320px"),
               tags$div(style = "margin-top: 8px;",
                        downloadButton("download_roc_auc_plot_jpg", tagList(icon("download"), " JPG (300 DPI)"), class = "btn-primary btn-sm", style = "margin-right: 6px;"),
                        downloadButton("download_roc_auc_plot_pdf", tagList(icon("download"), " PDF"), class = "btn-primary btn-sm")))
      )
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("chart-area"), " ROC Curves -- Training Data (Common Genes)"),
      width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      plotOutput("roc_curves_plot", height = "520px"),
      tags$div(style = "margin-top: 10px;",
        downloadButton("download_roc_plot_jpg", tagList(icon("download"), " JPG (300 DPI)"), class = "btn-primary btn-sm", style = "margin-right: 6px;"),
        downloadButton("download_roc_plot_pdf", tagList(icon("download"), " PDF"), class = "btn-primary btn-sm"))
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("box"), " Gene Expression -- Training Data (Normal vs Disease)"),
      width = 6, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      plotOutput("roc_boxplots_plot", height = "400px"),
      tags$div(style = "margin-top: 10px;",
        downloadButton("download_roc_boxplots_jpg", tagList(icon("download"), " JPG (300 DPI)"), class = "btn-warning btn-sm", style = "margin-right: 6px;"),
        downloadButton("download_roc_boxplots_pdf", tagList(icon("download"), " PDF"), class = "btn-warning btn-sm"))
    ),
    uiOutput("roc_validation_boxplots_ui")
  ),

  # ---- Multi-variable ROC (combined biomarker panel) ----
  fluidRow(
    box(
      title = tags$span(icon("layer-group"), " Multi-variable ROC (Combined Biomarker Panel)"),
      width = 12, status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p(
        "Logistic regression model using the final biomarker panel (common ML genes). ",
        "Compares AUC of the combined panel versus the best single-gene biomarker.",
        style = "margin-bottom: 10px; font-size: 12px;"
      ),
      fluidRow(
        column(6,
          tags$p(tags$strong("Combined-panel ROC curve"), style = "margin-bottom: 6px;"),
          plotOutput("roc_combined_panel_plot", height = "360px")
        ),
        column(6,
          tags$p(tags$strong("AUC comparison: single vs combined"), style = "margin-bottom: 6px;"),
          DT::dataTableOutput("roc_combined_panel_table"),
          tags$div(style = "margin-top: 6px;",
            downloadButton("download_roc_combined_panel_csv", tagList(icon("download"), " Table (CSV)"), class = "btn-primary btn-sm", style = "margin-right: 6px;"),
            downloadButton("download_roc_combined_panel_plot_jpg", tagList(icon("download"), " Plot JPG"), class = "btn-primary btn-sm", style = "margin-right: 4px;"),
            downloadButton("download_roc_combined_panel_plot_pdf", tagList(icon("download"), " Plot PDF"), class = "btn-primary btn-sm"))
        )
      )
    )
  ),

  # ---- External Validation ROC Results (shown after training results) ----
  uiOutput("roc_external_validation_panels_ui"),

  # ---- Gene Selection Panel (user picks final biomarker genes for downstream) ----
  fluidRow(
    box(
      title = tags$span(icon("hand-pointer"), " Select Final Biomarker Genes",
                        tags$span("FOR NOMOGRAM, GSEA & IMMUNE", class = "label label-warning",
                                  style = "margin-left: 8px; font-size: 11px;")),
      width = 12, status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$div(
        style = "padding: 12px 14px; background: linear-gradient(135deg, #fef9e7, #fdebd0); border-radius: 8px; border-left: 4px solid #f39c12; margin-bottom: 15px;",
        icon("lightbulb", style = "color: #f39c12; margin-right: 6px;"),
        tags$strong("Review the AUC scores above (Training & Validation) and select the genes you want to carry forward."),
        tags$br(),
        tags$span("Selected genes will be used for: ", style = "font-size: 13px;"),
        tags$strong("Diagnostic Nomogram"), ", ", tags$strong("GSEA Analysis"), ", and ", tags$strong("Immune Cell Correlation"), ".",
        tags$br(),
        tags$span("If you don't select any genes, all ML common genes will be used by default.", style = "font-size: 12px; color: #6c757d;")
      ),
      uiOutput("roc_gene_selector_ui"),
      uiOutput("roc_gene_selection_status_ui")
    )
  ),
  fluidRow(
    box(
      title = tags$span(icon("file-alt"), " Process Summary"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
      uiOutput("roc_process_summary_ui"))
  ),
  fluidRow(
    box(width = 12, status = "primary", solidHeader = FALSE,
        tags$div(style = "text-align: center; padding: 20px 0;",
                 actionButton("next_page_roc_to_nomogram",
                             tagList(icon("calculator"), " Continue to Diagnostic Nomogram"),
                             class = "btn-success btn-lg",
                             style = "font-size: 18px; padding: 12px 30px; border-radius: 25px; margin-right: 15px;"),
                 actionButton("next_page_roc_to_gsea",
                             tagList(icon("project-diagram"), " Continue to GSEA Analysis"),
                             class = "btn-primary btn-lg",
                             style = "font-size: 18px; padding: 12px 30px; border-radius: 25px; margin-right: 15px;"),
                 actionButton("next_page_roc_to_results",
                             tagList(icon("file-alt"), " View Results Summary"),
                             class = "btn-primary btn-lg",
                             style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;")))
  )
)
