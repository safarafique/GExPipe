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
  h2(icon("calculator"), " Step 14: Diagnostic Nomogram Model"),

  fluidRow(
    box(
      title = tags$span(icon("info-circle"), " About this step"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      uiOutput("nomogram_about_ui"),
      tags$p(
        tags$strong("Why can nomogram AUC be ~0.5 when per-gene AUCs are high?"),
        " ROC (Step 13) scores each gene alone. The nomogram uses a fixed linear combination from training. A different platform or scale can break that combination. Use a similar technology for external validation.",
        style = "margin: 8px 0 0 0; font-size: 12px; color: #555;"
      )
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
      gexp_ui_plot_download_bar("download_nomogram_panel_a", "download_nomogram_panel_a_jpg", "download_nomogram_panel_a_pdf", "btn-success btn-sm")
    )
  ),

  fluidRow(
    column(6,
      box(
        title = "Panel B: Training ROC",
        width = NULL, status = "danger", solidHeader = TRUE, collapsible = TRUE,
        gexp_ui_plot_download_bar("download_nomogram_roc_train_png", "download_nomogram_roc_train_jpg", "download_nomogram_roc_train_pdf", "btn-danger btn-sm"),
        plotOutput("nomogram_plot_roc_train", height = "360px")
      )
    ),
    column(6,
      box(
        title = "Panel B: Validation ROC",
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE,
        gexp_ui_plot_download_bar("download_nomogram_roc_val_png", "download_nomogram_roc_val_jpg", "download_nomogram_roc_val_pdf", "btn-primary btn-sm"),
        plotOutput("nomogram_plot_roc_val", height = "360px")
      )
    )
  ),

  fluidRow(
    column(6,
      box(
        title = "Panel C: Training Calibration",
        width = NULL, status = "warning", solidHeader = TRUE, collapsible = TRUE,
        gexp_ui_plot_download_bar("download_nomogram_cal_train_png", "download_nomogram_cal_train_jpg", "download_nomogram_cal_train_pdf", "btn-warning btn-sm"),
        plotOutput("nomogram_plot_cal_train", height = "320px")
      )
    ),
    column(6,
      box(
        title = "Panel C: Validation Calibration",
        width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE,
        gexp_ui_plot_download_bar("download_nomogram_cal_val_png", "download_nomogram_cal_val_jpg", "download_nomogram_cal_val_pdf", "btn-info btn-sm"),
        plotOutput("nomogram_plot_cal_val", height = "320px")
      )
    )
  ),

  fluidRow(
    column(6,
      box(
        title = "Panel D: Training DCA",
        width = NULL, status = "warning", solidHeader = TRUE, collapsible = TRUE,
        gexp_ui_plot_download_bar("download_nomogram_dca_train_png", "download_nomogram_dca_train_jpg", "download_nomogram_dca_train_pdf", "btn-warning btn-sm"),
        plotOutput("nomogram_plot_dca_train", height = "320px")
      )
    ),
    column(6,
      box(
        title = "Panel D: Validation DCA",
        width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE,
        gexp_ui_plot_download_bar("download_nomogram_dca_val_png", "download_nomogram_dca_val_jpg", "download_nomogram_dca_val_pdf", "btn-info btn-sm"),
        plotOutput("nomogram_plot_dca_val", height = "320px")
      )
    )
  ),

  fluidRow(
    column(6,
      box(
        title = "Panel E: Training Clinical Impact",
        width = NULL, status = "warning", solidHeader = TRUE, collapsible = TRUE,
        gexp_ui_plot_download_bar("download_nomogram_impact_train_png", "download_nomogram_impact_train_jpg", "download_nomogram_impact_train_pdf", "btn-warning btn-sm"),
        plotOutput("nomogram_plot_impact_train", height = "320px")
      )
    ),
    column(6,
      box(
        title = "Panel E: Validation Clinical Impact",
        width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE,
        gexp_ui_plot_download_bar("download_nomogram_impact_val_png", "download_nomogram_impact_val_jpg", "download_nomogram_impact_val_pdf", "btn-info btn-sm"),
        plotOutput("nomogram_plot_impact_val", height = "320px")
      )
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("table"), " Model Diagnostics (VIF, Coefficients)"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
      tags$p(
        style = "font-size: 12px; color: #666;",
        "Coefficient/Std_Error/OR come from the standard model. When a gene near-perfectly separates ",
        "the two groups, those can blow up to unrealistic values (huge SE, extreme OR) - the ",
        "_Firth columns are Firth's bias-reduced estimates, which stay finite and realistic under ",
        "separation; trust those when the two disagree."
      ),
      DT::dataTableOutput("nomogram_diagnostics_table"),
      tags$div(style = "margin-top: 8px;", downloadButton("download_nomogram_diagnostics", tagList(icon("download"), " Diagnostics (CSV)"), class = "btn-info btn-sm"))
    )
  ),

  conditionalPanel(
    condition = "output.nomogram_optimism_available",
    fluidRow(
      box(
        title = tags$span(icon("chart-line"), " Bootstrap Optimism Correction (Overfitting-Corrected C-index)"),
        width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        tags$p(
          style = "font-size: 12px; color: #666;",
          "The apparent training C-index is measured on the same data the model was fit on and is ",
          "always optimistic. This refits the model on 200 bootstrap resamples of the training data ",
          "and averages how much each resample's performance drops when applied back to the original ",
          "data (the 'optimism'). The Bootstrap_Corrected value is the more honest estimate of how ",
          "this panel will perform on new samples."
        ),
        DT::dataTableOutput("nomogram_optimism_table"),
        tags$div(style = "margin-top: 8px;", downloadButton("download_nomogram_optimism", tagList(icon("download"), " Optimism Correction (CSV)"), class = "btn-warning btn-sm"))
      )
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
    box(
      title = tags$span(icon("file-alt"), " Process Summary"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
      uiOutput("nomogram_process_summary_ui"))
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
