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
      ),
      tags$p(
        tags$strong("Minimum sample size:"),
        " you need at least 10 Disease AND at least 10 Normal samples, combined across your WHOLE training cohort",
        " (not per dataset - e.g. 7 samples from one GSE + 12 from another is fine as long as each group's total reaches 10 once combined).",
        " With exactly 10/group the gene panel is auto-trimmed to as few as 3 genes to stay statistically stable;",
        " a richer panel (4-5+ genes) really wants 30-50+ samples per group to avoid heavy trimming.",
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
      fluidRow(
        column(3, numericInput("nomogram_boot_B", "Bootstrap repetitions (B):", value = 1000, min = 100, max = 5000, step = 100)),
        column(3, numericInput("nomogram_seed", "Random seed:", value = 123, min = 1, step = 1)),
        column(6, tags$p(style = "margin-top: 28px; font-size: 12px; color: #666;",
          "B resamples drive the optimism-corrected C-index and the bias-corrected calibration curve (default 1000, range 100-5000). ",
          "The same seed reproduces the split and the bootstrap exactly."))
      ),
      radioButtons("nomogram_val_standardize",
        tags$span("External validation: how are the validation predictors standardized?",
                  tags$small(" (used in External mode)", style = "font-weight: normal; color: #888;")),
        choices = c(
          "Within the validation cohort (default; use when platforms/scales differ)" = "cohort",
          "Within the validation cohort, matched to the training prevalence (use when case-mix differs)" = "prevalence",
          "Training mean/SD (only when both cohorts share the same platform and scale)" = "training"
        ),
        selected = "cohort"),
      tags$p(style = "font-size: 12px; color: #666; margin: -4px 0 10px 0;",
        "The model is never refit and the threshold is never re-chosen on validation data in any option. ",
        "Within-cohort standardization assumes a similar disease prevalence; if training and validation differ a lot, ",
        "predicted risks shift (more false positives or negatives) even when the AUC is good - the prevalence-matched option removes that shift."),
      actionButton("run_nomogram", tagList(icon("play"), " Run Nomogram Analysis"),
        class = "btn-primary btn-lg", style = "min-width: 240px; white-space: nowrap;"),
      uiOutput("nomogram_placeholder_ui"),
      uiOutput("nomogram_status_ui"),
      uiOutput("nomogram_outcome_coding_ui")
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
        plotOutput("nomogram_plot_roc_train", height = "440px")
      )
    ),
    column(6,
      box(
        title = "Panel B: Validation ROC",
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE,
        gexp_ui_plot_download_bar("download_nomogram_roc_val_png", "download_nomogram_roc_val_jpg", "download_nomogram_roc_val_pdf", "btn-primary btn-sm"),
        plotOutput("nomogram_plot_roc_val", height = "440px")
      )
    )
  ),

  fluidRow(
    column(6, offset = 3,
      box(
        title = "Panel B: Training vs Validation ROC (combined)",
        width = NULL, status = "success", solidHeader = TRUE, collapsible = TRUE,
        gexp_ui_plot_download_bar("download_nomogram_roc_combined_png", "download_nomogram_roc_combined_jpg", "download_nomogram_roc_combined_pdf", "btn-success btn-sm"),
        plotOutput("nomogram_plot_roc_combined", height = "480px")
      )
    )
  ),

  fluidRow(
    column(6,
      box(
        title = "Panel C: Training Calibration",
        width = NULL, status = "warning", solidHeader = TRUE, collapsible = TRUE,
        gexp_ui_plot_download_bar("download_nomogram_cal_train_png", "download_nomogram_cal_train_jpg", "download_nomogram_cal_train_pdf", "btn-warning btn-sm"),
        plotOutput("nomogram_plot_cal_train", height = "440px")
      )
    ),
    column(6,
      box(
        title = "Panel C: Validation Calibration",
        width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE,
        gexp_ui_plot_download_bar("download_nomogram_cal_val_png", "download_nomogram_cal_val_jpg", "download_nomogram_cal_val_pdf", "btn-info btn-sm"),
        plotOutput("nomogram_plot_cal_val", height = "440px")
      )
    )
  ),

  fluidRow(
    column(6,
      box(
        title = "Panel C2: Training Confusion Matrix",
        width = NULL, status = "danger", solidHeader = TRUE, collapsible = TRUE,
        gexp_ui_plot_download_bar("download_nomogram_conf_train_png", "download_nomogram_conf_train_jpg", "download_nomogram_conf_train_pdf", "btn-danger btn-sm"),
        plotOutput("nomogram_plot_conf_train", height = "380px")
      )
    ),
    column(6,
      box(
        title = "Panel C2: Validation Confusion Matrix",
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE,
        gexp_ui_plot_download_bar("download_nomogram_conf_val_png", "download_nomogram_conf_val_jpg", "download_nomogram_conf_val_pdf", "btn-primary btn-sm"),
        plotOutput("nomogram_plot_conf_val", height = "380px")
      )
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("table"), " Confusion Matrix Counts & Calibration Statistics"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$h5(tags$strong("Confusion matrices (TP / TN / FP / FN)")),
      DT::dataTableOutput("nomogram_confusion_table"),
      tags$div(style = "margin: 8px 0 14px 0;", downloadButton("download_nomogram_confusion", tagList(icon("download"), " Confusion matrices (CSV)"), class = "btn-info btn-sm")),
      tags$h5(tags$strong("Calibration (intercept, slope, Brier score)")),
      tags$p(style = "font-size: 12px; color: #666;",
        "Intercept = calibration-in-the-large (ideal 0), slope = calibration slope (ideal 1). Estimated only when each class has at least 10 samples; ",
        "otherwise shown as 'not estimated'. Validation predictions use the training model unchanged, so a non-zero intercept reflects a real prevalence/scale difference, not a refit."),
      DT::dataTableOutput("nomogram_calibration_table"),
      tags$div(style = "margin-top: 8px;", downloadButton("download_nomogram_calibration", tagList(icon("download"), " Calibration statistics (CSV)"), class = "btn-info btn-sm"))
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
      uiOutput("nomogram_diagnostics_note_ui"),
      DT::dataTableOutput("nomogram_diagnostics_table"),
      tags$div(style = "margin-top: 8px;",
        downloadButton("download_nomogram_coefficients", tagList(icon("download"), " Model coefficients (CSV)"), class = "btn-info btn-sm", style = "margin-right: 6px;"),
        downloadButton("download_nomogram_firth", tagList(icon("download"), " Firth coefficients (CSV)"), class = "btn-info btn-sm", style = "margin-right: 6px;"),
        downloadButton("download_nomogram_diagnostics", tagList(icon("download"), " All diagnostics (CSV)"), class = "btn-info btn-sm"))
    )
  ),

  conditionalPanel(
    condition = "output.nomogram_optimism_available",
    fluidRow(
      box(
        title = tags$span(icon("chart-line"), " Bootstrap Optimism Correction (Overfitting-Corrected C-index)"),
        width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        uiOutput("nomogram_optimism_note_ui"),
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
      tags$p(style = "font-size: 12px; color: #666; margin-top: 6px;",
        "Validation uses the exact model and the exact threshold fitted on the training data (no refit, no new threshold). ",
        "95% CIs: exact Clopper-Pearson for accuracy, sensitivity, specificity, PPV and NPV; DeLong for AUC. ",
        "Sensitivity_n / Specificity_n give numerator/denominator."),
      tags$div(style = "margin-top: 8px;",
        downloadButton("download_nomogram_performance", tagList(icon("download"), " Comparison (CSV)"), class = "btn-success btn-sm", style = "margin-right: 6px;"),
        downloadButton("download_nomogram_perf_training", tagList(icon("download"), " Training performance (CSV)"), class = "btn-success btn-sm", style = "margin-right: 6px;"),
        downloadButton("download_nomogram_perf_validation", tagList(icon("download"), " Validation performance (CSV)"), class = "btn-success btn-sm"))
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("clipboard-list"), " Outcome Coding, Methods & Run Settings"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
      DT::dataTableOutput("nomogram_settings_table"),
      tags$div(style = "margin-top: 8px;", downloadButton("download_nomogram_settings", tagList(icon("download"), " Settings (CSV)"), class = "btn-info btn-sm"))
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
