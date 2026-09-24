# ==============================================================================
# UI_NORMALIZE.R - Step 2: Normalize Data Tab
# ==============================================================================

ui_normalize <- tabItem(
    tabName = "normalize",
    h2(icon("balance-scale"), " Step 2: Data Normalization"),

    # --------------------------------------------------------------------------
    # LEGACY single track (RNA-seq / microarray / Merged)
    # --------------------------------------------------------------------------
    conditionalPanel(
      condition = "input.analysis_type != 'parallel'",
      fluidRow(
        box(
          title = tags$span(icon("info-circle"), " About this step"),
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
          conditionalPanel(
            condition = "input.analysis_type == 'rnaseq'",
            tagList(
              tags$p(tags$strong("Purpose:"), " Prepare the RNA-seq matrix for the DE method you chose in Step 1.", style = "margin-bottom: 8px;"),
              tags$p(tags$strong("Auto:"), " DESeq2 / edgeR / limma-voom keep raw counts (no TMM for DE). limma uses TMM + log2-CPM, or log2(x+1) if FPKM/TPM is detected.", style = "margin-bottom: 0;")
            )
          ),
          conditionalPanel(
            condition = "input.analysis_type == 'microarray'",
            tagList(
              tags$p(tags$strong("Purpose:"), " Put each microarray GSE on a log scale for limma.", style = "margin-bottom: 8px;"),
              tags$p(tags$strong("Auto:"), " RMA (Affymetrix CEL), Agilent normexp if raw, log2+quantile if not logged, quantile or as-is if already log2.", style = "margin-bottom: 0;")
            )
          ),
          conditionalPanel(
            condition = "input.analysis_type == 'merged'",
            tagList(
              tags$p(tags$strong("Purpose:"), " Normalize each study with its own platform method, then keep common genes, then one global quantile so one limma DE can run.", style = "margin-bottom: 8px;"),
              tags$p(tags$strong("Auto:"), " RNA-seq TMM or log2(FPKM); microarray as above; then intersection; global quantile is on.", style = "margin-bottom: 0;")
            )
          )
        )
      ),
    fluidRow(
      box(
        title = tags$span(icon("cogs"), " Normalization Strategy"),
        width = 12, status = "success", solidHeader = TRUE,
        tags$div(
          style = "padding: 10px 0;",
          hr(),
          tags$p(tags$strong("Normalization mode:"), style = "margin-bottom: 8px;"),
          radioButtons(
            "normalize_mode",
            label = NULL,
            choices = c(
              "Auto (recommended) - use default methods" = "auto",
              "Manual - choose methods below" = "manual"
            ),
            selected = "auto",
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.normalize_mode == 'auto'",
            uiOutput("norm_auto_guide_ui")
          ),
          conditionalPanel(
            condition = "input.normalize_mode == 'manual'",
            uiOutput("norm_manual_guide_ui")
          ),
          conditionalPanel(
            condition = "input.normalize_mode == 'manual'",
            tagList(
              tags$p(tags$strong("Normalization method choices:"), style = "margin: 12px 0 10px 0;"),
              fluidRow(
                column(6,
                       conditionalPanel(
                         condition = "input.analysis_type == 'microarray' || input.analysis_type == 'merged'",
                         tagList(
                           tags$label("Microarray:", style = "font-weight: bold;"),
                           radioButtons("micro_norm_method", label = NULL,
                                       choices = list(
                                         "Quantile (processed / already log2)" = "quantile",
                                         "log2 then quantile (processed, not log)" = "log2_quantile",
                                         "RMA (Affymetrix CEL files)" = "rma",
                                         "Agilent single-color (normexp + quantile)" = "normexp"
                                       ), selected = "quantile", width = "100%")
                         )
                       )
                ),
                column(6,
                       conditionalPanel(
                         condition = "input.analysis_type == 'merged' || (input.analysis_type == 'rnaseq' && input.de_method == 'limma')",
                         tagList(
                           tags$label("RNA-seq:", style = "font-weight: bold;"),
                           radioButtons("rnaseq_norm_method", label = NULL,
                                       choices = list(
                                         "TMM + log2-CPM (raw/estimated counts)" = "TMM",
                                         "log2(x+1) (FPKM / TPM)" = "log2fpkm",
                                         "log2(CPM+1) only (quick exploration)" = "log2cpm_only"
                                       ), selected = "TMM", width = "100%")
                         )
                       ),
                       conditionalPanel(
                         condition = "input.analysis_type == 'rnaseq' && (input.de_method == 'deseq2' || input.de_method == 'edger' || input.de_method == 'limma_voom')",
                         tags$div(
                           class = "alert alert-success",
                           style = "margin: 8px 0 0 0; font-size: 13px; line-height: 1.55;",
                           icon("check-circle"),
                           tags$strong(" RNA-seq normalization skipped for DE."),
                           " DESeq2, edgeR, and limma-voom use raw counts. Click Apply."
                         )
                       )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.analysis_type == 'merged'",
            tagList(
              tags$div(
                style = "padding: 12px 14px; background: #e8f5e9; border-left: 4px solid #4caf50; border-radius: 5px; margin: 15px 0;",
                tags$p(tags$strong("Merged only - common genes"), style = "margin: 0 0 6px 0;"),
                tags$p(
                  "After per-dataset methods, GExPipe keeps genes present on both platforms, then one matrix for limma.",
                  style = "margin: 0; font-size: 13px; color: #495057;"
                )
              ),
              checkboxInput(
                "apply_global_quantile",
                tags$span(
                  tags$strong("Apply global quantile"),
                  " after common genes (on by default for Merged so RNA-seq and microarray share one scale)"
                ),
                value = TRUE
              )
            )
          ),
          hr(),
          tags$div(
            style = "text-align: center;",
            actionButton("apply_normalization", "Apply Normalization",
                         icon = icon("check-circle"), class = "btn-success btn-lg",
                         style = "font-size: 16px; padding: 12px 30px;")
          )
        )
      )
    ),

    fluidRow(
      box(
        title = tags$span(icon("chart-bar"), " Normalization Quality Assessment"),
        width = 12, status = "primary", solidHeader = TRUE,
        tags$div(
          style = "padding: 10px 0;",
          tags$p(
            tags$strong(icon("info-circle"), " Visualizations:"),
            " Check ", tags$strong("within each dataset"), " (samples should align). ",
            "For ", tags$strong("mixed microarray + RNA-seq"), " with global quantile ",
            tags$strong("off"), ", different median blocks across platforms are ",
            tags$em("expected"), " - alignment happens in Step 5 (batch correction), not here.",
            style = "color: #495057; font-size: 13px; margin-bottom: 15px; padding: 10px; background: #f8f9fa; border-radius: 5px;"
          )
        )
      )
    ),

    fluidRow(
      uiOutput("normalize_mixed_scale_ui")
    ),

    fluidRow(
      box(
        title = tags$span(icon("chart-bar"), " Expression Distribution by Dataset"),
        width = 12, status = "info", solidHeader = TRUE,
        plotOutput("normalization_plot", height = "400px"),
          tags$div(style = "margin-top: 6px;",
            gexp_ui_plot_download_bar("dl_norm_plot_png", "dl_norm_plot_jpg", "dl_norm_plot_pdf", "btn-default btn-xs"))
      )
    ),

    fluidRow(
      box(
        title = tags$span(icon("wave-square"), " Overall Expression Distribution"),
        width = 6, status = "success", solidHeader = TRUE,
        plotOutput("normalization_density", height = "350px"),
          tags$div(style = "margin-top: 6px;",
            gexp_ui_plot_download_bar("dl_norm_density_png", "dl_norm_density_jpg", "dl_norm_density_pdf", "btn-default btn-xs"))
      ),
      box(
        title = tags$span(icon("chart-line"), " Quantile-Quantile (Q-Q) Plot"),
        width = 6, status = "warning", solidHeader = TRUE,
        plotOutput("normalization_qq", height = "350px"),
          tags$div(style = "margin-top: 6px;",
            gexp_ui_plot_download_bar("dl_norm_qq_png", "dl_norm_qq_jpg", "dl_norm_qq_pdf", "btn-default btn-xs"))
      )
    ),

    fluidRow(
      box(
        title = tags$span(icon("chart-bar"), " Median & Range Alignment"),
        width = 6, status = "info", solidHeader = TRUE,
        plotOutput("normalization_median_range", height = "400px"),
          tags$div(style = "margin-top: 6px;",
            gexp_ui_plot_download_bar("dl_norm_median_range_png", "dl_norm_median_range_jpg", "dl_norm_median_range_pdf", "btn-default btn-xs"))
      ),
      box(
        title = tags$span(icon("wave-square"), " Distribution Overlap"),
        width = 6, status = "primary", solidHeader = TRUE,
        plotOutput("normalization_distribution_overlap", height = "400px"),
          tags$div(style = "margin-top: 6px;",
            gexp_ui_plot_download_bar("dl_norm_dist_overlap_png", "dl_norm_dist_overlap_jpg", "dl_norm_dist_overlap_pdf", "btn-default btn-xs"))
      )
    ),

    fluidRow(
      box(
        title = tags$span(icon("chart-area"), " Intensity Bias - MA Plot"),
        width = 6, status = "warning", solidHeader = TRUE,
        plotOutput("normalization_ma_plot", height = "400px"),
          tags$div(style = "margin-top: 6px;",
            gexp_ui_plot_download_bar("dl_norm_ma_png", "dl_norm_ma_jpg", "dl_norm_ma_pdf", "btn-default btn-xs"))
      ),
      box(
        title = tags$span(icon("dot-circle"), " Variance Stability - Mean-Variance Plot"),
        width = 6, status = "success", solidHeader = TRUE,
        plotOutput("normalization_mean_variance", height = "400px"),
          tags$div(style = "margin-top: 6px;",
            gexp_ui_plot_download_bar("dl_norm_mv_png", "dl_norm_mv_jpg", "dl_norm_mv_pdf", "btn-default btn-xs"))
      )
    ),

    fluidRow(
      box(
        title = tags$span(icon("th"), " Sample Correlation - Before Normalization"),
        width = 6, status = "danger", solidHeader = TRUE,
        plotOutput("normalization_corr_before", height = "400px"),
          tags$div(style = "margin-top: 6px;",
            gexp_ui_plot_download_bar("dl_norm_corr_before_png", "dl_norm_corr_before_jpg", "dl_norm_corr_before_pdf", "btn-default btn-xs"))
      ),
      box(
        title = tags$span(icon("th"), " Sample Correlation - After Normalization"),
        width = 6, status = "success", solidHeader = TRUE,
        plotOutput("normalization_corr_after", height = "400px"),
          tags$div(style = "margin-top: 6px;",
            gexp_ui_plot_download_bar("dl_norm_corr_after_png", "dl_norm_corr_after_jpg", "dl_norm_corr_after_pdf", "btn-default btn-xs"))
      )
    ),

    fluidRow(
      box(
        title = tags$span(icon("file-alt"), " Normalization Summary"),
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        tags$div(
          id = "normalization_summary_panel",
          tags$div(
            style = "margin-bottom: 20px;",
            tags$h4(icon("table"), " Gene Count Statistics",
                   style = "color: #2c3e50; margin-bottom: 15px;"),
            tableOutput("normalization_summary_table"),
            tags$div(style = "margin-top: 10px;",
              downloadButton("download_normalization_summary_csv", tagList(icon("download"), " Summary table (CSV)"), class = "btn-info btn-sm"))
          ),
          tags$hr(),
          tags$div(
            style = "margin-top: 20px;",
            tags$h4(icon("file-alt"), " Detailed Log",
                   style = "color: #2c3e50; margin-bottom: 15px;"),
            gexp_ui_log_box("normalization_log")
          ),
          tags$div(
            class = "step-timer",
            tags$span(class = "label", "Elapsed:"),
            textOutput("normalization_timer", inline = TRUE)
          )
        )
      )
    ),
    fluidRow(
      box(
        title = tags$span(icon("file-alt"), " Process Summary"),
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        uiOutput("normalize_process_summary_ui"))
    ),
    fluidRow(
      box(width = 12, status = "info", solidHeader = FALSE,
          tags$div(class = "next-btn", style = "text-align: center; padding: 20px 0;",
                   actionButton("next_page_normalize", "Next: QC & Visualization",
                                icon = icon("arrow-right"), class = "btn-success btn-lg",
                                style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;")))
    )
    ),

    # --------------------------------------------------------------------------
    # PARALLEL two-column (RNA-seq left, microarray right)
    # --------------------------------------------------------------------------
    conditionalPanel(
      condition = "input.analysis_type == 'parallel'",
      fluidRow(
        box(
          title = tags$span(icon("info-circle"), " About this step"),
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
          tags$p(
            tags$strong("Purpose:"),
            " Normalize each platform on its own matrix. RNA-seq (left) and microarray (right) never share a gene set here.",
            style = "margin-bottom: 8px;"
          ),
          tags$p(
            tags$strong("Apply once:"),
            " Microarray is always normalized here for limma. RNA-seq method choices appear only if Step 6 uses limma on TMM log-CPM. DESeq2 / edgeR / limma-voom skip RNA-seq normalization (they use raw counts).",
            style = "margin-bottom: 0;"
          )
        )
      ),
      fluidRow(
        box(
          title = tags$span(icon("cogs"), " Normalization (one Apply runs both)"),
          width = 12, status = "success", solidHeader = TRUE,
          radioButtons(
            "normalize_mode_parallel",
            label = tags$strong("Normalization mode:"),
            choices = c(
              "Auto (recommended) - use default methods" = "auto",
              "Manual - choose methods below" = "manual"
            ),
            selected = "auto",
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.normalize_mode_parallel == 'auto'",
            uiOutput("norm_auto_guide_parallel_ui")
          ),
          conditionalPanel(
            condition = "input.normalize_mode_parallel == 'manual'",
            uiOutput("norm_manual_guide_parallel_ui")
          ),
          conditionalPanel(
            condition = "input.normalize_mode_parallel == 'manual'",
          gexp_ui_parallel_two_col(
            tags$div(
              tags$label("RNA-seq:", style = "font-weight: bold;"),
              conditionalPanel(
                condition = "input.de_method_rna == 'limma'",
                radioButtons("rnaseq_norm_method_parallel", label = NULL,
                             choices = list(
                               "TMM + log2-CPM (raw/estimated counts)" = "TMM",
                               "log2(x+1) (FPKM / TPM)" = "log2fpkm",
                               "log2(CPM+1) only (quick exploration)" = "log2cpm_only"
                             ), selected = "TMM", width = "100%")
              ),
              conditionalPanel(
                condition = "input.de_method_rna == 'deseq2' || input.de_method_rna == 'edger' || input.de_method_rna == 'limma_voom'",
                tags$div(
                  class = "alert alert-success",
                  style = "margin: 8px 0 0 0; font-size: 13px; line-height: 1.55;",
                  icon("check-circle"),
                  tags$strong(" RNA-seq normalization skipped."),
                  " DESeq2, edgeR, and limma-voom work on raw counts and normalize internally. No TMM / log2 method is applied for DE."
                )
              )
            ),
            tags$div(
              tags$label("Microarray:", style = "font-weight: bold;"),
              radioButtons("micro_norm_method_parallel", label = NULL,
                           choices = list(
                             "Quantile (processed / already log2)" = "quantile",
                             "log2 then quantile (processed, not log)" = "log2_quantile",
                             "RMA (Affymetrix CEL files)" = "rma",
                             "Agilent single-color (normexp + quantile)" = "normexp"
                           ), selected = "quantile", width = "100%")
            )
          )
          ),
          tags$div(
            style = "text-align: center; margin-top: 12px;",
            actionButton("apply_normalization_parallel", "Apply Normalization",
                         icon = icon("check-circle"), class = "btn-success btn-lg",
                         style = "font-size: 16px; padding: 12px 30px;")
          )
        )
      ),
      gexp_ui_parallel_two_col(
        tagList(
          box(
            title = tags$span(icon("chart-bar"), " RNA-seq boxplot"),
            width = 12, status = "info", solidHeader = TRUE,
            plotOutput("normalization_plot_rna", height = "320px")
          ),
          box(
            title = tags$span(icon("wave-square"), " RNA-seq density"),
            width = 12, status = "success", solidHeader = TRUE,
            plotOutput("normalization_density_rna", height = "280px")
          ),
          box(
            title = tags$span(icon("chart-bar"), " RNA-seq median & range"),
            width = 12, status = "info", solidHeader = TRUE,
            plotOutput("normalization_median_range_rna", height = "320px")
          ),
          box(
            title = tags$span(icon("wave-square"), " RNA-seq distribution overlap"),
            width = 12, status = "success", solidHeader = TRUE,
            plotOutput("normalization_distribution_overlap_rna", height = "320px")
          )
        ),
        tagList(
          box(
            title = tags$span(icon("chart-bar"), " Microarray boxplot"),
            width = 12, status = "warning", solidHeader = TRUE,
            plotOutput("normalization_plot_micro", height = "320px")
          ),
          box(
            title = tags$span(icon("wave-square"), " Microarray density"),
            width = 12, status = "warning", solidHeader = TRUE,
            plotOutput("normalization_density_micro", height = "280px")
          ),
          box(
            title = tags$span(icon("chart-bar"), " Microarray median & range"),
            width = 12, status = "warning", solidHeader = TRUE,
            plotOutput("normalization_median_range_micro", height = "320px")
          ),
          box(
            title = tags$span(icon("wave-square"), " Microarray distribution overlap"),
            width = 12, status = "warning", solidHeader = TRUE,
            plotOutput("normalization_distribution_overlap_micro", height = "320px")
          )
        )
      ),
      gexp_ui_parallel_run_logs("normalization_log_micro", "normalization_log_rna"),
      fluidRow(
        box(width = 12, status = "info", solidHeader = FALSE,
            tags$div(class = "next-btn", style = "text-align: center; padding: 20px 0;",
                     actionButton("next_page_normalize_parallel", "Next: QC & Visualization",
                                  icon = icon("arrow-right"), class = "btn-success btn-lg",
                                  style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;")))
      )
    )
  )
