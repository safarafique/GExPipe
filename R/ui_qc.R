# ==============================================================================
# UI_QC.R - Step 3: QC & Visualization Tab
# ==============================================================================

ui_qc <- tabItem(
    tabName = "qc",
    h2(icon("chart-bar"), " Step 3: Quality Control & Common Genes"),

    # --------------------------------------------------------------------------
    # LEGACY single track
    # --------------------------------------------------------------------------
    conditionalPanel(
      condition = "input.analysis_type != 'parallel'",
      fluidRow(
        box(
          title = tags$span(icon("info-circle"), " About this step"),
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
          tags$p(tags$strong("Purpose:"), " After per-dataset normalization, check sample quality, flag outliers, and confirm common genes across datasets.", style = "margin-bottom: 8px;"),
          tags$p(tags$strong("Plots:"), " Venn and UpSet show gene overlap after Step 2; boxplot and density show normalized expression. Remove failed samples here, then re-normalize if needed.", style = "margin-bottom: 0;")
        )
      ),
      fluidRow(
        box(title = tags$span(icon("venus-mars"), " Venn Diagram - Gene Overlap"),
            width = 6, status = "primary", solidHeader = TRUE,
            plotOutput("venn_plot", height = "500px"),
            tags$div(style = "margin-top: 6px;",
              gexp_ui_plot_download_bar("dl_qc_venn_png", "dl_qc_venn_jpg", "dl_qc_venn_pdf", "btn-default btn-xs")),
            tags$p(icon("info-circle"), " Counts are per-dataset gene lists after Step 2 (Normalization). Overlap = ", tags$strong("common genes"), " used for batch correction and DE. ", "If overlap is 0, datasets likely still use different IDs; re-run Step 1 mapping to gene symbols.", style = "margin-top: 8px; font-size: 12px; color: #555;")),
        box(title = tags$span(icon("project-diagram"), " UpSet Plot - Gene Intersections"),
            width = 6, status = "info", solidHeader = TRUE,
            plotOutput("upset_plot", height = "600px"),
            tags$div(style = "margin-top: 6px;",
              gexp_ui_plot_download_bar("dl_qc_upset_png", "dl_qc_upset_jpg", "dl_qc_upset_pdf", "btn-default btn-xs")))
      ),
      fluidRow(
        box(title = tags$span(icon("chart-line"), " Quality Control Plots"),
            width = 12, status = "warning", solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Boxplot",
                plotOutput("qc_boxplot", height = "400px"),
                tags$div(style = "margin-top: 6px;",
                  gexp_ui_plot_download_bar("dl_qc_boxplot_png", "dl_qc_boxplot_jpg", "dl_qc_boxplot_pdf", "btn-default btn-xs"))),
              tabPanel("Density",
                plotOutput("qc_density", height = "400px"),
                tags$div(style = "margin-top: 6px;",
                  gexp_ui_plot_download_bar("dl_qc_density_png", "dl_qc_density_jpg", "dl_qc_density_pdf", "btn-default btn-xs")))
            )
        )
      )
    ),

    # --------------------------------------------------------------------------
    # PARALLEL two-column
    # --------------------------------------------------------------------------
    conditionalPanel(
      condition = "input.analysis_type == 'parallel'",
      fluidRow(
        box(
          title = tags$span(icon("info-circle"), " About this step"),
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
          tags$p(
            tags$strong("Purpose:"),
            " Check RNA-seq (left) and microarray (right) separately. Each platform keeps its own genes.",
            style = "margin-bottom: 8px;"
          ),
          tags$p(
            tags$strong("Overlap plots:"),
            " Venn / UpSet below are symbol overlap for information only. They are not used as common genes for DE.",
            style = "margin-bottom: 0;"
          )
        )
      ),
      gexp_ui_parallel_run_logs("qc_log_micro", "qc_log_rna"),
      gexp_ui_parallel_two_col(
        tagList(
          box(
            title = tags$span(icon("chart-bar"), " RNA-seq boxplot"),
            width = 12, status = "info", solidHeader = TRUE,
            plotOutput("qc_boxplot_rna", height = "320px")
          ),
          box(
            title = tags$span(icon("wave-square"), " RNA-seq density"),
            width = 12, status = "info", solidHeader = TRUE,
            plotOutput("qc_density_rna", height = "280px")
          ),
          box(
            title = tags$span(icon("table"), " RNA-seq outliers"),
            width = 12, status = "danger", solidHeader = TRUE,
            DT::DTOutput("qc_outlier_table_rna")
          )
        ),
        tagList(
          box(
            title = tags$span(icon("chart-bar"), " Microarray boxplot"),
            width = 12, status = "warning", solidHeader = TRUE,
            plotOutput("qc_boxplot_micro", height = "320px")
          ),
          box(
            title = tags$span(icon("wave-square"), " Microarray density"),
            width = 12, status = "warning", solidHeader = TRUE,
            plotOutput("qc_density_micro", height = "280px")
          ),
          box(
            title = tags$span(icon("table"), " Microarray outliers"),
            width = 12, status = "danger", solidHeader = TRUE,
            DT::DTOutput("qc_outlier_table_micro")
          )
        )
      ),
      fluidRow(
        box(
          title = tags$span(icon("info-circle"), " Symbol overlap (information only)"),
          width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
          tags$p(
            "These plots show gene-symbol overlap between datasets. Parallel DE does not intersect genes here.",
            style = "font-size: 13px; color: #555; margin-bottom: 10px;"
          ),
          fluidRow(
            column(6, plotOutput("venn_plot_parallel", height = "420px")),
            column(6, plotOutput("upset_plot_parallel", height = "420px"))
          )
        )
      )
    ),

    # ---- Sample Outlier Detection (shared; one run) ----
    fluidRow(
      box(
        title = tags$span(icon("search"), " Sample Outlier Detection",
                          tags$span("AFTER NORMALIZATION", class = "label label-success",
                                    style = "margin-left: 8px; font-size: 10px;")),
        width = 12, status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$div(
          style = "padding: 10px 14px; background: linear-gradient(135deg, #fef9e7, #fdebd0); border-left: 4px solid #f39c12; border-radius: 4px; margin-bottom: 15px;",
          icon("lightbulb", style = "color: #f39c12; margin-right: 6px;"),
          tags$strong("Detect and remove outlier samples after normalization. "),
          tags$span("If samples are removed, GExPipe re-normalizes the remaining data and re-computes common genes.", style = "font-size: 13px;"),
          tags$br(),
          tags$span(icon("chart-area", style = "margin-right: 4px;"), tags$strong("PCA + Mahalanobis distance:"),
                    " Identifies samples far from the cluster center in PC1-PC2 space (97.5% chi-squared threshold).",
                    style = "font-size: 12px; display: block; margin-top: 4px;"),
          tags$span(icon("project-diagram", style = "margin-right: 4px;"), tags$strong("Sample connectivity (signed network):"),
                    " Flags samples with low inter-sample correlation (mean - 2*SD threshold).",
                    style = "font-size: 12px; display: block; margin-top: 2px;")
        ),
        uiOutput("qc_excluded_info_ui"),
        fluidRow(
          column(3,
            actionButton("run_outlier_detection",
              tagList(icon("play"), " Run Outlier Detection"),
              class = "btn-danger btn-lg",
              style = "font-weight: bold; width: 100%; background: #e74c3c !important; background-image: none !important; border-color: #c0392b !important; color: #fff !important;")
          ),
          column(9, uiOutput("qc_outlier_summary_ui"))
        ),
        conditionalPanel(
          condition = "input.analysis_type != 'parallel'",
          uiOutput("qc_outlier_results_ui")
        )
      )
    ),

    conditionalPanel(
      condition = "input.analysis_type != 'parallel'",
      fluidRow(
        box(
          title = tags$span(icon("info-circle"), " Data Summary"),
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
          tags$div(
            style = "padding: 15px 0;",
            fluidRow(
              column(4, infoBoxOutput("datasets_box", width = 12)),
              column(4, infoBoxOutput("samples_box", width = 12)),
              column(4, infoBoxOutput("genes_box", width = 12))
            ),
            hr(),
            tags$h5(icon("dna"), " Gene Overlap Summary", style = "margin-top: 15px;"),
            uiOutput("gene_overlap_summary")
          )
        )
      )
    ),

    fluidRow(
      box(
        title = tags$span(icon("file-alt"), " Process Summary"),
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        uiOutput("qc_process_summary_ui"))
    ),
    fluidRow(
      box(width = 12, status = "info", solidHeader = FALSE,
          tags$div(class = "next-btn", style = "text-align: center; padding: 20px 0;",
                   uiOutput("qc_next_button")))
    ),
  )
