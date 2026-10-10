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
            " Venn / UpSet below show the common genes of the RNA-seq datasets and of the microarray datasets separately. The two platforms are never merged here.",
            style = "margin-bottom: 0;"
          )
        )
      ),
      gexp_ui_parallel_two_col(
        tagList(
          box(
            title = tags$span(icon("chart-bar"), " RNA-seq boxplot"),
            width = 12, status = "info", solidHeader = TRUE,
            plotOutput("qc_boxplot_rna", height = "320px"),
            gexp_ui_plot_download_bar("download_qc_boxplot_rna_png", "download_qc_boxplot_rna_jpg", "download_qc_boxplot_rna_pdf", "btn-info btn-xs")
          ),
          box(
            title = tags$span(icon("wave-square"), " RNA-seq density"),
            width = 12, status = "info", solidHeader = TRUE,
            plotOutput("qc_density_rna", height = "280px"),
            gexp_ui_plot_download_bar("download_qc_density_rna_png", "download_qc_density_rna_jpg", "download_qc_density_rna_pdf", "btn-info btn-xs")
          )
        ),
        tagList(
          box(
            title = tags$span(icon("chart-bar"), " Microarray boxplot"),
            width = 12, status = "warning", solidHeader = TRUE,
            plotOutput("qc_boxplot_micro", height = "320px"),
            gexp_ui_plot_download_bar("download_qc_boxplot_micro_png", "download_qc_boxplot_micro_jpg", "download_qc_boxplot_micro_pdf", "btn-warning btn-xs")
          ),
          box(
            title = tags$span(icon("wave-square"), " Microarray density"),
            width = 12, status = "warning", solidHeader = TRUE,
            plotOutput("qc_density_micro", height = "280px"),
            gexp_ui_plot_download_bar("download_qc_density_micro_png", "download_qc_density_micro_jpg", "download_qc_density_micro_pdf", "btn-warning btn-xs")
          )
        )
      ),
      # One box per platform (same two-column layout and colours as the
      # boxplot/density boxes above) so RNA-seq and microarray never read as
      # one merged block.
      gexp_ui_parallel_two_col(
        tagList(
          box(
            title = tags$span(icon("dna"), " RNA-seq: common genes across RNA-seq datasets"),
            width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
            uiOutput("common_genes_summary_rna"),
            tags$h5(tags$strong("Venn diagram"), style = "margin-top: 12px;"),
            plotOutput("venn_plot_rna", height = "380px"),
            gexp_ui_plot_download_bar("download_venn_plot_rna_png", "download_venn_plot_rna_jpg", "download_venn_plot_rna_pdf", "btn-info btn-xs")
          ),
          box(
            title = tags$span(icon("chart-bar"), " RNA-seq: UpSet plot"),
            width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
            plotOutput("upset_plot_rna", height = "380px"),
            gexp_ui_plot_download_bar("download_upset_plot_rna_png", "download_upset_plot_rna_jpg", "download_upset_plot_rna_pdf", "btn-info btn-xs")
          )
        ),
        tagList(
          box(
            title = tags$span(icon("th"), " Microarray: common genes across microarray datasets"),
            width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE,
            uiOutput("common_genes_summary_micro"),
            tags$h5(tags$strong("Venn diagram"), style = "margin-top: 12px;"),
            plotOutput("venn_plot_micro", height = "380px"),
            gexp_ui_plot_download_bar("download_venn_plot_micro_png", "download_venn_plot_micro_jpg", "download_venn_plot_micro_pdf", "btn-warning btn-xs")
          ),
          box(
            title = tags$span(icon("chart-bar"), " Microarray: UpSet plot"),
            width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE,
            plotOutput("upset_plot_micro", height = "380px"),
            gexp_ui_plot_download_bar("download_upset_plot_micro_png", "download_upset_plot_micro_jpg", "download_upset_plot_micro_pdf", "btn-warning btn-xs")
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
          tags$strong("Optional: flag possible outlier samples after normalization. "),
          tags$span("Each GSE is tested on its own samples (this step is before batch correction, so a pooled test would mostly flag study differences). ",
                    "Nothing is removed unless you tick it. If samples are removed, GExPipe re-normalizes the remaining data and re-computes common genes.",
                    style = "font-size: 13px;"),
          tags$br(),
          tags$span(icon("chart-area", style = "margin-right: 4px;"), tags$strong("PCA + Mahalanobis distance:"),
                    " Identifies samples far from their GSE's center in PC1-PC2 space (97.5% chi-squared threshold).",
                    style = "font-size: 12px; display: block; margin-top: 4px;"),
          tags$span(icon("project-diagram", style = "margin-right: 4px;"), tags$strong("Sample connectivity (signed network):"),
                    " Flags samples with low correlation to the rest of their GSE (z < -2, i.e. mean - 2*SD).",
                    style = "font-size: 12px; display: block; margin-top: 2px;"),
          tags$span(icon("lightbulb", style = "margin-right: 4px;"), tags$strong("Advice:"),
                    " a flag is not proof of a bad sample. Strong disease samples often look 'different'. Exclude only clear technical failures (STRONG = both tests), and check that DE is similar with and without them.",
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
        uiOutput("qc_outlier_results_ui")
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
    gexp_ui_parallel_run_logs("qc_log_micro", "qc_log_rna"),
    fluidRow(
      box(width = 12, status = "info", solidHeader = FALSE,
          tags$div(class = "next-btn", style = "text-align: center; padding: 20px 0;",
                   uiOutput("qc_next_button")))
    ),
  )
