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
          tags$p(tags$strong("Purpose:"), " After per-dataset normalization, check sample quality and confirm common genes across datasets.", style = "margin-bottom: 8px;"),
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
