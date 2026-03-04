# ==============================================================================
# UI_IMMUNE.R - Step 15: Immune Cell Deconvolution Analysis
# ==============================================================================
# Uses rv$batch_corrected and rv$wgcna_sample_info (or rv$unified_metadata).
# ==============================================================================

ui_immune <- tabItem(
  tabName = "immune",
  h2(icon("shield-alt"), " Step 15: Immune Cell Deconvolution Analysis"),

  fluidRow(
    box(
      title = tags$span(icon("info-circle"), " About this step"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p(tags$strong("Purpose:"), " Estimate immune cell proportions from bulk expression using EPIC, MCP-counter, or xCell. Compare proportions between groups, plot correlation heatmaps, and optionally correlate with signature genes.", style = "margin-bottom: 8px;"),
      tags$p(tags$strong("Requirements:"), " Batch-corrected expression (Step 5) and sample/group metadata (e.g. from WGCNA or unified metadata). Expression matrix must have genes as rows and samples as columns.", style = "margin-bottom: 0;")
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("cogs"), " Run Deconvolution"),
      width = 12, status = "primary", solidHeader = TRUE,
      fluidRow(
        column(3,
          selectInput("immune_method",
            tags$span("Deconvolution method",
              tags$i(class = "fa fa-question-circle param-help",
                     `data-toggle` = "tooltip", `data-placement` = "right",
                     title = "Algorithm for estimating immune cell proportions from bulk expression.<br><b>xCell:</b> Enrichment-based, 64 cell types, best for relative scores.<br><b>EPIC:</b> Constrained least squares, good absolute fractions.<br><b>MCP-counter:</b> Robust marker-based, 10 cell types.")),
            choices = c("xCell" = "xcell", "EPIC" = "epic", "MCP-counter" = "mcp_counter"),
            selected = "xcell")
        ),
        column(4,
          textAreaInput("immune_genes", "Genes for correlation (optional, comma-separated)",
            value = "", rows = 2, placeholder = "Leave empty to use common ML genes if available")
        ),
        column(3, tags$div(style = "margin-top: 25px;",
          actionButton("run_immune", tagList(icon("play"), " Run Deconvolution"), class = "btn-primary btn-lg btn-block",
            style = "min-width: 220px; white-space: nowrap; padding-left: 20px; padding-right: 20px;")))
      ),
      uiOutput("immune_placeholder_ui"),
      uiOutput("immune_status_ui")
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("table"), " Immune Cell Proportions"),
      width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      DT::dataTableOutput("immune_proportions_table"),
      tags$div(style = "margin-top: 10px;", downloadButton("download_immune_proportions", tagList(icon("download"), " Proportions (CSV)"), class = "btn-success"))
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("chart-bar"), " Immune Cell Comparison by Group"),
      width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      plotOutput("immune_boxplot", height = "520px"),
      tags$div(style = "margin-top: 10px;",
        tags$span(icon("image", style = "margin-right: 6px;"), tags$strong("Download image (300 DPI):")),
        downloadButton("download_immune_boxplot_jpg", tagList(icon("download"), " JPG"), class = "btn-primary btn-sm", style = "margin-left: 8px;"),
        downloadButton("download_immune_boxplot_pdf", tagList(icon("download"), " PDF"), class = "btn-primary btn-sm", style = "margin-left: 4px;"))
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("chart-area"), " Immune Cell Fractions (Violin Plot)"),
      width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p("Distribution of immune cell fractions by group. Each cell type shows two violins (one per group) with individual samples as points.", style = "margin-bottom: 10px; font-size: 12px; color: #555;"),
      plotOutput("immune_violin_plot", height = "580px"),
      tags$div(style = "margin-top: 10px;",
        tags$span(icon("image", style = "margin-right: 6px;"), tags$strong("Download image (300 DPI):")),
        downloadButton("download_immune_violin_jpg", tagList(icon("download"), " JPG"), class = "btn-success btn-sm", style = "margin-left: 8px;"),
        downloadButton("download_immune_violin_pdf", tagList(icon("download"), " PDF"), class = "btn-success btn-sm", style = "margin-left: 4px;"))
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("th"), " Immune Cell Correlation Heatmap"),
      width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      plotOutput("immune_heatmap", height = "500px"),
      tags$div(style = "margin-top: 10px;",
        tags$span(icon("image", style = "margin-right: 6px;"), tags$strong("Download image (300 DPI):")),
        downloadButton("download_immune_heatmap_jpg", tagList(icon("download"), " JPG"), class = "btn-warning btn-sm", style = "margin-left: 8px;"),
        downloadButton("download_immune_heatmap_pdf", tagList(icon("download"), " PDF"), class = "btn-warning btn-sm", style = "margin-left: 4px;"))
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("project-diagram"), " Gene–Immune Cell Correlation (if genes provided)"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      uiOutput("immune_gene_corr_ui"),
      plotOutput("immune_gene_heatmap", height = "400px"),
      tags$div(style = "margin-top: 10px;",
        tags$span(icon("image", style = "margin-right: 6px;"), tags$strong("Download image (300 DPI):")),
        downloadButton("download_immune_gene_heatmap_jpg", tagList(icon("download"), " JPG"), class = "btn-info btn-sm", style = "margin-left: 8px;"),
        downloadButton("download_immune_gene_heatmap_pdf", tagList(icon("download"), " PDF"), class = "btn-info btn-sm", style = "margin-left: 4px;"))
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("chart-line"), " Correlation Lollipop (per gene)"),
      width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p("Select a gene to view its correlation with each immune cell type. Lollipop length = correlation; color = p-value; size = |correlation|.", style = "margin-bottom: 10px; font-size: 12px; color: #555;"),
      fluidRow(
        column(4, uiOutput("immune_lollipop_gene_ui")),
        column(4, tags$div(style = "margin-top: 25px;",
          tags$span(icon("image", style = "margin-right: 6px;"), tags$strong("Download image (300 DPI):")),
          downloadButton("download_immune_lollipop_jpg", tagList(icon("download"), " JPG"), class = "btn-warning btn-sm", style = "margin-left: 8px;"),
          downloadButton("download_immune_lollipop_pdf", tagList(icon("download"), " PDF"), class = "btn-warning btn-sm", style = "margin-left: 4px;")))
      ),
      plotOutput("immune_lollipop_plot", height = "680px")
    )
  ),
  fluidRow(
    box(
      title = tags$span(icon("file-alt"), " Process Summary"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
      uiOutput("immune_process_summary_ui"))
  ),
  fluidRow(
    box(width = 12, status = "primary", solidHeader = FALSE,
        tags$div(style = "text-align: center; padding: 20px 0;",
                 actionButton("next_page_immune_to_results",
                             tagList(icon("file-alt"), " View Results Summary"),
                             class = "btn-success btn-lg",
                             style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;")))
  )
)
