# ==============================================================================
# UI_PPI.R - Step 9: PPI Interaction (Common Genes)
# ==============================================================================

ui_ppi <- tabItem(
  tabName = "ppi",
  h2(icon("project-diagram"), " Step 9: PPI Interaction (Common Genes)"),
  
  fluidRow(
    box(
      title = tags$span(icon("info-circle"), " About this step"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p(tags$strong("Purpose:"), " Build a protein–protein interaction (PPI) network from common genes using STRINGdb and identify hub genes by centrality (degree, betweenness, PageRank). Network views support interpretation and feature selection for ML.", style = "margin-bottom: 8px;"),
      tags$p(tags$strong("Methods:"), " STRINGdb mapping and interaction retrieval; consensus hubs (genes appearing in top lists of multiple centrality measures); four layout options (Fruchterman–Reingold, circular, ggraph, Kamada–Kawai). Optional: centrality-weighted features for ML.", style = "margin-bottom: 8px;"),
      tags$p(tags$strong("Requirements:"), " Step 8 (Common Genes) completed; click 'Compute Common Genes' before running PPI.", style = "margin-bottom: 0;")
    )
  ),
  
  fluidRow(
    box(
      title = tags$span(icon("cogs"), " Run PPI Analysis"),
      width = 12, status = "primary", solidHeader = TRUE,
      fluidRow(
        column(4,
               numericInput("ppi_score_threshold",
                 tags$span("STRING score threshold (150–900)",
                   tags$i(class = "fa fa-question-circle param-help",
                          `data-toggle` = "tooltip", `data-placement` = "right",
                          title = "Minimum confidence score for protein-protein interactions from STRINGdb.<br><b>400</b> = medium confidence (default, recommended), <b>700</b> = high confidence (fewer but more reliable edges), <b>150</b> = low confidence (more edges, more noise).")),
                 value = 400, min = 150, max = 900, step = 50)),
        column(4,
               tags$div(style = "margin-top: 25px;",
                        actionButton("run_ppi",
                                    tagList(icon("play"), " Run PPI Analysis"),
                                    class = "btn-primary btn-lg btn-block")))
      ),
      uiOutput("ppi_placeholder_ui"),
      uiOutput("ppi_status_ui"),
      tags$p(tags$strong("Step 1:"), " Run PPI to find interacting and non-interacting genes (counts and list below). ",
             tags$strong("Step 2:"), " Use the network graphs further down for the selected interacting genes.", style = "margin-top: 12px; color: #495057;")
    )
  ),
  
  fluidRow(
    box(
      title = tags$span(icon("link"), " 1. Interacting & non-interacting genes (counts and list)"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p("Genes with at least one PPI edge = Interactive; unmapped or no interactions = Non-interactive. Check counts and table, then scroll to graphs.", style = "margin-bottom: 10px;"),
      uiOutput("ppi_interactive_summary_ui"),
      tags$hr(),
      DT::dataTableOutput("ppi_gene_status_table"),
      tags$div(style = "margin-top: 10px;",
               downloadButton("download_ppi_gene_status", tagList(icon("download"), " Download Interactive/Non-interactive (CSV)"), class = "btn-info"))
    )
  ),
  
  fluidRow(
    box(
      title = tags$span(icon("hand-pointer"), " Gene set for network graphs and next step"),
      width = 12, status = "primary", solidHeader = TRUE,
      fluidRow(
        column(4,
               radioButtons("ppi_gene_select_mode",
                            "Choose gene set (graph + list for ML):",
                            choices = c("Hub genes only" = "hub", "Top N by degree" = "topn", "Select genes manually" = "manual"),
                            selected = "topn")),
        column(5,
               uiOutput("ppi_manual_select_ui"),
               uiOutput("ppi_topn_ui")),
        column(3,
               tags$div(style = "margin-top: 25px;",
                        actionButton("ppi_apply_gene_set",
                                     tagList(icon("play"), " Run"),
                                     class = "btn-primary btn-block",
                                     title = "Generate network graphs for the selected gene set")))
      ),
      tags$div(style = "margin-top: 10px; font-size: 12px; color: #555;",
               tags$strong("Hub genes only:"), " consensus hubs (in 3+ centrality methods). ",
               tags$strong("Top N by degree:"), " top N connected genes. The chosen set is shown in the graphs and used in the list below and for Extract Data for ML.")
    )
  ),
  
  fluidRow(
    box(
      title = tags$span(icon("sitemap"), " 2. Network graphs (selected interacting genes) – 4 layouts (2 per row)"),
      width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE,
      uiOutput("ppi_view_summary_ui"),
      tags$hr(),
      fluidRow(
        column(6,
               tags$h5(icon("circle-nodes"), " 1. Fruchterman–Reingold (force-directed)", style = "margin-top: 8px; margin-bottom: 6px; color: #1976D2; font-size: 13px;"),
               plotOutput("ppi_plot_traditional", height = "520px", width = "100%"),
               tags$div(style = "margin-top: 6px;",
                 downloadButton("download_ppi_traditional_png", tagList(icon("download"), " PNG"), class = "btn-success btn-sm", style = "margin-right: 4px;"),
                 downloadButton("download_ppi_traditional_pdf", tagList(icon("download"), " PDF"), class = "btn-success btn-sm"))),
        column(6,
               tags$h5(icon("circle"), " 2. Circular layout (by degree)", style = "margin-top: 8px; margin-bottom: 6px; color: #388E3C; font-size: 13px;"),
               plotOutput("ppi_plot_circular", height = "520px", width = "100%"),
               tags$div(style = "margin-top: 6px;",
                 downloadButton("download_ppi_circular_png", tagList(icon("download"), " PNG"), class = "btn-success btn-sm", style = "margin-right: 4px;"),
                 downloadButton("download_ppi_circular_pdf", tagList(icon("download"), " PDF"), class = "btn-success btn-sm"))),
      fluidRow(
        column(6,
               tags$h5(icon("project-diagram"), " 3. ggraph (modern edges + nodes)", style = "margin-top: 8px; margin-bottom: 6px; color: #7B1FA2; font-size: 13px;"),
               plotOutput("ppi_plot_ggraph", height = "520px", width = "100%"),
               tags$div(style = "margin-top: 6px;",
                 downloadButton("download_ppi_ggraph_png", tagList(icon("download"), " PNG"), class = "btn-success btn-sm", style = "margin-right: 4px;"),
                 downloadButton("download_ppi_ggraph_pdf", tagList(icon("download"), " PDF"), class = "btn-success btn-sm"))),
        column(6,
               tags$h5(icon("project-diagram"), " 4. Kamada–Kawai (stress-minimization)", style = "margin-top: 8px; margin-bottom: 6px; color: #E65100; font-size: 13px;"),
               plotOutput("ppi_plot_kamada", height = "520px", width = "100%"),
               tags$div(style = "margin-top: 6px;",
                 downloadButton("download_ppi_kamada_png", tagList(icon("download"), " PNG"), class = "btn-success btn-sm", style = "margin-right: 4px;"),
                 downloadButton("download_ppi_kamada_pdf", tagList(icon("download"), " PDF"), class = "btn-success btn-sm"))),
      )
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("sitemap"), " 2b. Hub-only or Other-only network (no Hub/Other legend)"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p("One network: shows hub genes only or other genes only (top N connected, non-hub) depending on your selection above (Hub genes only / Top N by degree). Single color; no Hub/Other legend.", style = "margin-bottom: 10px; font-size: 12px; color: #555;"),
      uiOutput("ppi_hub_or_other_title_ui"),
      plotOutput("ppi_plot_hub_or_other", height = "520px", width = "100%"),
      tags$div(style = "margin-top: 6px;",
        downloadButton("download_ppi_hub_or_other_png", tagList(icon("download"), " PNG"), class = "btn-info btn-sm", style = "margin-right: 4px;"),
        downloadButton("download_ppi_hub_or_other_pdf", tagList(icon("download"), " PDF"), class = "btn-info btn-sm"))
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("table"), " Hub Gene Scores & Consensus Hubs"),
      width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
      tags$h5("Hub scores (all nodes)", style = "margin-bottom: 10px;"),
      DT::dataTableOutput("ppi_hub_table"),
      tags$hr(),
      tags$h5("Consensus hub genes (appear in 3+ methods)", style = "margin-top: 15px;"),
      uiOutput("ppi_consensus_ui"),
      tags$div(style = "margin-top: 15px;",
               downloadButton("download_ppi_hub_scores", tagList(icon("download"), " Hub Scores (CSV)"), class = "btn-success"),
               downloadButton("download_ppi_consensus", tagList(icon("download"), " Consensus Hubs (CSV)"), class = "btn-info", style = "margin-left: 10px;"),
               downloadButton("download_ppi_network_png", tagList(icon("download"), " PNG (300 DPI)"), class = "btn-warning btn-sm", style = "margin-left: 6px;"),
               downloadButton("download_ppi_network_jpg", tagList(icon("download"), " JPG (300 DPI)"), class = "btn-warning btn-sm", style = "margin-left: 4px;"),
               downloadButton("download_ppi_network_pdf", tagList(icon("download"), " PDF"), class = "btn-warning btn-sm", style = "margin-left: 4px;"))
    )
  ),
  
  fluidRow(
    box(
      title = tags$span(icon("list"), " Interactive Gene List (for downstream / machine learning)"),
      width = 12, status = "success", solidHeader = TRUE,
      tags$div(
        class = "alert alert-info",
        "Use the interactive gene list below in your next step (e.g. machine learning). ",
        "You can download the list as CSV or the expression matrix (samples × genes) if WGCNA data is available."
      ),
      uiOutput("ppi_interactive_list_summary_ui"),
      tags$hr(),
      tags$h5("Interactive genes (with PPI connections)", style = "margin-bottom: 10px;"),
      DT::dataTableOutput("ppi_interactive_list_table"),
      tags$hr(),
      tags$div(style = "margin-top: 10px;",
               downloadButton("download_ppi_interactive_genes", tagList(icon("download"), " Download Interactive Gene List (CSV)"), class = "btn-success"),
               uiOutput("ppi_extract_expr_ui"))
    )
  ),

  # ========== EXTRACT DATA FOR ML ==========
  fluidRow(
    box(
      title = tags$span(icon("database"), " Extract Data for Machine Learning"),
      width = 12, status = "primary", solidHeader = TRUE,
      tags$div(
        class = "alert alert-info",
        "Extract expression data (samples × genes) for ML from WGCNA datExpr. ",
        "The gene set used here comes from your selection above (Hub genes only, Top N by degree, or manual) in the network graphs section. This matrix is used on the next tab for LASSO, Random Forest, SVM-RFE, etc."
      ),
      tags$div(style = "margin-bottom: 15px;",
               actionButton("extract_ml_data",
                           tagList(icon("cogs"), " Extract Data for ML"),
                           class = "btn-primary btn-lg")),
      uiOutput("ppi_extracted_ml_status_ui"),
      uiOutput("ppi_extracted_ml_genes_ui"),
      tags$div(style = "margin-top: 10px;", uiOutput("ppi_download_extracted_ml_ui"))
    )
  ),
  fluidRow(
    box(
      title = tags$span(icon("file-alt"), " Process Summary"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
      uiOutput("ppi_process_summary_ui"))
  ),
  fluidRow(
    box(width = 12, status = "info", solidHeader = FALSE,
        tags$div(class = "next-btn", style = "text-align: center; padding: 20px 0;",
                 actionButton("next_page_ppi",
                             tagList(icon("arrow-right"), " Next: Machine Learning Process"),
                             class = "btn-success btn-lg",
                             style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;")))
  )
  )
)
