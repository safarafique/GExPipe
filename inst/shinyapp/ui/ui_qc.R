# ==============================================================================
# UI_QC.R - Step 2: QC & Visualization Tab
# ==============================================================================

ui_qc <- tabItem(
    tabName = "qc",
    h2(icon("chart-bar"), " Step 2: Quality Control & Visualization"),

    fluidRow(
      box(
        title = tags$span(icon("info-circle"), " About this step"),
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p(tags$strong("Purpose:"), " Assess data quality and gene overlap across datasets before normalization. Visualizations help detect batch effects and confirm sample composition.", style = "margin-bottom: 8px;"),
        tags$p(tags$strong("Plots:"), " Venn and UpSet diagrams show gene set overlap; boxplot and density show expression distribution per sample. Use these to verify download success and identify outliers.", style = "margin-bottom: 0;")
      )
    ),
    fluidRow(
      box(title = tags$span(icon("venus-mars"), " Venn Diagram - Gene Overlap"), 
          width = 6, status = "primary", solidHeader = TRUE,
          plotOutput("venn_plot", height = "500px"),
          tags$p(icon("info-circle"), " Counts are per-dataset gene lists from Step 1 (Download). Overlap = ", tags$strong("exact match of rownames"), " (gene symbols or probe IDs). ", "If 4-way overlap is 0, datasets likely use ", tags$strong("different platforms"), " (different probe IDs); use same GPL or ensure all are mapped to gene symbols.", style = "margin-top: 8px; font-size: 12px; color: #555;")),
      box(title = tags$span(icon("project-diagram"), " UpSet Plot - Gene Intersections"), 
          width = 6, status = "info", solidHeader = TRUE,
          plotOutput("upset_plot", height = "500px"))
    ),
    
    fluidRow(
      box(title = tags$span(icon("chart-line"), " Quality Control Plots"), 
          width = 12, status = "warning", solidHeader = TRUE,
          tabsetPanel(
            tabPanel("Boxplot", plotOutput("qc_boxplot", height = "400px")),
            tabPanel("Density", plotOutput("qc_density", height = "400px"))
          )
      )
    ),

    # ---- Sample Outlier Detection ----
    fluidRow(
      box(
        title = tags$span(icon("search"), " Sample Outlier Detection",
                          tags$span("BEFORE NORMALIZATION", class = "label label-danger",
                                    style = "margin-left: 8px; font-size: 10px;")),
        width = 12, status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$div(
          style = "padding: 10px 14px; background: linear-gradient(135deg, #fef9e7, #fdebd0); border-left: 4px solid #f39c12; border-radius: 4px; margin-bottom: 15px;",
          icon("lightbulb", style = "color: #f39c12; margin-right: 6px;"),
          tags$strong("Detect and remove outlier samples before normalization. "),
          tags$span("Uses two complementary methods:", style = "font-size: 13px;"),
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
              class = "btn-danger btn-lg", style = "font-weight: bold; width: 100%;")
          ),
          column(9, uiOutput("qc_outlier_summary_ui"))
        ),
        uiOutput("qc_outlier_results_ui")
      )
    ),
    
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
    ),
    
    fluidRow(
      box(width = 12, status = "info", solidHeader = FALSE,
          tags$div(class = "next-btn", style = "text-align: center; padding: 20px 0;",
                   uiOutput("qc_next_button")))
    ),
  )
