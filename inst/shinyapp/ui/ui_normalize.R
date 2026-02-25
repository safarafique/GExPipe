# ==============================================================================
# UI_NORMALIZE.R - Step 3: Normalize Data Tab
# ==============================================================================

ui_normalize <- tabItem(
    tabName = "normalize",
    h2(icon("balance-scale"), " Step 3: Data Normalization"),

    # Auto-handled banner (visible when DESeq2 or edgeR is selected)
    conditionalPanel(
      condition = "input.de_method == 'deseq2' || input.de_method == 'edger'",
      fluidRow(
        box(
          width = 12, status = "success", solidHeader = TRUE,
          title = tags$span(icon("check-circle"), " Normalization Auto-Handled (Count-based DE Mode)"),
          tags$div(
            style = "padding: 15px; background: linear-gradient(135deg, #d5f5e3 0%, #abebc6 100%); border-radius: 8px;",
            tags$p(
              icon("dna"), tags$strong(" You selected a count-based DE method (DESeq2 / edgeR)."),
              style = "font-size: 15px; margin-bottom: 10px; color: #1e8449;"
            ),
            tags$ul(
              style = "font-size: 13px; color: #2c3e50; line-height: 2;",
              tags$li(tags$strong("For DE analysis:"), " DESeq2 uses median-of-ratios normalization; edgeR uses TMM normalization. Both work directly on raw counts — no manual normalization needed."),
              tags$li(tags$strong("For downstream steps:"), " Normalization was run automatically in the background (TMM + quantile) so that WGCNA, heatmaps, and other visualizations work correctly."),
              tags$li(tags$strong("Next step:"), " Proceed to ", tags$b("Step 4: Select Groups"), " — this step is already done for you.")
            ),
            tags$div(
              style = "text-align: center; margin-top: 15px;",
              actionButton("go_to_groups_from_norm", tagList(icon("arrow-right"), " Go to Step 4: Select Groups"),
                          class = "btn-success btn-lg", style = "font-size: 16px; padding: 12px 30px; border-radius: 25px;")
            )
          )
        )
      )
    ),

    # Standard normalization content (hidden when DESeq2 or edgeR is selected)
    conditionalPanel(
      condition = "input.de_method != 'deseq2' && input.de_method != 'edger'",
      fluidRow(
        box(
          title = tags$span(icon("info-circle"), " About this step"),
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
          tags$p(tags$strong("Purpose:"), " Normalize expression data to make samples comparable and reduce technical variation. Platform-specific methods are applied for accuracy.", style = "margin-bottom: 8px;"),
          tags$p(tags$strong("Methods:"), " RNA-seq: TMM (trimmed mean of M-values) + log2-CPM; Microarray: log2 transform + quantile normalization; Merged: global quantile normalization. Low-expression and non-common genes are filtered.", style = "margin-bottom: 0;")
        )
      ),
    fluidRow(
      box(
        title = tags$span(icon("cogs"), " Normalization Strategy"), 
        width = 12, status = "success", solidHeader = TRUE,
        tags$div(
          style = "padding: 10px 0;",
          tags$p(
            tags$strong("Platform-specific normalization:"),
            style = "font-size: 15px; margin-bottom: 10px;"
          ),
          tags$ul(
            tags$li(tags$b("RNA-seq:"), " TMM + log2-CPM or log2(CPM+1) only"),
            tags$li(tags$b("Microarray:"), " Quantile (Series Matrix) or RMA (probe-level, requires CEL)"),
            tags$li(tags$b("Combined:"), " Global quantile")
          ),
          hr(),
          tags$p(tags$strong("Normalization method choices:"), style = "margin-bottom: 10px;"),
          fluidRow(
            column(6,
                   tags$label("Microarray:", style = "font-weight: bold;"),
                   radioButtons("micro_norm_method", label = NULL,
                               choices = list(
                                 "Quantile (default, for Series Matrix)" = "quantile",
                                 "RMA (probe-level; requires CEL in GEO supplementary)" = "rma"
                               ), selected = "quantile", width = "100%")
            ),
            column(6,
                   tags$label("RNA-seq:", style = "font-weight: bold;"),
                   radioButtons("rnaseq_norm_method", label = NULL,
                               choices = list(
                                 "TMM + log2-CPM (recommended)" = "TMM",
                                 "log2(CPM+1) only (quick exploration)" = "log2cpm_only"
                               ), selected = "TMM", width = "100%")
            )
          ),
          tags$div(
            style = "padding: 15px; background: #e8f5e9; border-left: 4px solid #4caf50; border-radius: 5px; margin: 15px 0;",
            tags$p(
              tags$strong(icon("info-circle"), " Automatic Gene Filtering:"),
              style = "margin-bottom: 10px; color: #2c3e50; font-size: 15px;"
            ),
            tags$div(
              style = "padding: 10px; background: white; border-radius: 3px;",
              tags$p(
                tags$strong("Why filtering is necessary:"),
                style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"
              ),
              tags$ul(
                style = "margin: 0; padding-left: 20px; color: #495057; font-size: 13px; line-height: 1.8;",
                tags$li("Ensures consistent gene sets across all datasets for accurate comparison"),
                tags$li("Prevents errors in downstream analysis (batch correction, differential expression)"),
                tags$li("Removes platform-specific genes that cannot be compared across datasets"),
                tags$li("Keeps only high-confidence genes present in ALL datasets (intersection)")
              ),
              tags$p(
                tags$small(
                  icon("lightbulb", style = "margin-right: 5px; color: #ff9800;"),
                  tags$em("Filtering to common genes happens automatically in the background. Only genes present in all datasets will be retained for downstream analysis."),
                  style = "color: #6c757d; font-size: 12px; margin-top: 10px; display: block;"
                )
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
            " Multiple plots showing different aspects of normalization quality.",
            " After normalization, distributions should be more aligned across samples and datasets.",
            style = "color: #495057; font-size: 13px; margin-bottom: 15px; padding: 10px; background: #f8f9fa; border-radius: 5px;"
          )
        )
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("chart-bar"), " Expression Distribution by Dataset"), 
        width = 12, status = "info", solidHeader = TRUE,
        plotOutput("normalization_plot", height = "400px")
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("wave-square"), " Overall Expression Distribution"), 
        width = 6, status = "success", solidHeader = TRUE,
        plotOutput("normalization_density", height = "350px")
      ),
      box(
        title = tags$span(icon("chart-line"), " Quantile-Quantile (Q-Q) Plot"), 
        width = 6, status = "warning", solidHeader = TRUE,
        plotOutput("normalization_qq", height = "350px")
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("chart-bar"), " Median & Range Alignment"), 
        width = 6, status = "info", solidHeader = TRUE,
        plotOutput("normalization_median_range", height = "400px")
      ),
      box(
        title = tags$span(icon("wave-square"), " Distribution Overlap"), 
        width = 6, status = "primary", solidHeader = TRUE,
        plotOutput("normalization_distribution_overlap", height = "400px")
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("chart-area"), " Intensity Bias - MA Plot"), 
        width = 6, status = "warning", solidHeader = TRUE,
        plotOutput("normalization_ma_plot", height = "400px")
      ),
      box(
        title = tags$span(icon("dot-circle"), " Variance Stability - Mean-Variance Plot"), 
        width = 6, status = "success", solidHeader = TRUE,
        plotOutput("normalization_mean_variance", height = "400px")
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("th"), " Sample Correlation - Before Normalization"), 
        width = 6, status = "danger", solidHeader = TRUE,
        plotOutput("normalization_corr_before", height = "400px")
      ),
      box(
        title = tags$span(icon("th"), " Sample Correlation - After Normalization"), 
        width = 6, status = "success", solidHeader = TRUE,
        plotOutput("normalization_corr_after", height = "400px")
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
            tableOutput("normalization_summary_table")
          ),
          tags$hr(),
          tags$div(
            style = "margin-top: 20px;",
            tags$h4(icon("file-alt"), " Detailed Log", 
                   style = "color: #2c3e50; margin-bottom: 15px;"),
            verbatimTextOutput("normalization_log")
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
      box(width = 12, status = "info", solidHeader = FALSE,
          tags$div(class = "next-btn", style = "text-align: center; padding: 20px 0;",
                   actionButton("next_page_normalize", "Next: Select Groups",
                                icon = icon("arrow-right"), class = "btn-success btn-lg",
                                style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;")))
    )
    ) # end conditionalPanel for limma mode
  )
