# ==============================================================================
# UI_WGCNA.R - Step 7: WGCNA Analysis Tab
# ==============================================================================

ui_wgcna <- tabItem(
    tabName = "wgcna",
    h2(icon("project-diagram"), " Step 7: WGCNA Network Analysis"),
    
    fluidRow(
      box(
        title = tags$span(icon("network-wired"), " WGCNA - Weighted Gene Co-expression Network Analysis"), 
        width = 12, status = "info", solidHeader = TRUE,
        tags$div(
          class = "alert alert-info",
          style = "padding: 15px; margin: 0; border-left: 4px solid #17a2b8;",
          icon("info-circle"),
          " WGCNA requires batch-corrected data and defined groups.",
          tags$br(),
          tags$strong("Recommended workflow:"),
          " Complete Differential Expression Analysis (Step 6) before running WGCNA for best results.",
          tags$br(),
          "Traits are derived from your group mapping (Normal vs Disease).",
          tags$br(),
          tags$strong(icon("sliders-h"), " Disease-specific modules:"),
          " In Step 4 and Step 6 you can ", tags$strong("select any threshold"), " — GS P-value, MM correlation, and module p-value/correlation — to define which genes and modules are disease-related. Hover over ", icon("question-circle"), " next to each parameter for help.",
          tags$br(),
          tags$small(icon("lightbulb"), " Tip: After selecting top variable genes, check the sample dendrogram for outliers (Step 1); one bad sample can ruin the network. Use Step 2 to pick a soft-threshold power (R² > 0.8); grey module = unassigned genes.")
        )
      )
    ),
    
    # ========== STEP 1: DATA PREPARATION ==========
    fluidRow(
      box(
        title = tags$span(icon("database"), " Step 1: Data Preparation & QC"), 
        width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
        tags$div(
          style = "padding: 15px 0;",
          fluidRow(
            column(12,
              radioButtons("wgcna_gene_mode",
                           label = tags$span(tags$strong("Gene selection:"),
                             tags$i(class = "fa fa-question-circle param-help",
                                    `data-toggle` = "tooltip", `data-placement` = "right",
                                    title = "Use all common genes (from normalization + batch effect) or select top variable genes by variance.")),
                           choices = list(
                             "All genes" = "all_common",
                             "Top variable genes" = "top_variable"
                           ),
                           selected = "top_variable",
                           inline = TRUE,
                           width = "100%"),
              tags$small("Use all genes common across samples after normalization & batch effect, or filter by variance.",
                        style = "color: #6c757d; display: block; margin-bottom: 15px;")
            )
          ),
          fluidRow(
            column(4,
              conditionalPanel(
                condition = "input.wgcna_gene_mode == 'top_variable'",
                numericInput("wgcna_top_genes",
                            label = tags$span(tags$strong("Top Variable Genes:"),
                              tags$i(class = "fa fa-question-circle param-help",
                                     `data-toggle` = "tooltip", `data-placement` = "right",
                                     title = "Number of most variable genes to use for WGCNA. Higher values capture more biology but increase computation time.<br><b>5000</b> = standard, <b>3000</b> = faster/focused, <b>8000-10000</b> = comprehensive.")),
                            value = 5000,
                            min = 1000,
                            max = 15000,
                            step = 1000),
                tags$small("Select highly variable genes for WGCNA",
                          style = "color: #6c757d;")
              )
            ),
            column(4,
              numericInput("wgcna_min_samples",
                          label = tags$span(tags$strong("Min Samples per Gene (fraction):"),
                            tags$i(class = "fa fa-question-circle param-help",
                                   `data-toggle` = "tooltip", `data-placement` = "right",
                                   title = "Fraction of samples that must have non-missing expression for a gene to be included.<br><b>0.5</b> = at least 50% of samples (default), <b>0.7</b> = stricter, <b>0.3</b> = more lenient.")),
                          value = 0.5,
                          min = 0.1,
                          max = 1.0,
                          step = 0.1),
              tags$small("Fraction of samples with non-missing expression",
                        style = "color: #6c757d;")
            ),
            column(4,
              tags$div(
                style = "margin-top: 25px;",
                actionButton("prepare_wgcna", 
                            tagList(icon("cogs"), " Prepare Data"), 
                            class = "btn-primary btn-block",
                            style = "font-size: 16px; padding: 12px 20px;")
              )
            )
          ),
          tags$hr(),
          uiOutput("wgcna_qc_status"),
          tags$div(
            class = "alert alert-warning",
            style = "margin: 15px 0 10px 0; padding: 12px 15px;",
            tags$strong(icon("exclamation-triangle"), " Check for outliers"),
            tags$br(),
            tags$small(
              "One bad sample can ruin a WGCNA network. ",
              "If no outliers: skip and go to Step 2. If outliers: exclude them, then go to Step 3."
            ),
            tags$ul(
              style = "margin: 8px 0 0 18px; padding: 0;",
              tags$li(tags$strong("How to find outliers:"), " In the dendrogram, look for a ", tags$em("long vertical branch"), " before a sample joins the rest — that sample is far from others."),
              tags$li(tags$strong("If you see such a branch:"), " Note the sample ID at the bottom of that branch, then exclude it (right panel)."),
              tags$li(tags$strong("If all samples cluster without a very long single branch:"), " No outliers — click ", tags$em("No outliers — proceed"), " below.")
            )
          ),
          fluidRow(
            column(8,
              tags$div(
                style = "margin-top: 8px; width: 100%; overflow: hidden;",
                plotOutput("wgcna_sample_tree", height = "420px", width = "100%")
              )
            ),
            column(4,
              tags$div(
                style = "margin-top: 8px;",
                tags$label(tags$strong("Exclude outlier samples (optional):"), style = "display: block; margin-bottom: 6px;"),
                tags$small("Comma-separated sample IDs to remove. Or use \"Detect\" to suggest outliers by tree height.", style = "color: #6c757d; display: block; margin-bottom: 8px;"),
                textInput("wgcna_exclude_samples",
                          label = NULL,
                          placeholder = "e.g. Sample_12, GSM123",
                          width = "100%"),
                fluidRow(
                  column(6,
                    actionButton("wgcna_apply_exclude",
                                 tagList(icon("user-minus"), " Exclude & update"),
                                 class = "btn-outline-warning btn-sm",
                                 style = "margin-top: 6px; width: 100%;")
                  ),
                  column(6,
                    actionButton("wgcna_detect_outliers",
                                 tagList(icon("search"), " Detect"),
                                 class = "btn-outline-secondary btn-sm",
                                 style = "margin-top: 6px; width: 100%;",
                                 title = "Suggest outliers by dendrogram height")
                ),
                uiOutput("wgcna_suggested_outliers_ui"),
                uiOutput("wgcna_exclude_status"),
                tags$hr(style = "margin: 12px 0 8px 0;"),
                actionButton("wgcna_skip_outliers",
                             tagList(icon("forward"), " No outliers — proceed to Step 2"),
                             class = "btn-success btn-sm",
                             style = "margin-top: 4px; width: 100%;",
                             title = "No outliers in my data; continue workflow")
              )
            )
          ),
          tags$hr(),
          uiOutput("wgcna_gene_list_ui")
        )
      )
    ),
    
    # ========== STEP 2: SOFT THRESHOLD ==========
    fluidRow(
      box(
        title = tags$span(icon("sliders-h"), " Step 2: Pick Soft Threshold Power"), 
        width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        tags$div(
          style = "padding: 15px 0;",
          fluidRow(
            column(4,
              numericInput("wgcna_power_max",
                          label = tags$strong("Max Power to Test:"),
                          value = 20,
                          min = 10,
                          max = 30,
                          step = 2),
              tags$div(
                style = "margin-top: 15px;",
                actionButton("pick_soft_threshold", 
                            tagList(icon("calculator"), " Calculate Power"), 
                            class = "btn-warning btn-block",
                            style = "font-size: 16px; padding: 12px 20px;")
              )
            ),
            column(8,
              uiOutput("soft_threshold_result")
            )
          ),
          tags$hr(),
          plotOutput("soft_threshold_plot", height = "400px")
        )
      )
    ),
    
    # ========== STEP 3: NETWORK CONSTRUCTION ==========
    fluidRow(
      box(
        title = tags$span(icon("project-diagram"), " Step 3: Network Construction & Module Detection"), 
        width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        tags$div(
          style = "padding: 15px 0;",
          fluidRow(
            column(3,
              numericInput("wgcna_power",
                          label = tags$span(tags$strong("Soft Threshold Power:"),
                            tags$i(class = "fa fa-question-circle param-help",
                                   `data-toggle` = "tooltip", `data-placement` = "right",
                                   title = "Soft-thresholding power for the adjacency matrix. Use the value from Step 2 where scale-free R&sup2; &gt; 0.8.<br><b>6</b> = typical for signed networks, <b>8-12</b> = common range. Too high (e.g. 26) = very sparse network with few modules.")),
                          value = 6,
                          min = 1,
                          max = 30,
                          step = 1),
              tags$small("Power from Step 2 (typically 6–12). High power (e.g. 26) = fewer connections, may give few modules.",
                        style = "color: #6c757d;")
            ),
            column(3,
              numericInput("wgcna_min_module_size",
                          label = tags$span(tags$strong("Min Module Size:"),
                            tags$i(class = "fa fa-question-circle param-help",
                                   `data-toggle` = "tooltip", `data-placement` = "right",
                                   title = "Minimum number of genes required to form a module.<br><b>30</b> = standard for discovery, <b>20</b> = if you have few genes, <b>50</b> = larger, more robust modules.")),
                          value = 30,
                          min = 10,
                          max = 100,
                          step = 10),
              tags$small("Minimum genes per module",
                        style = "color: #6c757d;")
            ),
            column(3,
              tags$div(
                style = "margin-top: 25px;",
                actionButton("run_wgcna", 
                            tagList(icon("play"), " Build Network"), 
                            class = "btn-success btn-block",
                            style = "font-size: 16px; padding: 12px 20px;")
              )
            )
          ),
          fluidRow(
            column(4,
              selectInput("wgcna_deep_split",
                          label = tags$span(tags$strong("Deep split (sensitivity):"),
                            tags$i(class = "fa fa-question-circle param-help",
                                   `data-toggle` = "tooltip", `data-placement` = "right",
                                   title = "Controls module detection sensitivity (how aggressively the dendrogram is cut).<br><b>0</b> = conservative (fewer, larger modules), <b>2</b> = balanced default, <b>4</b> = aggressive (many smaller modules). Higher values may split biologically meaningful modules.")),
                          choices = setNames(0:4, c("0 (conservative)", "1", "2 (default)", "3", "4 (more modules)")),
                          selected = 2,
                          width = "100%"),
              tags$small("Higher = more, smaller modules; 2 is standard.",
                        style = "color: #6c757d;")
            )
          ),
          tags$hr(),
          uiOutput("wgcna_module_info"),
          tags$div(
            style = "margin-top: 15px;",
            plotOutput("wgcna_dendrogram", height = "500px")
          )
        )
      )
    ),
    
    # ========== STEP 4: MODULE-TRAIT ANALYSIS ==========
    fluidRow(
      box(
        title = tags$span(icon("chart-bar"), " Step 4: Module-Trait Relationships & Gene Metrics (GS/MM)"), 
        width = 12, status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        tags$div(
          style = "padding: 15px 0;",
          fluidRow(
            column(3,
              numericInput("gs_pval_threshold",
                          label = tags$span(tags$strong("Gene Sig (GS) P-value Threshold:"),
                            tags$i(class = "fa fa-question-circle param-help", `data-toggle` = "tooltip", `data-placement` = "top",
                                   title = "P-value cutoff for gene significance (GS) with the Disease trait.<br><b>0.05</b> = standard, <b>0.01</b> = stringent. You can choose any value for disease-specific gene sets.")),
                          value = 0.05,
                          min = 0,
                          max = 1,
                          step = 0.01),
              tags$small("Choose any value — defines disease-related genes (e.g. 0.05)",
                        style = "color: #17a2b8; font-weight: 500;")
            ),
            column(3,
              numericInput("mm_cor_threshold",
                          label = tags$span(tags$strong("Module Membership (MM) Threshold:"),
                            tags$i(class = "fa fa-question-circle param-help", `data-toggle` = "tooltip", `data-placement` = "top",
                                   title = "Minimum |correlation| for a gene to belong to a module.<br><b>0.8</b> = strong membership, <b>0.5</b> = looser. Choose any threshold for disease-specific gene lists.")),
                          value = 0.8,
                          min = 0,
                          max = 1,
                          step = 0.1),
              tags$small("Choose any value — genes with |MM| above this are in the module (e.g. 0.8)",
                        style = "color: #17a2b8; font-weight: 500;")
            ),
            column(6,
              tags$div(
                style = "margin-top: 25px;",
                actionButton("calculate_module_trait", 
                            tagList(icon("calculator"), " Calculate Correlations & GS/MM"), 
                            class = "btn-danger btn-block",
                            style = "font-size: 16px; padding: 12px 20px;")
              )
            )
          ),
          tags$hr(),
          uiOutput("significant_module_count_ui"),
          tags$div(
            style = "margin-top: 15px;",
            plotOutput("module_trait_heatmap", height = "600px")
          ),
          tags$hr(),
          tags$div(
            style = "text-align: center;",
            downloadButton("download_module_trait", tagList(icon("download"), " PNG"), class = "btn-success btn-sm", style = "margin-right: 6px;"),
            downloadButton("download_module_trait_jpg", tagList(icon("download"), " JPG"), class = "btn-success btn-sm", style = "margin-right: 6px;"),
            downloadButton("download_module_trait_pdf", tagList(icon("download"), " PDF"), class = "btn-success btn-sm", style = "margin-right: 10px;"),
            downloadButton("download_all_sig_genes", 
                          tagList(icon("download"), " Download All Significant Genes"),
                          class = "btn-warning")
          )
        )
      )
    ),
    
    # ========== STEP 5: ME RELATIONSHIPS ==========
    fluidRow(
      box(
        title = tags$span(icon("project-diagram"), " Step 5: Module Eigengene Relationships (Clustering)"), 
        width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        tags$div(
          style = "padding: 15px 0;",
          tags$div(
            style = "text-align: center; margin-bottom: 20px;",
            actionButton("calculate_me_relationships",
                        tagList(icon("link"), " Calculate Module Relationships"),
                        class = "btn-success btn-lg",
                        style = "font-size: 16px; padding: 12px 30px;")
          ),
          tags$hr(),
          fluidRow(
            column(6,
              tags$h5(icon("th"), " ME Correlation Heatmap",
                     style = "color: #2c3e50; margin-bottom: 15px;"),
              plotOutput("me_correlation_heatmap", height = "500px")
            ),
            column(6,
              tags$h5(icon("sitemap"), " ME Dendrogram",
                     style = "color: #2c3e50; margin-bottom: 15px;"),
              plotOutput("me_dendrogram_plot", height = "500px")
            )
          ),
          tags$hr(),
          fluidRow(
            column(6,
              tags$h5(icon("dot-circle"), " Module Eigengene Scatter Plot (ME1 vs ME2)",
                     style = "color: #2c3e50; margin-bottom: 15px;"),
              plotOutput("me_scatter_plot", height = "400px")
            ),
            column(6,
              tags$h5(icon("th"), " Eigengene Distance Heatmap",
                     style = "color: #2c3e50; margin-bottom: 15px;"),
              plotOutput("eigengene_distance_heatmap", height = "400px")
            )
          )
        )
      )
    ),
    
    # ========== STEP 6: SIGNIFICANT MODULE ANALYSIS ==========
    fluidRow(
      box(
        title = tags$span(icon("star"), " Step 6: Significant Module Analysis"), 
        width = 12, status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        tags$div(
          style = "padding: 15px 0;",
          fluidRow(
            column(3,
              numericInput("sig_module_pval_threshold",
                          label = tags$span(tags$strong("P-value Threshold:"),
                            tags$i(class = "fa fa-question-circle param-help",
                                   `data-toggle` = "tooltip", `data-placement` = "top",
                                   title = "Maximum p-value for a module-trait correlation to be considered significant.<br><b>0.05</b> = standard, <b>0.01</b> = stringent.")),
                          value = 0.05,
                          min = 0,
                          max = 1,
                          step = 0.01)),
            column(3,
              numericInput("sig_module_cor_threshold",
                          label = tags$span(tags$strong("Correlation Threshold:"),
                            tags$i(class = "fa fa-question-circle param-help",
                                   `data-toggle` = "tooltip", `data-placement` = "top",
                                   title = "Minimum absolute correlation between module eigengene and trait.<br><b>0.2</b> = lenient (captures weak associations), <b>0.4</b> = moderate, <b>0.6</b> = strong associations only.")),
                          value = 0.2,
                          min = 0,
                          max = 1,
                          step = 0.1)),
            column(6,
              tags$div(
                style = "margin-top: 25px;",
                actionButton("identify_significant_modules",
                            tagList(icon("search"), " Identify Significant Modules"),
                            class = "btn-danger btn-block",
                            style = "font-size: 16px; padding: 12px 20px;")
              )
            )
          ),
          tags$hr(),
          uiOutput("significant_modules_summary_ui"),
          tags$hr(),
          fluidRow(
            column(6,
              tags$h5(icon("chart-bar"), " Module Significance Barplot",
                     style = "color: #2c3e50; margin-bottom: 15px;"),
              plotOutput("module_significance_barplot", height = "400px")
            ),
            column(6,
              tags$h5(icon("chart-line"), " Module Size vs Correlation",
                     style = "color: #2c3e50; margin-bottom: 15px;"),
              plotOutput("module_size_correlation_plot", height = "400px")
            )
          ),
          tags$hr(),
          tags$h5(icon("table"), " Significant Module Summary Table",
                 style = "color: #2c3e50; margin-bottom: 15px;"),
          DT::dataTableOutput("significant_modules_table"),
          tags$hr(),
          fluidRow(
            column(4,
              downloadButton("download_sig_modules_table",
                            tagList(icon("download"), " Download Significant Modules Table"),
                            class = "btn-danger btn-block")
            ),
            column(4,
              downloadButton("download_sig_modules_genes",
                            tagList(icon("download"), " Download Gene Lists per Module"),
                            class = "btn-warning btn-block")
            ),
            column(4,
              downloadButton("download_module_analysis_plots",
                            tagList(icon("download"), " Download Analysis Plots (PDF)"),
                            class = "btn-info btn-block")
            )
          )
        )
      )
    ),
    
    # ========== MODULE GENES TABLE ==========
    fluidRow(
      box(
        title = tags$span(icon("table"), " Module Gene Lists & Significant Genes"), 
        width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        tags$div(
          style = "padding: 15px 0;",
          fluidRow(
            column(4,
              selectInput("select_module",
                         label = tags$strong("Select Module to view genes:"),
                         choices = NULL,
                         width = "100%")
            ),
            column(4,
              selectInput("filter_sig_genes",
                         label = tags$strong("Filter by Significance:"),
                         choices = c("All Genes" = "all",
                                   "Significant Genes Only (GS/MM)" = "sig_only"),
                         selected = "all",
                         width = "100%")
            ),
            column(4,
              uiOutput("module_gene_stats")
            )
          ),
          tags$hr(),
          DT::dataTableOutput("module_genes_table"),
          tags$hr(),
          fluidRow(
            column(4,
              downloadButton("download_module_genes",
                            tagList(icon("download"), " Download Module Genes (with GS/MM)"),
                            class = "btn-success btn-block")
            ),
            column(4,
              downloadButton("download_all_modules",
                            tagList(icon("download"), " Download All Module Assignments"),
                            class = "btn-primary btn-block")
            ),
            column(4,
              downloadButton("download_module_eigengenes",
                            tagList(icon("download"), " Download Module Eigengenes"),
                            class = "btn-info btn-block")
            )
          )
        )
      )
    ),
    
    # ========== PROCESSING LOG ==========
    fluidRow(
      box(
        title = tags$span(icon("terminal"), " WGCNA Processing Log"), 
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        tags$div(
          style = "margin-bottom: 10px;",
          actionButton("clear_wgcna_log_btn", 
                      tagList(icon("refresh"), " Refresh Log"), 
                      class = "btn-sm btn-warning")
        ),
        tags$div(
          class = "scrollable-log-area",
          style = "max-height: 500px; overflow-y: auto; font-family: 'Courier New', monospace; font-size: 12px; background: #263238; color: #66BB6A; padding: 15px; border-radius: 8px; white-space: pre-wrap;",
          verbatimTextOutput("wgcna_log", placeholder = TRUE)
        ),
        tags$div(
          class = "step-timer",
          style = "margin-top: 15px;",
          tags$span(class = "label", "Elapsed:"),
          textOutput("wgcna_timer", inline = TRUE)
        )
      )
    ),
    fluidRow(
      box(width = 12, status = "info", solidHeader = FALSE,
          tags$div(class = "next-btn", style = "text-align: center; padding: 20px 0;",
                   actionButton("next_page_wgcna", "Next: Common Genes (DEG & WGCNA)",
                                icon = icon("arrow-right"), class = "btn-success btn-lg",
                                style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;")))
    )
  )
  )  # tabItem
