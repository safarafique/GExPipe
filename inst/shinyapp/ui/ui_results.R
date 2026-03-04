# ==============================================================================
# UI_RESULTS.R - Step 6: Differential Gene Expression Analysis Tab
# ==============================================================================

ui_results <- tabItem(
    tabName = "results",
    h2(icon("dna"), " Step 6: Differential Gene Expression Analysis"),

    fluidRow(
      box(
        title = tags$span(icon("info-circle"), " About this step"),
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p(tags$strong("Purpose:"), " Identify genes that are differentially expressed between groups (e.g. disease vs control). The method you selected in Step 1 determines the statistical approach.", style = "margin-bottom: 8px;"),
        tags$p(tags$strong("limma:"), " Empirical Bayes moderated t-statistics — uses batch-corrected, normalized expression.", style = "margin-bottom: 4px;"),
        tags$p(tags$strong("DESeq2:"), " Negative binomial GLM — uses raw integer counts with batch as covariate. DESeq2 applies its own internal normalization (median-of-ratios), so it bypasses the app's normalization for DE.", style = "margin-bottom: 8px;"),
        tags$p(tags$strong("Parameters:"), " LogFC cutoff (fold-change threshold), adjusted p-value cutoff (Benjamini–Hochberg), and number of top genes for heatmap. Results include volcano plot, top-DEG table, and heatmap.", style = "margin-bottom: 0;")
      )
    ),
    # Show which method is active
    fluidRow(
      uiOutput("de_method_banner")
    ),
    fluidRow(
box(
        title = tags$span(icon("sliders-h"), " DE Parameters — choose any threshold"),
        width = 12, status = "primary", solidHeader = TRUE,
        tags$div(
          style = "padding: 14px 18px; margin-bottom: 16px; border-radius: 10px; background: linear-gradient(135deg, #e8f4f8 0%, #e8f0ff 100%); border-left: 4px solid #3498db;",
          tags$p(tags$strong(icon("info-circle"), " Disease-specific DEGs:"), " Set ", tags$strong("LogFC cutoff"), " and ", tags$strong("Adj. P-value"), " below. Genes with |log2FC| above the cutoff and adj.P.Val below the cutoff are your disease-associated set. Examples: 0.05 + 0.5 (standard), 0.01 + 1.0 (stringent). Hover over the ", icon("question-circle"), " for more.", style = "margin: 0; color: #2c3e50;")
        ),
        column(3,
          numericInput("logfc_cutoff", tags$span("LogFC cutoff:",
            tags$i(class = "fa fa-question-circle param-help",
                   `data-toggle` = "tooltip", `data-placement` = "top",
                   title = "Log2 fold-change threshold. Genes with |log2FC| above this value are considered differentially expressed.<br><b>0.5</b> = mild (1.4-fold), <b>1.0</b> = strong (2-fold), <b>1.5</b> = very strong (2.8-fold).")),
            0.5, step = 0.1)),
        column(3,
          numericInput("padj_cutoff", tags$span("Adj. P-value:",
            tags$i(class = "fa fa-question-circle param-help",
                   `data-toggle` = "tooltip", `data-placement` = "top",
                   title = "Benjamini-Hochberg adjusted p-value cutoff for multiple testing correction.<br><b>0.05</b> = standard (5% FDR), <b>0.01</b> = stringent. Lower values = fewer but more confident DEGs.")),
            0.05, step = 0.01)),
        column(3,
          numericInput("top_genes", tags$span("Heatmap Genes:",
            tags$i(class = "fa fa-question-circle param-help",
                   `data-toggle` = "tooltip", `data-placement` = "top",
                   title = "Number of top differentially expressed genes to display in the heatmap, ranked by adjusted p-value.<br><b>50</b> is a good default; use 20-30 for cleaner plots, 100+ for comprehensive views.")),
            50, step = 10)),
        column(3, br(), actionButton("run_de", "Run DE Analysis",
                                     icon = icon("rocket"), class = "btn-success btn-lg",
                                     style = "width:100%;"))
      )
    ),
    
    fluidRow(
      uiOutput("de_pipeline_verification")
    ),
    fluidRow(
      box(
        title = tags$span(icon("clipboard-check"), " How to check your results are valid"),
        width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        tags$p(tags$strong("Volcano & DE results (this step):"), style = "margin-bottom: 6px;"),
        tags$ul(
          style = "margin: 0 0 12px 0; padding-left: 20px;",
          tags$li("Confirm groups in ", tags$strong("Step 3 (Define Groups)"), " — Disease vs Normal (or your contrast) are correctly assigned. Wrong labels will flip or invalidate DEGs."),
          tags$li("Check the ", tags$strong("Pipeline verification"), " box above: it shows which DE method and batch correction were used. The volcano uses the actual statistics from that model."),
          tags$li("Sample size: ensure you have enough samples per group (e.g. ≥ 3 per group for DESeq2). Small n can give unstable estimates."),
          tags$li("Optional: compare top DEGs with published results for the same disease or GSE to spot-check.")
        ),
        tags$p(tags$strong("ML / prediction (Steps 10–12):"), style = "margin-bottom: 6px;"),
        tags$ul(
          style = "margin: 0; padding-left: 20px;",
          tags$li(tags$strong("Step 12 (ROC):"), " AUC ≥ 0.8 suggests good discrimination. Use ", tags$strong("Step 11 (Validation)"), " to load an external dataset — ROC and Nomogram then show training vs external AUC; similar values suggest the model generalizes."),
          tags$li(tags$strong("Step 12 (Nomogram):"), " Training vs validation AUC (70/30 split) — if validation AUC is much lower than training, the model may be overfitting. External validation (Step 11) is the strongest check.")
        )
      )
    ),
    fluidRow(
      box(title = tags$span(icon("mountain"), " Volcano Plot"),
          width = 8, status = "danger", solidHeader = TRUE,
          plotOutput("volcano_plot", height = "550px"),
          tags$div(style = "margin-top: 10px;",
            downloadButton("download_volcano_png", tagList(icon("download"), " PNG"), class = "btn-info btn-sm", style = "margin-right: 6px;"),
            downloadButton("download_volcano_pdf", tagList(icon("download"), " PDF"), class = "btn-info btn-sm"))),
      box(title = tags$span(icon("list-ol"), " Top DEGs"),
          width = 4, status = "info", solidHeader = TRUE,
          DTOutput("top_degs_table"))
    ),
    fluidRow(
      box(title = tags$span(icon("th"), " Heatmap - Top DE Genes"),
          width = 12, status = "success", solidHeader = TRUE,
          plotOutput("heatmap_plot", height = "600px"),
          tags$div(style = "margin-top: 10px;",
            downloadButton("download_heatmap_png", tagList(icon("download"), " PNG"), class = "btn-info btn-sm", style = "margin-right: 6px;"),
            downloadButton("download_heatmap_pdf", tagList(icon("download"), " PDF"), class = "btn-info btn-sm")))
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("file-csv"), " Download DE results"),
        width = 12, status = "info", solidHeader = TRUE,
        tags$p("Export the differential expression (volcano) table to check results in R, Excel, or other tools.", style = "margin-bottom: 12px; color: #555;"),
        downloadButton("download_de_results", tagList(icon("download"), " DE results / volcano table (CSV)"), class = "btn-primary btn-lg")
      )
    ),
    fluidRow(
      box(
        title = tags$span(icon("download"), " Download Results"), 
        width = 12, status = "warning", solidHeader = TRUE,
        column(4, downloadButton("download_de_results_alt", tagList(icon("download"), " DE Results"), class = "btn-success btn-block")),
        column(4, downloadButton("download_sig_genes", tagList(icon("download"), " Significant Genes"), class = "btn-success btn-block")),
        column(4, downloadButton("download_workspace", tagList(icon("download"), " Workspace"), class = "btn-success btn-block"))
      )
    ),
    
    fluidRow(
      box(
        width = 12, status = "info", solidHeader = FALSE,
        tags$div(
          style = "text-align: center; padding: 20px; background: linear-gradient(135deg, #e8f4f8 0%, #f0f4ff 100%); border-radius: 10px; border: 2px solid #3498db;",
          tags$p(
            tags$strong(icon("table"), " View Complete Results Table"),
            style = "margin: 0 0 10px 0; color: #2c3e50; font-size: 18px;"
          ),
          tags$p(
            "Click the button below to view all differential expression results in a searchable, sortable table",
            style = "margin: 0 0 20px 0; color: #6c757d; font-size: 14px;"
          ),
          actionButton("toggle_all_results",
                       tagList(icon("table"), " Show All Results Table"),
                       class = "btn-info btn-lg",
                       style = "font-size: 16px; padding: 12px 30px; border-radius: 25px; font-weight: bold;")
        )
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("table"), " All Results"), 
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        id = "all_results_box",
        tags$div(
          id = "all_results_table_container",
          DTOutput("all_de_table")
        )
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("info-circle"), " DE Analysis Summary"), 
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        tags$div(
          style = "padding: 15px 0;",
          fluidRow(
            column(4, infoBoxOutput("total_degs", width = 12)),
            column(4, infoBoxOutput("up_genes", width = 12)),
            column(4, infoBoxOutput("down_genes", width = 12))
          )
        )
      )
    ),

    fluidRow(
      box(
        title = tags$span(icon("file-alt"), " Process Summary"),
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        uiOutput("results_process_summary_ui"))
    ),
    fluidRow(
      box(
        width = 12, status = "primary", solidHeader = FALSE,
        tags$div(
          style = "text-align: center; padding: 20px;",
          actionButton("next_page_results",
                      tagList(icon("arrow-right"), " Next: WGCNA Analysis"),
                      class = "btn-primary btn-lg",
                      style = "font-size: 16px; padding: 12px 30px; border-radius: 25px; font-weight: bold;")
        )
      )
    ),
  )
