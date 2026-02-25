# ==============================================================================
# UI_DOWNLOAD.R - Step 1: Download Data Tab
# ==============================================================================

ui_download <- tabItem(
    tabName = "download",
    h2(icon("download"), " Step 1: Select Platform & Download Data"),

    # ----- Disease-specific analysis: quick guide -----
    fluidRow(
      tags$div(
        class = "disease-guide-box",
        style = "padding: 20px 24px; margin-bottom: 22px; border-radius: 12px; border: 2px solid #667eea; background: linear-gradient(135deg, #e8f0ff 0%, #f0e6ff 50%, #ffe8f0 100%); box-shadow: 0 6px 20px rgba(102,126,234,0.2);",
        tags$h4(icon("heartbeat"), " Finding disease-specific results — quick guide", style = "color: #2c3e50; margin-bottom: 12px; font-weight: bold;"),
        tags$p("You can ", tags$strong("choose any threshold"), " that fits your study. Key places:", style = "margin-bottom: 10px; color: #34495e;"),
        tags$ul(
          style = "margin: 0 0 10px 20px; color: #2c3e50; line-height: 1.8;",
          tags$li(tags$strong("Step 1 (here):"), " Optionally enter your ", tags$strong("Disease / Condition"), " name (e.g. Breast cancer). It is used in reports and for Normal vs Disease grouping."),
          tags$li(tags$strong("Step 4: Select Groups"), " — Assign samples to ", tags$em("Normal"), " and ", tags$em("Disease"), " so DE and WGCNA compare the right groups."),
          tags$li(tags$strong("Step 6: DE Parameters"), " — Set ", tags$strong("LogFC cutoff"), " and ", tags$strong("Adj. P-value"), " (e.g. 0.5 and 0.05). Genes passing both are your disease-specific DEGs."),
          tags$li(tags$strong("Step 7: WGCNA"), " — Use ", tags$strong("GS P-value"), ", ", tags$strong("MM correlation"), ", and ", tags$strong("significant module"), " thresholds to define disease-related modules and genes.")
        ),
        tags$p(tags$small(icon("lightbulb"), " Tip: Hover over ", tags$span(icon("question-circle"), style = "color: #667eea;"), " next to parameters for detailed help. All thresholds are flexible — pick what fits your question."), style = "margin: 0; color: #555; font-size: 13px;")
      )
    ),
    fluidRow(
      box(
        title = tags$span(icon("heartbeat"), " Analysis Context"),
        width = 12, status = "success", solidHeader = TRUE,
        tags$p("Specify the disease or condition you are analyzing. This helps keep your analysis organized and can be used in reports and filenames.", style = "margin-bottom: 12px;"),
        textInput("disease_name", "Disease / Condition (optional):",
                  placeholder = "e.g. Myocardial infarction, Breast cancer, COVID-19",
                  width = "100%")
      )
    ),
    fluidRow(
      box(
        title = tags$span(icon("info-circle"), " About this step"),
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p(tags$strong("Purpose:"), " Download expression data from NCBI GEO for RNA-seq and/or microarray. Data are mapped to gene symbols and merged to a common gene set for downstream analysis.", style = "margin-bottom: 8px;"),
        tags$p(tags$strong("Methods:"), " RNA-seq uses NCBI-provided raw counts with TMM normalization; microarray uses GEO Series Matrix with platform-specific annotation. Common genes (intersection across datasets) are retained.", style = "margin-bottom: 8px;"),
        tags$p(tags$strong("Output:"), " Combined expression matrix (genes × samples), sample metadata, and gene overlap statistics for QC.", style = "margin-bottom: 0;")
      )
    ),
    fluidRow(
      box(
        title = tags$span(icon("folder-open"), " Optional: Resume from saved data"),
        width = 12, status = "warning", solidHeader = TRUE,
        tags$p("Load a previously saved workspace (from the app folder) to continue where you left off, or skip and start a new analysis below.", style = "margin-bottom: 10px;"),
        fluidRow(
          column(12, tags$div(style = "margin-bottom: 12px;",
            actionButton("skip_load_btn", tagList(icon("arrow-down"), " Continue without loading — start new analysis below"), class = "btn-success btn-sm")))
        ),
        fluidRow(
          column(6, tags$p(tags$strong("Load from folder:"), style = "margin-bottom: 4px;"), uiOutput("load_from_folder_ui")),
          column(4, tags$div(style = "margin-top: 25px;",
                   actionButton("load_from_folder_btn", tagList(icon("folder-open"), " Load from folder"), class = "btn-info btn-block")))
        ),
        fluidRow(
          column(6, tags$p(tags$strong("Or upload a .rds file:"), style = "margin-bottom: 4px;"),
            fileInput("upload_workspace_file", NULL, accept = c(".rds"), buttonLabel = "Choose file", placeholder = "No file chosen")),
          column(4, tags$div(style = "margin-top: 25px;",
                   actionButton("load_uploaded_btn", tagList(icon("upload"), " Load uploaded file"), class = "btn-info btn-block")))
        )
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("laptop-code"), " Select Analysis Platform"), 
        width = 12, status = "primary", solidHeader = TRUE,
        fluidRow(
          column(6,
            radioButtons("analysis_type", "Choose Platform:",
                         choices = c("RNA-seq" = "rnaseq", 
                                     "Microarray" = "microarray", 
                                     "Merged (Both)" = "merged"),
                         selected = "rnaseq", inline = TRUE)
          ),
          column(6,
            radioButtons("de_method",
              tags$span("DE Method:",
              tags$i(class = "fa fa-question-circle param-help",
                     `data-toggle` = "tooltip", `data-placement` = "right",
                     title = "Choose the statistical method for differential expression analysis in Step 6.<br><br><b>limma (empirical Bayes):</b> Gold standard for microarray and works well with any normalized data. Uses moderated t-statistics on batch-corrected log-expression.<br><br><b>limma-voom (mean–variance weights):</b> Recommended when you want limma-style models on RNA-seq <i>counts</i>. voom converts counts to logCPM and estimates precision weights before limma.<br><br><b>DESeq2 (negative binomial):</b> Gold standard for RNA-seq count data. Uses its own internal normalization (median-of-ratios) — raw counts are preserved and passed directly to DESeq2.<br><br><b>edgeR (quasi-likelihood):</b> Robust alternative for RNA-seq count data. Uses TMM normalization and quasi-likelihood F-tests for DE — well-suited for small sample sizes.<br><br><em>Tip:</em> For pure RNA-seq counts, DESeq2, edgeR, or limma-voom are appropriate. For microarray or merged platforms, limma is recommended.")),
              choices = c(
                "limma — empirical Bayes (recommended for microarray/mixed)" = "limma",
                "limma-voom — voom + limma (RNA-seq counts)" = "limma_voom",
                "DESeq2 — negative binomial (RNA-seq counts)" = "deseq2",
                "edgeR — quasi-likelihood (RNA-seq counts)" = "edger"
              ),
              selected = "limma")
          )
        ),
        # Dynamic warning when DESeq2 or edgeR is selected for non-RNA-seq data
        conditionalPanel(
          condition = "(input.de_method == 'deseq2' || input.de_method == 'edger' || input.de_method == 'limma_voom') && input.analysis_type != 'rnaseq'",
          tags$div(
            class = "alert alert-warning", style = "margin-top: 10px; margin-bottom: 0;",
            icon("exclamation-triangle"),
            tags$strong(" Warning:"),
            " DESeq2, edgeR, and limma-voom are designed for RNA-seq count data. For microarray or merged platforms, ",
            tags$strong("limma is strongly recommended"),
            " to avoid unreliable results. These methods all require integer counts."
          )
        )
      )
    ),
    
    fluidRow(
      conditionalPanel(
        condition = "input.analysis_type == 'rnaseq' || input.analysis_type == 'merged'",
        box(title = tags$span(icon("dna"), " RNA-seq Datasets"), 
            width = 6, status = "info", solidHeader = TRUE,
            textAreaInput("rnaseq_gses", "GSE IDs (comma separated):",
                          value = "GSE50760, GSE104836", rows = 3))
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'microarray' || input.analysis_type == 'merged'",
        box(title = tags$span(icon("microchip"), " Microarray Datasets"), 
            width = 6, status = "warning", solidHeader = TRUE,
            textAreaInput("microarray_gses", "GSE IDs (comma separated):",
                          value = "", rows = 3))
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        fluidRow(
          column(12,
            actionButton("start_processing", "Start Processing", 
                         icon = icon("play-circle"), class = "btn-primary btn-lg",
                         style = "font-size: 18px; padding: 15px 40px;")
          )
        ),
        tags$p(
          icon("info-circle"),
          " To clear all data and start over, use the ",
          tags$strong("Reset App"),
          " button in the header above.",
          style = "margin-top: 12px; margin-bottom: 0; color: #6c757d; font-size: 13px;"
        )
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = tags$span(icon("file-alt"), " Processing Summary"), 
        status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        tags$div(
          id = "download_summary_panel",
          verbatimTextOutput("download_log"),
          tags$div(
            class = "step-timer",
            style = "margin-top: 15px;",
            tags$span(class = "label", "Elapsed:"),
            textOutput("download_timer", inline = TRUE)
          )
        )
      )
    ),
    fluidRow(
      box(width = 12, status = "info", solidHeader = FALSE,
          tags$div(class = "next-btn", style = "text-align: center; padding: 20px 0;",
                   actionButton("next_page_download", "Next: QC & Visualization",
                                icon = icon("arrow-right"), class = "btn-success btn-lg",
                                style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;")))
    )
  )
