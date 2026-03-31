# NOTE: migrated from inst/shinyapp/ui/ui_download.R to satisfy Bioconductor
# Shiny app code-organization guidance (UI code under R/).

gexp_ui_download <- function() {
  # Local bindings so R CMD check can resolve symbols without importing
  # a large number of Shiny UI helpers into the package namespace.
  h2 <- shiny::h2
  icon <- shiny::icon
  tags <- shiny::tags
  HTML <- shiny::HTML
  fluidRow <- shiny::fluidRow
  column <- shiny::column
  actionButton <- shiny::actionButton
  tagList <- shiny::tagList
  fileInput <- shiny::fileInput
  radioButtons <- shiny::radioButtons
  conditionalPanel <- shiny::conditionalPanel
  textAreaInput <- shiny::textAreaInput
  textInput <- shiny::textInput
  uiOutput <- shiny::uiOutput
  textOutput <- shiny::textOutput
  box <- shinydashboard::box

  shinydashboard::tabItem(
    tabName = "download",
    h2(icon("download"), " Step 1: Select Platform & Download Data"),
    tags$style(HTML(
      "#rnaseq_gses, #microarray_gses { background-color: #f8f9fa; color: #212529; }"
    )),
    fluidRow(
      box(
        title = tags$span(icon("heartbeat"), " Analysis Context (optional)"),
        width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        tags$p("Specify the disease or condition you are analyzing. This helps keep your analysis organized and can be used in reports and filenames.", style = "margin-bottom: 12px;"),
        textInput("disease_name", "Disease / Condition (optional):",
          placeholder = "e.g. Myocardial infarction, Breast cancer, COVID-19",
          width = "100%"
        )
      )
    ),
    fluidRow(
      box(
        title = tags$span(icon("info-circle"), " About this step"),
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        tags$p(tags$strong("Purpose:"), " Download expression data from NCBI GEO for RNA-seq and/or microarray. Data are mapped to gene symbols and merged to a common gene set for downstream analysis.", style = "margin-bottom: 8px;"),
        tags$p(tags$strong("Methods:"), " RNA-seq uses NCBI-provided raw counts with TMM normalization; microarray uses GEO Series Matrix with platform-specific annotation. Common genes (intersection across datasets) are retained.", style = "margin-bottom: 8px;"),
        tags$p(tags$strong("Storage:"), " Downloaded files are stored in ", tags$code("micro_data"), " (microarray/CEL) and ", tags$code("rna_data"), " (bulk RNA-seq). These folders are cleared at the start of each new run, so only the current run's data is kept-no accumulation across multiple uses.", style = "margin-bottom: 8px;"),
        tags$p(tags$strong("Output:"), " Combined expression matrix (genes \u00d7 samples), sample metadata, and gene overlap statistics for QC.", style = "margin-bottom: 0;")
      )
    ),
    fluidRow(
      box(
        title = tags$span(icon("folder-open"), " Optional: Resume from saved data"),
        width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        tags$p("Upload a saved workspace (.rds) to continue where you left off, or skip and start a new analysis below.", style = "margin-bottom: 10px;"),
        fluidRow(
          column(12, tags$div(
            style = "margin-bottom: 12px;",
            actionButton("skip_load_btn", tagList(icon("arrow-down"), " Continue without loading \u2014 start new analysis below"), class = "btn-success btn-sm")
          ))
        ),
        fluidRow(
          column(
            6, tags$p(tags$strong("Upload a .rds file:"), style = "margin-bottom: 4px;"),
            fileInput("upload_workspace_file", NULL, accept = c(".rds"), buttonLabel = "Choose file", placeholder = "No file chosen")
          ),
          column(4, tags$div(
            style = "margin-top: 25px;",
            actionButton("load_uploaded_btn", tagList(icon("upload"), " Load uploaded file"), class = "btn-info btn-block")
          ))
        )
      )
    ),
    fluidRow(
      box(
        title = tags$span(icon("laptop-code"), " Select Analysis Platform"),
        width = 12, status = "primary", solidHeader = TRUE,
        fluidRow(
          column(
            6,
            radioButtons("analysis_type", "Choose Platform:",
              choices = c(
                "RNA-seq" = "rnaseq",
                "Microarray" = "microarray",
                "Merged (Both)" = "merged"
              ),
              selected = "rnaseq", inline = TRUE
            )
          ),
          column(
            6,
            radioButtons(
              "dataset_mode",
              "Datasets:",
              choices = c(
                "Single dataset (1 GSE) \u2014 skip batch correction" = "single",
                "Multiple datasets (comma-separated) \u2014 batch correction recommended" = "multi"
              ),
              selected = "multi",
              inline = TRUE
            )
          ),
          column(
            12,
            radioButtons("de_method",
              tags$span(
                "DE Method:",
                tags$i(
                  class = "fa fa-question-circle param-help",
                  `data-toggle` = "tooltip", `data-placement` = "right",
                  title = "Choose the statistical method for differential expression analysis in Step 6.<br><br><b>limma (empirical Bayes):</b> Gold standard for microarray and works well with any normalized data. Uses moderated t-statistics on batch-corrected log-expression.<br><br><b>limma-voom (mean\u2013variance weights):</b> Recommended when you want limma-style models on RNA-seq <i>counts</i>. voom converts counts to logCPM and estimates precision weights before limma.<br><br><b>DESeq2 (negative binomial):</b> Gold standard for RNA-seq count data. Uses its own internal normalization (median-of-ratios) \u2014 raw counts are preserved and passed directly to DESeq2.<br><br><b>edgeR (quasi-likelihood):</b> Robust alternative for RNA-seq count data. Uses TMM normalization and quasi-likelihood F-tests for DE \u2014 well-suited for small sample sizes.<br><br><em>Tip:</em> For pure RNA-seq counts, DESeq2, edgeR, or limma-voom are appropriate. For microarray or merged platforms, limma is recommended."
                )
              ),
              choices = c(
                "limma \u2014 empirical Bayes (recommended for microarray/mixed)" = "limma",
                "limma-voom \u2014 voom + limma (RNA-seq counts)" = "limma_voom",
                "DESeq2 \u2014 negative binomial (RNA-seq counts)" = "deseq2",
                "edgeR \u2014 quasi-likelihood (RNA-seq counts)" = "edger"
              ),
              selected = "limma"
            )
          )
        ),
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
        box(
          title = tags$span(icon("dna"), " RNA-seq Datasets"),
          width = 6, status = "info", solidHeader = TRUE,
          textAreaInput("rnaseq_gses", "GSE IDs (comma separated):",
            value = "", placeholder = "e.g. GSE50760, GSE104836",
            rows = 3
          ),
          tags$p(
            style = "margin-top: 4px; margin-bottom: 0; color: #868e96; font-size: 12px;",
            icon("lightbulb"), " Enter one or more GEO Series IDs (e.g. GSE123456), comma-separated."
          )
        )
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'microarray' || input.analysis_type == 'merged'",
        box(
          title = tags$span(icon("microchip"), " Microarray Datasets"),
          width = 6, status = "warning", solidHeader = TRUE,
          textAreaInput("microarray_gses", "GSE IDs (comma separated):",
            value = "", placeholder = "e.g. GSE89076, GSE44076",
            rows = 3
          ),
          tags$p(
            style = "margin-top: 4px; margin-bottom: 0; color: #868e96; font-size: 12px;",
            icon("lightbulb"), " Enter one or more GEO Series IDs (e.g. GSE123456), comma-separated."
          )
        )
      )
    ),
    fluidRow(
      column(12,
        align = "center",
        actionButton("start_processing", tagList(icon("play-circle"), " Start Processing"),
          class = "btn btn-primary btn-lg",
          style = "margin-top: 20px; margin-bottom: 10px; width: 250px;"
        )
      )
    ),
    fluidRow(
      column(
        12,
        tags$div(
          style = "text-align: center; color: #868e96; font-size: 12px;",
          icon("info-circle"),
          " Save your workspace (sidebar) to resume your analysis later."
        )
      )
    ),
    fluidRow(
      box(
        title = tags$span(icon("file-alt"), " Process Summary"),
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        uiOutput("download_process_summary_ui")
      )
    ),
    fluidRow(
      box(
        title = tags$span(icon("terminal"), " Download log"),
        width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$div(
          style = "display:flex; align-items:center; justify-content: space-between; margin-bottom: 10px;",
          tags$div(tags$span(class = "step-timer", tags$span(class = "label", "Elapsed:"), textOutput("download_timer", inline = TRUE))),
          actionButton("toggle_download_summary", tagList(icon("plus"), " Toggle summary"), class = "btn btn-sm btn-default")
        ),
        tags$pre(style = "white-space: pre-wrap;", textOutput("download_log"))
      )
    ),
    fluidRow(
      column(12,
        class = "next-btn",
        actionButton("next_page_download", tagList(icon("arrow-right"), " Next: QC & Visualization"),
          class = "btn btn-success btn-lg",
          style = "margin-top: 20px;"
        )
      )
    )
  )
}
