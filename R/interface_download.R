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
    conditionalPanel(
      condition = "input.analysis_type != 'parallel'",
      fluidRow(
        box(
          title = tags$span(icon("info-circle"), " About this step"),
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
          tags$p(tags$strong("Purpose:"), " Download expression data from NCBI GEO for RNA-seq and/or microarray. Data are mapped to gene symbols and merged to a common gene set for downstream analysis.", style = "margin-bottom: 8px;"),
          tags$p(tags$strong("Methods:"), " RNA-seq uses NCBI-provided raw counts with TMM normalization; microarray uses GEO Series Matrix with platform-specific annotation. Common genes (intersection across datasets) are retained.", style = "margin-bottom: 8px;"),
          tags$p(tags$strong("Storage:"), " Downloaded files are stored in ", tags$code("micro_data"), " (microarray/CEL) and ", tags$code("rna_data"), " (bulk RNA-seq). These folders are cleared at the start of each new run, so only the current run's data is kept-no accumulation across multiple uses.", style = "margin-bottom: 8px;"),
          tags$p(tags$strong("Output:"), " Combined expression matrix (genes \u00d7 samples), sample metadata, and gene overlap statistics for QC.", style = "margin-bottom: 0;")
        )
      )
    ),
    conditionalPanel(
      condition = "input.analysis_type == 'parallel'",
      fluidRow(
        box(
          title = tags$span(icon("info-circle"), " About this step"),
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
          tags$p(tags$strong("Purpose:"), " Download RNA-seq and microarray as two separate pipelines. Each platform keeps its own genes and samples through DE.", style = "margin-bottom: 8px;"),
          tags$p(tags$strong("Layout:"), " RNA-seq is on the left; microarray is on the right. There is no common-gene merge at download.", style = "margin-bottom: 8px;"),
          tags$p(tags$strong("Storage:"), " Files go to ", tags$code("rna_data"), " (left) and ", tags$code("micro_data"), " (right). Folders are cleared at the start of each new run.", style = "margin-bottom: 0;")
        )
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
                "Merged (Both)" = "merged",
                "Parallel DE, then merge" = "parallel"
              ),
              selected = "rnaseq", inline = TRUE
            )
          ),
          conditionalPanel(
            condition = "input.analysis_type == 'merged'",
            column(12,
              tags$div(
                class = "alert alert-warning",
                style = "margin-top: 10px; margin-bottom: 0; font-size: 13px; line-height: 1.6;",
                icon("exclamation-triangle"),
                tags$strong(" Merged (Both) \u2014 same as the previous app"),
                tags$ul(
                  style = "margin: 8px 0 0 0; padding-left: 20px;",
                  tags$li("Normalize, QC, groups, then ", tags$strong("batch / merge first"), "."),
                  tags$li("Each platform box accepts ", tags$strong("one or more GSE IDs"), " (comma-separated)."),
                  tags$li("Step 6 is ", tags$strong("one limma DE"), " on the merged matrix. Use this path if you want the original merged workflow."),
                  tags$li("For separate RNA-seq + microarray DE, then common genes, choose ", tags$strong("Parallel DE, then merge"), ".")
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.analysis_type == 'parallel'",
            column(12,
              tags$div(
                class = "alert alert-info",
                style = "margin-top: 10px; margin-bottom: 0; font-size: 13px; line-height: 1.6;",
                icon("object-ungroup"),
                tags$strong(" Parallel DE, then merge"),
                tags$ul(
                  style = "margin: 8px 0 0 0; padding-left: 20px;",
                  tags$li("Normalize, QC, batch, and DE each platform on its own (no merge, no global quantile, no shared gene intersection)."),
                  tags$li("Each box accepts ", tags$strong("one or more GSE IDs"), " (comma-separated). RNA-seq can have several studies and microarray can have several studies in the same run."),
                  tags$li("After groups, Step 5 batch-corrects ", tags$strong("RNA-seq and microarray separately"), " (ComBat-ref if a platform has 2+ GSEs; one GSE is filtered only)."),
                  tags$li("Step 6 runs both DEs on those separate matrices (RNA-seq = your method; microarray = limma). Step 7 is RNA-seq \u2229 microarray (common same-direction DEGs). Step 8 is one WGCNA on one processed platform matrix, not on that DEG list."),
                  tags$li("Merged (Both) is unchanged: one shared normalize, one joint batch, one limma DE.")
                )
              )
            )
          ),
          column(
            6,
            conditionalPanel(
              condition = "input.analysis_type == 'rnaseq' || input.analysis_type == 'microarray'",
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
            conditionalPanel(
              condition = "input.analysis_type == 'merged' || input.analysis_type == 'parallel'",
              tags$div(
                style = "padding: 8px 0 0 0; font-size: 13px; color: #334155; line-height: 1.5;",
                tags$strong("Multiple GSEs allowed."),
                " Type several IDs in the RNA-seq box and/or the microarray box (comma-separated). All of them are downloaded. Batch correction runs when a platform has 2 or more GSEs."
              )
            )
          ),
          column(
            12,
            style = "margin-bottom: 6px;",
            tags$label("DE Method for Step 6:",
              style = "font-weight: 700; font-size: 14px; color: #2c3e50; display: block; margin-bottom: 4px;"
            ),
            conditionalPanel(
              condition = "input.analysis_type == 'parallel'",
              tagList(
                tags$p(
                  style = "margin: 0 0 10px 0; font-size: 13px; color: #334155;",
                  "Choose one method per platform. Both DEs run together when you click Run DE in Step 6."
                ),
                fluidRow(
                  column(
                    6,
                    tags$div(
                      style = "padding: 12px 14px; border: 1px solid #93c5fd; border-radius: 8px; background: #eff6ff; min-height: 170px;",
                      tags$p(tags$strong("RNA-seq DE"), style = "margin: 0 0 8px 0; color: #1e3a5f;"),
                      radioButtons(
                        "de_method_rna",
                        label = NULL,
                        choices = c(
                          "DESeq2 \u2014 negative binomial (recommended)" = "deseq2",
                          "edgeR \u2014 quasi-likelihood" = "edger",
                          "limma-voom \u2014 voom + limma" = "limma_voom",
                          "limma \u2014 on TMM log-CPM" = "limma"
                        ),
                        selected = "deseq2"
                      )
                    )
                  ),
                  column(
                    6,
                    tags$div(
                      style = "padding: 12px 14px; border: 1px solid #cbd5e1; border-radius: 8px; background: #f8fafc; min-height: 170px;",
                      tags$p(tags$strong("Microarray DE"), style = "margin: 0 0 8px 0; color: #1e3a5f;"),
                      radioButtons(
                        "de_method_micro",
                        label = NULL,
                        choices = c("limma \u2014 empirical Bayes (fixed for microarray)" = "limma"),
                        selected = "limma"
                      ),
                      tags$p(
                        style = "margin: 8px 0 0 0; font-size: 12px; color: #64748b;",
                        "Always limma. DESeq2 / edgeR are not used on arrays."
                      )
                    )
                  )
                )
              )
            ),
            conditionalPanel(
              condition = "input.analysis_type != 'parallel'",
              tags$div(
                id = "de_method_wrapper",
                radioButtons("de_method", label = NULL,
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
            tags$div(
              style = paste0(
                "margin-top: 10px; padding: 14px 16px;",
                "background: #f0f6ff; border-left: 4px solid #3b82f6;",
                "border-radius: 6px; font-size: 13px; line-height: 1.6; color: #1e3a5f;"
              ),
              tags$strong(icon("info-circle"), " Which DE method should I choose?"),
              tags$table(
                style = "margin-top: 8px; width: 100%; border-collapse: collapse;",
                tags$thead(
                  tags$tr(
                    tags$th("Method",   style = "text-align:left; padding: 4px 8px; background:#dbeafe; border-radius:4px 0 0 0;"),
                    tags$th("Use for",  style = "text-align:left; padding: 4px 8px; background:#dbeafe;"),
                    tags$th("Requires", style = "text-align:left; padding: 4px 8px; background:#dbeafe; border-radius:0 4px 0 0;")
                  )
                ),
                tags$tbody(
                  tags$tr(style = "background:#fff;",
                    tags$td(tags$strong("limma"), style = "padding:5px 8px; border-bottom:1px solid #e2e8f0;"),
                    tags$td("Microarray, merged, or any normalized data", style = "padding:5px 8px; border-bottom:1px solid #e2e8f0;"),
                    tags$td("Log-expression matrix (normalized)", style = "padding:5px 8px; border-bottom:1px solid #e2e8f0;")
                  ),
                  tags$tr(style = "background:#f8faff;",
                    tags$td(tags$strong("limma-voom"), style = "padding:5px 8px; border-bottom:1px solid #e2e8f0;"),
                    tags$td("RNA-seq counts with limma-style models", style = "padding:5px 8px; border-bottom:1px solid #e2e8f0;"),
                    tags$td("Raw integer counts", style = "padding:5px 8px; border-bottom:1px solid #e2e8f0;")
                  ),
                  tags$tr(style = "background:#fff;",
                    tags$td(tags$strong("DESeq2"), style = "padding:5px 8px; border-bottom:1px solid #e2e8f0;"),
                    tags$td("RNA-seq \u2014 gold standard negative binomial model", style = "padding:5px 8px; border-bottom:1px solid #e2e8f0;"),
                    tags$td("Raw integer counts", style = "padding:5px 8px; border-bottom:1px solid #e2e8f0;")
                  ),
                  tags$tr(style = "background:#f8faff;",
                    tags$td(tags$strong("edgeR"), style = "padding:5px 8px;"),
                    tags$td("RNA-seq \u2014 robust quasi-likelihood tests", style = "padding:5px 8px;"),
                    tags$td("Raw integer counts", style = "padding:5px 8px;")
                  )
                )
              ),
              tags$p(
                icon("lightbulb"),
                tags$strong(" Tip:"),
                " Microarray only (e.g. GSE268456) \u2192 select ",
                tags$strong("limma"),
                ". RNA-seq counts \u2192 select DESeq2, edgeR, or limma-voom.",
                style = "margin: 8px 0 0 0; font-size: 12.5px; color: #374151;"
              )
            )
          )
        ),
        conditionalPanel(
          condition = "(input.de_method == 'deseq2' || input.de_method == 'edger' || input.de_method == 'limma_voom') && input.analysis_type != 'rnaseq' && input.analysis_type != 'parallel'",
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
        condition = "input.analysis_type == 'rnaseq' || input.analysis_type == 'merged' || input.analysis_type == 'parallel'",
        box(
          title = tags$span(icon("dna"), " RNA-seq Datasets"),
          width = 6, status = "info", solidHeader = TRUE,
          textAreaInput("rnaseq_gses", "GSE IDs (comma separated; one or more):",
            value = "", placeholder = "e.g. GSE144119",
            rows = 3
          ),
          tags$p(
            style = "margin-top: 4px; margin-bottom: 0; color: #868e96; font-size: 12px;",
            icon("lightbulb"), " One or more RNA-seq GEO Series IDs, comma-separated (e.g. GSE144119)."
          )
        )
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'microarray' || input.analysis_type == 'merged' || input.analysis_type == 'parallel'",
        box(
          title = tags$span(icon("microchip"), " Microarray Datasets"),
          width = 6, status = "warning", solidHeader = TRUE,
          textAreaInput("microarray_gses", "GSE IDs (comma separated; one or more):",
            value = "", placeholder = "e.g. GSE268456",
            rows = 3
          ),
          tags$p(
            style = "margin-top: 4px; margin-bottom: 0; color: #868e96; font-size: 12px;",
            icon("lightbulb"), " One or more microarray GEO Series IDs, comma-separated (e.g. GSE268456)."
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
    gexp_ui_parallel_run_logs("download_log_micro", "download_log_rna"),
    conditionalPanel(
      condition = "input.analysis_type != 'parallel'",
      fluidRow(
        box(
          title = tags$span(icon("terminal"), " Download log"),
          width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
          tags$div(
            style = "margin-bottom: 10px;",
            tags$span(class = "step-timer", tags$span(class = "label", "Elapsed:"), textOutput("download_timer", inline = TRUE))
          ),
          gexp_ui_log_box("download_log")
        )
      )
    ),
    fluidRow(
      column(12,
        class = "next-btn",
        actionButton("next_page_download", tagList(icon("arrow-right"), " Next: Normalize Data"),
          class = "btn btn-success btn-lg",
          style = "margin-top: 20px;"
        )
      )
    )
  )
}
