# ==============================================================================
# UI_RESULTS_SUMMARY.R - Step 15: Results Summary (text-only overview, no plots)
# ==============================================================================

# Reusable step connector (arrow down)
step_arrow <- function() {
  tags$div(
    style = "text-align: center; padding: 8px 0; color: #95a5a6;",
    tags$span(icon("chevron-down"), style = "font-size: 20px;")
  )
}

# Reusable step card wrapper: title (icon + text), description, then content
step_card <- function(step_num, icon_name, title, description, status = "primary", ...) {
  tagList(
    step_arrow(),
    box(
      width = 12,
      status = status,
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = FALSE,
      title = tags$span(
        icon(icon_name),
        " ",
        tags$span(style = "color: #2c3e50; font-weight: 600;", paste0("Step ", step_num, ": ", title))
      ),
      tags$p(description, style = "margin-bottom: 14px; font-size: 13px; color: #5a6c7d; line-height: 1.5;"),
      ...
    )
  )
}

ui_results_summary <- tabItem(
  tabName = "results_summary",

  # ----- Page title -----
  tags$div(
    style = "margin-bottom: 24px; padding-bottom: 16px; border-bottom: 2px solid #ecf0f1;",
    tags$h2(
      icon("file-alt"),
      " Step 16: Results Summary",
      style = "color: #2c3e50; font-weight: 700; margin: 0; font-size: 28px;"
    ),
    uiOutput("results_summary_about_ui"),
    tags$p(
      "Pipeline overview and key results in order (text and tables only - see each step's own tab for its plots).",
      style = "margin-top: 8px; margin-bottom: 0; color: #7f8c8d; font-size: 14px;"
    )
  ),

  # ----- 1. Narrative summary (one paragraph) -----
  box(
    width = 12,
    status = "info",
    solidHeader = TRUE,
    title = tags$span(icon("align-left"), " Pipeline summary"),
    tags$div(
      style = "padding: 16px 0 8px 0; font-size: 15px; line-height: 1.75; color: #2c3e50; text-align: justify; background: linear-gradient(135deg, #f8f9fa 0%, #fff 100%); border-radius: 8px; padding: 20px !important;",
      uiOutput("results_summary_narrative")
    )
  ),

  # ----- 2. Normalization & batch -----
  step_arrow(),
  fluidRow(
    column(6,
      box(
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        title = tags$span(icon("balance-scale"), " Step 2: Normalization"),
        tags$p("Expression data normalized (e.g. log2, TMM, quantile). Gene counts and filtering applied.", style = "margin-bottom: 12px; font-size: 13px; color: #5a6c7d;"),
        uiOutput("results_summary_norm_batch")
      )
    ),
    column(6,
      box(
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        title = tags$span(icon("layer-group"), " Step 5: Batch correction"),
        tags$p("Batch effect removed (e.g. ComBat, limma).", style = "margin-bottom: 12px; font-size: 13px; color: #5a6c7d;"),
        uiOutput("results_summary_batch_only")
      )
    )
  ),

  # ----- 3. Differential expression -----
  step_card(
    "6", "chart-line", "Differential expression",
    "DEGs identified (e.g. limma).",
    "success",
    uiOutput("results_summary_de")
  ),

  # ----- 4. WGCNA -----
  step_card(
    "7", "project-diagram", "WGCNA co-expression",
    "Soft-threshold choice, modules, and module-trait correlation.",
    "primary",
    uiOutput("results_summary_wgcna")
  ),

  # ----- 5. Common genes & GO/KEGG -----
  step_card(
    "8", "venus-double", "Common genes (DEG n WGCNA)",
    "Intersection of DEGs and WGCNA module genes. This set is used for GO/KEGG enrichment and PPI.",
    "success",
    uiOutput("results_summary_common_genes")
  ),

  step_card(
    "8", "sitemap", "GO & KEGG enrichment",
    "Pathway enrichment of common genes.",
    "info",
    uiOutput("results_summary_go_kegg")
  ),

  # ----- 6. PPI -----
  step_card(
    "9", "project-diagram", "PPI network",
    "Protein-protein interaction network from common genes (STRINGdb). Hub genes by degree.",
    "info",
    uiOutput("results_summary_ppi")
  ),

  # ----- 7. Machine learning -----
  step_card(
    "10", "circle", "Machine learning",
    "Overlap of gene lists across selected ML methods. Common genes used for ROC and validation.",
    "warning",
    uiOutput("results_summary_ml")
  ),

  # ----- 8. Nomogram -----
  step_card(
    "13", "calculator", "Diagnostic nomogram",
    "Nomogram model and 70/30 validation. Training and validation AUC.",
    "danger",
    uiOutput("results_summary_nomogram_ui")
  ),

  # ----- 9. GSEA -----
  step_card(
    "14", "chart-area", "GSEA",
    "Gene Set Enrichment Analysis for target genes.",
    "info",
    uiOutput("results_summary_gsea")
  ),

  # ----- 10. Input & pipeline info -----
  step_card(
    "-", "dna", "Input & pipeline",
    "Common genes and expression matrix size after preprocessing.",
    "primary",
    uiOutput("results_summary_input_genes")
  )
)
