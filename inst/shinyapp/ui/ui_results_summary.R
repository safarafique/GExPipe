# ==============================================================================
# UI_RESULTS_SUMMARY.R - Step 16: Results Summary (aesthetic flow: summary → steps with arrow, description, figure)
# ==============================================================================

RESULTS_PLOT_HEIGHT <- "280px"

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
      " Results Summary",
      style = "color: #2c3e50; font-weight: 700; margin: 0; font-size: 28px;"
    ),
    tags$p(
      "Pipeline overview and key results in order. One summary below, then each step with a short description and figure.",
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
        title = tags$span(icon("balance-scale"), " Step 3: Normalization"),
        tags$p("Expression data normalized (e.g. log2, TMM, quantile). Gene counts and filtering applied.", style = "margin-bottom: 12px; font-size: 13px; color: #5a6c7d;"),
        uiOutput("results_summary_norm_batch")
      )
    ),
    column(6,
      box(
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        title = tags$span(icon("layer-group"), " Step 5: Batch correction"),
        tags$p("Batch effect removed (e.g. ComBat, limma). Before/after comparison below.", style = "margin-bottom: 12px; font-size: 13px; color: #5a6c7d;"),
        uiOutput("results_summary_batch_only")
      )
    )
  ),

  # ----- 3. Batch before vs after PCA -----
  step_card(
    "3–5", "chart-area", "Batch effect: Before vs After",
    "PCA of expression before and after batch correction. Samples should mix better after correction.",
    "primary",
    plotOutput("results_summary_batch_before_after", height = RESULTS_PLOT_HEIGHT)
  ),

  # ----- 4. Differential expression -----
  step_card(
    "6", "chart-line", "Differential expression",
    "DEGs identified (e.g. limma). Volcano: log2 FC vs adjusted P-value. Heatmap: top genes across samples.",
    "success",
    uiOutput("results_summary_de"),
    tags$div(style = "margin-top: 12px;",
      fluidRow(
        column(6, plotOutput("results_summary_volcano", height = RESULTS_PLOT_HEIGHT)),
        column(6, plotOutput("results_summary_de_heatmap", height = RESULTS_PLOT_HEIGHT))
      )
    )
  ),

  # ----- 5. WGCNA -----
  step_card(
    "7", "project-diagram", "WGCNA co-expression",
    "Soft-threshold choice, sample tree, gene dendrogram with module colors, and module–trait correlation.",
    "primary",
    uiOutput("results_summary_wgcna"),
    fluidRow(
      column(6, plotOutput("results_summary_soft_threshold", height = RESULTS_PLOT_HEIGHT)),
      column(6, plotOutput("results_summary_sample_tree", height = RESULTS_PLOT_HEIGHT))
    ),
    fluidRow(
      column(6, plotOutput("results_summary_wgcna_dendro", height = RESULTS_PLOT_HEIGHT)),
      column(6, plotOutput("results_summary_module_trait", height = RESULTS_PLOT_HEIGHT))
    )
  ),

  # ----- 6. Common genes & GO/KEGG -----
  step_card(
    "8", "venus-double", "Common genes (DEG ∩ WGCNA)",
    "Intersection of DEGs and WGCNA module genes. This set is used for GO/KEGG enrichment and PPI.",
    "success",
    uiOutput("results_summary_common_genes")
  ),

  step_card(
    "8", "sitemap", "GO & KEGG enrichment",
    "Pathway enrichment of common genes. GO dotplot and KEGG bar plot.",
    "info",
    uiOutput("results_summary_go_kegg"),
    fluidRow(
      column(6, plotOutput("results_summary_go_plot", height = RESULTS_PLOT_HEIGHT)),
      column(6, plotOutput("results_summary_kegg_plot", height = RESULTS_PLOT_HEIGHT))
    )
  ),

  # ----- 7. PPI -----
  step_card(
    "9", "project-diagram", "PPI network",
    "Protein–protein interaction network from common genes (STRINGdb). Hub genes by degree.",
    "info",
    uiOutput("results_summary_ppi"),
    plotOutput("results_summary_ppi_plot", height = RESULTS_PLOT_HEIGHT)
  ),

  # ----- 8. Machine learning & ROC -----
  step_card(
    "10", "circle", "Machine learning – Venn/UpSet",
    "Overlap of gene lists across selected ML methods. Common genes used for ROC and validation.",
    "warning",
    uiOutput("results_summary_ml"),
    plotOutput("results_summary_ml_venn", height = RESULTS_PLOT_HEIGHT)
  ),

  step_card(
    "12", "chart-line", "ROC curve",
    "ROC/AUC for mean signature of ML common genes (training data).",
    "success",
    plotOutput("results_summary_roc_plot", height = RESULTS_PLOT_HEIGHT)
  ),

  # ----- 9. Nomogram -----
  step_card(
    "13", "calculator", "Diagnostic nomogram",
    "Nomogram model and 70/30 validation. Training and validation AUC.",
    "danger",
    uiOutput("results_summary_nomogram_ui"),
    plotOutput("results_summary_nomogram_plot", height = RESULTS_PLOT_HEIGHT)
  ),

  # ----- 10. GSEA -----
  step_card(
    "14", "chart-area", "GSEA",
    "Gene Set Enrichment Analysis for target genes. Enrichment plot and pathways.",
    "info",
    uiOutput("results_summary_gsea"),
    plotOutput("results_summary_gsea_plot", height = RESULTS_PLOT_HEIGHT)
  ),

  # ----- 11. Immune -----
  step_card(
    "15", "shield-alt", "Immune cell deconvolution",
    "Estimated immune cell proportions (e.g. EPIC, xCell). Heatmap or boxplot.",
    "primary",
    uiOutput("results_summary_immune"),
    plotOutput("results_summary_immune_plot", height = RESULTS_PLOT_HEIGHT)
  ),

  # ----- 12. Input & pipeline info -----
  step_card(
    "—", "dna", "Input & pipeline",
    "Common genes and expression matrix size after preprocessing.",
    "primary",
    uiOutput("results_summary_input_genes")
  ),

  # ----- Cite (collapsible at bottom) -----
  fluidRow(
    column(12,
      box(
        title = tags$span(icon("quote-right"), " Cite this analysis"),
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        uiOutput("citation_text")
      )
    )
  )
)
