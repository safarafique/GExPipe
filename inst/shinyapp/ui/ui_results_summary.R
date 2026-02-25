# ==============================================================================
# UI_RESULTS_SUMMARY.R - Step 16: Results Summary (compact list, 2 per row)
# ==============================================================================
# Single page: results in pipeline order (Step 3 → last): normalization, batch, DE, WGCNA,
# common genes, GO/KEGG, PPI, ML Venn, GSEA, immune. Two items per row (column 6 + 6).
# ==============================================================================

# Compact height for all results list items (short, less space)
RESULTS_PLOT_HEIGHT <- "260px"

ui_results_summary <- tabItem(
  tabName = "results_summary",
  h2(icon("file-alt"), " Results Summary", style = "color: #2c3e50; font-weight: 700; margin-bottom: 20px;"),

  fluidRow(
    box(
      width = 12, status = "info", solidHeader = TRUE,
      title = tags$span(icon("info-circle"), " About this page"),
      tags$p("This page shows a narrative writeup (one paragraph summarizing the full pipeline from input to immune deconvolution) and all results in order: normalization, batch effect before/after, volcano, heatmap, WGCNA (soft threshold, sample tree, dendrogram, module-trait), common genes, GO & KEGG, PPI, machine learning Venn/UpSet, AUC/ROC curve, diagnostic nomogram, GSEA, and immune cell deconvolution. Each section includes a short summary and the relevant graph(s). Text and numbers update automatically with your data and inputs. Use the button below to download everything as a PDF.", style = "margin-bottom: 0; font-size: 14px;")
    )
  ),

  fluidRow(
    column(12, align = "center", style = "margin-bottom: 25px;",
           downloadButton("download_all_results_pdf",
                          tagList(icon("file-pdf"), " Download all results (PDF)"),
                          class = "btn-danger btn-lg",
                          style = "font-size: 18px; padding: 14px 40px; border-radius: 25px; box-shadow: 0 4px 15px rgba(0,0,0,0.2);"))
  ),

  # ----- Narrative writeup (one paragraph, generalized from data) -----
  fluidRow(
    box(
      title = tags$span(icon("align-left"), " Narrative writeup"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p("Summary of the full pipeline from input data to immune deconvolution. Text updates automatically with your results and input values.", style = "margin-bottom: 10px; font-size: 12px; color: #555;"),
      uiOutput("results_summary_narrative")
    )
  ),

  # ----- Cite this analysis -----
  fluidRow(
    box(
      title = tags$span(icon("quote-right"), " Cite this analysis"),
      width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
      uiOutput("citation_text")
    )
  ),

  # ----- Results list: 2 per row, in pipeline step order (Step 3 → last) -----
  # Row 1 (Step 3 & 5): Normalization | Batch effect removal
  fluidRow(
    column(6,
      box(
        title = tags$span(icon("balance-scale"), " Normalization"),
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        uiOutput("results_summary_norm_batch"),
        tags$p(tags$strong("Description:"), " Step 3 normalization applied to expression data.", style = "margin-top: 8px; font-size: 11px; color: #555;")
      )
    ),
    column(6,
      box(
        title = tags$span(icon("layer-group"), " Batch effect removal"),
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        uiOutput("results_summary_batch_only"),
        tags$p(tags$strong("Description:"), " Step 5 batch correction (e.g. ComBat) applied.", style = "margin-top: 8px; font-size: 11px; color: #555;")
      )
    )
  ),
  # Row 1b: Batch effect before vs after PCA
  fluidRow(
    column(12,
      box(
        title = tags$span(icon("chart-area"), " Batch effect: Before vs After (PCA)"),
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p("PCA of expression before and after batch correction. Samples should mix better after correction.", style = "margin-bottom: 8px; font-size: 12px; color: #555;"),
        plotOutput("results_summary_batch_before_after", height = RESULTS_PLOT_HEIGHT)
      )
    )
  ),
  # Row 2 (Step 6): DE Volcano | DE Heatmap
  fluidRow(
    column(6,
      box(
        title = tags$span(icon("chart-line"), " DE – Volcano"),
        width = NULL, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p("Differential expression (limma): log2 FC vs adjusted P-value. Summary below.", style = "margin-bottom: 6px; font-size: 12px; color: #555;"),
        uiOutput("results_summary_de"),
        plotOutput("results_summary_volcano", height = RESULTS_PLOT_HEIGHT)
      )
    ),
    column(6,
      box(
        title = tags$span(icon("th"), " DE – Heatmap"),
        width = NULL, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p("Heatmap of top DE genes across samples; rows scaled.", style = "margin-bottom: 6px; font-size: 12px; color: #555;"),
        plotOutput("results_summary_de_heatmap", height = RESULTS_PLOT_HEIGHT)
      )
    )
  ),
  # Row 3 (Step 7): WGCNA soft threshold | WGCNA sample tree
  fluidRow(
    column(6,
      box(
        title = tags$span(icon("project-diagram"), " WGCNA – Soft threshold"),
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p("Scale independence and mean connectivity for power selection.", style = "margin-bottom: 6px; font-size: 12px; color: #555;"),
        uiOutput("results_summary_wgcna"),
        plotOutput("results_summary_soft_threshold", height = RESULTS_PLOT_HEIGHT)
      )
    ),
    column(6,
      box(
        title = tags$span(icon("sitemap"), " WGCNA – Sample tree"),
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p("Sample clustering tree (WGCNA).", style = "margin-bottom: 6px; font-size: 12px; color: #555;"),
        plotOutput("results_summary_sample_tree", height = RESULTS_PLOT_HEIGHT)
      )
    )
  ),
  # Row 4 (Step 7): WGCNA dendro | WGCNA module-trait
  fluidRow(
    column(6,
      box(
        title = tags$span(icon("project-diagram"), " WGCNA – Dendrogram"),
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p("Gene dendrogram and module colors.", style = "margin-bottom: 6px; font-size: 12px; color: #555;"),
        plotOutput("results_summary_wgcna_dendro", height = RESULTS_PLOT_HEIGHT)
      )
    ),
    column(6,
      box(
        title = tags$span(icon("chart-bar"), " WGCNA – Module-trait"),
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p("Module–trait correlation heatmap.", style = "margin-bottom: 6px; font-size: 12px; color: #555;"),
        plotOutput("results_summary_module_trait", height = RESULTS_PLOT_HEIGHT)
      )
    )
  ),
  # Row 5 (Step 8): Common significant genes (DEG ∩ WGCNA) | GO & KEGG
  fluidRow(
    column(6,
      box(
        title = tags$span(icon("venus-double"), " Common significant genes"),
        width = NULL, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        uiOutput("results_summary_common_genes"),
        tags$p(tags$strong("Description:"), " DEG ∩ WGCNA; used for GO/KEGG and PPI.", style = "margin-top: 8px; font-size: 11px; color: #555;")
      )
    ),
    column(6,
      box(
        title = tags$span(icon("sitemap"), " GO & KEGG"),
        width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p("GO and KEGG enrichment of common genes (DEG ∩ WGCNA).", style = "margin-bottom: 6px; font-size: 12px; color: #555;"),
        uiOutput("results_summary_go_kegg"),
        plotOutput("results_summary_go_plot", height = RESULTS_PLOT_HEIGHT)
      )
    )
  ),
  # Row 6 (Step 9): KEGG plot | PPI
  fluidRow(
    column(6,
      box(
        title = tags$span(icon("sitemap"), " KEGG enrichment"),
        width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p("KEGG pathway enrichment bar plot.", style = "margin-bottom: 6px; font-size: 12px; color: #555;"),
        plotOutput("results_summary_kegg_plot", height = RESULTS_PLOT_HEIGHT)
      )
    ),
    column(6,
      box(
        title = tags$span(icon("project-diagram"), " PPI network"),
        width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p("Protein–protein interaction network of common genes (top by degree).", style = "margin-bottom: 6px; font-size: 12px; color: #555;"),
        uiOutput("results_summary_ppi"),
        plotOutput("results_summary_ppi_plot", height = RESULTS_PLOT_HEIGHT)
      )
    )
  ),
  # Row 7 (Step 10): Machine learning – UpSet/Venn | Summary
  fluidRow(
    column(6,
      box(
        title = tags$span(icon("circle"), " Machine learning – Venn/UpSet plot"),
        width = NULL, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p("Overlap of gene lists across selected ML methods. Common genes used for ROC and GSEA.", style = "margin-bottom: 8px; font-size: 12px; color: #555;"),
        uiOutput("results_summary_ml"),
        imageOutput("results_summary_ml_venn", height = RESULTS_PLOT_HEIGHT)
      )
    ),
    column(6,
      box(
        title = tags$span(icon("chart-line"), " AUC / ROC curve"),
        width = NULL, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p("ROC curve for mean signature of common ML genes (Step 12).", style = "margin-bottom: 8px; font-size: 12px; color: #555;"),
        plotOutput("results_summary_roc_plot", height = RESULTS_PLOT_HEIGHT)
      )
    )
  ),
  # Row 8: Nomogram | GSEA
  fluidRow(
    column(6,
      box(
        title = tags$span(icon("calculator"), " Diagnostic nomogram"),
        width = NULL, status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p("Diagnostic nomogram validation (Step 13). Summary and nomogram plot.", style = "margin-bottom: 8px; font-size: 12px; color: #555;"),
        uiOutput("results_summary_nomogram_ui"),
        plotOutput("results_summary_nomogram_plot", height = RESULTS_PLOT_HEIGHT)
      )
    ),
    column(6,
      box(
        title = tags$span(icon("chart-area"), " GSEA"),
        width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p("Gene Set Enrichment Analysis for target genes (Step 14).", style = "margin-bottom: 8px; font-size: 12px; color: #555;"),
        uiOutput("results_summary_gsea"),
        plotOutput("results_summary_gsea_plot", height = RESULTS_PLOT_HEIGHT)
      )
    )
  ),
  # Row 10: Immune | Input genes & pipeline
  fluidRow(
    column(6,
      box(
        title = tags$span(icon("shield-alt"), " Immune cell deconvolution"),
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p("Estimated immune cell proportions (e.g. EPIC, xCell).", style = "margin-bottom: 6px; font-size: 12px; color: #555;"),
        uiOutput("results_summary_immune"),
        plotOutput("results_summary_immune_plot", height = RESULTS_PLOT_HEIGHT)
      )
    ),
    column(6,
      box(
        title = tags$span(icon("dna"), " Input genes & pipeline"),
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        uiOutput("results_summary_input_genes"),
        tags$p(tags$strong("Description:"), " Common genes and expression matrix size.", style = "margin-top: 8px; font-size: 11px; color: #555;")
      )
    )
  )
)
