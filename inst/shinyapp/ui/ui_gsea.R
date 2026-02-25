# ==============================================================================
# UI_GSEA.R - Step 14: GSEA Analysis for Signature Genes
# ==============================================================================
# Uses rv$ml_common_genes (or common_genes_de_wgcna) and rv$batch_corrected.
# ==============================================================================

ui_gsea <- tabItem(
  tabName = "gsea",
  h2(icon("project-diagram"), " Step 14: GSEA Analysis (Signature Genes)"),

  fluidRow(
    box(
      title = tags$span(icon("info-circle"), " About this step"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p(tags$strong("Purpose:"), " Gene Set Enrichment Analysis (GSEA) for each target gene separately. For each gene, ranks all genes by correlation with that gene, runs GSEA with selected MSigDB collection(s) (Hallmark, GO, KEGG, Reactome, etc.), and shows one plot and one pathway list per gene.", style = "margin-bottom: 8px;"),
      tags$p(tags$strong("Requirements:"), " Run ML (Step 10) for common genes, or have common genes from Step 8. Batch-corrected expression must be available.", style = "margin-bottom: 0;")
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("cogs"), " Run GSEA"),
      width = 12, status = "primary", solidHeader = TRUE,
      fluidRow(
        column(7,
          selectInput("gsea_collection", "Gene Set Collection(s):",
            choices = c(
              "Hallmark (H)" = "H",
              "GO Biological Process (C5:BP)" = "C5_BP",
              "GO Molecular Function (C5:MF)" = "C5_MF",
              "KEGG Pathways (C2:CP:KEGG)" = "C2_KEGG",
              "Reactome (C2:CP:REACTOME)" = "C2_REACTOME",
              "Immunologic Signatures (C7)" = "C7",
              "Oncogenic Signatures (C6)" = "C6"
            ),
            selected = "H", multiple = TRUE, width = "100%")
        ),
        column(5, uiOutput("gsea_collection_info_ui"))
      ),
      fluidRow(
        column(4, tags$p(tags$strong("Target genes:"), " Common ML genes (from Step 10) or common DEG+WGCNA genes. You can also type custom gene symbols (comma-separated)."), style = "padding-top: 8px;"),
        column(5, textAreaInput("gsea_target_genes", "Custom target genes (optional, comma-separated)", value = "", rows = 3, placeholder = "Leave empty to use common ML genes")),
        column(3, tags$div(style = "margin-top: 25px;",
                 actionButton("run_gsea", tagList(icon("play"), " Run GSEA"), class = "btn-primary btn-lg btn-block")))
      ),
      uiOutput("gsea_status_ui"),
      uiOutput("gsea_placeholder_ui")
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("chart-area"), " GSEA per gene (plot and pathway list)"),
      width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p("For each target gene: left = enrichment plot, right = enriched pathways list. Scroll for more genes.", style = "margin-bottom: 10px; font-size: 12px;"),
      uiOutput("gsea_per_gene_container")
    )
  ),

  fluidRow(
    box(width = 12, status = "primary", solidHeader = FALSE,
        tags$div(style = "text-align: center; padding: 20px 0;",
                 actionButton("next_page_gsea_to_immune",
                             tagList(icon("shield-alt"), " Continue to Immune Cell Deconvolution"),
                             class = "btn-success btn-lg",
                             style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;")))
  )
)
