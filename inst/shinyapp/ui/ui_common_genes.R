# ==============================================================================
# UI_COMMON_GENES.R - Common Genes (DEG vs WGCNA) + GO & KEGG
# ==============================================================================

ui_common_genes <- tabItem(
  tabName = "common_genes",
  h2(icon("venus-double"), " Common Genes (DEG vs WGCNA) & Enrichment"),
  
  fluidRow(
    box(
      title = tags$span(icon("info-circle"), " About this step"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p(tags$strong("Purpose:"), " Intersect differentially expressed genes (DEG) with genes in significant WGCNA modules to obtain a high-confidence gene set, then perform GO and KEGG pathway enrichment for biological interpretation.", style = "margin-bottom: 8px;"),
      tags$p(tags$strong("Workflow:"), " 1) Compute Common Genes (DEG ∩ WGCNA) → 2) Run GO and optionally KEGG enrichment → 3) Choose next step: Path 1 = PPI then ML, or Path 2 = direct to ML.", style = "margin-bottom: 8px;"),
      tags$p(tags$strong("Requirements:"), " Step 6 (DE Analysis) and Step 7 (WGCNA, including Identify Significant Modules) must be completed.", style = "margin-bottom: 0;")
    )
  ),

  # ========== COMMON GENES ==========
  fluidRow(
    box(
      title = tags$span(icon("search"), " 1. Find Common Genes (DEG ∩ WGCNA)"),
      width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
      tags$div(
        style = "padding: 15px 0;",
        tags$div(
          style = "display: flex; align-items: center; gap: 15px; flex-wrap: wrap; margin-bottom: 15px;",
          actionButton("compute_common_genes",
                      tagList(icon("calculator"), " Compute Common Genes"),
                      class = "btn-primary btn-lg")
        ),
        uiOutput("common_genes_placeholder_ui"),
        uiOutput("common_genes_summary_ui"),
        tags$hr(),
        DT::dataTableOutput("common_genes_table"),
        tags$div(
          style = "margin-top: 15px;",
          downloadButton("download_common_genes",
                        tagList(icon("download"), " Download Common Genes (CSV)"),
                        class = "btn-success")
        )
      )
    )
  ),
  
  # ========== GO ENRICHMENT ==========
  fluidRow(
    box(
      title = tags$span(icon("sitemap"), " 2. GO Enrichment (Common Genes)"),
      width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
      tags$div(
        style = "padding: 15px 0;",
        fluidRow(
          column(3,
                 numericInput("go_pvalue_cutoff",
                   tags$span("GO p-value cutoff",
                     tags$i(class = "fa fa-question-circle param-help",
                            `data-toggle` = "tooltip", `data-placement` = "top",
                            title = "Maximum p-value for GO enrichment terms.<br><b>0.05</b> = standard, <b>0.01</b> = stricter. Enriched terms with p-value above this are excluded.")),
                   value = 0.05, min = 0.001, max = 0.5, step = 0.01)),
          column(3,
                 numericInput("go_qvalue_cutoff",
                   tags$span("GO q-value cutoff",
                     tags$i(class = "fa fa-question-circle param-help",
                            `data-toggle` = "tooltip", `data-placement` = "top",
                            title = "False discovery rate (FDR) adjusted q-value threshold for GO terms.<br><b>0.2</b> = lenient (default), <b>0.05</b> = stringent.")),
                   value = 0.2, min = 0.01, max = 1, step = 0.05)),
          column(4,
                 tags$div(style = "margin-top: 25px;",
                          actionButton("run_go_enrichment",
                                      tagList(icon("play"), " Run GO Enrichment"),
                                      class = "btn-success btn-block")))
        ),
        uiOutput("go_enrichment_status_ui"),
        tags$hr(),
        tags$h5(icon("chart-bar"), " GO Enrichment (Biological Process)", style = "margin-top: 15px;"),
        plotOutput("go_bp_plot", height = "500px"),
        tags$h5(icon("chart-bar"), " GO Enrichment (Molecular Function)", style = "margin-top: 20px;"),
        plotOutput("go_mf_plot", height = "500px"),
        tags$h5(icon("chart-bar"), " GO Enrichment (Cellular Component)", style = "margin-top: 20px;"),
        plotOutput("go_cc_plot", height = "500px"),
        tags$div(style = "margin-top: 15px;",
                 downloadButton("download_go_results", tagList(icon("download"), " Download GO Results (CSV)"), class = "btn-success"))
      )
    )
  ),
  
  # ========== KEGG ENRICHMENT ==========
  fluidRow(
    box(
      title = tags$span(icon("project-diagram"), " 3. KEGG Enrichment (Common Genes)"),
      width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$div(
        style = "padding: 15px 0;",
        fluidRow(
          column(3,
                 numericInput("kegg_pvalue_cutoff",
                   tags$span("KEGG p-value cutoff",
                     tags$i(class = "fa fa-question-circle param-help",
                            `data-toggle` = "tooltip", `data-placement` = "top",
                            title = "Maximum p-value for KEGG pathway enrichment.<br><b>0.05</b> = standard cutoff.")),
                   value = 0.05, min = 0.001, max = 0.5, step = 0.01)),
          column(2,
                 numericInput("kegg_chord_top_n",
                   tags$span("Chord pathways",
                     tags$i(class = "fa fa-question-circle param-help",
                            `data-toggle` = "tooltip", `data-placement` = "top",
                            title = "Number of top KEGG pathways to display in the chord diagram.<br><b>10</b> = clear visualization, increase for more pathways.")),
                   value = 10, min = 2, max = 30, step = 1)),
          column(4,
                 tags$div(style = "margin-top: 25px;",
                          actionButton("run_kegg_enrichment",
                                      tagList(icon("play"), " Run KEGG Enrichment"),
                                      class = "btn-warning btn-block")))
        ),
        uiOutput("kegg_enrichment_status_ui"),
        tags$p("If KEGG times out or returns no pathways, you can still go to PPI or ML using the \"Go to next step\" box at the end of this tab.", style = "margin-top: 8px; color: #6c757d; font-size: 13px;"),
        tags$hr(),
        tags$h5(icon("chart-bar"), " Bar plot – pathway count and p.adjust", style = "margin-top: 15px; color: #5a6268; font-weight: 600;"),
        fluidRow(column(12, plotOutput("kegg_barplot", height = "520px"))),
        tags$hr(style = "margin: 20px 0; border-color: #dee2e6;"),
        tags$h5(icon("chart-area"), " Chord diagram & Pathway list (side-by-side, no overlap)", style = "margin-top: 10px; margin-bottom: 12px; color: #5a6268; font-weight: 600;"),
        fluidRow(
          column(7,
                 style = "padding-right: 16px;",
                 tags$div(style = "background: #fafbfc; border-radius: 8px; padding: 12px; border: 1px solid #e9ecef;",
                          tags$p(icon("circle"), " Chord – pathway IDs on circle, genes as ribbons", style = "color: #495057; margin-bottom: 8px; font-size: 13px;"),
                          plotOutput("kegg_chord_plot", height = "600px"))),
          column(5,
                 style = "padding-left: 8px;",
                 tags$div(style = "background: #fafbfc; border-radius: 8px; padding: 12px; border: 1px solid #e9ecef; min-height: 620px;",
                          tags$p(icon("list"), " Pathway list (same as chord)", style = "color: #2c3e50; margin-bottom: 8px; font-weight: 600; font-size: 13px;"),
                          DT::dataTableOutput("kegg_pathway_list_table")))
        ),
        tags$div(style = "margin-top: 15px;",
                 downloadButton("download_kegg_results", tagList(icon("download"), " Download KEGG Results (CSV)"), class = "btn-warning"),
                 downloadButton("download_kegg_chord", tagList(icon("download"), " PNG"), class = "btn-info btn-sm", style = "margin-left: 6px;"),
                 downloadButton("download_kegg_chord_jpg", tagList(icon("download"), " JPG"), class = "btn-info btn-sm", style = "margin-left: 4px;"),
                 downloadButton("download_kegg_chord_pdf", tagList(icon("download"), " PDF"), class = "btn-info btn-sm", style = "margin-left: 4px;"))
      )
    )
  ),
  
  # ========== EXTRACT DATA FOR ML (ready for both PPI and ML paths) ==========
  fluidRow(
    box(
      title = tags$span(icon("database"), " Extract Data for Machine Learning (optional)"),
      width = 12, status = "primary", solidHeader = TRUE,
      tags$div(
        class = "alert alert-info",
        "If you plan to go to Machine Learning, extract expression data (samples × common genes) from WGCNA datExpr here. ",
        "This makes the data ready for the ML tab. If you go to PPI first, you can also extract from the PPI page (interacting genes)."
      ),
      tags$div(style = "margin-bottom: 15px;",
               actionButton("extract_ml_data_common_genes",
                           tagList(icon("cogs"), " Extract Data for ML (Common Genes)"),
                           class = "btn-primary btn-lg")),
      uiOutput("common_genes_extracted_ml_status_ui"),
      tags$div(style = "margin-top: 10px;", uiOutput("common_genes_download_extracted_ml_ui"))
    )
  ),

  # ========== GO TO NEXT STEP: two paths (PPI then ML, or direct to ML) ==========
  fluidRow(
    box(
      width = 12, status = "success", solidHeader = TRUE,
      title = tags$span(icon("route"), " Choose next step: PPI then ML, or direct to ML"),
      tags$p("You have two options. Pick one:", style = "margin-bottom: 10px; color: #2c3e50; font-weight: 600;"),
      tags$ul(
        style = "margin-bottom: 14px; padding-left: 20px; color: #495057;",
        tags$li(tags$strong("Path 1 – PPI then Machine Learning:"), " Build a protein–protein interaction network from common genes, then go to ML (e.g. with hub or interacting genes)."),
        tags$li(tags$strong("Path 2 – Direct to Machine Learning:"), " Skip PPI and go straight to ML using common genes (or extracted data from above).")
      ),
      tags$div(
        style = "display: flex; flex-wrap: wrap; gap: 15px; align-items: center;",
        actionButton("next_page_common_genes_end",
                    tagList(icon("project-diagram"), " Path 1: PPI Interaction → then ML"),
                    class = "btn-success btn-lg",
                    style = "font-size: 16px; padding: 12px 28px; border-radius: 25px;"),
        actionButton("next_page_common_genes_to_ml",
                    tagList(icon("brain"), " Path 2: Direct to Machine Learning"),
                    class = "btn-primary btn-lg",
                    style = "font-size: 16px; padding: 12px 28px; border-radius: 25px;")
      )
    )
  )
)
