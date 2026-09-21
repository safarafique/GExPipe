# ==============================================================================
# UI_CONSENSUS.R - Step 7: RNA-seq/microarray intersect
# ==============================================================================

ui_consensus <- tabItem(
  tabName = "consensus",
  h2(icon("object-ungroup"), " Step 7: RNA-seq \u2229 microarray"),

  fluidRow(
    box(
      title = tags$span(icon("info-circle"), " About this step"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p(
        tags$strong("Purpose:"),
        " After separate DE (Step 6), keep genes significant on ",
        tags$strong("both RNA-seq and microarray"),
        " with the same fold-change direction. This is the DEG list only. Merged (Both) and single-platform runs skip this step.",
        style = "margin-bottom: 8px;"
      ),
      tags$p(
        tags$strong("Next - WGCNA (Step 8):"),
        " Do ",
        tags$strong("not"),
        " feed this DEG list into WGCNA. Step 8 builds one network on a processed matrix (RNA-seq VST or microarray after batch; Auto = more samples) using the top 5,000-8,000 variable genes. Step 9 overlaps these consensus DEGs with those module genes.",
        style = "margin-bottom: 0;"
      )
    )
  ),

  fluidRow(
    uiOutput("consensus_status_ui")
  ),

  fluidRow(
    box(
      title = tags$span(icon("sliders-h"), " Consensus rules"),
      width = 12, status = "primary", solidHeader = TRUE,
      radioButtons(
        "consensus_mode_parallel",
        label = tags$strong("Consensus rule:"),
        choices = c(
          "Auto (recommended) - same direction on both platforms" = "auto",
          "Manual - optional same-direction rule" = "manual"
        ),
        selected = "auto",
        inline = TRUE
      ),
      uiOutput("consensus_parallel_guide_ui"),
      conditionalPanel(
        condition = "input.consensus_mode_parallel == 'manual'",
        checkboxInput(
          "consensus_same_direction",
          "Require same direction (up on both or down on both)",
          value = TRUE
        )
      ),
      tags$p(
        "Adjusted p-value and logFC cutoffs are those you set in Step 6. Re-run Step 6 to change them. This list is for Step 9, not WGCNA.",
        style = "color: #555; font-size: 13px; margin-bottom: 12px;"
      ),
      actionButton(
        "apply_consensus",
        tagList(icon("check-double"), " Apply consensus for later steps"),
        class = "btn-success btn-lg"
      )
    )
  ),

  uiOutput("consensus_count_legend_ui"),
  uiOutput("consensus_count_cards_ui"),

  fluidRow(
    box(
      title = tags$span(icon("chart-pie"), " Venn: Common = name overlap (center)"),
      width = 6, status = "warning", solidHeader = TRUE,
      tags$p(
        icon("circle"), " ", tags$strong("Common"),
        " = genes significant on both platforms (Venn center). Direction can differ.",
        tags$br(),
        icon("check-double"), " ", tags$strong("Consensus"),
        " = Common genes that go the same way (up on both or down on both). That is the table on the right and what Apply stores.",
        style = "font-size: 13px; color: #555; margin-bottom: 8px;"
      ),
      plotOutput("consensus_venn_plot", height = "440px"),
      tags$div(
        style = "margin-top: 10px;",
        downloadButton("download_consensus_venn_png", tagList(icon("download"), " PNG"), class = "btn-info btn-sm", style = "margin-right: 6px;"),
        downloadButton("download_consensus_venn_jpg", tagList(icon("download"), " JPG"), class = "btn-info btn-sm", style = "margin-right: 6px;"),
        downloadButton("download_consensus_venn_pdf", tagList(icon("download"), " PDF"), class = "btn-info btn-sm")
      )
    ),
    box(
      title = tags$span(icon("check-double"), " Consensus genes (same direction - Apply this list)"),
      width = 6, status = "success", solidHeader = TRUE,
      DTOutput("consensus_table"),
      tags$div(
        style = "margin-top: 12px;",
        downloadButton("download_consensus_genes", tagList(icon("download"), " Consensus genes (CSV)"), class = "btn-success"),
        downloadButton("download_consensus_rna_only", tagList(icon("download"), " RNA-only DEGs"), class = "btn-default", style = "margin-left: 8px;"),
        downloadButton("download_consensus_micro_only", tagList(icon("download"), " Microarray-only DEGs"), class = "btn-default", style = "margin-left: 8px;")
      )
    )
  ),

  fluidRow(
    box(
      width = 12, status = "primary", solidHeader = FALSE,
      tags$div(
        style = "text-align: center; padding: 20px;",
        uiOutput("consensus_next_button_ui")
      )
    )
  )
)
