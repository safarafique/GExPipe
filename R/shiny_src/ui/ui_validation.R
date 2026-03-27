# ==============================================================================
# UI_VALIDATION.R - Step 11: Validation Setup (External / Internal)
# ==============================================================================
# User selects External or Internal validation mode.
# External: download GEO validation dataset, select groups, run DE.
# Internal: proceed with 70/30 split for nomogram (no extra download needed).
# ==============================================================================

ui_validation <- tabItem(
  tabName = "validation",
  h2(icon("shield-alt"), " Step 11: Validation Setup"),

  fluidRow(
    box(
      title = tags$span(icon("info-circle"), " About this step"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p(tags$strong("Purpose:"), " Choose how to validate your biomarker genes. External validation uses an independent GEO dataset; Internal validation uses a 70/30 train/test split of your current data.", style = "margin-bottom: 8px;"),
      tags$p(tags$strong("External Validation:"), " Download an independent GSE dataset, categorize groups (Normal/Disease), run DE, then ROC & Nomogram use this external cohort.", style = "margin-bottom: 8px;"),
      tags$p(tags$strong("Internal Validation:"), " ROC is computed on the training data. Nomogram uses 70/30 stratified split-sample validation.", style = "margin-bottom: 0;")
    )
  ),

  # ---- Step A: Choose Validation Mode ----
  fluidRow(
    box(
      title = tags$span(icon("code-branch"), " Choose Validation Strategy"),
      width = 12, status = "primary", solidHeader = TRUE,
      tags$div(
        style = "padding: 15px 0;",
        fluidRow(
          column(6,
            tags$div(
              id = "val_mode_ext_card",
              style = "padding: 25px; background: #fff; border: 3px solid #27ae60; border-radius: 12px; text-align: center; cursor: pointer; transition: all 0.3s ease; min-height: 200px;",
              radioButtons("validation_mode", NULL,
                choices = c(
                  "External Validation (Independent GEO Dataset)" = "external",
                  "Internal Validation (70/30 Split)" = "internal"
                ),
                selected = "external", width = "100%"
              )
            )
          ),
          column(6,
            uiOutput("validation_mode_info_ui")
          )
        )
      )
    )
  ),

  # ---- Step B: External Validation Configuration (conditional) ----
  uiOutput("ext_val_config_ui"),

  # ---- Step B: Phenodata & Group Selection ----
  uiOutput("val_phenodata_ui"),

  # ---- Step C: Group Categorization ----
  uiOutput("val_run_ui"),

  # ---- Status ----
  uiOutput("val_status_ui"),

  # ---- DE Results (conditional) ----
  uiOutput("val_de_panel_ui"),

  fluidRow(
    box(
      title = tags$span(icon("file-alt"), " Process Summary"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
      uiOutput("validation_process_summary_ui"))
  ),
  # ---- Navigation ----
  fluidRow(
    box(width = 12, status = "primary", solidHeader = FALSE,
        tags$div(style = "text-align: center; padding: 20px 0;",
                 actionButton("next_page_validation_to_roc",
                             tagList(icon("chart-line"), " Continue to ROC Curve Analysis"),
                             class = "btn-success btn-lg",
                             style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;")))
  )
)
