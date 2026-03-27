# ==============================================================================
# UI_GROUPS.R - Step 4: Select Groups Tab
# ==============================================================================

ui_groups <- tabItem(
    tabName = "groups",
    h2(icon("users"), " Step 4: Select Group Columns & Categorize"),

    # DE Method Banner — tells user which pipeline is active
    uiOutput("groups_de_method_banner"),

    fluidRow(
      box(
        title = tags$span(icon("info-circle"), " About this step"),
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p(tags$strong("Purpose:"), " Define phenotype groups (e.g. control vs disease) for differential expression and WGCNA. Samples are assigned to groups using metadata columns from your datasets.", style = "margin-bottom: 8px;"),
        tags$p(tags$strong("Disease-specific analysis:"), " Assign your condition groups to ", tags$strong("Disease"), " and controls to ", tags$strong("Normal"), ". Step 6 (DE) and Step 7 (WGCNA) will use these to find disease-associated genes and modules.", style = "margin-bottom: 8px; color: #2c3e50;"),
        tags$p(tags$strong("Need both groups for DE:"), " Differential expression (Step 6) compares Normal vs Disease. You must have at least one sample in ", tags$strong("Normal"), " and one in ", tags$strong("Disease"), ". If you enter GSEs from the same sample source (e.g. same study with only one condition), you will not get DEGs until you add a dataset that contains the other condition.", style = "margin-bottom: 8px; color: #555;"),
        tags$p(tags$strong("Workflow:"), " Browse the phenodata table below to understand your metadata, select the phenotype column per dataset, extract unique group labels, categorize each as Normal, Disease, or None, then apply.", style = "margin-bottom: 0;")
      )
    ),
    fluidRow(
      box(
        title = tags$span(icon("info-circle"), " Quick Guide"), 
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        tags$div(
          style = "padding: 10px 0;",
          tags$ol(
            style = "font-size: 15px; line-height: 2;",
            tags$li(tags$strong("Browse Phenodata:"), " Review all metadata columns for each dataset"),
            tags$li(tags$strong("Select Column:"), " Choose the phenotype column that contains group info (e.g. tissue, disease state)"),
            tags$li(tags$strong("Extract Groups:"), " Click to extract unique groups from the selected column"),
            tags$li(tags$strong("Categorize:"), " Assign each group as Normal, Disease, or None"),
            tags$li(tags$strong("Apply:"), " Finalize your selections")
          )
        )
      )
    ),

    # ---- Phenodata Browser (full interactive table) ----
    fluidRow(
      box(
        title = tags$span(icon("table"), " Phenodata Browser — Browse All Metadata Columns"),
        width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$div(
          style = "padding: 8px 0 4px 0;",
          tags$p(
            icon("lightbulb", style = "color: #f39c12; margin-right: 5px;"),
            tags$em("Browse the full phenodata table for each dataset. Scroll horizontally to see all columns. Use the search box to filter. This helps you find the correct column for group assignment."),
            style = "color: #6c757d; font-size: 13px; margin-bottom: 10px; padding: 8px 12px; background: #f8f9fa; border-radius: 5px;"
          )
        ),
        uiOutput("phenodata_browser_ui")
      )
    ),
    
    # ---- Column selector per dataset ----
    fluidRow(
      box(
        title = tags$span(icon("database"), " Select Phenotype Columns"), 
        width = 12, status = "primary", solidHeader = TRUE,
        uiOutput("group_selector_ui")
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("search"), " Extract Groups"), 
        width = 12, status = "warning", solidHeader = TRUE,
        tags$div(
          style = "text-align: center; padding: 15px;",
          actionButton("extract_groups_btn", 
                       tagList(icon("search"), " Extract Groups from Selected Columns"), 
                       class = "btn-warning btn-lg",
                       style = "font-size: 16px; padding: 12px 30px;")
        )
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("tags"), " Categorize Groups"), 
        width = 12, status = "success", solidHeader = TRUE,
        uiOutput("extracted_groups_ui")
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("check-double"), " Apply Groups"), 
        width = 12, status = "danger", solidHeader = TRUE,
        tags$div(
          style = "text-align: center; padding: 15px;",
          actionButton("apply_groups_btn", 
                       tagList(icon("check"), " Apply Categorization"), 
                       class = "btn-danger btn-lg",
                       style = "font-size: 16px; padding: 12px 30px;")
        )
      )
    ),
    
    fluidRow(
      box(title = tags$span(icon("chart-pie"), " Group Summary"), 
          width = 12, status = "info", solidHeader = TRUE,
          uiOutput("group_summary_ui"))
    ),
    fluidRow(
      box(
        title = tags$span(icon("file-alt"), " Process Summary"),
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        uiOutput("groups_process_summary_ui"))
    ),
    fluidRow(
      box(width = 12, status = "info", solidHeader = FALSE,
          tags$div(class = "next-btn", style = "text-align: center; padding: 20px 0;",
                   actionButton("next_page_groups", "Next: Batch Correction",
                                icon = icon("arrow-right"), class = "btn-success btn-lg",
                                style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;")))
    ),
  )
