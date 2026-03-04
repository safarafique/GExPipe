# ==============================================================================
# UI_ML.R - Step 10: Machine Learning Process (LASSO, RF, SVM-RFE)
# ==============================================================================
# Uses rv$extracted_data_ml (from PPI "Extract Data for ML") and rv$wgcna_sample_info.
# ==============================================================================

ui_ml <- tabItem(
  tabName = "ml",
  h2(icon("brain"), " Step 10: Machine Learning Process"),

  fluidRow(
    box(
      title = tags$span(icon("info-circle"), " About this step"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p(tags$strong("Purpose:"), " Perform supervised machine learning on expression data (samples × genes) to rank genes by predictive importance for biomarker discovery. Supports LASSO, Elastic Net, Ridge, Random Forest, SVM-RFE, Boruta, sPLS-DA, and XGBoost+SHAP.", style = "margin-bottom: 8px;"),
      tags$p(tags$strong("Methods:"), " LASSO, Elastic Net, Ridge (glmnet), Random Forest, SVM-RFE, Boruta (all-relevant feature selection), sPLS-DA (sparse PLS-DA), XGBoost+SHAP (interpretable importance). Optional PPI centrality weights (from Step 9) can weight features. Labels from WGCNA sample metadata (e.g. Normal vs Disease).", style = "margin-bottom: 8px;"),
      tags$p(tags$strong("Requirements:"), " On the PPI tab, click 'Extract Data for ML' to provide expression matrix and sample labels.", style = "margin-bottom: 0;")
    )
  ),

  fluidRow(
    box(
      title = tags$span(icon("cogs"), " Run ML Analysis"),
      width = 12, status = "primary", solidHeader = TRUE,
      tags$p(tags$strong("Choose which methods to run:"), " Select 1 or more (up to all 8). Results and Venn diagram will use only the selected methods.", style = "margin-bottom: 10px; font-size: 12px;"),
      fluidRow(
        column(4,
               checkboxGroupInput("ml_methods", "Methods to run",
                                  choices = c("LASSO" = "lasso", "Elastic Net" = "elastic", "Ridge" = "ridge", "Random Forest" = "rf", "SVM-RFE" = "svm", "Boruta" = "boruta", "sPLS-DA" = "splsda", "XGBoost+SHAP" = "xgboost"),
                                  selected = c("lasso", "elastic", "ridge", "rf", "svm"),
                                  inline = FALSE)
        ),
        column(2, numericInput("ml_rf_top_genes",
          tags$span("Top N (RF)",
            tags$i(class = "fa fa-question-circle param-help",
                   `data-toggle` = "tooltip", `data-placement` = "top",
                   title = "Number of top-ranked genes to select from Random Forest importance scores.")),
          value = 8, min = 5, max = 100, step = 1)),
        column(2, numericInput("ml_svm_top_genes",
          tags$span("Top N (SVM-RFE)",
            tags$i(class = "fa fa-question-circle param-help",
                   `data-toggle` = "tooltip", `data-placement` = "top",
                   title = "Number of top-ranked genes to select from SVM Recursive Feature Elimination.")),
          value = 10, min = 5, max = 100, step = 1)),
        column(2, numericInput("ml_elastic_top_genes",
          tags$span("Top N (Elastic Net)",
            tags$i(class = "fa fa-question-circle param-help",
                   `data-toggle` = "tooltip", `data-placement` = "top",
                   title = "Number of top genes from Elastic Net (L1+L2 regularization, alpha=0.5).")),
          value = 10, min = 5, max = 100, step = 1)),
        column(2, numericInput("ml_ridge_top_genes",
          tags$span("Top N (Ridge)",
            tags$i(class = "fa fa-question-circle param-help",
                   `data-toggle` = "tooltip", `data-placement` = "top",
                   title = "Number of top genes from Ridge regression (L2 regularization). Ridge keeps all variables but shrinks coefficients.")),
          value = 10, min = 5, max = 100, step = 1))
      ),
      fluidRow(
        column(2, numericInput("ml_boruta_top_genes",
          tags$span("Top N (Boruta)",
            tags$i(class = "fa fa-question-circle param-help",
                   `data-toggle` = "tooltip", `data-placement` = "top",
                   title = "Max genes from Boruta all-relevant feature selection. Boruta identifies all features significantly more important than random shadow features.")),
          value = 15, min = 5, max = 100, step = 1)),
        column(2, numericInput("ml_splsda_top_genes",
          tags$span("Top N (sPLS-DA)",
            tags$i(class = "fa fa-question-circle param-help",
                   `data-toggle` = "tooltip", `data-placement` = "top",
                   title = "Number of genes from sparse PLS-DA (mixOmics). sPLS-DA selects discriminant genes in a multivariate latent variable framework.")),
          value = 15, min = 5, max = 100, step = 1)),
        column(2, numericInput("ml_xgboost_top_genes",
          tags$span("Top N (XGBoost+SHAP)",
            tags$i(class = "fa fa-question-circle param-help",
                   `data-toggle` = "tooltip", `data-placement` = "top",
                   title = "Number of top genes from XGBoost importance ranked by SHAP values. SHAP provides interpretable, additive feature attributions.")),
          value = 15, min = 5, max = 100, step = 1)),
        column(3, tags$div(style = "margin-top: 25px; min-width: 200px;",
                 actionButton("run_ml",
                             tagList(icon("play"), " Run ML Analysis"),
                             class = "btn-primary btn-lg btn-block")))
      ),
      uiOutput("ml_status_ui"),
      uiOutput("ml_placeholder_ui")
    )
  ),

  # --- First: Venn diagram and Final list (as in reference) ---
  fluidRow(
    column(6,
      box(
        title = tags$span(icon("circle"), " Venn Diagram (Selected Methods)"),
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        uiOutput("ml_venn_message"),
        plotOutput("ml_venn_plot", height = "380px"),
        tags$div(style = "margin-top: 8px;",
        downloadButton("download_ml_venn", tagList(icon("download"), " PNG (300 DPI)"), class = "btn-primary btn-sm", style = "margin-right: 6px;"),
        downloadButton("download_ml_venn_pdf", tagList(icon("download"), " PDF"), class = "btn-primary btn-sm"))
      )
    ),
    column(6,
      box(
        title = tags$span(icon("list-ol"), " Final List (for ROC & GSEA)"),
        width = NULL, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p(tags$strong("Common genes (selected methods):"), " Use in Step 12 (ROC) and Step 14 (GSEA).", style = "margin-bottom: 8px; font-size: 12px;"),
        uiOutput("ml_final_list_ui"),
        DT::dataTableOutput("ml_final_list_table"),
        tags$div(style = "margin-top: 8px;",
                 downloadButton("download_ml_final_list", tagList(icon("download"), " Final List (CSV)"), class = "btn-success btn-sm"),
                 tags$span(style = "margin-left: 8px;"),
                 downloadButton("download_ml_combined_list", tagList(icon("download"), " Combined List (CSV)"), class = "btn-info btn-sm"))
      )
    )
  ),

  # --- Correlation & Co-expression of final biomarker panel ---
  fluidRow(
    box(
      title = tags$span(icon("project-diagram"), " Correlation & Co-expression of Final Biomarkers"),
      width = 12, status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p("Spearman gene–gene correlation and co-expression network for the final biomarker panel (common ML genes).",
             style = "margin-bottom: 10px; font-size: 12px;"),
      fluidRow(
        column(6,
          tags$p(tags$strong("Gene–gene Spearman correlation heatmap"), style = "margin-bottom: 6px;"),
          plotOutput("ml_biomarker_cor_heatmap", height = "380px")
        ),
        column(6,
          tags$p(tags$strong("Co-expression network (|Spearman| \u2265 0.7)"), style = "margin-bottom: 6px;"),
          plotOutput("ml_biomarker_coexp_network", height = "380px")
        )
      )
    )
  ),

  # --- Diagnostic plots (selected methods): RF error/importance, LASSO/Elastic/Ridge path & deviance ---
  fluidRow(
    box(
      title = tags$span(icon("chart-area"), " Diagnostic plots (selected methods)"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p("Plots for methods that were run: Random Forest (error rates, importance), LASSO / Elastic Net / Ridge (coefficient path and deviance vs lambda).", style = "margin-bottom: 12px; font-size: 12px;"),
      uiOutput("ml_diagnostic_plots_ui")
    )
  ),

  # --- At the end: All gene lists by ML method ---
  fluidRow(
    box(
      title = tags$span(icon("list"), " All gene lists by ML method"),
      width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      tags$p("Gene lists from each machine learning method. Use Final List (above) for ROC and GSEA; use these tables to see which genes were selected by LASSO, Elastic Net, Ridge, Random Forest, SVM-RFE, Boruta, sPLS-DA, and XGBoost+SHAP.", style = "margin-bottom: 12px; font-size: 12px;")
    )
  ),
  fluidRow(
    column(6,
      box(
        title = tags$span(icon("chart-line"), " LASSO (gene list)"),
        width = NULL, status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        DT::dataTableOutput("ml_lasso_table"),
        tags$div(style = "margin-top: 8px;", downloadButton("download_ml_lasso", tagList(icon("download"), " LASSO (CSV)"), class = "btn-success btn-sm"))
      )
    ),
    column(6,
      box(
        title = tags$span(icon("chart-area"), " Elastic Net (gene list)"),
        width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        DT::dataTableOutput("ml_elastic_table"),
        tags$div(style = "margin-top: 8px;", downloadButton("download_ml_elastic", tagList(icon("download"), " Elastic Net (CSV)"), class = "btn-info btn-sm"))
      )
    )
  ),
  fluidRow(
    column(6,
      box(
        title = tags$span(icon("chart-bar"), " Ridge (gene list)"),
        width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        DT::dataTableOutput("ml_ridge_table"),
        tags$div(style = "margin-top: 8px;", downloadButton("download_ml_ridge", tagList(icon("download"), " Ridge (CSV)"), class = "btn-primary btn-sm"))
      )
    ),
    column(6,
      box(
        title = tags$span(icon("tree"), " Random Forest (gene list)"),
        width = NULL, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        DT::dataTableOutput("ml_rf_table"),
        tags$div(style = "margin-top: 8px;", downloadButton("download_ml_rf", tagList(icon("download"), " RF (CSV)"), class = "btn-success btn-sm"))
      )
    )
  ),
  fluidRow(
    column(6,
      box(
        title = tags$span(icon("sort-amount-down"), " SVM-RFE (gene list)"),
        width = NULL, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        DT::dataTableOutput("ml_svm_table"),
        tags$div(style = "margin-top: 8px;", downloadButton("download_ml_svm", tagList(icon("download"), " SVM-RFE (CSV)"), class = "btn-warning btn-sm"))
      )
    ),
    column(6,
      box(
        title = tags$span(icon("fire"), " Boruta (gene list)"),
        width = NULL, status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        DT::dataTableOutput("ml_boruta_table"),
        tags$div(style = "margin-top: 8px;", downloadButton("download_ml_boruta", tagList(icon("download"), " Boruta (CSV)"), class = "btn-danger btn-sm"))
      )
    )
  ),
  fluidRow(
    column(6,
      box(
        title = tags$span(icon("chart-pie"), " sPLS-DA (gene list)"),
        width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        DT::dataTableOutput("ml_splsda_table"),
        tags$div(style = "margin-top: 8px;", downloadButton("download_ml_splsda", tagList(icon("download"), " sPLS-DA (CSV)"), class = "btn-info btn-sm"))
      )
    ),
    column(6,
      box(
        title = tags$span(icon("bolt"), " XGBoost+SHAP (gene list)"),
        width = NULL, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        DT::dataTableOutput("ml_xgboost_table"),
        tags$div(style = "margin-top: 8px;", downloadButton("download_ml_xgboost", tagList(icon("download"), " XGBoost+SHAP (CSV)"), class = "btn-success btn-sm"))
      )
    )
  ),
  fluidRow(
    column(6,
      box(
        title = tags$span(icon("venus-double"), " Common genes (selected methods)"),
        width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        uiOutput("ml_common_genes_ui"),
        DT::dataTableOutput("ml_common_genes_table"),
        tags$div(style = "margin-top: 8px;", downloadButton("download_ml_common_genes", tagList(icon("download"), " Common Genes (CSV)"), class = "btn-info btn-sm"))
      )
    )
  ),
  fluidRow(
    box(
      title = tags$span(icon("file-alt"), " Process Summary"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
      uiOutput("ml_process_summary_ui"))
  ),
  fluidRow(
    box(width = 12, status = "primary", solidHeader = FALSE,
        tags$div(style = "text-align: center; padding: 20px 0;",
                 actionButton("next_page_ml_to_roc",
                             tagList(icon("shield-alt"), " Continue to Validation Setup"),
                             class = "btn-success btn-lg",
                             style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;")))
  )
)
