# ==============================================================================
# UI_BATCH.R - Step 5: Batch Correction Tab
# ==============================================================================

ui_batch <- tabItem(
    tabName = "batch",
    h2(icon("filter"), " Step 5: Gene Filtering & Batch Correction"),
    
    fluidRow(
      box(title = tags$span(icon("chart-bar"), " Gene Variance Distribution"), 
          width = 12, status = "warning", solidHeader = TRUE,
          plotOutput("gene_variance_plot", height = "300px"))
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("filter"), " Gene Filtering - Remove Low Variance Genes"), 
        width = 12, status = "info", solidHeader = TRUE,
        tags$div(
          style = "padding: 15px 0;",
          fluidRow(
            column(6,
                   tags$div(
                     style = "padding-right: 15px;",
                     tags$label(
                       tags$strong(icon("sliders-h"), " Variance Percentile Cutoff:"),
                       tags$i(class = "fa fa-question-circle param-help",
                              `data-toggle` = "tooltip", `data-placement` = "right",
                              title = "Remove genes with the lowest expression variance across samples. These low-variance genes add noise without contributing to differential analysis.<br><b>25%</b> = moderate (removes bottom quarter), <b>10%</b> = conservative, <b>40%</b> = aggressive filtering."),
                       style = "font-size: 16px; color: #2c3e50; margin-bottom: 10px; display: block;"
                     ),
                     tags$p(
                       "Select the percentile below which genes will be filtered out.",
                       style = "color: #6c757d; font-size: 13px; margin-bottom: 15px;"
                     ),
                     sliderInput("variance_percentile",
                                 label = NULL,
                                 min = 0,
                                 max = 50,
                                 value = 25,
                                 step = 1,
                                 post = "%",
                                 width = "100%"),
                     tags$div(
                       style = "margin-top: 10px; padding: 12px; background: #e8f4f8; border-left: 4px solid #3498db; border-radius: 5px;",
                       tags$div(
                         style = "display: flex; justify-content: space-between; align-items: center;",
                         tags$span(
                           tags$strong("Genes to keep: "),
                           tags$span(textOutput("genes_to_keep", inline = TRUE), 
                                    style = "color: #3498db; font-weight: bold; font-size: 16px;")
                         ),
                         tags$span(
                           tags$strong("Genes to remove: "),
                           tags$span(textOutput("genes_to_remove", inline = TRUE), 
                                    style = "color: #e74c3c; font-weight: bold; font-size: 16px;")
                         )
                       ),
                       tags$div(
                         style = "margin-top: 8px; padding-top: 8px; border-top: 1px solid #b8daff;",
                         tags$small(
                           icon("info-circle", style = "margin-right: 5px;"),
                           textOutput("filter_info", inline = TRUE),
                           style = "color: #495057;"
                         )
                       )
                     )
                   )
            ),
            column(6,
                   tags$div(
                     style = "padding-left: 15px;",
                     tags$label(
                       tags$strong(icon("magic"), " Batch Correction Method:"),
                       tags$i(class = "fa fa-question-circle param-help",
                              `data-toggle` = "tooltip", `data-placement` = "right",
                              title = "Method to remove technical batch effects.<br><b>ComBat-ref:</b> Recommended — largest dataset as reference.<br><b>SVA:</b> Surrogate variables — unknown/hidden confounders.<br><b>limma:</b> Fast linear model.<br><b>ComBat:</b> Empirical Bayes.<br><b>Quantile+limma:</b> Two-step.<br><b>Hybrid:</b> Quantile + ComBat."),
                       style = "font-size: 16px; color: #2c3e50; margin-bottom: 15px; display: block;"
                     ),
                     tags$div(
                       style = "margin-bottom: 20px;",
                       radioButtons("batch_method",
                                    label = NULL,
                                    choices = list(
                                      "ComBat-ref (Recommended)" = "combat_ref",
                                      "SVA (surrogate variables)" = "sva",
                                      "limma removeBatchEffect" = "limma",
                                      "ComBat" = "combat",
                                      "Quantile + limma" = "quantile_limma",
                                      "Hybrid" = "hybrid"
                                    ),
                                    selected = "combat_ref",
                                    width = "100%")
                     ),
                     tags$div(
                       style = "margin-top: 20px;",
                       actionButton("apply_batch", 
                                    tagList(icon("magic"), " Apply Batch Correction"), 
                                    class = "btn-success btn-lg",
                                    style = "width: 100%; font-size: 16px; padding: 12px 20px;")
                     )
                   )
            )
          ),
          tags$div(
            style = "margin-top: 20px; padding: 15px; background: #f8f9fa; border-left: 4px solid #3498db; border-radius: 5px;",
            tags$p(
              tags$strong(icon("info-circle"), " Method Descriptions:"),
              style = "margin-bottom: 10px; color: #2c3e50; font-size: 14px;"
            ),
            tags$ul(
              style = "margin: 0; padding-left: 20px; color: #495057; font-size: 13px; line-height: 1.8;",
              tags$li(tags$strong("ComBat-ref:"), " Recommended for multiple datasets with reference batch"),
              tags$li(tags$strong("SVA:"), " Surrogate variable analysis — captures unknown/hidden confounders"),
              tags$li(tags$strong("limma:"), " Fast batch effect removal using linear models"),
              tags$li(tags$strong("ComBat:"), " Empirical Bayes batch correction"),
              tags$li(tags$strong("Quantile + limma:"), " Quantile normalization followed by limma"),
              tags$li(tags$strong("Hybrid:"), " Quantile normalization + ComBat combination")
            )
          )
        )
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("chart-line"), " PCA Visualization - Batch Effect Assessment"), 
        width = 12, status = "primary", solidHeader = TRUE,
        tags$div(
          style = "padding: 10px 0; margin-bottom: 15px;",
          tags$p(
            tags$strong(icon("info-circle"), " Interpretation Guide:"),
            style = "color: #2c3e50; font-size: 13px; margin-bottom: 10px;"
          ),
          tags$ul(
            style = "margin: 0; padding-left: 20px; color: #495057; font-size: 12px; line-height: 1.8;",
            tags$li(tags$strong("By Dataset:"), " Compare left (before) vs right (after) - datasets should be intermingled after correction"),
            tags$li(tags$strong("By Condition:"), " Compare left (before) vs right (after) - biological signal should be clearer after correction")
          )
        )
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("exclamation-triangle"), " Before Batch Correction - By Dataset"), 
        width = 6, status = "warning", solidHeader = TRUE,
        plotOutput("pca_before_dataset", height = "400px")
      ),
      box(
        title = tags$span(icon("check-circle"), " After Batch Correction - By Dataset"), 
        width = 6, status = "success", solidHeader = TRUE,
        plotOutput("pca_after_dataset", height = "400px")
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("exclamation-triangle"), " Before Batch Correction - By Condition"), 
        width = 6, status = "warning", solidHeader = TRUE,
        plotOutput("pca_before_condition", height = "400px")
      ),
      box(
        title = tags$span(icon("check-circle"), " After Batch Correction - By Condition"), 
        width = 6, status = "success", solidHeader = TRUE,
        plotOutput("pca_after_condition", height = "400px")
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("sitemap"), " Hierarchical Clustering Heatmap - Before Batch Correction"), 
        width = 12, status = "warning", solidHeader = TRUE,
        tags$div(
          style = "padding: 10px 0;",
          tags$p(
            tags$strong(icon("info-circle"), " Interpretation:"),
            " Before correction, samples cluster by Dataset (Study ID).",
            " The dendrogram branches separate different datasets.",
            style = "color: #495057; font-size: 12px; margin-bottom: 10px; padding: 8px; background: #fff3cd; border-radius: 5px;"
          ),
          plotOutput("hclust_before", height = "500px")
        )
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("sitemap"), " Hierarchical Clustering Heatmap - After Batch Correction"), 
        width = 12, status = "success", solidHeader = TRUE,
        tags$div(
          style = "padding: 10px 0;",
          tags$p(
            tags$strong(icon("info-circle"), " Interpretation:"),
            " After correction, samples cluster by Condition (e.g., Normal vs Disease).",
            " The dendrogram branches separate biological conditions, not datasets.",
            style = "color: #495057; font-size: 12px; margin-bottom: 10px; padding: 8px; background: #d4edda; border-radius: 5px;"
          ),
          plotOutput("hclust_after", height = "500px")
        )
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("chart-pie"), " PVCA (Principal Variance Component Analysis)"), 
        width = 12, status = "info", solidHeader = TRUE,
        tags$div(
          style = "padding: 10px 0;",
          tags$p(
            tags$strong(icon("info-circle"), " PVCA Interpretation:"),
            " Quantifies the proportion of variance explained by Dataset vs Condition.",
            " Before correction: Dataset variance is high. After correction: Dataset variance decreases, Condition variance increases.",
            style = "color: #495057; font-size: 12px; margin-bottom: 15px; padding: 8px; background: #d1ecf1; border-radius: 5px;"
          ),
          fluidRow(
            column(6,
              plotOutput("pvca_before", height = "350px")
            ),
            column(6,
              plotOutput("pvca_after", height = "350px")
            )
          )
        )
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("info-circle"), " Filtering Summary"), 
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        tags$div(
          style = "padding: 15px 0;",
          fluidRow(
            column(4, infoBoxOutput("genes_before_filter", width = 12)),
            column(4, infoBoxOutput("genes_after_filter", width = 12)),
            column(4, infoBoxOutput("variance_cutoff", width = 12))
          )
        )
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("file-alt"), " Batch Correction Summary"), 
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        tags$div(
          id = "batch_summary_panel",
          verbatimTextOutput("batch_log"),
          tags$div(
            class = "step-timer",
            tags$span(class = "label", "Elapsed:"),
            textOutput("batch_timer", inline = TRUE)
          )
        )
      )
    ),
    
    fluidRow(
      box(width = 12, status = "info", solidHeader = FALSE,
          tags$div(class = "next-btn", style = "text-align: center; padding: 20px 0;",
                   actionButton("next_page_batch", "Next: Differential Expression",
                                icon = icon("arrow-right"), class = "btn-success btn-lg",
                                style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;")))
    ),
  )
