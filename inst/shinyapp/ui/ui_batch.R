# ==============================================================================
# UI_BATCH.R - Step 5: Batch Correction Tab
# ==============================================================================

ui_batch <- tabItem(
    tabName = "batch",
    h2(icon("filter"), " Step 5: Gene Filtering & Batch Correction"),

    # When only one dataset is selected, batch correction is skipped automatically.
    uiOutput("batch_merged_platform_ui"),
    uiOutput("batch_single_dataset_ui"),

    fluidRow(
      box(
        title = tags$span(icon("table"), " Dataset \u00d7 Condition confounding"),
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p(
          "Check that each dataset includes both Normal and Disease samples before interpreting batch correction and DE.",
          style = "font-size: 13px; color: #495057; margin-bottom: 10px;"
        ),
        uiOutput("batch_confounding_ui")
      )
    ),
    
    fluidRow(
      box(title = tags$span(icon("chart-bar"), " Gene Variance Distribution"), 
          width = 12, status = "warning", solidHeader = TRUE,
          plotOutput("gene_variance_plot", height = "300px"),
          tags$div(style = "margin-top: 6px;",
            downloadButton("download_gene_variance_png", tagList(icon("download"), " PNG"), class = "btn-warning btn-sm", style = "margin-right: 4px;"),
            downloadButton("download_gene_variance_jpg", tagList(icon("download"), " JPG"), class = "btn-warning btn-sm", style = "margin-right: 4px;"),
            downloadButton("download_gene_variance_pdf", tagList(icon("download"), " PDF"), class = "btn-warning btn-sm"))
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
                     uiOutput("batch_method_guidance_ui"),
                     tags$div(
                       class = "alert alert-warning",
                       style = "margin: 10px 0 0 0; font-size: 12px; line-height: 1.55;",
                       icon("exclamation-triangle"),
                       tags$strong(" Key note (confounding risk): "),
                       "ComBat-based methods protect ",
                       tags$strong("Condition"),
                       " (and ",
                       tags$strong("Platform"),
                       " when estimable) in the model matrix. If ",
                       tags$strong("Dataset is confounded with Condition"),
                       " (e.g., one dataset is all Disease and another all Normal), batch correction can still remove biological signal. ",
                       "Prefer ",
                       tags$strong("limma removeBatchEffect"),
                       " or ",
                       tags$strong("SVA"),
                       " in that situation."
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
              tags$strong(icon("compass"), " Which method for which analysis?"),
              style = "margin-bottom: 10px; color: #2c3e50; font-size: 14px;"
            ),
            tags$table(
              class = "table table-bordered table-condensed",
              style = "font-size: 12px; background: white; margin-bottom: 10px;",
              tags$thead(
                tags$tr(
                  tags$th("Your setup (Step 1 platform)"),
                  tags$th("Recommended method"),
                  tags$th("Why")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(tags$strong("Microarray"), " — 2+ GEO studies, groups crossed"),
                  tags$td(tags$span(class = "label label-success", "ComBat-ref")),
                  tags$td("Largest study as reference; protects Condition in the model.")
                ),
                tags$tr(
                  tags$td(tags$strong("RNA-seq"), " — 2+ studies, log/TMM normalized"),
                  tags$td(tags$span(class = "label label-success", "ComBat-ref")),
                  tags$td("Standard multi-study correction on expression scale; pair with DESeq2/edgeR batch in DE if using counts.")
                ),
                tags$tr(
                  tags$td(tags$strong("Merged"), " — microarray + RNA-seq"),
                  tags$td(tags$span(class = "label label-primary", "limma")),
                  tags$td("Gentler linear adjustment across technologies; use ", tags$strong("limma"), " DE at Step 6.")
                ),
                tags$tr(
                  tags$td("Studies on very different scales (medians far apart, same platform)"),
                  tags$td(tags$span(class = "label label-info", "Quantile + limma"), " or ", tags$span(class = "label label-info", "Hybrid")),
                  tags$td("Extra quantile step before batch removal (after Step 3 global quantile if needed).")
                ),
                tags$tr(
                  tags$td("Hidden confounders (batch unknown, e.g. processing date)"),
                  tags$td(tags$span(class = "label label-warning", "SVA")),
                  tags$td("Estimates surrogate variables; falls back to ComBat if SVA fails.")
                ),
                tags$tr(
                  tags$td(tags$span(style = "color: #c0392b;", "Dataset confounded with Condition"), " (see table above)"),
                  tags$td(tags$span(class = "label label-warning", "limma"), " or ", tags$span(class = "label label-warning", "SVA")),
                  tags$td(tags$em("Avoid ComBat"), " — can remove disease signal when one GSE is all Normal and another all Disease.")
                ),
                tags$tr(
                  tags$td("Equal-sized batches, well-balanced design"),
                  tags$td(tags$span(class = "label label-default", "ComBat")),
                  tags$td("Classic empirical Bayes; similar to ComBat-ref when batches are balanced.")
                )
              )
            ),
            tags$p(
              tags$strong(icon("info-circle"), " Method summaries:"),
              style = "margin-bottom: 8px; color: #2c3e50; font-size: 13px;"
            ),
            tags$ul(
              style = "margin: 0; padding-left: 20px; color: #495057; font-size: 12px; line-height: 1.7;",
              tags$li(tags$strong("ComBat-ref:"), " ComBat with the largest dataset as reference batch (default for most multi-GSE runs)."),
              tags$li(tags$strong("ComBat:"), " Standard ComBat when batch sizes are similar."),
              tags$li(tags$strong("limma:"), " Fast ", tags$code("removeBatchEffect"), " — preferred for merged platforms or confounded designs."),
              tags$li(tags$strong("SVA:"), " Surrogate variable analysis for unknown batch factors."),
              tags$li(tags$strong("Quantile + limma:"), " Global quantile alignment, then limma batch removal."),
              tags$li(tags$strong("Hybrid:"), " Quantile normalization followed by ComBat.")
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
            tags$li(tags$strong("By Condition:"), " Compare left (before) vs right (after) - biological signal should be clearer after correction"),
            tags$li(tags$strong("By Platform (merged runs):"), " Microarray vs RNA-seq should intermingle after correction; persistent separation indicates residual platform bias")
          )
        )
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("exclamation-triangle"), " Before Batch Correction - By Dataset"), 
        width = 6, status = "warning", solidHeader = TRUE,
        plotOutput("pca_before_dataset", height = "400px"),
        tags$div(style = "margin-top: 6px;", downloadButton("download_pca_before_dataset_png", tagList(icon("download"), " PNG"), class = "btn-sm btn-warning", style = "margin-right: 4px;"), downloadButton("download_pca_before_dataset_jpg", tagList(icon("download"), " JPG"), class = "btn-sm btn-warning", style = "margin-right: 4px;"), downloadButton("download_pca_before_dataset_pdf", tagList(icon("download"), " PDF"), class = "btn-sm btn-warning"))
      ),
      box(
        title = tags$span(icon("check-circle"), " After Batch Correction - By Dataset"), 
        width = 6, status = "success", solidHeader = TRUE,
        plotOutput("pca_after_dataset", height = "400px"),
        tags$div(style = "margin-top: 6px;", downloadButton("download_pca_after_dataset_png", tagList(icon("download"), " PNG"), class = "btn-sm btn-success", style = "margin-right: 4px;"), downloadButton("download_pca_after_dataset_jpg", tagList(icon("download"), " JPG"), class = "btn-sm btn-success", style = "margin-right: 4px;"), downloadButton("download_pca_after_dataset_pdf", tagList(icon("download"), " PDF"), class = "btn-sm btn-success"))
      )
    ),
    
    fluidRow(
      box(
        title = tags$span(icon("exclamation-triangle"), " Before Batch Correction - By Condition"), 
        width = 6, status = "warning", solidHeader = TRUE,
        plotOutput("pca_before_condition", height = "400px"),
        tags$div(style = "margin-top: 6px;", downloadButton("download_pca_before_condition_png", tagList(icon("download"), " PNG"), class = "btn-sm btn-warning", style = "margin-right: 4px;"), downloadButton("download_pca_before_condition_jpg", tagList(icon("download"), " JPG"), class = "btn-sm btn-warning", style = "margin-right: 4px;"), downloadButton("download_pca_before_condition_pdf", tagList(icon("download"), " PDF"), class = "btn-sm btn-warning"))
      ),
      box(
        title = tags$span(icon("check-circle"), " After Batch Correction - By Condition"), 
        width = 6, status = "success", solidHeader = TRUE,
        plotOutput("pca_after_condition", height = "400px"),
        tags$div(style = "margin-top: 6px;", downloadButton("download_pca_after_condition_png", tagList(icon("download"), " PNG"), class = "btn-sm btn-success", style = "margin-right: 4px;"), downloadButton("download_pca_after_condition_jpg", tagList(icon("download"), " JPG"), class = "btn-sm btn-success", style = "margin-right: 4px;"), downloadButton("download_pca_after_condition_pdf", tagList(icon("download"), " PDF"), class = "btn-sm btn-success"))
      )
    ),
    
    fluidRow(
      uiOutput("batch_qc_decision_ui")
    ),
    
    uiOutput("batch_platform_pca_row"),
    
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
          plotOutput("hclust_before", height = "500px"),
          tags$div(style = "margin-top: 6px;", downloadButton("download_hclust_before_png", tagList(icon("download"), " PNG"), class = "btn-sm btn-warning", style = "margin-right: 4px;"), downloadButton("download_hclust_before_jpg", tagList(icon("download"), " JPG"), class = "btn-sm btn-warning", style = "margin-right: 4px;"), downloadButton("download_hclust_before_pdf", tagList(icon("download"), " PDF"), class = "btn-sm btn-warning"))
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
          plotOutput("hclust_after", height = "500px"),
          tags$div(style = "margin-top: 6px;", downloadButton("download_hclust_after_png", tagList(icon("download"), " PNG"), class = "btn-sm btn-success", style = "margin-right: 4px;"), downloadButton("download_hclust_after_jpg", tagList(icon("download"), " JPG"), class = "btn-sm btn-success", style = "margin-right: 4px;"), downloadButton("download_hclust_after_pdf", tagList(icon("download"), " PDF"), class = "btn-sm btn-success"))
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
            " Bars show how much of the top PCA variance is tagged to Dataset, Platform, or Condition.",
            " Before: expect visible Dataset/Platform bars.",
            " After: those should shrink; a large Residual bar is normal (most biology sits outside these factors).",
            style = "color: #495057; font-size: 12px; margin-bottom: 15px; padding: 8px; background: #d1ecf1; border-radius: 5px;"
          ),
          fluidRow(
            column(6,
              plotOutput("pvca_before", height = "350px"),
              tags$div(style = "margin-top: 6px;", downloadButton("download_pvca_before_png", tagList(icon("download"), " PNG"), class = "btn-sm btn-info", style = "margin-right: 4px;"), downloadButton("download_pvca_before_jpg", tagList(icon("download"), " JPG"), class = "btn-sm btn-info", style = "margin-right: 4px;"), downloadButton("download_pvca_before_pdf", tagList(icon("download"), " PDF"), class = "btn-sm btn-info"))
            ),
            column(6,
              plotOutput("pvca_after", height = "350px"),
              tags$div(style = "margin-top: 6px;", downloadButton("download_pvca_after_png", tagList(icon("download"), " PNG"), class = "btn-sm btn-info", style = "margin-right: 4px;"), downloadButton("download_pvca_after_jpg", tagList(icon("download"), " JPG"), class = "btn-sm btn-info", style = "margin-right: 4px;"), downloadButton("download_pvca_after_pdf", tagList(icon("download"), " PDF"), class = "btn-sm btn-info"))
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
      box(
        title = tags$span(icon("file-csv"), " Download expression data (batch step)"),
        width = 12, status = "warning", solidHeader = TRUE,
        tags$p("Export expression before and after batch correction to verify the pipeline in R, Excel, or other tools.", style = "margin-bottom: 12px; color: #555;"),
        fluidRow(
          column(6,
            downloadButton("download_expr_before_batch", tagList(icon("download"), " Before batch (expression CSV)"), class = "btn-warning btn-block")),
          column(6,
            downloadButton("download_expr_after_batch", tagList(icon("download"), " After batch (expression CSV)"), class = "btn-success btn-block"))
        )
      )
    ),
    fluidRow(
      box(
        title = tags$span(icon("file-alt"), " Process Summary"),
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        uiOutput("batch_process_summary_ui"))
    ),
    fluidRow(
      box(width = 12, status = "info", solidHeader = FALSE,
          tags$div(class = "next-btn", style = "text-align: center; padding: 20px 0;",
                   actionButton("next_page_batch", "Next: Differential Expression",
                                icon = icon("arrow-right"), class = "btn-success btn-lg",
                                style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;"))))
    ),
  )
