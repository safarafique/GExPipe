# Suppress R CMD check NOTES for NSE column names and Shiny/ggplot symbols.
# Shiny attach happens at app runtime via gexp_app_attach_packages().
utils::globalVariables(c(
  # NSE / ggplot aesthetic columns
  ".data", "AUC", "After", "Before", "Condition", "Connectivity", "Correlation",
  "Count", "Dataset", "Description", "Direction", "Disease", "Expression",
  "Factor", "GS_trait", "Gene", "Group", "IsOutlier", "Kept", "Label",
  "ME1", "ME2", "MM", "Mean", "Module", "N", "Normal", "Observed", "Outcome",
  "PC1", "PC2", "Platform", "Predicted", "Predicted_Prob", "SYMBOL", "SE",
  "Sample", "Sample1", "Sample2", "Significance", "Significant", "Size",
  "Source", "Stage", "Variance", "degree", "fill_val", "hub_status",
  "logFC", "neg_log10_padj", "nodes", "p.adjust", "plogis", "r", "theta",
  "timeout_seen", "x", "y", "ymax", "ymin",
  # Shiny / ggplot helpers resolved after attach
  "renderText", "renderUI", "renderPlot", "renderPrint", "renderTable",
  "renderImage", "renderInfoBox", "renderDataTable", "downloadHandler",
  "observe", "observeEvent", "req", "invalidateLater", "showNotification",
  "removeNotification", "withProgress", "incProgress", "reactive", "eventReactive",
  "reactiveVal", "reactiveValues", "validate", "need",
  "updateRadioButtons", "updateSelectInput", "updateTextInput",
  "updateCheckboxInput", "updateCheckboxGroupInput", "updateSliderInput",
  "updateNumericInput", "updateActionButton", "updateTabItems",
  "selectizeInput", "showModal", "removeModal", "infoBox", "valueBox",
  "ggplot", "aes", "theme", "theme_bw", "labs", "ggsave",
  "element_text", "element_line", "element_blank", "element_rect", "margin",
  "geom_abline", "geom_bar", "geom_boxplot", "geom_density", "geom_hline",
  "geom_line", "geom_point", "geom_rect", "geom_smooth", "geom_tile",
  "scale_color_manual", "scale_fill_gradient2", "scale_fill_identity",
  "scale_fill_manual", "scale_x_continuous", "scale_x_log10",
  "scale_y_continuous", "scale_y_log10", "coord_polar", "facet_wrap", "ylim",
  "print", "plot", "points", "lines", "abline", "arrows", "quantile",
  "write.csv", "na.omit", "%>%", "+<-"
))
