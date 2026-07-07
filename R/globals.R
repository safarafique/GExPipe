# Suppress R CMD check NOTES for Shiny symbols resolved at app runtime
# (after gexp_app_attach_packages() attaches namespaces).
utils::globalVariables(c(
  ".data", "Outcome", "Predicted_Prob", "Predicted", "Observed", "N", "SE",
  "Factor", "Variance", "Kept", "timeout_seen",
  "renderText", "renderUI", "renderPlot", "renderPrint", "renderTable",
  "renderImage", "renderInfoBox", "renderDataTable", "downloadHandler",
  "observe", "observeEvent", "req", "invalidateLater", "showNotification",
  "removeNotification", "withProgress", "incProgress", "reactive", "eventReactive",
  "validate", "need", "updateRadioButtons", "updateSelectInput", "updateTextInput",
  "updateCheckboxInput", "updateSliderInput", "showModal", "removeModal",
  "infoBox", "valueBox", "ggplot", "aes", "geom_", "theme", "theme_bw",
  "scale_", "labs", "element_text", "element_line", "element_blank",
  "coord_", "ggsave", "print", "plot", "points", "lines", "abline", "arrows",
  "quantile", "write.csv", "na.omit"
))
