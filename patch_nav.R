lines <- readLines("e:/GExPipe/R/observers_navigation.R")
# We want to insert `shinyjs::runjs("window.scrollTo(0, 0);")` immediately after every `shinydashboard::updateTabItems` call.
for (i in seq_along(lines)) {
  if (grepl("shinydashboard::updateTabItems", lines[i]) && !grepl("scrollTo", lines[i])) {
    lines[i] <- paste0(lines[i], "\n    shinyjs::runjs(\"window.scrollTo(0, 0);\")")
  }
}
writeLines(lines, "e:/GExPipe/R/observers_navigation.R")

# Also do it for server_validation.R where next_page_validation_to_roc is
lines_val <- readLines("e:/GExPipe/R/shiny_src/server/server_validation.R")
for (i in seq_along(lines_val)) {
  if (grepl("updateTabItems\\(session, \"sidebar_menu\"", lines_val[i]) && !grepl("scrollTo", lines_val[i])) {
    lines_val[i] <- paste0(lines_val[i], "\n    shinyjs::runjs(\"window.scrollTo(0, 0);\")")
  }
}
writeLines(lines_val, "e:/GExPipe/R/shiny_src/server/server_validation.R")
