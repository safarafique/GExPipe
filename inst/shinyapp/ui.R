# Legacy RStudio / runGitHub entry — delegates to the installed package UI.
ui <- function() {
  if (!requireNamespace("GExPipe", quietly = TRUE)) {
    stop("Install/load GExPipe before running the Shiny app.", call. = FALSE)
  }
  getFromNamespace("gexp_app_ui", "GExPipe")()
}
