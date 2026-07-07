# Legacy RStudio / runGitHub entry — delegates to the installed package server.
server <- function(input, output, session) {
  if (!requireNamespace("GExPipe", quietly = TRUE)) {
    stop("Install/load GExPipe before running the Shiny app.", call. = FALSE)
  }
  getFromNamespace("gexp_app_server", "GExPipe")(input, output, session)
}
