#' Run the GExPipe Shiny application
#'
#' Launches the GExPipe Shiny app for multi-omics RNA analysis (bulk RNA-seq,
#' microarray, GEO download, QC, normalization, differential expression,
#' WGCNA, pathway enrichment, PPI, machine learning, and more).
#'
#' @param launch.browser If TRUE, open the app in the default browser (default).
#'   Set to FALSE in Google Colab or headless servers (no local browser).
#' @param port The TCP port to listen on.
#' @param host The host to bind. Use "0.0.0.0" in Google Colab so port forwarding works.
#' @return A Shiny app object. Users should run it with [shiny::runApp()].
#' @export
#' @examples
#' if (interactive()) {
#'   app <- runGExPipe(launch.browser = TRUE, port = 3838)
#'   shiny::runApp(app)
#' }
runGExPipe <- function(launch.browser = TRUE, port = getOption("shiny.port", 3838), host = getOption("shiny.host", "127.0.0.1")) {
  # Bioconductor Shiny guidance: do not launch the app inside the package.
  # This function must return a Shiny app object.
  port <- as.integer(port)
  options(shiny.launch.browser = isTRUE(launch.browser))
  options(shiny.host = host)
  options(shiny.port = port)

  app <- shiny::shinyApp(
    ui = gexp_app_ui(),
    server = gexp_app_server,
    onStart = gexp_app_onStart
  )
  if (isTRUE(launch.browser)) {
    url <- sprintf("http://%s:%s", if (host == "0.0.0.0") "127.0.0.1" else host, port)
    message("GExPipe: app ready at ", url, " (run with runApp(app, host = host, port = port)).")
  }
  app
}
