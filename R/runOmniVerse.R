#' Run the OmniVerse Shiny application
#'
#' Launches the OmniVerse Shiny app for multi-omics RNA analysis (bulk RNA-seq,
#' microarray, GEO download, QC, normalization, differential expression,
#' WGCNA, pathway enrichment, PPI, machine learning, and more).
#'
#' @param launch.browser If TRUE, open the app in the default browser (default).
#' @param port The TCP port to listen on (0 = random available port).
#' @param host The host to bind (e.g. "0.0.0.0" for external access).
#' @return Invisible. Runs the Shiny app.
#' @export
#' @examples
#' \dontrun{
#' runOmniVerse()
#' runOmniVerse(launch.browser = TRUE, port = 3838)
#' }
runOmniVerse <- function(launch.browser = TRUE, port = getOption("shiny.port", 3838), host = getOption("shiny.host", "127.0.0.1")) {
  app_dir <- system.file("shinyapp", package = "OmniVerse")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("OmniVerse app directory not found. Reinstall the package.")
  }
  options(shiny.launch.browser = launch.browser)
  shiny::runApp(app_dir, host = host, port = port)
}
