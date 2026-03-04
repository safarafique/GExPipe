#' Run the GExPipe Shiny application
#'
#' Launches the GExPipe Shiny app for multi-omics RNA analysis (bulk RNA-seq,
#' microarray, GEO download, QC, normalization, differential expression,
#' WGCNA, pathway enrichment, PPI, machine learning, and more).
#'
#' @param launch.browser If TRUE, open the app in the default browser (default).
#'   Set to FALSE in Google Colab or headless servers (no local browser).
#' @param port The TCP port to listen on (0 = random available port).
#' @param host The host to bind. Use "0.0.0.0" in Google Colab so port forwarding works.
#' @return Invisible. Runs the Shiny app.
#' @export
#' @examples
#' if (interactive()) {
#'   runOmniVerse()
#'   runOmniVerse(launch.browser = TRUE, port = 3838)
#'   # In Google Colab: runOmniVerse(launch.browser = FALSE, host = "0.0.0.0", port = 3838)
#' }
runOmniVerse <- function(launch.browser = TRUE, port = getOption("shiny.port", 3838), host = getOption("shiny.host", "127.0.0.1")) {
  app_dir <- system.file("shinyapp", package = "GExPipe")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("GExPipe app directory not found. Reinstall the package from source or with BiocManager::install(\"GExPipe\") when available.")
  }
  port <- as.integer(port)
  if (port == 0) port <- 3838
  url <- sprintf("http://%s:%s", if (host == "0.0.0.0") "127.0.0.1" else host, port)
  message("GExPipe: open in browser: ", url, " (if it does not open automatically, paste this URL)")
  options(shiny.launch.browser = launch.browser)
  shiny::runApp(app_dir, host = host, port = port)
}
