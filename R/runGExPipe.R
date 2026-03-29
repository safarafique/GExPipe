#' Run the GExPipe Shiny application
#'
#' Launches the GExPipe Shiny app for multi-omics RNA analysis (bulk RNA-seq,
#' microarray, GEO download, QC, normalization, differential expression,
#' WGCNA, pathway enrichment, PPI, machine learning, and more).
#'
#' @param launch.browser If TRUE, open the app in the default browser (default).
#'   Set to FALSE in Google Colab or headless servers (no local browser).
#' @param port TCP port for Shiny. Use a **positive** port (e.g. `3838`) when you will open
#'   the app manually in a browser. **`0`** means "pick a free port" (fine for automated
#'   tests); some setups print `http://127.0.0.1:0`, which **cannot** be pasted into a
#'   browser - then use `launch.browser = TRUE` or `port = 3838`.
#' @param host The host to bind. Use "0.0.0.0" in Google Colab so port forwarding works.
#' @section Startup time:
#' The welcome screen loads first; the full dashboard (15 tabs) is inserted only after
#' **Go to Analysis**, so the first response avoids sourcing every tab UI file.
#' `runGExPipe()` still builds a lazy session UI, and attaching many analysis packages
#' on first open can take **tens of seconds to a few minutes** on a cold session (DLL loads).
#' Prefer `GExPipe::runGExPipe()` from an **installed** package rather than opening
#' `inst/shinyapp/` in RStudio (that path runs `global.R`, which can auto-install
#' packages and is slower).
#'
#' Optional tuning before `runGExPipe()`:
#' * `options(gexpipe.wgcna_threads = 1L)` - fewer WGCNA threads (less parallel setup noise).
#' * `options(gexpipe.wgcna_threads = 0L)` or `FALSE` - skip [WGCNA::enableWGCNAThreads()] at app start.
#' * `options(shiny.testmode = TRUE)` with `options(gexpipe.minimal_attach_in_testmode = TRUE)` (default
#'   when unset) attaches only the Shiny stack first, then the rest after the first flush, so
#'   automated tests can connect quickly. Set `gexpipe.minimal_attach_in_testmode = FALSE` for a
#'   full attach on the first tick (e.g. deep `shinytest2` scenarios).
#' @return A Shiny app object. Users should run it with [shiny::runApp()], which
#'   starts the server and **blocks** the R session until the app is stopped
#'   (e.g. RStudio Stop button).
#' @export
#' @examples
#' # Non-interactive: build an app object (this runs during R CMD check / `example()`).
#' app <- runGExPipe(launch.browser = FALSE, port = 0L)
#' stopifnot(inherits(app, "shiny.appobj"))
#'
#' # Interactive: launch in a browser (blocks until the app is stopped).
#' if (interactive()) {
#'   app <- runGExPipe(launch.browser = TRUE, port = 3838L)
#'   shiny::runApp(app, port = 3838L)
#' }
runGExPipe <- function(launch.browser = TRUE, port = getOption("shiny.port", 3838), host = getOption("shiny.host", "127.0.0.1")) {
  # Bioconductor Shiny guidance: do not launch the app inside the package.
  # This function must return a Shiny app object.
  port <- as.integer(port)
  options(shiny.launch.browser = isTRUE(launch.browser))
  options(shiny.host = host)
  options(shiny.port = port)
  options(gexpipe.attach.done = NULL)
  options(gexpipe.attach.shiny_stack_only_done = NULL)
  options(gexpipe.attach.allow_full_now = NULL)

  if (interactive() && identical(port, 0L) && !isTRUE(launch.browser)) {
    message(
      "GExPipe: port = 0 with launch.browser = FALSE - if the console later shows http://127.0.0.1:0, ",
      "that address will not work in a browser. Prefer launch.browser = TRUE, or port = 3838, ",
      "and pass the same port to shiny::runApp(app, port = ...)."
    )
  }

  if (interactive()) {
    message("GExPipe: app object ready (UI builds on first browser load; first load can take several minutes).")
  }
  app <- shiny::shinyApp(
    ui = function() {
      if (interactive()) {
        message("GExPipe: building Shiny UI (first session)...")
      }
      gexp_app_ui()
    },
    server = gexp_app_server,
    onStart = gexp_app_onStart
  )
  if (interactive()) {
    message("GExPipe: call shiny::runApp(app) to start the server (Listening on http://...).")
  }
  if (isTRUE(launch.browser)) {
    url <- sprintf("http://%s:%s", if (host == "0.0.0.0") "127.0.0.1" else host, port)
    message("GExPipe: app ready at ", url, " (run with runApp(app, host = host, port = port)).")
  }
  app
}
