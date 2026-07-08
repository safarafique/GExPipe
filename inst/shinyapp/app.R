# ==============================================================================
# APP.R - GExPipe (Gene Expression Pipeline) launcher
# ==============================================================================
# Recommended entry: GExPipe::runGExPipe()
# This file returns a Shiny app object for RStudio "Run App".
# ==============================================================================

if (requireNamespace("GExPipe", quietly = TRUE)) {
  message("GExPipe — launching via installed package (GExPipe::runGExPipe).")
  GExPipe::runGExPipe(launch.browser = FALSE)
} else {
  cat("\n  GExPipe — Gene Expression Pipeline\n  Loading (first-run dependency bootstrap)...\n\n")

  .gexpipe_detect_app_dir <- function() {
    for (i in rev(seq_len(sys.nframe()))) {
      fi <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
      if (!is.null(fi) && nzchar(fi)) {
        d <- dirname(normalizePath(fi, winslash = "/", mustWork = FALSE))
        if (file.exists(file.path(d, "global.R"))) return(d)
      }
    }
    wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    if (file.exists(file.path(wd, "global.R"))) return(wd)
    inst_dir <- system.file("shinyapp", package = "GExPipe")
    if (nzchar(inst_dir) && file.exists(file.path(inst_dir, "global.R"))) return(inst_dir)
    wd
  }

  app_dir <- .gexpipe_detect_app_dir()
  options(gexpipe.app_dir = app_dir)
  old_wd <- getwd()
  setwd(app_dir)
  on.exit(setwd(old_wd), add = TRUE)

  required_files <- c("global.R", "ui.R", "server.R")
  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files) > 0L) {
    stop(
      "Missing required files: ", paste(missing_files, collapse = ", "),
      "\nInstall GExPipe: BiocManager::install('GExPipe'), then GExPipe::runGExPipe()",
      call. = FALSE
    )
  }

  err_global <- tryCatch(source("global.R"), error = function(e) e)
  if (inherits(err_global, "error")) {
    stop("Failed to load global.R: ", conditionMessage(err_global), call. = FALSE)
  }
  err_ui <- tryCatch(source("ui.R"), error = function(e) e)
  if (inherits(err_ui, "error")) {
    stop("Failed to load ui.R: ", conditionMessage(err_ui), call. = FALSE)
  }
  err_server <- tryCatch(source("server.R"), error = function(e) e)
  if (inherits(err_server, "error")) {
    stop("Failed to load server.R: ", conditionMessage(err_server), call. = FALSE)
  }

  cat("  Ready. Starting Shiny app...\n\n")
  shiny::shinyApp(ui = ui, server = server)
}
