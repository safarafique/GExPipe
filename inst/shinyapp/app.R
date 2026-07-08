# ==============================================================================
# APP.R - GExPipe (Gene Expression Pipeline) launcher
# ==============================================================================
# Recommended entry: GExPipe::runGExPipe() after load_all / install.
# This file returns a Shiny app object for RStudio "Run App".
# ==============================================================================

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

.gexpipe_is_src_root <- function(dir) {
  desc <- file.path(dir, "DESCRIPTION")
  if (!file.exists(desc)) return(FALSE)
  lines <- tryCatch(readLines(desc, n = 10L, warn = FALSE), error = function(e) character(0))
  any(grepl("^Package:\\s*GExPipe\\s*$", lines))
}

.gexpipe_find_src_root <- function(app_dir) {
  candidates <- unique(c(
    normalizePath(file.path(app_dir, "..", ".."), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = FALSE),
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  ))
  for (d in candidates) {
    if (.gexpipe_is_src_root(d)) return(d)
  }
  NULL
}

app_dir <- .gexpipe_detect_app_dir()
src_root <- .gexpipe_find_src_root(app_dir)

# Prefer local source tree so edits (ID mapping, GEO fixes) apply after restart
# without waiting for a reinstall. Fall back to installed package otherwise.
if (!is.null(src_root) &&
    (requireNamespace("pkgload", quietly = TRUE) || requireNamespace("devtools", quietly = TRUE))) {
  message("GExPipe — loading local source from: ", src_root)
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(src_root, quiet = TRUE, export_all = FALSE)
  } else {
    devtools::load_all(src_root, quiet = TRUE)
  }
  GExPipe::runGExPipe(launch.browser = FALSE)
} else if (requireNamespace("GExPipe", quietly = TRUE)) {
  message(
    "GExPipe — launching via installed package.\n",
    "  Tip: for the latest source fixes use:\n",
    "    setwd('<GExPipe repo>'); pkgload::load_all('.'); shiny::runApp(GExPipe::runGExPipe(launch.browser=FALSE))"
  )
  GExPipe::runGExPipe(launch.browser = FALSE)
} else {
  cat("\n  GExPipe — Gene Expression Pipeline\n  Loading (first-run dependency bootstrap)...\n\n")

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
