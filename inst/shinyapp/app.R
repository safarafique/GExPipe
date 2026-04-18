# ==============================================================================
# APP.R - GExPipe (Gene Expression Pipeline) launcher
# ==============================================================================
# Run: use GExPipe::runGExPipe() (recommended) or Run App in RStudio.
# ==============================================================================

cat("\n  GExPipe — Gene Expression Pipeline\n  Loading...\n\n")

# Resolve the app directory robustly across local runs, GitHub checkouts,
# and installed-package launches. Relative source() calls depend on this.
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

# Check for required files
required_files <- c("global.R", "ui.R", "server.R")
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop("Missing required files: ", paste(missing_files, collapse = ", "),
       "\nMake sure global.R, ui.R, and server.R are in the same directory as app.R")
}

err_global <- tryCatch(source("global.R"), error = function(e) e)
if (inherits(err_global, "error")) {
  stop("Failed to load global.R: ", conditionMessage(err_global),
       "\nInstall missing packages with: BiocManager::install(\"GExPipe\") when available, or install the package from source.")
}

err_ui <- tryCatch(source("ui.R"), error = function(e) e)
if (inherits(err_ui, "error")) {
  stop("Failed to load ui.R: ", conditionMessage(err_ui))
}

err_server <- tryCatch(source("server.R"), error = function(e) e)
if (inherits(err_server, "error")) {
  stop("Failed to load server.R: ", conditionMessage(err_server))
}

cat("  ✓ Ready. Starting Shiny app...\n\n")

# Return the Shiny app object (user code will run it)
shinyApp(ui = ui, server = server)
