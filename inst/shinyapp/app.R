# ==============================================================================
# APP.R - GExPipe (Gene Expression Pipeline) launcher
# ==============================================================================
# Run: source("app.R") or Run App in RStudio. Dependencies auto-install on first run.
# ==============================================================================

cat("\n  GExPipe — Gene Expression Pipeline\n  Loading...\n\n")

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

# Run the app and open in your default browser (browser interface, not RStudio Viewer)
app <- shinyApp(ui = ui, server = server)
runApp(app, launch.browser = TRUE)
