# ==============================================================================
# APP.R - OmniVerse launcher (R 4.5.2; no additional setup required)
# ==============================================================================
# Run: source("app.R") or Run App in RStudio. Dependencies auto-install on first run.
# ==============================================================================

cat("\n  OmniVerse — Bulk RNA-seq | Microarray | Merged\n  Loading...\n\n")

# Check for required files
required_files <- c("global.R", "ui.R", "server.R")
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop("Missing required files: ", paste(missing_files, collapse = ", "), 
       "\nMake sure global.R, ui.R, and server.R are in the same directory as app.R")
}

source("global.R")
source("ui.R")
source("server.R")
cat("  ✓ Ready. Starting Shiny app...\n\n")

# Force app to open in the default web browser (not RStudio viewer)
options(shiny.launch.browser = TRUE)

# Run the app
shinyApp(ui = ui, server = server)
