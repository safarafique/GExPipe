## UI welcome page (migrated via thin loader).
## We keep the load lazy so we only evaluate Shiny UI code after Shiny
## namespaces are available (Bioconductor checks may run without starting app).

ui_welcome <- NULL

.gexp_load_inst_ui_welcome <- function() {
  p <- .gexp_inst_file("shiny_src/ui/ui_welcome.R")

  env <- new.env(parent = parent.frame())
  source(p, local = env)
  if (!exists("ui_welcome", envir = env, inherits = FALSE)) {
    stop("GExPipe: ui_welcome not found after sourcing R/shiny_src/ui/ui_welcome.R.")
  }
  assign("ui_welcome", env$ui_welcome, envir = parent.frame())
  invisible(TRUE)
}

gexp_ui_welcome <- function() {
  gexp_app_attach_packages()
  if (is.null(ui_welcome)) .gexp_load_inst_ui_welcome()
  ui_welcome
}

