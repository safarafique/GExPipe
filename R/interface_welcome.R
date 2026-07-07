## Welcome landing page UI (R/ui_welcome.R).

gexp_ui_welcome <- function() {
  gexp_app_attach_packages()
  get("ui_welcome", envir = asNamespace("GExPipe"), inherits = FALSE)
}
