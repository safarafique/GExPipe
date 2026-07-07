# Reusable plot download UI helpers (sourced by ui.R and package tab loaders).

gexp_ui_plot_download_jpg_pdf <- function(jpg_id, pdf_id, btn_class = "btn-success btn-sm") {
  tags$div(
    class = "gexp-plot-download-bar",
    style = "margin-top: 8px;",
    downloadButton(jpg_id, tagList(icon("download"), " JPG (300 DPI)"), class = btn_class, style = "margin-right: 6px;"),
    downloadButton(pdf_id, tagList(icon("download"), " PDF"), class = btn_class)
  )
}

gexp_ui_plot_download_bar <- function(png_id, jpg_id, pdf_id, btn_class = "btn-success btn-sm") {
  tags$div(
    class = "gexp-plot-download-bar",
    style = "margin-top: 8px;",
    downloadButton(png_id, tagList(icon("download"), " PNG (300 DPI)"), class = btn_class, style = "margin-right: 6px;"),
    downloadButton(jpg_id, tagList(icon("download"), " JPG (300 DPI)"), class = btn_class, style = "margin-right: 6px;"),
    downloadButton(pdf_id, tagList(icon("download"), " PDF"), class = btn_class)
  )
}
