test_that("workspace saves settings inputs but never buttons, uploads or table internals", {
  skip_if_not_installed("shiny")
  f <- getFromNamespace(".gexp_workspace_inputs", "GExPipe")
  btn <- structure(3L, class = c("shinyActionButtonValue", "integer"))
  input <- shiny::reactiveValues(
    analysis_type = "parallel", de_method = "limma", logfc_cutoff = 0.5, ml_methods = c("lasso", "rf"),
    run_de = 2L, apply_consensus = 1L, load_uploaded_btn = 1L, any_click = btn,
    upload_workspace_file = data.frame(name = "x.rds", datapath = "p"),
    de_table_rows_selected = 4L, sidebarCollapsed = FALSE
  )
  saved <- f(input)
  expect_setequal(names(saved), c("analysis_type", "de_method", "logfc_cutoff", "ml_methods"))
  expect_identical(saved$ml_methods, c("lasso", "rf"))
})
