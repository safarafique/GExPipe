def edit(path, pairs, append=None):
    s = open(path, encoding="utf-8").read()
    for old, new in pairs:
        assert old in s, "NOT FOUND in %s: %s" % (path, old[:80])
        s = s.replace(old, new, 1)
    if append:
        s = s.rstrip("\n") + "\n" + append
    open(path, "w", encoding="utf-8").write(s)

# ---------------------------------------------------------------- 1. pure helper (testable)
helper = '''
#' Gene funnel: how many genes survive at each pipeline step
#'
#' @param n Named list of counts (NULL / NA = step not run yet): de_rna, de_micro, de_merged,
#'   consensus, common, ml, roc_tested, roc_pass, roc_selected, nomogram.
#' @param parallel Whether the run is in Parallel mode.
#' @return data.frame(Step, Genes, Status, Hint); Status is "not run", "ok" or "STOPS HERE"
#'   (the first step that reached zero genes).
#' @noRd
gexp_gene_funnel_table <- function(n, parallel = FALSE) {
  val <- function(k) { v <- n[[k]]; if (is.null(v) || length(v) != 1L || is.na(v)) NA_integer_ else as.integer(v) }
  steps <- list(
    list("Step 6 - significant genes, RNA-seq", "de_rna", parallel,
         "No gene passes the Step 6 cutoffs. Lower the LogFC cutoff and keep adjusted P at 0.05; check that the groups are Normal vs Disease and that the DE method suits the data."),
    list("Step 6 - significant genes, microarray", "de_micro", parallel,
         "No gene passes the Step 6 cutoffs. Microarray fold changes are small, so try a LogFC cutoff of 0.2-0.3; check the groups."),
    list("Step 6 - significant genes (merged)", "de_merged", !parallel,
         "No gene passes the Step 6 cutoffs. Lower the LogFC cutoff; check the groups and that batch correction did not remove the disease signal."),
    list("Step 7 - consensus of both platforms", "consensus", parallel,
         "No gene is significant on both platforms with the same direction. Relax the Step 6 cutoffs, turn off 'same direction', or switch to Union (exploratory)."),
    list("Step 8 - common genes (DEG and WGCNA)", "common", TRUE,
         "No overlap between the significant genes and the WGCNA modules. Relax the DE cutoffs or pick other WGCNA modules."),
    list("Step 10 - machine-learning genes", "ml", TRUE,
         "The ML step kept no gene. Check that Step 8 produced genes and that each group has enough samples."),
    list("Step 12 - genes with training AUC >= 0.8", "roc_pass", TRUE,
         "No gene reaches AUC 0.8 on the training data (fixed filter). The effect may be weak, or groups/batches may be mixed; recheck Steps 4-6."),
    list("Step 12 - genes you selected", "roc_selected", TRUE,
         "Select genes in Step 12 (ROC) to carry into the nomogram."),
    list("Step 14 - nomogram predictors", "nomogram", TRUE,
         "The nomogram needs at least 10 Normal and 10 Disease samples and trims the panel to what the sample size supports.")
  )
  rows <- Filter(function(s) isTRUE(s[[3L]]), steps)
  out <- data.frame(Step = vapply(rows, `[[`, character(1), 1L), Genes = vapply(rows, function(s) val(s[[2L]]), integer(1)),
                    Status = NA_character_, Hint = "", stringsAsFactors = FALSE)
  stopped <- FALSE
  for (i in seq_len(nrow(out))) {
    g <- out$Genes[i]
    if (is.na(g)) { out$Status[i] <- "not run"; next }
    if (g == 0L && !stopped) { out$Status[i] <- "STOPS HERE"; out$Hint[i] <- rows[[i]][[4L]]; stopped <- TRUE }
    else if (g == 0L) out$Status[i] <- "empty"
    else out$Status[i] <- "ok"
  }
  out
}
'''
edit("R/gexpipe_shiny_helpers.R", [], append=helper)

# ---------------------------------------------------------------- 2. ROC step stores its counts
edit("R/server_roc.R", [
('''  output$roc_filter_message_ui <- renderUI({''',
 '''  # Counts for the Results Summary "gene funnel"
  observe({
    roc <- tryCatch(roc_results(), error = function(e) NULL)
    rv$roc_n_tested <- if (is.null(roc)) NULL else nrow(roc$df_all)
    rv$roc_n_pass <- if (is.null(roc)) NULL else nrow(roc$df)
  })

  output$roc_filter_message_ui <- renderUI({'''),
])

# ---------------------------------------------------------------- 3. rv defaults
edit("R/server_app.R", [
('''    sigval_history = NULL,\n''', '''    sigval_history = NULL,\n    roc_n_tested = NULL,\n    roc_n_pass = NULL,\n'''),
])

# ---------------------------------------------------------------- 4. summary server + UI
edit("R/server_results_summary.R", [
('''server_results_summary <- function(input, output, session, rv) {
''',
 '''server_results_summary <- function(input, output, session, rv) {

  # ---- Gene funnel: where do genes drop out of the pipeline? ----
  output$results_summary_gene_funnel <- renderUI({
    nrow_or_na <- function(x) if (is.data.frame(x)) nrow(x) else NA_integer_
    len_or_na <- function(x) if (is.null(x)) NA_integer_ else length(x)
    parallel <- isTRUE(rv$merge_after_de) || identical(rv$analysis_type, "parallel")
    counts <- list(
      de_rna = nrow_or_na(rv$sig_genes_rna), de_micro = nrow_or_na(rv$sig_genes_micro),
      de_merged = if (isTRUE(rv$consensus_complete)) NA_integer_ else nrow_or_na(rv$sig_genes),
      consensus = if (isTRUE(rv$consensus_complete)) nrow_or_na(rv$sig_genes) else NA_integer_,
      common = len_or_na(rv$common_genes_de_wgcna), ml = len_or_na(rv$ml_common_genes),
      roc_pass = if (is.null(rv$roc_n_pass)) NA_integer_ else rv$roc_n_pass,
      roc_selected = len_or_na(rv$roc_selected_genes), nomogram = len_or_na(rv$nomogram_available_genes))
    tab <- gexp_gene_funnel_table(counts, parallel = parallel)
    if (all(tab$Status == "not run")) {
      return(tags$p(style = "color:#7f8c8d;", icon("info-circle"), " Run the analysis steps to see how many genes survive each one."))
    }
    stop_row <- which(tab$Status == "STOPS HERE")[1L]
    col <- c(ok = "#27ae60", `not run` = "#95a5a6", empty = "#e67e22", `STOPS HERE` = "#c0392b")
    tagList(
      if (!is.na(stop_row)) tags$div(class = "alert alert-danger", style = "font-size: 14px;",
        icon("exclamation-triangle"), tags$strong(" No biomarker can be identified because genes drop to zero at: "), tab$Step[stop_row], ".",
        tags$br(), tab$Hint[stop_row]),
      tags$table(class = "table table-condensed", style = "font-size: 13px;",
        tags$thead(tags$tr(tags$th("Step"), tags$th("Genes"), tags$th("Status"))),
        tags$tbody(lapply(seq_len(nrow(tab)), function(i) tags$tr(
          tags$td(tab$Step[i]), tags$td(tags$strong(if (is.na(tab$Genes[i])) "-" else format(tab$Genes[i], big.mark = ","))),
          tags$td(tags$span(style = paste0("color:", col[[tab$Status[i]]], "; font-weight:600;"), tab$Status[i]))))))
    )
  })
'''),
])
edit("R/ui_results_summary.R", [
('''  # ----- 2. Normalization & batch -----
  step_arrow(),''',
 '''  # ----- 1b. Gene funnel -----
  box(
    width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
    title = tags$span(icon("filter"), " Where did my genes go? (gene funnel)"),
    uiOutput("results_summary_gene_funnel")
  ),

  # ----- 2. Normalization & batch -----
  step_arrow(),'''),
])
print("funnel edits applied")
