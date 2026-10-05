# ==============================================================================
# SERVER_SIGNATURE_VALIDATION.R - signature-level external validation
# ==============================================================================
# Appears in the Validation step once an external cohort has been categorized.
# Scores every validation sample with the biomarker signature (signed mean of
# standardized expression; direction learned on the TRAINING datasets), compares
# the AUC with random gene sets, runs the same procedure leave-one-dataset-out
# across the training datasets, and keeps a log of EVERY validation attempt.
# Helpers: R/gexp_signature_validation.R
# ==============================================================================

server_signature_validation <- function(input, output, session, rv) {

  sv <- reactiveValues(res = NULL, lodo = NULL, sweep = NULL, label = NULL, n_top = NULL)

  .has <- function(x) !is.null(x) && length(x) > 0L

  # Training matrix (genes x samples, per-dataset normalized, before batch correction),
  # outcome (1 = Disease) and dataset label.
  .sv_training <- function() {
    expr <- rv$combined_expr
    md <- rv$unified_metadata
    if (is.null(expr) || is.null(md) || !"Condition" %in% names(md)) return(NULL)
    ids <- intersect(colnames(expr), rownames(md))
    if (length(ids) < 6L) return(NULL)
    cond <- as.character(md[ids, "Condition"])
    keep <- cond %in% c("Normal", "Disease")
    ids <- ids[keep]; cond <- cond[keep]
    ds <- if ("Dataset" %in% names(md)) as.character(md[ids, "Dataset"]) else rep("Training", length(ids))
    list(expr = as.matrix(expr[, ids, drop = FALSE]), y = as.integer(cond == "Disease"), ds = ds)
  }

  output$val_signature_ui <- renderUI({
    if (is.null(rv$external_validation_expr) || is.null(rv$external_validation_outcome)) return(NULL)
    choices <- c("Top genes from the cross-dataset meta-analysis (recommended)" = "meta",
                 "All genes passing the training criteria" = "all")
    if (.has(rv$common_genes_de_wgcna)) choices <- c(choices, "Step 8 common genes (DEG and WGCNA)" = "step8")
    if (.has(rv$ml_common_genes)) choices <- c(choices, "Step 10 machine-learning genes" = "ml")
    if (.has(rv$roc_selected_genes)) choices <- c(choices, "Step 12 ROC-selected genes" = "roc")
    choices <- c(choices, "My own gene list" = "custom")
    fluidRow(
      box(
        title = tags$span(icon("fingerprint"), " Signature validation (recommended)"),
        width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p(style = "font-size: 13px; color: #495057;",
          "Single-gene significance and fold-change cutoffs do not transfer between platforms or tissues (different scales). ",
          "Instead, every validation sample gets a ", tags$strong("signature score"),
          " (average of the signed, standardized expression of the signature genes; the direction of each gene comes from the training datasets). ",
          "Its AUC is compared with random gene sets, and the same procedure is repeated ",
          tags$strong("leave-one-dataset-out"), " across your training datasets."),
        tags$div(class = "alert alert-warning", style = "font-size: 12px; padding: 8px 12px;",
          icon("exclamation-triangle"), tags$strong(" Decide the gene source and N BEFORE looking at the result. "),
          "Every run is added to the log below; report all validation sets you tried, not only the best one."),
        fluidRow(
          column(4, selectInput("sigval_source", "Signature genes:", choices = choices, selected = "meta", width = "100%")),
          column(2, numericInput("sigval_top_n", "Top N genes:", value = 100, min = 5, max = 5000, step = 25)),
          column(2, numericInput("sigval_perm", "Random sets:", value = 1000, min = 200, max = 5000, step = 200)),
          column(2, numericInput("sigval_seed", "Seed:", value = 123, min = 1, step = 1)),
          column(2, tags$div(style = "margin-top: 25px;",
            actionButton("sigval_run", tagList(icon("play"), " Run"), class = "btn-success btn-block")))
        ),
        conditionalPanel("input.sigval_source == 'custom'",
          textAreaInput("sigval_custom", "Genes (gene symbols, comma or space separated):", rows = 2, width = "100%")),
        uiOutput("sigval_results_ui"),
        uiOutput("sigval_history_ui")
      )
    )
  })

  observeEvent(input$sigval_run, {
    tr <- .sv_training()
    if (is.null(tr)) {
      showNotification("Signature validation needs the training data with groups applied (Step 4) and normalization (Step 2).", type = "error", duration = 8)
      return()
    }
    ev <- tryCatch(t(as.matrix(rv$external_validation_expr)), error = function(e) NULL)
    y_val <- suppressWarnings(as.integer(rv$external_validation_outcome))
    if (is.null(ev) || length(y_val) != ncol(ev) || anyNA(y_val) || !all(y_val %in% 0:1)) {
      showNotification("External validation data and outcome labels do not match. Re-run the validation categorization.", type = "error", duration = 8)
      return()
    }
    if (length(unique(y_val)) < 2L) {
      showNotification("The validation cohort has only one outcome class.", type = "error", duration = 8)
      return()
    }
    source <- if (length(input$sigval_source) == 1L) input$sigval_source else "meta"
    top_n <- suppressWarnings(as.integer(input$sigval_top_n)); if (length(top_n) != 1L || is.na(top_n)) top_n <- 100L
    top_n <- max(5L, min(5000L, top_n))
    n_perm <- suppressWarnings(as.integer(input$sigval_perm)); if (length(n_perm) != 1L || is.na(n_perm)) n_perm <- 1000L
    n_perm <- max(200L, min(5000L, n_perm))
    seed <- suppressWarnings(as.integer(input$sigval_seed)); if (length(seed) != 1L || is.na(seed)) seed <- 123L

    out <- tryCatch(withProgress(message = "Signature validation...", value = 0.05, {
      meta <- gexp_sig_meta(tr$expr, tr$y, tr$ds)
      n_valid_ds <- sum(vapply(unique(tr$ds), function(d) { yy <- tr$y[tr$ds == d]; sum(yy == 1L) >= 2L && sum(yy == 0L) >= 2L }, logical(1)))
      min_ds <- min(2L, max(1L, n_valid_ds))
      pool <- meta$Gene[!is.na(meta$FDR) & meta$FDR < 0.05 & meta$Consistent & meta$N_Datasets >= min_ds]
      signs <- stats::setNames(sign(meta$Z), meta$Gene); signs <- signs[is.finite(signs) & signs != 0]
      incProgress(0.1, detail = "Selecting genes on the training datasets...")
      label <- switch(source,
        meta = paste0("Top ", top_n, " meta-analysis genes"), all = "All genes passing training criteria",
        step8 = "Step 8 common genes", ml = "Step 10 ML genes", roc = "Step 12 ROC-selected genes", custom = "Custom gene list")
      genes <- switch(source,
        meta = gexp_sig_select(meta, top_n, min_datasets = min_ds),
        all = gexp_sig_select(meta, 1e6, min_datasets = min_ds),
        step8 = rv$common_genes_de_wgcna, ml = rv$ml_common_genes, roc = rv$roc_selected_genes,
        custom = unique(trimws(unlist(strsplit(as.character(input$sigval_custom), "[,;[:space:]]+")))))
      genes <- intersect(genes[nzchar(genes)], names(signs))
      if (length(genes) < 3L) stop("Fewer than 3 signature genes have a training direction (they must be measured in at least one training dataset with both groups).", call. = FALSE)
      incProgress(0.1, detail = "Scoring the validation cohort...")
      res <- gexp_sig_validate(ev, y_val, genes, signs, pool_genes = pool, n_perm = n_perm, seed = seed)
      sweep <- tryCatch(gexp_sig_sweep(ev, y_val, meta, min_datasets = min_ds), error = function(e) NULL)
      incProgress(0.4, detail = "Leave-one-dataset-out on the training datasets...")
      lodo <- if (n_valid_ds >= 2L) {
        tryCatch(gexp_sig_lodo(tr$expr, tr$y, tr$ds, top_n = if (source %in% c("meta", "all")) (if (source == "all") 1e6 else top_n) else top_n,
                               n_perm = min(n_perm, 500L), seed = seed), error = function(e) NULL)
      } else NULL
      list(res = res, sweep = sweep, lodo = lodo, label = label, n_top = length(genes), n_perm = n_perm, seed = seed, source = source)
    }), error = function(e) { showNotification(paste0("Signature validation failed: ", conditionMessage(e)), type = "error", duration = 10); NULL })
    if (is.null(out)) return()

    sv$res <- out$res; sv$sweep <- out$sweep; sv$lodo <- out$lodo; sv$label <- out$label; sv$n_top <- out$n_top
    r <- out$res
    row <- data.frame(
      Run_Time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      Validation_GSE = if (.has(input$ext_val_gse_ids)) trimws(paste(input$ext_val_gse_ids, collapse = " ")) else NA_character_,
      N_Validation = length(y_val), N_Disease = sum(y_val == 1L), N_Normal = sum(y_val == 0L),
      Signature_Source = out$label, Genes_Used = r$n_used,
      AUC = r$auc, AUC_Lower = r$ci[2L], AUC_Upper = r$ci[3L],
      P_vs_Random_Genes = r$p_a, P_vs_Training_Significant_Genes = r$p_b,
      Same_Direction_Pct = 100 * mean(r$per_gene$Same_Direction, na.rm = TRUE),
      Replicated_Pct = 100 * mean(r$per_gene$Replicated, na.rm = TRUE),
      Random_Sets = out$n_perm, Seed = out$seed, stringsAsFactors = FALSE)
    rv$sigval_history <- rbind(rv$sigval_history, row)
    # save tables and figures to the existing export folder
    dir <- tryCatch(CSV_EXPORT_DIR(), error = function(e) NULL)
    if (!is.null(dir)) {
      try(utils::write.csv(r$per_gene, file.path(dir, "Signature_Validation_Per_Gene.csv"), row.names = FALSE), silent = TRUE)
      if (!is.null(out$sweep)) try(utils::write.csv(out$sweep, file.path(dir, "Signature_Validation_Size_Sweep.csv"), row.names = FALSE), silent = TRUE)
      if (!is.null(out$lodo)) try(utils::write.csv(out$lodo, file.path(dir, "Signature_Leave_One_Dataset_Out.csv"), row.names = FALSE), silent = TRUE)
      try(utils::write.csv(rv$sigval_history, file.path(dir, "Signature_Validation_Attempts_Log.csv"), row.names = FALSE), silent = TRUE)
      for (type in c("png", "pdf")) {
        try({ gexp_plot_device_open(file.path(dir, paste0("Signature_Validation_Null.", type)), 6.5, 5, type = type); tryCatch(.sv_plot_null(), finally = grDevices::dev.off()) }, silent = TRUE)
        if (!is.null(out$lodo)) try({ gexp_plot_device_open(file.path(dir, paste0("Signature_Leave_One_Dataset_Out.", type)), 6.5, 4.2, type = type); tryCatch(.sv_plot_lodo(), finally = grDevices::dev.off()) }, silent = TRUE)
      }
    }
    showNotification(sprintf("Signature validation: AUC %.3f (95%% CI %.3f-%.3f), %d genes.", r$auc, r$ci[2L], r$ci[3L], r$n_used), type = "message", duration = 8)
  })

  .sv_plot_null <- function() {
    req(sv$res)
    gexp_sig_plot_null(sv$res, paste0("Signature validation: ", sv$label))
  }
  .sv_plot_lodo <- function() {
    req(sv$lodo)
    gexp_sig_plot_lodo(sv$lodo)
  }
  output$sigval_null_plot <- renderPlot({ .sv_plot_null() }, height = 380, res = 96)
  output$sigval_lodo_plot <- renderPlot({ .sv_plot_lodo() }, height = 300, res = 96)
  gexp_register_plot_downloads(output, "sigval_null", function() .sv_plot_null(), 6.5, 5, "Signature_Validation_Null", csv_dir = CSV_EXPORT_DIR)
  gexp_register_plot_downloads(output, "sigval_lodo", function() .sv_plot_lodo(), 6.5, 4.2, "Signature_Leave_One_Dataset_Out", csv_dir = CSV_EXPORT_DIR)

  .fmt <- function(df) { for (j in names(df)) if (is.numeric(df[[j]])) df[[j]] <- signif(df[[j]], 4); df }
  output$sigval_pergene_table <- DT::renderDataTable({ req(sv$res); DT::datatable(.fmt(sv$res$per_gene), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) })
  output$sigval_sweep_table <- DT::renderDataTable({ req(sv$sweep); DT::datatable(.fmt(sv$sweep), options = list(dom = "t"), rownames = FALSE) })
  output$sigval_lodo_table <- DT::renderDataTable({ req(sv$lodo); DT::datatable(.fmt(sv$lodo), options = list(dom = "t", scrollX = TRUE), rownames = FALSE) })
  output$sigval_history_table <- DT::renderDataTable({ req(rv$sigval_history); DT::datatable(.fmt(rv$sigval_history), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) })

  .csv_dl <- function(id, name, get) {
    output[[id]] <- downloadHandler(
      filename = function() paste0(name, ".csv"),
      content = function(file) {
        tab <- get(); if (is.null(tab)) tab <- data.frame(Note = "Not available for this run.")
        utils::write.csv(tab, file, row.names = FALSE)
        try(utils::write.csv(tab, file.path(CSV_EXPORT_DIR(), paste0(name, ".csv")), row.names = FALSE), silent = TRUE)
      })
  }
  .csv_dl("download_sigval_pergene", "Signature_Validation_Per_Gene", function() if (is.null(sv$res)) NULL else sv$res$per_gene)
  .csv_dl("download_sigval_sweep", "Signature_Validation_Size_Sweep", function() sv$sweep)
  .csv_dl("download_sigval_lodo", "Signature_Leave_One_Dataset_Out", function() sv$lodo)
  .csv_dl("download_sigval_history", "Signature_Validation_Attempts_Log", function() rv$sigval_history)

  output$sigval_results_ui <- renderUI({
    r <- sv$res
    if (is.null(r)) return(tags$p(style = "color:#6c757d;", icon("info-circle"), " Choose the signature and click Run."))
    same <- 100 * mean(r$per_gene$Same_Direction, na.rm = TRUE); rep_ <- 100 * mean(r$per_gene$Replicated, na.rm = TRUE)
    msgs <- c(
      if (is.finite(r$p_a) && r$p_a < 0.05) "The signature separates Disease from Normal better than random genes."
      else "The signature is not distinguishable from random genes in this cohort.",
      if (is.finite(r$p_b) && r$p_b >= 0.05)
        "It is NOT better than random genes that passed the same training filter: the very strongest training genes may be specific to the training tissue or cohort. Compare the larger signature sizes below.",
      if (same < 60) "Fewer than 60% of the genes keep their training direction, which is typical of a tissue or platform mismatch between training and validation."
    )
    tagList(
      tags$hr(),
      tags$div(class = if (is.finite(r$p_a) && r$p_a < 0.05) "alert alert-success" else "alert alert-warning", style = "font-size: 14px;",
        tags$strong(sprintf("%s: AUC %.3f (95%% CI %.3f-%.3f)", sv$label, r$auc, r$ci[2L], r$ci[3L])),
        sprintf(" | %d genes used (%d not in the validation data) | validation n = %d", r$n_used, r$n_missing, length(r$scores)),
        tags$br(),
        sprintf("Permutation p vs random genes = %s%s | same direction as training: %.1f%% | replicated (direction and FDR < 0.05): %.1f%%",
                format.pval(r$p_a, digits = 2, eps = 1e-3),
                if (is.finite(r$p_b)) paste0(" | vs random training-significant genes = ", format.pval(r$p_b, digits = 2, eps = 1e-3)) else "",
                same, rep_),
        tags$br(), tags$small(paste(msgs, collapse = " "))),
      fluidRow(
        column(6, plotOutput("sigval_null_plot", height = "380px"),
          gexp_ui_plot_download_bar("download_sigval_null_png", "download_sigval_null_jpg", "download_sigval_null_pdf", "btn-success btn-xs")),
        column(6, tags$h5(tags$strong("AUC by signature size (shown openly - fix N in advance)")),
          DT::dataTableOutput("sigval_sweep_table"),
          tags$div(style = "margin-top: 6px;", downloadButton("download_sigval_sweep", tagList(icon("download"), " Size sweep (CSV)"), class = "btn-info btn-xs")))
      ),
      if (!is.null(sv$lodo)) tagList(
        tags$h5(tags$strong("Leave-one-dataset-out across the training datasets"),
                tags$small(" (genes selected on the other datasets, scored in the held-out one)", style = "color:#6c757d;")),
        fluidRow(
          column(6, plotOutput("sigval_lodo_plot", height = "300px"),
            gexp_ui_plot_download_bar("download_sigval_lodo_png", "download_sigval_lodo_jpg", "download_sigval_lodo_pdf", "btn-success btn-xs")),
          column(6, DT::dataTableOutput("sigval_lodo_table"),
            tags$div(style = "margin-top: 6px;", downloadButton("download_sigval_lodo", tagList(icon("download"), " Leave-one-out (CSV)"), class = "btn-info btn-xs")))
        )
      ),
      tags$h5(tags$strong("Per-gene replication")),
      DT::dataTableOutput("sigval_pergene_table"),
      tags$div(style = "margin-top: 6px;", downloadButton("download_sigval_pergene", tagList(icon("download"), " Per-gene table (CSV)"), class = "btn-info btn-xs"))
    )
  })

  output$sigval_history_ui <- renderUI({
    if (is.null(rv$sigval_history) || nrow(rv$sigval_history) == 0L) return(NULL)
    tagList(
      tags$hr(),
      tags$h5(icon("clipboard-list"), tags$strong(" Validation attempts log"),
              tags$small(" - every run is recorded; report all of them", style = "color:#6c757d;")),
      DT::dataTableOutput("sigval_history_table"),
      tags$div(style = "margin-top: 6px;", downloadButton("download_sigval_history", tagList(icon("download"), " Attempts log (CSV)"), class = "btn-info btn-xs"))
    )
  })
}
