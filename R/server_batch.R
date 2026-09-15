# ==============================================================================
# SERVER_BATCH.R - Step 5: Batch Correction Module
# ==============================================================================

server_batch <- function(input, output, session, rv) {

  output$batch_log_micro <- renderText({
    if (!is.null(rv$batch_log_micro) && nzchar(rv$batch_log_micro)) rv$batch_log_micro
    else "Apply batch correction to see the microarray run log."
  })
  output$batch_log_rna <- renderText({
    if (!is.null(rv$batch_log_rna) && nzchar(rv$batch_log_rna)) rv$batch_log_rna
    else "Apply batch correction to see the RNA-seq run log."
  })

  output$batch_merged_platform_ui <- renderUI({
    if (isTRUE(rv$merge_after_de) || identical(input$analysis_type, "parallel")) {
      return(NULL)
    }
    if (is.null(rv$unified_metadata) || !gexpipe_has_mixed_platforms(rv$unified_metadata)) {
      return(NULL)
    }
    info <- gexpipe_batch_covariate_info(rv$unified_metadata)
    tags$div(
      class = "alert alert-info",
      style = "margin-top: 10px; font-size: 13px; line-height: 1.6;",
      icon("dna"),
      tags$strong(" Mixed microarray + RNA-seq integration"),
      tags$p(
        "Merged (Both): one batch correction on the combined matrix, then one limma DE — same as the previous app.",
        style = "margin: 8px 0;"
      ),
      tags$ul(
        style = "margin-bottom: 8px; padding-left: 20px;",
        tags$li(tags$strong("DE:"),
                if (isTRUE(rv$merge_after_de)) {
                  " microarray = limma on the array matrix; RNA-seq = your Step 1 method on RNA-seq only."
                } else {
                  tagList(" use ", tags$strong("limma"), " (recommended for merged data).")
                }),
        tags$li(tags$strong("Batch / DE models:"), " ",
                if (info$include_platform_covariate) {
                  "Platform is included as an explicit covariate (not confounded with Dataset)."
                } else {
                  "Platform is confounded with Dataset (e.g. one microarray GSE + one RNA-seq GSE) - the Dataset term absorbs the platform effect."
                }),
        tags$li(tags$strong("Check:"), " review the ", tags$strong("Platform PCA"), " plots below - samples should intermingle by platform after batch correction.")
      )
    )
  })

  output$batch_single_dataset_ui <- renderUI({
    if (isTRUE(rv$single_dataset)) {
      return(
        tags$div(
          class = "alert alert-info",
          style = "margin-top: 10px;",
          icon("info-circle"),
          tags$strong(" Single dataset detected: "),
          "Batch correction will be skipped automatically. You can proceed to Results."
        )
      )
    }
    NULL
  })

  output$batch_confounding_ui <- renderUI({
    if (is.null(rv$unified_metadata)) {
      return(tags$p(class = "text-muted", "Assign sample groups in Step 4 first."))
    }
    sm <- gexpipe_batch_confounding_summary(rv$unified_metadata)
    if (is.null(sm$table)) {
      return(tags$div(class = "alert alert-info", style = "margin: 0;", sm$message))
    }
    status <- if (isTRUE(sm$confounded)) "warning" else "success"
    icon_nm <- if (isTRUE(sm$confounded)) "exclamation-triangle" else "check-circle"
    tags$div(
      class = paste0("alert alert-", status),
      style = "margin: 0;",
      tags$p(style = "margin: 0 0 10px 0;",
             icon(icon_nm), " ", tags$strong(sm$message)),
      tags$table(
        class = "table table-bordered table-condensed",
        style = "max-width: 480px; background: #fff; color: #000; margin-bottom: 0;",
        tags$thead(tags$tr(
          tags$th(style = "color: #000 !important; background: #f8f9fa;", "Dataset"),
          lapply(colnames(sm$table), function(col) {
            tags$th(style = "color: #000 !important; background: #f8f9fa;", col)
          })
        )),
        tags$tbody(lapply(rownames(sm$table), function(rn) {
          tags$tr(
            tags$td(style = "color: #000 !important;", tags$strong(rn)),
            lapply(seq_len(ncol(sm$table)), function(j) {
              tags$td(style = "color: #000 !important;", as.character(sm$table[rn, j]))
            })
          )
        }))
      )
    )
  })

  output$batch_parallel_guide_ui <- renderUI({
    de_rna <- if (!is.null(input$de_method_rna) && nzchar(input$de_method_rna)) {
      input$de_method_rna
    } else {
      "deseq2"
    }
    confounded <- FALSE
    if (!is.null(rv$unified_metadata)) {
      sm <- gexpipe_batch_confounding_summary(rv$unified_metadata)
      confounded <- isTRUE(sm$confounded)
    }
    defs <- gexpipe_parallel_batch_defaults(de_rna, confounded)
    de_lab <- switch(
      de_rna,
      deseq2 = "DESeq2",
      edger = "edgeR",
      limma_voom = "limma-voom",
      limma = "limma",
      de_rna
    )
    rna_lab <- if (identical(defs$rna, "limma")) {
      "limma removeBatchEffect (protects Condition; count DE still uses raw counts + Dataset at Step 6)"
    } else {
      "ComBat-ref on the TMM / log matrix (RNA DE is limma)"
    }
    mode <- if (is.null(input$batch_mode_parallel)) "auto" else input$batch_mode_parallel
    if (identical(mode, "manual")) {
      tags$div(
        class = "alert alert-warning",
        style = "margin: 8px 0 10px 0; font-size: 13px; line-height: 1.55;",
        tags$strong("Manual — pick each platform so DE stays valid."),
        tags$ul(
          style = "margin: 6px 0 0 0; padding-left: 18px;",
          tags$li(tags$strong("Microarray:"), " ComBat-ref if 2+ GSEs and groups are crossed. Use limma/SVA if Dataset is confounded with Condition. Quantile+limma or Hybrid if study medians are far apart."),
          tags$li(tags$strong("RNA-seq + DESeq2/edgeR/voom:"), " prefer limma here. Do not ComBat raw counts; those engines keep counts and add batch at DE."),
          tags$li(tags$strong("RNA-seq + limma:"), " ComBat-ref on TMM log-CPM."),
          tags$li(tags$strong("One GSE on a side:"), " that side is variance-filtered only.")
        )
      )
    } else {
      tags$div(
        class = "alert alert-info",
        style = "margin: 8px 0 10px 0; font-size: 13px; line-height: 1.55;",
        icon("magic"),
        tags$strong(" Auto (recommended). "),
        "One Apply runs both platforms with different methods. No joint ComBat.",
        tags$ul(
          style = "margin: 6px 0 0 0; padding-left: 18px;",
          tags$li(tags$strong("Microarray: "), "ComBat-ref."),
          tags$li(tags$strong("RNA-seq (", de_lab, "): "), rna_lab),
          if (isTRUE(confounded)) {
            tags$li(tags$strong("Confounding detected: "), "both sides use limma so disease signal is not absorbed.")
          } else {
            NULL
          }
        )
      )
    }
  })

  output$batch_method_guidance_ui <- renderUI({
    at <- if (!is.null(input$analysis_type)) input$analysis_type else "microarray"
    at_label <- switch(
      at,
      rnaseq = "RNA-seq only",
      microarray = "Microarray only",
      merged = "Merged (microarray + RNA-seq)",
      at
    )
    mixed <- !is.null(rv$unified_metadata) && gexpipe_has_mixed_platforms(rv$unified_metadata)
    n_ds <- if (!is.null(rv$unified_metadata)) {
      length(unique(rv$unified_metadata$Dataset))
    } else {
      NA_integer_
    }
    confounded <- FALSE
    conf_msg <- NULL
    if (!is.null(rv$unified_metadata)) {
      sm <- gexpipe_batch_confounding_summary(rv$unified_metadata)
      confounded <- isTRUE(sm$confounded)
      conf_msg <- sm$message
    }

    if (confounded) {
      rec_method <- "limma"
      rec_label <- "limma removeBatchEffect"
      rec_why <- paste0(
        "Dataset and Condition are confounded. ComBat-ref/ComBat/Hybrid can absorb disease signal. ",
        "Use limma (or SVA if you suspect hidden batch factors)."
      )
      status <- "warning"
    } else if (isTRUE(rv$merge_after_de) || identical(at, "parallel")) {
      rec_method <- "combat_ref"
      rec_label <- "ComBat-ref (per platform)"
      rec_why <- paste0(
        "Parallel DE: this method is applied inside RNA-seq and inside microarray separately. ",
        "A platform with only one GSE is variance-filtered only. No joint RNA+array ComBat."
      )
      status <- "success"
    } else if (isTRUE(mixed) || identical(at, "merged")) {
      rec_method <- "limma"
      rec_label <- "limma removeBatchEffect"
      rec_why <- paste0(
        "Merged (Both): one correction on the combined matrix. ",
        "limma is conservative for mixed technologies. Then Step 6 runs one limma DE."
      )
      status <- "info"
    } else if (identical(at, "rnaseq")) {
      rec_method <- "combat_ref"
      rec_label <- "ComBat-ref (Recommended)"
      rec_why <- "Multiple RNA-seq GSEs on a common gene set: reference-batch ComBat with Condition protected in the model."
      status <- "success"
    } else {
      rec_method <- "combat_ref"
      rec_label <- "ComBat-ref (Recommended)"
      rec_why <- "Multiple microarray GSEs (e.g. different median levels between studies): ComBat-ref aligns batches while keeping the largest study as reference."
      status <- "success"
    }

    alt_note <- if (!confounded && !mixed && identical(at, "microarray") && !is.na(n_ds) && n_ds >= 2L) {
      tags$p(
        style = "margin: 8px 0 0 0; font-size: 12px; color: #555;",
        tags$strong("If medians stay far apart after Step 3:"),
        " enable global quantile in Step 3, or try ",
        tags$strong("Quantile + limma"),
        " / ",
        tags$strong("Hybrid"),
        " here."
      )
    } else {
      NULL
    }

    selected <- if (!is.null(input$batch_method)) input$batch_method else "combat_ref"
    match_note <- if (!identical(selected, rec_method)) {
      tags$p(
        style = "margin: 8px 0 0 0; font-size: 12px; color: #856404;",
        icon("info-circle"),
        " You selected ",
        tags$strong(selected),
        "; recommended for this run is ",
        tags$strong(rec_method),
        "."
      )
    } else {
      tags$p(
        style = "margin: 8px 0 0 0; font-size: 12px; color: #155724;",
        icon("check-circle"),
        " Your current selection matches the recommendation."
      )
    }

    tags$div(
      class = paste0("alert alert-", status),
      style = "margin: 12px 0 0 0; font-size: 13px; line-height: 1.55;",
      tags$p(
        style = "margin: 0 0 6px 0;",
        icon("lightbulb"),
        tags$strong(" Guidance for this run"),
        if (!is.na(n_ds)) tags$span(paste0(" (", n_ds, " dataset(s), ", at_label, ")"))
      ),
      tags$p(
        style = "margin: 0 0 6px 0;",
        tags$strong("Recommended: "),
        tags$span(class = "label label-success", style = "font-size: 12px;", rec_label)
      ),
      tags$p(style = "margin: 0;", rec_why),
      if (confounded && !is.null(conf_msg)) {
        tags$p(style = "margin: 8px 0 0 0; font-size: 12px;", icon("exclamation-triangle"), " ", conf_msg)
      },
      alt_note,
      match_note
    )
  })

  output$batch_qc_decision_ui <- renderUI({
    confounded <- FALSE
    conf_msg <- "Assign Normal/Disease in Step 4 to assess confounding."
    if (!is.null(rv$unified_metadata)) {
      sm <- gexpipe_batch_confounding_summary(rv$unified_metadata)
      confounded <- isTRUE(sm$confounded)
      if (!is.null(sm$message)) conf_msg <- sm$message
    }

    checklist <- tags$div(
      style = "padding: 12px 15px; background: #f8f9fa; border-radius: 6px; margin-bottom: 12px;",
      tags$p(
        tags$strong(icon("clipboard-check"), " QC checklist (compare PCA plots above)"),
        style = "margin: 0 0 8px 0; font-size: 14px;"
      ),
      tags$ul(
        style = "margin: 0; padding-left: 20px; font-size: 12px; line-height: 1.75; color: #333;",
        tags$li(tags$strong("By Dataset (after):"), " GSE/study colors should ", tags$strong("mix"), " - not separate clusters."),
        tags$li(tags$strong("By Condition (after):"), " Normal vs Disease should be ", tags$strong("separated"), " and clearer than before."),
        tags$li(tags$strong("Hclust (after):"), " branches should follow ", tags$strong("Condition"), " more than Dataset."),
        tags$li(tags$strong("PVCA (after):"), " Dataset variance should ", tags$strong("drop"), "; Condition variance should rise."),
        tags$li(tags$strong("Confounding table:"), " each dataset needs both Normal and Disease.")
      )
    )

    if (!isTRUE(rv$batch_complete) && !isTRUE(rv$single_dataset)) {
      return(
        column(
          12,
          box(
            title = tags$span(icon("search"), " Is batch correction OK? - check after you apply"),
            width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
            tags$p(
              "Apply batch correction, then use this panel and the PCA plots to decide whether to proceed or re-run with another method.",
              style = "font-size: 13px; color: #495057; margin-bottom: 10px;"
            ),
            checklist
          )
        )
      )
    }

    if (isTRUE(rv$single_dataset)) {
      return(
        column(
          12,
          box(
            title = tags$span(icon("check-circle"), " Batch correction - not required"),
            width = 12, status = "success", solidHeader = TRUE,
            tags$div(
              class = "alert alert-success",
              style = "margin: 0; font-size: 13px;",
              icon("check-circle"),
              tags$strong(" Proceed to Step 6."),
              " Only one dataset was selected - between-study batch correction was skipped."
            )
          )
        )
      )
    }

    method <- if (!is.null(input$batch_method)) input$batch_method else "combat_ref"
    combat_family <- method %in% c("combat", "combat_ref", "hybrid", "quantile_limma")

    if (confounded && combat_family) {
      verdict <- "modify"
      status <- "warning"
      title_icon <- "exclamation-triangle"
      headline <- "Consider re-running batch correction"
      body <- paste0(
        "Batch correction finished, but Dataset and Condition may be confounded. ",
        "You used a ComBat-family method (", method, "), which can remove biological signal in this design. ",
        "Switch to ", tags$strong("limma removeBatchEffect"), " or ", tags$strong("SVA"), ", then click Apply Batch Correction again."
      )
    } else if (confounded) {
      verdict <- "caution"
      status <- "warning"
      title_icon <- "exclamation-triangle"
      headline <- "Proceed with caution"
      body <- paste0(
        "Confounding was detected, but you used ", tags$strong(method), " (not ComBat). ",
        "Review Condition PCA and PVCA; if disease signal looks weak, try ", tags$strong("limma"), " or ", tags$strong("SVA"), "."
      )
    } else {
      verdict <- "proceed"
      status <- "success"
      title_icon <- "check-circle"
      headline <- "Batch correction looks OK - proceed to Step 6"
      body <- paste0(
        "If your PCA plots match the checklist (datasets mixed, conditions separated), ",
        "you do ", tags$strong("not"), " need to change Step 5. Continue to ",
        tags$strong("Differential Expression"), "."
      )
    }

    column(
      12,
      box(
        title = tags$span(icon(title_icon), " Is batch correction OK?"),
        width = 12,
        status = status,
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,
        checklist,
        tags$div(
          class = paste0("alert alert-", status),
          style = "margin: 0; font-size: 13px; line-height: 1.6;",
          icon(title_icon),
          tags$strong(" ", headline),
          tags$p(style = "margin: 8px 0 0 0;", body),
          if (confounded) {
            tags$p(style = "margin: 8px 0 0 0; font-size: 12px;", icon("table"), " ", conf_msg)
          },
          if (identical(verdict, "proceed")) {
            tags$p(
              style = "margin: 10px 0 0 0;",
              tags$span(class = "label label-success", style = "font-size: 12px;",
                        "Next: Step 6 - Differential Expression")
            )
          } else {
            tags$p(
              style = "margin: 10px 0 0 0;",
              tags$span(class = "label label-warning", style = "font-size: 12px;",
                        "Action: change method above and re-apply batch correction")
            )
          }
        )
      )
    )
  })

  # Auto-skip batch correction when there is only one dataset.
  observe({
    if (!isTRUE(rv$single_dataset)) return()
    if (!isTRUE(rv$groups_applied)) return()
    if (isTRUE(rv$batch_complete)) return()

    base_expr <- NULL
    if (!is.null(rv$combined_expr) && (is.matrix(rv$combined_expr) || is.data.frame(rv$combined_expr)) && nrow(rv$combined_expr) > 0) {
      base_expr <- rv$combined_expr
    } else if (!is.null(rv$combined_expr_raw) && (is.matrix(rv$combined_expr_raw) || is.data.frame(rv$combined_expr_raw)) && nrow(rv$combined_expr_raw) > 0) {
      base_expr <- rv$combined_expr_raw
    }
    req(base_expr)

    rv$expr_filtered <- base_expr
    rv$batch_corrected <- base_expr
    rv$batch_corrected_rna <- rv$expr_rna
    rv$batch_corrected_micro <- rv$expr_micro
    rv$batch_complete <- TRUE
    rv$batch_running <- FALSE

    output$batch_log <- renderText({
      paste0(
        "OK Batch correction skipped (single dataset)\n\n",
        "Reason: Only 1 dataset was selected, so there is no between-study batch to remove.\n",
        "Downstream steps will use the current expression matrix.\n"
      )
    })
  })

  output$batch_timer <- renderText({
    if (!isTRUE(rv$batch_running) || is.null(rv$batch_start)) return("00:00")
    invalidateLater(1000, session)
    elapsed <- as.integer(difftime(Sys.time(), rv$batch_start, units = "secs"))
    sprintf("%02d:%02d", elapsed %/% 60, elapsed %% 60)
  })

  output$batch_process_summary_ui <- renderUI({
    if (!isTRUE(rv$batch_complete) && !isTRUE(rv$single_dataset)) {
      return(tags$p(style = "color: #6c757d; margin: 0;", icon("info-circle"), " Run batch correction (or skip if single dataset) to see process summary."))
    }
    expr <- rv$batch_corrected
    if (is.null(expr)) expr <- rv$combined_expr
    n_genes <- if (!is.null(expr)) nrow(expr) else 0
    n_samp <- if (!is.null(expr)) ncol(expr) else 0
    tags$div(
      style = "font-size: 14px; line-height: 1.6; color: #333;",
      tags$p(tags$strong("Step 5 complete."), if (isTRUE(rv$single_dataset)) " Single dataset: batch correction skipped." else " Batch correction applied. Before/after PCA and variance explained are shown above."),
      tags$p(format(n_genes, big.mark = ","), " genes \u00d7 ", format(n_samp, big.mark = ","), " samples ready for DE."))
  })

  # Expression BEFORE batch correction (genes x samples) - in batch step
  output$download_expr_before_batch <- downloadHandler(
    filename = function() paste0("Expression_before_batch_", Sys.Date(), ".csv"),
    content = function(file) {
      expr <- rv$expr_filtered
      if (is.null(expr)) expr <- rv$combined_expr
      req(expr)
      M <- as.data.frame(expr, stringsAsFactors = FALSE)
      M <- cbind(Gene = rownames(M), M)
      rownames(M) <- NULL
      fn <- paste0("Expression_before_batch_", Sys.Date(), ".csv")
      write.csv(M, file, row.names = FALSE)
      write.csv(M, file.path(CSV_EXPORT_DIR(), fn), row.names = FALSE)
    }
  )

  # Expression AFTER batch correction (genes x samples) - in batch step
  output$download_expr_after_batch <- downloadHandler(
    filename = function() paste0("Expression_after_batch_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$batch_corrected)
      M <- as.data.frame(rv$batch_corrected, stringsAsFactors = FALSE)
      M <- cbind(Gene = rownames(M), M)
      rownames(M) <- NULL
      fn <- paste0("Expression_after_batch_", Sys.Date(), ".csv")
      write.csv(M, file, row.names = FALSE)
      write.csv(M, file.path(CSV_EXPORT_DIR(), fn), row.names = FALSE)
    }
  )
  
  # Info boxes
  output$genes_before_filter <- renderInfoBox({
    n <- if (!is.null(rv$combined_expr)) nrow(rv$combined_expr) else 0
    infoBox("Genes Before Filter", n, icon = icon("dna", class = "fa-2x"), 
            color = "blue", fill = TRUE)
  })
  
  output$genes_after_filter <- renderInfoBox({
    n <- if (!is.null(rv$expr_filtered)) nrow(rv$expr_filtered) else 0
    infoBox("Genes After Filter", n, icon = icon("filter", class = "fa-2x"), 
            color = "green", fill = TRUE)
  })
  
  # Reactive to calculate variance cutoff based on user input
  variance_cutoff <- reactive({
    req(rv$combined_expr, input$variance_percentile)
    gene_vars <- apply(rv$combined_expr, 1, var, na.rm = TRUE)
    percentile <- input$variance_percentile / 100
    quantile(gene_vars, percentile, na.rm = TRUE)
  })

  .batch_var_keep_n <- function(expr, pct) {
    if (is.null(expr) || !is.matrix(expr) || nrow(expr) < 1L) {
      return(c(keep = 0L, remove = 0L, cutoff = NA_real_))
    }
    gene_vars <- apply(expr, 1, stats::var, na.rm = TRUE)
    cutoff <- as.numeric(stats::quantile(gene_vars, pct / 100, na.rm = TRUE))
    n_keep <- sum(!is.na(gene_vars) & gene_vars > cutoff)
    n_remove <- sum(is.na(gene_vars) | gene_vars <= cutoff)
    c(keep = n_keep, remove = n_remove, cutoff = cutoff)
  }

  # Calculate genes to keep and remove based on current percentile
  output$genes_to_keep <- renderText({
    req(input$variance_percentile)
    if (identical(input$analysis_type, "parallel") || isTRUE(rv$merge_after_de)) {
      rna <- .batch_var_keep_n(rv$expr_rna, input$variance_percentile)
      micro <- .batch_var_keep_n(rv$expr_micro, input$variance_percentile)
      return(paste0(
        "RNA-seq ", format(as.integer(rna[["keep"]]), big.mark = ","),
        " | array ", format(as.integer(micro[["keep"]]), big.mark = ",")
      ))
    }
    req(rv$combined_expr)
    gene_vars <- apply(rv$combined_expr, 1, var, na.rm = TRUE)
    cutoff <- variance_cutoff()
    n_keep <- sum(!is.na(gene_vars) & gene_vars > cutoff)
    format(n_keep, big.mark = ",")
  })

  output$genes_to_remove <- renderText({
    req(input$variance_percentile)
    if (identical(input$analysis_type, "parallel") || isTRUE(rv$merge_after_de)) {
      rna <- .batch_var_keep_n(rv$expr_rna, input$variance_percentile)
      micro <- .batch_var_keep_n(rv$expr_micro, input$variance_percentile)
      return(paste0(
        "RNA-seq ", format(as.integer(rna[["remove"]]), big.mark = ","),
        " | array ", format(as.integer(micro[["remove"]]), big.mark = ",")
      ))
    }
    req(rv$combined_expr)
    gene_vars <- apply(rv$combined_expr, 1, var, na.rm = TRUE)
    cutoff <- variance_cutoff()
    n_remove <- sum(is.na(gene_vars) | gene_vars <= cutoff)
    format(n_remove, big.mark = ",")
  })

  output$filter_info <- renderText({
    req(input$variance_percentile)
    if (identical(input$analysis_type, "parallel") || isTRUE(rv$merge_after_de)) {
      return("Each platform is filtered on its own gene set. The two histograms above are RNA-seq (left) and microarray (right).")
    }
    req(rv$combined_expr)
    total_genes <- nrow(rv$combined_expr)
    gene_vars <- apply(rv$combined_expr, 1, var, na.rm = TRUE)
    cutoff <- variance_cutoff()
    n_remove <- sum(is.na(gene_vars) | gene_vars <= cutoff)
    percent_remove <- round(100 * n_remove / total_genes, 1)
    paste0("Removing ", percent_remove, "% of genes (", format(n_remove, big.mark = ","),
           " genes) with variance below ", round(cutoff, 4))
  })
  
  output$variance_cutoff <- renderInfoBox({
    val <- if (!is.null(rv$combined_expr) && !is.null(input$variance_percentile)) {
      round(variance_cutoff(), 4)
    } else 0
    infoBox("Variance Cutoff", val, icon = icon("chart-line", class = "fa-2x"), 
            color = "purple", fill = TRUE)
  })
  
  batch_gene_variance_plot <- reactive({
    req(rv$combined_expr, input$variance_percentile)
    gene_vars <- apply(rv$combined_expr, 1, var, na.rm = TRUE)
    gene_vars[is.na(gene_vars)] <- 0
    cutoff <- variance_cutoff()
    df <- data.frame(Variance = gene_vars, Kept = ifelse(gene_vars > cutoff, "Retained", "Filtered"))
    ggplot2::ggplot(df, ggplot2::aes(x = log10(Variance), fill = Kept)) +
      ggplot2::geom_histogram(bins = 50, alpha = 0.7) +
      ggplot2::geom_vline(xintercept = log10(cutoff), linetype = "dashed", color = "red", linewidth = 1.5) +
      ggplot2::scale_fill_manual(values = c("Retained" = "#2ecc71", "Filtered" = "#e74c3c")) +
      ggplot2::theme_bw(base_size = 14) +
      ggplot2::labs(title = "Gene Variance Distribution",
           subtitle = paste0("Cutoff (", input$variance_percentile, "th percentile): ", round(cutoff, 4)),
           x = "Log10(Variance)", y = "Count", fill = "Status") +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), legend.position = "top")
  })

  output$gene_variance_plot <- renderPlot({
    if (identical(input$analysis_type, "parallel") || isTRUE(rv$merge_after_de)) {
      plot.new()
      text(0.5, 0.5, "Parallel: RNA-seq and microarray variance plots are separate.", cex = 1.1, col = "gray40")
      return(invisible(NULL))
    }
    p <- batch_gene_variance_plot()
    if (!is.null(p)) p
  })

  .batch_variance_gg <- function(expr, title) {
    if (is.null(expr) || !is.matrix(expr) || nrow(expr) < 2L) {
      plot.new()
      text(0.5, 0.5, "Complete Step 2 to see this plot.", cex = 1.1, col = "gray40")
      return(invisible(NULL))
    }
    req(input$variance_percentile)
    gene_vars <- apply(expr, 1, stats::var, na.rm = TRUE)
    gene_vars[is.na(gene_vars)] <- 0
    cutoff <- as.numeric(stats::quantile(gene_vars, input$variance_percentile / 100, na.rm = TRUE))
    df <- data.frame(
      Variance = gene_vars,
      Kept = ifelse(gene_vars > cutoff, "Retained", "Filtered"),
      stringsAsFactors = FALSE
    )
    ggplot2::ggplot(df, ggplot2::aes(x = log10(Variance + 1e-12), fill = Kept)) +
      ggplot2::geom_histogram(bins = 50, alpha = 0.7) +
      ggplot2::geom_vline(xintercept = log10(cutoff + 1e-12), linetype = "dashed", color = "red", linewidth = 1.2) +
      ggplot2::scale_fill_manual(values = c("Retained" = "#2ecc71", "Filtered" = "#e74c3c")) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::labs(
        title = title,
        subtitle = paste0("Cutoff (", input$variance_percentile, "th percentile)"),
        x = "Log10(Variance)", y = "Count", fill = "Status"
      ) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13), legend.position = "top")
  }

  output$gene_variance_plot_rna <- renderPlot({
    .batch_variance_gg(rv$expr_rna, "RNA-seq gene variance")
  })
  output$gene_variance_plot_micro <- renderPlot({
    .batch_variance_gg(rv$expr_micro, "Microarray gene variance")
  })

  output$pca_before_dataset_rna <- renderPlot({
    req(rv$expr_rna, rv$unified_metadata)
    .batch_pca_polar_plot(
      rv$expr_rna, rv$unified_metadata, "Dataset",
      "RNA-seq before batch - by dataset",
      "This platform only"
    )
  })
  output$pca_after_dataset_rna <- renderPlot({
    req(rv$batch_corrected_rna, rv$unified_metadata)
    .batch_pca_polar_plot(
      rv$batch_corrected_rna, rv$unified_metadata, "Dataset",
      "RNA-seq after batch - by dataset",
      "This platform only"
    )
  })
  output$pca_before_dataset_micro <- renderPlot({
    req(rv$expr_micro, rv$unified_metadata)
    .batch_pca_polar_plot(
      rv$expr_micro, rv$unified_metadata, "Dataset",
      "Microarray before batch - by dataset",
      "This platform only"
    )
  })
  output$pca_after_dataset_micro <- renderPlot({
    req(rv$batch_corrected_micro, rv$unified_metadata)
    .batch_pca_polar_plot(
      rv$batch_corrected_micro, rv$unified_metadata, "Dataset",
      "Microarray after batch - by dataset",
      "This platform only"
    )
  })

  output$download_gene_variance_png <- downloadHandler(
    filename = function() "Batch_Gene_Variance.png",
    content = function(file) {
      p <- batch_gene_variance_plot()
      if (!is.null(p)) ggplot2::ggsave(file, plot = p, width = 9, height = 5, dpi = IMAGE_DPI, units = "in", bg = "white", device = "png")
    }
  )
  output$download_gene_variance_jpg <- downloadHandler(
    filename = function() "Batch_Gene_Variance.jpg",
    content = function(file) {
      p <- batch_gene_variance_plot()
      if (!is.null(p)) ggplot2::ggsave(file, plot = p, width = 9, height = 5, dpi = IMAGE_DPI, units = "in", bg = "white", device = "jpeg")
    }
  )
  output$download_gene_variance_pdf <- downloadHandler(
    filename = function() "Batch_Gene_Variance.pdf",
    content = function(file) {
      p <- batch_gene_variance_plot()
      if (!is.null(p)) ggplot2::ggsave(file, plot = p, width = 9, height = 5, device = "pdf", bg = "white")
    }
  )
  
  observeEvent(input$apply_batch, {
    # If only one dataset is present, skip batch correction and mark complete.
    if (isTRUE(rv$single_dataset)) {
      if (!isTRUE(rv$groups_applied)) {
        showNotification("Step 4 required: apply group labels before proceeding.", type = "error", duration = 6)
        return()
      }
      base_expr <- NULL
      if (!is.null(rv$combined_expr) && (is.matrix(rv$combined_expr) || is.data.frame(rv$combined_expr)) && nrow(rv$combined_expr) > 0) {
        base_expr <- rv$combined_expr
      } else if (!is.null(rv$combined_expr_raw) && (is.matrix(rv$combined_expr_raw) || is.data.frame(rv$combined_expr_raw)) && nrow(rv$combined_expr_raw) > 0) {
        base_expr <- rv$combined_expr_raw
      }
      req(base_expr)
      rv$expr_filtered <- base_expr
      rv$batch_corrected <- base_expr
      rv$batch_corrected_rna <- rv$expr_rna
      rv$batch_corrected_micro <- rv$expr_micro
      rv$batch_complete <- TRUE
      rv$batch_running <- FALSE
      output$batch_log <- renderText({
        paste0(
          "OK Batch correction skipped (single dataset)\n\n",
          "Reason: Only 1 dataset was selected, so there is no between-study batch to remove.\n",
          "Downstream steps will use the current expression matrix.\n"
        )
      })
      showNotification("Single dataset detected: batch correction skipped.", type = "message", duration = 6)
      return()
    }
    if (!isTRUE(rv$groups_applied)) {
      showNotification(
        tags$div(icon("exclamation-triangle"), tags$strong(" Step 4 required:"),
                 " Apply group labels (Step 4: Select Groups) before batch correction."),
        type = "error", duration = 6)
      return()
    }
    req(input$variance_percentile)
    
    # Disable button and show loading
    shinyjs::disable("apply_batch")
    shinyjs::html("apply_batch", 
                  HTML('<i class="fa fa-spinner fa-spin"></i> Processing...'))
    
    rv$batch_start <- Sys.time()
    rv$batch_running <- TRUE
    
    # Show processing notification
    showNotification(
      tags$div(
        tags$div(class = "status-indicator processing"),
        tags$strong("Batch correction in progress..."),
        tags$br(),
        tags$span("Filtering genes and applying batch correction. Please wait..."),
        style = "font-size: 13px;"
      ),
      type = "message",
      duration = NULL,
      id = "batch_processing"
    )
    
    withProgress(message = 'Batch correction...', value = 0, {
      de_rna <- if (!is.null(input$de_method_rna) && nzchar(input$de_method_rna)) {
        input$de_method_rna
      } else {
        "deseq2"
      }
      confounded <- FALSE
      if (!is.null(rv$unified_metadata)) {
        sm <- tryCatch(gexpipe_batch_confounding_summary(rv$unified_metadata), error = function(e) NULL)
        confounded <- isTRUE(sm$confounded)
      }
      defs <- gexpipe_parallel_batch_defaults(de_rna, confounded)
      batch_mode_p <- if (is.null(input$batch_mode_parallel) || !nzchar(input$batch_mode_parallel)) {
        "auto"
      } else {
        input$batch_mode_parallel
      }
      rna_method_use <- if (identical(batch_mode_p, "manual") && !is.null(input$batch_method_rna)) {
        input$batch_method_rna
      } else {
        defs$rna
      }
      micro_method_use <- if (identical(batch_mode_p, "manual") && !is.null(input$batch_method_micro)) {
        input$batch_method_micro
      } else {
        defs$micro
      }
      rv$last_batch_method_rna <- rna_method_use
      rv$last_batch_method_micro <- micro_method_use
      parallel_batch <- isTRUE(rv$merge_after_de) || identical(input$analysis_type, "parallel")

      res <- if (isTRUE(parallel_batch)) {
        gexp_batch_correct_by_platform(
          expr = if (!is.null(rv$combined_expr_before_global_norm)) {
            rv$combined_expr_before_global_norm
          } else {
            rv$combined_expr
          },
          metadata = rv$unified_metadata,
          variance_percentile = input$variance_percentile,
          rna_method = rna_method_use,
          micro_method = micro_method_use,
          expr_rna = rv$expr_rna,
          expr_micro = rv$expr_micro
        )
      } else {
        gexp_batch_correct(
          expr = rv$combined_expr,
          metadata = rv$unified_metadata,
          variance_percentile = input$variance_percentile,
          method = input$batch_method
        )
      }
      
      rv$expr_filtered <- res$expr_filtered
      rv$batch_corrected <- res$batch_corrected
      rv$batch_corrected_rna <- res$batch_corrected_rna
      rv$batch_corrected_micro <- res$batch_corrected_micro
      
      genes_before <- res$genes_before
      genes_after <- res$genes_after
      filter_percent <- res$filter_percent
      
      rv$batch_complete <- TRUE
      rv$batch_running <- FALSE
      micro_method_label <- if (!is.null(res$n_datasets_micro) && res$n_datasets_micro < 2L) {
        paste0("variance filter only (1 GSE; ", micro_method_use, " not applied)")
      } else {
        micro_method_use
      }
      rna_method_label <- if (!is.null(res$n_datasets_rna) && res$n_datasets_rna < 2L) {
        paste0("variance filter only (1 GSE; ", rna_method_use, " not applied)")
      } else {
        rna_method_use
      }
      if (isTRUE(parallel_batch)) {
        n_r <- if (is.null(rv$batch_corrected_rna)) 0L else nrow(rv$batch_corrected_rna)
        s_r <- if (is.null(rv$batch_corrected_rna)) 0L else ncol(rv$batch_corrected_rna)
        n_m <- if (is.null(rv$batch_corrected_micro)) 0L else nrow(rv$batch_corrected_micro)
        s_m <- if (is.null(rv$batch_corrected_micro)) 0L else ncol(rv$batch_corrected_micro)
        n_m0 <- if (!is.null(res$genes_before_micro)) res$genes_before_micro else n_m
        n_r0 <- if (!is.null(res$genes_before_rna)) res$genes_before_rna else n_r
        rv$batch_log_micro <- gexpipe_format_separate_run_log(
          1L, "MICROARRAY",
          paste0(
            "Method: ", micro_method_label, " (inside microarray only)\n",
            "Merged with RNA-seq: no\n",
            "Genes: ", format(n_m0, big.mark = ","), " \u2192 ", format(n_m, big.mark = ","), "\n",
            "\nOK Microarray batch complete.\n",
            "  Genes:   ", format(n_m, big.mark = ","), "\n",
            "  Samples: ", format(s_m, big.mark = ","), "\n"
          )
        )
        rv$batch_log_rna <- gexpipe_format_separate_run_log(
          2L, "RNA-SEQ",
          paste0(
            "Method: ", rna_method_label, " (inside RNA-seq only)\n",
            "Merged with microarray: no\n",
            "Genes: ", format(n_r0, big.mark = ","), " \u2192 ", format(n_r, big.mark = ","), "\n",
            "\nOK RNA-seq batch complete.\n",
            "  Genes:   ", format(n_r, big.mark = ","), "\n",
            "  Samples: ", format(s_r, big.mark = ","), "\n"
          )
        )
      }
      
      output$batch_log <- renderText({
        if (isTRUE(parallel_batch)) {
          hr <- paste(rep("\u2501", 56L), collapse = "")
          n_r <- if (is.null(rv$batch_corrected_rna)) 0L else nrow(rv$batch_corrected_rna)
          s_r <- if (is.null(rv$batch_corrected_rna)) 0L else ncol(rv$batch_corrected_rna)
          n_m <- if (is.null(rv$batch_corrected_micro)) 0L else nrow(rv$batch_corrected_micro)
          s_m <- if (is.null(rv$batch_corrected_micro)) 0L else ncol(rv$batch_corrected_micro)
          paste0(
            "Parallel DE: two separate batch runs (not one joint ComBat).\n\n",
            hr, "\n",
            "RUN 1 \u2014 MICROARRAY (separate)\n",
            hr, "\n",
            "Method: ", micro_method_label, " (inside microarray only)\n",
            "OK Microarray batch complete.\n",
            "  Genes:   ", format(n_m, big.mark = ","), "\n",
            "  Samples: ", format(s_m, big.mark = ","), "\n\n",
            hr, "\n",
            "RUN 2 \u2014 RNA-SEQ (separate)\n",
            hr, "\n",
            "Method: ", rna_method_label, " (inside RNA-seq only)\n",
            "OK RNA-seq batch complete.\n",
            "  Genes:   ", format(n_r, big.mark = ","), "\n",
            "  Samples: ", format(s_r, big.mark = ","), "\n\n",
            res$log_text
          )
        } else {
          paste0(
            "OK Batch correction complete\n",
            "Method: ", input$batch_method, "\n\n",
            res$log_text,
            "\nFinal Dataset:\n",
            "  Genes: ", format(genes_after, big.mark = ","), "\n",
            "  Samples: ", format(ncol(rv$batch_corrected), big.mark = ",")
          )
        }
      })
      
      # Re-enable button
      shinyjs::enable("apply_batch")
      shinyjs::html("apply_batch", 
                    HTML('<i class="fa fa-magic"></i> Apply Batch Correction'))
      
      # Remove processing notification
      removeNotification("batch_processing")
      
      showNotification(
        if (isTRUE(parallel_batch)) {
          n_m0 <- if (!is.null(res$genes_before_micro)) res$genes_before_micro else n_m
          n_r0 <- if (!is.null(res$genes_before_rna)) res$genes_before_rna else n_r
          tags$div(
            tags$strong("OK Batch correction complete — two separate runs."),
            tags$br(),
            tags$span(
              "Microarray: ", format(n_m0, big.mark = ","), " \u2192 ",
              format(n_m, big.mark = ","), " genes, ",
              format(s_m, big.mark = ","), " samples."
            ),
            tags$br(),
            tags$span(
              "RNA-seq: ", format(n_r0, big.mark = ","), " \u2192 ",
              format(n_r, big.mark = ","), " genes, ",
              format(s_r, big.mark = ","), " samples."
            ),
            tags$br(),
            tags$span("Not one joint batch on all 134 samples."),
            style = "font-size: 13px;"
          )
        } else {
          tags$div(
            tags$strong("OK Batch correction complete!"),
            tags$br(),
            tags$span(
              "Genes filtered: ",
              format(genes_before, big.mark = ","), " -> ",
              format(genes_after, big.mark = ","), " (", filter_percent, "% removed)"
            ),
            tags$br(),
            tags$span(
              "Final: ",
              format(genes_after, big.mark = ","), " genes, ",
              format(ncol(rv$batch_corrected), big.mark = ","), " samples"
            ),
            style = "font-size: 13px;"
          )
        },
        type = "message", duration = 10
      )
    })

    rv$batch_running <- FALSE
  })
  
  .batch_pvca_ggplot <- function(pvca_results, title, subtitle) {
    ggplot(pvca_results, aes(x = Factor, y = Variance, fill = Factor)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      scale_fill_manual(values = c(
        "Dataset" = "#e74c3c",
        "Platform" = "#e67e22",
        "Condition" = "#3498db",
        "Residual" = "#95a5a6"
      )) +
      theme_bw(base_size = 14) +
      labs(
        title = title,
        subtitle = subtitle,
        x = "Factor",
        y = "Proportion of Variance",
        fill = "Factor"
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
        legend.position = "right",
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95")
      ) +
      ylim(0, 1)
  }

  .batch_pvca_expr_for_plot <- function(before = TRUE) {
    .pvca <- function(expr_mat, meta) {
      if (exists(".gexpipe_call", mode = "function", inherits = TRUE)) {
        return(.gexpipe_call("gexpipe_pvca_df", expr_mat, meta))
      }
      gexpipe_pvca_df(expr_mat, meta)
    }
    if (before) {
      expr_mat <- rv$expr_filtered
      pv <- if (!is.null(expr_mat)) {
        .pvca(expr_mat, rv$unified_metadata)
      } else {
        list(ok = FALSE, data = NULL, message = "Expression before batch correction not available.")
      }
      if (!isTRUE(pv$ok) && !is.null(rv$combined_expr)) {
        pv <- .pvca(rv$combined_expr, rv$unified_metadata)
      }
      pv
    } else {
      .pvca(rv$batch_corrected, rv$unified_metadata)
    }
  }

  .batch_pvca_render <- function(before = TRUE) {
    req(rv$unified_metadata)
    if (before) {
      req(rv$batch_complete)
    } else {
      req(rv$batch_corrected)
    }
    pv <- .batch_pvca_expr_for_plot(before = before)
    if (!isTRUE(pv$ok)) {
      plot.new()
      text(0.5, 0.5, paste("PVCA unavailable:", pv$message), cex = 1)
      return(invisible(NULL))
    }
    subtitle <- if (before) {
      "Dataset / Platform variance should be visible before correction"
    } else {
      "Dataset variance should drop; large Residual is normal (biology + noise in top PCs)"
    }
    title <- if (before) {
      "PVCA - Before Batch Correction"
    } else {
      "PVCA - After Batch Correction"
    }
    .batch_pvca_ggplot(pv$data, title, subtitle)
  }

  .batch_pca_polar_theme <- function() {
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
  }

  .batch_pca_polar_plot <- function(expr, meta, color_by, title, subtitle) {
    df <- gexpipe_pca_polar_df(expr, meta, color_by)
    base <- ggplot(df, aes(x = theta, y = r)) +
      coord_polar(theta = "x", start = -pi / 2, direction = 1) +
      scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi),
                        labels = c("-180 deg", "-90 deg", "0 deg", "90 deg", "180 deg")) +
      scale_y_continuous(limits = c(0, NA)) +
      theme_bw(base_size = 14) +
      labs(title = title, subtitle = subtitle, x = "Angle (PC1-PC2)", y = "Radius") +
      .batch_pca_polar_theme()

    if (color_by == "Condition" &&
        (all(is.na(df$Condition)) || length(unique(df$Condition[!is.na(df$Condition)])) < 2)) {
      base + geom_point(size = 3.5, alpha = 0.7, color = "gray60")
    } else if (color_by == "Condition") {
      base +
        geom_point(size = 3.5, alpha = 0.7, aes(color = Condition)) +
        scale_color_manual(values = c("Normal" = "#3498db", "Disease" = "#e74c3c", "None" = "#95a5a6"),
                          na.value = "gray60") +
        labs(color = "Condition")
    } else {
      base +
        geom_point(size = 3.5, alpha = 0.7, aes(color = .data[[color_by]])) +
        labs(color = color_by)
    }
  }

  # PCA Before Batch Correction - Colored by Dataset (circular / polar)
  output$pca_before_dataset <- renderPlot({
    req(rv$expr_filtered, rv$unified_metadata)
    .batch_pca_polar_plot(
      rv$expr_filtered, rv$unified_metadata, "Dataset",
      "Before Batch Correction - By Dataset",
      "Batch effects visible as dataset separation (circular)"
    )
  })
  
  # PCA Before Batch Correction - Colored by Condition (circular / polar)
  output$pca_before_condition <- renderPlot({
    req(rv$expr_filtered, rv$unified_metadata)
    .batch_pca_polar_plot(
      rv$expr_filtered, rv$unified_metadata, "Condition",
      "Before Batch Correction - By Condition",
      if (all(is.na(rv$unified_metadata$Condition)) ||
          length(unique(rv$unified_metadata$Condition[!is.na(rv$unified_metadata$Condition)])) < 2) {
        "Conditions not yet assigned (circular)"
      } else {
        "Biological signal may be obscured by batch effects (circular)"
      }
    )
  })
  
  # PCA Before / After - coloured by Platform (mixed microarray + RNA-seq only)
  output$batch_platform_pca_row <- renderUI({
    if (isTRUE(rv$merge_after_de) || identical(input$analysis_type, "parallel")) {
      return(NULL)
    }
    if (is.null(rv$unified_metadata) || !gexpipe_has_mixed_platforms(rv$unified_metadata)) {
      return(NULL)
    }
    fluidRow(
      box(
        title = tags$span(icon("exclamation-triangle"), " Before Batch Correction - By Platform"),
        width = 6, status = "warning", solidHeader = TRUE,
        plotOutput("pca_before_platform", height = "400px"),
        tags$div(style = "margin-top: 6px;",
          downloadButton("download_pca_before_platform_png", tagList(icon("download"), " PNG"), class = "btn-sm btn-warning", style = "margin-right: 4px;"),
          downloadButton("download_pca_before_platform_jpg", tagList(icon("download"), " JPG"), class = "btn-sm btn-warning", style = "margin-right: 4px;"),
          downloadButton("download_pca_before_platform_pdf", tagList(icon("download"), " PDF"), class = "btn-sm btn-warning"))
      ),
      box(
        title = tags$span(icon("check-circle"), " After Batch Correction - By Platform"),
        width = 6, status = "success", solidHeader = TRUE,
        plotOutput("pca_after_platform", height = "400px"),
        tags$div(style = "margin-top: 6px;",
          downloadButton("download_pca_after_platform_png", tagList(icon("download"), " PNG"), class = "btn-sm btn-success", style = "margin-right: 4px;"),
          downloadButton("download_pca_after_platform_jpg", tagList(icon("download"), " JPG"), class = "btn-sm btn-success", style = "margin-right: 4px;"),
          downloadButton("download_pca_after_platform_pdf", tagList(icon("download"), " PDF"), class = "btn-sm btn-success"))
      )
    )
  })

  .batch_pca_platform_plot <- function(expr, meta, when = c("before", "after")) {
    when <- match.arg(when)
    df <- gexpipe_pca_polar_df(expr, meta, "Platform")
    ttl <- if (when == "before") {
      "Before Batch Correction - By Platform"
    } else {
      "After Batch Correction - By Platform"
    }
    sub <- if (when == "before") {
      "Strong platform separation indicates a technology effect to remove"
    } else {
      "Platforms should intermingle; persistent separation suggests residual platform bias"
    }
    ggplot(df, aes(x = theta, y = r, color = Platform)) +
      geom_point(size = 3.5, alpha = 0.7) +
      scale_color_manual(values = c("Microarray" = "#e67e22", "RNAseq" = "#9b59b6")) +
      coord_polar(theta = "x", start = -pi / 2, direction = 1) +
      scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi),
                        labels = c("-180 deg", "-90 deg", "0 deg", "90 deg", "180 deg")) +
      scale_y_continuous(limits = c(0, NA)) +
      theme_bw(base_size = 14) +
      labs(title = ttl, subtitle = sub, x = "Angle (PC1-PC2)", y = "Radius", color = "Platform") +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
        legend.position = "right",
        panel.grid.minor = element_blank()
      )
  }

  output$pca_before_platform <- renderPlot({
    req(rv$expr_filtered, rv$unified_metadata)
    .batch_pca_platform_plot(rv$expr_filtered, rv$unified_metadata, "before")
  })

  output$pca_after_platform <- renderPlot({
    req(rv$batch_corrected, rv$unified_metadata)
    .batch_pca_platform_plot(rv$batch_corrected, rv$unified_metadata, "after")
  })
  
  # PCA After Batch Correction - Colored by Dataset (circular / polar)
  output$pca_after_dataset <- renderPlot({
    req(rv$batch_corrected, rv$unified_metadata)
    .batch_pca_polar_plot(
      rv$batch_corrected, rv$unified_metadata, "Dataset",
      "After Batch Correction - By Dataset",
      "Datasets should be intermingled (batch effects removed) (circular)"
    )
  })
  
  # PCA After Batch Correction - Colored by Condition (circular / polar)
  output$pca_after_condition <- renderPlot({
    req(rv$batch_corrected, rv$unified_metadata)
    .batch_pca_polar_plot(
      rv$batch_corrected, rv$unified_metadata, "Condition",
      "After Batch Correction - By Condition",
      if (all(is.na(rv$unified_metadata$Condition)) ||
          length(unique(rv$unified_metadata$Condition[!is.na(rv$unified_metadata$Condition)])) < 2) {
        "Conditions not yet assigned (circular)"
      } else {
        "Biological signal should be clearly visible (circular)"
      }
    )
  })

  # Hierarchical Clustering Heatmap - Before Batch Correction
  output$hclust_before <- renderPlot({
    req(rv$expr_filtered)
    
    tryCatch({
      # Limit samples for performance
      n_samples <- min(50, ncol(rv$expr_filtered))
      if (ncol(rv$expr_filtered) > n_samples) {
        withr::local_seed(123)
        sample_idx <- sample(seq_len(ncol(rv$expr_filtered)), n_samples)
        expr_subset <- rv$expr_filtered[, sample_idx]
        metadata_subset <- rv$unified_metadata[sample_idx, ]
      } else {
        expr_subset <- rv$expr_filtered
        metadata_subset <- rv$unified_metadata
      }
      
      # Calculate correlation/distance matrix
      cor_matrix <- cor(expr_subset, use = "pairwise.complete.obs")
      dist_matrix <- as.dist(1 - cor_matrix)
      
      # Perform hierarchical clustering
      hclust_result <- hclust(dist_matrix, method = "ward.D2")
      
      # Create annotation for heatmap
      annotation_col <- data.frame(
        Dataset = metadata_subset$Dataset,
        Condition = ifelse(is.na(metadata_subset$Condition), "Unknown", metadata_subset$Condition),
        row.names = colnames(expr_subset)
      )
      if (gexpipe_has_mixed_platforms(metadata_subset)) {
        annotation_col$Platform <- metadata_subset$Platform
      }
      
      # Reorder correlation matrix by dendrogram
      cor_matrix_ordered <- cor_matrix[hclust_result$order, hclust_result$order]
      
      # Create heatmap
      pheatmap::pheatmap(
        cor_matrix_ordered,
        cluster_rows = FALSE,
        cluster_cols = FALSE,
        annotation_col = annotation_col,
        annotation_row = annotation_col,
        color = colorRampPalette(c("#e74c3c", "white", "#3498db"))(100),
        main = "Hierarchical Clustering - Before Batch Correction\n(Samples cluster by Dataset)",
        fontsize = 8,
        fontsize_row = 7,
        fontsize_col = 7,
        show_rownames = FALSE,
        show_colnames = FALSE
      )
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error generating heatmap:", e$message), cex = 1.2)
    })
  })
  
  # Hierarchical Clustering Heatmap - After Batch Correction
  output$hclust_after <- renderPlot({
    req(rv$batch_corrected)
    
    tryCatch({
      # Limit samples for performance
      n_samples <- min(50, ncol(rv$batch_corrected))
      if (ncol(rv$batch_corrected) > n_samples) {
        withr::local_seed(123)
        sample_idx <- sample(seq_len(ncol(rv$batch_corrected)), n_samples)
        expr_subset <- rv$batch_corrected[, sample_idx]
        metadata_subset <- rv$unified_metadata[sample_idx, ]
      } else {
        expr_subset <- rv$batch_corrected
        metadata_subset <- rv$unified_metadata
      }
      
      # Calculate correlation/distance matrix
      cor_matrix <- cor(expr_subset, use = "pairwise.complete.obs")
      dist_matrix <- as.dist(1 - cor_matrix)
      
      # Perform hierarchical clustering
      hclust_result <- hclust(dist_matrix, method = "ward.D2")
      
      # Create annotation for heatmap
      annotation_col <- data.frame(
        Dataset = metadata_subset$Dataset,
        Condition = ifelse(is.na(metadata_subset$Condition), "Unknown", metadata_subset$Condition),
        row.names = colnames(expr_subset)
      )
      if (gexpipe_has_mixed_platforms(metadata_subset)) {
        annotation_col$Platform <- metadata_subset$Platform
      }
      
      # Reorder correlation matrix by dendrogram
      cor_matrix_ordered <- cor_matrix[hclust_result$order, hclust_result$order]
      
      # Create heatmap
      pheatmap::pheatmap(
        cor_matrix_ordered,
        cluster_rows = FALSE,
        cluster_cols = FALSE,
        annotation_col = annotation_col,
        annotation_row = annotation_col,
        color = colorRampPalette(c("#e74c3c", "white", "#3498db"))(100),
        main = "Hierarchical Clustering - After Batch Correction\n(Samples cluster by Condition)",
        fontsize = 8,
        fontsize_row = 7,
        fontsize_col = 7,
        show_rownames = FALSE,
        show_colnames = FALSE
      )
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error generating heatmap:", e$message), cex = 1.2)
    })
  })
  
  # PVCA (Principal Variance Component Analysis) - Before
  output$pvca_before <- renderPlot({
    p <- .batch_pvca_render(before = TRUE)
    if (!is.null(p)) p
  })
  
  # PVCA (Principal Variance Component Analysis) - After
  output$pvca_after <- renderPlot({
    p <- .batch_pvca_render(before = FALSE)
    if (!is.null(p)) p
  })
  
  output$next_to_results_btn <- renderUI({
    req(rv$batch_complete)
    actionButton("go_to_results", "Next: View Results", 
                 icon = icon("arrow-right"), class = "btn-success btn-lg")
  })

  # Batch plot download helpers: re-use same logic as renderPlot, draw to file
  batch_save_ggplot <- function(p, file, device = "png") {
    if (is.null(p)) return()
    if (device == "png") ggplot2::ggsave(file, plot = p, width = 7, height = 5, dpi = IMAGE_DPI, units = "in", bg = "white", device = "png")
    else if (device %in% c("jpeg", "jpg")) ggplot2::ggsave(file, plot = p, width = 7, height = 5, dpi = IMAGE_DPI, units = "in", bg = "white", device = "jpeg")
    else ggplot2::ggsave(file, plot = p, width = 7, height = 5, device = "pdf", bg = "white")
  }

  .batch_pca_subtitle <- function(color_by, when, meta) {
    if (color_by == "Dataset") {
      if (when == "before") {
        "Batch effects visible as dataset separation (circular)"
      } else {
        "Datasets should be intermingled (batch effects removed) (circular)"
      }
    } else {
      unassigned <- all(is.na(meta$Condition)) ||
        length(unique(meta$Condition[!is.na(meta$Condition)])) < 2
      if (when == "before") {
        if (unassigned) "Conditions not yet assigned (circular)" else "Biological signal may be obscured by batch effects (circular)"
      } else if (unassigned) {
        "Conditions not yet assigned (circular)"
      } else {
        "Biological signal should be clearly visible (circular)"
      }
    }
  }

  .batch_download_pca <- function(file, expr, meta, color_by, when, device = "png") {
    if (is.null(expr) || is.null(meta)) return()
    title <- paste0(if (when == "before") "Before" else "After", " Batch Correction - By ", color_by)
    subtitle <- .batch_pca_subtitle(color_by, when, meta)
    p <- tryCatch(
      .batch_pca_polar_plot(expr, meta, color_by, title, subtitle),
      error = function(e) {
        showNotification(paste("PCA export failed:", conditionMessage(e)), type = "error", duration = 5)
        NULL
      }
    )
    batch_save_ggplot(p, file, device)
  }

  .batch_download_pca_platform <- function(file, expr, meta, when, device = "png") {
    if (is.null(expr) || is.null(meta)) return()
    p <- tryCatch(
      .batch_pca_platform_plot(expr, meta, when),
      error = function(e) {
        showNotification(paste("Platform PCA export failed:", conditionMessage(e)), type = "error", duration = 5)
        NULL
      }
    )
    batch_save_ggplot(p, file, device)
  }

  output$download_pca_before_dataset_png <- downloadHandler(
    filename = function() "Batch_PCA_Before_Dataset.png",
    content = function(file) .batch_download_pca(file, rv$expr_filtered, rv$unified_metadata, "Dataset", "before", "png")
  )
  output$download_pca_before_dataset_jpg <- downloadHandler(
    filename = function() "Batch_PCA_Before_Dataset.jpg",
    content = function(file) .batch_download_pca(file, rv$expr_filtered, rv$unified_metadata, "Dataset", "before", "jpeg")
  )
  output$download_pca_before_dataset_pdf <- downloadHandler(
    filename = function() "Batch_PCA_Before_Dataset.pdf",
    content = function(file) .batch_download_pca(file, rv$expr_filtered, rv$unified_metadata, "Dataset", "before", "pdf")
  )
  output$download_pca_after_dataset_png <- downloadHandler(
    filename = function() "Batch_PCA_After_Dataset.png",
    content = function(file) .batch_download_pca(file, rv$batch_corrected, rv$unified_metadata, "Dataset", "after", "png")
  )
  output$download_pca_after_dataset_jpg <- downloadHandler(
    filename = function() "Batch_PCA_After_Dataset.jpg",
    content = function(file) .batch_download_pca(file, rv$batch_corrected, rv$unified_metadata, "Dataset", "after", "jpeg")
  )
  output$download_pca_after_dataset_pdf <- downloadHandler(
    filename = function() "Batch_PCA_After_Dataset.pdf",
    content = function(file) .batch_download_pca(file, rv$batch_corrected, rv$unified_metadata, "Dataset", "after", "pdf")
  )
  output$download_pca_before_condition_png <- downloadHandler(
    filename = function() "Batch_PCA_Before_Condition.png",
    content = function(file) .batch_download_pca(file, rv$expr_filtered, rv$unified_metadata, "Condition", "before", "png")
  )
  output$download_pca_before_condition_jpg <- downloadHandler(
    filename = function() "Batch_PCA_Before_Condition.jpg",
    content = function(file) .batch_download_pca(file, rv$expr_filtered, rv$unified_metadata, "Condition", "before", "jpeg")
  )
  output$download_pca_before_condition_pdf <- downloadHandler(
    filename = function() "Batch_PCA_Before_Condition.pdf",
    content = function(file) .batch_download_pca(file, rv$expr_filtered, rv$unified_metadata, "Condition", "before", "pdf")
  )
  output$download_pca_after_condition_png <- downloadHandler(
    filename = function() "Batch_PCA_After_Condition.png",
    content = function(file) .batch_download_pca(file, rv$batch_corrected, rv$unified_metadata, "Condition", "after", "png")
  )
  output$download_pca_after_condition_jpg <- downloadHandler(
    filename = function() "Batch_PCA_After_Condition.jpg",
    content = function(file) .batch_download_pca(file, rv$batch_corrected, rv$unified_metadata, "Condition", "after", "jpeg")
  )
  output$download_pca_after_condition_pdf <- downloadHandler(
    filename = function() "Batch_PCA_After_Condition.pdf",
    content = function(file) .batch_download_pca(file, rv$batch_corrected, rv$unified_metadata, "Condition", "after", "pdf")
  )
  output$download_pca_before_platform_png <- downloadHandler(
    filename = function() "Batch_PCA_Before_Platform.png",
    content = function(file) .batch_download_pca_platform(file, rv$expr_filtered, rv$unified_metadata, "before", "png")
  )
  output$download_pca_before_platform_jpg <- downloadHandler(
    filename = function() "Batch_PCA_Before_Platform.jpg",
    content = function(file) .batch_download_pca_platform(file, rv$expr_filtered, rv$unified_metadata, "before", "jpeg")
  )
  output$download_pca_before_platform_pdf <- downloadHandler(
    filename = function() "Batch_PCA_Before_Platform.pdf",
    content = function(file) .batch_download_pca_platform(file, rv$expr_filtered, rv$unified_metadata, "before", "pdf")
  )
  output$download_pca_after_platform_png <- downloadHandler(
    filename = function() "Batch_PCA_After_Platform.png",
    content = function(file) .batch_download_pca_platform(file, rv$batch_corrected, rv$unified_metadata, "after", "png")
  )
  output$download_pca_after_platform_jpg <- downloadHandler(
    filename = function() "Batch_PCA_After_Platform.jpg",
    content = function(file) .batch_download_pca_platform(file, rv$batch_corrected, rv$unified_metadata, "after", "jpeg")
  )
  output$download_pca_after_platform_pdf <- downloadHandler(
    filename = function() "Batch_PCA_After_Platform.pdf",
    content = function(file) .batch_download_pca_platform(file, rv$batch_corrected, rv$unified_metadata, "after", "pdf")
  )

  # Hclust and PVCA: draw to device (pheatmap / base)
  batch_hclust_to_file <- function(file, dev_fun, before = TRUE) {
    expr_mat <- if (before) rv$expr_filtered else rv$batch_corrected
    if (is.null(expr_mat)) return()
    n_samples <- min(50, ncol(expr_mat))
    if (ncol(expr_mat) > n_samples) { withr::local_seed(123); sample_idx <- sample(seq_len(ncol(expr_mat)), n_samples); expr_subset <- expr_mat[, sample_idx]; metadata_subset <- rv$unified_metadata[sample_idx, ] } else { expr_subset <- expr_mat; metadata_subset <- rv$unified_metadata }
    cor_matrix <- cor(expr_subset, use = "pairwise.complete.obs")
    dist_matrix <- as.dist(1 - cor_matrix)
    hclust_result <- hclust(dist_matrix, method = "ward.D2")
    annotation_col <- data.frame(Dataset = metadata_subset$Dataset, Condition = ifelse(is.na(metadata_subset$Condition), "Unknown", metadata_subset$Condition), row.names = colnames(expr_subset))
    if (gexpipe_has_mixed_platforms(metadata_subset)) {
      annotation_col$Platform <- metadata_subset$Platform
    }
    cor_matrix_ordered <- cor_matrix[hclust_result$order, hclust_result$order]
    dev_fun(file)
    tryCatch({
      pheatmap::pheatmap(cor_matrix_ordered, cluster_rows = FALSE, cluster_cols = FALSE, annotation_col = annotation_col, annotation_row = annotation_col,
        color = grDevices::colorRampPalette(c("#e74c3c", "white", "#3498db"))(100),
        main = if (before) "Hierarchical Clustering - Before Batch Correction\n(Samples cluster by Dataset)" else "Hierarchical Clustering - After Batch Correction\n(Samples cluster by Condition)",
        fontsize = 8, fontsize_row = 7, fontsize_col = 7, show_rownames = FALSE, show_colnames = FALSE)
    }, error = function(e) { plot.new(); text(0.5, 0.5, paste("Error:", e$message), cex = 1) })
    dev.off()
  }
  output$download_hclust_before_png <- downloadHandler(
    filename = function() "Batch_Hclust_Before.png",
    content = function(file) batch_hclust_to_file(file, function(f) png(f, width = 10, height = 8, res = IMAGE_DPI, units = "in", bg = "white"), before = TRUE)
  )
  output$download_hclust_before_jpg <- downloadHandler(
    filename = function() "Batch_Hclust_Before.jpg",
    content = function(file) batch_hclust_to_file(file, function(f) jpeg(f, width = 10, height = 8, res = IMAGE_DPI, units = "in", bg = "white", quality = 95), before = TRUE)
  )
  output$download_hclust_before_pdf <- downloadHandler(
    filename = function() "Batch_Hclust_Before.pdf",
    content = function(file) batch_hclust_to_file(file, function(f) pdf(f, width = 10, height = 8, bg = "white"), before = TRUE)
  )
  output$download_hclust_after_png <- downloadHandler(
    filename = function() "Batch_Hclust_After.png",
    content = function(file) batch_hclust_to_file(file, function(f) png(f, width = 10, height = 8, res = IMAGE_DPI, units = "in", bg = "white"), before = FALSE)
  )
  output$download_hclust_after_jpg <- downloadHandler(
    filename = function() "Batch_Hclust_After.jpg",
    content = function(file) batch_hclust_to_file(file, function(f) jpeg(f, width = 10, height = 8, res = IMAGE_DPI, units = "in", bg = "white", quality = 95), before = FALSE)
  )
  output$download_hclust_after_pdf <- downloadHandler(
    filename = function() "Batch_Hclust_After.pdf",
    content = function(file) batch_hclust_to_file(file, function(f) pdf(f, width = 10, height = 8, bg = "white"), before = FALSE)
  )

  # PVCA: rebuild bar plot and ggsave
  batch_pvca_to_plot <- function(before = TRUE) {
    pv <- .batch_pvca_expr_for_plot(before = before)
    if (!isTRUE(pv$ok)) return(NULL)
    subtitle <- if (before) {
      "Dataset / Platform variance should be visible before correction"
    } else {
      "Dataset variance should drop; large Residual is normal (biology + noise in top PCs)"
    }
    title <- paste0("PVCA - ", if (before) "Before" else "After", " Batch Correction")
    .batch_pvca_ggplot(pv$data, title, subtitle)
  }
  output$download_pvca_before_png <- downloadHandler(
    filename = function() "Batch_PVCA_Before.png",
    content = function(file) { p <- batch_pvca_to_plot(before = TRUE); if (!is.null(p)) batch_save_ggplot(p, file, "png") }
  )
  output$download_pvca_before_jpg <- downloadHandler(
    filename = function() "Batch_PVCA_Before.jpg",
    content = function(file) { p <- batch_pvca_to_plot(before = TRUE); if (!is.null(p)) batch_save_ggplot(p, file, "jpeg") }
  )
  output$download_pvca_before_pdf <- downloadHandler(
    filename = function() "Batch_PVCA_Before.pdf",
    content = function(file) { p <- batch_pvca_to_plot(before = TRUE); if (!is.null(p)) batch_save_ggplot(p, file, "pdf") }
  )
  output$download_pvca_after_png <- downloadHandler(
    filename = function() "Batch_PVCA_After.png",
    content = function(file) { p <- batch_pvca_to_plot(before = FALSE); if (!is.null(p)) batch_save_ggplot(p, file, "png") }
  )
  output$download_pvca_after_jpg <- downloadHandler(
    filename = function() "Batch_PVCA_After.jpg",
    content = function(file) { p <- batch_pvca_to_plot(before = FALSE); if (!is.null(p)) batch_save_ggplot(p, file, "jpeg") }
  )
  output$download_pvca_after_pdf <- downloadHandler(
    filename = function() "Batch_PVCA_After.pdf",
    content = function(file) { p <- batch_pvca_to_plot(before = FALSE); if (!is.null(p)) batch_save_ggplot(p, file, "pdf") }
  )
  
}


