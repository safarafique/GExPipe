# ==============================================================================
# SERVER_NORMALIZE.R - Step 2: Normalization Module
# ==============================================================================

server_normalize <- function(input, output, session, rv) {

  .norm_plot_context <- function() {
    mixed <- FALSE
    if (!is.null(rv$unified_metadata)) {
      mixed <- gexpipe_has_mixed_platforms(rv$unified_metadata)
    } else {
      n_micro <- length(if (is.null(rv$micro_expr_list)) list() else rv$micro_expr_list)
      n_rna <- length(if (is.null(rv$rna_counts_list)) list() else rv$rna_counts_list)
      mixed <- n_micro > 0L && n_rna > 0L
    }
    gq_on <- !is.null(input$apply_global_quantile) && isTRUE(input$apply_global_quantile)
    list(mixed = mixed, gq_on = gq_on, mixed_no_gq = mixed && !gq_on, mixed_gq = mixed && gq_on)
  }

  output$normalization_log_micro <- renderText({
    if (!is.null(rv$normalization_log_micro) && nzchar(rv$normalization_log_micro)) {
      rv$normalization_log_micro
    } else {
      "Apply Normalization to see the microarray run log."
    }
  })
  output$normalization_log_rna <- renderText({
    if (!is.null(rv$normalization_log_rna) && nzchar(rv$normalization_log_rna)) {
      rv$normalization_log_rna
    } else {
      "Apply Normalization to see the RNA-seq run log."
    }
  })

  # Store last ggplot for each normalization plot (for download)
  norm_plots <- reactiveValues(
    plot = NULL, density = NULL, qq = NULL, median_range = NULL,
    distribution_overlap = NULL, ma_plot = NULL, mean_variance = NULL,
    corr_before = NULL, corr_after = NULL
  )

  .norm_active_de_method <- function() {
    if (identical(input$analysis_type, "parallel")) {
      if (!is.null(input$de_method_rna) && nzchar(input$de_method_rna)) {
        input$de_method_rna
      } else {
        "deseq2"
      }
    } else if (!is.null(input$de_method) && nzchar(input$de_method)) {
      input$de_method
    } else {
      "limma"
    }
  }

  output$norm_auto_guide_ui <- renderUI({
    gexpipe_ui_norm_auto_guide(input$analysis_type, .norm_active_de_method())
  })
  output$norm_manual_guide_ui <- renderUI({
    gexpipe_ui_norm_manual_guide(input$analysis_type, .norm_active_de_method())
  })
  output$norm_auto_guide_parallel_ui <- renderUI({
    gexpipe_ui_norm_auto_guide("parallel", .norm_active_de_method())
  })
  output$norm_manual_guide_parallel_ui <- renderUI({
    gexpipe_ui_norm_manual_guide("parallel", .norm_active_de_method())
  })

  # Auto vs manual: Auto forces pipeline defaults on Apply; radios exist only in Manual.
  observe({
    mode <- if (is.null(input$normalize_mode) || !nzchar(input$normalize_mode)) "auto" else input$normalize_mode
    if (!requireNamespace("shinyjs", quietly = TRUE)) return()
    if (identical(mode, "auto")) {
      tryCatch(updateRadioButtons(session, "micro_norm_method", selected = "quantile"), error = function(e) NULL)
      tryCatch(updateRadioButtons(session, "rnaseq_norm_method", selected = "TMM"), error = function(e) NULL)
    }
  })

  observe({
    mode <- if (is.null(input$normalize_mode_parallel) || !nzchar(input$normalize_mode_parallel)) {
      "auto"
    } else {
      input$normalize_mode_parallel
    }
    if (!requireNamespace("shinyjs", quietly = TRUE)) return()
    if (identical(mode, "auto")) {
      tryCatch(updateRadioButtons(session, "micro_norm_method_parallel", selected = "quantile"), error = function(e) NULL)
      tryCatch(updateRadioButtons(session, "rnaseq_norm_method_parallel", selected = "TMM"), error = function(e) NULL)
      tryCatch(shinyjs::disable("micro_norm_method_parallel"), error = function(e) NULL)
      tryCatch(shinyjs::disable("rnaseq_norm_method_parallel"), error = function(e) NULL)
    } else {
      tryCatch(shinyjs::enable("micro_norm_method_parallel"), error = function(e) NULL)
      tryCatch(shinyjs::enable("rnaseq_norm_method_parallel"), error = function(e) NULL)
    }
  })

  output$normalization_timer <- renderText({
    if (!isTRUE(rv$normalize_running) || is.null(rv$normalize_start)) return("00:00")
    invalidateLater(1000, session)
    elapsed <- as.integer(difftime(Sys.time(), rv$normalize_start, units = "secs"))
    sprintf("%02d:%02d", elapsed %/% 60, elapsed %% 60)
  })

  # Single source of truth: R/gexp_normalize_pipeline.R
  .apply_norm_pipeline_to_rv <- function(rv, micro_norm_method, rnaseq_norm_method, de_method,
                                       apply_global_quantile = TRUE,
                                       keep_platforms_separate = FALSE) {
    micro_list <- if (is.null(rv$micro_expr_list)) list() else rv$micro_expr_list
    rna_list <- if (is.null(rv$rna_counts_list)) list() else rv$rna_counts_list
    if (length(micro_list) == 0L && length(rna_list) == 0L) {
      stop("No expression data to normalize. Complete Step 1 (Download) first.")
    }

    norm_out <- .gexpipe_call(
      "gexp_normalize_and_intersect",
      micro_expr_list = micro_list,
      rna_counts_list = rna_list,
      micro_norm_method = micro_norm_method,
      rnaseq_norm_method = rnaseq_norm_method,
      micro_cel_paths = rv$micro_cel_paths,
      platform_per_gse = rv$platform_per_gse,
      micro_eset_list = rv$micro_eset_list,
      de_method = de_method,
      apply_global_quantile = isTRUE(apply_global_quantile),
      keep_platforms_separate = isTRUE(keep_platforms_separate)
    )

    rv$common_genes <- norm_out$common_genes
    rv$all_expr_norm_list <- norm_out$all_expr_norm_list
    rv$expr_micro <- norm_out$expr_micro
    rv$expr_rna <- norm_out$expr_rna
    rv$combined_expr_before_global_norm <- norm_out$combined_expr_before_global
    rv$combined_expr <- norm_out$combined_expr
    rv$raw_counts_for_deseq2 <- norm_out$raw_counts_for_deseq2
    rv$unified_metadata <- gexp_enrich_unified_metadata_with_full_pdata(
      norm_out$unified_metadata,
      rna_metadata_list = rv$rna_metadata_list,
      micro_metadata_list = rv$micro_metadata_list
    )
    rv$normalization_stats <- norm_out$normalization_stats
    rv$normalization_summary_table <- norm_out$normalization_summary_table

    if (!is.null(norm_out$raw_counts_for_deseq2) && !is.null(norm_out$raw_counts_metadata)) {
      rv$raw_counts_metadata <- norm_out$raw_counts_metadata
    } else {
      rv$raw_counts_metadata <- NULL
    }

    stats <- norm_out$normalization_stats
    initial_total <- stats$initial_total
    after_filter_total <- stats$after_filter_total
    rnaseq_removed <- stats$rnaseq_removed
    final_count <- stats$final_count

    rv$normalization_caption <- if (isTRUE(keep_platforms_separate)) {
      paste0(
        "Parallel normalization: each platform kept its own gene set. ",
        "Microarray: ", format(if (is.null(rv$expr_micro)) 0L else nrow(rv$expr_micro), big.mark = ","),
        " genes. RNA-seq: ",
        format(if (is.null(rv$expr_rna)) 0L else nrow(rv$expr_rna), big.mark = ","),
        " genes. Symbol overlap (", format(final_count, big.mark = ","),
        ") is information only - batch and DE do not mix the matrices."
      )
    } else {
      paste0(
        "Gene Expression Normalization Pipeline: Starting with ",
        format(initial_total, big.mark = ","), " total genes across ",
        length(norm_out$all_expr_norm_list), " dataset(s)",
        if (rnaseq_removed > 0) {
          paste0(", ", format(rnaseq_removed, big.mark = ","),
                 " genes were removed due to low-expression filtering (RNA-seq)")
        },
        ", resulting in ", format(after_filter_total, big.mark = ","),
        " genes after individual dataset normalization. ",
        "After automatic filtering to common genes (intersection), ",
        format(final_count, big.mark = ","),
        " high-confidence genes present in all datasets were retained for downstream analysis."
      )
    }

    if (isTRUE(keep_platforms_separate)) {
      log_text <- norm_out$log_text
    } else {
      log_text <- paste0(
        norm_out$log_text,
        "\n\u2713 Normalization Complete!\n",
        "\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\n",
        "Gene Statistics Summary:\n",
        "  Initial total genes:     ", format(initial_total, big.mark = ","), "\n"
      )
      if (rnaseq_removed > 0) {
        log_text <- paste0(
          log_text,
          "  Removed (low expression): ", format(rnaseq_removed, big.mark = ","), "\n",
          "  After filtering:         ", format(after_filter_total, big.mark = ","), "\n"
        )
      }
      log_text <- paste0(
        log_text,
        "  Gene Filtering:           Filtered to common genes (intersection)\n",
        "  Common genes retained: ", format(final_count, big.mark = ","), "\n",
        "\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\n",
        "Final Dataset:\n",
        "  Genes:   ", format(nrow(rv$combined_expr), big.mark = ","), "\n",
        "  Samples: ", format(ncol(rv$combined_expr), big.mark = ","), "\n",
        "\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\u2501\n",
        "\nNote: DE analysis applies independent gene filtering (filterByExpr) at Step 6.\n",
        "      Batch correction may apply an optional variance filter in Step 5.\n"
      )
    }

    .norm_method_label <- function(mat) {
      info <- attr(mat, "normalization_info")
      if (!is.null(info) && !is.null(info$method) && nzchar(as.character(info$method)[[1L]])) {
        as.character(info$method)[[1L]]
      } else {
        "auto"
      }
    }
    micro_method_lab <- if (!is.null(rv$expr_micro)) {
      paste(unique(vapply(norm_out$all_expr_norm_list[intersect(names(micro_list), names(norm_out$all_expr_norm_list))],
                          .norm_method_label, character(1))), collapse = ", ")
    } else {
      ""
    }
    rna_method_lab <- if (!is.null(rv$expr_rna)) {
      paste(unique(vapply(norm_out$all_expr_norm_list[intersect(names(rna_list), names(norm_out$all_expr_norm_list))],
                          .norm_method_label, character(1))), collapse = ", ")
    } else {
      ""
    }
    rv$last_norm_platform_summary <- list(
      keep_separate = isTRUE(keep_platforms_separate),
      micro_genes = if (is.null(rv$expr_micro)) 0L else nrow(rv$expr_micro),
      micro_samples = if (is.null(rv$expr_micro)) 0L else ncol(rv$expr_micro),
      micro_method = micro_method_lab,
      rna_genes = if (is.null(rv$expr_rna)) 0L else nrow(rv$expr_rna),
      rna_samples = if (is.null(rv$expr_rna)) 0L else ncol(rv$expr_rna),
      rna_method = rna_method_lab,
      display_genes = nrow(rv$combined_expr),
      display_samples = ncol(rv$combined_expr)
    )
    rv$normalization_log_micro <- norm_out$log_text_micro
    rv$normalization_log_rna <- norm_out$log_text_rna

    list(
      log_text = log_text,
      log_text_micro = norm_out$log_text_micro,
      log_text_rna = norm_out$log_text_rna,
      total_genes = nrow(rv$combined_expr),
      total_samples = ncol(rv$combined_expr),
      keep_separate = isTRUE(keep_platforms_separate),
      platform_summary = rv$last_norm_platform_summary
    )
  }
  
  observeEvent(input$analysis_type, {
    if (identical(input$analysis_type, "merged")) {
      tryCatch(
        updateCheckboxInput(session, "apply_global_quantile", value = TRUE),
        error = function(e) NULL
      )
    } else if (identical(input$analysis_type, "parallel")) {
      tryCatch(
        updateCheckboxInput(session, "apply_global_quantile", value = FALSE),
        error = function(e) NULL
      )
    }
  })

  # Both buttons must run this handler. shinyjs::click() on the hidden
  # legacy button does nothing in Parallel (conditionalPanel display:none),
  # which looks like the app froze after download.
  observeEvent(
    list(input$apply_normalization, input$apply_normalization_parallel),
    {
    if (!isTRUE(rv$download_complete)) {
      showNotification(
        tags$div(icon("exclamation-triangle"), tags$strong(" Step 1 required:"),
                 " Complete data download (Step 1) before normalizing."),
        type = "error", duration = 6)
      return()
    }
    
    # Disable button and show loading
    shinyjs::disable("apply_normalization")
    shinyjs::html("apply_normalization",
                  HTML('<i class="fa fa-spinner fa-spin"></i> Normalizing...'))
    tryCatch(shinyjs::disable("apply_normalization_parallel"), error = function(e) NULL)
    tryCatch(
      shinyjs::html("apply_normalization_parallel",
                    HTML('<i class="fa fa-spinner fa-spin"></i> Normalizing...')),
      error = function(e) NULL
    )
    
    rv$normalize_start <- Sys.time()
    rv$normalize_running <- TRUE

    .norm_reset_ui <- function() {
      rv$normalize_running <- FALSE
      tryCatch(shinyjs::enable("apply_normalization"), error = function(e) NULL)
      tryCatch(
        shinyjs::html("apply_normalization", HTML('<i class="fa fa-check-circle"></i> Apply Normalization')),
        error = function(e) NULL
      )
      tryCatch(shinyjs::enable("apply_normalization_parallel"), error = function(e) NULL)
      tryCatch(
        shinyjs::html("apply_normalization_parallel", HTML('<i class="fa fa-check-circle"></i> Apply Normalization')),
        error = function(e) NULL
      )
      removeNotification("normalize_processing")
    }
    
    # Show processing notification
    showNotification(
      tags$div(
        tags$div(class = "status-indicator processing"),
        tags$strong("Normalization in progress..."),
        tags$br(),
        tags$span("This may take a few minutes. Please wait..."),
        style = "font-size: 13px;"
      ),
      type = "message",
      duration = NULL,
      id = "normalize_processing"
    )
    
    withProgress(message = 'Normalizing...', value = 0, {
      keep_sep <- identical(input$analysis_type, "parallel") || isTRUE(rv$merge_after_de)
      mode <- if (isTRUE(keep_sep)) {
        if (is.null(input$normalize_mode_parallel) || !nzchar(input$normalize_mode_parallel)) {
          "auto"
        } else {
          input$normalize_mode_parallel
        }
      } else if (is.null(input$normalize_mode) || !nzchar(input$normalize_mode)) {
        "auto"
      } else {
        input$normalize_mode
      }
      de_method <- if (isTRUE(keep_sep)) {
        if (!is.null(input$de_method_rna) && nzchar(input$de_method_rna)) {
          input$de_method_rna
        } else {
          "deseq2"
        }
      } else if (!is.null(input$de_method)) {
        input$de_method
      } else {
        "limma"
      }
      if (identical(mode, "auto")) {
        micro_norm_method <- "auto"
        rnaseq_norm_method <- "auto"
      } else if (isTRUE(keep_sep)) {
        micro_norm_method <- if (!is.null(input$micro_norm_method_parallel)) input$micro_norm_method_parallel else "quantile"
        rnaseq_norm_method <- if (!is.null(input$rnaseq_norm_method_parallel)) input$rnaseq_norm_method_parallel else "TMM"
      } else {
        micro_norm_method <- if (!is.null(input$micro_norm_method)) input$micro_norm_method else "quantile"
        rnaseq_norm_method <- if (!is.null(input$rnaseq_norm_method)) input$rnaseq_norm_method else "TMM"
      }
      # Count-based DE (RNA-seq only or Parallel RNA) must not use leftover TMM for DE.
      # Merged always needs a log-scale RNA matrix for the one limma DE.
      if (.gexpipe_is_count_de(de_method) && !identical(input$analysis_type, "merged")) {
        rnaseq_norm_method <- "auto"
      }
      apply_gq <- if (isTRUE(keep_sep)) {
        FALSE
      } else if (!is.null(input$apply_global_quantile)) {
        isTRUE(input$apply_global_quantile)
      } else {
        TRUE
      }
      rv$last_micro_norm_method <- micro_norm_method
      rv$last_rnaseq_norm_method <- rnaseq_norm_method
      rv$last_apply_global_quantile <- apply_gq
      rv$last_de_method <- de_method
      rv$last_keep_platforms_separate <- keep_sep

      norm_res <- tryCatch(
        .apply_norm_pipeline_to_rv(
          rv, micro_norm_method, rnaseq_norm_method, de_method,
          apply_global_quantile = apply_gq,
          keep_platforms_separate = keep_sep
        ),
        error = function(e) {
          .norm_reset_ui()
          showNotification(
            tags$div(
              icon("exclamation-triangle"),
              tags$strong(" Normalization failed: "),
              conditionMessage(e)
            ),
            type = "error", duration = 12
          )
          NULL
        }
      )
      if (is.null(norm_res)) return()

      log_text <- norm_res$log_text
      total_genes <- norm_res$total_genes
      total_samples <- norm_res$total_samples

      rv$normalization_complete <- TRUE
      rv$normalize_running <- FALSE
      
      output$normalization_log <- renderText({ log_text })
      output$normalization_log_micro <- renderText({
        if (!is.null(norm_res$log_text_micro)) norm_res$log_text_micro else "Microarray run log will appear after Apply Normalization."
      })
      output$normalization_log_rna <- renderText({
        if (!is.null(norm_res$log_text_rna)) norm_res$log_text_rna else "RNA-seq run log will appear after Apply Normalization."
      })
      
      shinyjs::enable("apply_normalization")
      shinyjs::html("apply_normalization",
                    HTML('<i class="fa fa-check-circle"></i> Apply Normalization'))
      tryCatch(shinyjs::enable("apply_normalization_parallel"), error = function(e) NULL)
      tryCatch(
        shinyjs::html("apply_normalization_parallel",
                      HTML('<i class="fa fa-check-circle"></i> Apply Normalization')),
        error = function(e) NULL
      )
      
      removeNotification("normalize_processing")
      
      sm <- norm_res$platform_summary
      showNotification(
        if (isTRUE(norm_res$keep_separate) && !is.null(sm)) {
          tags$div(
            tags$strong("OK Normalization complete - two methods (not one joint method)."),
            tags$br(),
            tags$span(
              "Microarray: ", format(sm$micro_samples, big.mark = ","), " samples, ",
              format(sm$micro_genes, big.mark = ","), " genes (", sm$micro_method, ")."
            ),
            tags$br(),
            tags$span(
              "RNA-seq: ", format(sm$rna_samples, big.mark = ","), " samples, ",
              format(sm$rna_genes, big.mark = ","), " genes (", sm$rna_method,
              " for plots; raw counts kept for DESeq2/edgeR/voom)."
            ),
            tags$br(),
            tags$span(
              "Display union: ", format(sm$display_genes, big.mark = ","), " genes | ",
              format(sm$display_samples, big.mark = ","), " samples (storage only; not one shared normalize)."
            ),
            style = "font-size: 13px;"
          )
        } else {
          tags$div(
            tags$strong("OK Normalization complete!"),
            tags$br(),
            tags$span("Genes: ", format(total_genes, big.mark = ","),
                      " | Samples: ", format(total_samples, big.mark = ",")),
            style = "font-size: 13px;"
          )
        },
        type = "message", duration = 10
      )
    })

    if (!isTRUE(rv$normalization_complete)) {
      rv$normalize_running <- FALSE
    }
  },
  ignoreInit = TRUE
  )
  
  # ==========================================================================
  # AUTO-NORMALIZE FOR COUNT-BASED METHODS (silent, no UI interaction)
  # ==========================================================================
  # When DESeq2 is selected + download finishes, auto-run normalization so
  # downstream steps (WGCNA, heatmaps, etc.) have normalized data - the user
  # stays on the current tab and just sees a brief notification.
  observeEvent(rv$download_complete, {
    if (!isTRUE(rv$download_complete)) return()
    if (is.null(input$de_method) || !(input$de_method %in% c("deseq2", "edger", "limma_voom"))) return()
    if (isTRUE(rv$normalization_complete)) return()
    if (identical(input$analysis_type, "parallel")) return()
    
    # Run the same normalization logic silently via R/ helper
    tryCatch({
      rv$normalize_running <- TRUE
      de_method <- if (!is.null(input$de_method)) input$de_method else "limma"
      apply_gq <- if (!is.null(input$apply_global_quantile)) isTRUE(input$apply_global_quantile) else TRUE
      .apply_norm_pipeline_to_rv(rv, "auto", "auto", de_method, apply_global_quantile = apply_gq)
      rv$last_micro_norm_method <- "auto"
      rv$last_rnaseq_norm_method <- "auto"
      rv$last_apply_global_quantile <- apply_gq
      rv$last_de_method <- de_method
      rv$last_keep_platforms_separate <- FALSE
      rv$normalization_complete <- TRUE
      rv$normalize_running <- FALSE
      
      method_label <- if (!is.null(input$de_method) && input$de_method == "edger") "edgeR" else "DESeq2"
      showNotification(
        tags$div(icon("check-circle"),
                 tags$strong(paste0(" Auto-normalization complete (", method_label, " mode).")),
                 tags$br(),
                 tags$span(paste0("Genes: ", format(nrow(rv$combined_expr), big.mark = ","),
                                  " | Samples: ", format(ncol(rv$combined_expr), big.mark = ",")))),
        type = "message", duration = 5)
      
    }, error = function(e) {
      rv$normalize_running <- FALSE
      showNotification(
        tags$div(icon("exclamation-triangle"),
                 tags$strong(" Auto-normalization failed: "), conditionMessage(e)),
        type = "error", duration = 8)
    })
  })
  
  .norm_platform_boxplot <- function(expr, title, fill = "#93c5fd") {
    if (is.null(expr) || !is.matrix(expr) || ncol(expr) < 1L) {
      plot.new()
      text(0.5, 0.5, "Apply Normalization to see this plot.", cex = 1.1, col = "gray40")
      return(invisible(NULL))
    }
    use <- expr
    if (nrow(use) > 8000L) {
      withr::local_seed(123)
      use <- use[sample.int(nrow(use), 8000L), , drop = FALSE]
    }
    if (ncol(use) > 50L) {
      withr::local_seed(123)
      use <- use[, sample.int(ncol(use), 50L), drop = FALSE]
    }
    df <- data.frame(
      Expression = as.vector(use),
      Sample = rep(colnames(use), each = nrow(use)),
      stringsAsFactors = FALSE
    )
    ggplot2::ggplot(df, ggplot2::aes(x = Sample, y = Expression)) +
      ggplot2::geom_boxplot(fill = fill, outlier.size = 0.4, alpha = 0.85) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = title, y = "Expression") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(face = "bold", size = 13)
      )
  }

  .norm_platform_density <- function(expr, title, color = "#2563eb") {
    if (is.null(expr) || !is.matrix(expr) || ncol(expr) < 1L) {
      plot.new()
      text(0.5, 0.5, "Apply Normalization to see this plot.", cex = 1.1, col = "gray40")
      return(invisible(NULL))
    }
    dd <- gexp_qc_prepare_density_data(expr, max_samples = 50L)
    plot(
      dd$first, main = title, xlab = "Expression", col = color, lwd = 2,
      ylim = c(0, max(dd$first$y, na.rm = TRUE) * 1.2)
    )
    if (length(dd$others) > 0L) {
      for (i in seq_along(dd$others)) {
        graphics::lines(dd$others[[i]], col = dd$colors[i + 1L], lwd = 1)
      }
    }
  }

  .norm_is_parallel <- function() {
    identical(input$analysis_type, "parallel") || isTRUE(rv$merge_after_de)
  }

  .norm_dataset_labels <- function(sample_ids) {
    labs <- rep("Dataset", length(sample_ids))
    meta <- rv$unified_metadata
    if (!is.null(meta) && is.data.frame(meta)) {
      key <- if ("SampleID" %in% names(meta)) {
        match(sample_ids, as.character(meta$SampleID))
      } else {
        match(sample_ids, rownames(meta))
      }
      if ("Dataset" %in% names(meta)) {
        hit <- as.character(meta$Dataset[key])
        labs[!is.na(hit) & nzchar(hit)] <- hit[!is.na(hit) & nzchar(hit)]
      }
    }
    labs
  }

  .norm_platform_comparison <- function(after) {
    if (is.null(after) || !is.matrix(after) || ncol(after) < 1L) {
      return(NULL)
    }
    before_all <- rv$combined_expr_before_global_norm
    if (is.null(before_all)) before_all <- rv$combined_expr_raw
    if (is.null(before_all) || !is.matrix(before_all)) {
      return(list(before = after, after = after))
    }
    samp <- intersect(colnames(after), colnames(before_all))
    genes <- intersect(rownames(after), rownames(before_all))
    if (length(samp) == 0L || length(genes) == 0L) {
      return(list(before = after, after = after))
    }
    list(
      before = before_all[genes, samp, drop = FALSE],
      after = after[genes, samp, drop = FALSE]
    )
  }

  .norm_median_range_plot <- function(expr, title) {
    if (is.null(expr) || !is.matrix(expr) || ncol(expr) < 1L) {
      plot.new()
      text(0.5, 0.5, "Apply Normalization to see this plot.", cex = 1.1, col = "gray40")
      return(invisible(NULL))
    }
    use <- expr
    if (ncol(use) > 50L) {
      withr::local_seed(123)
      use <- use[, sample.int(ncol(use), 50L), drop = FALSE]
    }
    ds <- .norm_dataset_labels(colnames(use))
    plot_data <- data.frame(
      Sample = rep(colnames(use), each = nrow(use)),
      Expression = as.vector(use),
      Dataset = rep(ds, each = nrow(use)),
      stringsAsFactors = FALSE
    )
    ggplot2::ggplot(plot_data, ggplot2::aes(x = Sample, y = Expression, fill = Dataset)) +
      ggplot2::geom_boxplot(alpha = 0.7, outlier.size = 0.4, outlier.alpha = 0.3) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::labs(
        title = title,
        subtitle = "This platform only - datasets from the other platform are not included",
        x = "Sample",
        y = "Expression Value",
        fill = "Dataset"
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 13, hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5, color = "gray50"),
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 7),
        legend.position = "right"
      )
  }

  .norm_distribution_overlap_plot <- function(cmp, title) {
    if (is.null(cmp) || is.null(cmp$after) || ncol(cmp$after) < 1L) {
      plot.new()
      text(0.5, 0.5, "Apply Normalization to see this plot.", cex = 1.1, col = "gray40")
      return(invisible(NULL))
    }
    expr_before <- cmp$before
    expr_after <- cmp$after
    n_genes <- min(20000L, nrow(expr_after))
    if (nrow(expr_after) > n_genes) {
      withr::local_seed(123)
      keep <- sample.int(nrow(expr_after), n_genes)
      expr_before <- expr_before[keep, , drop = FALSE]
      expr_after <- expr_after[keep, , drop = FALSE]
    }
    n_samples <- min(20L, ncol(expr_after))
    if (ncol(expr_after) > n_samples) {
      withr::local_seed(123)
      keep_s <- sample.int(ncol(expr_after), n_samples)
      expr_before <- expr_before[, keep_s, drop = FALSE]
      expr_after <- expr_after[, keep_s, drop = FALSE]
    }
    rows <- list()
    for (i in seq_len(ncol(expr_after))) {
      sample_name <- colnames(expr_after)[i]
      dataset_name <- .norm_dataset_labels(sample_name)[[1L]]
      dens_before <- stats::density(expr_before[, i], na.rm = TRUE)
      dens_after <- stats::density(expr_after[, i], na.rm = TRUE)
      rows[[length(rows) + 1L]] <- data.frame(
        x = dens_before$x, y = dens_before$y,
        Sample = sample_name, Dataset = dataset_name, Stage = "Before",
        stringsAsFactors = FALSE
      )
      rows[[length(rows) + 1L]] <- data.frame(
        x = dens_after$x, y = dens_after$y,
        Sample = sample_name, Dataset = dataset_name, Stage = "After",
        stringsAsFactors = FALSE
      )
    }
    plot_data <- do.call(rbind, rows)
    plot_data$Stage <- factor(plot_data$Stage, levels = c("Before", "After"))
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = x, y = y, color = Stage, group = interaction(Sample, Stage))
    ) +
      ggplot2::geom_line(alpha = 0.65, linewidth = 0.7) +
      ggplot2::scale_color_manual(values = c("Before" = "#e74c3c", "After" = "#2ecc71")) +
      ggplot2::facet_wrap(~ Dataset, scales = "free", ncol = min(2L, length(unique(plot_data$Dataset)))) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::labs(
        title = title,
        subtitle = "This platform only - the other platform is not on this plot",
        x = "Expression Value",
        y = "Density",
        color = "Stage"
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 13, hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5, color = "gray50"),
        legend.position = "top",
        strip.background = ggplot2::element_rect(fill = "#3498db", color = "white"),
        strip.text = ggplot2::element_text(color = "white", face = "bold")
      )
  }

  output$normalization_plot_rna <- renderPlot({
    req(isTRUE(rv$normalization_complete))
    .norm_platform_boxplot(rv$expr_rna, "RNA-seq expression (this platform only)", "#93c5fd")
  })
  output$normalization_density_rna <- renderPlot({
    req(isTRUE(rv$normalization_complete))
    .norm_platform_density(rv$expr_rna, "RNA-seq density (this platform only)", "#2563eb")
  })
  output$normalization_median_range_rna <- renderPlot({
    req(isTRUE(rv$normalization_complete))
    .norm_median_range_plot(rv$expr_rna, "RNA-seq median & range")
  })
  output$normalization_distribution_overlap_rna <- renderPlot({
    req(isTRUE(rv$normalization_complete))
    .norm_distribution_overlap_plot(
      .norm_platform_comparison(rv$expr_rna),
      "RNA-seq distribution overlap"
    )
  })
  output$normalization_plot_micro <- renderPlot({
    req(isTRUE(rv$normalization_complete))
    .norm_platform_boxplot(rv$expr_micro, "Microarray expression (this platform only)", "#fde68a")
  })
  output$normalization_density_micro <- renderPlot({
    req(isTRUE(rv$normalization_complete))
    .norm_platform_density(rv$expr_micro, "Microarray density (this platform only)", "#d97706")
  })
  output$normalization_median_range_micro <- renderPlot({
    req(isTRUE(rv$normalization_complete))
    .norm_median_range_plot(rv$expr_micro, "Microarray median & range")
  })
  output$normalization_distribution_overlap_micro <- renderPlot({
    req(isTRUE(rv$normalization_complete))
    .norm_distribution_overlap_plot(
      .norm_platform_comparison(rv$expr_micro),
      "Microarray distribution overlap"
    )
  })

  # Normalization quality visualization: Box plots showing distribution before/after
  output$normalization_plot <- renderPlot({
    if (isTRUE(.norm_is_parallel())) {
      plot.new()
      text(0.5, 0.5, "Parallel: RNA-seq and microarray plots are separate.", cex = 1.1, col = "gray40")
      return(invisible(NULL))
    }
    req(rv$normalization_complete, rv$combined_expr)
    
    # Use before global normalization if available, otherwise use current combined_expr
    expr_before <- if (!is.null(rv$combined_expr_before_global_norm)) {
      rv$combined_expr_before_global_norm
    } else if (!is.null(rv$combined_expr_raw)) {
      rv$combined_expr_raw
    } else {
      # Fallback: use a subset of current data
      rv$combined_expr
    }
    
    expr_after <- rv$combined_expr
    
    tryCatch({
      # Ensure same dimensions
      common_genes <- intersect(rownames(expr_before), rownames(expr_after))
      common_samples <- intersect(colnames(expr_before), colnames(expr_after))
      
      if (length(common_genes) == 0 || length(common_samples) == 0) {
        stop("No common genes or samples for comparison")
      }
      
      expr_before <- expr_before[common_genes, common_samples, drop = FALSE]
      expr_after <- expr_after[common_genes, common_samples, drop = FALSE]
      
      # Sample a subset of genes for faster plotting (if too many)
      n_genes <- nrow(expr_before)
      if (n_genes > 10000) {
        withr::local_seed(123)
        sample_genes <- sample.int(n_genes, 10000)
        expr_before <- expr_before[sample_genes, ]
        expr_after <- expr_after[sample_genes, ]
      }
      
      # Prepare data for plotting
      n_samples <- min(50, ncol(expr_before))  # Limit to 50 samples for readability
      if (ncol(expr_before) > n_samples) {
        withr::local_seed(123)
        sample_idx <- sample(seq_len(ncol(expr_before)), n_samples)
        expr_before <- expr_before[, sample_idx]
        expr_after <- expr_after[, sample_idx]
      }
      
      # Create data frames for plotting
      plot_data_before <- data.frame(
        Expression = as.vector(expr_before),
        Sample = rep(colnames(expr_before), each = nrow(expr_before)),
        Dataset = rep(rv$unified_metadata$Dataset[match(colnames(expr_before), rv$unified_metadata$SampleID)], 
                     each = nrow(expr_before)),
        Stage = "Before Normalization"
      )
      
      plot_data_after <- data.frame(
        Expression = as.vector(expr_after),
        Sample = rep(colnames(expr_after), each = nrow(expr_after)),
        Dataset = rep(rv$unified_metadata$Dataset[match(colnames(expr_after), rv$unified_metadata$SampleID)], 
                     each = nrow(expr_after)),
        Stage = "After Normalization"
      )
      
      # Combine
      plot_data <- rbind(plot_data_before, plot_data_after)
      plot_data$Stage <- factor(plot_data$Stage, levels = c("Before Normalization", "After Normalization"))
      
      # Create box plot
      p <- ggplot(plot_data, aes(x = Stage, y = Expression, fill = Stage)) +
        geom_boxplot(alpha = 0.7, outlier.size = 0.5, outlier.alpha = 0.3) +
        scale_fill_manual(values = c("Before Normalization" = "#e74c3c", 
                                     "After Normalization" = "#2ecc71")) +
        facet_wrap(~ Dataset, scales = "free_y", ncol = min(3, length(unique(plot_data$Dataset)))) +
        theme_bw(base_size = 12) +
        labs(
          title = "Normalization Effect: Expression Distribution Comparison",
          subtitle = paste0("Showing distribution of expression values before and after normalization"),
          x = "",
          y = "Expression Value",
          fill = "Stage"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray50", margin = margin(b = 15)),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",
          legend.title = element_text(face = "bold"),
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_line(color = "gray95"),
          strip.background = element_rect(fill = "#3498db", color = "white"),
          strip.text = element_text(color = "white", face = "bold")
        )
      norm_plots$plot <- p
      p
    }, error = function(e) {
      # Fallback: Simple density plot
      tryCatch({
        plot_data_before <- data.frame(
          Expression = as.vector(rv$combined_expr_raw),
          Stage = "Before Normalization"
        )
        plot_data_after <- data.frame(
          Expression = as.vector(rv$combined_expr),
          Stage = "After Normalization"
        )
        plot_data <- rbind(plot_data_before, plot_data_after)
        plot_data$Stage <- factor(plot_data$Stage, levels = c("Before Normalization", "After Normalization"))
        
        p <- ggplot(plot_data, aes(x = Expression, fill = Stage, color = Stage)) +
          geom_density(alpha = 0.6) +
          scale_fill_manual(values = c("Before Normalization" = "#e74c3c", 
                                       "After Normalization" = "#2ecc71")) +
          scale_color_manual(values = c("Before Normalization" = "#c0392b", 
                                       "After Normalization" = "#27ae60")) +
          theme_bw(base_size = 14) +
          labs(
            title = "Normalization Effect: Expression Distribution",
            x = "Expression Value",
            y = "Density",
            fill = "Stage",
            color = "Stage"
          ) +
          theme(
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            legend.position = "top"
          )
        norm_plots$plot <- p
        p
      }, error = function(e2) {
        plot.new()
        text(0.5, 0.5, "Unable to generate normalization plot", cex = 1.2)
      })
    })
  })
  
  # Helper function to get expression data for comparison
  get_expr_comparison <- function() {
    expr_before <- if (!is.null(rv$combined_expr_before_global_norm)) {
      rv$combined_expr_before_global_norm
    } else if (!is.null(rv$combined_expr_raw)) {
      rv$combined_expr_raw
    } else {
      rv$combined_expr
    }
    
    expr_after <- rv$combined_expr
    
    # Ensure same dimensions
    common_genes <- intersect(rownames(expr_before), rownames(expr_after))
    common_samples <- intersect(colnames(expr_before), colnames(expr_after))
    
    if (length(common_genes) == 0 || length(common_samples) == 0) {
      return(NULL)
    }
    
    expr_before <- expr_before[common_genes, common_samples, drop = FALSE]
    expr_after <- expr_after[common_genes, common_samples, drop = FALSE]
    
    return(list(before = expr_before, after = expr_after))
  }
  
  # Plot 2: Density plots showing overall distribution
  output$normalization_density <- renderPlot({
    req(rv$normalization_complete, rv$combined_expr)
    
    expr_data <- get_expr_comparison()
    if (is.null(expr_data)) {
      plot.new()
      text(0.5, 0.5, "Unable to generate plot", cex = 1.2)
      return()
    }
    
    expr_before <- expr_data$before
    expr_after <- expr_data$after
    
    tryCatch({
      # Sample genes if too many
      n_genes <- nrow(expr_before)
      if (n_genes > 50000) {
        withr::local_seed(123)
        sample_genes <- sample(seq_len(n_genes), 50000)
        expr_before <- expr_before[sample_genes, ]
        expr_after <- expr_after[sample_genes, ]
      }
      
      plot_data_before <- data.frame(
        Expression = as.vector(expr_before),
        Stage = "Before Normalization"
      )
      plot_data_after <- data.frame(
        Expression = as.vector(expr_after),
        Stage = "After Normalization"
      )
      plot_data <- rbind(plot_data_before, plot_data_after)
      plot_data$Stage <- factor(plot_data$Stage, levels = c("Before Normalization", "After Normalization"))
      
      p <- ggplot(plot_data, aes(x = Expression, fill = Stage, color = Stage)) +
        geom_density(alpha = 0.6, linewidth = 0.8) +
        scale_fill_manual(values = c("Before Normalization" = "#e74c3c", 
                                     "After Normalization" = "#2ecc71")) +
        scale_color_manual(values = c("Before Normalization" = "#c0392b", 
                                     "After Normalization" = "#27ae60")) +
        theme_bw(base_size = 14) +
        labs(
          title = "Normalization Effect: Overall Expression Distribution",
          subtitle = "Density plots showing expression value distributions",
          x = "Expression Value",
          y = "Density",
          fill = "Stage",
          color = "Stage"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
          legend.position = "top",
          legend.title = element_text(face = "bold"),
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_line(color = "gray95")
        )
      norm_plots$density <- p
      p
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
    })
  })
  
  # Plot 3: Sample correlation heatmap before normalization
  output$normalization_corr_before <- renderPlot({
    req(rv$normalization_complete, rv$combined_expr)
    
    expr_data <- get_expr_comparison()
    if (is.null(expr_data)) {
      plot.new()
      text(0.5, 0.5, "Unable to generate plot", cex = 1.2)
      return()
    }
    
    expr_before <- expr_data$before
    
    tryCatch({
      # Limit samples for performance
      n_samples <- min(30, ncol(expr_before))
      if (ncol(expr_before) > n_samples) {
        withr::local_seed(123)
        sample_idx <- sample(seq_len(ncol(expr_before)), n_samples)
        expr_before <- expr_before[, sample_idx]
      }
      
      # Calculate correlation
      cor_matrix <- cor(expr_before, use = "pairwise.complete.obs")
      
      # Convert to long format
      cor_df <- expand.grid(Sample1 = colnames(cor_matrix), Sample2 = colnames(cor_matrix))
      cor_df$Correlation <- as.vector(cor_matrix)
      
      # Add dataset info
      cor_df$Dataset1 <- rv$unified_metadata$Dataset[match(cor_df$Sample1, rv$unified_metadata$SampleID)]
      cor_df$Dataset2 <- rv$unified_metadata$Dataset[match(cor_df$Sample2, rv$unified_metadata$SampleID)]
      
      p <- ggplot(cor_df, aes(x = Sample1, y = Sample2, fill = Correlation)) +
        geom_tile() +
        scale_fill_gradient2(low = "#e74c3c", mid = "white", high = "#3498db", 
                            midpoint = 0.5, limits = c(0, 1)) +
        theme_bw(base_size = 10) +
        labs(
          title = "Sample Correlation - Before Normalization",
          subtitle = "Higher correlation (blue) indicates similar expression profiles",
          x = "",
          y = "",
          fill = "Correlation"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray50"),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          legend.position = "right"
        )
      norm_plots$corr_before <- p
      p
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
    })
  })
  
  # Plot 4: Sample correlation heatmap after normalization
  output$normalization_corr_after <- renderPlot({
    req(rv$normalization_complete, rv$combined_expr)
    
    expr_after <- rv$combined_expr
    
    tryCatch({
      # Limit samples for performance
      n_samples <- min(30, ncol(expr_after))
      if (ncol(expr_after) > n_samples) {
        withr::local_seed(123)
        sample_idx <- sample(seq_len(ncol(expr_after)), n_samples)
        expr_after <- expr_after[, sample_idx]
      }
      
      # Calculate correlation
      cor_matrix <- cor(expr_after, use = "pairwise.complete.obs")
      
      # Convert to long format
      cor_df <- expand.grid(Sample1 = colnames(cor_matrix), Sample2 = colnames(cor_matrix))
      cor_df$Correlation <- as.vector(cor_matrix)
      
      # Add dataset info
      cor_df$Dataset1 <- rv$unified_metadata$Dataset[match(cor_df$Sample1, rv$unified_metadata$SampleID)]
      cor_df$Dataset2 <- rv$unified_metadata$Dataset[match(cor_df$Sample2, rv$unified_metadata$SampleID)]
      
      p <- ggplot(cor_df, aes(x = Sample1, y = Sample2, fill = Correlation)) +
        geom_tile() +
        scale_fill_gradient2(low = "#e74c3c", mid = "white", high = "#3498db", 
                            midpoint = 0.5, limits = c(0, 1)) +
        theme_bw(base_size = 10) +
        labs(
          title = "Sample Correlation - After Normalization",
          subtitle = "Normalized samples should show more uniform correlations",
          x = "",
          y = "",
          fill = "Correlation"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray50"),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          legend.position = "right"
        )
      norm_plots$corr_after <- p
      p
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
    })
  })
  
  # Plot 5: Quantile-Quantile (Q-Q) plot
  output$normalization_qq <- renderPlot({
    req(rv$normalization_complete, rv$combined_expr)
    
    expr_data <- get_expr_comparison()
    if (is.null(expr_data)) {
      plot.new()
      text(0.5, 0.5, "Unable to generate plot", cex = 1.2)
      return()
    }
    
    expr_before <- expr_data$before
    expr_after <- expr_data$after
    
    tryCatch({
      # Sample genes for Q-Q plot
      n_genes <- min(10000, nrow(expr_before))
      if (nrow(expr_before) > n_genes) {
        withr::local_seed(123)
        sample_genes <- sample(seq_len(nrow(expr_before)), n_genes)
        expr_before <- expr_before[sample_genes, ]
        expr_after <- expr_after[sample_genes, ]
      }
      
      # Get quantiles
      q_before <- quantile(as.vector(expr_before), probs = seq(0, 1, 0.01), na.rm = TRUE)
      q_after <- quantile(as.vector(expr_after), probs = seq(0, 1, 0.01), na.rm = TRUE)
      
      qq_df <- data.frame(
        Before = q_before,
        After = q_after
      )
      
      # Calculate reference line (y=x)
      min_val <- min(c(qq_df$Before, qq_df$After), na.rm = TRUE)
      max_val <- max(c(qq_df$Before, qq_df$After), na.rm = TRUE)
      
      p <- ggplot(qq_df, aes(x = Before, y = After)) +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
        geom_point(color = "#3498db", alpha = 0.6, size = 2) +
        theme_bw(base_size = 14) +
        labs(
          title = "Quantile-Quantile (Q-Q) Plot",
          subtitle = "Points on diagonal line indicate successful normalization",
          x = "Quantiles - Before Normalization",
          y = "Quantiles - After Normalization"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_line(color = "gray95")
        )
      norm_plots$qq <- p
      p
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
    })
  })
  
  # Plot 6: Median & Range Alignment - Boxplot
  output$normalization_median_range <- renderPlot({
    if (isTRUE(.norm_is_parallel())) {
      plot.new()
      text(0.5, 0.5, "Parallel: RNA-seq and microarray median/range plots are separate.", cex = 1.1, col = "gray40")
      return(invisible(NULL))
    }
    req(rv$normalization_complete, rv$combined_expr)
    
    expr_after <- rv$combined_expr
    
    tryCatch({
      # Limit samples for readability
      n_samples <- min(50, ncol(expr_after))
      if (ncol(expr_after) > n_samples) {
        withr::local_seed(123)
        sample_idx <- sample(seq_len(ncol(expr_after)), n_samples)
        expr_after <- expr_after[, sample_idx]
      }
      
      # Calculate median and range for each sample
      sample_stats <- data.frame(
        Sample = colnames(expr_after),
        Median = apply(expr_after, 2, median, na.rm = TRUE),
        Q25 = apply(expr_after, 2, quantile, 0.25, na.rm = TRUE),
        Q75 = apply(expr_after, 2, quantile, 0.75, na.rm = TRUE),
        Dataset = rv$unified_metadata$Dataset[match(colnames(expr_after), rv$unified_metadata$SampleID)]
      )
      
      # Create boxplot data
      plot_data <- data.frame(
        Sample = rep(colnames(expr_after), each = nrow(expr_after)),
        Expression = as.vector(expr_after),
        Dataset = rep(sample_stats$Dataset, each = nrow(expr_after))
      )

      ctx <- .norm_plot_context()
      med_sub <- if (isTRUE(ctx$mixed_no_gq)) {
        "Mixed platform, global quantile OFF: OK if each GSE block is internally aligned (offset across GSEs is expected)"
      } else if (isTRUE(ctx$mixed_gq)) {
        "Global quantile ON: all medians match visually - alignment is forced; not recommended for mixed microarray + RNA-seq"
      } else {
        "Aligned medians and ranges across samples indicate successful normalization"
      }
      
      p <- ggplot(plot_data, aes(x = Sample, y = Expression, fill = Dataset)) +
        geom_boxplot(alpha = 0.7, outlier.size = 0.5, outlier.alpha = 0.3) +
        theme_bw(base_size = 11) +
        labs(
          title = "Median & Range Alignment",
          subtitle = med_sub,
          x = "Sample",
          y = "Expression Value",
          fill = "Dataset"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
          legend.position = "right",
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_line(color = "gray95")
        )
      norm_plots$median_range <- p
      p
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
    })
  })
  
  # Plot 7: Distribution Overlap - Density Plot
  output$normalization_distribution_overlap <- renderPlot({
    if (isTRUE(.norm_is_parallel())) {
      plot.new()
      text(0.5, 0.5, "Parallel: RNA-seq and microarray overlap plots are separate.", cex = 1.1, col = "gray40")
      return(invisible(NULL))
    }
    req(rv$normalization_complete, rv$combined_expr)
    
    expr_data <- get_expr_comparison()
    if (is.null(expr_data)) {
      plot.new()
      text(0.5, 0.5, "Unable to generate plot", cex = 1.2)
      return()
    }
    
    expr_before <- expr_data$before
    expr_after <- expr_data$after
    
    tryCatch({
      # Sample genes if too many
      n_genes <- min(50000, nrow(expr_before))
      if (nrow(expr_before) > n_genes) {
        withr::local_seed(123)
        sample_genes <- sample(seq_len(nrow(expr_before)), n_genes)
        expr_before <- expr_before[sample_genes, ]
        expr_after <- expr_after[sample_genes, ]
      }
      
      # Limit samples
      n_samples <- min(20, ncol(expr_before))
      if (ncol(expr_before) > n_samples) {
        withr::local_seed(123)
        sample_idx <- sample(seq_len(ncol(expr_before)), n_samples)
        expr_before <- expr_before[, sample_idx]
        expr_after <- expr_after[, sample_idx]
      }
      
      # Create density data for multiple samples
      plot_data_list <- list()
      
      for (i in seq_len(ncol(expr_before))) {
        sample_name <- colnames(expr_before)[i]
        dataset_name <- rv$unified_metadata$Dataset[match(sample_name, rv$unified_metadata$SampleID)]
        
        # Before
        dens_before <- density(expr_before[, i], na.rm = TRUE)
        plot_data_list[[length(plot_data_list) + 1]] <- data.frame(
          x = dens_before$x,
          y = dens_before$y,
          Sample = sample_name,
          Dataset = dataset_name,
          Stage = "Before"
        )
        
        # After
        dens_after <- density(expr_after[, i], na.rm = TRUE)
        plot_data_list[[length(plot_data_list) + 1]] <- data.frame(
          x = dens_after$x,
          y = dens_after$y,
          Sample = sample_name,
          Dataset = dataset_name,
          Stage = "After"
        )
      }
      
      plot_data <- do.call(rbind, plot_data_list)
      plot_data$Stage <- factor(plot_data$Stage, levels = c("Before", "After"))

      ctx <- .norm_plot_context()
      dist_sub <- if (isTRUE(ctx$mixed_no_gq)) {
        "Within each GSE facet, curves should overlap; different peak positions between facets are expected without global quantile"
      } else if (isTRUE(ctx$mixed_gq)) {
        "Global quantile warped all samples to one scale - visual overlap does not mean biologically safe for mixed platforms"
      } else {
        "Overlapping distributions indicate successful normalization"
      }
      
      p <- ggplot(plot_data, aes(x = x, y = y, color = Stage, group = interaction(Sample, Stage))) +
        geom_line(alpha = 0.6, linewidth = 0.7) +
        scale_color_manual(values = c("Before" = "#e74c3c", "After" = "#2ecc71")) +
        facet_wrap(~ Dataset, scales = "free", ncol = min(2, length(unique(plot_data$Dataset)))) +
        theme_bw(base_size = 12) +
        labs(
          title = "Distribution Overlap",
          subtitle = dist_sub,
          x = "Expression Value",
          y = "Density",
          color = "Stage"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
          legend.position = "top",
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_line(color = "gray95"),
          strip.background = element_rect(fill = "#3498db", color = "white"),
          strip.text = element_text(color = "white", face = "bold")
        )
      norm_plots$distribution_overlap <- p
      p
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
    })
  })
  
  # Plot 8: Intensity Bias - MA-Plot
  output$normalization_ma_plot <- renderPlot({
    req(rv$normalization_complete, rv$combined_expr)
    
    expr_data <- get_expr_comparison()
    if (is.null(expr_data)) {
      plot.new()
      text(0.5, 0.5, "Unable to generate plot", cex = 1.2)
      return()
    }
    
    expr_before <- expr_data$before
    expr_after <- expr_data$after
    
    tryCatch({
      # Sample genes
      n_genes <- min(10000, nrow(expr_before))
      if (nrow(expr_before) > n_genes) {
        withr::local_seed(123)
        sample_genes <- sample(seq_len(nrow(expr_before)), n_genes)
        expr_before <- expr_before[sample_genes, ]
        expr_after <- expr_after[sample_genes, ]
      }
      
      # Use first two samples for MA plot (or compare median before vs after)
      if (ncol(expr_before) >= 2) {
        # Compare two samples
        sample1 <- expr_before[, 1]
        sample2 <- expr_before[, 2]
        
        # Calculate M and A
        M_before <- sample1 - sample2
        A_before <- (sample1 + sample2) / 2
        
        sample1_after <- expr_after[, 1]
        sample2_after <- expr_after[, 2]
        M_after <- sample1_after - sample2_after
        A_after <- (sample1_after + sample2_after) / 2
        
        ma_data <- rbind(
          data.frame(M = M_before, A = A_before, Stage = "Before Normalization"),
          data.frame(M = M_after, A = A_after, Stage = "After Normalization")
        )
        ma_data$Stage <- factor(ma_data$Stage, levels = c("Before Normalization", "After Normalization"))
        
        p <- ggplot(ma_data, aes(x = A, y = M, color = Stage)) +
          geom_point(alpha = 0.3, size = 0.5) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
          geom_smooth(method = "loess", se = TRUE, linewidth = 1.2) +
          scale_color_manual(values = c("Before Normalization" = "#e74c3c", 
                                       "After Normalization" = "#2ecc71")) +
          facet_wrap(~ Stage, ncol = 2) +
          theme_bw(base_size = 12) +
          labs(
            title = "Intensity Bias - MA Plot",
            subtitle = "M = log2(sample1) - log2(sample2), A = (log2(sample1) + log2(sample2))/2",
            x = "A (Average Intensity)",
            y = "M (Intensity Difference)",
            color = "Stage"
          ) +
          theme(
            plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
            plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray50"),
            legend.position = "none",
            panel.grid.major = element_line(color = "gray90"),
            panel.grid.minor = element_line(color = "gray95")
          )
      } else {
        # Compare median before vs after
        median_before <- apply(expr_before, 1, median, na.rm = TRUE)
        median_after <- apply(expr_after, 1, median, na.rm = TRUE)
        
        M <- median_before - median_after
        A <- (median_before + median_after) / 2
        
        ma_data <- data.frame(M = M, A = A)
        
        p <- ggplot(ma_data, aes(x = A, y = M)) +
          geom_point(alpha = 0.3, size = 0.5, color = "#3498db") +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
          geom_smooth(method = "loess", se = TRUE, linewidth = 1.2, color = "#2ecc71") +
          theme_bw(base_size = 12) +
          labs(
            title = "Intensity Bias - MA Plot",
            subtitle = "M = median(before) - median(after), A = (median(before) + median(after))/2",
            x = "A (Average Intensity)",
            y = "M (Intensity Difference)"
          ) +
          theme(
            plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
            plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray50"),
            panel.grid.major = element_line(color = "gray90"),
            panel.grid.minor = element_line(color = "gray95")
          )
      }
      norm_plots$ma_plot <- p
      p
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
    })
  })
  
  # Plot 9: Variance Stability - Mean-Variance Plot
  output$normalization_mean_variance <- renderPlot({
    req(rv$normalization_complete, rv$combined_expr)
    
    expr_data <- get_expr_comparison()
    if (is.null(expr_data)) {
      plot.new()
      text(0.5, 0.5, "Unable to generate plot", cex = 1.2)
      return()
    }
    
    expr_before <- expr_data$before
    expr_after <- expr_data$after
    
    tryCatch({
      # Sample genes
      n_genes <- min(10000, nrow(expr_before))
      if (nrow(expr_before) > n_genes) {
        withr::local_seed(123)
        sample_genes <- sample(seq_len(nrow(expr_before)), n_genes)
        expr_before <- expr_before[sample_genes, ]
        expr_after <- expr_after[sample_genes, ]
      }
      
      # Calculate mean and variance for each gene
      mean_before <- rowMeans(expr_before, na.rm = TRUE)
      var_before <- apply(expr_before, 1, var, na.rm = TRUE)
      
      mean_after <- rowMeans(expr_after, na.rm = TRUE)
      var_after <- apply(expr_after, 1, var, na.rm = TRUE)
      
      mv_data <- rbind(
        data.frame(Mean = mean_before, Variance = var_before, Stage = "Before Normalization"),
        data.frame(Mean = mean_after, Variance = var_after, Stage = "After Normalization")
      )
      mv_data$Stage <- factor(mv_data$Stage, levels = c("Before Normalization", "After Normalization"))
      
      # Remove infinite and NA values
      mv_data <- mv_data[is.finite(mv_data$Mean) & is.finite(mv_data$Variance) & 
                        !is.na(mv_data$Mean) & !is.na(mv_data$Variance), ]
      
      p <- ggplot(mv_data, aes(x = Mean, y = Variance, color = Stage)) +
        geom_point(alpha = 0.3, size = 0.5) +
        geom_smooth(method = "loess", se = TRUE, linewidth = 1.2) +
        scale_color_manual(values = c("Before Normalization" = "#e74c3c", 
                                     "After Normalization" = "#2ecc71")) +
        scale_y_log10() +
        scale_x_log10() +
        facet_wrap(~ Stage, ncol = 2) +
        theme_bw(base_size = 12) +
        labs(
          title = "Variance Stability - Mean-Variance Plot",
          subtitle = "Stable variance across expression levels indicates successful normalization",
          x = "Mean Expression (log10)",
          y = "Variance (log10)",
          color = "Stage"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray50"),
          legend.position = "none",
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_line(color = "gray95")
        )
      norm_plots$mean_variance <- p
      p
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
    })
  })
  
  # Download handlers for each normalization plot (PNG/PDF)
  norm_save <- function(p, file, device = "png") {
    if (is.null(p)) return()
    if (device == "png") ggplot2::ggsave(file, plot = p, width = 9, height = 5, dpi = IMAGE_DPI, units = "in", bg = "white", device = "png")
    else if (device %in% c("jpeg", "jpg")) ggplot2::ggsave(file, plot = p, width = 9, height = 5, dpi = IMAGE_DPI, units = "in", bg = "white", device = "jpeg")
    else ggplot2::ggsave(file, plot = p, width = 9, height = 5, device = "pdf", bg = "white")
  }
  output$dl_norm_plot_png <- downloadHandler(filename = function() "Normalization_Distribution.png", content = function(file) { req(norm_plots$plot); norm_save(norm_plots$plot, file, "png") })
  output$dl_norm_plot_jpg <- downloadHandler(filename = function() "Normalization_Distribution.jpg", content = function(file) { req(norm_plots$plot); norm_save(norm_plots$plot, file, "jpeg") })
  output$dl_norm_plot_pdf <- downloadHandler(filename = function() "Normalization_Distribution.pdf", content = function(file) { req(norm_plots$plot); norm_save(norm_plots$plot, file, "pdf") })
  output$dl_norm_density_png <- downloadHandler(filename = function() "Normalization_Density.png", content = function(file) { req(norm_plots$density); norm_save(norm_plots$density, file, "png") })
  output$dl_norm_density_jpg <- downloadHandler(filename = function() "Normalization_Density.jpg", content = function(file) { req(norm_plots$density); norm_save(norm_plots$density, file, "jpeg") })
  output$dl_norm_density_pdf <- downloadHandler(filename = function() "Normalization_Density.pdf", content = function(file) { req(norm_plots$density); norm_save(norm_plots$density, file, "pdf") })
  output$dl_norm_qq_png <- downloadHandler(filename = function() "Normalization_QQ.png", content = function(file) { req(norm_plots$qq); norm_save(norm_plots$qq, file, "png") })
  output$dl_norm_qq_jpg <- downloadHandler(filename = function() "Normalization_QQ.jpg", content = function(file) { req(norm_plots$qq); norm_save(norm_plots$qq, file, "jpeg") })
  output$dl_norm_qq_pdf <- downloadHandler(filename = function() "Normalization_QQ.pdf", content = function(file) { req(norm_plots$qq); norm_save(norm_plots$qq, file, "pdf") })
  output$dl_norm_median_range_png <- downloadHandler(filename = function() "Normalization_Median_Range.png", content = function(file) { req(norm_plots$median_range); norm_save(norm_plots$median_range, file, "png") })
  output$dl_norm_median_range_jpg <- downloadHandler(filename = function() "Normalization_Median_Range.jpg", content = function(file) { req(norm_plots$median_range); norm_save(norm_plots$median_range, file, "jpeg") })
  output$dl_norm_median_range_pdf <- downloadHandler(filename = function() "Normalization_Median_Range.pdf", content = function(file) { req(norm_plots$median_range); norm_save(norm_plots$median_range, file, "pdf") })
  output$dl_norm_dist_overlap_png <- downloadHandler(filename = function() "Normalization_Distribution_Overlap.png", content = function(file) { req(norm_plots$distribution_overlap); norm_save(norm_plots$distribution_overlap, file, "png") })
  output$dl_norm_dist_overlap_jpg <- downloadHandler(filename = function() "Normalization_Distribution_Overlap.jpg", content = function(file) { req(norm_plots$distribution_overlap); norm_save(norm_plots$distribution_overlap, file, "jpeg") })
  output$dl_norm_dist_overlap_pdf <- downloadHandler(filename = function() "Normalization_Distribution_Overlap.pdf", content = function(file) { req(norm_plots$distribution_overlap); norm_save(norm_plots$distribution_overlap, file, "pdf") })
  output$dl_norm_ma_png <- downloadHandler(filename = function() "Normalization_MA_Plot.png", content = function(file) { req(norm_plots$ma_plot); norm_save(norm_plots$ma_plot, file, "png") })
  output$dl_norm_ma_jpg <- downloadHandler(filename = function() "Normalization_MA_Plot.jpg", content = function(file) { req(norm_plots$ma_plot); norm_save(norm_plots$ma_plot, file, "jpeg") })
  output$dl_norm_ma_pdf <- downloadHandler(filename = function() "Normalization_MA_Plot.pdf", content = function(file) { req(norm_plots$ma_plot); norm_save(norm_plots$ma_plot, file, "pdf") })
  output$dl_norm_mv_png <- downloadHandler(filename = function() "Normalization_Mean_Variance.png", content = function(file) { req(norm_plots$mean_variance); norm_save(norm_plots$mean_variance, file, "png") })
  output$dl_norm_mv_jpg <- downloadHandler(filename = function() "Normalization_Mean_Variance.jpg", content = function(file) { req(norm_plots$mean_variance); norm_save(norm_plots$mean_variance, file, "jpeg") })
  output$dl_norm_mv_pdf <- downloadHandler(filename = function() "Normalization_Mean_Variance.pdf", content = function(file) { req(norm_plots$mean_variance); norm_save(norm_plots$mean_variance, file, "pdf") })
  output$dl_norm_corr_before_png <- downloadHandler(filename = function() "Normalization_Corr_Before.png", content = function(file) { req(norm_plots$corr_before); norm_save(norm_plots$corr_before, file, "png") })
  output$dl_norm_corr_before_jpg <- downloadHandler(filename = function() "Normalization_Corr_Before.jpg", content = function(file) { req(norm_plots$corr_before); norm_save(norm_plots$corr_before, file, "jpeg") })
  output$dl_norm_corr_before_pdf <- downloadHandler(filename = function() "Normalization_Corr_Before.pdf", content = function(file) { req(norm_plots$corr_before); norm_save(norm_plots$corr_before, file, "pdf") })
  output$dl_norm_corr_after_png <- downloadHandler(filename = function() "Normalization_Corr_After.png", content = function(file) { req(norm_plots$corr_after); norm_save(norm_plots$corr_after, file, "png") })
  output$dl_norm_corr_after_jpg <- downloadHandler(filename = function() "Normalization_Corr_After.jpg", content = function(file) { req(norm_plots$corr_after); norm_save(norm_plots$corr_after, file, "jpeg") })
  output$dl_norm_corr_after_pdf <- downloadHandler(filename = function() "Normalization_Corr_After.pdf", content = function(file) { req(norm_plots$corr_after); norm_save(norm_plots$corr_after, file, "pdf") })

  output$download_normalization_summary_csv <- downloadHandler(
    filename = function() "Normalization_Summary_Table.csv",
    content = function(file) {
      req(rv$normalization_summary_table)
      write.csv(rv$normalization_summary_table, file, row.names = FALSE)
      write.csv(rv$normalization_summary_table, file.path(CSV_EXPORT_DIR(), "Normalization_Summary_Table.csv"), row.names = FALSE)
    }
  )

  output$normalize_mixed_scale_ui <- renderUI({
    if (isTRUE(rv$merge_after_de) || identical(input$analysis_type, "parallel")) return(NULL)
    if (!isTRUE(rv$normalization_complete)) return(NULL)
    mixed <- FALSE
    if (!is.null(rv$unified_metadata)) {
      mixed <- gexpipe_has_mixed_platforms(rv$unified_metadata)
    } else {
      n_micro <- length(if (is.null(rv$micro_expr_list)) list() else rv$micro_expr_list)
      n_rna <- length(if (is.null(rv$rna_counts_list)) list() else rv$rna_counts_list)
      mixed <- n_micro > 0L && n_rna > 0L
    }
    if (!mixed) return(NULL)
    gq_on <- !is.null(input$apply_global_quantile) && isTRUE(input$apply_global_quantile)
    if (gq_on) return(NULL)

    column(
      12,
      box(
        title = tags$span(icon("dna"), " Mixed platform - different scales are expected"),
        width = 12,
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,
        tags$div(
          class = "alert alert-info",
          style = "margin: 0; font-size: 13px; line-height: 1.65;",
          tags$p(
            style = "margin: 0 0 10px 0;",
            icon("info-circle"),
            tags$strong(" You correctly left global quantile OFF."),
            " Microarray and RNA-seq are normalized correctly ",
            tags$strong("within each study"),
            " (TMM/log-CPM vs quantile log-intensity). They ",
            tags$strong("should not"),
            " share the same median when global quantile is off - that offset is ",
            tags$em("not"),
            " failed normalization."
          ),
          tags$p(
            style = "margin: 0 0 10px 0; padding: 8px; background: #fff3cd; border-radius: 4px; font-size: 12px;",
            icon("exclamation-triangle"),
            tags$strong(" Why global quantile looks \"better\": "),
            "it forces every sample to the same distribution, so plots look perfect. For mixed platforms that can ",
            tags$strong("hide technology bias"),
            " and distort RNA-seq counts. Use batch correction (Step 5) instead of global quantile for cross-platform alignment."
          ),
          tags$p(tags$strong("What to do before Step 5 (batch correction):"), style = "margin: 0 0 6px 0;"),
          tags$ol(
            style = "margin: 0 0 10px 0; padding-left: 22px;",
            tags$li(tags$strong("Step 4:"), " Assign Normal / Disease groups."),
            tags$li(tags$strong("Step 5:"), " Use ", tags$strong("limma removeBatchEffect"), " (not ComBat alone). Check Platform PCA - platforms should intermingle after correction."),
            tags$li(tags$strong("Step 6:"), " Use ", tags$strong("limma"), " DE (auto-selected for merged). Platform is included in the model when estimable.")
          ),
          tags$p(
            style = "margin: 0; font-size: 12px; color: #555;",
            icon("lightbulb"),
            " Within each dataset, samples should already align (density facets per GSE). Between-platform alignment is handled in ",
            tags$strong("batch correction"),
            ", not by global quantile here."
          )
        )
      )
    )
  })

  output$normalize_process_summary_ui <- renderUI({
    if (is.null(rv$normalization_summary_table) || nrow(rv$normalization_summary_table) == 0) {
      return(tags$p(style = "color: #6c757d; margin: 0;", icon("info-circle"), " Run normalization to see process summary."))
    }
    sm <- rv$last_norm_platform_summary
    if (isTRUE(rv$last_keep_platforms_separate) && !is.null(sm)) {
      return(tags$div(
        style = "font-size: 14px; line-height: 1.6; color: #333;",
        tags$p(tags$strong("Step 2 complete - Parallel (two methods).")),
        tags$p(
          tags$strong("Microarray: "),
          format(sm$micro_samples, big.mark = ","), " samples, ",
          format(sm$micro_genes, big.mark = ","), " genes. Method: ", sm$micro_method, "."
        ),
        tags$p(
          tags$strong("RNA-seq: "),
          format(sm$rna_samples, big.mark = ","), " samples, ",
          format(sm$rna_genes, big.mark = ","), " genes. Method: ", sm$rna_method,
          " (plots / QC). Raw counts are kept for DESeq2 / edgeR / voom."
        ),
        tags$p(
          "The ", format(sm$display_samples, big.mark = ","),
          " sample count is both platforms listed together. They did not share one normalize method."
        )
      ))
    }
    st <- rv$normalization_summary_table
    n_genes <- if ("Final_Common_Genes" %in% names(st)) max(st$Final_Common_Genes, na.rm = TRUE) else NA
    tags$div(
      style = "font-size: 14px; line-height: 1.6; color: #333;",
      tags$p(tags$strong("Step 2 complete."), " Normalization applied per dataset; common genes are shown in Step 3 (QC). See table and log below."),
      tags$p("Final common genes: ", format(n_genes, big.mark = ","), "."))
  })

  # Render summary table
  output$normalization_summary_table <- renderTable({
    req(rv$normalization_summary_table)
    
    # Format numbers with commas
    summary_table <- rv$normalization_summary_table
    summary_table$Initial_Genes <- format(summary_table$Initial_Genes, big.mark = ",")
    summary_table$After_Filtering <- format(summary_table$After_Filtering, big.mark = ",")
    summary_table$Final_Common_Genes <- format(summary_table$Final_Common_Genes, big.mark = ",")
    
    summary_table
  }, striped = TRUE, bordered = TRUE, hover = TRUE, 
     spacing = "m", width = "100%", align = "l")
  
  output$next_to_groups_btn <- renderUI({
    req(rv$normalization_complete)
    actionButton("go_to_groups", "Next: QC & Visualization", 
                 icon = icon("arrow-right"), class = "btn-success btn-lg")
  })
  
}


