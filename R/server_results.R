# ==============================================================================
# SERVER_RESULTS.R - Step 6: Differential Gene Expression Analysis Module
# ==============================================================================

server_results <- function(input, output, session, rv) {

  .record_de_transparency <- function(meta_used, method, formula_desc, filter_note,
                                      total_meta = NULL) {
    if (is.null(total_meta)) total_meta <- rv$unified_metadata
    rv$de_design_formula <- formula_desc
    rv$de_gene_filter_note <- filter_note
    rv$de_sample_info <- .gexpipe_call(
      "gexpipe_de_sample_info",
      meta_used, total_meta = total_meta, method = method
    )
  }

  .mixed_platforms <- function() {
    isTRUE(.gexpipe_call("gexpipe_has_mixed_platforms", rv$unified_metadata))
  }

  .write_parallel_de_logs <- function() {
    if (!isTRUE(rv$merge_after_de) && !identical(isolate(input$analysis_type), "parallel")) {
      return()
    }
    rna_n <- if (is.null(rv$de_results_rna)) 0L else nrow(rv$de_results_rna)
    rna_sig <- if (is.null(rv$sig_genes_rna)) 0L else nrow(rv$sig_genes_rna)
    micro_n <- if (is.null(rv$de_results_micro)) 0L else nrow(rv$de_results_micro)
    micro_sig <- if (is.null(rv$sig_genes_micro)) 0L else nrow(rv$sig_genes_micro)
    rna_lab <- switch(
      if (is.null(rv$de_method)) "limma" else rv$de_method,
      deseq2 = "DESeq2",
      edger = "edgeR",
      limma_voom = "limma-voom",
      "limma"
    )
    rv$de_log_micro <- gexpipe_format_separate_run_log(
      1L, "MICROARRAY",
      paste0(
        "Method: limma (array matrix only)\n",
        "Thresholds: |log2FC| >= ", format(if (is.null(rv$de_logfc_micro)) 0.5 else rv$de_logfc_micro, digits = 3),
        ", adj.P <= ", format(if (is.null(rv$de_padj_micro)) 0.05 else rv$de_padj_micro, digits = 3), "\n",
        "Genes tested: ", format(micro_n, big.mark = ","), "\n",
        "Significant DEGs: ", format(micro_sig, big.mark = ","), "\n",
        "Merged with RNA-seq: no\n",
        "\nOK Microarray DE complete.\n"
      )
    )
    rv$de_log_rna <- gexpipe_format_separate_run_log(
      2L, "RNA-SEQ",
      paste0(
        "Method: ", rna_lab, " (RNA-seq only)\n",
        "Thresholds: |log2FC| >= ", format(if (is.null(rv$de_logfc_rna)) 0.5 else rv$de_logfc_rna, digits = 3),
        ", adj.P <= ", format(if (is.null(rv$de_padj_rna)) 0.05 else rv$de_padj_rna, digits = 3), "\n",
        "Genes tested: ", format(rna_n, big.mark = ","), "\n",
        "Significant DEGs: ", format(rna_sig, big.mark = ","), "\n",
        "Merged with microarray: no\n",
        "\nOK RNA-seq DE complete.\n"
      )
    )
  }

  output$de_log_micro <- renderText({
    if (!is.null(rv$de_log_micro) && nzchar(rv$de_log_micro)) rv$de_log_micro
    else "Run DE to see the microarray run log."
  })
  output$de_log_rna <- renderText({
    if (!is.null(rv$de_log_rna) && nzchar(rv$de_log_rna)) rv$de_log_rna
    else "Run DE to see the RNA-seq run log."
  })

  .pre_batch_expr <- function() {
    if (isTRUE(rv$merge_after_de) && !is.null(rv$batch_corrected)) {
      return(rv$batch_corrected)
    }
    expr <- rv$combined_expr_before_global_norm
    if (is.null(expr)) expr <- rv$combined_expr
    expr
  }

  .platform_expr <- function(platform) {
    if (identical(platform, "Microarray") && !is.null(rv$batch_corrected_micro)) {
      return(rv$batch_corrected_micro)
    }
    if (identical(platform, "RNAseq") && !is.null(rv$batch_corrected_rna)) {
      return(rv$batch_corrected_rna)
    }
    if (identical(platform, "Microarray") && !is.null(rv$expr_micro)) {
      return(rv$expr_micro)
    }
    if (identical(platform, "RNAseq") && !is.null(rv$expr_rna)) {
      return(rv$expr_rna)
    }
    .pre_batch_expr()
  }

  .de_num <- function(x, default) {
    v <- suppressWarnings(as.numeric(x)[[1]])
    if (length(v) != 1L || is.na(v) || !is.finite(v)) default else v
  }

  .de_cutoffs <- function(platform = NULL) {
    parallel <- isTRUE(rv$merge_after_de) || identical(input$analysis_type, "parallel")
    if (isTRUE(parallel) && !is.null(platform) && grepl("micro", platform, ignore.case = TRUE)) {
      list(
        logfc = .de_num(input$logfc_cutoff_micro, 0.5),
        padj = .de_num(input$padj_cutoff_micro, 0.05),
        top = as.integer(.de_num(input$top_genes_micro, 50))
      )
    } else if (isTRUE(parallel) && !is.null(platform)) {
      list(
        logfc = .de_num(input$logfc_cutoff_rna, 0.5),
        padj = .de_num(input$padj_cutoff_rna, 0.05),
        top = as.integer(.de_num(input$top_genes_rna, 50))
      )
    } else {
      list(
        logfc = .de_num(input$logfc_cutoff, 0.5),
        padj = .de_num(input$padj_cutoff, 0.05),
        top = as.integer(.de_num(input$top_genes, 50))
      )
    }
  }

  .run_platform_limma <- function(platform, ref_lab, alt_lab) {
    expr <- .platform_expr(platform)
    meta <- rv$unified_metadata
    if (is.null(expr) || is.null(meta)) {
      stop("Normalized expression and metadata are required for separate-platform DE.")
    }
    ids <- .gexpipe_call("gexpipe_platform_sample_ids", meta, platform)
    ids <- intersect(ids, colnames(expr))
    ids <- intersect(ids, rownames(meta))
    if (length(ids) < 4L) {
      stop(platform, " DE needs at least 4 samples after group assignment (found ", length(ids), ").")
    }
    cuts <- .de_cutoffs(platform)
    .gexpipe_call(
      "gexpipe_run_limma_on_subset",
      expr[, ids, drop = FALSE],
      meta[ids, , drop = FALSE],
      logfc_cutoff = cuts$logfc,
      padj_cutoff = cuts$padj,
      ref_lab = ref_lab,
      alt_lab = alt_lab
    )
  }

  # ---------- METHOD BANNER (shows active DE method on Step 6) ----------
  output$results_process_summary_ui <- renderUI({
    if (isTRUE(rv$merge_after_de) || identical(input$analysis_type, "parallel")) {
      if (is.null(rv$sig_genes_rna) && is.null(rv$sig_genes_micro)) {
        return(tags$p(style = "color: #6c757d; margin: 0;", icon("info-circle"), " Run DE analysis to see process summary."))
      }
      n_r <- if (is.null(rv$sig_genes_rna)) 0L else nrow(rv$sig_genes_rna)
      n_m <- if (is.null(rv$sig_genes_micro)) 0L else nrow(rv$sig_genes_micro)
      rna_lab <- .gexpipe_de_method_label(if (is.null(rv$de_method)) "deseq2" else rv$de_method)
      return(tags$div(
        style = "font-size: 14px; line-height: 1.6; color: #333;",
        tags$p(tags$strong("Step 6 complete - two separate DEs."), " Consensus is Step 7."),
        tags$p("RNA-seq (", rna_lab, "): ", format(n_r, big.mark = ","), " DEGs",
               if (!is.null(rv$de_logfc_rna)) paste0(" (|log2FC| >= ", rv$de_logfc_rna, ", adj.P <= ", rv$de_padj_rna, ")") else "",
               "."),
        tags$p("Microarray (limma): ", format(n_m, big.mark = ","), " DEGs",
               if (!is.null(rv$de_logfc_micro)) paste0(" (|log2FC| >= ", rv$de_logfc_micro, ", adj.P <= ", rv$de_padj_micro, ")") else "",
               ".")
      ))
    }
    if (is.null(rv$sig_genes) || nrow(rv$sig_genes) == 0) {
      return(tags$p(style = "color: #6c757d; margin: 0;", icon("info-circle"), " Run DE analysis to see process summary."))
    }
    n_sig <- nrow(rv$sig_genes)
    n_up <- if (!is.null(rv$de_results)) sum(rv$de_results$Significance == "Up-regulated", na.rm = TRUE) else NA
    n_down <- if (!is.null(rv$de_results)) sum(rv$de_results$Significance == "Down-regulated", na.rm = TRUE) else NA
    tags$div(
      style = "font-size: 14px; line-height: 1.6; color: #333;",
      tags$p(tags$strong("Step 6 complete."), " Significant DEGs: ", format(n_sig, big.mark = ","), "."),
      if (!is.na(n_up))     tags$p("Up-regulated: ", n_up, "; Down-regulated: ", n_down, ". Volcano plot and heatmap above.") else NULL)
  })

  output$de_platform_view_ui <- renderUI({
    NULL
  })

  output$de_parallel_guide_ui <- renderUI({
    de_rna <- if (!is.null(input$de_method_rna) && nzchar(input$de_method_rna)) {
      input$de_method_rna
    } else {
      "deseq2"
    }
    defs <- gexpipe_parallel_de_defaults(de_rna)
    rna_lab <- .gexpipe_de_method_label(defs$rna)
    mode <- if (is.null(input$de_mode_parallel)) "auto" else input$de_mode_parallel
    if (identical(mode, "manual")) {
      tags$div(
        class = "alert alert-warning",
        style = "margin: 8px 0 0 0; font-size: 13px; line-height: 1.55;",
        tags$strong("Manual - pick the RNA-seq engine. Microarray stays limma."),
        tags$ul(
          style = "margin: 6px 0 0 0; padding-left: 18px;",
          tags$li(tags$strong("DESeq2 / edgeR / voom:"), " raw RNA-seq counts only; Dataset is a covariate when that side has 2+ GSEs. Not applied to microarray."),
          tags$li(tags$strong("limma on TMM:"), " use when RNA DE should stay on the log-CPM matrix."),
          tags$li(tags$strong("Microarray:"), " always limma on the array matrix.")
        )
      )
    } else {
      count_note <- if (.gexpipe_is_count_de(defs$rna)) {
        " on raw counts (batch covariate if 2+ RNA GSEs)"
      } else {
        " on the TMM / log-CPM matrix"
      }
      tags$div(
        class = "alert alert-info",
        style = "margin: 8px 0 0 0; font-size: 13px; line-height: 1.55;",
        icon("magic"),
        tags$strong(" Auto (recommended). "),
        "One Run DE starts both platforms. No mixed matrix.",
        tags$ul(
          style = "margin: 6px 0 0 0; padding-left: 18px;",
          tags$li(tags$strong("Microarray: "), "limma."),
          tags$li(tags$strong("RNA-seq: "), rna_lab, count_note, ".")
        )
      )
    }
  })

  output$de_method_banner <- renderUI({
    method <- rv$de_method
    if (isTRUE(rv$merge_after_de) || identical(input$analysis_type, "parallel")) {
      rna_lab <- switch(
        method,
        deseq2 = "DESeq2",
        edger = "edgeR",
        limma_voom = "limma-voom",
        "limma"
      )
      return(tags$div(
        class = "alert alert-success",
        style = "margin: 0 15px 10px 15px; padding: 12px 18px; border-left: 5px solid #27ae60;",
        icon("object-ungroup"),
        tags$strong(" Parallel DE: two methods."),
        " Microarray = ", tags$strong("limma"),
        " on the array matrix. RNA-seq = ", tags$strong(rna_lab),
        " on RNA-seq only. Matrices are not mixed. Auto uses Step 1; Manual is on this step."
      ))
    }
    if (is.null(method) || method == "limma") {
      tags$div(
        class = "alert alert-info",
        style = "margin: 0 15px 10px 15px; padding: 12px 18px; border-left: 5px solid #3498db;",
        icon("flask"),
        tags$strong(" Active DE method: limma"),
        " - Empirical Bayes moderated t-statistics on batch-corrected, normalized expression.",
        tags$small(" (Change in Step 1 before running DE)", style = "color: #6c757d;")
      )
    } else if (method == "limma_voom") {
      tags$div(
        class = "alert alert-info",
        style = "margin: 0 15px 10px 15px; padding: 12px 18px; border-left: 5px solid #8e44ad;",
        icon("flask"),
        tags$strong(" Active DE method: limma-voom"),
        " - voom transforms RNA-seq counts to logCPM with precision weights, then uses limma's empirical Bayes linear models (supports batch covariates).",
        tags$small(" (Change in Step 1 before running DE)", style = "color: #6c757d;")
      )
    } else if (method == "deseq2") {
      tags$div(
        class = "alert alert-success",
        style = "margin: 0 15px 10px 15px; padding: 12px 18px; border-left: 5px solid #27ae60;",
        icon("dna"),
        tags$strong(" Active DE method: DESeq2"),
        " - Negative binomial GLM on raw counts with batch as covariate. DESeq2's internal normalization (median-of-ratios) is used.",
        tags$small(" (Change in Step 1 before running DE)", style = "color: #6c757d;")
      )
    } else {
      tags$div(
        class = "alert alert-warning",
        style = "margin: 0 15px 10px 15px; padding: 12px 18px; border-left: 5px solid #f39c12;",
        icon("chart-bar"),
        tags$strong(" Active DE method: edgeR"),
        " - Quasi-likelihood F-test on raw counts with TMM normalization and batch as covariate.",
        tags$small(" (Change in Step 1 before running DE)", style = "color: #6c757d;")
      )
    }
  })
  
  observeEvent(input$de_view_platform, {
    if (is.null(input$de_view_platform)) return()
    if (identical(input$de_view_platform, "micro") && !is.null(rv$de_results_micro)) {
      rv$de_results <- rv$de_results_micro
      if (!isTRUE(rv$consensus_complete)) rv$sig_genes <- rv$sig_genes_micro
    } else if (!is.null(rv$de_results_rna)) {
      rv$de_results <- rv$de_results_rna
      if (!isTRUE(rv$consensus_complete)) rv$sig_genes <- rv$sig_genes_rna
    }
  }, ignoreInit = TRUE)

  # ---------- RUN DE ANALYSIS ----------
  .run_de_analysis <- function() {
    parallel_de <- isTRUE(rv$merge_after_de) || identical(input$analysis_type, "parallel")
    if (!isTRUE(rv$batch_complete)) {
      if (isTRUE(parallel_de)) {
        if (is.null(rv$batch_corrected_rna) && !is.null(rv$expr_rna)) {
          rv$batch_corrected_rna <- rv$expr_rna
        }
        if (is.null(rv$batch_corrected_micro) && !is.null(rv$expr_micro)) {
          rv$batch_corrected_micro <- rv$expr_micro
        }
        if (is.null(rv$batch_corrected) && !is.null(rv$combined_expr)) {
          rv$batch_corrected <- rv$combined_expr
        }
        if (is.null(rv$batch_corrected_rna) && is.null(rv$batch_corrected_micro) &&
            is.null(rv$batch_corrected) && is.null(rv$raw_counts_for_deseq2)) {
          showNotification(
            tags$div(
              icon("exclamation-triangle"),
              tags$strong(" Need normalized data for DE."),
              " Finish Step 2 (Normalize) and Step 4 (Groups), then click Run DE."
            ),
            type = "error",
            duration = 8
          )
          return()
        }
        rv$batch_complete <- TRUE
        showNotification(
          "Step 5 was not applied. DE will use each platform's normalized matrix (RNA-seq count DE still uses raw counts).",
          type = "warning",
          duration = 7
        )
      } else {
        showNotification(
          tags$div(icon("exclamation-triangle"), tags$strong(" Step 5 required:"),
                   " Complete batch correction (Step 5) before running DE analysis."),
          type = "error", duration = 6)
        return()
      }
    }

    # DE requires both groups; otherwise there is no contrast (e.g. same sample source = one condition only).
    # Use the current (possibly renamed) reference/comparison labels, not the
    # literal "Normal"/"Disease" strings - group names may have been
    # customized in Step 4.
    precheck_ref_lab <- if (!is.null(rv$condition_ref_label) && nzchar(rv$condition_ref_label)) {
      rv$condition_ref_label
    } else {
      "Normal"
    }
    precheck_alt_lab <- if (!is.null(rv$condition_alt_label) && nzchar(rv$condition_alt_label)) {
      rv$condition_alt_label
    } else {
      "Disease"
    }
    cond_counts <- table(rv$unified_metadata$Condition)
    n_normal <- if (precheck_ref_lab %in% names(cond_counts)) cond_counts[[precheck_ref_lab]] else 0L
    n_disease <- if (precheck_alt_lab %in% names(cond_counts)) cond_counts[[precheck_alt_lab]] else 0L
    if (n_normal == 0L || n_disease == 0L) {
      showNotification(
        tags$div(
          icon("exclamation-triangle"), tags$strong(paste0(" Need both ", precheck_ref_lab, " and ", precheck_alt_lab, " samples for DE.")),
          tags$br(),
          "You have ", n_normal, " ", precheck_ref_lab, " and ", n_disease, " ", precheck_alt_lab, ". Differential expression compares these two groups.",
          " If you entered GSEs from the same sample source (e.g. same study or only one condition), add a dataset that contains the other group, or in Step 3 assign some samples to ", precheck_ref_lab, " and some to ", precheck_alt_lab, "."
        ),
        type = "error", duration = 12)
      rv$de_running <- FALSE
      return()
    }

    # parallel_de already set above
    if (isTRUE(parallel_de)) {
      de_rna_in <- if (!is.null(input$de_method_rna) && nzchar(input$de_method_rna)) {
        input$de_method_rna
      } else {
        "deseq2"
      }
      defs <- gexpipe_parallel_de_defaults(de_rna_in)
      de_mode_p <- if (is.null(input$de_mode_parallel) || !nzchar(input$de_mode_parallel)) {
        "auto"
      } else {
        input$de_mode_parallel
      }
      method <- if (identical(de_mode_p, "manual") && !is.null(input$de_method_rna_step6)) {
        input$de_method_rna_step6
      } else {
        defs$rna
      }
      rv$de_method <- method
      rv$de_method_micro <- "limma"
    } else {
      method <- rv$de_method
      if (is.null(method)) method <- "limma"
    }
    if (.mixed_platforms() && !isTRUE(parallel_de) &&
        method %in% c("deseq2", "edger", "limma_voom")) {
      method <- "limma"
      showNotification(
        "Merged (Both) uses limma on the combined matrix (original workflow). For DESeq2/edgeR/voom plus microarray limma, use Parallel DE, then merge.",
        type = "warning",
        duration = 8
      )
    }
    ref_lab <- precheck_ref_lab
    alt_lab <- precheck_alt_lab
    de_contrast_label <- paste0(alt_lab, " vs ", ref_lab)
    
    rv$de_start <- Sys.time()
    rv$de_running <- TRUE
    
    tryCatch({
      # ------------------------------------------------------------------
      # Helper: attempt to rebuild raw counts on-the-fly from rna_counts_list
      # when the user switches DE method after normalisation (counts were not
      # cached during that particular normalization run).
      # ------------------------------------------------------------------
      .try_rebuild_raw_counts <- function() {
        # rv$raw_counts_for_deseq2 is never cleared when the user changes
        # which GSEs are loaded (e.g. an earlier RNA-seq attempt, or a GSE
        # that was later moved from the RNA-seq box to the Microarray box).
        # A stale matrix from a completely different sample set would
        # otherwise look "available" here despite matching nothing in the
        # current run, silently corrupting the design matrix downstream.
        # Require real overlap with the samples actually in play now.
        current_ids <- if (!is.null(rv$unified_metadata)) rownames(rv$unified_metadata) else character(0)
        if (!is.null(rv$raw_counts_for_deseq2)) {
          overlap <- length(intersect(colnames(rv$raw_counts_for_deseq2), current_ids))
          if (overlap >= 3L) return(TRUE)
          rv$raw_counts_for_deseq2 <- NULL # stale - stop treating it as available
        }
        if (length(rv$rna_counts_list) == 0)   return(FALSE)   # pure microarray - can't rebuild
        rna_counts_overlap <- sum(vapply(rv$rna_counts_list, function(m) {
          length(intersect(colnames(m), current_ids)) > 0L
        }, logical(1)))
        if (rna_counts_overlap == 0L) return(FALSE) # rna_counts_list is stale too - none of it matches this run

        if (isTRUE(parallel_de)) {
          built <- gexpipe_bind_rna_counts(rv$rna_counts_list)
          if (is.null(built)) return(FALSE)
          rv$raw_counts_for_deseq2 <- built
          return(TRUE)
        }

        common_g <- rv$common_genes
        if (is.null(common_g) || length(common_g) == 0) {
          # Derive common genes from combined_expr as fallback
          common_g <- rownames(if (!is.null(rv$batch_corrected)) rv$batch_corrected else rv$combined_expr)
        }
        raw_list <- list()
        for (gse in names(rv$rna_counts_list)) {
          raw_mat <- as.matrix(rv$rna_counts_list[[gse]])
          keep    <- intersect(common_g, rownames(raw_mat))
          if (length(keep) > 0) raw_list[[gse]] <- raw_mat[keep, , drop = FALSE]
        }
        if (length(raw_list) == 0) return(FALSE)

        built <- round(do.call(cbind, raw_list))
        storage.mode(built) <- "integer"
        rv$raw_counts_for_deseq2 <- built
        TRUE
      }

      # ------------------------------------------------------------------
      # Helper: reject non-count data before it reaches the count engines.
      # A series that only published normalized / log-scale values otherwise
      # fails inside DESeq2 with "some values in assay are negative".
      # ------------------------------------------------------------------
      .counts_usable_for <- function(engine) {
        bad_neg <- .gexpipe_call(
          ".gexpipe_negative_count_datasets",
          rv$rna_counts_list,
          combined = rv$raw_counts_for_deseq2
        )
        if (length(bad_neg) > 0L) {
          showNotification(
            tags$div(
              icon("exclamation-triangle"),
              tags$strong(paste0(" ", engine, " needs raw counts.")),
              tags$p(
                paste0("Negative values were found in: ", paste(bad_neg, collapse = ", "),
                       ". GEO published normalized (log-scale) values for these series, ",
                       "not raw integer counts."),
                style = "margin-top: 8px; font-size: 12px;"
              ),
              tags$p(
                "Falling back to limma, which is the correct model for continuous log-scale data.",
                style = "margin-top: 4px; font-size: 12px;"
              )
            ),
            type = "warning", duration = 12
          )
          return(FALSE)
        }
        bad_noncount <- .gexpipe_call(
          ".gexpipe_noncount_datasets",
          rv$rna_counts_list,
          combined = rv$raw_counts_for_deseq2
        )
        if (length(bad_noncount) > 0L) {
          showNotification(
            tags$div(
              icon("exclamation-triangle"),
              tags$strong(paste0(" ", engine, " needs raw integer counts.")),
              tags$p(
                paste0("Non-integer values were found in: ", paste(bad_noncount, collapse = ", "),
                       ". GEO likely published normalized (FPKM/TPM/CPM) values for these series ",
                       "under a counts-like filename, not raw integer counts. Using these with ",
                       engine, " would silently filter out every gene instead of failing cleanly."),
                style = "margin-top: 8px; font-size: 12px;"
              ),
              tags$p(
                "Falling back to limma, which is the correct model for continuous normalized data.",
                style = "margin-top: 4px; font-size: 12px;"
              )
            ),
            type = "warning", duration = 12
          )
          return(FALSE)
        }
        TRUE
      }

      # Pre-checks for count-based methods (DESeq2 / edgeR / limma-voom).
      # For each method, if raw counts are missing we first try to rebuild
      # them from the stored rna_counts_list.  Only if that also fails (pure
      # microarray data) do we fall back to limma - we never hard-block.
      if (method == "deseq2") {
        if (!requireNamespace("DESeq2", quietly = TRUE)) {
          showNotification(
            tags$div(icon("exclamation-triangle"), tags$strong(" DESeq2 not installed."),
                     " Install with: BiocManager::install('DESeq2'). Falling back to limma."),
            type = "error", duration = 8)
          method <- "limma"
        } else if (!.try_rebuild_raw_counts()) {
          showNotification(
            tags$div(icon("info-circle"), tags$strong(" No RNA-seq count data available."),
                     " DESeq2 requires integer counts. Your data appears to be microarray-based.",
                     " Automatically falling back to limma for differential expression."),
            type = "warning", duration = 10)
          method <- "limma"
        } else if (!.counts_usable_for("DESeq2")) {
          method <- "limma"
        }
      }
      if (method == "edger") {
        if (!.try_rebuild_raw_counts()) {
          showNotification(
            tags$div(icon("info-circle"), tags$strong(" No RNA-seq count data available."),
                     " edgeR requires integer counts. Your data appears to be microarray-based.",
                     " Automatically falling back to limma for differential expression."),
            type = "warning", duration = 10)
          method <- "limma"
        } else if (!.counts_usable_for("edgeR")) {
          method <- "limma"
        }
      }
      if (method == "limma_voom") {
        if (!.try_rebuild_raw_counts()) {
          showNotification(
            tags$div(icon("info-circle"), tags$strong(" No RNA-seq count data available."),
                     " limma-voom requires integer counts. Your data appears to be microarray-based.",
                     " Automatically falling back to limma for differential expression."),
            type = "warning", duration = 10)
          method <- "limma"
        } else if (!.counts_usable_for("limma-voom")) {
          method <- "limma"
        }
      }
      
      if (isTRUE(parallel_de)) {
        rna_cuts <- .de_cutoffs("RNAseq")
        micro_cuts <- .de_cutoffs("Microarray")
        rv$de_logfc_rna <- rna_cuts$logfc
        rv$de_padj_rna <- rna_cuts$padj
        rv$de_top_rna <- rna_cuts$top
        rv$de_logfc_micro <- micro_cuts$logfc
        rv$de_padj_micro <- micro_cuts$padj
        rv$de_top_micro <- micro_cuts$top
        withProgress(message = "Parallel DE (two engines)...", value = 0, {
          incProgress(0.2, detail = "Microarray limma...")
          micro_out <- .run_platform_limma("Microarray", ref_lab, alt_lab)
          rv$de_results_micro <- micro_out$de_results
          rv$sig_genes_micro <- micro_out$sig_genes

          incProgress(0.15, detail = paste0("RNA-seq ", method, "..."))
          if (method %in% c("deseq2", "edger", "limma_voom")) {
            count_mat <- rv$raw_counts_for_deseq2
            meta <- rv$unified_metadata
            if (is.null(count_mat) || is.null(meta)) {
              stop("RNA-seq count DE needs raw counts and metadata.")
            }
            rna_ids <- intersect(
              .gexpipe_call("gexpipe_platform_sample_ids", meta, "RNAseq"),
              colnames(count_mat)
            )
            rna_ids <- intersect(rna_ids, rownames(meta))
            if (length(rna_ids) < 3L) {
              stop("RNA-seq count DE needs >= 3 samples matching counts and metadata.")
            }
            rna_out <- gexpipe_run_count_de(
              count_mat[, rna_ids, drop = FALSE],
              meta[rna_ids, , drop = FALSE],
              method = method,
              logfc_cutoff = rna_cuts$logfc,
              padj_cutoff = rna_cuts$padj,
              ref_lab = ref_lab,
              alt_lab = alt_lab
            )
          } else {
            rna_out <- .run_platform_limma("RNAseq", ref_lab, alt_lab)
          }
          rv$de_results_rna <- rna_out$de_results
          rv$sig_genes_rna <- rna_out$sig_genes
          rv$de_results <- rna_out$de_results
          rv$sig_genes <- rna_out$sig_genes
          rv$consensus_complete <- FALSE
          rv$consensus_result <- NULL
          rna_ids_meta <- .gexpipe_call("gexpipe_platform_sample_ids", rv$unified_metadata, "RNAseq")
          .record_de_transparency(
            rv$unified_metadata[intersect(rna_ids_meta, rownames(rv$unified_metadata)), , drop = FALSE],
            method,
            paste0(
              "Parallel DE. RNA-seq (", method, "): ", rna_out$formula_desc,
              " | Microarray (limma): ", micro_out$formula_desc
            ),
            paste(rna_out$filter_note, micro_out$filter_note, sep = " | "),
            rv$unified_metadata
          )
          incProgress(0.15, detail = "Done!")
        })
        .write_parallel_de_logs()
        showNotification(
          tags$div(
            icon("check-circle"),
            tags$strong(" Parallel DE complete - two separate engines."),
            paste0(
              " RNA-seq (", method, "): ", nrow(rv$sig_genes_rna),
              " DEGs. Microarray (limma): ", nrow(rv$sig_genes_micro),
              " DEGs. Apply RNA-seq \u2229 microarray in Step 7."
            )
          ),
          type = "message",
          duration = 8
        )
      } else if (method == "deseq2") {
        withProgress(message = 'DESeq2 analysis...', value = 0, {
          
          # Build sample metadata for DESeq2
          count_mat <- rv$raw_counts_for_deseq2
          
          # Use the metadata that has Condition labels applied
          meta <- rv$unified_metadata
          
          # Align samples between count matrix and metadata
          common_samples <- intersect(colnames(count_mat), rownames(meta))
          if (.mixed_platforms()) {
            common_samples <- intersect(
              common_samples,
              .gexpipe_call("gexpipe_platform_sample_ids", meta, "RNAseq")
            )
          }
          if (length(common_samples) < 3) {
            showNotification(
              tags$div(icon("exclamation-triangle"), tags$strong(" Too few samples."),
                       " DESeq2 needs >= 3 samples matching between raw counts and metadata."),
              type = "error", duration = 8)
            rv$de_running <- FALSE
            return()
          }
          count_mat <- count_mat[, common_samples, drop = FALSE]
          meta <- meta[common_samples, , drop = FALSE]
          total_meta <- rv$unified_metadata
          
          # Ensure Condition is a factor
          meta$Condition <- factor(meta$Condition, levels = c(ref_lab, alt_lab))
          
          ds_design <- gexpipe_deseq2_design(meta)
          design_mm <- stats::model.matrix(ds_design$formula, data = meta)
          filt <- .gexpipe_call("gexpipe_independent_filter", count_mat, design = design_mm)
          count_mat <- filt$expr
          .record_de_transparency(meta, "deseq2", ds_design$formula_desc, filt$note, total_meta)
          
          incProgress(0.2, detail = "Creating DESeqDataSet...")
          
          dds <- DESeq2::DESeqDataSetFromMatrix(
            countData = count_mat,
            colData = meta,
            design = ds_design$formula
          )
          
          incProgress(0.3, detail = "Running DESeq2...")
          
          # Run DESeq2
          dds <- DESeq2::DESeq(dds, quiet = TRUE)
          
          incProgress(0.3, detail = "Extracting results...")
          
          # Extract results (Disease vs Normal)
          res <- DESeq2::results(
            dds,
            contrast = gexp_condition_contrast(ref_lab, alt_lab),
            alpha = input$padj_cutoff
          )
          res_df <- as.data.frame(res)
          res_df$Gene <- rownames(res_df)
          
          # Rename columns to match limma format for downstream compatibility
          de_results <- data.frame(
            Gene = res_df$Gene,
            logFC = res_df$log2FoldChange,
            AveExpr = res_df$baseMean,
            P.Value = res_df$pvalue,
            adj.P.Val = res_df$padj,
            stringsAsFactors = FALSE
          )
          
          # Remove NA rows (genes with insufficient data)
          de_results <- de_results[!is.na(de_results$adj.P.Val), ]
          rownames(de_results) <- de_results$Gene
          
          # Classify significance
          de_results$Significance <- "Not Significant"
          de_results$Significance[de_results$adj.P.Val < input$padj_cutoff &
                                    de_results$logFC > input$logfc_cutoff] <- "Up-regulated"
          de_results$Significance[de_results$adj.P.Val < input$padj_cutoff &
                                    de_results$logFC < -input$logfc_cutoff] <- "Down-regulated"
          de_results$Significance <- as.character(de_results$Significance)
          
          rv$de_results <- de_results
          rv$sig_genes <- de_results[de_results$Significance != "Not Significant", ]
          
          incProgress(0.2, detail = "Done!")
        })
        
        showNotification(
          tags$div(
            icon("check-circle"),
            tags$strong(" DESeq2 analysis complete."),
            paste0(" Found ", nrow(rv$sig_genes), " significant DEGs.")
          ),
          type = "message", duration = 5)
        
      } else if (method == "edger") {
        # ==================================================================
        # edgeR PATHWAY (quasi-likelihood F-test on raw counts)
        # ==================================================================
        withProgress(message = 'edgeR analysis...', value = 0, {
          
          count_mat <- rv$raw_counts_for_deseq2  # shared raw counts matrix
          meta <- rv$unified_metadata
          
          # Align samples
          common_samples <- intersect(colnames(count_mat), rownames(meta))
          if (.mixed_platforms()) {
            common_samples <- intersect(
              common_samples,
              .gexpipe_call("gexpipe_platform_sample_ids", meta, "RNAseq")
            )
          }
          if (length(common_samples) < 3) {
            showNotification(
              tags$div(icon("exclamation-triangle"), tags$strong(" Too few samples."),
                       " edgeR needs >= 3 samples matching between raw counts and metadata."),
              type = "error", duration = 8)
            rv$de_running <- FALSE
            return()
          }
          count_mat <- count_mat[, common_samples, drop = FALSE]
          meta <- meta[common_samples, , drop = FALSE]
          total_meta <- rv$unified_metadata
          
          meta$Condition <- factor(meta$Condition, levels = c(ref_lab, alt_lab))
          
          de_design <- gexpipe_build_de_design(meta)
          design <- de_design$design
          filt <- .gexpipe_call("gexpipe_independent_filter", count_mat, design = design)
          count_mat <- filt$expr
          .record_de_transparency(meta, "edger", de_design$formula_desc, filt$note, total_meta)
          
          incProgress(0.2, detail = "Creating DGEList...")
          
          dge <- edgeR::DGEList(counts = count_mat, group = meta$Condition)
          dge <- edgeR::calcNormFactors(dge, method = "TMM")
          
          incProgress(0.2, detail = "Estimating dispersion...")
          
          # Estimate dispersion and fit GLM
          dge <- edgeR::estimateDisp(dge, design)
          fit <- edgeR::glmQLFit(dge, design)
          
          incProgress(0.3, detail = "Testing for DE...")
          
          # Test the Condition coefficient (last column)
          qlf <- edgeR::glmQLFTest(fit, coef = de_design$coef_condition)
          res <- edgeR::topTags(qlf, n = Inf, sort.by = "PValue")$table
          
          incProgress(0.2, detail = "Formatting results...")
          
          # Format to match limma output for downstream compatibility
          de_results <- data.frame(
            Gene = rownames(res),
            logFC = res$logFC,
            AveExpr = res$logCPM,
            P.Value = res$PValue,
            adj.P.Val = res$FDR,
            stringsAsFactors = FALSE
          )
          
          de_results <- de_results[!is.na(de_results$adj.P.Val), ]
          rownames(de_results) <- de_results$Gene
          
          de_results$Significance <- "Not Significant"
          de_results$Significance[de_results$adj.P.Val < input$padj_cutoff &
                                    de_results$logFC > input$logfc_cutoff] <- "Up-regulated"
          de_results$Significance[de_results$adj.P.Val < input$padj_cutoff &
                                    de_results$logFC < -input$logfc_cutoff] <- "Down-regulated"
          de_results$Significance <- as.character(de_results$Significance)
          
          rv$de_results <- de_results
          rv$sig_genes <- de_results[de_results$Significance != "Not Significant", ]
          
          incProgress(0.1, detail = "Done!")
        })
        
        showNotification(
          tags$div(
            icon("check-circle"),
            tags$strong(" edgeR analysis complete."),
            paste0(" Found ", nrow(rv$sig_genes), " significant DEGs.")
          ),
          type = "message", duration = 5)
        
      } else if (method == "limma_voom") {
        # ==================================================================
        # LIMMA-VOOM PATHWAY (voom weights on raw counts + limma)
        # ==================================================================
        withProgress(message = 'limma-voom DE analysis...', value = 0, {
          
          count_mat <- rv$raw_counts_for_deseq2  # shared raw counts matrix
          meta <- rv$unified_metadata
          
          # Align samples
          common_samples <- intersect(colnames(count_mat), rownames(meta))
          if (.mixed_platforms()) {
            common_samples <- intersect(
              common_samples,
              .gexpipe_call("gexpipe_platform_sample_ids", meta, "RNAseq")
            )
          }
          if (length(common_samples) < 3) {
            showNotification(
              tags$div(icon("exclamation-triangle"), tags$strong(" Too few samples."),
                       " limma-voom needs >= 3 samples matching between raw counts and metadata."),
              type = "error", duration = 8)
            rv$de_running <- FALSE
            return()
          }
          count_mat <- count_mat[, common_samples, drop = FALSE]
          meta <- meta[common_samples, , drop = FALSE]
          total_meta <- rv$unified_metadata
          
          # Ensure Condition is a factor
          meta$Condition <- factor(meta$Condition, levels = c(ref_lab, alt_lab))
          
          de_design <- gexpipe_build_de_design(meta)
          design <- de_design$design
          filt <- .gexpipe_call("gexpipe_independent_filter", count_mat, design = design)
          count_mat <- filt$expr
          .record_de_transparency(meta, "limma_voom", de_design$formula_desc, filt$note, total_meta)
          
          incProgress(0.3, detail = "Estimating mean-variance with voom...")
          
          # Apply voom to compute logCPM and precision weights
          v <- limma::voom(count_mat, design = design, plot = FALSE)
          
          incProgress(0.3, detail = "Fitting limma model...")
          
          fit <- limma::lmFit(v, design)
          fit <- limma::eBayes(fit)
          
          # Extract results for Condition effect (last column of design)
          coef_idx <- de_design$coef_condition
          tt <- limma::topTable(fit, coef = coef_idx, number = Inf, adjust.method = "BH", sort.by = "P")
          
          # Format to match limma output for downstream compatibility
          tt$Gene <- rownames(tt)
          de_results <- tt[, c("Gene", "logFC", "AveExpr", "P.Value", "adj.P.Val")]
          
          de_results$Significance <- "Not Significant"
          de_results$Significance[de_results$adj.P.Val < input$padj_cutoff &
                                    de_results$logFC > input$logfc_cutoff] <- "Up-regulated"
          de_results$Significance[de_results$adj.P.Val < input$padj_cutoff &
                                    de_results$logFC < -input$logfc_cutoff] <- "Down-regulated"
          de_results$Significance <- as.character(de_results$Significance)
          
          rv$de_results <- de_results
          rv$sig_genes <- de_results[de_results$Significance != "Not Significant", ]
          
          incProgress(0.1, detail = "Done!")
        })
        
        showNotification(
          tags$div(
            icon("check-circle"),
            tags$strong(" limma-voom analysis complete."),
            paste0(" Found ", nrow(rv$sig_genes), " significant DEGs.")
          ),
          type = "message", duration = 5)
        
      } else {
        # ==================================================================
        # LIMMA PATHWAY (single platform; batch-aware when multiple datasets)
        # ==================================================================
        withProgress(message = 'limma DE analysis...', value = 0, {
          
          if (is.null(rv$batch_corrected) || is.null(rv$unified_metadata)) {
            showNotification(
              tags$div(icon("exclamation-triangle"), tags$strong(" Missing data."),
                       " Run Steps 1-5 (Download, Groups, QC, Normalize, Batch) first."),
              type = "error", duration = 8)
            rv$de_running <- FALSE
            return()
          }
          
          meta <- rv$unified_metadata
          meta$Condition <- factor(meta$Condition, levels = c(ref_lab, alt_lab))
          rv$unified_metadata <- meta
          
          # Align samples: batch_corrected and metadata must match
          common_samp <- intersect(colnames(rv$batch_corrected), rownames(meta))
          if (length(common_samp) < 2) {
            showNotification(
              tags$div(icon("exclamation-triangle"), tags$strong(" Too few samples."),
                       " Need at least 2 samples with groups assigned."),
              type = "error", duration = 8)
            rv$de_running <- FALSE
            return()
          }
          rv$batch_corrected <- rv$batch_corrected[, common_samp, drop = FALSE]
          meta <- meta[common_samp, , drop = FALSE]
          total_meta <- rv$unified_metadata
          
          plat_info <- gexpipe_batch_covariate_info(meta)
          use_contrast <- length(unique(meta$Dataset)) == 1L && !plat_info$include_platform_covariate
          expr_de <- rv$batch_corrected
          
          if (use_contrast) {
            design <- model.matrix(~ 0 + Condition, data = meta)
            colnames(design) <- levels(meta$Condition)
            filt <- .gexpipe_call("gexpipe_independent_filter", expr_de, design = design)
            expr_de <- filt$expr
            contrast_expr <- paste0(alt_lab, " - ", ref_lab)
            contrast <- limma::makeContrasts(contrasts = contrast_expr, levels = design)
            fit <- limma::lmFit(expr_de, design)
            fit2 <- limma::contrasts.fit(fit, contrast)
            fit2 <- limma::eBayes(fit2)
            de_results <- limma::topTable(fit2, number = Inf, adjust.method = "BH")
            .record_de_transparency(
              meta, "limma", paste0("~ Condition (contrast: ", de_contrast_label, ")"), filt$note, total_meta
            )
          } else {
            de_design <- gexpipe_build_de_design(meta)
            design <- de_design$design
            filt <- .gexpipe_call("gexpipe_independent_filter", expr_de, design = design)
            expr_de <- filt$expr
            fit <- limma::lmFit(expr_de, design)
            fit2 <- limma::eBayes(fit)
            de_results <- limma::topTable(fit2, coef = de_design$coef_condition, number = Inf, adjust.method = "BH")
            .record_de_transparency(meta, "limma", de_design$formula_desc, filt$note, total_meta)
          }
          
          incProgress(0.5)
          de_results$Gene <- rownames(de_results)
          de_results <- de_results[, c("Gene", "logFC", "AveExpr", "P.Value", "adj.P.Val")]
          
          de_results$Significance <- "Not Significant"
          de_results$Significance[de_results$adj.P.Val < input$padj_cutoff &
                                    de_results$logFC > input$logfc_cutoff] <- "Up-regulated"
          de_results$Significance[de_results$adj.P.Val < input$padj_cutoff &
                                    de_results$logFC < -input$logfc_cutoff] <- "Down-regulated"
          de_results$Significance <- as.character(de_results$Significance)
          
          rv$de_results <- de_results
          rv$sig_genes <- de_results[de_results$Significance != "Not Significant", ]
          rv$consensus_complete <- TRUE
          rv$de_results_rna <- NULL
          rv$de_results_micro <- NULL
          rv$sig_genes_rna <- NULL
          rv$sig_genes_micro <- NULL
        })
      }

      if (!.mixed_platforms() && !is.null(rv$de_results)) {
        rv$consensus_complete <- TRUE
      }
    }, error = function(e) {
      msg <- conditionMessage(e)
      hint <- NULL
      if (grepl("negative", msg, ignore.case = TRUE)) {
        hint <- paste0(
          " GEO likely published normalized/log-scale values (not raw integer counts). ",
          "Re-run DE with limma, or re-download so NCBI raw counts are preferred over ",
          "normalized supplementary tables."
        )
      }
      showNotification(
        tags$div(icon("times-circle"), tags$strong(" DE analysis failed: "),
                 msg, hint),
        type = "error", duration = 12)
    })

    rv$de_running <- FALSE
  }

  observeEvent(input$run_de, .run_de_analysis())
  observeEvent(input$run_de_parallel, .run_de_analysis())
  
  output$total_degs <- renderInfoBox({
    if ((isTRUE(rv$merge_after_de) || identical(input$analysis_type, "parallel")) &&
        (!is.null(rv$sig_genes_rna) || !is.null(rv$sig_genes_micro))) {
      n_r <- if (is.null(rv$sig_genes_rna)) 0L else nrow(rv$sig_genes_rna)
      n_m <- if (is.null(rv$sig_genes_micro)) 0L else nrow(rv$sig_genes_micro)
      infoBox("RNA / Array DEGs", paste0(n_r, " / ", n_m), icon = icon("star", class = "fa-2x"),
              color = "yellow", fill = TRUE)
    } else {
      n <- if (!is.null(rv$sig_genes)) nrow(rv$sig_genes) else 0
      infoBox("Total DEGs", n, icon = icon("star", class = "fa-2x"),
              color = "yellow", fill = TRUE)
    }
  })
  
  output$up_genes <- renderInfoBox({
    if ((isTRUE(rv$merge_after_de) || identical(input$analysis_type, "parallel")) &&
        (!is.null(rv$de_results_rna) || !is.null(rv$de_results_micro))) {
      n_r <- if (is.null(rv$de_results_rna)) 0L else sum(rv$de_results_rna$Significance == "Up-regulated", na.rm = TRUE)
      n_m <- if (is.null(rv$de_results_micro)) 0L else sum(rv$de_results_micro$Significance == "Up-regulated", na.rm = TRUE)
      infoBox("Up (RNA / Array)", paste0(n_r, " / ", n_m), icon = icon("arrow-up", class = "fa-2x"),
              color = "red", fill = TRUE)
    } else {
      n <- if (!is.null(rv$de_results)) sum(rv$de_results$Significance == "Up-regulated") else 0
      infoBox("Up-regulated", n, icon = icon("arrow-up", class = "fa-2x"),
              color = "red", fill = TRUE)
    }
  })
  
  output$down_genes <- renderInfoBox({
    if ((isTRUE(rv$merge_after_de) || identical(input$analysis_type, "parallel")) &&
        (!is.null(rv$de_results_rna) || !is.null(rv$de_results_micro))) {
      n_r <- if (is.null(rv$de_results_rna)) 0L else sum(rv$de_results_rna$Significance == "Down-regulated", na.rm = TRUE)
      n_m <- if (is.null(rv$de_results_micro)) 0L else sum(rv$de_results_micro$Significance == "Down-regulated", na.rm = TRUE)
      infoBox("Down (RNA / Array)", paste0(n_r, " / ", n_m), icon = icon("arrow-down", class = "fa-2x"),
              color = "blue", fill = TRUE)
    } else {
      n <- if (!is.null(rv$de_results)) sum(rv$de_results$Significance == "Down-regulated") else 0
      infoBox("Down-regulated", n, icon = icon("arrow-down", class = "fa-2x"),
              color = "blue", fill = TRUE)
    }
  })
  
  # Pipeline verification: confirm DE method, design formula, and samples used
  output$de_model_details <- renderUI({
    req(rv$de_results)
    formula_txt <- if (!is.null(rv$de_design_formula) && nzchar(rv$de_design_formula)) {
      rv$de_design_formula
    } else {
      "Not recorded"
    }
    filter_txt <- if (!is.null(rv$de_gene_filter_note) && nzchar(rv$de_gene_filter_note)) {
      rv$de_gene_filter_note
    } else {
      "Independent filtering applied at DE time"
    }
    info <- rv$de_sample_info
    sample_txt <- if (!is.null(info) && !is.null(info$note)) info$note else "See unified metadata"
    plat_txt <- if (!is.null(info) && !is.null(info$platform_summary)) info$platform_summary else ""
    cond_txt <- if (!is.null(info) && !is.null(info$condition_summary)) info$condition_summary else ""
    tags$div(
      class = "alert alert-secondary",
      style = "margin: 0 15px 12px 15px; padding: 14px 18px; border-left: 5px solid #6c757d;",
      tags$p(style = "margin: 0 0 8px 0; font-weight: 700; font-size: 14px;",
             icon("calculator"), " Statistical model (this DE run)"),
      tags$p(style = "margin: 0 0 6px 0; font-size: 13px;",
             tags$strong("Design formula:"), " ", tags$code(formula_txt)),
      tags$p(style = "margin: 0 0 6px 0; font-size: 13px;",
             tags$strong("Gene filter:"), " ", filter_txt),
      tags$p(style = "margin: 0 0 6px 0; font-size: 13px;",
             tags$strong("Samples:"), " ", sample_txt),
      if (nzchar(plat_txt)) tags$p(style = "margin: 0 0 4px 0; font-size: 12px; color: #555;",
             tags$strong("Platform breakdown:"), " ", plat_txt) else NULL,
      if (nzchar(cond_txt)) tags$p(style = "margin: 0; font-size: 12px; color: #555;",
             tags$strong("Condition breakdown:"), " ", cond_txt) else NULL
    )
  })

  output$de_pipeline_verification <- renderUI({
    req(rv$de_results)
    method <- if (is.null(rv$de_method)) "limma" else rv$de_method
    batch_lab <- list(
      "limma" = "limma removeBatchEffect",
      "combat" = "ComBat (empirical Bayes)",
      "combat_ref" = "ComBat with reference batch",
      "quantile_limma" = "Quantile + limma",
      "hybrid" = "Hybrid (quantile + ComBat)",
      "sva" = "SVA + ComBat"
    )
    batch_method <- if (is.null(input$batch_method)) "combat_ref" else input$batch_method
    batch_label <- if (batch_method %in% names(batch_lab)) batch_lab[[batch_method]] else batch_method
    batch_done <- isTRUE(rv$batch_complete) && !is.null(rv$batch_corrected)
    n_batches <- length(unique(if (is.null(rv$unified_metadata$Dataset)) "1" else rv$unified_metadata$Dataset))
    batch_in_model <- n_batches > 1 && method %in% c("deseq2", "edger", "limma_voom")
    if (isTRUE(rv$merge_after_de) || identical(input$analysis_type, "parallel")) {
      rna_lab <- .gexpipe_de_method_label(method)
      b_rna <- if (!is.null(rv$last_batch_method_rna)) rv$last_batch_method_rna else batch_method
      b_micro <- if (!is.null(rv$last_batch_method_micro)) rv$last_batch_method_micro else batch_method
      b_rna_lab <- if (b_rna %in% names(batch_lab)) batch_lab[[b_rna]] else b_rna
      b_micro_lab <- if (b_micro %in% names(batch_lab)) batch_lab[[b_micro]] else b_micro
      return(tags$div(
        class = "alert alert-info",
        style = "margin: 0 15px 16px 15px; padding: 16px 20px; border-radius: 12px; border-left: 5px solid #3498db; background: linear-gradient(90deg, #e8f4f8 0%, #f8fafc 100%);",
        tags$p(
          style = "margin: 0 0 10px 0; font-weight: 700; font-size: 15px; color: #1e293b;",
          icon("check-circle", style = "color: #10b981; margin-right: 8px;"),
          "Pipeline verification - two separate DE engines"
        ),
        tags$p(
          style = "margin: 0 0 6px 0; font-size: 13px; color: #334155; line-height: 1.6;",
          tags$strong("RNA-seq DE:"), " ", rna_lab,
          if (.gexpipe_is_count_de(method)) " on raw counts (Dataset in the model if 2+ RNA GSEs)." else " on the TMM / log-CPM matrix."
        ),
        tags$p(
          style = "margin: 0 0 6px 0; font-size: 13px; color: #334155; line-height: 1.6;",
          tags$strong("Microarray DE:"), " limma on the array matrix only."
        ),
        tags$p(
          style = "margin: 0 0 6px 0; font-size: 13px; color: #334155; line-height: 1.6;",
          tags$strong("Step 5 batch:"), " RNA-seq ", b_rna_lab, "; microarray ", b_micro_lab, "."
        ),
        tags$p(
          style = "margin: 0; font-size: 12px; color: #64748b;",
          "Matrices were not mixed. Step 7 intersects same-direction DEGs."
        )
      ))
    }
    tags$div(
      class = "alert alert-info",
      style = "margin: 0 15px 16px 15px; padding: 16px 20px; border-radius: 12px; border-left: 5px solid #3498db; background: linear-gradient(90deg, #e8f4f8 0%, #f8fafc 100%);",
      tags$p(
        style = "margin: 0 0 10px 0; font-weight: 700; font-size: 15px; color: #1e293b;",
        icon("check-circle", style = "color: #10b981; margin-right: 8px;"),
        "Pipeline verification - Volcano shows real DE results"
      ),
      tags$p(
        style = "margin: 0 0 6px 0; font-size: 13px; color: #334155; line-height: 1.6;",
        tags$strong("DE method:"), " ", if (method == "deseq2") "DESeq2" else if (method == "edger") "edgeR" else if (method == "limma_voom") "limma-voom" else "limma",
        if (batch_in_model) " - batch (Dataset) included in the statistical model, so the volcano reflects differential expression after adjusting for batch." else "."
      ),
      tags$p(
        style = "margin: 0 0 6px 0; font-size: 13px; color: #334155; line-height: 1.6;",
        tags$strong("Batch correction (Step 5):"), " ",
        if (batch_done) paste0(batch_label, " - applied to the expression matrix used for heatmaps, WGCNA, and downstream steps.")
        else "Not applied (single dataset or Step 5 skipped)."
      ),
      tags$p(
        style = "margin: 0; font-size: 12px; color: #64748b;",
        "All prior steps (Download -> Normalize -> Groups -> Batch correction) were completed before DE. The volcano plot uses the actual test statistics (log2FC and adjusted p-value) from the selected DE method. ",
        "Use the ", tags$strong("How to check your results are valid"), " box below to verify groups and ML prediction performance."
      )
    )
  })

  .gexpipe_draw_volcano <- function(de_results, title, method_label, logfc = NULL, padj = NULL) {
    volcano_data <- as.data.frame(de_results, stringsAsFactors = FALSE)
    if (!"Gene" %in% names(volcano_data)) volcano_data$Gene <- rownames(de_results)
    volcano_data$Gene <- as.character(volcano_data$Gene)
    if (!"Significance" %in% names(volcano_data)) volcano_data$Significance <- "Not Significant"
    volcano_data$Significance <- as.character(volcano_data$Significance)
    volcano_data$Significance[!volcano_data$Significance %in% c("Up-regulated", "Down-regulated", "Not Significant")] <- "Not Significant"
    volcano_data$Significance <- factor(volcano_data$Significance, levels = c("Not Significant", "Down-regulated", "Up-regulated"))
    min_padj <- min(volcano_data$adj.P.Val[volcano_data$adj.P.Val > 0], na.rm = TRUE)
    if (is.infinite(min_padj) || is.na(min_padj)) min_padj <- 1e-300
    volcano_data$adj.P.Val[volcano_data$adj.P.Val == 0] <- min_padj
    volcano_data$neg_log10_padj <- -log10(volcano_data$adj.P.Val)
    max_finite <- max(volcano_data$neg_log10_padj[is.finite(volcano_data$neg_log10_padj)], na.rm = TRUE)
    if (is.finite(max_finite)) volcano_data$neg_log10_padj[!is.finite(volcano_data$neg_log10_padj)] <- max_finite + 1
    volcano_data <- volcano_data[is.finite(volcano_data$logFC) & is.finite(volcano_data$neg_log10_padj), ]
    volcano_data$Label <- ""
    top_genes_to_label <- rbind(
      head(volcano_data[order(volcano_data$adj.P.Val), ], 15),
      head(volcano_data[order(-abs(volcano_data$logFC)), ], 15)
    )
    volcano_data$Label[volcano_data$Gene %in% top_genes_to_label$Gene] <-
      volcano_data$Gene[volcano_data$Gene %in% top_genes_to_label$Gene]
    n_up <- sum(volcano_data$Significance == "Up-regulated", na.rm = TRUE)
    n_down <- sum(volcano_data$Significance == "Down-regulated", na.rm = TRUE)
    n_sig <- n_up + n_down
    if (is.null(logfc)) logfc <- .de_cutoffs()$logfc
    if (is.null(padj)) padj <- .de_cutoffs()$padj
    ggplot2::ggplot(volcano_data, ggplot2::aes(x = logFC, y = neg_log10_padj, color = Significance)) +
      ggplot2::geom_point(alpha = 0.6, size = 2) +
      ggplot2::scale_color_manual(
        values = c("Up-regulated" = "#e74c3c", "Down-regulated" = "#3498db", "Not Significant" = "gray70"),
        name = "Significance"
      ) +
      gexpipe_pub_theme(base_size = 13) +
      ggplot2::labs(
        title = title,
        subtitle = paste0(method_label, " - DEGs: ", n_sig, " (Up: ", n_up, ", Down: ", n_down,
                          ") | LogFC +/-", logfc, ", Adj.P <= ", padj),
        x = "log2 fold change",
        y = "-log10(adjusted p-value)"
      ) +
      ggplot2::geom_hline(yintercept = -log10(padj), linetype = "dashed", color = "gray40", alpha = 0.7) +
      ggplot2::geom_vline(xintercept = c(-logfc, logfc), linetype = "dashed", color = "gray40", alpha = 0.7) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5))
  }

  .gexpipe_top_degs_dt <- function(sig) {
    if (is.null(sig) || !is.data.frame(sig) || nrow(sig) == 0L) {
      return(DT::datatable(
        data.frame(Message = "Run DE to see this table."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    cols <- intersect(c("Gene", "logFC", "adj.P.Val", "Significance"), names(sig))
    top <- head(sig[order(sig$adj.P.Val), cols, drop = FALSE], 30)
    dt <- DT::datatable(top, options = list(pageLength = 15, dom = "t"), rownames = FALSE)
    num_cols <- intersect(c("logFC", "adj.P.Val"), names(top))
    if (length(num_cols) > 0L) dt <- DT::formatRound(dt, columns = num_cols, digits = 4)
    dt
  }

  output$volcano_plot_rna <- renderPlot({
    req(rv$de_results_rna)
    rna_lab <- switch(
      if (is.null(rv$de_method)) "limma" else rv$de_method,
      deseq2 = "DESeq2", edger = "edgeR", limma_voom = "limma-voom", "limma"
    )
    .gexpipe_draw_volcano(
      rv$de_results_rna, "RNA-seq volcano (this platform only)", rna_lab,
      logfc = .de_cutoffs("RNAseq")$logfc, padj = .de_cutoffs("RNAseq")$padj
    )
  })
  output$volcano_plot_micro <- renderPlot({
    req(rv$de_results_micro)
    .gexpipe_draw_volcano(
      rv$de_results_micro, "Microarray volcano (this platform only)", "limma",
      logfc = .de_cutoffs("Microarray")$logfc, padj = .de_cutoffs("Microarray")$padj
    )
  })
  output$top_degs_table_rna <- renderDT({
    .gexpipe_top_degs_dt(rv$sig_genes_rna)
  })
  output$top_degs_table_micro <- renderDT({
    .gexpipe_top_degs_dt(rv$sig_genes_micro)
  })

  output$volcano_plot <- renderPlot({
    req(rv$de_results)
    tryCatch({
      volcano_data <- as.data.frame(rv$de_results, stringsAsFactors = FALSE)
      if (!"Gene" %in% names(volcano_data)) volcano_data$Gene <- rownames(rv$de_results)
      volcano_data$Gene <- as.character(volcano_data$Gene)
      if (!"Significance" %in% names(volcano_data)) volcano_data$Significance <- "Not Significant"
      volcano_data$Significance <- as.character(volcano_data$Significance)
      volcano_data$Significance[!volcano_data$Significance %in% c("Up-regulated", "Down-regulated", "Not Significant")] <- "Not Significant"
      volcano_data$Significance <- factor(volcano_data$Significance, levels = c("Not Significant", "Down-regulated", "Up-regulated"))

      min_padj <- min(volcano_data$adj.P.Val[volcano_data$adj.P.Val > 0], na.rm = TRUE)
      if (is.infinite(min_padj) || is.na(min_padj)) min_padj <- 1e-300
      volcano_data$adj.P.Val[volcano_data$adj.P.Val == 0] <- min_padj
      volcano_data$neg_log10_padj <- -log10(volcano_data$adj.P.Val)
      max_finite <- max(volcano_data$neg_log10_padj[is.finite(volcano_data$neg_log10_padj)], na.rm = TRUE)
      if (is.finite(max_finite)) volcano_data$neg_log10_padj[!is.finite(volcano_data$neg_log10_padj)] <- max_finite + 1
      volcano_data <- volcano_data[is.finite(volcano_data$logFC) & is.finite(volcano_data$neg_log10_padj), ]

      volcano_data$Label <- ""
      top_genes_to_label <- rbind(
        head(volcano_data[order(volcano_data$adj.P.Val), ], 15),
        head(volcano_data[order(-abs(volcano_data$logFC)), ], 15)
      )
      volcano_data$Label[volcano_data$Gene %in% top_genes_to_label$Gene] <-
        volcano_data$Gene[volcano_data$Gene %in% top_genes_to_label$Gene]

      n_up <- sum(volcano_data$Significance == "Up-regulated", na.rm = TRUE)
      n_down <- sum(volcano_data$Significance == "Down-regulated", na.rm = TRUE)
      n_sig <- n_up + n_down
      method <- if (is.null(rv$de_method)) "limma" else rv$de_method
      method_label <- switch(method, deseq2 = "DESeq2", edger = "edgeR", limma_voom = "limma-voom", "limma")
      n_batches <- length(unique(if (is.null(rv$unified_metadata$Dataset)) "1" else rv$unified_metadata$Dataset))
      batch_note <- if (n_batches > 1 && method %in% c("deseq2", "edger", "limma_voom")) " | batch in model" else ""
      sub_line1 <- paste0(method_label, batch_note, " \u2014 DEGs: ", n_sig, " (Up: ", n_up, ", Down: ", n_down, ")")
      sub_line2 <- paste0("LogFC \u00b1", input$logfc_cutoff, ", Adj.P \u2264 ", input$padj_cutoff)

      ref_lab <- if (!is.null(rv$condition_ref_label)) rv$condition_ref_label else "Normal"
      alt_lab <- if (!is.null(rv$condition_alt_label)) rv$condition_alt_label else "Disease"
      volcano_title <- paste0("Volcano Plot: ", alt_lab, " vs ", ref_lab)

      p <- ggplot2::ggplot(volcano_data, ggplot2::aes(x = logFC, y = neg_log10_padj, color = Significance)) +
        ggplot2::geom_point(alpha = 0.6, size = 2) +
        ggplot2::scale_color_manual(
          values = c("Up-regulated" = "#e74c3c", "Down-regulated" = "#3498db", "Not Significant" = "gray70"),
          name = "Significance"
        ) +
        gexpipe_pub_theme(base_size = 14) +
        ggplot2::labs(
          title = volcano_title,
          subtitle = paste0(sub_line1, "\n", sub_line2),
          x = "Log2 Fold Change",
          y = "-Log10 Adjusted P-value"
        ) +
        ggplot2::geom_hline(yintercept = -log10(input$padj_cutoff), linetype = "dashed", color = "gray40", alpha = 0.7) +
        ggplot2::geom_vline(xintercept = c(-input$logfc_cutoff, input$logfc_cutoff), linetype = "dashed", color = "gray40", alpha = 0.7) +
        ggrepel::geom_text_repel(
          ggplot2::aes(label = Label),
          size = 3,
          max.overlaps = 20,
          box.padding = 0.5,
          segment.color = "gray50"
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 16),
          plot.subtitle = ggplot2::element_text(size = 12),
          legend.position = "right"
        ) +
        ggplot2::scale_x_continuous(breaks = pretty(volcano_data$logFC, n = 8)) +
        ggplot2::scale_y_continuous(breaks = pretty(volcano_data$neg_log10_padj, n = 8))
      p
    }, error = function(e) {
      plot.new()
      text(0.5, 0.6, "Volcano plot error", cex = 1.5, font = 2)
      text(0.5, 0.45, conditionMessage(e), cex = 1, col = "gray40")
      text(0.5, 0.3, "Check DE results and try Run DE Analysis again.", cex = 0.9, col = "gray50")
    })
  })
  
  output$heatmap_plot <- renderPlot({
    req(rv$de_results, rv$batch_corrected)
    
    tryCatch({
      # Get top DE genes sorted by adjusted p-value
      top <- head(rv$de_results[order(rv$de_results$adj.P.Val), ], input$top_genes)
      
      # Filter to genes that actually exist in the batch-corrected matrix
      valid_genes <- intersect(top$Gene, rownames(rv$batch_corrected))
      
      if (length(valid_genes) == 0) {
        # Fallback: try case-insensitive match
        bc_genes_upper <- toupper(rownames(rv$batch_corrected))
        names(bc_genes_upper) <- rownames(rv$batch_corrected)
        top_upper <- toupper(top$Gene)
        matched <- bc_genes_upper[bc_genes_upper %in% top_upper]
        valid_genes <- names(matched)
      }
      
      if (length(valid_genes) < 2) {
        plot.new()
        text(0.5, 0.5,
             paste0("Cannot generate heatmap: only ", length(valid_genes),
                    " of ", nrow(top), " top DE genes found in the expression matrix.\n",
                    "This can happen when DE was run on raw counts (DESeq2/edgeR)\n",
                    "and gene names differ from the normalized matrix."),
             cex = 1.2, col = "gray40")
        return()
      }
      
      expr <- rv$batch_corrected[valid_genes, , drop = FALSE]
      
      # Remove rows with zero variance (constant expression - can't scale)
      row_vars <- apply(expr, 1, var, na.rm = TRUE)
      expr <- expr[!is.na(row_vars) & row_vars > 0, , drop = FALSE]
      
      if (nrow(expr) < 2) {
        plot.new()
        text(0.5, 0.5, "Too few genes with variable expression for heatmap.",
             cex = 1.2, col = "gray40")
        return()
      }
      
      expr_scaled <- t(scale(t(expr)))
      # Align metadata to expression columns so row.names length = nrow(annot) (avoids "dimnames [1] not equal to array extent")
      samp <- colnames(expr_scaled)
      meta <- rv$unified_metadata
      idx <- match(samp, rownames(meta))
      if (any(is.na(idx)) && "SampleID" %in% names(meta)) idx <- match(samp, as.character(meta$SampleID))
      cond <- if ("Condition" %in% names(meta) && all(!is.na(idx))) meta$Condition[idx] else rep(NA_character_, length(samp))
      dset <- if ("Dataset" %in% names(meta) && all(!is.na(idx))) meta$Dataset[idx] else rep(NA_character_, length(samp))
      if (length(cond) != length(samp)) cond <- rep(NA_character_, length(samp))
      if (length(dset) != length(samp)) dset <- rep(NA_character_, length(samp))
      annot <- data.frame(Condition = cond, Dataset = dset, row.names = samp)
      
      annot_colors <- list(Condition = c(Normal = "#3498db", Disease = "#e74c3c"))
      
      pheatmap::pheatmap(expr_scaled, annotation_col = annot, annotation_colors = annot_colors,
               color = colorRampPalette(c("#3498db", "white", "#e74c3c"))(100),
               show_colnames = FALSE, fontsize_row = max(6, 12 - nrow(expr)/10),
               main = paste0("Top ", nrow(expr), " DE Genes (of ", input$top_genes, " requested)"),
               border_color = NA)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Heatmap error:", conditionMessage(e)),
           cex = 1.0, col = "#e74c3c")
    })
  })

  # Download volcano plot (PNG)
  output$download_volcano_png <- downloadHandler(
    filename = function() "Volcano_Plot.png",
    content = function(file) {
      req(rv$de_results)
      volcano_data <- as.data.frame(rv$de_results, stringsAsFactors = FALSE)
      if (!"Gene" %in% names(volcano_data)) volcano_data$Gene <- rownames(rv$de_results)
      volcano_data$Gene <- as.character(volcano_data$Gene)
      if (!"Significance" %in% names(volcano_data)) volcano_data$Significance <- "Not Significant"
      volcano_data$Significance <- as.character(volcano_data$Significance)
      volcano_data$Significance[!volcano_data$Significance %in% c("Up-regulated", "Down-regulated", "Not Significant")] <- "Not Significant"
      volcano_data$Significance <- factor(volcano_data$Significance, levels = c("Not Significant", "Down-regulated", "Up-regulated"))
      min_padj <- min(volcano_data$adj.P.Val[volcano_data$adj.P.Val > 0], na.rm = TRUE)
      if (is.infinite(min_padj) || is.na(min_padj)) min_padj <- 1e-300
      volcano_data$adj.P.Val[volcano_data$adj.P.Val == 0] <- min_padj
      volcano_data$neg_log10_padj <- -log10(volcano_data$adj.P.Val)
      max_finite <- max(volcano_data$neg_log10_padj[is.finite(volcano_data$neg_log10_padj)], na.rm = TRUE)
      if (is.finite(max_finite)) volcano_data$neg_log10_padj[!is.finite(volcano_data$neg_log10_padj)] <- max_finite + 1
      volcano_data <- volcano_data[is.finite(volcano_data$logFC) & is.finite(volcano_data$neg_log10_padj), ]
      volcano_data$Label <- ""
      top_genes_to_label <- rbind(
        head(volcano_data[order(volcano_data$adj.P.Val), ], 15),
        head(volcano_data[order(-abs(volcano_data$logFC)), ], 15)
      )
      volcano_data$Label[volcano_data$Gene %in% top_genes_to_label$Gene] <- volcano_data$Gene[volcano_data$Gene %in% top_genes_to_label$Gene]
      n_up <- sum(volcano_data$Significance == "Up-regulated", na.rm = TRUE)
      n_down <- sum(volcano_data$Significance == "Down-regulated", na.rm = TRUE)
      n_sig <- n_up + n_down
      logfc_cut <- if (!is.null(input$logfc_cutoff)) input$logfc_cutoff else 0.5
      padj_cut <- if (!is.null(input$padj_cutoff)) input$padj_cutoff else 0.05
      p <- ggplot2::ggplot(volcano_data, ggplot2::aes(x = logFC, y = neg_log10_padj, color = Significance)) +
        ggplot2::geom_point(alpha = 0.6, size = 2) +
        ggplot2::scale_color_manual(values = c("Up-regulated" = "#e74c3c", "Down-regulated" = "#3498db", "Not Significant" = "gray70"), name = "Significance") +
        gexpipe_pub_theme(base_size = 14) +
        ref_lab <- if (!is.null(rv$condition_ref_label)) rv$condition_ref_label else "Normal"
        alt_lab <- if (!is.null(rv$condition_alt_label)) rv$condition_alt_label else "Disease"
        ggplot2::labs(title = paste0("Volcano Plot: ", alt_lab, " vs ", ref_lab), subtitle = paste0("DEGs: ", n_sig, " (Up: ", n_up, ", Down: ", n_down, ")"), x = "Log2 Fold Change", y = "-Log10 Adjusted P-value") +
        ggplot2::geom_hline(yintercept = -log10(padj_cut), linetype = "dashed", color = "gray40", alpha = 0.7) +
        ggplot2::geom_vline(xintercept = c(-logfc_cut, logfc_cut), linetype = "dashed", color = "gray40", alpha = 0.7) +
        ggrepel::geom_text_repel(ggplot2::aes(label = Label), size = 3, max.overlaps = 20, box.padding = 0.5, segment.color = "gray50") +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 16), plot.subtitle = ggplot2::element_text(size = 12), legend.position = "right")
      ggplot2::ggsave(file, plot = p, device = "png", width = 10, height = 7, dpi = 150, bg = "white")
    }
  )

  output$download_volcano_jpg <- downloadHandler(
    filename = function() "Volcano_Plot.jpg",
    content = function(file) {
      req(rv$de_results)
      volcano_data <- as.data.frame(rv$de_results, stringsAsFactors = FALSE)
      if (!"Gene" %in% names(volcano_data)) volcano_data$Gene <- rownames(rv$de_results)
      volcano_data$Gene <- as.character(volcano_data$Gene)
      if (!"Significance" %in% names(volcano_data)) volcano_data$Significance <- "Not Significant"
      volcano_data$Significance <- as.character(volcano_data$Significance)
      volcano_data$Significance[!volcano_data$Significance %in% c("Up-regulated", "Down-regulated", "Not Significant")] <- "Not Significant"
      volcano_data$Significance <- factor(volcano_data$Significance, levels = c("Not Significant", "Down-regulated", "Up-regulated"))
      min_padj <- min(volcano_data$adj.P.Val[volcano_data$adj.P.Val > 0], na.rm = TRUE)
      if (is.infinite(min_padj) || is.na(min_padj)) min_padj <- 1e-300
      volcano_data$adj.P.Val[volcano_data$adj.P.Val == 0] <- min_padj
      volcano_data$neg_log10_padj <- -log10(volcano_data$adj.P.Val)
      max_finite <- max(volcano_data$neg_log10_padj[is.finite(volcano_data$neg_log10_padj)], na.rm = TRUE)
      if (is.finite(max_finite)) volcano_data$neg_log10_padj[!is.finite(volcano_data$neg_log10_padj)] <- max_finite + 1
      volcano_data <- volcano_data[is.finite(volcano_data$logFC) & is.finite(volcano_data$neg_log10_padj), ]
      volcano_data$Label <- ""
      top_genes_to_label <- rbind(head(volcano_data[order(volcano_data$adj.P.Val), ], 15), head(volcano_data[order(-abs(volcano_data$logFC)), ], 15))
      volcano_data$Label[volcano_data$Gene %in% top_genes_to_label$Gene] <- volcano_data$Gene[volcano_data$Gene %in% top_genes_to_label$Gene]
      n_up <- sum(volcano_data$Significance == "Up-regulated", na.rm = TRUE)
      n_down <- sum(volcano_data$Significance == "Down-regulated", na.rm = TRUE)
      n_sig <- n_up + n_down
      logfc_cut <- if (!is.null(input$logfc_cutoff)) input$logfc_cutoff else 0.5
      padj_cut <- if (!is.null(input$padj_cutoff)) input$padj_cutoff else 0.05
      p <- ggplot2::ggplot(volcano_data, ggplot2::aes(x = logFC, y = neg_log10_padj, color = Significance)) +
        ggplot2::geom_point(alpha = 0.6, size = 2) +
        ggplot2::scale_color_manual(values = c("Up-regulated" = "#e74c3c", "Down-regulated" = "#3498db", "Not Significant" = "gray70"), name = "Significance") +
        gexpipe_pub_theme(base_size = 14) +
        ref_lab <- if (!is.null(rv$condition_ref_label)) rv$condition_ref_label else "Normal"
        alt_lab <- if (!is.null(rv$condition_alt_label)) rv$condition_alt_label else "Disease"
        ggplot2::labs(title = paste0("Volcano Plot: ", alt_lab, " vs ", ref_lab), subtitle = paste0("DEGs: ", n_sig, " (Up: ", n_up, ", Down: ", n_down, ")"), x = "Log2 Fold Change", y = "-Log10 Adjusted P-value") +
        ggplot2::geom_hline(yintercept = -log10(padj_cut), linetype = "dashed", color = "gray40", alpha = 0.7) +
        ggplot2::geom_vline(xintercept = c(-logfc_cut, logfc_cut), linetype = "dashed", color = "gray40", alpha = 0.7) +
        ggrepel::geom_text_repel(ggplot2::aes(label = Label), size = 3, max.overlaps = 20, box.padding = 0.5, segment.color = "gray50") +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 16), plot.subtitle = ggplot2::element_text(size = 12), legend.position = "right")
      ggplot2::ggsave(file, plot = p, device = "jpeg", width = 10, height = 7, dpi = IMAGE_DPI, bg = "white")
    }
  )

  # Download volcano plot (PDF)
  output$download_volcano_pdf <- downloadHandler(
    filename = function() "Volcano_Plot.pdf",
    content = function(file) {
      req(rv$de_results)
      volcano_data <- as.data.frame(rv$de_results, stringsAsFactors = FALSE)
      if (!"Gene" %in% names(volcano_data)) volcano_data$Gene <- rownames(rv$de_results)
      volcano_data$Gene <- as.character(volcano_data$Gene)
      if (!"Significance" %in% names(volcano_data)) volcano_data$Significance <- "Not Significant"
      volcano_data$Significance <- as.character(volcano_data$Significance)
      volcano_data$Significance[!volcano_data$Significance %in% c("Up-regulated", "Down-regulated", "Not Significant")] <- "Not Significant"
      volcano_data$Significance <- factor(volcano_data$Significance, levels = c("Not Significant", "Down-regulated", "Up-regulated"))
      min_padj <- min(volcano_data$adj.P.Val[volcano_data$adj.P.Val > 0], na.rm = TRUE)
      if (is.infinite(min_padj) || is.na(min_padj)) min_padj <- 1e-300
      volcano_data$adj.P.Val[volcano_data$adj.P.Val == 0] <- min_padj
      volcano_data$neg_log10_padj <- -log10(volcano_data$adj.P.Val)
      max_finite <- max(volcano_data$neg_log10_padj[is.finite(volcano_data$neg_log10_padj)], na.rm = TRUE)
      if (is.finite(max_finite)) volcano_data$neg_log10_padj[!is.finite(volcano_data$neg_log10_padj)] <- max_finite + 1
      volcano_data <- volcano_data[is.finite(volcano_data$logFC) & is.finite(volcano_data$neg_log10_padj), ]
      volcano_data$Label <- ""
      top_genes_to_label <- rbind(head(volcano_data[order(volcano_data$adj.P.Val), ], 15), head(volcano_data[order(-abs(volcano_data$logFC)), ], 15))
      volcano_data$Label[volcano_data$Gene %in% top_genes_to_label$Gene] <- volcano_data$Gene[volcano_data$Gene %in% top_genes_to_label$Gene]
      n_up <- sum(volcano_data$Significance == "Up-regulated", na.rm = TRUE)
      n_down <- sum(volcano_data$Significance == "Down-regulated", na.rm = TRUE)
      n_sig <- n_up + n_down
      logfc_cut <- if (!is.null(input$logfc_cutoff)) input$logfc_cutoff else 0.5
      padj_cut <- if (!is.null(input$padj_cutoff)) input$padj_cutoff else 0.05
      p <- ggplot2::ggplot(volcano_data, ggplot2::aes(x = logFC, y = neg_log10_padj, color = Significance)) +
        ggplot2::geom_point(alpha = 0.6, size = 2) +
        ggplot2::scale_color_manual(values = c("Up-regulated" = "#e74c3c", "Down-regulated" = "#3498db", "Not Significant" = "gray70"), name = "Significance") +
        gexpipe_pub_theme(base_size = 14) +
        ref_lab <- if (!is.null(rv$condition_ref_label)) rv$condition_ref_label else "Normal"
        alt_lab <- if (!is.null(rv$condition_alt_label)) rv$condition_alt_label else "Disease"
        ggplot2::labs(title = paste0("Volcano Plot: ", alt_lab, " vs ", ref_lab), subtitle = paste0("DEGs: ", n_sig, " (Up: ", n_up, ", Down: ", n_down, ")"), x = "Log2 Fold Change", y = "-Log10 Adjusted P-value") +
        ggplot2::geom_hline(yintercept = -log10(padj_cut), linetype = "dashed", color = "gray40", alpha = 0.7) +
        ggplot2::geom_vline(xintercept = c(-logfc_cut, logfc_cut), linetype = "dashed", color = "gray40", alpha = 0.7) +
        ggrepel::geom_text_repel(ggplot2::aes(label = Label), size = 3, max.overlaps = 20, box.padding = 0.5, segment.color = "gray50") +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 16), plot.subtitle = ggplot2::element_text(size = 12), legend.position = "right")
      ggplot2::ggsave(file, plot = p, device = "pdf", width = 10, height = 7, bg = "white")
    }
  )

  # Download heatmap (PNG)
  output$download_heatmap_png <- downloadHandler(
    filename = function() "DE_Heatmap_Top_Genes.png",
    content = function(file) {
      req(rv$de_results, rv$batch_corrected)
      top <- head(rv$de_results[order(rv$de_results$adj.P.Val), ], input$top_genes)
      valid_genes <- intersect(top$Gene, rownames(rv$batch_corrected))
      if (length(valid_genes) == 0) {
        bc_genes_upper <- toupper(rownames(rv$batch_corrected)); names(bc_genes_upper) <- rownames(rv$batch_corrected)
        top_upper <- toupper(top$Gene); matched <- bc_genes_upper[bc_genes_upper %in% top_upper]; valid_genes <- names(matched)
      }
      if (length(valid_genes) < 2) return()
      expr <- rv$batch_corrected[valid_genes, , drop = FALSE]
      row_vars <- apply(expr, 1, var, na.rm = TRUE)
      expr <- expr[!is.na(row_vars) & row_vars > 0, , drop = FALSE]
      if (nrow(expr) < 2) return()
      expr_scaled <- t(scale(t(expr)))
      samp <- colnames(expr_scaled)
      meta <- rv$unified_metadata
      idx <- match(samp, rownames(meta))
      if (any(is.na(idx)) && "SampleID" %in% names(meta)) idx <- match(samp, as.character(meta$SampleID))
      cond <- if ("Condition" %in% names(meta) && all(!is.na(idx))) meta$Condition[idx] else rep(NA_character_, length(samp))
      dset <- if ("Dataset" %in% names(meta) && all(!is.na(idx))) meta$Dataset[idx] else rep(NA_character_, length(samp))
      if (length(cond) != length(samp)) cond <- rep(NA_character_, length(samp))
      if (length(dset) != length(samp)) dset <- rep(NA_character_, length(samp))
      annot <- data.frame(Condition = cond, Dataset = dset, row.names = samp)
      annot_colors <- list(Condition = c(Normal = "#3498db", Disease = "#e74c3c"))
      png(file, width = 1200, height = 800, res = 150, bg = "white")
      pheatmap::pheatmap(expr_scaled, annotation_col = annot, annotation_colors = annot_colors,
               color = colorRampPalette(c("#3498db", "white", "#e74c3c"))(100),
               show_colnames = FALSE, fontsize_row = max(6, 12 - nrow(expr)/10),
               main = paste0("Top ", nrow(expr), " DE Genes (of ", input$top_genes, " requested)"), border_color = NA)
      dev.off()
    }
  )

  output$download_heatmap_jpg <- downloadHandler(
    filename = function() "DE_Heatmap_Top_Genes.jpg",
    content = function(file) {
      req(rv$de_results, rv$batch_corrected)
      top <- head(rv$de_results[order(rv$de_results$adj.P.Val), ], input$top_genes)
      valid_genes <- intersect(top$Gene, rownames(rv$batch_corrected))
      if (length(valid_genes) == 0) {
        bc_genes_upper <- toupper(rownames(rv$batch_corrected)); names(bc_genes_upper) <- rownames(rv$batch_corrected)
        top_upper <- toupper(top$Gene); matched <- bc_genes_upper[bc_genes_upper %in% top_upper]; valid_genes <- names(matched)
      }
      if (length(valid_genes) < 2) return()
      expr <- rv$batch_corrected[valid_genes, , drop = FALSE]
      row_vars <- apply(expr, 1, var, na.rm = TRUE)
      expr <- expr[!is.na(row_vars) & row_vars > 0, , drop = FALSE]
      if (nrow(expr) < 2) return()
      expr_scaled <- t(scale(t(expr)))
      samp <- colnames(expr_scaled)
      meta <- rv$unified_metadata
      idx <- match(samp, rownames(meta))
      if (any(is.na(idx)) && "SampleID" %in% names(meta)) idx <- match(samp, as.character(meta$SampleID))
      cond <- if ("Condition" %in% names(meta) && all(!is.na(idx))) meta$Condition[idx] else rep(NA_character_, length(samp))
      dset <- if ("Dataset" %in% names(meta) && all(!is.na(idx))) meta$Dataset[idx] else rep(NA_character_, length(samp))
      if (length(cond) != length(samp)) cond <- rep(NA_character_, length(samp))
      if (length(dset) != length(samp)) dset <- rep(NA_character_, length(samp))
      annot <- data.frame(Condition = cond, Dataset = dset, row.names = samp)
      annot_colors <- list(Condition = c(Normal = "#3498db", Disease = "#e74c3c"))
      jpeg(file, width = 8, height = 5.33, res = IMAGE_DPI, units = "in", bg = "white", quality = 95)
      pheatmap::pheatmap(expr_scaled, annotation_col = annot, annotation_colors = annot_colors,
               color = colorRampPalette(c("#3498db", "white", "#e74c3c"))(100),
               show_colnames = FALSE, fontsize_row = max(6, 12 - nrow(expr)/10),
               main = paste0("Top ", nrow(expr), " DE Genes (of ", input$top_genes, " requested)"), border_color = NA)
      dev.off()
    }
  )

  # Download heatmap (PDF)
  output$download_heatmap_pdf <- downloadHandler(
    filename = function() "DE_Heatmap_Top_Genes.pdf",
    content = function(file) {
      req(rv$de_results, rv$batch_corrected)
      top <- head(rv$de_results[order(rv$de_results$adj.P.Val), ], input$top_genes)
      valid_genes <- intersect(top$Gene, rownames(rv$batch_corrected))
      if (length(valid_genes) == 0) {
        bc_genes_upper <- toupper(rownames(rv$batch_corrected)); names(bc_genes_upper) <- rownames(rv$batch_corrected)
        top_upper <- toupper(top$Gene); matched <- bc_genes_upper[bc_genes_upper %in% top_upper]; valid_genes <- names(matched)
      }
      if (length(valid_genes) < 2) return()
      expr <- rv$batch_corrected[valid_genes, , drop = FALSE]
      row_vars <- apply(expr, 1, var, na.rm = TRUE)
      expr <- expr[!is.na(row_vars) & row_vars > 0, , drop = FALSE]
      if (nrow(expr) < 2) return()
      expr_scaled <- t(scale(t(expr)))
      samp <- colnames(expr_scaled)
      meta <- rv$unified_metadata
      idx <- match(samp, rownames(meta))
      if (any(is.na(idx)) && "SampleID" %in% names(meta)) idx <- match(samp, as.character(meta$SampleID))
      cond <- if ("Condition" %in% names(meta) && all(!is.na(idx))) meta$Condition[idx] else rep(NA_character_, length(samp))
      dset <- if ("Dataset" %in% names(meta) && all(!is.na(idx))) meta$Dataset[idx] else rep(NA_character_, length(samp))
      if (length(cond) != length(samp)) cond <- rep(NA_character_, length(samp))
      if (length(dset) != length(samp)) dset <- rep(NA_character_, length(samp))
      annot <- data.frame(Condition = cond, Dataset = dset, row.names = samp)
      annot_colors <- list(Condition = c(Normal = "#3498db", Disease = "#e74c3c"))
      pdf(file, width = 10, height = 7, bg = "white")
      pheatmap::pheatmap(expr_scaled, annotation_col = annot, annotation_colors = annot_colors,
               color = colorRampPalette(c("#3498db", "white", "#e74c3c"))(100),
               show_colnames = FALSE, fontsize_row = max(6, 12 - nrow(expr)/10),
               main = paste0("Top ", nrow(expr), " DE Genes (of ", input$top_genes, " requested)"), border_color = NA)
      dev.off()
    }
  )

  output$top_degs_table <- renderDT({
    req(rv$sig_genes)
    
    top <- head(rv$sig_genes[order(rv$sig_genes$adj.P.Val), 
                              c("Gene", "logFC", "adj.P.Val", "Significance")], 30)
    
    datatable(top, options = list(pageLength = 15, dom = 't'), rownames = FALSE) %>%
      formatRound(columns = c("logFC", "adj.P.Val"), digits = 4)
  })
  
  output$all_de_table <- renderDT({
    req(rv$de_results)
    
    datatable(rv$de_results, options = list(pageLength = 25), rownames = FALSE, filter = 'top') %>%
      formatRound(columns = c("logFC", "AveExpr", "P.Value", "adj.P.Val"), digits = 4)
  })
  
  # Track if table is visible
  table_visible <- reactiveVal(FALSE)
  
  # Initialize: hide the table box when DE results are available
  observe({
    if (!is.null(rv$de_results) && !table_visible()) {
      shinyjs::hide("all_results_box")
    }
  })
  
  # Toggle all results table visibility
  observeEvent(input$toggle_all_results, {
    current_state <- table_visible()
    
    if (!current_state) {
      # Show the table
      shinyjs::show("all_results_box")
      table_visible(TRUE)
      
      # Update button text
      updateActionButton(session, "toggle_all_results",
                        label = tagList(icon("eye-slash"), " Hide All Results Table"),
                        icon = icon("eye-slash"))
    } else {
      # Hide the table
      shinyjs::hide("all_results_box")
      table_visible(FALSE)
      
      # Update button text
      updateActionButton(session, "toggle_all_results",
                        label = tagList(icon("table"), " Show All Results Table"),
                        icon = icon("table"))
    }
  })
  
  # ==============================================================================
  # DOWNLOADS (DE step: DE results only; batch expression downloads are in server_batch.R)
  # ==============================================================================

  output$download_de_results <- downloadHandler(
    filename = function() paste0("DE_Results_", Sys.Date(), ".csv"),
    content = function(file) {
      fn <- paste0("DE_Results_", Sys.Date(), ".csv")
      write.csv(rv$de_results, file, row.names = FALSE)
      write.csv(rv$de_results, file.path(CSV_EXPORT_DIR(), fn), row.names = FALSE)
    }
  )

  # Same content as download_de_results, for the second "Download Results" box
  output$download_de_results_alt <- downloadHandler(
    filename = function() paste0("DE_Results_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$de_results)
      fn <- paste0("DE_Results_", Sys.Date(), ".csv")
      write.csv(rv$de_results, file, row.names = FALSE)
      write.csv(rv$de_results, file.path(CSV_EXPORT_DIR(), fn), row.names = FALSE)
    }
  )
  
  output$download_sig_genes <- downloadHandler(
    filename = function() paste0("Significant_Genes_", Sys.Date(), ".csv"),
    content = function(file) {
      fn <- paste0("Significant_Genes_", Sys.Date(), ".csv")
      write.csv(rv$sig_genes, file, row.names = FALSE)
      write.csv(rv$sig_genes, file.path(CSV_EXPORT_DIR(), fn), row.names = FALSE)
    }
  )

  # Parallel DE: RNA-seq and microarray results are separate tables, so give
  # each platform its own download instead of only the platform currently
  # toggled into rv$de_results/rv$sig_genes.
  output$download_de_results_rna <- downloadHandler(
    filename = function() paste0("DE_Results_RNAseq_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$de_results_rna)
      fn <- paste0("DE_Results_RNAseq_", Sys.Date(), ".csv")
      write.csv(rv$de_results_rna, file, row.names = FALSE)
      write.csv(rv$de_results_rna, file.path(CSV_EXPORT_DIR(), fn), row.names = FALSE)
    }
  )
  output$download_de_results_micro <- downloadHandler(
    filename = function() paste0("DE_Results_Microarray_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$de_results_micro)
      fn <- paste0("DE_Results_Microarray_", Sys.Date(), ".csv")
      write.csv(rv$de_results_micro, file, row.names = FALSE)
      write.csv(rv$de_results_micro, file.path(CSV_EXPORT_DIR(), fn), row.names = FALSE)
    }
  )
  output$download_sig_genes_rna <- downloadHandler(
    filename = function() paste0("Significant_Genes_RNAseq_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$sig_genes_rna)
      fn <- paste0("Significant_Genes_RNAseq_", Sys.Date(), ".csv")
      write.csv(rv$sig_genes_rna, file, row.names = FALSE)
      write.csv(rv$sig_genes_rna, file.path(CSV_EXPORT_DIR(), fn), row.names = FALSE)
    }
  )
  output$download_sig_genes_micro <- downloadHandler(
    filename = function() paste0("Significant_Genes_Microarray_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$sig_genes_micro)
      fn <- paste0("Significant_Genes_Microarray_", Sys.Date(), ".csv")
      write.csv(rv$sig_genes_micro, file, row.names = FALSE)
      write.csv(rv$sig_genes_micro, file.path(CSV_EXPORT_DIR(), fn), row.names = FALSE)
    }
  )

  output$download_analysis_report <- downloadHandler(
    filename = function() paste0("GExPipe_Analysis_Report_", Sys.Date(), ".txt"),
    content = function(file) {
      params <- list(
        analysis_type = if (!is.null(input$analysis_type)) input$analysis_type else "n/a",
        de_method = if (!is.null(rv$de_method)) rv$de_method else "n/a",
        batch_method = if (!is.null(input$batch_method)) input$batch_method else "n/a",
        logfc_cutoff = if (!is.null(input$logfc_cutoff)) input$logfc_cutoff else "n/a",
        padj_cutoff = if (!is.null(input$padj_cutoff)) input$padj_cutoff else "n/a",
        logfc_cutoff_rna = if (!is.null(rv$de_logfc_rna)) rv$de_logfc_rna else "n/a",
        padj_cutoff_rna = if (!is.null(rv$de_padj_rna)) rv$de_padj_rna else "n/a",
        logfc_cutoff_micro = if (!is.null(rv$de_logfc_micro)) rv$de_logfc_micro else "n/a",
        padj_cutoff_micro = if (!is.null(rv$de_padj_micro)) rv$de_padj_micro else "n/a",
        variance_percentile = if (!is.null(input$variance_percentile)) input$variance_percentile else "n/a",
        global_quantile = if (!is.null(input$apply_global_quantile)) input$apply_global_quantile else "n/a",
        de_design_formula = if (!is.null(rv$de_design_formula)) rv$de_design_formula else "n/a",
        de_gene_filter = if (!is.null(rv$de_gene_filter_note)) rv$de_gene_filter_note else "n/a",
        n_genes_de = if (!is.null(rv$de_results)) nrow(rv$de_results) else 0L,
        n_sig_genes = if (!is.null(rv$sig_genes)) nrow(rv$sig_genes) else 0L,
        n_samples = if (!is.null(rv$unified_metadata)) nrow(rv$unified_metadata) else 0L
      )
      if (!is.null(rv$unified_metadata)) {
        sm <- gexpipe_batch_confounding_summary(rv$unified_metadata)
        params$batch_confounding <- sm$message
      }
      if (!is.null(rv$de_sample_info)) {
        params$de_samples <- rv$de_sample_info$note
      }
      lines <- gexpipe_analysis_report_text(params, include_session = TRUE)
      writeLines(lines, file)
    }
  )
  
  # download_workspace is defined in server.R (single handler for sidebar + results tab)
}


