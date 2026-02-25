# ==============================================================================
# SERVER_NORMALIZE.R - Step 3: Normalization Module
# ==============================================================================

server_normalize <- function(input, output, session, rv) {

  output$normalization_timer <- renderText({
    if (!isTRUE(rv$normalize_running) || is.null(rv$normalize_start)) return("00:00")
    invalidateLater(1000, session)
    elapsed <- as.integer(difftime(Sys.time(), rv$normalize_start, units = "secs"))
    sprintf("%02d:%02d", elapsed %/% 60, elapsed %% 60)
  })
  
  observeEvent(input$apply_normalization, {
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
    
    rv$normalize_start <- Sys.time()
    rv$normalize_running <- TRUE
    
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
      
      log_text <- "Normalizing data...\n\n"
      
      all_expr_norm <- list()
      normalization_stats <- list()
      
      # Normalization method choices (defaults if not set)
      micro_norm_method <- if (!is.null(input$micro_norm_method)) input$micro_norm_method else "quantile"
      rnaseq_norm_method <- if (!is.null(input$rnaseq_norm_method)) input$rnaseq_norm_method else "TMM"
      
      # Normalize microarray
      if (length(rv$micro_expr_list) > 0) {
        log_text <- paste0(log_text, "Microarray normalization (method: ", micro_norm_method, "):\n")
        for (gse in names(rv$micro_expr_list)) {
          use_rma <- (micro_norm_method == "rma") && !is.null(rv$micro_cel_paths) && length(rv$micro_cel_paths[[gse]]) > 0
          if (use_rma) {
            plat <- if (!is.null(rv$platform_per_gse)) rv$platform_per_gse[[gse]] else NULL
            probe_mat <- normalize_microarray_rma(rv$micro_cel_paths[[gse]], plat, dataset_name = gse)
            if (!is.null(probe_mat) && nrow(probe_mat) > 0 && ncol(probe_mat) > 0) {
              micro_eset <- rv$micro_eset_list[[gse]]
              fdata <- if (!is.null(micro_eset)) Biobase::fData(micro_eset) else data.frame()
              gene_symbols <- suppressMessages(map_microarray_ids(probe_mat, fdata, micro_eset, gse_id = gse))
              rownames(probe_mat) <- gene_symbols
              valid <- !is.na(gene_symbols) & trimws(gene_symbols) != ""
              expr_norm <- probe_mat[valid, , drop = FALSE]
              if (nrow(expr_norm) > 0 && any(duplicated(rownames(expr_norm)))) {
                expr_norm <- limma::avereps(expr_norm, ID = rownames(expr_norm))
              }
              norm_info <- attr(probe_mat, "normalization_info")
              if (is.null(norm_info)) norm_info <- list(initial_genes = nrow(probe_mat), final_genes = nrow(expr_norm), method = "RMA")
              attr(expr_norm, "normalization_info") <- norm_info
              all_expr_norm[[gse]] <- expr_norm
              normalization_stats[[gse]] <- norm_info
              log_text <- paste0(log_text, "  ", gse, ": RMA → ", format(nrow(expr_norm), big.mark = ","), " genes ✓\n")
            } else {
              expr_norm <- normalize_microarray(rv$micro_expr_list[[gse]], dataset_name = gse, method = "quantile")
              norm_info <- attr(expr_norm, "normalization_info")
              all_expr_norm[[gse]] <- expr_norm
              normalization_stats[[gse]] <- norm_info
              log_text <- paste0(log_text, "  ", gse, ": RMA not available, used Quantile. ", format(norm_info$final_genes, big.mark = ","), " genes ✓\n")
            }
          } else {
            expr_norm <- normalize_microarray(rv$micro_expr_list[[gse]], dataset_name = gse, method = "quantile")
            norm_info <- attr(expr_norm, "normalization_info")
            all_expr_norm[[gse]] <- expr_norm
            normalization_stats[[gse]] <- norm_info
            log_text <- paste0(log_text, "  ", gse, ": ", format(norm_info$initial_genes, big.mark = ","), " → ",
                              format(norm_info$final_genes, big.mark = ","), " genes ✓\n")
          }
        }
      }
      
      incProgress(0.5)
      
      # Normalize RNA-seq
      if (length(rv$rna_counts_list) > 0) {
        log_text <- paste0(log_text, "\nRNA-seq normalization (method: ", rnaseq_norm_method, "):\n")
        for (gse in names(rv$rna_counts_list)) {
          expr_norm <- normalize_rnaseq(rv$rna_counts_list[[gse]], dataset_name = gse, method = rnaseq_norm_method)
          norm_info <- attr(expr_norm, "normalization_info")
          
          all_expr_norm[[gse]] <- expr_norm
          normalization_stats[[gse]] <- norm_info
          
          log_text <- paste0(log_text, "  ", gse, ": ", 
                            format(norm_info$initial_genes, big.mark = ","), " → ",
                            format(norm_info$genes_after_filtering, big.mark = ","),
                            " (removed ", format(norm_info$genes_removed, big.mark = ","), " low-expression) ✓\n")
        }
      }
      
      # Calculate statistics before filtering
      gene_lists <- lapply(all_expr_norm, rownames)
      initial_total <- sum(sapply(normalization_stats, function(info) {
        if (!is.null(info)) info$initial_genes else 0
      }), na.rm = TRUE)
      
      after_filter_total <- sum(sapply(normalization_stats, function(info) {
        if (!is.null(info)) {
          if (!is.null(info$genes_after_filtering)) {
            info$genes_after_filtering
          } else {
            info$final_genes
          }
        } else 0
      }), na.rm = TRUE)
      
      rnaseq_removed <- sum(sapply(normalization_stats, function(info) {
        if (!is.null(info) && !is.null(info$genes_removed)) {
          info$genes_removed
        } else 0
      }), na.rm = TRUE)
      
      # Automatically filter to common genes (intersection) - required for downstream analysis
      log_text <- paste0(log_text, "\nAutomatic gene filtering (background process):\n")
      log_text <- paste0(log_text, "  Filtering to common genes ensures consistent gene sets across datasets.\n")
      log_text <- paste0(log_text, "  This is required for accurate batch correction and differential expression analysis.\n")
      
      # Filter to common genes (intersection) - always done automatically
      rv$common_genes <- Reduce(intersect, gene_lists)
      for (i in seq_along(all_expr_norm)) {
        all_expr_norm[[i]] <- all_expr_norm[[i]][rv$common_genes, ]
      }
      
      filter_status <- "Filtered to common genes (intersection)"
      filter_note <- paste0("  Common genes retained: ", format(length(rv$common_genes), big.mark = ","), "\n")
      final_count <- length(rv$common_genes)
      
      log_text <- paste0(log_text, "  ✓ Common genes identified: ", format(length(rv$common_genes), big.mark = ","), "\n")
      
      # Store statistics for reporting
      rv$normalization_stats <- list(
        initial_total = initial_total,
        after_filter_total = after_filter_total,
        rnaseq_removed = rnaseq_removed,
        final_count = final_count,
        filter_method = "intersection"
      )
      
      # Store pre-combined normalized data for visualization
      rv$all_expr_norm_list <- all_expr_norm
      
      # Combine individual normalized datasets (before global normalization)
      combined_before_global <- do.call(cbind, all_expr_norm)
      
      # Store before global normalization for comparison plot
      rv$combined_expr_before_global_norm <- combined_before_global
      
      # ====================================================================
      # SAVE RAW COUNTS FOR DESeq2 (before global quantile normalization)
      # ====================================================================
      # Count-based methods (DESeq2 / edgeR / limma-voom) require integer counts (raw, un-normalized).
      # For RNA-seq: save the original counts (pre-TMM) filtered to common genes.
      # For microarray: these methods are not appropriate (continuous data); raw not saved.
      if (length(rv$rna_counts_list) > 0 && isTRUE(rv$de_method %in% c("deseq2", "edger", "limma_voom"))) {
        raw_counts_list <- list()
        for (gse in names(rv$rna_counts_list)) {
          raw_mat <- as.matrix(rv$rna_counts_list[[gse]])
          # Keep only common genes
          common_in_raw <- intersect(rv$common_genes, rownames(raw_mat))
          if (length(common_in_raw) > 0) {
            raw_counts_list[[gse]] <- raw_mat[common_in_raw, , drop = FALSE]
          }
        }
        if (length(raw_counts_list) > 0) {
          rv$raw_counts_for_deseq2 <- do.call(cbind, raw_counts_list)
          # Ensure integer counts (round any fractional values from gene mapping)
          rv$raw_counts_for_deseq2 <- round(rv$raw_counts_for_deseq2)
          storage.mode(rv$raw_counts_for_deseq2) <- "integer"
          log_text <- paste0(log_text, "  ✓ Raw counts saved for DESeq2: ",
                            format(nrow(rv$raw_counts_for_deseq2), big.mark = ","), " genes × ",
                            format(ncol(rv$raw_counts_for_deseq2), big.mark = ","), " samples\n")
        }
      }
      
      # Apply global quantile normalization
      rv$combined_expr <- normalizeBetweenArrays(combined_before_global, method = "quantile")
      
      # Create metadata
      micro_n <- if (length(rv$micro_expr_list) > 0) sum(vapply(rv$micro_expr_list, ncol, integer(1))) else 0L
      rna_n <- if (length(rv$rna_counts_list) > 0) sum(vapply(rv$rna_counts_list, ncol, integer(1))) else 0L
      platform_labels <- c(rep("Microarray", micro_n), rep("RNAseq", rna_n))
      dataset_labels <- rep(names(all_expr_norm), times = vapply(all_expr_norm, ncol, integer(1)))
      
      rv$unified_metadata <- data.frame(
        SampleID = colnames(rv$combined_expr),
        Platform = platform_labels,
        Dataset = dataset_labels,
        Condition = NA_character_,
        row.names = colnames(rv$combined_expr)
      )
      
      total_genes <- nrow(rv$combined_expr)
      total_samples <- ncol(rv$combined_expr)
      
      # Store metadata for DESeq2 raw counts (align samples if raw counts were saved)
      if (!is.null(rv$raw_counts_for_deseq2)) {
        raw_samples <- colnames(rv$raw_counts_for_deseq2)
        meta_samples <- rv$unified_metadata$SampleID
        common_samples <- intersect(raw_samples, meta_samples)
        if (length(common_samples) > 0) {
          rv$raw_counts_for_deseq2 <- rv$raw_counts_for_deseq2[, common_samples, drop = FALSE]
          rv$raw_counts_metadata <- rv$unified_metadata[rv$unified_metadata$SampleID %in% common_samples, , drop = FALSE]
        }
      }
      
      # Create detailed summary table
      summary_data <- data.frame(
        Dataset = names(normalization_stats),
        Initial_Genes = sapply(normalization_stats, function(x) {
          if (!is.null(x)) x$initial_genes else 0
        }),
        After_Filtering = sapply(normalization_stats, function(x) {
          if (!is.null(x)) {
            if (!is.null(x$genes_after_filtering)) {
              x$genes_after_filtering
            } else {
              x$final_genes
            }
          } else 0
        }),
        Final_Common_Genes = final_count,
        stringsAsFactors = FALSE
      )
      
      # Add totals row
      summary_data <- rbind(summary_data,
                            data.frame(
                              Dataset = "TOTAL/COMMON",
                              Initial_Genes = sum(summary_data$Initial_Genes),
                              After_Filtering = sum(summary_data$After_Filtering),
                              Final_Common_Genes = final_count,
                              stringsAsFactors = FALSE
                            ))
      
      rv$normalization_summary_table <- summary_data
      
      # Generate detailed summary
      log_text <- paste0(log_text, "\n✓ Normalization Complete!\n",
                         "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
                         "Gene Statistics Summary:\n",
                         "  Initial total genes:     ", format(initial_total, big.mark = ","), "\n")
      
      if (rnaseq_removed > 0) {
        log_text <- paste0(log_text,
                           "  Removed (low expression): ", format(rnaseq_removed, big.mark = ","), "\n",
                           "  After filtering:         ", format(after_filter_total, big.mark = ","), "\n")
      }
      
      log_text <- paste0(log_text,
                         "  Gene Filtering:           ", filter_status, "\n",
                         filter_note,
                         "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
                         "Final Dataset:\n",
                         "  Genes:   ", format(total_genes, big.mark = ","), "\n",
                         "  Samples: ", format(total_samples, big.mark = ","), "\n",
                         "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
                         "\nNote: Gene count may be reduced after group selection\n",
                         "      and variance filtering in batch correction.\n")
      
      # Generate report caption
      rv$normalization_caption <- paste0(
        "Gene Expression Normalization Pipeline: Starting with ", 
        format(initial_total, big.mark = ","), " total genes across ", 
        length(all_expr_norm), " dataset(s)",
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
      
      rv$normalization_complete <- TRUE
      rv$normalize_running <- FALSE
      
      output$normalization_log <- renderText({ log_text })
      
      # Re-enable button
      shinyjs::enable("apply_normalization")
      shinyjs::html("apply_normalization", 
                    HTML('<i class="fa fa-check-circle"></i> Apply Normalization'))
      
      # Remove processing notification
      removeNotification("normalize_processing")
      
      # Show notification with gene count
      showNotification(
        tags$div(
          tags$strong("✓ Normalization complete!"),
          tags$br(),
          tags$span("Genes: ", format(total_genes, big.mark = ","), 
                    " | Samples: ", format(total_samples, big.mark = ",")),
          style = "font-size: 13px;"
        ),
        type = "message", duration = 6
      )
    })

    rv$normalize_running <- FALSE
  })
  
  # ==========================================================================
  # AUTO-NORMALIZE FOR COUNT-BASED METHODS (silent, no UI interaction)
  # ==========================================================================
  # When DESeq2 is selected + download finishes, auto-run normalization so
  # downstream steps (WGCNA, heatmaps, etc.) have normalized data — the user
  # stays on the current tab and just sees a brief notification.
  observeEvent(rv$download_complete, {
    if (!isTRUE(rv$download_complete)) return()
    if (is.null(input$de_method) || !(input$de_method %in% c("deseq2", "edger", "limma_voom"))) return()
    if (isTRUE(rv$normalization_complete)) return()
    
    # Run the same normalization logic silently
    tryCatch({
      rv$normalize_running <- TRUE
      
      all_expr_norm <- list()
      
      # Normalize microarray
      if (length(rv$micro_expr_list) > 0) {
        for (gse in names(rv$micro_expr_list)) {
          all_expr_norm[[gse]] <- normalize_microarray(rv$micro_expr_list[[gse]], dataset_name = gse)
        }
      }
      
      # Normalize RNA-seq
      if (length(rv$rna_counts_list) > 0) {
        for (gse in names(rv$rna_counts_list)) {
          all_expr_norm[[gse]] <- normalize_rnaseq(rv$rna_counts_list[[gse]], dataset_name = gse)
        }
      }
      
      # Filter to common genes
      gene_lists <- lapply(all_expr_norm, rownames)
      rv$common_genes <- Reduce(intersect, gene_lists)
      for (i in seq_along(all_expr_norm)) {
        all_expr_norm[[i]] <- all_expr_norm[[i]][rv$common_genes, ]
      }
      
      # Save raw counts for DESeq2 before global normalization
      if (length(rv$rna_counts_list) > 0) {
        raw_counts_list <- list()
        for (gse in names(rv$rna_counts_list)) {
          raw_mat <- as.matrix(rv$rna_counts_list[[gse]])
          common_in_raw <- intersect(rv$common_genes, rownames(raw_mat))
          if (length(common_in_raw) > 0) {
            raw_counts_list[[gse]] <- raw_mat[common_in_raw, , drop = FALSE]
          }
        }
        if (length(raw_counts_list) > 0) {
          rv$raw_counts_for_deseq2 <- round(do.call(cbind, raw_counts_list))
          storage.mode(rv$raw_counts_for_deseq2) <- "integer"
        }
      }
      
      # Combine and apply global quantile normalization
      combined_before_global <- do.call(cbind, all_expr_norm)
      rv$combined_expr_before_global_norm <- combined_before_global
      rv$combined_expr <- normalizeBetweenArrays(combined_before_global, method = "quantile")
      rv$all_expr_norm_list <- all_expr_norm
      
      # Create metadata
      micro_n <- if (length(rv$micro_expr_list) > 0) sum(vapply(rv$micro_expr_list, ncol, integer(1))) else 0L
      rna_n   <- if (length(rv$rna_counts_list) > 0) sum(vapply(rv$rna_counts_list, ncol, integer(1))) else 0L
      platform_labels <- c(rep("Microarray", micro_n), rep("RNAseq", rna_n))
      dataset_labels  <- rep(names(all_expr_norm), times = vapply(all_expr_norm, ncol, integer(1)))
      
      rv$unified_metadata <- data.frame(
        SampleID  = colnames(rv$combined_expr),
        Platform  = platform_labels,
        Dataset   = dataset_labels,
        Condition = NA_character_,
        row.names = colnames(rv$combined_expr)
      )
      
      # Align raw counts metadata
      if (!is.null(rv$raw_counts_for_deseq2)) {
        common_samp <- intersect(colnames(rv$raw_counts_for_deseq2), rv$unified_metadata$SampleID)
        if (length(common_samp) > 0) {
          rv$raw_counts_for_deseq2 <- rv$raw_counts_for_deseq2[, common_samp, drop = FALSE]
          rv$raw_counts_metadata   <- rv$unified_metadata[rv$unified_metadata$SampleID %in% common_samp, , drop = FALSE]
        }
      }
      
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
  
  # Normalization quality visualization: Box plots showing distribution before/after
  output$normalization_plot <- renderPlot({
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
        set.seed(123)
        sample_genes <- sample(1:n_genes, 10000)
        expr_before <- expr_before[sample_genes, ]
        expr_after <- expr_after[sample_genes, ]
      }
      
      # Prepare data for plotting
      n_samples <- min(50, ncol(expr_before))  # Limit to 50 samples for readability
      if (ncol(expr_before) > n_samples) {
        set.seed(123)
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
      
      print(p)
      
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
        
        print(p)
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
        set.seed(123)
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
      
      print(p)
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
        set.seed(123)
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
      
      print(p)
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
        set.seed(123)
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
      
      print(p)
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
        set.seed(123)
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
      
      print(p)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
    })
  })
  
  # Plot 6: Median & Range Alignment - Boxplot
  output$normalization_median_range <- renderPlot({
    req(rv$normalization_complete, rv$combined_expr)
    
    expr_after <- rv$combined_expr
    
    tryCatch({
      # Limit samples for readability
      n_samples <- min(50, ncol(expr_after))
      if (ncol(expr_after) > n_samples) {
        set.seed(123)
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
      
      p <- ggplot(plot_data, aes(x = Sample, y = Expression, fill = Dataset)) +
        geom_boxplot(alpha = 0.7, outlier.size = 0.5, outlier.alpha = 0.3) +
        theme_bw(base_size = 11) +
        labs(
          title = "Median & Range Alignment",
          subtitle = "Aligned medians and ranges indicate successful normalization",
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
      
      print(p)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
    })
  })
  
  # Plot 7: Distribution Overlap - Density Plot
  output$normalization_distribution_overlap <- renderPlot({
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
        set.seed(123)
        sample_genes <- sample(seq_len(nrow(expr_before)), n_genes)
        expr_before <- expr_before[sample_genes, ]
        expr_after <- expr_after[sample_genes, ]
      }
      
      # Limit samples
      n_samples <- min(20, ncol(expr_before))
      if (ncol(expr_before) > n_samples) {
        set.seed(123)
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
      
      p <- ggplot(plot_data, aes(x = x, y = y, color = Stage, group = interaction(Sample, Stage))) +
        geom_line(alpha = 0.6, linewidth = 0.7) +
        scale_color_manual(values = c("Before" = "#e74c3c", "After" = "#2ecc71")) +
        facet_wrap(~ Dataset, scales = "free", ncol = min(2, length(unique(plot_data$Dataset)))) +
        theme_bw(base_size = 12) +
        labs(
          title = "Distribution Overlap",
          subtitle = "Overlapping distributions indicate successful normalization",
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
      
      print(p)
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
        set.seed(123)
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
      
      print(p)
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
        set.seed(123)
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
      
      print(p)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
    })
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
    actionButton("go_to_groups", "Next: Select Groups", 
                 icon = icon("arrow-right"), class = "btn-success btn-lg")
  })
  
}


