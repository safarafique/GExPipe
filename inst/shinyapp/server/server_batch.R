# ==============================================================================
# SERVER_BATCH.R - Step 5: Batch Correction Module
# ==============================================================================

server_batch <- function(input, output, session, rv) {

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
    rv$batch_complete <- TRUE
    rv$batch_running <- FALSE

    output$batch_log <- renderText({
      paste0(
        "✓ Batch correction skipped (single dataset)\n\n",
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

  # Expression BEFORE batch correction (genes x samples) — in batch step
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

  # Expression AFTER batch correction (genes x samples) — in batch step
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
    gene_vars <- apply(rv$combined_expr, 1, var)
    percentile <- input$variance_percentile / 100
    quantile(gene_vars, percentile)
  })
  
  # Calculate genes to keep and remove based on current percentile
  output$genes_to_keep <- renderText({
    req(rv$combined_expr, input$variance_percentile)
    gene_vars <- apply(rv$combined_expr, 1, var)
    cutoff <- variance_cutoff()
    n_keep <- sum(gene_vars > cutoff)
    format(n_keep, big.mark = ",")
  })
  
  output$genes_to_remove <- renderText({
    req(rv$combined_expr, input$variance_percentile)
    gene_vars <- apply(rv$combined_expr, 1, var)
    cutoff <- variance_cutoff()
    n_remove <- sum(gene_vars <= cutoff)
    format(n_remove, big.mark = ",")
  })
  
  output$filter_info <- renderText({
    req(rv$combined_expr, input$variance_percentile)
    total_genes <- nrow(rv$combined_expr)
    gene_vars <- apply(rv$combined_expr, 1, var)
    cutoff <- variance_cutoff()
    n_remove <- sum(gene_vars <= cutoff)
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
    gene_vars <- apply(rv$combined_expr, 1, var)
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
    p <- batch_gene_variance_plot()
    if (!is.null(p)) print(p)
  })

  output$download_gene_variance_png <- downloadHandler(
    filename = function() "Batch_Gene_Variance.png",
    content = function(file) {
      p <- batch_gene_variance_plot()
      if (!is.null(p)) ggplot2::ggsave(file, plot = p, width = 9, height = 5, dpi = IMAGE_DPI, units = "in", bg = "white", device = "png")
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
      rv$batch_complete <- TRUE
      rv$batch_running <- FALSE
      output$batch_log <- renderText({
        paste0(
          "✓ Batch correction skipped (single dataset)\n\n",
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
      
      # Filter genes using user-selected percentile
      gene_vars <- apply(rv$combined_expr, 1, var)
      percentile <- input$variance_percentile / 100
      cutoff <- quantile(gene_vars, percentile)
      high_var <- gene_vars > cutoff
      rv$expr_filtered <- rv$combined_expr[high_var, ]
      
      design <- model.matrix(~ Condition, data = rv$unified_metadata)
      
      incProgress(0.5)
      
      # Apply method
      if (input$batch_method == "limma") {
        rv$batch_corrected <- limma::removeBatchEffect(
          rv$expr_filtered,
          batch = rv$unified_metadata$Dataset,
          design = design
        )
      } else if (input$batch_method == "combat") {
        rv$batch_corrected <- sva::ComBat(
          rv$expr_filtered,
          batch = rv$unified_metadata$Dataset,
          mod = NULL, par.prior = TRUE, prior.plots = FALSE
        )
      } else if (input$batch_method == "quantile_limma") {
        expr_q <- limma::normalizeBetweenArrays(rv$expr_filtered, method = "quantile")
        rv$batch_corrected <- limma::removeBatchEffect(
          expr_q,
          batch = rv$unified_metadata$Dataset,
          design = design
        )
      } else if (input$batch_method == "hybrid") {
        expr_q <- limma::normalizeBetweenArrays(rv$expr_filtered, method = "quantile")
        rv$batch_corrected <- sva::ComBat(
          expr_q,
          batch = rv$unified_metadata$Dataset,
          mod = NULL, par.prior = TRUE, prior.plots = FALSE
        )
      } else if (input$batch_method == "combat_ref") {
        sizes <- table(rv$unified_metadata$Dataset)
        ref <- names(sizes)[which.max(sizes)]
        rv$batch_corrected <- sva::ComBat(
          rv$expr_filtered,
          batch = rv$unified_metadata$Dataset,
          mod = NULL, par.prior = TRUE, prior.plots = FALSE,
          ref.batch = ref
        )
      } else if (input$batch_method == "sva") {
        # Surrogate variable analysis: estimate hidden confounders, then ComBat with mod = design + SVs
        mod <- model.matrix(~ Condition, data = rv$unified_metadata)
        mod0 <- model.matrix(~ 1, data = rv$unified_metadata)
        n_sv <- tryCatch(sva::num.sv(rv$expr_filtered, mod, method = "be"), error = function(e) 0L)
        n_sv <- max(0L, min(n_sv, 10L))
        if (n_sv > 0) {
          svobj <- tryCatch(
            sva::sva(as.matrix(rv$expr_filtered), mod, mod0, n.sv = n_sv),
            error = function(e) NULL
          )
          if (!is.null(svobj) && ncol(svobj$sv) > 0) {
            mod_sv <- cbind(mod, svobj$sv)
            rv$batch_corrected <- sva::ComBat(
              rv$expr_filtered,
              batch = rv$unified_metadata$Dataset,
              mod = mod_sv, par.prior = TRUE, prior.plots = FALSE
            )
          } else {
            rv$batch_corrected <- sva::ComBat(
              rv$expr_filtered,
              batch = rv$unified_metadata$Dataset,
              mod = mod, par.prior = TRUE, prior.plots = FALSE
            )
          }
        } else {
          rv$batch_corrected <- sva::ComBat(
            rv$expr_filtered,
            batch = rv$unified_metadata$Dataset,
            mod = mod, par.prior = TRUE, prior.plots = FALSE
          )
        }
      }
      
      # Calculate gene reduction
      genes_before <- nrow(rv$combined_expr)
      genes_after <- nrow(rv$batch_corrected)
      genes_filtered <- genes_before - genes_after
      filter_percent <- round(100 * genes_filtered / genes_before, 1)
      
      rv$batch_complete <- TRUE
      rv$batch_running <- FALSE
      
      output$batch_log <- renderText({
        paste0("✓ Batch correction complete\n",
               "Method: ", input$batch_method, "\n\n",
               "Gene Filtering:\n",
               "  Before filter: ", format(genes_before, big.mark = ","), " genes\n",
               "  After filter:  ", format(genes_after, big.mark = ","), " genes\n",
               "  Filtered out:  ", format(genes_filtered, big.mark = ","), " genes (", filter_percent, "%)\n\n",
               "Final Dataset:\n",
               "  Genes: ", format(genes_after, big.mark = ","), "\n",
               "  Samples: ", format(ncol(rv$batch_corrected), big.mark = ","))
      })
      
      # Re-enable button
      shinyjs::enable("apply_batch")
      shinyjs::html("apply_batch", 
                    HTML('<i class="fa fa-magic"></i> Apply Batch Correction'))
      
      # Remove processing notification
      removeNotification("batch_processing")
      
      # Show notification with gene count change
      showNotification(
        tags$div(
          tags$strong("✓ Batch correction complete!"),
          tags$br(),
          tags$span("Genes filtered: ", format(genes_before, big.mark = ","), 
                    " → ", format(genes_after, big.mark = ","), 
                    " (", filter_percent, "% removed)"),
          tags$br(),
          tags$span("Final: ", format(genes_after, big.mark = ","), " genes, ", 
                    format(ncol(rv$batch_corrected), big.mark = ","), " samples"),
          style = "font-size: 13px;"
        ),
        type = "message", duration = 8
      )
    })

    rv$batch_running <- FALSE
  })
  
  # PCA Before Batch Correction - Colored by Dataset (circular / polar)
  output$pca_before_dataset <- renderPlot({
    req(rv$expr_filtered)

    pca <- prcomp(t(rv$expr_filtered), scale. = TRUE)
    var_exp <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
    pc1 <- pca$x[, 1]
    pc2 <- pca$x[, 2]
    theta <- atan2(pc2, pc1)
    r <- sqrt(pc1^2 + pc2^2)
    if (max(r) > 0) r <- r / max(r)
    df <- data.frame(theta = theta, r = r, Dataset = rv$unified_metadata$Dataset)

    ggplot(df, aes(x = theta, y = r, color = Dataset)) +
      geom_point(size = 3.5, alpha = 0.7) +
      coord_polar(theta = "x", start = -pi / 2, direction = 1) +
      scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi),
                        labels = c("-180°", "-90°", "0°", "90°", "180°")) +
      scale_y_continuous(limits = c(0, NA)) +
      theme_bw(base_size = 14) +
      labs(title = "Before Batch Correction - By Dataset",
           subtitle = "Batch effects visible as dataset separation (circular)",
           x = "Angle (PC1–PC2)", y = "Radius", color = "Dataset") +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
        legend.position = "right",
        panel.grid.minor = element_blank()
      )
  })
  
  # PCA Before Batch Correction - Colored by Condition (circular / polar)
  output$pca_before_condition <- renderPlot({
    req(rv$expr_filtered)

    pca <- prcomp(t(rv$expr_filtered), scale. = TRUE)
    var_exp <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
    pc1 <- pca$x[, 1]
    pc2 <- pca$x[, 2]
    theta <- atan2(pc2, pc1)
    r <- sqrt(pc1^2 + pc2^2)
    if (max(r) > 0) r <- r / max(r)
    df <- data.frame(theta = theta, r = r,
                     Condition = rv$unified_metadata$Condition,
                     Dataset = rv$unified_metadata$Dataset)

    if (all(is.na(df$Condition)) || length(unique(df$Condition[!is.na(df$Condition)])) < 2) {
      ggplot(df, aes(x = theta, y = r)) +
        geom_point(size = 3.5, alpha = 0.7, color = "gray60") +
        coord_polar(theta = "x", start = -pi / 2, direction = 1) +
        scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi),
                          labels = c("-180°", "-90°", "0°", "90°", "180°")) +
        scale_y_continuous(limits = c(0, NA)) +
        theme_bw(base_size = 14) +
        labs(title = "Before Batch Correction - By Condition",
             subtitle = "Conditions not yet assigned (circular)",
             x = "Angle (PC1–PC2)", y = "Radius") +
        theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
              plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
              panel.grid.minor = element_blank())
    } else {
      ggplot(df, aes(x = theta, y = r, color = Condition)) +
        geom_point(size = 3.5, alpha = 0.7) +
        scale_color_manual(values = c("Normal" = "#3498db", "Disease" = "#e74c3c", "None" = "#95a5a6"),
                          na.value = "gray60") +
        coord_polar(theta = "x", start = -pi / 2, direction = 1) +
        scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi),
                          labels = c("-180°", "-90°", "0°", "90°", "180°")) +
        scale_y_continuous(limits = c(0, NA)) +
        theme_bw(base_size = 14) +
        labs(title = "Before Batch Correction - By Condition",
             subtitle = "Biological signal may be obscured by batch effects (circular)",
             x = "Angle (PC1–PC2)", y = "Radius", color = "Condition") +
        theme(
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
          legend.position = "right",
          panel.grid.minor = element_blank()
        )
    }
  })
  
  # PCA After Batch Correction - Colored by Dataset (circular / polar)
  output$pca_after_dataset <- renderPlot({
    req(rv$batch_corrected)

    pca <- prcomp(t(rv$batch_corrected), scale. = TRUE)
    var_exp <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
    pc1 <- pca$x[, 1]
    pc2 <- pca$x[, 2]
    theta <- atan2(pc2, pc1)
    r <- sqrt(pc1^2 + pc2^2)
    if (max(r) > 0) r <- r / max(r)
    df <- data.frame(theta = theta, r = r, Dataset = rv$unified_metadata$Dataset)

    ggplot(df, aes(x = theta, y = r, color = Dataset)) +
      geom_point(size = 3.5, alpha = 0.7) +
      coord_polar(theta = "x", start = -pi / 2, direction = 1) +
      scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi),
                        labels = c("-180°", "-90°", "0°", "90°", "180°")) +
      scale_y_continuous(limits = c(0, NA)) +
      theme_bw(base_size = 14) +
      labs(title = "After Batch Correction - By Dataset",
           subtitle = "Datasets should be intermingled (batch effects removed) (circular)",
           x = "Angle (PC1–PC2)", y = "Radius", color = "Dataset") +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
        legend.position = "right",
        panel.grid.minor = element_blank()
      )
  })
  
  # PCA After Batch Correction - Colored by Condition (circular / polar)
  output$pca_after_condition <- renderPlot({
    req(rv$batch_corrected)

    pca <- prcomp(t(rv$batch_corrected), scale. = TRUE)
    var_exp <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
    pc1 <- pca$x[, 1]
    pc2 <- pca$x[, 2]
    theta <- atan2(pc2, pc1)
    r <- sqrt(pc1^2 + pc2^2)
    if (max(r) > 0) r <- r / max(r)
    df <- data.frame(theta = theta, r = r, Condition = rv$unified_metadata$Condition)

    if (all(is.na(df$Condition)) || length(unique(df$Condition[!is.na(df$Condition)])) < 2) {
      ggplot(df, aes(x = theta, y = r)) +
        geom_point(size = 3.5, alpha = 0.7, color = "gray60") +
        coord_polar(theta = "x", start = -pi / 2, direction = 1) +
        scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi),
                          labels = c("-180°", "-90°", "0°", "90°", "180°")) +
        scale_y_continuous(limits = c(0, NA)) +
        theme_bw(base_size = 14) +
        labs(title = "After Batch Correction - By Condition",
             subtitle = "Conditions not yet assigned (circular)",
             x = "Angle (PC1–PC2)", y = "Radius") +
        theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
              plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
              panel.grid.minor = element_blank())
    } else {
      ggplot(df, aes(x = theta, y = r, color = Condition)) +
        geom_point(size = 3.5, alpha = 0.7) +
        scale_color_manual(values = c("Normal" = "#3498db", "Disease" = "#e74c3c", "None" = "#95a5a6"),
                          na.value = "gray60") +
        coord_polar(theta = "x", start = -pi / 2, direction = 1) +
        scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi),
                          labels = c("-180°", "-90°", "0°", "90°", "180°")) +
        scale_y_continuous(limits = c(0, NA)) +
        theme_bw(base_size = 14) +
        labs(title = "After Batch Correction - By Condition",
             subtitle = "Biological signal should be clearly visible (circular)",
             x = "Angle (PC1–PC2)", y = "Radius", color = "Condition") +
        theme(
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
          legend.position = "right",
          panel.grid.minor = element_blank()
        )
    }
  })

  # Hierarchical Clustering Heatmap - Before Batch Correction
  output$hclust_before <- renderPlot({
    req(rv$expr_filtered)
    
    tryCatch({
      # Limit samples for performance
      n_samples <- min(50, ncol(rv$expr_filtered))
      if (ncol(rv$expr_filtered) > n_samples) {
        set.seed(123)
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
      
      # Reorder correlation matrix by dendrogram
      cor_matrix_ordered <- cor_matrix[hclust_result$order, hclust_result$order]
      
      # Create heatmap
      pheatmap(
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
        set.seed(123)
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
      
      # Reorder correlation matrix by dendrogram
      cor_matrix_ordered <- cor_matrix[hclust_result$order, hclust_result$order]
      
      # Create heatmap
      pheatmap(
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
    req(rv$expr_filtered)
    
    tryCatch({
      # Limit samples for performance
      n_samples <- min(100, ncol(rv$expr_filtered))
      if (ncol(rv$expr_filtered) > n_samples) {
        set.seed(123)
        sample_idx <- sample(seq_len(ncol(rv$expr_filtered)), n_samples)
        expr_subset <- rv$expr_filtered[, sample_idx]
        metadata_subset <- rv$unified_metadata[sample_idx, ]
      } else {
        expr_subset <- rv$expr_filtered
        metadata_subset <- rv$unified_metadata
      }
      
      # Perform PCA
      pca <- prcomp(t(expr_subset), scale. = TRUE)
      
      # Use top PCs (explain >80% variance or top 10)
      cum_var <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
      n_pcs <- min(10, which(cum_var >= 0.8)[1])
      if (is.na(n_pcs)) n_pcs <- min(10, ncol(pca$x))
      
      pca_scores <- pca$x[, 1:n_pcs]
      
      # Calculate variance components
      pvca_results <- data.frame(
        Factor = character(),
        Variance = numeric(),
        stringsAsFactors = FALSE
      )
      
      # Dataset variance
      if (length(unique(metadata_subset$Dataset)) > 1) {
        dataset_var <- sum(apply(pca_scores, 2, function(pc) {
          aov_result <- aov(pc ~ metadata_subset$Dataset)
          sum_sq <- summary(aov_result)[[1]]$`Sum Sq`
          sum_sq[1] / sum(sum_sq)
        })) / n_pcs
        pvca_results <- rbind(pvca_results, 
                             data.frame(Factor = "Dataset", Variance = dataset_var))
      }
      
      # Condition variance
      if (!all(is.na(metadata_subset$Condition)) && 
          length(unique(metadata_subset$Condition[!is.na(metadata_subset$Condition)])) > 1) {
        condition_var <- sum(apply(pca_scores, 2, function(pc) {
          aov_result <- aov(pc ~ metadata_subset$Condition)
          sum_sq <- summary(aov_result)[[1]]$`Sum Sq`
          sum_sq[1] / sum(sum_sq)
        })) / n_pcs
        pvca_results <- rbind(pvca_results, 
                             data.frame(Factor = "Condition", Variance = condition_var))
      }
      
      # Residual variance
      total_var <- sum(pvca_results$Variance)
      residual_var <- max(0, 1 - total_var)
      pvca_results <- rbind(pvca_results, 
                           data.frame(Factor = "Residual", Variance = residual_var))
      
      # Create bar plot
      pvca_results$Factor <- factor(pvca_results$Factor, 
                                    levels = c("Dataset", "Condition", "Residual"))
      
      p <- ggplot(pvca_results, aes(x = Factor, y = Variance, fill = Factor)) +
        geom_bar(stat = "identity", alpha = 0.8) +
        scale_fill_manual(values = c("Dataset" = "#e74c3c", 
                                     "Condition" = "#3498db", 
                                     "Residual" = "#95a5a6")) +
        theme_bw(base_size = 14) +
        labs(
          title = "PVCA - Before Batch Correction",
          subtitle = "Variance explained by Dataset vs Condition",
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
      
      print(p)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error generating PVCA:", e$message), cex = 1.2)
    })
  })
  
  # PVCA (Principal Variance Component Analysis) - After
  output$pvca_after <- renderPlot({
    req(rv$batch_corrected)
    
    tryCatch({
      # Limit samples for performance
      n_samples <- min(100, ncol(rv$batch_corrected))
      if (ncol(rv$batch_corrected) > n_samples) {
        set.seed(123)
        sample_idx <- sample(seq_len(ncol(rv$batch_corrected)), n_samples)
        expr_subset <- rv$batch_corrected[, sample_idx]
        metadata_subset <- rv$unified_metadata[sample_idx, ]
      } else {
        expr_subset <- rv$batch_corrected
        metadata_subset <- rv$unified_metadata
      }
      
      # Perform PCA
      pca <- prcomp(t(expr_subset), scale. = TRUE)
      
      # Use top PCs (explain >80% variance or top 10)
      cum_var <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
      n_pcs <- min(10, which(cum_var >= 0.8)[1])
      if (is.na(n_pcs)) n_pcs <- min(10, ncol(pca$x))
      
      pca_scores <- pca$x[, 1:n_pcs]
      
      # Calculate variance components
      pvca_results <- data.frame(
        Factor = character(),
        Variance = numeric(),
        stringsAsFactors = FALSE
      )
      
      # Dataset variance (should be reduced after batch correction)
      if (length(unique(metadata_subset$Dataset)) > 1) {
        dataset_var <- sum(apply(pca_scores, 2, function(pc) {
          aov_result <- aov(pc ~ metadata_subset$Dataset)
          sum_sq <- summary(aov_result)[[1]]$`Sum Sq`
          sum_sq[1] / sum(sum_sq)
        })) / n_pcs
        pvca_results <- rbind(pvca_results, 
                             data.frame(Factor = "Dataset", Variance = dataset_var))
      }
      
      # Condition variance (should be increased after batch correction)
      if (!all(is.na(metadata_subset$Condition)) && 
          length(unique(metadata_subset$Condition[!is.na(metadata_subset$Condition)])) > 1) {
        condition_var <- sum(apply(pca_scores, 2, function(pc) {
          aov_result <- aov(pc ~ metadata_subset$Condition)
          sum_sq <- summary(aov_result)[[1]]$`Sum Sq`
          sum_sq[1] / sum(sum_sq)
        })) / n_pcs
        pvca_results <- rbind(pvca_results, 
                             data.frame(Factor = "Condition", Variance = condition_var))
      }
      
      # Residual variance
      total_var <- sum(pvca_results$Variance)
      residual_var <- max(0, 1 - total_var)
      pvca_results <- rbind(pvca_results, 
                           data.frame(Factor = "Residual", Variance = residual_var))
      
      # Create bar plot
      pvca_results$Factor <- factor(pvca_results$Factor, 
                                    levels = c("Dataset", "Condition", "Residual"))
      
      p <- ggplot(pvca_results, aes(x = Factor, y = Variance, fill = Factor)) +
        geom_bar(stat = "identity", alpha = 0.8) +
        scale_fill_manual(values = c("Dataset" = "#e74c3c", 
                                     "Condition" = "#3498db", 
                                     "Residual" = "#95a5a6")) +
        theme_bw(base_size = 14) +
        labs(
          title = "PVCA - After Batch Correction",
          subtitle = "Dataset variance reduced, Condition variance increased",
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
      
      print(p)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error generating PVCA:", e$message), cex = 1.2)
    })
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
    else ggplot2::ggsave(file, plot = p, width = 7, height = 5, device = "pdf", bg = "white")
  }
  output$download_pca_before_dataset_png <- downloadHandler(
    filename = function() "Batch_PCA_Before_Dataset.png",
    content = function(file) {
      req(rv$expr_filtered)
      pca <- prcomp(t(rv$expr_filtered), scale. = TRUE)
      pc1 <- pca$x[, 1]; pc2 <- pca$x[, 2]
      theta <- atan2(pc2, pc1); r <- sqrt(pc1^2 + pc2^2); if (max(r) > 0) r <- r / max(r)
      df <- data.frame(theta = theta, r = r, Dataset = rv$unified_metadata$Dataset)
      p <- ggplot2::ggplot(df, ggplot2::aes(x = theta, y = r, color = Dataset)) +
        ggplot2::geom_point(size = 3.5, alpha = 0.7) +
        ggplot2::coord_polar(theta = "x", start = -pi / 2, direction = 1) +
        ggplot2::scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), labels = c("-180°", "-90°", "0°", "90°", "180°")) +
        ggplot2::scale_y_continuous(limits = c(0, NA)) +
        ggplot2::theme_bw(base_size = 14) +
        ggplot2::labs(title = "Before Batch Correction - By Dataset", subtitle = "Batch effects visible as dataset separation (circular)", x = "Angle (PC1–PC2)", y = "Radius", color = "Dataset") +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5), plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5, color = "gray50"), legend.position = "right", panel.grid.minor = ggplot2::element_blank())
      batch_save_ggplot(p, file, "png")
    }
  )
  output$download_pca_before_dataset_pdf <- downloadHandler(
    filename = function() "Batch_PCA_Before_Dataset.pdf",
    content = function(file) {
      req(rv$expr_filtered)
      pca <- prcomp(t(rv$expr_filtered), scale. = TRUE)
      pc1 <- pca$x[, 1]; pc2 <- pca$x[, 2]
      theta <- atan2(pc2, pc1); r <- sqrt(pc1^2 + pc2^2); if (max(r) > 0) r <- r / max(r)
      df <- data.frame(theta = theta, r = r, Dataset = rv$unified_metadata$Dataset)
      p <- ggplot2::ggplot(df, ggplot2::aes(x = theta, y = r, color = Dataset)) +
        ggplot2::geom_point(size = 3.5, alpha = 0.7) +
        ggplot2::coord_polar(theta = "x", start = -pi / 2, direction = 1) +
        ggplot2::scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), labels = c("-180°", "-90°", "0°", "90°", "180°")) +
        ggplot2::scale_y_continuous(limits = c(0, NA)) +
        ggplot2::theme_bw(base_size = 14) +
        ggplot2::labs(title = "Before Batch Correction - By Dataset", subtitle = "Batch effects visible as dataset separation (circular)", x = "Angle (PC1–PC2)", y = "Radius", color = "Dataset") +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5), plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5, color = "gray50"), legend.position = "right", panel.grid.minor = ggplot2::element_blank())
      batch_save_ggplot(p, file, "pdf")
    }
  )
  output$download_pca_after_dataset_png <- downloadHandler(
    filename = function() "Batch_PCA_After_Dataset.png",
    content = function(file) {
      req(rv$batch_corrected)
      pca <- prcomp(t(rv$batch_corrected), scale. = TRUE)
      pc1 <- pca$x[, 1]; pc2 <- pca$x[, 2]
      theta <- atan2(pc2, pc1); r <- sqrt(pc1^2 + pc2^2); if (max(r) > 0) r <- r / max(r)
      df <- data.frame(theta = theta, r = r, Dataset = rv$unified_metadata$Dataset)
      p <- ggplot2::ggplot(df, ggplot2::aes(x = theta, y = r, color = Dataset)) +
        ggplot2::geom_point(size = 3.5, alpha = 0.7) +
        ggplot2::coord_polar(theta = "x", start = -pi / 2, direction = 1) +
        ggplot2::scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), labels = c("-180°", "-90°", "0°", "90°", "180°")) +
        ggplot2::scale_y_continuous(limits = c(0, NA)) +
        ggplot2::theme_bw(base_size = 14) +
        ggplot2::labs(title = "After Batch Correction - By Dataset", subtitle = "Batch effects reduced (circular)", x = "Angle (PC1–PC2)", y = "Radius", color = "Dataset") +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5), plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5, color = "gray50"), legend.position = "right", panel.grid.minor = ggplot2::element_blank())
      batch_save_ggplot(p, file, "png")
    }
  )
  output$download_pca_after_dataset_pdf <- downloadHandler(
    filename = function() "Batch_PCA_After_Dataset.pdf",
    content = function(file) {
      req(rv$batch_corrected)
      pca <- prcomp(t(rv$batch_corrected), scale. = TRUE)
      pc1 <- pca$x[, 1]; pc2 <- pca$x[, 2]
      theta <- atan2(pc2, pc1); r <- sqrt(pc1^2 + pc2^2); if (max(r) > 0) r <- r / max(r)
      df <- data.frame(theta = theta, r = r, Dataset = rv$unified_metadata$Dataset)
      p <- ggplot2::ggplot(df, ggplot2::aes(x = theta, y = r, color = Dataset)) +
        ggplot2::geom_point(size = 3.5, alpha = 0.7) +
        ggplot2::coord_polar(theta = "x", start = -pi / 2, direction = 1) +
        ggplot2::scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), labels = c("-180°", "-90°", "0°", "90°", "180°")) +
        ggplot2::scale_y_continuous(limits = c(0, NA)) +
        ggplot2::theme_bw(base_size = 14) +
        ggplot2::labs(title = "After Batch Correction - By Dataset", subtitle = "Batch effects reduced (circular)", x = "Angle (PC1–PC2)", y = "Radius", color = "Dataset") +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5), plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5, color = "gray50"), legend.position = "right", panel.grid.minor = ggplot2::element_blank())
      batch_save_ggplot(p, file, "pdf")
    }
  )
  output$download_pca_before_condition_png <- downloadHandler(
    filename = function() "Batch_PCA_Before_Condition.png",
    content = function(file) {
      p <- tryCatch({
        req(rv$expr_filtered)
        pca <- prcomp(t(rv$expr_filtered), scale. = TRUE)
        pc1 <- pca$x[, 1]; pc2 <- pca$x[, 2]
        theta <- atan2(pc2, pc1); r <- sqrt(pc1^2 + pc2^2); if (max(r) > 0) r <- r / max(r)
        df <- data.frame(theta = theta, r = r, Condition = rv$unified_metadata$Condition, Dataset = rv$unified_metadata$Dataset)
        if (all(is.na(df$Condition)) || length(unique(df$Condition[!is.na(df$Condition)])) < 2) {
          ggplot2::ggplot(df, ggplot2::aes(x = theta, y = r)) + ggplot2::geom_point(size = 3.5, alpha = 0.7, color = "gray60") +
            ggplot2::coord_polar(theta = "x", start = -pi / 2, direction = 1) +
            ggplot2::scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), labels = c("-180°", "-90°", "0°", "90°", "180°")) +
            ggplot2::scale_y_continuous(limits = c(0, NA)) + ggplot2::theme_bw(base_size = 14) +
            ggplot2::labs(title = "Before Batch Correction - By Condition", subtitle = "Conditions not yet assigned (circular)", x = "Angle (PC1–PC2)", y = "Radius") +
            ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5), plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5, color = "gray50"), panel.grid.minor = ggplot2::element_blank())
        } else {
          ggplot2::ggplot(df, ggplot2::aes(x = theta, y = r, color = Condition)) + ggplot2::geom_point(size = 3.5, alpha = 0.7) +
            ggplot2::scale_color_manual(values = c("Normal" = "#3498db", "Disease" = "#e74c3c", "None" = "#95a5a6"), na.value = "gray60") +
            ggplot2::coord_polar(theta = "x", start = -pi / 2, direction = 1) +
            ggplot2::scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), labels = c("-180°", "-90°", "0°", "90°", "180°")) +
            ggplot2::scale_y_continuous(limits = c(0, NA)) + ggplot2::theme_bw(base_size = 14) +
            ggplot2::labs(title = "Before Batch Correction - By Condition", subtitle = "Biological signal may be obscured by batch effects (circular)", x = "Angle (PC1–PC2)", y = "Radius", color = "Condition") +
            ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5), plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5, color = "gray50"), legend.position = "right", panel.grid.minor = ggplot2::element_blank())
        }
      }, error = function(e) NULL)
      batch_save_ggplot(p, file, "png")
    }
  )
  output$download_pca_before_condition_pdf <- downloadHandler(
    filename = function() "Batch_PCA_Before_Condition.pdf",
    content = function(file) {
      p <- tryCatch({
        req(rv$expr_filtered)
        pca <- prcomp(t(rv$expr_filtered), scale. = TRUE)
        pc1 <- pca$x[, 1]; pc2 <- pca$x[, 2]
        theta <- atan2(pc2, pc1); r <- sqrt(pc1^2 + pc2^2); if (max(r) > 0) r <- r / max(r)
        df <- data.frame(theta = theta, r = r, Condition = rv$unified_metadata$Condition, Dataset = rv$unified_metadata$Dataset)
        if (all(is.na(df$Condition)) || length(unique(df$Condition[!is.na(df$Condition)])) < 2) {
          ggplot2::ggplot(df, ggplot2::aes(x = theta, y = r)) + ggplot2::geom_point(size = 3.5, alpha = 0.7, color = "gray60") +
            ggplot2::coord_polar(theta = "x", start = -pi / 2, direction = 1) +
            ggplot2::scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), labels = c("-180°", "-90°", "0°", "90°", "180°")) +
            ggplot2::scale_y_continuous(limits = c(0, NA)) + ggplot2::theme_bw(base_size = 14) +
            ggplot2::labs(title = "Before Batch Correction - By Condition", subtitle = "Conditions not yet assigned (circular)", x = "Angle (PC1–PC2)", y = "Radius") +
            ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5), plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5, color = "gray50"), panel.grid.minor = ggplot2::element_blank())
        } else {
          ggplot2::ggplot(df, ggplot2::aes(x = theta, y = r, color = Condition)) + ggplot2::geom_point(size = 3.5, alpha = 0.7) +
            ggplot2::scale_color_manual(values = c("Normal" = "#3498db", "Disease" = "#e74c3c", "None" = "#95a5a6"), na.value = "gray60") +
            ggplot2::coord_polar(theta = "x", start = -pi / 2, direction = 1) +
            ggplot2::scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), labels = c("-180°", "-90°", "0°", "90°", "180°")) +
            ggplot2::scale_y_continuous(limits = c(0, NA)) + ggplot2::theme_bw(base_size = 14) +
            ggplot2::labs(title = "Before Batch Correction - By Condition", subtitle = "Biological signal may be obscured by batch effects (circular)", x = "Angle (PC1–PC2)", y = "Radius", color = "Condition") +
            ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5), plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5, color = "gray50"), legend.position = "right", panel.grid.minor = ggplot2::element_blank())
        }
      }, error = function(e) NULL)
      batch_save_ggplot(p, file, "pdf")
    }
  )
  output$download_pca_after_condition_png <- downloadHandler(
    filename = function() "Batch_PCA_After_Condition.png",
    content = function(file) {
      p <- tryCatch({
        req(rv$batch_corrected)
        pca <- prcomp(t(rv$batch_corrected), scale. = TRUE)
        pc1 <- pca$x[, 1]; pc2 <- pca$x[, 2]
        theta <- atan2(pc2, pc1); r <- sqrt(pc1^2 + pc2^2); if (max(r) > 0) r <- r / max(r)
        df <- data.frame(theta = theta, r = r, Condition = rv$unified_metadata$Condition, Dataset = rv$unified_metadata$Dataset)
        if (all(is.na(df$Condition)) || length(unique(df$Condition[!is.na(df$Condition)])) < 2) {
          ggplot2::ggplot(df, ggplot2::aes(x = theta, y = r)) + ggplot2::geom_point(size = 3.5, alpha = 0.7, color = "gray60") +
            ggplot2::coord_polar(theta = "x", start = -pi / 2, direction = 1) +
            ggplot2::scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), labels = c("-180°", "-90°", "0°", "90°", "180°")) +
            ggplot2::scale_y_continuous(limits = c(0, NA)) + ggplot2::theme_bw(base_size = 14) +
            ggplot2::labs(title = "After Batch Correction - By Condition", subtitle = "Conditions (circular)", x = "Angle (PC1–PC2)", y = "Radius") +
            ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5), panel.grid.minor = ggplot2::element_blank())
        } else {
          ggplot2::ggplot(df, ggplot2::aes(x = theta, y = r, color = Condition)) + ggplot2::geom_point(size = 3.5, alpha = 0.7) +
            ggplot2::scale_color_manual(values = c("Normal" = "#3498db", "Disease" = "#e74c3c", "None" = "#95a5a6"), na.value = "gray60") +
            ggplot2::coord_polar(theta = "x", start = -pi / 2, direction = 1) +
            ggplot2::scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), labels = c("-180°", "-90°", "0°", "90°", "180°")) +
            ggplot2::scale_y_continuous(limits = c(0, NA)) + ggplot2::theme_bw(base_size = 14) +
            ggplot2::labs(title = "After Batch Correction - By Condition", subtitle = "Biological signal (circular)", x = "Angle (PC1–PC2)", y = "Radius", color = "Condition") +
            ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5), legend.position = "right", panel.grid.minor = ggplot2::element_blank())
        }
      }, error = function(e) NULL)
      batch_save_ggplot(p, file, "png")
    }
  )
  output$download_pca_after_condition_pdf <- downloadHandler(
    filename = function() "Batch_PCA_After_Condition.pdf",
    content = function(file) {
      p <- tryCatch({
        req(rv$batch_corrected)
        pca <- prcomp(t(rv$batch_corrected), scale. = TRUE)
        pc1 <- pca$x[, 1]; pc2 <- pca$x[, 2]
        theta <- atan2(pc2, pc1); r <- sqrt(pc1^2 + pc2^2); if (max(r) > 0) r <- r / max(r)
        df <- data.frame(theta = theta, r = r, Condition = rv$unified_metadata$Condition, Dataset = rv$unified_metadata$Dataset)
        if (all(is.na(df$Condition)) || length(unique(df$Condition[!is.na(df$Condition)])) < 2) {
          ggplot2::ggplot(df, ggplot2::aes(x = theta, y = r)) + ggplot2::geom_point(size = 3.5, alpha = 0.7, color = "gray60") +
            ggplot2::coord_polar(theta = "x", start = -pi / 2, direction = 1) +
            ggplot2::scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), labels = c("-180°", "-90°", "0°", "90°", "180°")) +
            ggplot2::scale_y_continuous(limits = c(0, NA)) + ggplot2::theme_bw(base_size = 14) +
            ggplot2::labs(title = "After Batch Correction - By Condition", subtitle = "Conditions (circular)", x = "Angle (PC1–PC2)", y = "Radius") +
            ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5), panel.grid.minor = ggplot2::element_blank())
        } else {
          ggplot2::ggplot(df, ggplot2::aes(x = theta, y = r, color = Condition)) + ggplot2::geom_point(size = 3.5, alpha = 0.7) +
            ggplot2::scale_color_manual(values = c("Normal" = "#3498db", "Disease" = "#e74c3c", "None" = "#95a5a6"), na.value = "gray60") +
            ggplot2::coord_polar(theta = "x", start = -pi / 2, direction = 1) +
            ggplot2::scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), labels = c("-180°", "-90°", "0°", "90°", "180°")) +
            ggplot2::scale_y_continuous(limits = c(0, NA)) + ggplot2::theme_bw(base_size = 14) +
            ggplot2::labs(title = "After Batch Correction - By Condition", subtitle = "Biological signal (circular)", x = "Angle (PC1–PC2)", y = "Radius", color = "Condition") +
            ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5), legend.position = "right", panel.grid.minor = ggplot2::element_blank())
        }
      }, error = function(e) NULL)
      batch_save_ggplot(p, file, "pdf")
    }
  )

  # Hclust and PVCA: draw to device (pheatmap / base)
  batch_hclust_to_file <- function(file, dev_fun, before = TRUE) {
    expr_mat <- if (before) rv$expr_filtered else rv$batch_corrected
    if (is.null(expr_mat)) return()
    n_samples <- min(50, ncol(expr_mat))
    if (ncol(expr_mat) > n_samples) { set.seed(123); sample_idx <- sample(seq_len(ncol(expr_mat)), n_samples); expr_subset <- expr_mat[, sample_idx]; metadata_subset <- rv$unified_metadata[sample_idx, ] } else { expr_subset <- expr_mat; metadata_subset <- rv$unified_metadata }
    cor_matrix <- cor(expr_subset, use = "pairwise.complete.obs")
    dist_matrix <- as.dist(1 - cor_matrix)
    hclust_result <- hclust(dist_matrix, method = "ward.D2")
    annotation_col <- data.frame(Dataset = metadata_subset$Dataset, Condition = ifelse(is.na(metadata_subset$Condition), "Unknown", metadata_subset$Condition), row.names = colnames(expr_subset))
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
  output$download_hclust_before_pdf <- downloadHandler(
    filename = function() "Batch_Hclust_Before.pdf",
    content = function(file) batch_hclust_to_file(file, function(f) pdf(f, width = 10, height = 8, bg = "white"), before = TRUE)
  )
  output$download_hclust_after_png <- downloadHandler(
    filename = function() "Batch_Hclust_After.png",
    content = function(file) batch_hclust_to_file(file, function(f) png(f, width = 10, height = 8, res = IMAGE_DPI, units = "in", bg = "white"), before = FALSE)
  )
  output$download_hclust_after_pdf <- downloadHandler(
    filename = function() "Batch_Hclust_After.pdf",
    content = function(file) batch_hclust_to_file(file, function(f) pdf(f, width = 10, height = 8, bg = "white"), before = FALSE)
  )

  # PVCA: rebuild bar plot and ggsave
  batch_pvca_to_plot <- function(before = TRUE) {
    expr_mat <- if (before) rv$expr_filtered else rv$batch_corrected
    if (is.null(expr_mat)) return(NULL)
    n_samples <- min(100, ncol(expr_mat))
    if (ncol(expr_mat) > n_samples) { set.seed(123); sample_idx <- sample(seq_len(ncol(expr_mat)), n_samples); expr_subset <- expr_mat[, sample_idx]; metadata_subset <- rv$unified_metadata[sample_idx, ] } else { expr_subset <- expr_mat; metadata_subset <- rv$unified_metadata }
    pca <- prcomp(t(expr_subset), scale. = TRUE)
    cum_var <- cumsum(pca$sdev^2 / sum(pca$sdev^2)); n_pcs <- min(10, which(cum_var >= 0.8)[1]); if (is.na(n_pcs)) n_pcs <- min(10, ncol(pca$x))
    pca_scores <- pca$x[, 1:n_pcs]
    pvca_results <- data.frame(Factor = character(), Variance = numeric(), stringsAsFactors = FALSE)
    if (length(unique(metadata_subset$Dataset)) > 1) {
      dataset_var <- sum(apply(pca_scores, 2, function(pc) { aov_result <- aov(pc ~ metadata_subset$Dataset); sum_sq <- summary(aov_result)[[1]]$`Sum Sq`; sum_sq[1] / sum(sum_sq) })) / n_pcs
      pvca_results <- rbind(pvca_results, data.frame(Factor = "Dataset", Variance = dataset_var))
    }
    if (!all(is.na(metadata_subset$Condition)) && length(unique(metadata_subset$Condition[!is.na(metadata_subset$Condition)])) > 1) {
      condition_var <- sum(apply(pca_scores, 2, function(pc) { aov_result <- aov(pc ~ metadata_subset$Condition); sum_sq <- summary(aov_result)[[1]]$`Sum Sq`; sum_sq[1] / sum(sum_sq) })) / n_pcs
      pvca_results <- rbind(pvca_results, data.frame(Factor = "Condition", Variance = condition_var))
    }
    total_var <- sum(pvca_results$Variance); residual_var <- max(0, 1 - total_var)
    pvca_results <- rbind(pvca_results, data.frame(Factor = "Residual", Variance = residual_var))
    pvca_results$Factor <- factor(pvca_results$Factor, levels = c("Dataset", "Condition", "Residual"))
    ggplot2::ggplot(pvca_results, ggplot2::aes(x = Factor, y = Variance, fill = Factor)) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
      ggplot2::scale_fill_manual(values = c("Dataset" = "#e74c3c", "Condition" = "#3498db", "Residual" = "#95a5a6")) +
      ggplot2::theme_bw(base_size = 14) +
      ggplot2::labs(title = paste0("PVCA - ", if (before) "Before" else "After", " Batch Correction"), subtitle = "Variance explained by Dataset vs Condition", x = "Factor", y = "Proportion of Variance", fill = "Factor") +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5), plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5, color = "gray50"), legend.position = "right", panel.grid.major = ggplot2::element_line(color = "gray90"), panel.grid.minor = ggplot2::element_line(color = "gray95")) +
      ggplot2::ylim(0, 1)
  }
  output$download_pvca_before_png <- downloadHandler(
    filename = function() "Batch_PVCA_Before.png",
    content = function(file) { p <- batch_pvca_to_plot(before = TRUE); if (!is.null(p)) batch_save_ggplot(p, file, "png") }
  )
  output$download_pvca_before_pdf <- downloadHandler(
    filename = function() "Batch_PVCA_Before.pdf",
    content = function(file) { p <- batch_pvca_to_plot(before = TRUE); if (!is.null(p)) batch_save_ggplot(p, file, "pdf") }
  )
  output$download_pvca_after_png <- downloadHandler(
    filename = function() "Batch_PVCA_After.png",
    content = function(file) { p <- batch_pvca_to_plot(before = FALSE); if (!is.null(p)) batch_save_ggplot(p, file, "png") }
  )
  output$download_pvca_after_pdf <- downloadHandler(
    filename = function() "Batch_PVCA_After.pdf",
    content = function(file) { p <- batch_pvca_to_plot(before = FALSE); if (!is.null(p)) batch_save_ggplot(p, file, "pdf") }
  )
  
}


