# ==============================================================================
# SERVER_BATCH.R - Step 5: Batch Correction Module
# ==============================================================================

server_batch <- function(input, output, session, rv) {

  output$batch_timer <- renderText({
    if (!isTRUE(rv$batch_running) || is.null(rv$batch_start)) return("00:00")
    invalidateLater(1000, session)
    elapsed <- as.integer(difftime(Sys.time(), rv$batch_start, units = "secs"))
    sprintf("%02d:%02d", elapsed %/% 60, elapsed %% 60)
  })
  
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
  
  output$gene_variance_plot <- renderPlot({
    req(rv$combined_expr, input$variance_percentile)
    
    gene_vars <- apply(rv$combined_expr, 1, var)
    cutoff <- variance_cutoff()
    
    df <- data.frame(
      Variance = gene_vars,
      Kept = ifelse(gene_vars > cutoff, "Retained", "Filtered")
    )
    
    ggplot(df, aes(x = log10(Variance), fill = Kept)) +
      geom_histogram(bins = 50, alpha = 0.7) +
      geom_vline(xintercept = log10(cutoff), linetype = "dashed", color = "red", size = 1.5) +
      scale_fill_manual(values = c("Retained" = "#2ecc71", "Filtered" = "#e74c3c")) +
      theme_bw(base_size = 14) +
      labs(title = "Gene Variance Distribution",
           subtitle = paste0("Cutoff (", input$variance_percentile, "th percentile): ", round(cutoff, 4)),
           x = "Log10(Variance)", y = "Count",
           fill = "Status") +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "top")
  })
  
  observeEvent(input$apply_batch, {
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
        rv$batch_corrected <- removeBatchEffect(rv$expr_filtered,
                                                 batch = rv$unified_metadata$Dataset,
                                                 design = design)
      } else if (input$batch_method == "combat") {
        rv$batch_corrected <- ComBat(rv$expr_filtered,
                                      batch = rv$unified_metadata$Dataset,
                                      mod = NULL, par.prior = TRUE, prior.plots = FALSE)
      } else if (input$batch_method == "quantile_limma") {
        expr_q <- normalizeBetweenArrays(rv$expr_filtered, method = "quantile")
        rv$batch_corrected <- removeBatchEffect(expr_q,
                                                 batch = rv$unified_metadata$Dataset,
                                                 design = design)
      } else if (input$batch_method == "hybrid") {
        expr_q <- normalizeBetweenArrays(rv$expr_filtered, method = "quantile")
        rv$batch_corrected <- ComBat(expr_q, batch = rv$unified_metadata$Dataset,
                                      mod = NULL, par.prior = TRUE, prior.plots = FALSE)
      } else if (input$batch_method == "combat_ref") {
        sizes <- table(rv$unified_metadata$Dataset)
        ref <- names(sizes)[which.max(sizes)]
        rv$batch_corrected <- ComBat(rv$expr_filtered,
                                      batch = rv$unified_metadata$Dataset,
                                      mod = NULL, par.prior = TRUE, prior.plots = FALSE,
                                      ref.batch = ref)
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
            rv$batch_corrected <- ComBat(rv$expr_filtered,
                                         batch = rv$unified_metadata$Dataset,
                                         mod = mod_sv, par.prior = TRUE, prior.plots = FALSE)
          } else {
            rv$batch_corrected <- ComBat(rv$expr_filtered,
                                         batch = rv$unified_metadata$Dataset,
                                         mod = mod, par.prior = TRUE, prior.plots = FALSE)
          }
        } else {
          rv$batch_corrected <- ComBat(rv$expr_filtered,
                                       batch = rv$unified_metadata$Dataset,
                                       mod = mod, par.prior = TRUE, prior.plots = FALSE)
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
  
  # PCA Before Batch Correction - Colored by Dataset
  output$pca_before_dataset <- renderPlot({
    req(rv$expr_filtered)
    
    pca <- prcomp(t(rv$expr_filtered), scale. = TRUE)
    var_exp <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
    
    df <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2],
                     Dataset = rv$unified_metadata$Dataset)
    
    ggplot(df, aes(PC1, PC2, color = Dataset)) +
      geom_point(size = 3.5, alpha = 0.7) +
      theme_bw(base_size = 14) +
      labs(title = "Before Batch Correction - By Dataset",
           subtitle = "Batch effects visible as dataset separation",
           x = paste0("PC1 (", var_exp[1], "%)"),
           y = paste0("PC2 (", var_exp[2], "%)"),
           color = "Dataset") +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
        legend.position = "right"
      )
  })
  
  # PCA Before Batch Correction - Colored by Condition
  output$pca_before_condition <- renderPlot({
    req(rv$expr_filtered)
    
    pca <- prcomp(t(rv$expr_filtered), scale. = TRUE)
    var_exp <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
    
    df <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2],
                     Condition = rv$unified_metadata$Condition,
                     Dataset = rv$unified_metadata$Dataset)
    
    # Only plot if conditions are available
    if (all(is.na(df$Condition)) || length(unique(df$Condition[!is.na(df$Condition)])) < 2) {
      ggplot(df, aes(PC1, PC2)) +
        geom_point(size = 3.5, alpha = 0.7, color = "gray60") +
        theme_bw(base_size = 14) +
        labs(title = "Before Batch Correction - By Condition",
             subtitle = "Conditions not yet assigned",
             x = paste0("PC1 (", var_exp[1], "%)"),
             y = paste0("PC2 (", var_exp[2], "%)")) +
        theme(
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50")
        )
    } else {
      ggplot(df, aes(PC1, PC2, color = Condition)) +
        geom_point(size = 3.5, alpha = 0.7) +
        scale_color_manual(values = c("Normal" = "#3498db", "Disease" = "#e74c3c", "None" = "#95a5a6"),
                          na.value = "gray60") +
        theme_bw(base_size = 14) +
        labs(title = "Before Batch Correction - By Condition",
             subtitle = "Biological signal may be obscured by batch effects",
             x = paste0("PC1 (", var_exp[1], "%)"),
             y = paste0("PC2 (", var_exp[2], "%)"),
             color = "Condition") +
        theme(
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
          legend.position = "right"
        )
    }
  })
  
  # PCA After Batch Correction - Colored by Dataset
  output$pca_after_dataset <- renderPlot({
    req(rv$batch_corrected)
    
    pca <- prcomp(t(rv$batch_corrected), scale. = TRUE)
    var_exp <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
    
    df <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2],
                     Dataset = rv$unified_metadata$Dataset)
    
    ggplot(df, aes(PC1, PC2, color = Dataset)) +
      geom_point(size = 3.5, alpha = 0.7) +
      theme_bw(base_size = 14) +
      labs(title = "After Batch Correction - By Dataset",
           subtitle = "Datasets should be intermingled (batch effects removed)",
           x = paste0("PC1 (", var_exp[1], "%)"),
           y = paste0("PC2 (", var_exp[2], "%)"),
           color = "Dataset") +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
        legend.position = "right"
      )
  })
  
  # PCA After Batch Correction - Colored by Condition
  output$pca_after_condition <- renderPlot({
    req(rv$batch_corrected)
    
    pca <- prcomp(t(rv$batch_corrected), scale. = TRUE)
    var_exp <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
    
    df <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2],
                     Condition = rv$unified_metadata$Condition)
    
    # Only plot if conditions are available
    if (all(is.na(df$Condition)) || length(unique(df$Condition[!is.na(df$Condition)])) < 2) {
      ggplot(df, aes(PC1, PC2)) +
        geom_point(size = 3.5, alpha = 0.7, color = "gray60") +
        theme_bw(base_size = 14) +
        labs(title = "After Batch Correction - By Condition",
             subtitle = "Conditions not yet assigned",
             x = paste0("PC1 (", var_exp[1], "%)"),
             y = paste0("PC2 (", var_exp[2], "%)")) +
        theme(
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50")
        )
    } else {
      ggplot(df, aes(PC1, PC2, color = Condition)) +
        geom_point(size = 3.5, alpha = 0.7) +
        scale_color_manual(values = c("Normal" = "#3498db", "Disease" = "#e74c3c", "None" = "#95a5a6"),
                          na.value = "gray60") +
        theme_bw(base_size = 14) +
        labs(title = "After Batch Correction - By Condition",
             subtitle = "Biological signal should be clearly visible",
             x = paste0("PC1 (", var_exp[1], "%)"),
             y = paste0("PC2 (", var_exp[2], "%)"),
             color = "Condition") +
        theme(
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
          legend.position = "right"
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
  
}


