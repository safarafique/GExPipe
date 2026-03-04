# ==============================================================================
# SERVER_RESULTS.R - Step 6: Differential Gene Expression Analysis Module
# ==============================================================================

server_results <- function(input, output, session, rv) {

  # ---------- METHOD BANNER (shows active DE method on Step 6) ----------
  output$results_process_summary_ui <- renderUI({
    if (is.null(rv$sig_genes) || nrow(rv$sig_genes) == 0) {
      return(tags$p(style = "color: #6c757d; margin: 0;", icon("info-circle"), " Run DE analysis to see process summary."))
    }
    n_sig <- nrow(rv$sig_genes)
    n_up <- if (!is.null(rv$de_results)) sum(rv$de_results$Significance == "Up-regulated", na.rm = TRUE) else NA
    n_down <- if (!is.null(rv$de_results)) sum(rv$de_results$Significance == "Down-regulated", na.rm = TRUE) else NA
    tags$div(
      style = "font-size: 14px; line-height: 1.6; color: #333;",
      tags$p(tags$strong("Step 6 complete."), " Significant DEGs: ", format(n_sig, big.mark = ","), "."),
      if (!is.na(n_up)) tags$p("Up-regulated: ", n_up, "; Down-regulated: ", n_down, ". Volcano plot and heatmap above.") else NULL)
  })

  output$de_method_banner <- renderUI({
    method <- rv$de_method
    if (is.null(method) || method == "limma") {
      tags$div(
        class = "alert alert-info",
        style = "margin: 0 15px 10px 15px; padding: 12px 18px; border-left: 5px solid #3498db;",
        icon("flask"),
        tags$strong(" Active DE method: limma"),
        " — Empirical Bayes moderated t-statistics on batch-corrected, normalized expression.",
        tags$small(" (Change in Step 1 before running DE)", style = "color: #6c757d;")
      )
    } else if (method == "limma_voom") {
      tags$div(
        class = "alert alert-info",
        style = "margin: 0 15px 10px 15px; padding: 12px 18px; border-left: 5px solid #8e44ad;",
        icon("flask"),
        tags$strong(" Active DE method: limma-voom"),
        " — voom transforms RNA-seq counts to logCPM with precision weights, then uses limma's empirical Bayes linear models (supports batch covariates).",
        tags$small(" (Change in Step 1 before running DE)", style = "color: #6c757d;")
      )
    } else if (method == "deseq2") {
      tags$div(
        class = "alert alert-success",
        style = "margin: 0 15px 10px 15px; padding: 12px 18px; border-left: 5px solid #27ae60;",
        icon("dna"),
        tags$strong(" Active DE method: DESeq2"),
        " — Negative binomial GLM on raw counts with batch as covariate. DESeq2's internal normalization (median-of-ratios) is used.",
        tags$small(" (Change in Step 1 before running DE)", style = "color: #6c757d;")
      )
    } else {
      tags$div(
        class = "alert alert-warning",
        style = "margin: 0 15px 10px 15px; padding: 12px 18px; border-left: 5px solid #f39c12;",
        icon("chart-bar"),
        tags$strong(" Active DE method: edgeR"),
        " — Quasi-likelihood F-test on raw counts with TMM normalization and batch as covariate.",
        tags$small(" (Change in Step 1 before running DE)", style = "color: #6c757d;")
      )
    }
  })
  
  # ---------- RUN DE ANALYSIS ----------
  observeEvent(input$run_de, {
    if (!isTRUE(rv$batch_complete)) {
      showNotification(
        tags$div(icon("exclamation-triangle"), tags$strong(" Step 5 required:"),
                 " Complete batch correction (Step 5) before running DE analysis."),
        type = "error", duration = 6)
      return()
    }

    # DE requires both Normal and Disease; otherwise there is no contrast (e.g. same sample source = one condition only)
    cond_counts <- table(rv$unified_metadata$Condition)
    n_normal <- if ("Normal" %in% names(cond_counts)) cond_counts[["Normal"]] else 0L
    n_disease <- if ("Disease" %in% names(cond_counts)) cond_counts[["Disease"]] else 0L
    if (n_normal == 0L || n_disease == 0L) {
      showNotification(
        tags$div(
          icon("exclamation-triangle"), tags$strong(" Need both Normal and Disease samples for DE."),
          tags$br(),
          "You have ", n_normal, " Normal and ", n_disease, " Disease. Differential expression compares these two groups.",
          " If you entered GSEs from the same sample source (e.g. same study or only one condition), add a dataset that contains the other group, or in Step 3 assign some samples to Normal and some to Disease."
        ),
        type = "error", duration = 12)
      rv$de_running <- FALSE
      return()
    }

    method <- rv$de_method
    if (is.null(method)) method <- "limma"
    
    rv$de_start <- Sys.time()
    rv$de_running <- TRUE
    
    tryCatch({
      # Pre-checks for count-based methods (DESeq2 / edgeR)
      if (method == "deseq2") {
        if (!requireNamespace("DESeq2", quietly = TRUE)) {
          showNotification(
            tags$div(icon("exclamation-triangle"), tags$strong(" DESeq2 not installed."),
                     " Install with: BiocManager::install('DESeq2'). Falling back to limma."),
            type = "error", duration = 8)
          method <- "limma"
        } else if (is.null(rv$raw_counts_for_deseq2)) {
          showNotification(
            tags$div(icon("exclamation-triangle"), tags$strong(" Raw counts not available."),
                     " DESeq2 requires RNA-seq raw counts. Re-run normalization (Step 3) with DESeq2 selected, or switch to limma."),
            type = "error", duration = 8)
          rv$de_running <- FALSE
          return()
        }
      }
      if (method == "edger") {
        if (is.null(rv$raw_counts_for_deseq2)) {
          showNotification(
            tags$div(icon("exclamation-triangle"), tags$strong(" Raw counts not available."),
                     " edgeR requires RNA-seq raw counts. Re-run normalization (Step 3) with edgeR selected, or switch to limma."),
            type = "error", duration = 8)
          rv$de_running <- FALSE
          return()
        }
      }
      if (method == "limma_voom") {
        if (is.null(rv$raw_counts_for_deseq2)) {
          showNotification(
            tags$div(icon("exclamation-triangle"), tags$strong(" Raw counts not available."),
                     " limma-voom requires RNA-seq raw counts. Re-run normalization (Step 3) with limma-voom selected, or switch to limma."),
            type = "error", duration = 8)
          rv$de_running <- FALSE
          return()
        }
      }
      
      if (method == "deseq2") {
        withProgress(message = 'DESeq2 analysis...', value = 0, {
          
          # Build sample metadata for DESeq2
          count_mat <- rv$raw_counts_for_deseq2
          
          # Use the metadata that has Condition labels applied
          meta <- rv$unified_metadata
          
          # Align samples between count matrix and metadata
          common_samples <- intersect(colnames(count_mat), rownames(meta))
          if (length(common_samples) < 3) {
            showNotification(
              tags$div(icon("exclamation-triangle"), tags$strong(" Too few samples."),
                       " DESeq2 needs ≥ 3 samples matching between raw counts and metadata."),
              type = "error", duration = 8)
            rv$de_running <- FALSE
            return()
          }
          count_mat <- count_mat[, common_samples, drop = FALSE]
          meta <- meta[common_samples, , drop = FALSE]
          
          # Ensure Condition is a factor
          meta$Condition <- factor(meta$Condition, levels = c("Normal", "Disease"))
          
          # Remove any rows with all zeros
          keep <- rowSums(count_mat) > 0
          count_mat <- count_mat[keep, , drop = FALSE]
          
          incProgress(0.2, detail = "Creating DESeqDataSet...")
          
          # Build DESeqDataSet with batch (Dataset) as covariate if > 1 batch
          n_batches <- length(unique(meta$Dataset))
          if (n_batches > 1) {
            meta$Dataset <- factor(meta$Dataset)
            dds <- DESeq2::DESeqDataSetFromMatrix(
              countData = count_mat,
              colData = meta,
              design = ~ Dataset + Condition
            )
          } else {
            dds <- DESeq2::DESeqDataSetFromMatrix(
              countData = count_mat,
              colData = meta,
              design = ~ Condition
            )
          }
          
          incProgress(0.3, detail = "Running DESeq2...")
          
          # Run DESeq2
          dds <- DESeq2::DESeq(dds, quiet = TRUE)
          
          incProgress(0.3, detail = "Extracting results...")
          
          # Extract results (Disease vs Normal)
          res <- DESeq2::results(dds, contrast = c("Condition", "Disease", "Normal"),
                                 alpha = input$padj_cutoff)
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
          
          meta$Condition <- factor(meta$Condition, levels = c("Normal", "Disease"))
          
          # Remove zero-count genes
          keep <- rowSums(count_mat) > 0
          count_mat <- count_mat[keep, , drop = FALSE]
          
          incProgress(0.2, detail = "Creating DGEList...")
          
          # Create DGEList and apply TMM normalization
          dge <- edgeR::DGEList(counts = count_mat, group = meta$Condition)
          dge <- edgeR::calcNormFactors(dge, method = "TMM")
          
          # Build design matrix with batch if multiple datasets
          n_batches <- length(unique(meta$Dataset))
          if (n_batches > 1) {
            meta$Dataset <- factor(meta$Dataset)
            design <- model.matrix(~ Dataset + Condition, data = meta)
          } else {
            design <- model.matrix(~ Condition, data = meta)
          }
          
          incProgress(0.2, detail = "Estimating dispersion...")
          
          # Estimate dispersion and fit GLM
          dge <- edgeR::estimateDisp(dge, design)
          fit <- edgeR::glmQLFit(dge, design)
          
          incProgress(0.3, detail = "Testing for DE...")
          
          # Test the Condition coefficient (last column)
          qlf <- edgeR::glmQLFTest(fit, coef = ncol(design))
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
          if (length(common_samples) < 3) {
            showNotification(
              tags$div(icon("exclamation-triangle"), tags$strong(" Too few samples."),
                       " limma-voom needs ≥ 3 samples matching between raw counts and metadata."),
              type = "error", duration = 8)
            rv$de_running <- FALSE
            return()
          }
          count_mat <- count_mat[, common_samples, drop = FALSE]
          meta <- meta[common_samples, , drop = FALSE]
          
          # Ensure Condition is a factor
          meta$Condition <- factor(meta$Condition, levels = c("Normal", "Disease"))
          
          # Remove genes with all zero counts
          keep <- rowSums(count_mat) > 0
          count_mat <- count_mat[keep, , drop = FALSE]
          
          # Build design matrix with batch (Dataset) as covariate if > 1 batch
          n_batches <- length(unique(meta$Dataset))
          if (n_batches > 1) {
            meta$Dataset <- factor(meta$Dataset)
            design <- model.matrix(~ Dataset + Condition, data = meta)
          } else {
            design <- model.matrix(~ Condition, data = meta)
          }
          
          incProgress(0.3, detail = "Estimating mean–variance with voom...")
          
          # Apply voom to compute logCPM and precision weights
          v <- limma::voom(count_mat, design = design, plot = FALSE)
          
          incProgress(0.3, detail = "Fitting limma model...")
          
          fit <- limma::lmFit(v, design)
          fit <- limma::eBayes(fit)
          
          # Extract results for Condition effect (last column of design)
          coef_idx <- ncol(design)
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
        # LIMMA PATHWAY (microarray, or merged RNA+microarray; batch-aware when multiple datasets)
        # ==================================================================
        withProgress(message = 'limma DE analysis...', value = 0, {
          
          if (is.null(rv$batch_corrected) || is.null(rv$unified_metadata)) {
            showNotification(
              tags$div(icon("exclamation-triangle"), tags$strong(" Missing data."),
                       " Run Steps 1–5 (Download, Groups, QC, Normalize, Batch) first."),
              type = "error", duration = 8)
            rv$de_running <- FALSE
            return()
          }
          
          meta <- rv$unified_metadata
          meta$Condition <- factor(meta$Condition, levels = c("Normal", "Disease"))
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
          
          # Design: include Dataset (batch) when multiple datasets (merged or multi-GSE)
          n_batches <- length(unique(meta$Dataset))
          if (n_batches > 1) {
            meta$Dataset <- factor(meta$Dataset)
            design <- model.matrix(~ Dataset + Condition, data = meta)
            coef_condition <- ncol(design)  # last column is ConditionDisease
            fit <- limma::lmFit(rv$batch_corrected, design)
            fit2 <- limma::eBayes(fit)
            de_results <- limma::topTable(fit2, coef = coef_condition, number = Inf, adjust.method = "BH")
          } else {
            design <- model.matrix(~ 0 + Condition, data = meta)
            colnames(design) <- levels(meta$Condition)
            contrast <- limma::makeContrasts(Disease - Normal, levels = design)
            fit <- limma::lmFit(rv$batch_corrected, design)
            fit2 <- limma::contrasts.fit(fit, contrast)
            fit2 <- limma::eBayes(fit2)
            de_results <- limma::topTable(fit2, number = Inf, adjust.method = "BH")
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
        })
      }
    }, error = function(e) {
      showNotification(
        tags$div(icon("times-circle"), tags$strong(" DE analysis failed: "),
                 conditionMessage(e)),
        type = "error", duration = 10)
    })

    rv$de_running <- FALSE
  })
  
  output$total_degs <- renderInfoBox({
    n <- if (!is.null(rv$sig_genes)) nrow(rv$sig_genes) else 0
    infoBox("Total DEGs", n, icon = icon("star", class = "fa-2x"), 
            color = "yellow", fill = TRUE)
  })
  
  output$up_genes <- renderInfoBox({
    n <- if (!is.null(rv$de_results)) sum(rv$de_results$Significance == "Up-regulated") else 0
    infoBox("Up-regulated", n, icon = icon("arrow-up", class = "fa-2x"), 
            color = "red", fill = TRUE)
  })
  
  output$down_genes <- renderInfoBox({
    n <- if (!is.null(rv$de_results)) sum(rv$de_results$Significance == "Down-regulated") else 0
    infoBox("Down-regulated", n, icon = icon("arrow-down", class = "fa-2x"), 
            color = "blue", fill = TRUE)
  })
  
  # Pipeline verification: confirm DESeq2 + ComBat_ref and that volcano = real DE results
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
    tags$div(
      class = "alert alert-info",
      style = "margin: 0 15px 16px 15px; padding: 16px 20px; border-radius: 12px; border-left: 5px solid #3498db; background: linear-gradient(90deg, #e8f4f8 0%, #f8fafc 100%);",
      tags$p(
        style = "margin: 0 0 10px 0; font-weight: 700; font-size: 15px; color: #1e293b;",
        icon("check-circle", style = "color: #10b981; margin-right: 8px;"),
        "Pipeline verification — Volcano shows real DE results"
      ),
      tags$p(
        style = "margin: 0 0 6px 0; font-size: 13px; color: #334155; line-height: 1.6;",
        tags$strong("DE method:"), " ", if (method == "deseq2") "DESeq2" else if (method == "edger") "edgeR" else if (method == "limma_voom") "limma-voom" else "limma",
        if (batch_in_model) " — batch (Dataset) included in the statistical model, so the volcano reflects differential expression after adjusting for batch." else "."
      ),
      tags$p(
        style = "margin: 0 0 6px 0; font-size: 13px; color: #334155; line-height: 1.6;",
        tags$strong("Batch correction (Step 5):"), " ",
        if (batch_done) paste0(batch_label, " — applied to the expression matrix used for heatmaps, WGCNA, and downstream steps.")
        else "Not applied (single dataset or Step 5 skipped)."
      ),
      tags$p(
        style = "margin: 0; font-size: 12px; color: #64748b;",
        "All prior steps (Download → Normalize → Groups → Batch correction) were completed before DE. The volcano plot uses the actual test statistics (log2FC and adjusted p-value) from the selected DE method. ",
        "Use the ", tags$strong("How to check your results are valid"), " box below to verify groups and ML prediction performance."
      )
    )
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

      p <- ggplot2::ggplot(volcano_data, ggplot2::aes(x = logFC, y = neg_log10_padj, color = Significance)) +
        ggplot2::geom_point(alpha = 0.6, size = 2) +
        ggplot2::scale_color_manual(
          values = c("Up-regulated" = "#e74c3c", "Down-regulated" = "#3498db", "Not Significant" = "gray70"),
          name = "Significance"
        ) +
        ggplot2::theme_bw(base_size = 14) +
        ggplot2::labs(
          title = "Volcano Plot: Disease vs Normal",
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
      print(p)
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
      
      # Remove rows with zero variance (constant expression — can't scale)
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
      
      pheatmap(expr_scaled, annotation_col = annot, annotation_colors = annot_colors,
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
        ggplot2::theme_bw(base_size = 14) +
        ggplot2::labs(title = "Volcano Plot: Disease vs Normal", subtitle = paste0("DEGs: ", n_sig, " (Up: ", n_up, ", Down: ", n_down, ")"), x = "Log2 Fold Change", y = "-Log10 Adjusted P-value") +
        ggplot2::geom_hline(yintercept = -log10(padj_cut), linetype = "dashed", color = "gray40", alpha = 0.7) +
        ggplot2::geom_vline(xintercept = c(-logfc_cut, logfc_cut), linetype = "dashed", color = "gray40", alpha = 0.7) +
        ggrepel::geom_text_repel(ggplot2::aes(label = Label), size = 3, max.overlaps = 20, box.padding = 0.5, segment.color = "gray50") +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 16), plot.subtitle = ggplot2::element_text(size = 12), legend.position = "right")
      ggplot2::ggsave(file, plot = p, device = "png", width = 10, height = 7, dpi = 150, bg = "white")
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
        ggplot2::theme_bw(base_size = 14) +
        ggplot2::labs(title = "Volcano Plot: Disease vs Normal", subtitle = paste0("DEGs: ", n_sig, " (Up: ", n_up, ", Down: ", n_down, ")"), x = "Log2 Fold Change", y = "-Log10 Adjusted P-value") +
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
      pheatmap(expr_scaled, annotation_col = annot, annotation_colors = annot_colors,
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
      pheatmap(expr_scaled, annotation_col = annot, annotation_colors = annot_colors,
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
  
  # download_workspace is defined in server.R (single handler for sidebar + results tab)
}


