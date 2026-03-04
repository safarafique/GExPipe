# ==============================================================================
# SERVER_RESULTS_SUMMARY.R - Step 14: Results Summary + PDF download
# ==============================================================================
# Renders summary sections from rv.
# ==============================================================================

server_results_summary <- function(input, output, session, rv) {

  # Build narrative paragraph from rv (used by UI and PDF download; do not call output$ from downloadHandler)
  narrative_paragraph <- function() {
    expr <- rv$batch_corrected
    if (is.null(expr)) expr <- rv$combined_expr
    n_genes_expr <- if (!is.null(expr)) nrow(expr) else 0L
    n_samp <- if (!is.null(expr)) ncol(expr) else 0L
    norm_ok <- isTRUE(rv$normalization_complete)
    batch_ok <- isTRUE(rv$batch_complete)
    n_sig <- if (!is.null(rv$sig_genes)) nrow(rv$sig_genes) else 0L
    n_up <- if (!is.null(rv$de_results)) sum(rv$de_results$Significance == "Up-regulated", na.rm = TRUE) else 0L
    n_down <- if (!is.null(rv$de_results)) sum(rv$de_results$Significance == "Down-regulated", na.rm = TRUE) else 0L
    mods <- rv$significant_modules
    n_mods <- if (is.null(mods)) 0L else length(mods)
    n_common <- if (is.null(rv$common_genes_de_wgcna)) 0L else length(rv$common_genes_de_wgcna)
    n_bp <- if (!is.null(rv$go_bp) && inherits(rv$go_bp, "enrichResult")) nrow(as.data.frame(rv$go_bp)) else 0L
    n_mf <- if (!is.null(rv$go_mf) && inherits(rv$go_mf, "enrichResult")) nrow(as.data.frame(rv$go_mf)) else 0L
    n_cc <- if (!is.null(rv$go_cc) && inherits(rv$go_cc, "enrichResult")) nrow(as.data.frame(rv$go_cc)) else 0L
    kdf <- tryCatch(as.data.frame(rv$kegg_enrichment), error = function(e) NULL)
    n_kegg <- if (!is.null(kdf) && nrow(kdf) > 0) nrow(kdf) else 0L
    n_hub <- if (is.data.frame(rv$ppi_consensus_hubs)) nrow(rv$ppi_consensus_hubs) else (if (is.null(rv$ppi_consensus_hubs)) 0L else length(rv$ppi_consensus_hubs))
    n_inter <- if (is.null(rv$ppi_interactive_genes)) 0L else length(rv$ppi_interactive_genes)
    methods_run <- if (is.null(rv$ml_methods_run)) character(0) else rv$ml_methods_run
    n_ml <- if (is.null(rv$ml_common_genes)) 0L else length(rv$ml_common_genes)
    n_gsea <- if (is.null(rv$gsea_results_by_gene)) 0L else length(rv$gsea_results_by_gene)
    n_immune_samp <- if (!is.null(rv$immune_matrix)) nrow(rv$immune_matrix) else 0L
    n_cells <- if (!is.null(rv$immune_matrix)) length(setdiff(colnames(rv$immune_matrix), "SampleID")) else 0L
    immune_meth <- if (is.null(rv$immune_method)) "" else rv$immune_method
    nomo_ok <- isTRUE(rv$nomogram_complete)
    train_auc <- if (!is.null(rv$nomogram_train_metrics) && "AUC" %in% names(rv$nomogram_train_metrics)) round(rv$nomogram_train_metrics$AUC, 3) else NA
    val_auc <- if (!is.null(rv$nomogram_val_metrics) && "AUC" %in% names(rv$nomogram_val_metrics)) round(rv$nomogram_val_metrics$AUC, 3) else NA
    thresh <- if (!is.null(rv$nomogram_optimal_threshold)) round(rv$nomogram_optimal_threshold, 3) else NA

    s1 <- sprintf("This analysis pipeline processed %s genes across %s samples. ", format(n_genes_expr, big.mark = ","), format(n_samp, big.mark = ","))
    s2 <- if (norm_ok) "Normalization (Step 3) was applied; " else "Normalization was not run. "
    s3 <- if (batch_ok) "batch correction (Step 5) was performed to reduce technical variation, as shown in the before/after PCA. " else "Batch correction was not run. "
    s4 <- sprintf("Differential expression (limma) identified %s significant genes (%s up-regulated, %s down-regulated), summarized in the volcano plot and top-gene heatmap. ", format(n_sig, big.mark = ","), format(n_up, big.mark = ","), format(n_down, big.mark = ","))
    s5 <- sprintf("WGCNA yielded %s significant modules; the soft-threshold plot, sample clustering tree, gene dendrogram, and module-trait heatmap are shown. ", format(n_mods, big.mark = ","))
    s6 <- sprintf("The intersection of DEGs and WGCNA genes gave %s common genes, used for GO/KEGG enrichment (%s BP, %s MF, %s CC terms; %s KEGG pathways) and for PPI network analysis (%s hub genes, %s interactive genes). ", format(n_common, big.mark = ","), n_bp, n_mf, n_cc, n_kegg, format(n_hub, big.mark = ","), format(n_inter, big.mark = ","))
    s7 <- if (length(methods_run) > 0) sprintf("Machine learning (%s) was run; the Venn/UpSet plot shows overlap across methods, with %s genes common to all selected methods. ", paste(methods_run, collapse = ", "), format(n_ml, big.mark = ",")) else "Machine learning was not run. "
    s8 <- if (nomo_ok && !is.na(train_auc) && !is.na(val_auc)) sprintf("The diagnostic nomogram (70/30 split-sample validation) achieved training AUC %s and validation AUC %s (optimal threshold %s). ", train_auc, val_auc, thresh) else if (n_ml > 0) "ROC analysis (Step 11) and the diagnostic nomogram (Step 12) are available when run. "
    s9 <- sprintf("GSEA was performed for %s target gene(s). ", format(n_gsea, big.mark = ","))
    s10 <- if (n_immune_samp > 0) sprintf("Immune cell deconvolution (%s) estimated proportions for %s cell types across %s samples. ", immune_meth, n_cells, format(n_immune_samp, big.mark = ",")) else "Immune deconvolution was not run. "

    paste0(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10)
  }

  # ----- Narrative writeup (one paragraph, generalized from rv) -----
  output$results_summary_narrative <- renderUI({
    para <- narrative_paragraph()
    tags$div(
      style = "font-size: 14px; line-height: 1.7; color: #333; text-align: justify; padding: 12px 0;",
      tags$p(para)
    )
  })

  # ----- Cite this analysis helper -----
  output$citation_text <- renderUI({
    limma_cite <- tryCatch(
      paste(capture.output(utils::citation("limma")), collapse = "\n"),
      error = function(e) "limma citation not available in this session."
    )
    tags$div(
      tags$h4("If you use GExPipe in your work, please cite:"),
      tags$p("GExPipe (Gene Expression Pipeline) v0.1 (please add your own manuscript or DOI when available)."),
      tags$p("Key methods and packages: limma, WGCNA, clusterProfiler, STRINGdb, pROC, rms, rmda, immunedeconv (if used)."),
      tags$p(tags$strong("Example citation for limma:"), style = "margin-top: 8px;"),
      tags$pre(limma_cite, style = "font-size: 11px; max-height: 220px; overflow-y: auto; background: #f8f9fa; border: 1px solid #ddd; padding: 8px;")
    )
  })

  # ----- Batch effect: before vs after PCA -----
  output$results_summary_batch_before_after <- renderPlot({
    before <- rv$expr_filtered
    after <- rv$batch_corrected
    meta <- rv$unified_metadata
    if (is.null(before) || is.null(after) || is.null(meta)) {
      plot.new(); text(0.5, 0.5, "Batch correction data needed (Step 5).", cex = 1, col = "gray40"); return(invisible(NULL))
    }
    common <- intersect(colnames(before), colnames(after))
    common <- intersect(common, rownames(meta))
    if (length(common) < 3) { plot.new(); text(0.5, 0.5, "Too few common samples.", cex = 1); return(invisible(NULL)) }
    before <- before[, common]; after <- after[, common]; meta <- meta[common, , drop = FALSE]
    cond <- if ("Condition" %in% names(meta)) meta$Condition else meta[[1]]
    par(mfrow = c(1, 2), mar = c(3, 3, 2, 1))
    pca_b <- prcomp(t(before), scale. = TRUE, center = TRUE)
    plot(pca_b$x[, 1], pca_b$x[, 2], col = as.numeric(factor(cond)), pch = 19, main = "Before batch correction", xlab = "PC1", ylab = "PC2")
    pca_a <- prcomp(t(after), scale. = TRUE, center = TRUE)
    plot(pca_a$x[, 1], pca_a$x[, 2], col = as.numeric(factor(cond)), pch = 19, main = "After batch correction", xlab = "PC1", ylab = "PC2")
  }, height = 260)

  # ----- ROC curve (one representative curve from common genes) -----
  output$results_summary_roc_plot <- renderPlot({
    if (is.null(rv$ml_common_genes) || length(rv$ml_common_genes) == 0 || is.null(rv$extracted_data_ml) || is.null(rv$wgcna_sample_info)) {
      plot.new(); text(0.5, 0.5, "Run ML (Step 10) and ROC (Step 11) for AUC curves.", cex = 1, col = "gray40"); return(invisible(NULL))
    }
    expr_mat <- as.matrix(rv$extracted_data_ml)
    sample_info <- rv$wgcna_sample_info
    common_samples <- intersect(rownames(expr_mat), rownames(sample_info))
    if (length(common_samples) < 3) { plot.new(); text(0.5, 0.5, "Too few samples.", cex = 1); return(invisible(NULL)) }
    expr_mat <- expr_mat[common_samples, , drop = FALSE]
    sample_info <- sample_info[common_samples, , drop = FALSE]
    cond_col <- if ("Condition" %in% names(sample_info)) "Condition" else names(sample_info)[1]
    y_binary <- as.integer(sample_info[[cond_col]] %in% c("Disease", "disease", "2"))
    if (length(unique(y_binary)) < 2) { plot.new(); text(0.5, 0.5, "Need two groups.", cex = 1); return(invisible(NULL)) }
    genes_avail <- intersect(rv$ml_common_genes, colnames(expr_mat))
    if (length(genes_avail) == 0) { plot.new(); text(0.5, 0.5, "No common genes in data.", cex = 1); return(invisible(NULL)) }
    sig_expr <- rowMeans(expr_mat[, genes_avail, drop = FALSE])
    roc_obj <- tryCatch(pROC::roc(y_binary, sig_expr, quiet = TRUE), error = function(e) NULL)
    if (is.null(roc_obj)) { plot.new(); text(0.5, 0.5, "ROC failed.", cex = 1); return(invisible(NULL)) }
    plot(roc_obj, col = "#3498db", lwd = 2, main = "ROC (mean signature of common ML genes)", legacy.axes = TRUE)
    abline(0, 1, lty = 2, col = "grey60")
    auc_val <- round(as.numeric(pROC::auc(roc_obj)), 3)
    text(0.6, 0.3, sprintf("AUC = %s", auc_val), cex = 1.2, col = "#3498db", font = 2)
  }, height = 260)

  # ----- Nomogram summary -----
  output$results_summary_nomogram_ui <- renderUI({
    if (!isTRUE(rv$nomogram_complete)) return(tags$p(style = "color: #7f8c8d;", "Run Diagnostic Nomogram (Step 12) to see summary here."))
    train_m <- rv$nomogram_train_metrics
    val_m <- rv$nomogram_val_metrics
    n_pred <- length(if (!is.null(rv$nomogram_available_genes)) rv$nomogram_available_genes else character(0))
    thresh <- rv$nomogram_optimal_threshold
    tags$div(
      tags$p(tags$strong("Predictors:"), n_pred, " | ", tags$strong("Threshold:"), if (!is.null(thresh)) round(thresh, 3) else "—"),
      tags$p(tags$strong("Training AUC:"), if (!is.null(train_m$AUC)) round(train_m$AUC, 3) else "—", " | ", tags$strong("Validation AUC:"), if (!is.null(val_m$AUC)) round(val_m$AUC, 3) else "—"),
      tags$p(tags$strong("Training Accuracy:"), if (!is.null(train_m$Accuracy)) round(train_m$Accuracy, 3) else "—", " | ", tags$strong("Validation Accuracy:"), if (!is.null(val_m$Accuracy)) round(val_m$Accuracy, 3) else "—")
    )
  })

  output$results_summary_nomogram_plot <- renderPlot({
    if (!isTRUE(rv$nomogram_complete) || is.null(rv$nomogram_model) || is.null(rv$nomogram_train_data) || is.null(rv$nomogram_available_genes) || length(rv$nomogram_available_genes) == 0) {
      plot.new(); text(0.5, 0.5, "Run Nomogram (Step 12) to see plot.", cex = 1, col = "gray40"); return(invisible(NULL))
    }
    tryCatch({
      genes <- rv$nomogram_available_genes
      train_df <- rv$nomogram_train_data[, c(genes, "Outcome"), drop = FALSE]
      dd <- rms::datadist(train_df)
      options(datadist = "dd")
      on.exit(options(datadist = NULL), add = TRUE)
      np <- rms::nomogram(rv$nomogram_model, fun = plogis, fun.at = c(0.2, 0.5, 0.8), funlabel = "Risk", lp = FALSE)
      plot(np)
      title(main = "Diagnostic Nomogram", cex.main = 1, font.main = 2)
    }, error = function(e) { plot.new(); text(0.5, 0.5, paste("Plot error:", conditionMessage(e)), cex = 0.8, col = "red") })
  }, height = 260)

  # ----- 1. Input genes & pipeline -----
  output$results_summary_input_genes <- renderUI({
    n_genes <- if (is.null(rv$common_genes)) 0L else length(rv$common_genes)
    expr <- rv$batch_corrected
    if (is.null(expr)) expr <- rv$combined_expr
    n_samp <- if (!is.null(expr)) ncol(expr) else 0L
    n_genes_expr <- if (!is.null(expr)) nrow(expr) else 0L
    tags$div(
      tags$p(tags$strong("Common input genes (after pipeline):"), format(n_genes, big.mark = ",")),
      tags$p(tags$strong("Expression matrix:"), sprintf("%s genes × %s samples", format(n_genes_expr, big.mark = ","), format(n_samp, big.mark = ","))),
      if (n_genes == 0 && n_genes_expr == 0) tags$p(style = "color: #7f8c8d;", "Complete Steps 1–5 to see input summary here.")
    )
  })

  # ----- 2. Normalization & batch -----
  output$results_summary_norm_batch <- renderUI({
    norm_ok <- isTRUE(rv$normalization_complete)
    tags$div(
      tags$p(tags$strong("Normalization:"), if (norm_ok) "Applied (Step 3)" else "Not run"),
      if (!norm_ok) tags$p(style = "color: #7f8c8d;", "Complete Step 3 to see normalization here.")
    )
  })

  output$results_summary_batch_only <- renderUI({
    batch_ok <- isTRUE(rv$batch_complete)
    tags$div(
      tags$p(tags$strong("Batch correction:"), if (batch_ok) "Applied (Step 5)" else "Not run"),
      if (!batch_ok) tags$p(style = "color: #7f8c8d;", "Complete Step 5 to see batch correction here.")
    )
  })

  # ----- 3. DE -----
  output$results_summary_de <- renderUI({
    sig <- rv$sig_genes
    de <- rv$de_results
    n_sig <- if (!is.null(sig)) nrow(sig) else 0L
    n_up <- if (!is.null(de)) sum(de$Significance == "Up-regulated", na.rm = TRUE) else 0L
    n_down <- if (!is.null(de)) sum(de$Significance == "Down-regulated", na.rm = TRUE) else 0L
    tags$div(
      tags$p(tags$strong("DE method:"), "limma"),
      tags$p(tags$strong("Significant genes (DEG):"), format(n_sig, big.mark = ",")),
      tags$p(tags$strong("Up-regulated:"), format(n_up, big.mark = ","), " | ", tags$strong("Down-regulated:"), format(n_down, big.mark = ",")),
      if (n_sig == 0) tags$p(style = "color: #7f8c8d;", "Run DE analysis (Step 6) to see results here.")
    )
  })

  output$results_summary_volcano <- renderPlot({
    if (is.null(rv$de_results)) { plot.new(); text(0.5, 0.5, "Run DE analysis (Step 6) to see volcano plot.", cex = 1.1); return(invisible(NULL)) }
    tryCatch({
      volcano_data <- as.data.frame(rv$de_results, stringsAsFactors = FALSE)
      if (!"Gene" %in% names(volcano_data)) volcano_data$Gene <- rownames(rv$de_results)
      if (!"Significance" %in% names(volcano_data)) volcano_data$Significance <- "Not Significant"
      volcano_data$Significance <- factor(as.character(volcano_data$Significance), levels = c("Not Significant", "Down-regulated", "Up-regulated"))
      min_p <- min(volcano_data$adj.P.Val[volcano_data$adj.P.Val > 0], na.rm = TRUE)
      volcano_data$adj.P.Val[volcano_data$adj.P.Val == 0] <- if (length(min_p) && is.finite(min_p)) min_p else 1e-300
      volcano_data$neg_log10_padj <- -log10(volcano_data$adj.P.Val)
      volcano_data <- volcano_data[is.finite(volcano_data$logFC) & is.finite(volcano_data$neg_log10_padj), ]
      logfc_cut <- if (is.numeric(input$logfc_cutoff)) input$logfc_cutoff else 1
      padj_cut <- if (is.numeric(input$padj_cutoff)) input$padj_cutoff else 0.05
      n_up <- sum(volcano_data$Significance == "Up-regulated", na.rm = TRUE)
      n_down <- sum(volcano_data$Significance == "Down-regulated", na.rm = TRUE)
      p <- ggplot2::ggplot(volcano_data, ggplot2::aes(x = logFC, y = neg_log10_padj, color = Significance)) +
        ggplot2::geom_point(alpha = 0.6, size = 2) +
        ggplot2::scale_color_manual(values = c("Up-regulated" = "#e74c3c", "Down-regulated" = "#3498db", "Not Significant" = "gray70"), name = "Significance") +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::labs(title = "Volcano Plot", subtitle = paste0("DEGs: ", n_up + n_down, " (Up: ", n_up, ", Down: ", n_down), x = "Log2 FC", y = "-Log10 adj.P.Val") +
        ggplot2::geom_hline(yintercept = -log10(padj_cut), linetype = "dashed", color = "gray40") +
        ggplot2::geom_vline(xintercept = c(-logfc_cut, logfc_cut), linetype = "dashed", color = "gray40")
      print(p)
    }, error = function(e) { plot.new(); text(0.5, 0.5, paste("Error:", conditionMessage(e)), cex = 0.9, col = "red") })
  }, height = 260)

  output$results_summary_de_heatmap <- renderPlot({
    if (is.null(rv$de_results) || is.null(rv$batch_corrected)) { plot.new(); text(0.5, 0.5, "Run DE analysis to see heatmap.", cex = 1.1); return(invisible(NULL)) }
    tryCatch({
      top_n <- min(20, nrow(rv$de_results))
      top <- head(rv$de_results[order(rv$de_results$adj.P.Val), ], top_n)
      gene_col <- if ("Gene" %in% names(top)) "Gene" else rownames(top)[1]
      genes <- if ("Gene" %in% names(top)) top$Gene else rownames(top)
      expr <- rv$batch_corrected[genes, , drop = FALSE]
      expr_scaled <- t(scale(t(expr)))
      samp <- colnames(expr_scaled)
      meta <- rv$unified_metadata
      idx <- match(samp, rownames(meta))
      if (any(is.na(idx)) && "SampleID" %in% names(meta)) idx <- match(samp, as.character(meta$SampleID))
      cond <- if ("Condition" %in% names(meta) && all(!is.na(idx))) meta$Condition[idx] else rep(NA_character_, length(samp))
      if (length(cond) != length(samp)) cond <- rep(NA_character_, length(samp))
      annot <- data.frame(Condition = cond, row.names = samp)
      pheatmap::pheatmap(expr_scaled, annotation_col = annot, color = colorRampPalette(c("#3498db", "white", "#e74c3c"))(100),
                         show_colnames = FALSE, fontsize_row = 8, main = paste0("Top ", top_n, " DE Genes"), border_color = NA)
    }, error = function(e) { plot.new(); text(0.5, 0.5, paste("Error:", conditionMessage(e)), cex = 0.9, col = "red") })
  }, height = 260)

  # ----- 4. WGCNA (simple HTML table) -----
  output$results_summary_wgcna <- renderUI({
    mods <- rv$significant_modules
    colors <- rv$moduleColors
    if (is.null(mods) || length(mods) == 0)
      return(tags$p(style = "color: #7f8c8d;", "Run WGCNA and identify significant modules to see list here."))
    if (is.data.frame(mods)) {
      df <- mods
      if (!"N_genes" %in% names(df) && "Size" %in% names(df))
        df$N_genes <- as.integer(df$Size)
    } else {
      color_from_me <- sub("^ME", "", names(mods))
      t <- table(colors)
      n_genes <- as.integer(t[match(color_from_me, names(t))])
      df <- data.frame(Module = names(mods), N_genes = n_genes, stringsAsFactors = FALSE)
    }
    tags$div(
      tags$p(tags$strong("Significant modules (with color):")),
      tags$table(
        class = "table table-bordered",
        tags$thead(tags$tr(tags$th("Module"), tags$th("N genes"))),
        tags$tbody(
          lapply(seq_len(min(20, nrow(df))), function(i) {
            r <- df[i, ]
            mod_name <- if ("Module" %in% names(r)) as.character(r$Module) else names(mods)[i]
            n_g <- if ("N_genes" %in% names(r)) r$N_genes else if ("Size" %in% names(r)) r$Size else NA_integer_
            n_str <- if (is.na(n_g)) "—" else format(as.integer(n_g), big.mark = ",")
            tags$tr(tags$td(mod_name), tags$td(n_str))
          })
        )
      ),
      if (nrow(df) > 20) tags$p(paste0("... and ", nrow(df) - 20, " more modules"))
    )
  })

  output$results_summary_soft_threshold <- renderPlot({
    if (is.null(rv$soft_threshold)) { plot.new(); text(0.5, 0.5, "Run soft threshold (Step 7) to see plot.", cex = 1.1); return(invisible(NULL)) }
    sft <- rv$soft_threshold
    powers <- if (!is.null(rv$soft_threshold_powers)) rv$soft_threshold_powers else sft$fitIndices[, 1]
    x1 <- sft$fitIndices[, 1]
    y1 <- -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2]
    y2 <- sft$fitIndices[, 5]
    op <- par(mfrow = c(1, 2), bg = "#f8fafb", fg = "#2c3e50", col.main = "#1a252f", col.axis = "#34495e", col.lab = "#34495e")
    on.exit(par(op), add = TRUE)
    plot(x1, y1, xlab = "Soft Threshold (power)", ylab = "signed R²", type = "n", main = "Scale independence")
    points(x1, y1, pch = 19, col = "#2980b9", cex = 1)
    text(x1, y1, labels = powers, cex = 0.75, col = "#1a252f", pos = 4, offset = 0.2)
    abline(h = 0.8, col = "#e74c3c", lty = 2, lwd = 1.2)
    box(col = "#dfe6e9", lwd = 1)
    plot(x1, y2, xlab = "Soft Threshold (power)", ylab = "Mean Connectivity", type = "n", main = "Mean connectivity")
    points(x1, y2, pch = 19, col = "#27ae60", cex = 1)
    text(x1, y2, labels = powers, cex = 0.75, col = "#1a252f", pos = 4, offset = 0.2)
    box(col = "#dfe6e9", lwd = 1)
  }, height = 260)

  output$results_summary_sample_tree <- renderPlot({
    if (is.null(rv$wgcna_sample_tree)) { plot.new(); text(0.5, 0.5, "Prepare WGCNA data to see sample tree.", cex = 1.1); return(invisible(NULL)) }
    ht <- rv$wgcna_sample_tree
    d <- as.dendrogram(ht)
    set_edge_par <- function(node) {
      if (is.leaf(node)) return(node)
      attr(node, "edgePar") <- list(col = "#2980b9", lwd = 1.2)
      node
    }
    d <- dendrapply(d, set_edge_par)
    op <- par(mar = c(2, 3.5, 3, 1), bg = "#f8fafb", fg = "#2c3e50", xpd = NA,
              plt = c(0.12, 0.98, 0.15, 0.88))
    on.exit(par(op), add = TRUE)
    plot(d, main = "Sample Clustering Tree", xlab = "", ylab = "Height", leaflab = "none", axes = FALSE)
    h_max <- max(ht$height)
    y_ticks <- pretty(c(0, h_max), n = 5)
    axis(2, at = y_ticks, col = "#5d6d7e", col.axis = "#2c3e50", cex.axis = 0.85, las = 1)
    box(col = "#dfe6e9", lwd = 1)
  }, width = 640, height = 260, res = 96)

  output$results_summary_wgcna_dendro <- renderPlot({
    if (is.null(rv$geneTree) || is.null(rv$moduleColors)) { plot.new(); text(0.5, 0.5, "Build WGCNA network to see dendrogram.", cex = 1.1); return(invisible(NULL)) }
    if (!requireNamespace("WGCNA", quietly = TRUE)) { par(bg = "white"); plot.new(); text(0.5, 0.5, "WGCNA required", cex = 1.2); return(invisible(NULL)) }
    op <- par(bg = "white", fg = "#2c3e50")
    on.exit(par(op), add = TRUE)
    WGCNA::plotDendroAndColors(rv$geneTree, rv$moduleColors, "Module Colors", main = "Gene Dendrogram and Module Colors", dendroLabels = FALSE, hang = 0.03, addGuide = TRUE, guideHang = 0.05)
  }, height = 260)

  output$results_summary_module_trait <- renderPlot({
    if (is.null(rv$moduleTraitCor) || is.null(rv$trait_data)) { par(bg = "white"); plot.new(); text(0.5, 0.5, "Calculate module-trait (Step 7) to see heatmap.", cex = 1); return(invisible(NULL)) }
    if (!requireNamespace("WGCNA", quietly = TRUE)) { par(bg = "white"); plot.new(); text(0.5, 0.5, "WGCNA required", cex = 1.2); return(invisible(NULL)) }
    op <- par(bg = "white", fg = "#2c3e50")
    on.exit(par(op), add = TRUE)
    WGCNA::labeledHeatmap(Matrix = rv$moduleTraitCor, xLabels = colnames(rv$trait_data), yLabels = rownames(rv$moduleTraitCor), ySymbols = rownames(rv$moduleTraitCor), colorLabels = FALSE, colors = WGCNA::blueWhiteRed(50), textMatrix = NULL, main = "Module-Trait Relationships")
  }, height = 260)

  # ----- 5. Common genes -----
  output$results_summary_common_genes <- renderUI({
    genes <- rv$common_genes_de_wgcna
    n <- if (is.null(genes)) 0L else length(genes)
    tags$div(
      tags$p(tags$strong("Common significant genes (DEG ∩ WGCNA):"), format(n, big.mark = ",")),
      if (n > 0) tags$p(paste(head(genes, 50), collapse = ", "), if (n > 50) paste0(" ... and ", n - 50, " more") else ""),
      if (n == 0) tags$p(style = "color: #7f8c8d;", "Run Common Genes (Step 8) to see the list here.")
    )
  })

  # ----- 6. GO & KEGG -----
  output$results_summary_go_kegg <- renderUI({
    n_bp <- if (!is.null(rv$go_bp) && inherits(rv$go_bp, "enrichResult")) nrow(as.data.frame(rv$go_bp)) else 0L
    n_mf <- if (!is.null(rv$go_mf) && inherits(rv$go_mf, "enrichResult")) nrow(as.data.frame(rv$go_mf)) else 0L
    n_cc <- if (!is.null(rv$go_cc) && inherits(rv$go_cc, "enrichResult")) nrow(as.data.frame(rv$go_cc)) else 0L
    kdf <- tryCatch(as.data.frame(rv$kegg_enrichment), error = function(e) NULL)
    n_kegg <- if (!is.null(kdf) && nrow(kdf) > 0) nrow(kdf) else 0L
    tags$div(
      tags$p(tags$strong("GO BP:"), n_bp, " | ", tags$strong("GO MF:"), n_mf, " | ", tags$strong("GO CC:"), n_cc, " | ", tags$strong("KEGG pathways:"), n_kegg),
      if (n_bp == 0 && n_mf == 0 && n_cc == 0 && n_kegg == 0) tags$p(style = "color: #7f8c8d;", "Run GO/KEGG enrichment (Step 8) to see results here.")
    )
  })

  output$results_summary_go_plot <- renderPlot({
    go_obj <- rv$go_bp
    if (is.null(go_obj)) go_obj <- rv$go_mf
    if (is.null(go_obj)) go_obj <- rv$go_cc
    if (is.null(go_obj) || !inherits(go_obj, "enrichResult")) {
      plot.new(); text(0.5, 0.5, "No GO enrichment results", cex = 1.2); return(invisible(NULL))
    }
    tryCatch({
      df <- as.data.frame(go_obj)
      if (nrow(df) == 0) { plot.new(); text(0.5, 0.5, "No significant GO terms", cex = 1.2); return(invisible(NULL)) }
      n_show <- min(15, nrow(df))
      enrichplot::dotplot(go_obj, showCategory = n_show, x = "GeneRatio", color = "p.adjust", size = "Count") +
        ggplot2::theme_minimal(base_size = 11) + ggplot2::ggtitle("GO Enrichment (Representative)")
    }, error = function(e) { plot.new(); text(0.5, 0.5, paste("Error:", conditionMessage(e)), cex = 0.9, col = "red") })
  }, height = 260)

  output$results_summary_kegg_plot <- renderPlot({
    k <- rv$kegg_enrichment
    if (is.null(k)) { plot.new(); text(0.5, 0.5, "No KEGG enrichment results", cex = 1.2); return(invisible(NULL)) }
    tryCatch({
      df <- as.data.frame(k)
      if (is.null(df) || nrow(df) == 0) { plot.new(); text(0.5, 0.5, "No significant KEGG pathways", cex = 1.2); return(invisible(NULL)) }
      n_show <- min(15, nrow(df))
      o <- order(as.numeric(df$p.adjust))
      df <- df[o, ][seq_len(n_show), , drop = FALSE]
      df$Description <- factor(df$Description, levels = rev(unique(as.character(df$Description))))
      if (!"Count" %in% names(df) && "geneID" %in% names(df))
        df$Count <- lengths(strsplit(as.character(df$geneID), "/", fixed = TRUE))
      if (!"Count" %in% names(df)) df$Count <- 1L
      ggplot2::ggplot(df, ggplot2::aes(x = Description, y = Count, fill = p.adjust)) +
        ggplot2::geom_col() + ggplot2::coord_flip() +
        ggplot2::scale_fill_continuous(low = "#E64B35", high = "#4DBBD5", name = "p.adjust") +
        ggplot2::theme_minimal(base_size = 11) + ggplot2::labs(title = "KEGG Enrichment", x = NULL, y = "Count")
    }, error = function(e) { plot.new(); text(0.5, 0.5, paste("Error:", conditionMessage(e)), cex = 0.9, col = "red") })
  }, height = 260)

  # ----- 7. PPI -----
  output$results_summary_ppi <- renderUI({
    hubs <- rv$ppi_consensus_hubs
    if (is.null(hubs)) hubs <- rv$ppi_hub_scores
    n_hub <- if (is.data.frame(hubs)) nrow(hubs) else (if (is.null(hubs)) 0L else length(hubs))
    n_inter <- if (is.null(rv$ppi_interactive_genes)) 0L else length(rv$ppi_interactive_genes)
    tags$div(
      tags$p(tags$strong("PPI hub genes:"), format(n_hub, big.mark = ",")),
      tags$p(tags$strong("Interactive genes (in PPI):"), format(n_inter, big.mark = ",")),
      if (n_hub == 0 && n_inter == 0) tags$p(style = "color: #7f8c8d;", "Run PPI (Step 9) to see results here.")
    )
  })

  output$results_summary_ppi_plot <- renderPlot({
    if (is.null(rv$ppi_graph)) { plot.new(); text(0.5, 0.5, "Run PPI (Step 9) to see network.", cex = 1.1); return(invisible(NULL)) }
    tryCatch({
      g <- rv$ppi_graph
      n_max <- min(40, igraph::vcount(g))
      if (n_max < 2) { plot.new(); text(0.5, 0.5, "Too few nodes for PPI plot.", cex = 1.1); return(invisible(NULL)) }
      if (!"degree" %in% names(igraph::vertex_attr(g))) igraph::V(g)$degree <- igraph::degree(g)
      ord <- order(igraph::V(g)$degree, decreasing = TRUE)[seq_len(n_max)]
      g_sub <- igraph::induced_subgraph(g, ord)
      layout_fr <- igraph::layout_with_fr(g_sub, niter = 500)
      op <- par(mar = c(2, 2, 3, 2), bg = "#FAFAFA")
      on.exit(par(op), add = TRUE)
      vlab <- igraph::V(g_sub)$SYMBOL; if (is.null(vlab)) vlab <- igraph::V(g_sub)$name
      igraph::plot.igraph(g_sub, layout = layout_fr, vertex.label = vlab,
                          vertex.label.cex = 0.8, vertex.size = 8, vertex.color = "#4DBBD5", edge.color = "gray70",
                          main = "PPI network (top genes by degree)")
    }, error = function(e) { plot.new(); text(0.5, 0.5, paste("Error:", conditionMessage(e)), cex = 0.9, col = "red") })
  }, height = 260)

  # ----- 8. ML & Venn -----
  output$results_summary_ml <- renderUI({
    common <- rv$ml_common_genes
    if (is.null(common)) common <- rv$ml_final_list
    n_common <- if (is.data.frame(common)) nrow(common) else (if (is.null(common)) 0L else length(common))
    if (is.character(common)) n_common <- length(common)
    tags$div(
      tags$p(tags$strong("Common genes across ML methods:"), format(n_common, big.mark = ",")),
      if (n_common == 0) tags$p(style = "color: #7f8c8d;", "Run ML (Step 10) to see Venn and common list here.")
    )
  })

  # Venn/UpSet drawn in-memory only; no file saved until user clicks Download (ML tab)
  output$results_summary_ml_venn <- renderPlot({
    if (is.null(rv$ml_venn_sets) || length(rv$ml_venn_sets) < 2) {
      plot.new()
      text(0.5, 0.5, "Run ML (Step 10) with 2+ methods to see Venn/UpSet here.", cex = 1, col = "gray40")
      return(invisible())
    }
    sets <- rv$ml_venn_sets
    n_sets <- length(sets)
    ml_venn_fill <- c("#E53935", "#1E88E5", "#43A047", "#FB8C00", "#8E24AA", "#009688", "#795548", "#607D8B")
    if (n_sets > 5) {
      all_genes <- unique(unlist(sets, use.names = FALSE))
      if (length(all_genes) == 0) { plot.new(); text(0.5, 0.5, "No genes", cex = 1, col = "gray40"); return(invisible()) }
      upset_matrix <- matrix(0L, nrow = length(all_genes), ncol = n_sets)
      rownames(upset_matrix) <- all_genes
      colnames(upset_matrix) <- names(sets)
      for (i in seq_len(n_sets)) {
        g <- sets[[i]]
        upset_matrix[intersect(all_genes, g), i] <- 1L
      }
      upset_df <- as.data.frame(upset_matrix)
      max_set_size <- max(sapply(sets, length))
      tryCatch({
        UpSetR::upset(upset_df, sets = colnames(upset_df), keep.order = TRUE, order.by = "freq",
                      main.bar.color = "#3498db", sets.bar.color = ml_venn_fill[seq_len(n_sets)],
                      matrix.color = "#2ecc71", point.size = 3, line.size = 0.8,
                      text.scale = c(1.3, 1.1, 1.1, 1, 1.3, 1.1), mb.ratio = c(0.6, 0.4),
                      set_size.show = TRUE, set_size.scale_max = max(1, max_set_size * 1.1))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "UpSet error", cex = 1, col = "red") })
    } else {
      fill_colors <- ml_venn_fill[seq_len(n_sets)]
      cat_names <- names(sets)
      if (is.null(cat_names)) cat_names <- c("LASSO", "Elastic Net", "Ridge", "Random Forest", "SVM-RFE", "Boruta", "sPLS-DA", "XGBoost+SHAP")[seq_len(n_sets)]
      grid::grid.newpage()
      vp <- VennDiagram::venn.diagram(x = sets, category.names = cat_names, filename = NULL, output = TRUE,
                                      disable.logging = TRUE, fill = fill_colors, alpha = 0.65, cex = 1, main = "ML methods Venn", main.cex = 1.2)
      grid::grid.draw(vp)
    }
  }, height = 260)

  # ----- 9. GSEA -----
  output$results_summary_gsea <- renderUI({
    by_gene <- rv$gsea_results_by_gene
    n_genes <- if (is.null(by_gene)) 0L else length(by_gene)
    n_pathways <- 0L
    if (length(by_gene) > 0) {
      n_pathways <- sum(sapply(by_gene, function(x) {
        if (inherits(x, "enrichResult")) nrow(as.data.frame(x)) else 0L
      }))
    }
    tags$div(
      tags$p(tags$strong("GSEA target genes:"), format(n_genes, big.mark = ",")),
      tags$p(tags$strong("Total pathways reported:"), format(n_pathways, big.mark = ",")),
      if (n_genes == 0) tags$p(style = "color: #7f8c8d;", "Run GSEA (Step 12) to see results here.")
    )
  })

  output$results_summary_gsea_plot <- renderPlot({
    by_gene <- rv$gsea_results_by_gene
    if (is.null(by_gene) || length(by_gene) == 0) { plot.new(); text(0.5, 0.5, "Run GSEA (Step 12) to see enrichment plot.", cex = 1); return(invisible(NULL)) }
    result <- by_gene[[1]]
    if (!inherits(result, "enrichResult")) { plot.new(); text(0.5, 0.5, "No GSEA result for first gene.", cex = 1); return(invisible(NULL)) }
    tryCatch({
      df <- as.data.frame(result)
      if (is.null(df) || nrow(df) == 0) { plot.new(); text(0.5, 0.5, "No pathways for first gene.", cex = 1); return(invisible(NULL)) }
      gene_name <- names(by_gene)[1]
      p <- enrichplot::gseaplot2(result, geneSetID = 1, title = paste0("GSEA: ", gene_name))
      print(p)
    }, error = function(e) { plot.new(); text(0.5, 0.5, paste("Error:", conditionMessage(e)), cex = 0.9, col = "red") })
  }, height = 260)

  # ----- 10. Immune -----
  output$results_summary_immune <- renderUI({
    M <- rv$immune_matrix
    if (is.null(M) || nrow(M) == 0) return(tags$p(style = "color: #7f8c8d;", "Run Immune deconvolution (Step 13) to see results here."))
    n_samp <- nrow(M)
    cell_cols <- setdiff(colnames(M), "SampleID")
    n_cells <- length(cell_cols)
    tags$div(
      tags$p(tags$strong("Samples:"), format(n_samp, big.mark = ","), " | ", tags$strong("Cell types:"), format(n_cells, big.mark = ",")),
      if (n_cells > 0) tags$p(paste(cell_cols, collapse = ", "))
    )
  })

  output$results_summary_immune_plot <- renderPlot({
    M <- rv$immune_matrix
    cols <- rv$immune_cell_cols
    if (is.null(cols) && !is.null(M)) cols <- setdiff(colnames(M), "SampleID")
    if (is.null(M) || nrow(M) == 0 || length(cols) < 2) {
      plot.new()
      text(0.5, 0.5, "No immune cell correlation heatmap (run Step 13).", cex = 1.1, col = "gray40")
      return(invisible(NULL))
    }
    mat_raw <- M[, cols, drop = FALSE]
    # Convert to numeric and clean infinities/NA
    mat_num <- as.data.frame(lapply(mat_raw, function(x) as.numeric(as.character(x))))
    rownames(mat_num) <- if (!is.null(M$SampleID)) M$SampleID else seq_len(nrow(M))
    if (!all(is.finite(as.matrix(mat_num)))) {
      m <- as.matrix(mat_num)
      m[!is.finite(m)] <- NA_real_
      mat_num <- as.data.frame(m)
    }
    # Correlation between immune cell types (same as Immune tab)
    cr <- tryCatch(
      cor(mat_num, method = "spearman", use = "pairwise.complete.obs"),
      error = function(e) NULL
    )
    if (is.null(cr) || nrow(cr) == 0 || ncol(cr) == 0) {
      plot.new()
      text(0.5, 0.5, "Not enough immune data to compute correlations.", cex = 1, col = "gray40")
      return(invisible(NULL))
    }
    if (any(is.na(cr))) cr[is.na(cr)] <- 0
    heatmap_colors <- colorRampPalette(c("#2166AC", "white", "#B2182B"))(100)
    tryCatch(
      {
        pheatmap::pheatmap(
          cr,
          color = heatmap_colors,
          clustering_distance_rows = "euclidean",
          clustering_distance_cols = "euclidean",
          main = "Immune Cell Correlation Heatmap",
          fontsize_row = 7,
          fontsize_col = 8,
          border_color = NA
        )
      },
      error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", conditionMessage(e)), cex = 0.9, col = "red")
      }
    )
  }, height = 260)
}
