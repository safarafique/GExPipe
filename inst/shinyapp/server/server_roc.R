# ==============================================================================
# SERVER_ROC.R - ROC Curve Analysis for Common ML Genes
# ==============================================================================
# Uses rv$ml_common_genes, rv$extracted_data_ml, rv$wgcna_sample_info.
# Computes AUC per gene; keeps only biomarkers with AUC >= AUC_MIN (e.g. 0.8).
# Plots ROC curves and boxplots for the filtered set.
# When external validation data exists (from Step 11), also computes external ROC.
# ==============================================================================

AUC_MIN <- 0.8

server_roc <- function(input, output, session, rv) {

  # ---- Validation mode indicator ----
  output$roc_validation_mode_ui <- renderUI({
    mode <- rv$validation_mode
    if (is.null(mode)) mode <- "internal"
    if (mode == "external") {
      has_ext <- !is.null(rv$external_validation_expr)
      tags$div(
        class = if (has_ext) "alert alert-success" else "alert alert-warning",
        style = "margin-bottom: 15px;",
        icon(if (has_ext) "check-circle" else "exclamation-triangle"),
        tags$strong(if (has_ext) " External Validation Mode -- Active" else " External Validation Mode"),
        if (has_ext) {
          paste0(" | Validation dataset loaded: ", nrow(rv$external_validation_expr), " samples. ",
                 "ROC/AUC is computed on the validation data for your ML common genes (shown below), ",
                 "with training data ROC shown further down for reference.")
        } else {
          " | Go back to Step 11 to load an external validation dataset. ROC below uses training data only."
        }
      )
    } else {
      tags$div(
        class = "alert alert-info",
        style = "margin-bottom: 15px;",
        icon("info-circle"),
        tags$strong(" Internal Validation Mode. "),
        "ROC/AUC computed on training data only. For an unbiased assessment, go to Step 11 and load an external validation dataset."
      )
    }
  })

  output$roc_placeholder_ui <- renderUI({
    if (!is.null(rv$ml_common_genes) && length(rv$ml_common_genes) > 0) return(NULL)
    tags$div(
      class = "alert alert-warning",
      icon("hand-point-right"),
      " Run Step 10 (Machine Learning) and ensure common genes exist. Then return here to view AUC and ROC curves."
    )
  })

  # Reactive: ROC/AUC results for common genes (training data)
  roc_results <- reactive({
    req(rv$ml_common_genes, rv$extracted_data_ml, rv$wgcna_sample_info)
    common_genes <- rv$ml_common_genes
    expr_mat <- as.matrix(rv$extracted_data_ml)
    sample_info <- rv$wgcna_sample_info
    if (length(common_genes) == 0 || nrow(expr_mat) < 3) return(NULL)
    common_samples <- intersect(rownames(expr_mat), rownames(sample_info))
    if (length(common_samples) < 3) return(NULL)
    expr_mat <- expr_mat[common_samples, , drop = FALSE]
    sample_info <- sample_info[common_samples, , drop = FALSE]
    cond_col <- if ("Condition" %in% names(sample_info)) "Condition" else names(sample_info)[1]
    grp <- sample_info[[cond_col]]
    y_binary <- as.integer(grp %in% c("Disease", "disease", "2"))
    if (length(unique(y_binary)) < 2) return(NULL)
    available_genes <- intersect(common_genes, colnames(expr_mat))
    if (length(available_genes) == 0) return(NULL)
    res <- lapply(available_genes, function(g) {
      ex <- expr_mat[, g]
      roc_obj <- tryCatch(pROC::roc(y_binary, ex, quiet = TRUE), error = function(e) NULL)
      auc_val <- if (!is.null(roc_obj)) as.numeric(pROC::auc(roc_obj)) else NA_real_
      list(Gene = g, AUC = auc_val, roc = roc_obj)
    })
    df <- data.frame(
      Gene = vapply(res, function(x) x$Gene, character(1)),
      AUC = vapply(res, function(x) x$AUC, numeric(1)),
      stringsAsFactors = FALSE
    )
    df <- df[!is.na(df$AUC), , drop = FALSE]
    df <- df[order(-df$AUC), , drop = FALSE]
    curves_all <- setNames(lapply(res, function(x) x$roc), vapply(res, function(x) x$Gene, character(1)))
    removed <- df[df$AUC < AUC_MIN, , drop = FALSE]
    df <- df[df$AUC >= AUC_MIN, , drop = FALSE]
    curves <- curves_all[names(curves_all) %in% df$Gene]
    list(
      df = df,
      curves = curves,
      n_removed = nrow(removed),
      removed_genes = if (nrow(removed) > 0) removed$Gene else character(0)
    )
  })

  output$roc_filter_message_ui <- renderUI({
    roc <- roc_results()
    if (is.null(roc)) return(NULL)
    n_removed <- roc$n_removed
    n_keep <- nrow(roc$df)
    if (n_removed == 0) {
      if (n_keep > 0) return(tags$p(icon("check-circle"), " All ", n_keep, " biomarker(s) have AUC >= ", AUC_MIN, ".", style = "color: #27ae60; margin-bottom: 8px;"))
      return(NULL)
    }
    msg <- paste0(n_removed, " biomarker(s) with AUC < ", AUC_MIN, " were removed. Proceeding with ", n_keep, " biomarker(s) (AUC >= ", AUC_MIN, ").")
    if (length(roc$removed_genes) > 0 && length(roc$removed_genes) <= 15) {
      msg <- paste0(msg, " Removed: ", paste(roc$removed_genes, collapse = ", "), ".")
    } else if (length(roc$removed_genes) > 15) {
      msg <- paste0(msg, " Removed: ", paste(head(roc$removed_genes, 15), collapse = ", "), ", ... (", length(roc$removed_genes), " total).")
    }
    tags$div(
      class = "alert alert-warning",
      style = "margin-bottom: 10px;",
      icon("filter"),
      tags$strong(" AUC filter (>= ", AUC_MIN, "): "),
      tags$span(msg)
    )
  })

  output$roc_auc_table <- DT::renderDataTable({
    roc <- roc_results()
    if (is.null(roc) || nrow(roc$df) == 0) return(NULL)
    roc$df$AUC <- round(roc$df$AUC, 4)
    DT::datatable(roc$df, options = list(pageLength = 20), rownames = FALSE)
  })

  roc_auc_plot_data <- reactive({
    roc <- roc_results()
    if (is.null(roc) || nrow(roc$df) == 0) return(NULL)
    df <- roc$df
    df$AUC <- round(df$AUC, 3)
    top_n <- min(15, nrow(df))
    df <- df[seq_len(top_n), , drop = FALSE]
    df$Gene <- factor(df$Gene, levels = rev(df$Gene))
    df
  })

  output$roc_auc_barplot <- renderPlot({
    df <- roc_auc_plot_data()
    if (is.null(df)) {
      plot.new()
      text(0.5, 0.5, "No biomarkers with AUC >= 0.8.", cex = 0.95, col = "gray40")
      return()
    }
    cols <- RColorBrewer::brewer.pal(max(3, min(8, nrow(df))), "Set2")
    cols <- rep(cols, length.out = nrow(df))
    op <- par(mar = c(5, 7, 3, 1), cex.main = 1, cex.lab = 0.95, cex.axis = 0.9)
    on.exit(par(op), add = TRUE)
    barplot(df$AUC, names.arg = df$Gene, horiz = TRUE, las = 1, col = rev(cols),
            xlim = c(0, 1), main = "Top AUC biomarkers", xlab = "AUC (>= 0.8)")
    abline(v = 0.8, col = "red", lty = 2)
  }, height = 320)

  output$download_roc_auc <- downloadHandler(
    filename = function() "ROC_AUC_scores.csv",
    content = function(file) {
      roc <- roc_results()
      if (is.null(roc) || nrow(roc$df) == 0) return()
      write.csv(roc$df, file, row.names = FALSE)
      write.csv(roc$df, file.path(CSV_EXPORT_DIR(), "ROC_AUC_scores.csv"), row.names = FALSE)
    }
  )

  output$download_roc_auc_plot_jpg <- downloadHandler(
    filename = function() "ROC_AUC_barplot.jpg",
    content = function(file) {
      df <- roc_auc_plot_data()
      if (is.null(df)) return()
      cols <- RColorBrewer::brewer.pal(max(3, min(8, nrow(df))), "Set2")
      cols <- rep(cols, length.out = nrow(df))
      jpeg(file, width = 7, height = 5, units = "in", res = IMAGE_DPI, quality = 95)
      op <- par(mar = c(5, 7, 3, 1), cex.main = 1, cex.lab = 0.95, cex.axis = 0.9)
      on.exit(par(op), add = TRUE)
      barplot(df$AUC, names.arg = df$Gene, horiz = TRUE, las = 1, col = rev(cols),
              xlim = c(0, 1), main = "Top AUC biomarkers", xlab = "AUC (>= 0.8)")
      abline(v = 0.8, col = "red", lty = 2)
      dev.off()
    }
  )

  output$download_roc_auc_plot_pdf <- downloadHandler(
    filename = function() "ROC_AUC_barplot.pdf",
    content = function(file) {
      df <- roc_auc_plot_data()
      if (is.null(df)) return()
      cols <- RColorBrewer::brewer.pal(max(3, min(8, nrow(df))), "Set2")
      cols <- rep(cols, length.out = nrow(df))
      pdf(file, width = 7, height = 5)
      op <- par(mar = c(5, 7, 3, 1), cex.main = 1, cex.lab = 0.95, cex.axis = 0.9)
      on.exit(par(op), add = TRUE)
      barplot(df$AUC, names.arg = df$Gene, horiz = TRUE, las = 1, col = rev(cols),
              xlim = c(0, 1), main = "Top AUC biomarkers", xlab = "AUC (>= 0.8)")
      abline(v = 0.8, col = "red", lty = 2)
      dev.off()
    }
  )

  # Shared color map for genes (training + validation) so colors are consistent
  roc_gene_colors <- reactive({
    roc <- roc_results()
    ext <- roc_external_results()
    genes_train <- if (!is.null(roc) && !is.null(roc$df) && nrow(roc$df) > 0) roc$df$Gene else character(0)
    genes_ext <- if (!is.null(ext) && !is.null(ext$df) && nrow(ext$df) > 0) ext$df$Gene else character(0)
    genes <- unique(c(genes_train, genes_ext))
    if (length(genes) == 0) return(character(0))
    base_cols <- c("#E53935", "#1E88E5", "#43A047", "#FB8C00", "#8E24AA",
                   "#00ACC1", "#7B1FA2", "#C62828", "#2E7D32", "#EF6C00")
    cols <- rep(base_cols, length.out = length(genes))
    stats::setNames(cols, genes)
  })

  # ============================================================================
  # Multi-variable ROC: combined biomarker panel (logistic regression)
  # ============================================================================
  roc_combined_panel <- reactive({
    roc <- roc_results()
    req(roc, rv$extracted_data_ml, rv$wgcna_sample_info)

    # Use genes that passed AUC filter (AUC >= AUC_MIN)
    panel_genes <- roc$df$Gene
    if (length(panel_genes) < 2) return(NULL)  # need at least 2 for a panel

    # Limit to top 10 by AUC to avoid overfitting / singular fits
    if (length(panel_genes) > 10) {
      panel_genes <- panel_genes[seq_len(10)]
    }

    expr_mat <- as.matrix(rv$extracted_data_ml)          # samples x genes
    sample_info <- rv$wgcna_sample_info
    common_samples <- intersect(rownames(expr_mat), rownames(sample_info))
    if (length(common_samples) < 10) return(NULL)

    expr_mat <- expr_mat[common_samples, , drop = FALSE]
    sample_info <- sample_info[common_samples, , drop = FALSE]
    genes <- intersect(panel_genes, colnames(expr_mat))
    if (length(genes) < 2) return(NULL)

    cond_col <- if ("Condition" %in% names(sample_info)) "Condition" else names(sample_info)[1]
    grp <- sample_info[[cond_col]]
    y_binary <- as.integer(grp %in% c("Disease", "disease", "2"))
    if (length(unique(y_binary)) < 2) return(NULL)

    df <- data.frame(group = y_binary, expr_mat[, genes, drop = FALSE], check.names = FALSE)
    formula_obj <- stats::as.formula(paste("group ~", paste(genes, collapse = " + ")))

    combined_model <- tryCatch(
      glm(formula_obj, data = df, family = "binomial"),
      error = function(e) NULL
    )
    if (is.null(combined_model)) return(NULL)

    pred <- stats::predict(combined_model, type = "response")
    roc_obj <- tryCatch(
      pROC::roc(df$group, pred, quiet = TRUE),
      error = function(e) NULL
    )
    if (is.null(roc_obj)) return(NULL)

    auc_val <- as.numeric(pROC::auc(roc_obj))

    # Best single gene (by AUC) for comparison
    best_gene <- roc$df$Gene[1]
    best_auc <- roc$df$AUC[1]
    best_curve <- roc$curves[[best_gene]]

    list(
      model = combined_model,
      roc = roc_obj,
      auc = auc_val,
      genes = genes,
      best_gene = best_gene,
      best_auc = best_auc,
      best_roc = best_curve
    )
  })

  output$roc_combined_panel_plot <- renderPlot({
    res <- roc_combined_panel()
    if (is.null(res)) {
      plot.new()
      text(0.5, 0.5, "Multi-variable ROC not available (need ≥ 2 panel genes with valid ROC).", cex = 0.9, col = "gray40")
      return()
    }

    combined_roc <- res$roc
    best_roc <- res$best_roc

    cols <- c("Combined panel" = "#E74C3C", "Best single gene" = "#3498DB")
    pROC::plot.roc(combined_roc, col = cols["Combined panel"], lwd = 2.5,
                   main = "Multi-variable ROC (Combined Biomarker Panel)", legacy.axes = TRUE)
    if (!is.null(best_roc)) {
      pROC::lines.roc(best_roc, col = cols["Best single gene"], lwd = 2, lty = 2)
    }
    abline(0, 1, lty = 2, col = "grey60")

    legend("bottomright",
           legend = c(
             paste0("Combined panel (AUC=", round(res$auc, 3), ")"),
             if (!is.null(best_roc)) paste0("Best single gene: ", res$best_gene, " (AUC=", round(res$best_auc, 3), ")") else NULL
           ),
           col = cols[seq_along(cols)], lwd = c(2.5, 2), lty = c(1, 2), bty = "n", cex = 0.9)
  }, height = 360)

  output$roc_combined_panel_table <- DT::renderDataTable({
    res <- roc_combined_panel()
    roc <- roc_results()
    if (is.null(res) || is.null(roc) || nrow(roc$df) == 0) return(NULL)

    df_single <- head(roc$df[order(-roc$df$AUC), c("Gene", "AUC"), drop = FALSE], 10)
    names(df_single) <- c("Feature", "AUC")
    df_single$Type <- "Single-gene"

    df_combined <- data.frame(
      Feature = paste0("Combined panel (", length(res$genes), " genes)"),
      AUC = res$auc,
      Type = "Multi-gene",
      stringsAsFactors = FALSE
    )

    df_out <- rbind(df_combined, df_single)
    df_out$AUC <- round(df_out$AUC, 4)
    df_out <- df_out[, c("Feature", "Type", "AUC")]

    DT::datatable(df_out, options = list(pageLength = 11, scrollX = TRUE), rownames = FALSE)
  })

  output$roc_curves_plot <- renderPlot({
    roc <- roc_results()
    if (is.null(roc) || nrow(roc$df) == 0) {
      plot.new()
      text(0.5, 0.5, "No biomarkers with AUC >= 0.8 (or no common genes). Run ML first; only genes with AUC >= 0.8 are shown.", cex = 0.95, col = "gray40")
      return()
    }
    curves <- roc$curves
    curves <- curves[!sapply(curves, is.null)]
    if (length(curves) == 0) {
      plot.new()
      text(0.5, 0.5, "No valid ROC curves.", cex = 1, col = "gray40")
      return()
    }
    n_plot <- min(10, length(curves))
    top_genes <- roc$df$Gene[seq_len(n_plot)]
    curves_plot <- curves[top_genes]
    curves_plot <- curves_plot[!sapply(curves_plot, is.null)]
    if (length(curves_plot) == 0) return()
    gene_cols <- roc_gene_colors()
    cols <- unname(gene_cols[names(curves_plot)])
    # Fallback palette if some colors are NA
    if (any(!nzchar(cols) | is.na(cols))) {
      base_cols <- c("#E53935", "#1E88E5", "#43A047", "#FB8C00", "#8E24AA",
                     "#00ACC1", "#7B1FA2", "#C62828", "#2E7D32", "#EF6C00")
      cols <- rep(base_cols, length.out = length(curves_plot))
    }
    op <- par(mar = c(4, 4, 3, 2), bg = "#FAFAFA")
    on.exit(par(op), add = TRUE)
    pROC::plot.roc(curves_plot[[1]], col = cols[1], lwd = 2, main = "ROC Curves (Common ML Genes)", cex.main = 1.2)
    for (i in seq_along(curves_plot)[-1]) {
      pROC::lines.roc(curves_plot[[i]], col = cols[i], lwd = 2)
    }
    auc_vals <- vapply(curves_plot, function(r) as.numeric(pROC::auc(r)), numeric(1))
    legend("bottomright", legend = paste0(names(curves_plot), " (AUC=", round(auc_vals, 3), ")"),
           col = cols[seq_along(curves_plot)], lwd = 2, cex = 0.85, bty = "n")
  }, width = 600, height = 500, res = 96)

  roc_curves_plot_data <- reactive({
    roc <- roc_results()
    if (is.null(roc)) return(NULL)
    curves <- roc$curves[!sapply(roc$curves, is.null)]
    n_plot <- min(10, length(curves))
    top_genes <- roc$df$Gene[seq_len(n_plot)]
    curves_plot <- curves[top_genes]
    curves_plot <- curves_plot[!sapply(curves_plot, is.null)]
    if (length(curves_plot) == 0) return(NULL)
    list(curves_plot = curves_plot)
  })

  output$download_roc_plot_jpg <- downloadHandler(
    filename = function() "ROC_curves_common_genes.jpg",
    content = function(file) {
      dat <- roc_curves_plot_data()
      if (is.null(dat)) return()
      curves_plot <- dat$curves_plot
      cols <- c("#E53935", "#1E88E5", "#43A047", "#FB8C00", "#8E24AA", "#00ACC1", "#7B1FA2", "#C62828", "#2E7D32", "#EF6C00")
      cols <- rep(cols, length.out = length(curves_plot))
      jpeg(file, width = 8, height = 6.67, res = IMAGE_DPI, units = "in", bg = "#FAFAFA", quality = 95)
      op <- par(mar = c(4, 4, 3, 2))
      on.exit(par(op), add = TRUE)
      pROC::plot.roc(curves_plot[[1]], col = cols[1], lwd = 2, main = "ROC Curves (Common ML Genes)", cex.main = 1.2)
      for (i in seq_along(curves_plot)[-1]) pROC::lines.roc(curves_plot[[i]], col = cols[i], lwd = 2)
      auc_vals <- vapply(curves_plot, function(r) as.numeric(pROC::auc(r)), numeric(1))
      legend("bottomright", legend = paste0(names(curves_plot), " (AUC=", round(auc_vals, 3), ")"),
             col = cols[seq_along(curves_plot)], lwd = 2, cex = 0.9, bty = "n")
      dev.off()
    }
  )

  output$download_roc_plot_pdf <- downloadHandler(
    filename = function() "ROC_curves_common_genes.pdf",
    content = function(file) {
      dat <- roc_curves_plot_data()
      if (is.null(dat)) return()
      curves_plot <- dat$curves_plot
      cols <- c("#E53935", "#1E88E5", "#43A047", "#FB8C00", "#8E24AA", "#00ACC1", "#7B1FA2", "#C62828", "#2E7D32", "#EF6C00")
      cols <- rep(cols, length.out = length(curves_plot))
      pdf(file, width = 8, height = 6.67, bg = "#FAFAFA")
      op <- par(mar = c(4, 4, 3, 2))
      on.exit(par(op), add = TRUE)
      pROC::plot.roc(curves_plot[[1]], col = cols[1], lwd = 2, main = "ROC Curves (Common ML Genes)", cex.main = 1.2)
      for (i in seq_along(curves_plot)[-1]) pROC::lines.roc(curves_plot[[i]], col = cols[i], lwd = 2)
      auc_vals <- vapply(curves_plot, function(r) as.numeric(pROC::auc(r)), numeric(1))
      legend("bottomright", legend = paste0(names(curves_plot), " (AUC=", round(auc_vals, 3), ")"),
             col = cols[seq_along(curves_plot)], lwd = 2, cex = 0.9, bty = "n")
      dev.off()
    }
  )

  # Boxplots
  roc_boxplot_data <- reactive({
    roc <- roc_results()
    if (is.null(roc) || nrow(roc$df) == 0) return(NULL)
    req(rv$extracted_data_ml, rv$wgcna_sample_info)
    expr_mat <- as.matrix(rv$extracted_data_ml)
    sample_info <- rv$wgcna_sample_info
    common_samples <- intersect(rownames(expr_mat), rownames(sample_info))
    if (length(common_samples) < 2) return(NULL)
    cond_col <- if ("Condition" %in% names(sample_info)) "Condition" else names(sample_info)[1]
    genes <- roc$df$Gene
    genes <- intersect(genes, colnames(expr_mat))
    if (length(genes) == 0) return(NULL)
    expr_sub <- expr_mat[common_samples, genes, drop = FALSE]
    grp <- sample_info[common_samples, cond_col]
    wide <- data.frame(SampleID = common_samples, Group = as.character(grp), expr_sub, check.names = FALSE)
    long <- tidyr::pivot_longer(wide, cols = dplyr::all_of(genes), names_to = "Gene", values_to = "Expression")
    long$Gene <- factor(long$Gene, levels = genes)
    long
  })

  output$roc_boxplots_plot <- renderPlot({
    long <- roc_boxplot_data()
    if (is.null(long) || nrow(long) == 0) {
      plot.new()
      text(0.5, 0.5, "No data for boxplots.", cex = 1, col = "gray40")
      return()
    }
    n_genes <- length(unique(long$Gene))
    if (n_genes > 12) long <- long[long$Gene %in% levels(long$Gene)[seq_len(12)], , drop = FALSE]
    grp_levels <- unique(long$Group)
    fill_colors <- if (length(grp_levels) >= 2) {
      setNames(c("#43A047", "#E53935")[seq_along(grp_levels)], grp_levels)
    } else {
      setNames("#43A047", grp_levels[1])
    }
    p <- ggplot2::ggplot(long, ggplot2::aes(x = Group, y = Expression, fill = Group)) +
      ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.85) +
      ggplot2::geom_jitter(width = 0.15, size = 1, alpha = 0.5) +
      ggplot2::facet_wrap(~Gene, scales = "free_y", ncol = min(4, n_genes)) +
      ggplot2::scale_fill_manual(values = fill_colors) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(legend.position = "top", axis.title.x = ggplot2::element_blank()) +
      ggplot2::labs(y = "Expression", title = "Gene Expression: Normal vs Disease")
    print(p)
  }, width = 700, height = 400, res = 96)

  output$download_roc_boxplots_jpg <- downloadHandler(
    filename = function() "ROC_gene_expression_boxplots.jpg",
    content = function(file) {
      long <- roc_boxplot_data()
      if (is.null(long) || nrow(long) == 0) return()
      n_genes <- length(unique(long$Gene))
      if (n_genes > 12) long <- long[long$Gene %in% levels(long$Gene)[seq_len(12)], , drop = FALSE]
      grp_levels <- unique(long$Group)
      fill_colors <- if (length(grp_levels) >= 2) setNames(c("#43A047", "#E53935")[seq_along(grp_levels)], grp_levels) else setNames("#43A047", grp_levels[1])
      p <- ggplot2::ggplot(long, ggplot2::aes(x = Group, y = Expression, fill = Group)) +
        ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.85) +
        ggplot2::geom_jitter(width = 0.15, size = 1, alpha = 0.5) +
        ggplot2::facet_wrap(~Gene, scales = "free_y", ncol = min(4, n_genes)) +
        ggplot2::scale_fill_manual(values = fill_colors) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(legend.position = "top", axis.title.x = ggplot2::element_blank()) +
        ggplot2::labs(y = "Expression", title = "Gene Expression: Normal vs Disease")
      ggplot2::ggsave(file, plot = p, width = max(8, 2.5 * min(4, n_genes)), height = 6, dpi = IMAGE_DPI, units = "in", bg = "white", device = "jpeg")
    }
  )

  output$download_roc_boxplots_pdf <- downloadHandler(
    filename = function() "ROC_gene_expression_boxplots.pdf",
    content = function(file) {
      long <- roc_boxplot_data()
      if (is.null(long) || nrow(long) == 0) return()
      n_genes <- length(unique(long$Gene))
      if (n_genes > 12) long <- long[long$Gene %in% levels(long$Gene)[seq_len(12)], , drop = FALSE]
      grp_levels <- unique(long$Group)
      fill_colors <- if (length(grp_levels) >= 2) setNames(c("#43A047", "#E53935")[seq_along(grp_levels)], grp_levels) else setNames("#43A047", grp_levels[1])
      p <- ggplot2::ggplot(long, ggplot2::aes(x = Group, y = Expression, fill = Group)) +
        ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.85) +
        ggplot2::geom_jitter(width = 0.15, size = 1, alpha = 0.5) +
        ggplot2::facet_wrap(~Gene, scales = "free_y", ncol = min(4, n_genes)) +
        ggplot2::scale_fill_manual(values = fill_colors) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(legend.position = "top", axis.title.x = ggplot2::element_blank()) +
        ggplot2::labs(y = "Expression", title = "Gene Expression: Normal vs Disease")
      ggplot2::ggsave(file, plot = p, width = max(8, 2.5 * min(4, n_genes)), height = 6, device = "pdf", bg = "white")
    }
  )

  # ============================================================================
  # EXTERNAL VALIDATION ROC — Per-gene ROC on external dataset
  # ============================================================================
  roc_external_results <- reactive({
    req(rv$external_validation_expr, rv$external_validation_outcome, rv$ml_common_genes)
    ext_expr <- rv$external_validation_expr
    ext_outcome <- rv$external_validation_outcome
    common_genes <- rv$ml_common_genes
    if (length(common_genes) == 0 || nrow(ext_expr) < 3) return(NULL)
    if (length(unique(ext_outcome)) < 2) return(NULL)

    available_genes <- intersect(common_genes, colnames(ext_expr))
    if (length(available_genes) == 0) return(NULL)

    res <- lapply(available_genes, function(g) {
      ex <- ext_expr[, g]
      roc_obj <- tryCatch(pROC::roc(ext_outcome, ex, quiet = TRUE), error = function(e) NULL)
      auc_val <- if (!is.null(roc_obj)) as.numeric(pROC::auc(roc_obj)) else NA_real_
      list(Gene = g, AUC = auc_val, roc = roc_obj)
    })

    df <- data.frame(
      Gene = vapply(res, function(x) x$Gene, character(1)),
      AUC_External = vapply(res, function(x) x$AUC, numeric(1)),
      stringsAsFactors = FALSE
    )
    df <- df[!is.na(df$AUC_External), , drop = FALSE]
    df <- df[order(-df$AUC_External), , drop = FALSE]
    curves <- setNames(lapply(res, function(x) x$roc), vapply(res, function(x) x$Gene, character(1)))
    curves <- curves[names(curves) %in% df$Gene]

    roc_int <- roc_results()
    if (!is.null(roc_int) && nrow(roc_int$df) > 0) {
      df <- merge(df, roc_int$df[, c("Gene", "AUC"), drop = FALSE], by = "Gene", all.x = TRUE)
      names(df)[names(df) == "AUC"] <- "AUC_Internal"
      df$Delta <- round(df$AUC_External - df$AUC_Internal, 4)
      df <- df[order(-df$AUC_External), , drop = FALSE]
    }

    list(df = df, curves = curves, n_genes_matched = length(available_genes),
         n_genes_total = length(common_genes))
  })

  # Dynamic UI for validation ROC panels (shown prominently when validation data exists)
  output$roc_external_validation_panels_ui <- renderUI({
    ext <- roc_external_results()
    if (is.null(ext)) return(NULL)

    n_disease <- rv$external_validation_n_disease
    n_normal <- rv$external_validation_n_normal
    if (is.null(n_disease)) n_disease <- "?"
    if (is.null(n_normal)) n_normal <- "?"

    tagList(
      fluidRow(
        box(
          title = tags$span(icon("check-double"), " Validation ROC Analysis",
                            tags$span("FROM VALIDATION DATA", class = "label label-success",
                                      style = "margin-left: 8px; font-size: 11px;")),
          width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,

          # Info banner
          tags$div(
            class = "alert alert-success", style = "margin-bottom: 15px;",
            icon("star", style = "color: #f39c12; margin-right: 4px;"),
            tags$strong(" Validation ROC: "),
            paste0("AUC/ROC computed on the independent validation dataset (",
                   nrow(rv$external_validation_expr), " samples: ",
                   n_disease, " Disease / ", n_normal, " Normal) for the same ",
                   ext$n_genes_matched, "/", ext$n_genes_total,
                   " ML common genes. This gives an unbiased view of biomarker diagnostic performance.")
          ),

          # Row 1: AUC Comparison Table + Comparison Bar Chart
          fluidRow(
            column(6,
              tags$p(tags$strong(icon("table"), " AUC: Training vs Validation"), style = "margin-bottom: 6px;"),
              DT::dataTableOutput("roc_ext_auc_comparison_table"),
              tags$div(style = "margin-top: 8px;",
                downloadButton("download_roc_ext_auc", tagList(icon("download"), " Comparison (CSV)"),
                  class = "btn-success btn-sm"))
            ),
            column(6,
              tags$p(tags$strong(icon("chart-bar"), " AUC Comparison (Training vs Validation)"), style = "margin-bottom: 6px;"),
              plotOutput("roc_comparison_auc_plot", height = "380px"),
              tags$div(style = "margin-top: 8px;",
                downloadButton("download_roc_comparison_plot_jpg", tagList(icon("download"), " JPG (300 DPI)"), class = "btn-success btn-sm", style = "margin-right: 6px;"),
                downloadButton("download_roc_comparison_plot_pdf", tagList(icon("download"), " PDF"), class = "btn-success btn-sm"))
            )
          ),

          # Row 2: Validation ROC Curves (gene expression boxplots are shown side-by-side with training above)
          fluidRow(
            column(12,
              tags$p(tags$strong(icon("chart-area"), " Validation ROC Curves"), style = "margin-bottom: 6px; margin-top: 20px;"),
              plotOutput("roc_ext_curves_plot", height = "450px"),
              tags$div(style = "margin-top: 8px;",
                downloadButton("download_roc_ext_curves_jpg", tagList(icon("download"), " JPG (300 DPI)"), class = "btn-success btn-sm", style = "margin-right: 6px;"),
                downloadButton("download_roc_ext_curves_pdf", tagList(icon("download"), " PDF"), class = "btn-success btn-sm"))
            )
          )
        )
      )
    )
  })

  output$roc_ext_auc_comparison_table <- DT::renderDataTable({
    ext <- roc_external_results()
    if (is.null(ext) || nrow(ext$df) == 0) return(NULL)
    df <- ext$df
    for (col in c("AUC_External", "AUC_Internal", "Delta")) {
      if (col %in% names(df)) df[[col]] <- round(df[[col]], 4)
    }
    # Rename columns for clarity
    names(df)[names(df) == "AUC_External"] <- "AUC_Validation"
    names(df)[names(df) == "AUC_Internal"] <- "AUC_Training"
    # Reorder: Gene, AUC_Validation, AUC_Training, Delta
    col_order <- intersect(c("Gene", "AUC_Validation", "AUC_Training", "Delta"), names(df))
    df <- df[, col_order, drop = FALSE]
    DT::datatable(df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })

  output$roc_ext_curves_plot <- renderPlot({
    ext <- roc_external_results()
    if (is.null(ext) || length(ext$curves) == 0) {
      plot.new()
      text(0.5, 0.5, "No overlapping genes in external dataset.", cex = 1, col = "gray40")
      return()
    }
    curves <- ext$curves[!sapply(ext$curves, is.null)]
    if (length(curves) == 0) return()
    n_plot <- min(10, length(curves))
    top_genes <- ext$df$Gene[seq_len(n_plot)]
    curves_plot <- curves[intersect(top_genes, names(curves))]
    curves_plot <- curves_plot[!sapply(curves_plot, is.null)]
    if (length(curves_plot) == 0) return()

    gene_cols <- roc_gene_colors()
    cols <- unname(gene_cols[names(curves_plot)])
    if (any(!nzchar(cols) | is.na(cols))) {
      base_cols <- c("#27AE60", "#1ABC9C", "#2ECC71", "#16A085", "#009688",
                     "#00BCD4", "#26A69A", "#66BB6A", "#81C784", "#A5D6A7")
      cols <- rep(base_cols, length.out = length(curves_plot))
    }
    op <- par(mar = c(4, 4, 3, 2), bg = "#FAFAFA")
    on.exit(par(op), add = TRUE)
    pROC::plot.roc(curves_plot[[1]], col = cols[1], lwd = 2,
                   main = "Validation ROC Curves", cex.main = 1.2)
    for (i in seq_along(curves_plot)[-1]) {
      pROC::lines.roc(curves_plot[[i]], col = cols[i], lwd = 2)
    }
    auc_vals <- vapply(curves_plot, function(r) as.numeric(pROC::auc(r)), numeric(1))
    legend("bottomright",
           legend = paste0(names(curves_plot), " (AUC=", round(auc_vals, 3), ")"),
           col = cols[seq_along(curves_plot)], lwd = 2, cex = 0.85, bty = "n")
  }, width = 600, height = 450, res = 96)

  output$download_roc_ext_auc <- downloadHandler(
    filename = function() "ROC_AUC_Training_vs_Validation.csv",
    content = function(file) {
      ext <- roc_external_results()
      if (is.null(ext) || nrow(ext$df) == 0) return()
      write.csv(ext$df, file, row.names = FALSE)
      write.csv(ext$df, file.path(CSV_EXPORT_DIR(), "ROC_AUC_Training_vs_Validation.csv"), row.names = FALSE)
    }
  )

  # ============================================================================
  # COMPARISON AUC PLOT (Training vs Validation grouped bar chart)
  # ============================================================================
  roc_comparison_plot_fn <- function() {
    ext <- roc_external_results()
    if (is.null(ext) || nrow(ext$df) == 0) return(NULL)
    df <- ext$df
    if (!"AUC_Internal" %in% names(df)) return(NULL)
    df <- df[!is.na(df$AUC_Internal) & !is.na(df$AUC_External), , drop = FALSE]
    if (nrow(df) == 0) return(NULL)
    top_n <- min(15, nrow(df))
    df <- df[seq_len(top_n), , drop = FALSE]
    long_auc <- data.frame(
      Gene = rep(df$Gene, 2),
      AUC = c(round(df$AUC_Internal, 3), round(df$AUC_External, 3)),
      Source = rep(c("Training", "Validation"), each = nrow(df)),
      stringsAsFactors = FALSE
    )
    long_auc$Gene <- factor(long_auc$Gene, levels = rev(df$Gene))
    long_auc$Source <- factor(long_auc$Source, levels = c("Training", "Validation"))
    p <- ggplot2::ggplot(long_auc, ggplot2::aes(x = Gene, y = AUC, fill = Source)) +
      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
      ggplot2::geom_text(ggplot2::aes(label = AUC), position = ggplot2::position_dodge(width = 0.8),
                         hjust = -0.1, size = 3, fontface = "bold") +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = c("Training" = "#3498db", "Validation" = "#27ae60")) +
      ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", alpha = 0.7) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(title = "AUC: Training vs Validation", x = "", y = "AUC", fill = "Data Source") +
      ggplot2::theme(legend.position = "top", plot.title = ggplot2::element_text(face = "bold", size = 14)) +
      ggplot2::scale_y_continuous(limits = c(0, 1.15), breaks = seq(0, 1, 0.2))
    p
  }

  output$roc_comparison_auc_plot <- renderPlot({
    p <- roc_comparison_plot_fn()
    if (is.null(p)) {
      plot.new()
      text(0.5, 0.5, "No AUC comparison data available.", cex = 1, col = "gray40")
      return()
    }
    print(p)
  }, height = 380, res = 96)

  output$download_roc_comparison_plot_jpg <- downloadHandler(
    filename = function() "AUC_Training_vs_Validation.jpg",
    content = function(file) {
      p <- roc_comparison_plot_fn()
      if (is.null(p)) return()
      ggplot2::ggsave(file, plot = p, width = 8, height = 6, dpi = IMAGE_DPI, units = "in", bg = "white", device = "jpeg")
    }
  )

  output$download_roc_comparison_plot_pdf <- downloadHandler(
    filename = function() "AUC_Training_vs_Validation.pdf",
    content = function(file) {
      p <- roc_comparison_plot_fn()
      if (is.null(p)) return()
      ggplot2::ggsave(file, plot = p, width = 8, height = 6, device = "pdf", bg = "white")
    }
  )

  # ============================================================================
  # VALIDATION BOXPLOTS (Gene expression from validation data)
  # ============================================================================
  roc_validation_boxplot_data <- reactive({
    req(rv$external_validation_expr, rv$external_validation_outcome, rv$ml_common_genes)
    ext_expr <- rv$external_validation_expr
    ext_outcome <- rv$external_validation_outcome
    common_genes <- rv$ml_common_genes
    available_genes <- intersect(common_genes, colnames(ext_expr))
    if (length(available_genes) == 0) return(NULL)
    if (nrow(ext_expr) < 3) return(NULL)
    ext_sub <- ext_expr[, available_genes, drop = FALSE]
    grp <- ifelse(ext_outcome == 0, "Normal", "Disease")
    wide <- data.frame(SampleID = rownames(ext_sub), Group = grp, ext_sub, check.names = FALSE)
    long <- tidyr::pivot_longer(wide, cols = dplyr::all_of(available_genes), names_to = "Gene", values_to = "Expression")
    long$Gene <- factor(long$Gene, levels = available_genes)
    long
  })

  roc_validation_boxplot_fn <- function() {
    long <- roc_validation_boxplot_data()
    if (is.null(long) || nrow(long) == 0) return(NULL)
    n_genes <- length(unique(long$Gene))
    if (n_genes > 12) long <- long[long$Gene %in% levels(long$Gene)[seq_len(12)], , drop = FALSE]
    grp_levels <- unique(long$Group)
    fill_colors <- if (length(grp_levels) >= 2) {
      setNames(c("#27ae60", "#e74c3c")[seq_along(grp_levels)], grp_levels)
    } else {
      setNames("#27ae60", grp_levels[1])
    }
    p <- ggplot2::ggplot(long, ggplot2::aes(x = Group, y = Expression, fill = Group)) +
      ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.85) +
      ggplot2::geom_jitter(width = 0.15, size = 1, alpha = 0.5) +
      ggplot2::facet_wrap(~Gene, scales = "free_y", ncol = min(4, n_genes)) +
      ggplot2::scale_fill_manual(values = fill_colors) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(legend.position = "top", axis.title.x = ggplot2::element_blank()) +
      ggplot2::labs(y = "Expression", title = "Validation Data: Gene Expression (Normal vs Disease)")
    p
  }

  output$roc_validation_boxplots_plot <- renderPlot({
    p <- roc_validation_boxplot_fn()
    if (is.null(p)) {
      plot.new()
      text(0.5, 0.5, "No validation data for boxplots.", cex = 1, col = "gray40")
      return()
    }
    print(p)
  }, height = 450, res = 96)

  output$download_roc_val_boxplots_jpg <- downloadHandler(
    filename = function() "Validation_Gene_Expression_Boxplots.jpg",
    content = function(file) {
      p <- roc_validation_boxplot_fn()
      if (is.null(p)) return()
      long <- roc_validation_boxplot_data()
      n_genes <- length(unique(long$Gene))
      ggplot2::ggsave(file, plot = p, width = max(8, 2.5 * min(4, n_genes)), height = 6, dpi = IMAGE_DPI, units = "in", bg = "white", device = "jpeg")
    }
  )

  output$download_roc_val_boxplots_pdf <- downloadHandler(
    filename = function() "Validation_Gene_Expression_Boxplots.pdf",
    content = function(file) {
      p <- roc_validation_boxplot_fn()
      if (is.null(p)) return()
      long <- roc_validation_boxplot_data()
      n_genes <- length(unique(long$Gene))
      ggplot2::ggsave(file, plot = p, width = max(8, 2.5 * min(4, n_genes)), height = 6, device = "pdf", bg = "white")
    }
  )

  # Validation boxplots UI: placed side-by-side with training boxplots in ui_roc
  output$roc_validation_boxplots_ui <- renderUI({
    # Only show when external validation data exists
    if (is.null(rv$external_validation_expr) || is.null(rv$external_validation_outcome)) return(NULL)
    long <- tryCatch(roc_validation_boxplot_data(), error = function(e) NULL)
    if (is.null(long) || nrow(long) == 0) return(NULL)
    box(
      title = tags$span(icon("box"), " Gene Expression -- Validation Data (Normal vs Disease)"),
      width = 6, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      plotOutput("roc_validation_boxplots_plot", height = "400px"),
      tags$div(style = "margin-top: 10px;",
        downloadButton("download_roc_val_boxplots_jpg", tagList(icon("download"), " JPG (300 DPI)"), class = "btn-success btn-sm", style = "margin-right: 6px;"),
        downloadButton("download_roc_val_boxplots_pdf", tagList(icon("download"), " PDF"), class = "btn-success btn-sm"))
    )
  })

  # ---- Download handlers for validation ROC curves ----
  output$download_roc_ext_curves_jpg <- downloadHandler(
    filename = function() "Validation_ROC_Curves.jpg",
    content = function(file) {
      ext <- roc_external_results()
      if (is.null(ext) || length(ext$curves) == 0) return()
      curves <- ext$curves[!sapply(ext$curves, is.null)]
      if (length(curves) == 0) return()
      n_plot <- min(10, length(curves))
      top_genes <- ext$df$Gene[seq_len(n_plot)]
      curves_plot <- curves[intersect(top_genes, names(curves))]
      curves_plot <- curves_plot[!sapply(curves_plot, is.null)]
      if (length(curves_plot) == 0) return()
      cols <- c("#27AE60", "#1ABC9C", "#2ECC71", "#16A085", "#009688",
                "#00BCD4", "#26A69A", "#66BB6A", "#81C784", "#A5D6A7")
      cols <- rep(cols, length.out = length(curves_plot))
      jpeg(file, width = 8, height = 6, res = IMAGE_DPI, units = "in", bg = "#FAFAFA", quality = 95)
      op <- par(mar = c(4, 4, 3, 2))
      on.exit(par(op), add = TRUE)
      pROC::plot.roc(curves_plot[[1]], col = cols[1], lwd = 2,
                     main = "Validation ROC Curves", cex.main = 1.2)
      for (i in seq_along(curves_plot)[-1]) {
        pROC::lines.roc(curves_plot[[i]], col = cols[i], lwd = 2)
      }
      auc_vals <- vapply(curves_plot, function(r) as.numeric(pROC::auc(r)), numeric(1))
      legend("bottomright",
             legend = paste0(names(curves_plot), " (AUC=", round(auc_vals, 3), ")"),
             col = cols[seq_along(curves_plot)], lwd = 2, cex = 0.85, bty = "n")
      dev.off()
    }
  )

  output$download_roc_ext_curves_pdf <- downloadHandler(
    filename = function() "Validation_ROC_Curves.pdf",
    content = function(file) {
      ext <- roc_external_results()
      if (is.null(ext) || length(ext$curves) == 0) return()
      curves <- ext$curves[!sapply(ext$curves, is.null)]
      if (length(curves) == 0) return()
      n_plot <- min(10, length(curves))
      top_genes <- ext$df$Gene[seq_len(n_plot)]
      curves_plot <- curves[intersect(top_genes, names(curves))]
      curves_plot <- curves_plot[!sapply(curves_plot, is.null)]
      if (length(curves_plot) == 0) return()
      cols <- c("#27AE60", "#1ABC9C", "#2ECC71", "#16A085", "#009688",
                "#00BCD4", "#26A69A", "#66BB6A", "#81C784", "#A5D6A7")
      cols <- rep(cols, length.out = length(curves_plot))
      pdf(file, width = 8, height = 6, bg = "#FAFAFA")
      op <- par(mar = c(4, 4, 3, 2))
      on.exit(par(op), add = TRUE)
      pROC::plot.roc(curves_plot[[1]], col = cols[1], lwd = 2,
                     main = "Validation ROC Curves", cex.main = 1.2)
      for (i in seq_along(curves_plot)[-1]) {
        pROC::lines.roc(curves_plot[[i]], col = cols[i], lwd = 2)
      }
      auc_vals <- vapply(curves_plot, function(r) as.numeric(pROC::auc(r)), numeric(1))
      legend("bottomright",
             legend = paste0(names(curves_plot), " (AUC=", round(auc_vals, 3), ")"),
             col = cols[seq_along(curves_plot)], lwd = 2, cex = 0.85, bty = "n")
      dev.off()
    }
  )

  # ============================================================================
  # GENE SELECTION PANEL -- User picks final biomarker genes for downstream
  # ============================================================================
  output$roc_gene_selector_ui <- renderUI({
    common_genes <- rv$ml_common_genes
    if (is.null(common_genes) || length(common_genes) == 0) {
      return(tags$div(
        class = "alert alert-warning", style = "margin-top: 10px;",
        icon("exclamation-triangle"),
        " No ML common genes available yet. Run ML (Step 10) first."
      ))
    }

    # Build AUC info for each gene
    roc_train <- roc_results()
    ext <- tryCatch(roc_external_results(), error = function(e) NULL)

    gene_info <- lapply(common_genes, function(g) {
      auc_train <- NA_real_
      auc_val <- NA_real_
      if (!is.null(roc_train) && nrow(roc_train$df) > 0) {
        idx <- which(roc_train$df$Gene == g)
        if (length(idx) > 0) auc_train <- roc_train$df$AUC[idx[1]]
      }
      if (!is.null(ext) && nrow(ext$df) > 0) {
        idx <- which(ext$df$Gene == g)
        if (length(idx) > 0) auc_val <- ext$df$AUC_External[idx[1]]
      }
      list(gene = g, auc_train = auc_train, auc_val = auc_val)
    })

    # Sort by validation AUC (or training AUC if no validation)
    gene_info <- gene_info[order(
      -sapply(gene_info, function(x) ifelse(is.na(x$auc_val), x$auc_train, x$auc_val)),
      na.last = TRUE
    )]

    has_validation <- !is.null(ext) && nrow(ext$df) > 0

    # Build checkbox labels with AUC info
    checkbox_choices <- setNames(
      sapply(gene_info, function(x) x$gene),
      sapply(gene_info, function(x) {
        lbl <- x$gene
        parts <- c()
        if (!is.na(x$auc_train)) parts <- c(parts, paste0("Training AUC: ", round(x$auc_train, 3)))
        if (!is.na(x$auc_val)) parts <- c(parts, paste0("Validation AUC: ", round(x$auc_val, 3)))
        if (length(parts) > 0) lbl <- paste0(lbl, "  [", paste(parts, collapse = " | "), "]")
        lbl
      })
    )

    # Pre-select genes with both AUCs >= 0.7 (or all if no validation data)
    preselected <- sapply(gene_info, function(x) {
      if (has_validation) {
        !is.na(x$auc_val) && x$auc_val >= 0.7 && !is.na(x$auc_train) && x$auc_train >= 0.7
      } else {
        !is.na(x$auc_train) && x$auc_train >= 0.8
      }
    })
    preselected_genes <- sapply(gene_info[preselected], function(x) x$gene)
    if (length(preselected_genes) == 0) preselected_genes <- sapply(gene_info, function(x) x$gene)

    tagList(
      fluidRow(
        column(8,
          tags$div(
            style = "padding: 12px; background: #fff; border: 1px solid #ddd; border-radius: 8px; max-height: 350px; overflow-y: auto;",
            checkboxGroupInput("roc_gene_checkboxes", NULL,
              choices = checkbox_choices,
              selected = preselected_genes,
              width = "100%")
          )
        ),
        column(4,
          tags$div(
            style = "padding: 15px; background: #f8f9fa; border-radius: 8px; border: 1px solid #dee2e6;",
            tags$p(tags$strong(icon("list-ol"), " Quick Actions:"), style = "margin-bottom: 10px;"),
            actionButton("roc_select_all_genes", tagList(icon("check-double"), " Select All"),
              class = "btn-default btn-sm btn-block", style = "margin-bottom: 6px;"),
            actionButton("roc_deselect_all_genes", tagList(icon("square"), " Deselect All"),
              class = "btn-default btn-sm btn-block", style = "margin-bottom: 6px;"),
            if (has_validation) {
              actionButton("roc_select_high_auc", tagList(icon("star"), " Select AUC >= 0.7 (both)"),
                class = "btn-warning btn-sm btn-block", style = "margin-bottom: 6px;")
            },
            tags$hr(style = "margin: 12px 0;"),
            tags$p(tags$strong("Selected: "), textOutput("roc_n_selected_text", inline = TRUE),
                   style = "font-size: 14px; margin-bottom: 10px;"),
            actionButton("roc_confirm_gene_selection",
              tagList(icon("check-circle"), " Confirm Selection"),
              class = "btn-danger btn-lg btn-block",
              style = "font-weight: bold; font-size: 15px;")
          )
        )
      )
    )
  })

  output$roc_n_selected_text <- renderText({
    sel <- input$roc_gene_checkboxes
    if (is.null(sel)) return("0 genes")
    paste0(length(sel), " gene(s)")
  })

  # Quick action buttons
  observeEvent(input$roc_select_all_genes, {
    common_genes <- rv$ml_common_genes
    if (!is.null(common_genes) && length(common_genes) > 0) {
      updateCheckboxGroupInput(session, "roc_gene_checkboxes", selected = common_genes)
    }
  })

  observeEvent(input$roc_deselect_all_genes, {
    updateCheckboxGroupInput(session, "roc_gene_checkboxes", selected = character(0))
  })

  observeEvent(input$roc_select_high_auc, {
    common_genes <- rv$ml_common_genes
    if (is.null(common_genes) || length(common_genes) == 0) return()
    roc_train <- roc_results()
    ext <- tryCatch(roc_external_results(), error = function(e) NULL)
    sel <- sapply(common_genes, function(g) {
      auc_t <- NA_real_; auc_v <- NA_real_
      if (!is.null(roc_train) && nrow(roc_train$df) > 0) {
        idx <- which(roc_train$df$Gene == g)
        if (length(idx) > 0) auc_t <- roc_train$df$AUC[idx[1]]
      }
      if (!is.null(ext) && nrow(ext$df) > 0) {
        idx <- which(ext$df$Gene == g)
        if (length(idx) > 0) auc_v <- ext$df$AUC_External[idx[1]]
      }
      !is.na(auc_t) && auc_t >= 0.7 && !is.na(auc_v) && auc_v >= 0.7
    })
    high_genes <- common_genes[sel]
    if (length(high_genes) == 0) {
      showNotification("No genes meet AUC >= 0.7 in both datasets.", type = "warning", duration = 4)
      return()
    }
    updateCheckboxGroupInput(session, "roc_gene_checkboxes", selected = high_genes)
  })

  # Confirm selection
  observeEvent(input$roc_confirm_gene_selection, {
    sel <- input$roc_gene_checkboxes
    if (is.null(sel) || length(sel) == 0) {
      showNotification(
        tags$div(icon("exclamation-triangle"), tags$strong(" Select at least one gene."),
                 " Your selected genes will be used for Nomogram, GSEA, and Immune analysis."),
        type = "warning", duration = 5)
      return()
    }
    rv$roc_selected_genes <- sel
    showNotification(
      tags$div(icon("check-circle"), tags$strong(paste0(" ", length(sel), " gene(s) confirmed: ")),
               tags$span(paste(sel, collapse = ", "), style = "font-weight: normal;"),
               tags$br(),
               tags$span("These genes will be used for Nomogram, GSEA, and Immune Cell analysis.",
                         style = "font-size: 12px; color: #6c757d;")),
      type = "message", duration = 8)
  })

  output$roc_gene_selection_status_ui <- renderUI({
    sel <- rv$roc_selected_genes
    if (is.null(sel) || length(sel) == 0) return(NULL)
    tags$div(
      class = "alert alert-success", style = "margin-top: 12px;",
      icon("check-circle"),
      tags$strong(paste0(" ", length(sel), " gene(s) confirmed for downstream analysis: ")),
      tags$span(paste(sel, collapse = ", "), style = "font-weight: normal;"),
      tags$br(),
      tags$small(
        icon("arrow-right", style = "margin-right: 4px;"),
        "Nomogram, GSEA, and Immune Cell Correlation will use these genes.",
        style = "color: #1e8449;"
      )
    )
  })
}
