# ==============================================================================
# SERVER_IMMUNE.R - Step 13: Immune Cell Deconvolution Analysis
# ==============================================================================
# Uses rv$batch_corrected, rv$wgcna_sample_info or rv$unified_metadata.
# immunedeconv expects genes x samples.
# ==============================================================================

server_immune <- function(input, output, session, rv) {

  output$immune_placeholder_ui <- renderUI({
    expr <- rv$batch_corrected
    if (!is.null(expr) && (is.matrix(expr) || is.data.frame(expr)) && nrow(expr) > 0) return(NULL)
    tags$div(
      class = "alert alert-warning",
      icon("hand-point-right"),
      " Run Batch Correction (Step 5) first so batch-corrected expression is available. For group comparison, run WGCNA (Step 7) to have sample metadata, or ensure unified metadata has group labels."
    )
  })

  output$immune_status_ui <- renderUI({
    if (!isTRUE(rv$immune_complete)) return(NULL)
    tags$div(
      class = "alert alert-success",
      icon("check-circle"),
      " Deconvolution complete. View proportions, boxplot, and heatmaps below."
    )
  })

  # Get expression matrix (genes x samples for immunedeconv)
  get_expr_for_immune <- reactive({
    expr <- rv$batch_corrected
    if (is.null(expr) || !(is.matrix(expr) || is.data.frame(expr)) || nrow(expr) < 10 || ncol(expr) < 3) return(NULL)
    mat <- as.matrix(expr)
    if (nrow(mat) < ncol(mat)) mat <- t(mat)
    mat
  })

  # Get sample info with Group and SampleID
  get_sample_info_immune <- reactive({
    meta <- rv$wgcna_sample_info
    if (is.null(meta) || nrow(meta) == 0) meta <- rv$unified_metadata
    if (is.null(meta) || nrow(meta) == 0) return(NULL)
    if (!"SampleID" %in% colnames(meta)) meta$SampleID <- rownames(meta)
    group_col <- NULL
    for (c in c("Group", "group", "Condition", "condition")) {
      if (c %in% colnames(meta)) { group_col <- c; break }
    }
    if (is.null(group_col)) meta$Group <- "Group1" else meta$Group <- meta[[group_col]]
    meta
  })

  observeEvent(input$run_immune, {
    safe_run({
      expr_mat <- get_expr_for_immune()
      if (is.null(expr_mat)) {
        showNotification(
          tags$div(icon("exclamation-triangle"), tags$strong(" Step 5 required:"),
                   " Complete batch correction (Step 5) to provide expression data for immune deconvolution."),
          type = "error", duration = 6)
        return(NULL)
      }
      if (!requireNamespace("immunedeconv", quietly = TRUE)) {
        showNotification("Package immunedeconv not installed. Optional: install from GitHub: remotes::install_github('omnideconv/immunedeconv')", type = "error", duration = 10)
        return(NULL)
      }
      method <- input$immune_method
      withProgress(message = "Running immune deconvolution...", value = 0.2, {
        incProgress(0.2, detail = paste("Method:", method))
        # Explicit support for xCell, EPIC, MCP-counter (and CIBERSORT if ever enabled)
        immune_raw <- switch(
          method,
          "cibersort"   = immunedeconv::deconvolute(expr_mat, method = "cibersort"),
          "mcp_counter" = immunedeconv::deconvolute(expr_mat, method = "mcp_counter"),
          "xcell"       = immunedeconv::deconvolute(expr_mat, method = "xcell"),
          "epic"        = immunedeconv::deconvolute(expr_mat, method = "epic", tumor = FALSE),
          immunedeconv::deconvolute(expr_mat, method = method)
        )
        incProgress(0.5, detail = "Processing results...")
        # immunedeconv returns: first column = cell_type (names), rest = sample columns (fractions/scores)
        cell_col <- which(tolower(colnames(immune_raw)) %in% c("cell_type", "cell type", "celltype"))[1]
        if (is.na(cell_col) || length(cell_col) == 0) cell_col <- 1
        immune_matrix <- as.data.frame(t(immune_raw[, -cell_col, drop = FALSE]))
        colnames(immune_matrix) <- as.character(immune_raw[[cell_col]])
        immune_matrix$SampleID <- rownames(immune_matrix)
        sample_info <- get_sample_info_immune()
        if (!is.null(sample_info)) {
          common <- intersect(immune_matrix$SampleID, sample_info$SampleID)
          if (length(common) > 0) {
            immune_matrix <- immune_matrix[immune_matrix$SampleID %in% common, , drop = FALSE]
            sample_info <- sample_info[sample_info$SampleID %in% common, , drop = FALSE]
            immune_data <- merge(immune_matrix, sample_info[, c("SampleID", "Group")], by = "SampleID", all.x = TRUE)
          } else {
            immune_data <- immune_matrix
            immune_data$Group <- "Unknown"
          }
        } else {
          immune_data <- immune_matrix
          immune_data$Group <- "Unknown"
        }
        cell_cols <- setdiff(colnames(immune_matrix), "SampleID")
        immune_long <- reshape2::melt(immune_data,
          id.vars = c("SampleID", "Group"),
          measure.vars = cell_cols,
          variable.name = "Cell_Type", value.name = "Proportion")
        immune_long$Proportion <- as.numeric(as.character(immune_long$Proportion))
        rv$immune_raw <- immune_raw
        rv$immune_matrix <- immune_matrix
        rv$immune_data <- immune_data
        rv$immune_long <- immune_long
        rv$immune_method <- method
        rv$immune_cell_cols <- cell_cols
        rv$immune_complete <- TRUE
        incProgress(1, detail = "Done")
        showNotification(paste("Deconvolution complete.", length(cell_cols), "cell types,", nrow(immune_matrix), "samples."), type = "message", duration = 5)
      })
      NULL
    }, step_name = "Immune deconvolution", session = session)
  })

  output$immune_proportions_table <- DT::renderDataTable({
    req(rv$immune_matrix)
    DT::datatable(rv$immune_matrix, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE, filter = "top")
  })

  output$download_immune_proportions <- downloadHandler(
    filename = function() "Immune_Cell_Proportions.csv",
    content = function(file) {
      req(rv$immune_matrix)
      write.csv(rv$immune_matrix, file, row.names = FALSE)
      write.csv(rv$immune_matrix, file.path(CSV_EXPORT_DIR(), "Immune_Cell_Proportions.csv"), row.names = FALSE)
    }
  )

  output$immune_boxplot <- renderPlot({
    req(rv$immune_long)
    dl <- rv$immune_long
    if (length(unique(dl$Cell_Type)) > 30) dl <- dl[dl$Cell_Type %in% unique(dl$Cell_Type)[1:30], ]
    group_lev <- unique(dl$Group)
    pal <- if (length(group_lev) == 2) setNames(c("#56B4E9", "#E69F00"), group_lev) else setNames(rainbow(length(group_lev)), group_lev)
    if (requireNamespace("ggpubr", quietly = TRUE)) {
      p <- ggpubr::ggboxplot(dl, x = "Group", y = "Proportion", fill = "Group", facet.by = "Cell_Type", scales = "free_y", palette = pal) +
        ggpubr::stat_compare_means(method = "wilcox.test", label = "p.signif") +
        theme_minimal(base_size = 11) +
        theme(strip.text = element_text(size = 9, face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") +
        labs(title = "Immune cell comparison by group", x = "", y = "Cell proportion")
    } else {
      p <- ggplot(dl, aes(x = Group, y = Proportion, fill = Group)) +
        geom_boxplot(outlier.size = 0.8) +
        facet_wrap(~Cell_Type, scales = "free_y", ncol = 5) +
        scale_fill_manual(values = pal) +
        theme_minimal(base_size = 11) +
        theme(strip.text = element_text(size = 9, face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") +
        labs(title = "Immune cell comparison by group", x = "", y = "Cell proportion")
    }
    print(p)
  }, width = 800, height = 520, res = 96)

  output$download_immune_boxplot_jpg <- downloadHandler(
    filename = function() "Immune_Cell_Comparison_Boxplot.jpg",
    content = function(file) {
      req(rv$immune_long)
      dl <- rv$immune_long
      if (length(unique(dl$Cell_Type)) > 30) dl <- dl[dl$Cell_Type %in% unique(dl$Cell_Type)[1:30], ]
      group_lev <- unique(dl$Group)
      pal <- if (length(group_lev) == 2) setNames(c("#56B4E9", "#E69F00"), group_lev) else setNames(rainbow(length(group_lev)), group_lev)
      if (requireNamespace("ggpubr", quietly = TRUE)) {
        p <- ggpubr::ggboxplot(dl, x = "Group", y = "Proportion", fill = "Group", facet.by = "Cell_Type", scales = "free_y", palette = pal) +
          ggpubr::stat_compare_means(method = "wilcox.test", label = "p.signif") +
          theme_minimal(base_size = 11) + theme(strip.text = element_text(size = 9, face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") +
          labs(title = "Immune cell comparison by group", x = "", y = "Cell proportion")
      } else {
        p <- ggplot(dl, aes(x = Group, y = Proportion, fill = Group)) + geom_boxplot(outlier.size = 0.8) +
          facet_wrap(~Cell_Type, scales = "free_y", ncol = 5) + scale_fill_manual(values = pal) +
          theme_minimal(base_size = 11) + theme(strip.text = element_text(size = 9, face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") +
          labs(title = "Immune cell comparison by group", x = "", y = "Cell proportion")
      }
      ggsave(file, plot = p, width = 14, height = 10, dpi = IMAGE_DPI, units = "in", bg = "white", device = "jpeg")
    }
  )
  output$download_immune_boxplot_pdf <- downloadHandler(
    filename = function() "Immune_Cell_Comparison_Boxplot.pdf",
    content = function(file) {
      req(rv$immune_long)
      dl <- rv$immune_long
      if (length(unique(dl$Cell_Type)) > 30) dl <- dl[dl$Cell_Type %in% unique(dl$Cell_Type)[1:30], ]
      group_lev <- unique(dl$Group)
      pal <- if (length(group_lev) == 2) setNames(c("#56B4E9", "#E69F00"), group_lev) else setNames(rainbow(length(group_lev)), group_lev)
      if (requireNamespace("ggpubr", quietly = TRUE)) {
        p <- ggpubr::ggboxplot(dl, x = "Group", y = "Proportion", fill = "Group", facet.by = "Cell_Type", scales = "free_y", palette = pal) +
          ggpubr::stat_compare_means(method = "wilcox.test", label = "p.signif") +
          theme_minimal(base_size = 11) + theme(strip.text = element_text(size = 9, face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") +
          labs(title = "Immune cell comparison by group", x = "", y = "Cell proportion")
      } else {
        p <- ggplot(dl, aes(x = Group, y = Proportion, fill = Group)) + geom_boxplot(outlier.size = 0.8) +
          facet_wrap(~Cell_Type, scales = "free_y", ncol = 5) + scale_fill_manual(values = pal) +
          theme_minimal(base_size = 11) + theme(strip.text = element_text(size = 9, face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") +
          labs(title = "Immune cell comparison by group", x = "", y = "Cell proportion")
      }
      ggsave(file, plot = p, width = 14, height = 10, device = cairo_pdf, bg = "white")
    }
  )

  output$immune_violin_plot <- renderPlot({
    req(rv$immune_long)
    dl <- rv$immune_long
    dl$Proportion <- as.numeric(as.character(dl$Proportion))
    dl <- dl[!is.na(dl$Proportion), ]
    if (nrow(dl) == 0) return()
    n_cells <- length(unique(dl$Cell_Type))
    if (n_cells > 45) dl <- dl[dl$Cell_Type %in% unique(dl$Cell_Type)[1:45], ]
    group_lev <- unique(dl$Group)
    pal <- if (length(group_lev) == 2) setNames(c("#E69F00", "#56B4E9"), group_lev) else setNames(rainbow(length(group_lev)), group_lev)
    p <- ggplot(dl, aes(x = .data$Cell_Type, y = .data$Proportion, fill = .data$Group)) +
      geom_violin(position = position_dodge(0.8), scale = "width", trim = TRUE, alpha = 0.7) +
      geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8), size = 1.2, alpha = 0.8, color = "grey20") +
      scale_fill_manual(values = pal, name = "Group") +
      labs(title = "Immune Cell Fractions (Violin Plot)", x = "", y = "Fraction") +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
        panel.grid.major.x = element_blank()
      )
    if (n_cells > 20) p <- p + theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7))
    print(p)
  }, width = 900, height = 580, res = 96)

  output$download_immune_violin_jpg <- downloadHandler(
    filename = function() "Immune_Cell_Fractions_Violin_Plot.jpg",
    content = function(file) {
      req(rv$immune_long)
      dl <- rv$immune_long
      dl$Proportion <- as.numeric(as.character(dl$Proportion))
      dl <- dl[!is.na(dl$Proportion), ]
      if (nrow(dl) == 0) return()
      n_cells <- length(unique(dl$Cell_Type))
      if (n_cells > 45) dl <- dl[dl$Cell_Type %in% unique(dl$Cell_Type)[1:45], ]
      group_lev <- unique(dl$Group)
      pal <- if (length(group_lev) == 2) setNames(c("#E69F00", "#56B4E9"), group_lev) else setNames(rainbow(length(group_lev)), group_lev)
      p <- ggplot(dl, aes(x = .data$Cell_Type, y = .data$Proportion, fill = .data$Group)) +
        geom_violin(position = position_dodge(0.8), scale = "width", trim = TRUE, alpha = 0.7) +
        geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8), size = 1.5, alpha = 0.8, color = "grey20") +
        scale_fill_manual(values = pal, name = "Group") +
        labs(title = "Immune Cell Fractions (Violin Plot)", x = "", y = "Fraction") +
        theme_minimal(base_size = 11) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          legend.position = "top",
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          panel.grid.major.x = element_blank()
        )
      if (n_cells > 20) p <- p + theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8))
      ggplot2::ggsave(file, plot = p, width = max(12, n_cells * 0.35), height = 8, dpi = IMAGE_DPI, units = "in", bg = "white", device = "jpeg")
    }
  )
  output$download_immune_violin_pdf <- downloadHandler(
    filename = function() "Immune_Cell_Fractions_Violin_Plot.pdf",
    content = function(file) {
      req(rv$immune_long)
      dl <- rv$immune_long
      dl$Proportion <- as.numeric(as.character(dl$Proportion))
      dl <- dl[!is.na(dl$Proportion), ]
      if (nrow(dl) == 0) return()
      n_cells <- length(unique(dl$Cell_Type))
      if (n_cells > 45) dl <- dl[dl$Cell_Type %in% unique(dl$Cell_Type)[1:45], ]
      group_lev <- unique(dl$Group)
      pal <- if (length(group_lev) == 2) setNames(c("#E69F00", "#56B4E9"), group_lev) else setNames(rainbow(length(group_lev)), group_lev)
      p <- ggplot(dl, aes(x = .data$Cell_Type, y = .data$Proportion, fill = .data$Group)) +
        geom_violin(position = position_dodge(0.8), scale = "width", trim = TRUE, alpha = 0.7) +
        geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8), size = 1.5, alpha = 0.8, color = "grey20") +
        scale_fill_manual(values = pal, name = "Group") +
        labs(title = "Immune Cell Fractions (Violin Plot)", x = "", y = "Fraction") +
        theme_minimal(base_size = 11) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          legend.position = "top",
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          panel.grid.major.x = element_blank()
        )
      if (n_cells > 20) p <- p + theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8))
      ggplot2::ggsave(file, plot = p, width = max(12, n_cells * 0.35), height = 8, device = cairo_pdf, bg = "white")
    }
  )

  output$immune_heatmap <- renderPlot({
    req(rv$immune_matrix, rv$immune_cell_cols)
    M <- rv$immune_matrix[, rv$immune_cell_cols, drop = FALSE]
    M <- as.data.frame(sapply(M, function(x) as.numeric(as.character(x))))
    rownames(M) <- rv$immune_matrix$SampleID
    cr <- cor(M, method = "spearman", use = "pairwise.complete.obs")
    if (any(is.na(cr))) cr[is.na(cr)] <- 0
    heatmap_colors <- colorRampPalette(c("#2166AC", "white", "#B2182B"))(100)
    pheatmap::pheatmap(cr, color = heatmap_colors, clustering_distance_rows = "euclidean", clustering_distance_cols = "euclidean",
      main = "Correlation between immune cells", fontsize = 8, border_color = NA)
  }, width = 600, height = 500, res = 96)

  output$download_immune_heatmap_jpg <- downloadHandler(
    filename = function() "Immune_Cell_Correlation_Heatmap.jpg",
    content = function(file) {
      req(rv$immune_matrix, rv$immune_cell_cols)
      M <- rv$immune_matrix[, rv$immune_cell_cols, drop = FALSE]
      M <- as.data.frame(sapply(M, function(x) as.numeric(as.character(x))))
      rownames(M) <- rv$immune_matrix$SampleID
      cr <- cor(M, method = "spearman", use = "pairwise.complete.obs")
      if (any(is.na(cr))) cr[is.na(cr)] <- 0
      heatmap_colors <- colorRampPalette(c("#2166AC", "white", "#B2182B"))(100)
      jpeg(file, width = 10, height = 8.33, res = IMAGE_DPI, units = "in", bg = "white", quality = 95)
      pheatmap::pheatmap(cr, color = heatmap_colors, clustering_distance_rows = "euclidean", clustering_distance_cols = "euclidean",
        main = "Correlation between immune cells", fontsize = 10, border_color = NA)
      dev.off()
    }
  )
  output$download_immune_heatmap_pdf <- downloadHandler(
    filename = function() "Immune_Cell_Correlation_Heatmap.pdf",
    content = function(file) {
      req(rv$immune_matrix, rv$immune_cell_cols)
      M <- rv$immune_matrix[, rv$immune_cell_cols, drop = FALSE]
      M <- as.data.frame(sapply(M, function(x) as.numeric(as.character(x))))
      rownames(M) <- rv$immune_matrix$SampleID
      cr <- cor(M, method = "spearman", use = "pairwise.complete.obs")
      if (any(is.na(cr))) cr[is.na(cr)] <- 0
      heatmap_colors <- colorRampPalette(c("#2166AC", "white", "#B2182B"))(100)
      pdf(file, width = 10, height = 8.33, bg = "white")
      pheatmap::pheatmap(cr, color = heatmap_colors, clustering_distance_rows = "euclidean", clustering_distance_cols = "euclidean",
        main = "Correlation between immune cells", fontsize = 10, border_color = NA)
      dev.off()
    }
  )

  # Gene–immune correlation: priority: custom input > ROC selected genes > ML common genes
  immune_correlation_genes <- reactive({
    custom <- trimws(unlist(strsplit(gsub("[,;\n]+", ",", input$immune_genes), ",")))
    custom <- custom[nzchar(custom)]
    if (length(custom) > 0) return(custom)
    if (!is.null(rv$roc_selected_genes) && length(rv$roc_selected_genes) > 0) return(rv$roc_selected_genes)
    if (!is.null(rv$ml_common_genes) && length(rv$ml_common_genes) > 0) return(rv$ml_common_genes)
    NULL
  })

  output$immune_gene_corr_ui <- renderUI({
    if (!isTRUE(rv$immune_complete)) return(NULL)
    genes <- immune_correlation_genes()
    if (is.null(genes) || length(genes) == 0) {
      tags$p("No genes for correlation. Enter comma-separated gene symbols above, or run Step 10 (ML) to use common genes.", class = "text-muted")
    } else {
      NULL
    }
  })

  gene_immune_corr_result <- reactive({
    req(rv$immune_complete, rv$immune_matrix, rv$immune_cell_cols)
    genes <- immune_correlation_genes()
    if (is.null(genes) || length(genes) == 0) return(NULL)
    expr_mat <- get_expr_for_immune()
    if (is.null(expr_mat)) return(NULL)
    available <- intersect(genes, rownames(expr_mat))
    if (length(available) == 0) return(NULL)
    immune_mat <- rv$immune_matrix
    cell_cols <- rv$immune_cell_cols
    numeric_immune <- as.data.frame(lapply(immune_mat[, cell_cols, drop = FALSE], function(x) as.numeric(as.character(x))))
    rownames(numeric_immune) <- immune_mat$SampleID
    common_samp <- intersect(colnames(expr_mat), immune_mat$SampleID)
    if (length(common_samp) < 3) return(NULL)
    expr_mat <- expr_mat[, common_samp, drop = FALSE]
    numeric_immune <- numeric_immune[common_samp, , drop = FALSE]
    cor_list <- list()
    pval_list <- list()
    for (gene in available) {
      gex <- as.numeric(expr_mat[gene, ])
      cors <- vapply(numeric_immune, function(y) {
        ct <- suppressWarnings(cor.test(gex, y, method = "spearman", exact = FALSE))
        if (is.na(ct$estimate)) return(NA_real_)
        as.numeric(ct$estimate)
      }, numeric(1))
      pvals <- vapply(numeric_immune, function(y) {
        ct <- suppressWarnings(cor.test(gex, y, method = "spearman", exact = FALSE))
        if (is.null(ct$p.value) || is.na(ct$p.value)) return(NA_real_)
        as.numeric(ct$p.value)
      }, numeric(1))
      cor_list[[gene]] <- cors
      pval_list[[gene]] <- pvals
    }
    cmat <- do.call(rbind, cor_list)
    pmat <- do.call(rbind, pval_list)
    rownames(cmat) <- rownames(pmat) <- available
    colnames(pmat) <- colnames(cmat)
    list(cor_matrix = cmat, pval_matrix = pmat, genes = available, cell_cols = cell_cols)
  })

  output$immune_gene_heatmap <- renderPlot({
    res <- gene_immune_corr_result()
    if (is.null(res) || nrow(res$cor_matrix) == 0) {
      plot.new()
      text(0.5, 0.5, "No gene–immune correlation (add genes above or run ML for common genes).", cex = 1, col = "gray40")
      return()
    }
    heatmap_colors <- colorRampPalette(c("#2166AC", "white", "#B2182B"))(100)
    pheatmap::pheatmap(res$cor_matrix, color = heatmap_colors, main = "Gene–immune cell correlation (Spearman)", fontsize = 9, border_color = NA)
  }, width = 600, height = 400, res = 96)

  output$download_immune_gene_heatmap_jpg <- downloadHandler(
    filename = function() "Gene_Immune_Cell_Correlation_Heatmap.jpg",
    content = function(file) {
      res <- gene_immune_corr_result()
      if (is.null(res) || nrow(res$cor_matrix) == 0) return()
      heatmap_colors <- colorRampPalette(c("#2166AC", "white", "#B2182B"))(100)
      jpeg(file, width = 9.33, height = 6.67, res = IMAGE_DPI, units = "in", bg = "white", quality = 95)
      pheatmap::pheatmap(res$cor_matrix, color = heatmap_colors, main = "Gene–immune cell correlation (Spearman)", fontsize = 10, border_color = NA)
      dev.off()
    }
  )
  output$download_immune_gene_heatmap_pdf <- downloadHandler(
    filename = function() "Gene_Immune_Cell_Correlation_Heatmap.pdf",
    content = function(file) {
      res <- gene_immune_corr_result()
      if (is.null(res) || nrow(res$cor_matrix) == 0) return()
      heatmap_colors <- colorRampPalette(c("#2166AC", "white", "#B2182B"))(100)
      pdf(file, width = 9.33, height = 6.67, bg = "white")
      pheatmap::pheatmap(res$cor_matrix, color = heatmap_colors, main = "Gene–immune cell correlation (Spearman)", fontsize = 10, border_color = NA)
      dev.off()
    }
  )

  # Lollipop plot: one gene at a time
  output$immune_lollipop_gene_ui <- renderUI({
    res <- gene_immune_corr_result()
    if (is.null(res) || length(res$genes) == 0) return(NULL)
    choices <- setNames(res$genes, res$genes)
    selectInput("immune_lollipop_gene", "Select gene", choices = choices, selected = res$genes[1])
  })

  immune_lollipop_data <- reactive({
    res <- gene_immune_corr_result()
    gene <- input$immune_lollipop_gene
    if (is.null(res) || is.null(gene) || !gene %in% res$genes) return(NULL)
    cors <- res$cor_matrix[gene, ]
    pvals <- res$pval_matrix[gene, ]
    df <- data.frame(
      cell_type = names(cors),
      cor = as.numeric(cors),
      pval = as.numeric(pvals),
      abs_cor = abs(as.numeric(cors)),
      stringsAsFactors = FALSE
    )
    df <- df[order(df$cor), ]
    df$cell_type <- factor(df$cell_type, levels = df$cell_type)
    list(df = df, gene = gene)
  })

  immune_method_label <- reactive({
    m <- rv$immune_method
    if (is.null(m)) return("")
    if (m == "xcell") return("xCell")
    if (m == "mcp_counter") return("MCP-counter")
    if (m == "epic") return("EPIC")
    m
  })

  output$immune_lollipop_plot <- renderPlot({
    dat <- immune_lollipop_data()
    if (is.null(dat)) {
      plot.new()
      text(0.5, 0.5, "Run deconvolution with genes above to see lollipop. Select a gene.", cex = 1, col = "gray40")
      return()
    }
    df <- dat$df
    gene <- dat$gene
    method_label <- immune_method_label()
    x_range <- range(df$cor, na.rm = TRUE)
    x_lim <- c(x_range[1] - 0.02, x_range[2] + 0.15)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$cor, y = .data$cell_type)) +
      ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$cor, y = .data$cell_type, yend = .data$cell_type), color = "grey70", linewidth = 0.6) +
      ggplot2::geom_vline(xintercept = 0, linetype = "solid", color = "grey40", linewidth = 0.5) +
      ggplot2::geom_point(ggplot2::aes(color = .data$pval, size = .data$abs_cor), alpha = 0.9) +
      ggplot2::scale_color_gradientn(
        colours = c("#D73027", "#FC8D59", "#FEE08B", "#91CF60", "#4575B4", "#762A83"),
        name = "P-value",
        limits = c(0, 1),
        na.value = "grey50"
      ) +
      ggplot2::scale_size_continuous(name = "|Correlation|", range = c(2.5, 9), limits = c(0, 1)) +
      ggplot2::labs(
        title = paste0("Correlation between ", gene, " and Immune Cells"),
        subtitle = if (nzchar(method_label)) paste0("Method: ", method_label) else NULL,
        x = "Correlation Coefficient",
        y = ""
      ) +
      ggplot2::xlim(x_lim) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5, color = "grey40"),
        axis.text.y = ggplot2::element_text(size = 9),
        legend.position = "right",
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )
    pval_sig <- df$pval < 0.05
    pval_x <- max(df$cor, na.rm = TRUE) + 0.03
    p <- p + ggplot2::geom_text(
      ggplot2::aes(x = pval_x, y = .data$cell_type, label = sprintf("%.3f", .data$pval)),
      size = 2.8, hjust = 0,
      color = ifelse(pval_sig, "#D73027", "grey30"),
      fontface = ifelse(pval_sig, "bold", "plain")
    )
    print(p)
  }, width = 800, height = 680, res = 96)

  output$download_immune_lollipop_jpg <- downloadHandler(
    filename = function() {
      gene <- input$immune_lollipop_gene
      if (is.null(gene) || !nzchar(gene)) return("Immune_Lollipop.jpg")
      paste0("Immune_Lollipop_", gsub("[^A-Za-z0-9_-]", "_", gene), ".jpg")
    },
    content = function(file) {
      dat <- immune_lollipop_data()
      if (is.null(dat)) return()
      df <- dat$df
      gene <- dat$gene
      method_label <- immune_method_label()
      x_range <- range(df$cor, na.rm = TRUE)
      x_lim <- c(x_range[1] - 0.02, x_range[2] + 0.15)
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$cor, y = .data$cell_type)) +
        ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$cor, y = .data$cell_type, yend = .data$cell_type), color = "grey70", linewidth = 0.6) +
        ggplot2::geom_vline(xintercept = 0, linetype = "solid", color = "grey40", linewidth = 0.5) +
        ggplot2::geom_point(ggplot2::aes(color = .data$pval, size = .data$abs_cor), alpha = 0.9) +
        ggplot2::scale_color_gradientn(
          colours = c("#D73027", "#FC8D59", "#FEE08B", "#91CF60", "#4575B4", "#762A83"),
          name = "P-value", limits = c(0, 1), na.value = "grey50"
        ) +
        ggplot2::scale_size_continuous(name = "|Correlation|", range = c(2.5, 9), limits = c(0, 1)) +
        ggplot2::labs(
          title = paste0("Correlation between ", gene, " and Immune Cells"),
          subtitle = if (nzchar(method_label)) paste0("Method: ", method_label) else NULL,
          x = "Correlation Coefficient", y = ""
        ) +
        ggplot2::xlim(x_lim) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5, color = "grey40"),
          axis.text.y = ggplot2::element_text(size = 10),
          legend.position = "right",
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )
      pval_sig <- df$pval < 0.05
      pval_x <- max(df$cor, na.rm = TRUE) + 0.03
      p <- p + ggplot2::geom_text(
        ggplot2::aes(x = pval_x, y = .data$cell_type, label = sprintf("%.3f", .data$pval)),
        size = 3, hjust = 0,
        color = ifelse(pval_sig, "#D73027", "grey30"),
        fontface = ifelse(pval_sig, "bold", "plain")
      )
      ggplot2::ggsave(file, plot = p, width = 12, height = max(8, nrow(df) * 0.2), dpi = IMAGE_DPI, units = "in", bg = "white", device = "jpeg")
    }
  )
  output$download_immune_lollipop_pdf <- downloadHandler(
    filename = function() {
      gene <- input$immune_lollipop_gene
      if (is.null(gene) || !nzchar(gene)) return("Immune_Lollipop.pdf")
      paste0("Immune_Lollipop_", gsub("[^A-Za-z0-9_-]", "_", gene), ".pdf")
    },
    content = function(file) {
      dat <- immune_lollipop_data()
      if (is.null(dat)) return()
      df <- dat$df
      gene <- dat$gene
      method_label <- immune_method_label()
      x_range <- range(df$cor, na.rm = TRUE)
      x_lim <- c(x_range[1] - 0.02, x_range[2] + 0.15)
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$cor, y = .data$cell_type)) +
        ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$cor, y = .data$cell_type, yend = .data$cell_type), color = "grey70", linewidth = 0.6) +
        ggplot2::geom_vline(xintercept = 0, linetype = "solid", color = "grey40", linewidth = 0.5) +
        ggplot2::geom_point(ggplot2::aes(color = .data$pval, size = .data$abs_cor), alpha = 0.9) +
        ggplot2::scale_color_gradientn(
          colours = c("#D73027", "#FC8D59", "#FEE08B", "#91CF60", "#4575B4", "#762A83"),
          name = "P-value", limits = c(0, 1), na.value = "grey50"
        ) +
        ggplot2::scale_size_continuous(name = "|Correlation|", range = c(2.5, 9), limits = c(0, 1)) +
        ggplot2::labs(
          title = paste0("Correlation between ", gene, " and Immune Cells"),
          subtitle = if (nzchar(method_label)) paste0("Method: ", method_label) else NULL,
          x = "Correlation Coefficient", y = ""
        ) +
        ggplot2::xlim(x_lim) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5, color = "grey40"),
          axis.text.y = ggplot2::element_text(size = 10),
          legend.position = "right",
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )
      pval_sig <- df$pval < 0.05
      pval_x <- max(df$cor, na.rm = TRUE) + 0.03
      p <- p + ggplot2::geom_text(
        ggplot2::aes(x = pval_x, y = .data$cell_type, label = sprintf("%.3f", .data$pval)),
        size = 3, hjust = 0,
        color = ifelse(pval_sig, "#D73027", "grey30"),
        fontface = ifelse(pval_sig, "bold", "plain")
      )
      ggplot2::ggsave(file, plot = p, width = 12, height = max(8, nrow(df) * 0.2), device = cairo_pdf, bg = "white")
    }
  )
}
