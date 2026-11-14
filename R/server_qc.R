# ==============================================================================
# SERVER_QC.R - Step 3: QC & Visualization Module
# ==============================================================================

server_qc <- function(input, output, session, rv) {

  output$qc_log_micro <- renderText({
    if (!is.null(rv$qc_log_micro) && nzchar(rv$qc_log_micro)) rv$qc_log_micro
    else "Complete Step 2 to see the microarray QC log."
  })
  output$qc_log_rna <- renderText({
    if (!is.null(rv$qc_log_rna) && nzchar(rv$qc_log_rna)) rv$qc_log_rna
    else "Complete Step 2 to see the RNA-seq QC log."
  })

  observe({
    if (!isTRUE(rv$merge_after_de) || !isTRUE(rv$normalization_complete)) {
      return()
    }
    micro <- rv$expr_micro
    rna <- rv$expr_rna
    n_mg <- if (is.null(micro)) 0L else nrow(micro)
    n_ms <- if (is.null(micro)) 0L else ncol(micro)
    n_rg <- if (is.null(rna)) 0L else nrow(rna)
    n_rs <- if (is.null(rna)) 0L else ncol(rna)
    rv$qc_log_micro <- gexpipe_format_separate_run_log(
      1L, "MICROARRAY",
      paste0(
        "QC on microarray matrix only.\n",
        "  Genes:   ", format(n_mg, big.mark = ","), "\n",
        "  Samples: ", format(n_ms, big.mark = ","), "\n",
        "  Merged with RNA-seq: no\n",
        "  Common-gene intersection for DE: not applied\n",
        "\nOK Microarray QC recorded.\n"
      )
    )
    rv$qc_log_rna <- gexpipe_format_separate_run_log(
      2L, "RNA-SEQ",
      paste0(
        "QC on RNA-seq matrix only.\n",
        "  Genes:   ", format(n_rg, big.mark = ","), "\n",
        "  Samples: ", format(n_rs, big.mark = ","), "\n",
        "  Merged with microarray: no\n",
        "  Common-gene intersection for DE: not applied\n",
        "\nOK RNA-seq QC recorded.\n"
      )
    )
  })

  .qc_expr <- function() {
    if (isTRUE(rv$normalization_complete) && !is.null(rv$combined_expr) &&
        is.matrix(rv$combined_expr) && ncol(rv$combined_expr) > 0L) {
      rv$combined_expr
    } else {
      rv$combined_expr_raw
    }
  }

  # ==============================================================================
  # INFO BOXES
  # ==============================================================================
  
  output$datasets_box <- renderInfoBox({
    infoBox(
      "Datasets", 
      length(rv$all_genes_list), 
      icon = icon("database", class = "fa-2x"), 
      color = "blue", 
      fill = TRUE
    )
  })
  
  output$samples_box <- renderInfoBox({
    n <- if (!is.null(.qc_expr())) ncol(.qc_expr()) else 0
    infoBox(
      "Samples", 
      n, 
      icon = icon("vial", class = "fa-2x"), 
      color = "green", 
      fill = TRUE
    )
  })
  
  output$genes_box <- renderInfoBox({
    n <- if (!is.null(rv$common_genes)) length(rv$common_genes) else 0
    infoBox(
      "Genes", 
      n, 
      icon = icon("dna", class = "fa-2x"), 
      color = "purple", 
      fill = TRUE
    )
  })
  
  output$gene_overlap_summary <- renderUI({
    req(rv$all_genes_list, rv$common_genes)
    # These are per-dataset gene counts from Step 1 (Download). Expect ~37,673 for full NCBI RNA-seq.
    overlap_df <- gexp_qc_gene_overlap_summary(rv$all_genes_list, rv$common_genes)
    total_genes_per_dataset <- overlap_df$Total_Genes
    common_count <- length(rv$common_genes)
    
    tags$div(
      style = "padding: 15px;",
      tags$h4(icon("info-circle"), " Gene Overlap Analysis", style = "color: #00a65a;"),
      tags$table(
        class = "table table-striped table-bordered",
        style = "margin-top: 15px;",
        tags$thead(
          tags$tr(
            tags$th("Dataset", style = "text-align: center; background-color: #3c8dbc; color: white;"),
            tags$th("Total Genes", style = "text-align: center; background-color: #3c8dbc; color: white;"),
            tags$th("% in Common", style = "text-align: center; background-color: #3c8dbc; color: white;")
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(overlap_df)), function(i) {
            gse <- overlap_df$Dataset[[i]]
            total <- overlap_df$Total_Genes[[i]]
            pct <- overlap_df$Pct_in_Common[[i]]
            tags$tr(
              tags$td(tags$b(gse), style = "text-align: center;"),
              tags$td(format(total, big.mark = ","), style = "text-align: center;"),
              tags$td(
                paste0(pct, "%"),
                style = paste0("text-align: center; font-weight: bold; color: ",
                               ifelse(pct > 80, "#00a65a", ifelse(pct > 60, "#f39c12", "#dd4b39")), ";")
              )
            )
          })
        )
      ),
      tags$div(
        style = "margin-top: 15px; padding: 10px; background-color: #d4edda; border-radius: 5px;",
        tags$h5(icon("check-circle"), 
                tags$b(paste0(" Common Genes: ", format(common_count, big.mark = ","))),
                style = "color: #155724; margin: 0;")
      ),
      if (common_count == 0 || (length(total_genes_per_dataset) >= 3 && common_count < 10)) {
        tags$div(
          class = "alert alert-warning",
          style = "margin-top: 15px;",
          tags$strong(icon("exclamation-triangle"), " Why is there no (or almost no) overlap?"),
          tags$p("Overlap is computed by ", tags$strong("exact match of row identifiers"), " (e.g. gene symbols or probe IDs). If one or more datasets use a ", tags$strong("different microarray platform"), ", they have different probe IDs and will not match even when they measure the same genes.", style = "margin-top: 8px;"),
          tags$p("Your datasets (e.g. GSE62646, GSE123342, GSE75214, GSE36807) may use different GPLs. A dataset with ~70k rows is likely a different platform (e.g. HuGene) than ones with ~20k (e.g. HG-U133 Plus 2).", style = "margin-top: 4px;"),
          tags$p(tags$strong("What to do:"), " Use datasets on the ", tags$strong("same platform"), " (same GPL) for maximum overlap, or ensure all GSEs are annotated to ", tags$strong("gene symbols"), " in Step 1 (check the download log for \"unique gene symbols\" per GSE).", style = "margin-top: 4px;")
        )
      } else {
        NULL
      }
    )
  })
  
  # ==============================================================================
  # VENN & UPSET (shared draw-to-dev helpers for display + download)
  # ==============================================================================

  draw_qc_venn_to_dev <- function(gene_list = rv$all_genes_list) {
    venn_prep <- gexp_qc_prepare_venn_sets(gene_list, max_sets = 5L)
    if (!isTRUE(venn_prep$ok)) {
      plot.new()
      text(0.5, 0.5, venn_prep$message, cex = 1.8, col = "gray40")
      return(invisible())
    }
    sets <- venn_prep$sets
    if (length(sets) == 2 && setequal(sets[[1]], sets[[2]])) {
      # VennDiagram draws identical sets as one circle labelled
      # "n n (Coincidental)", which reads like an error; say it plainly.
      n <- format(length(unique(sets[[1]])), big.mark = ",")
      grid::grid.newpage()
      grid::grid.circle(x = 0.5, y = 0.45, r = 0.3,
                        gp = grid::gpar(fill = grDevices::adjustcolor("#3498db", 0.45), col = "black", lwd = 2))
      grid::grid.text(paste0(n, " genes"), x = 0.5, y = 0.49, gp = grid::gpar(fontsize = 16, fontface = "bold"))
      grid::grid.text("shared by both datasets (100% overlap)", x = 0.5, y = 0.41, gp = grid::gpar(fontsize = 11))
      grid::grid.text(paste(names(sets), collapse = "  =  "), x = 0.5, y = 0.88,
                      gp = grid::gpar(fontsize = 13, fontface = "bold", col = "#2c3e50"))
      grid::grid.text("Both datasets contain exactly the same gene set", x = 0.5, y = 0.82,
                      gp = grid::gpar(fontsize = 10, col = "gray40"))
    } else if (length(sets) == 2) {
      tryCatch({
        # Category names with totals displayed clearly
        cat_names <- paste0(names(sets), "\n", format(vapply(sets, length, integer(1)), big.mark = ","))

        venn.plot <- VennDiagram::venn.diagram(
          x = sets,
          category.names = cat_names,
          filename = NULL,
          output = TRUE,
          disable.logging = TRUE,
          fill = c("#f39c12", "#3498db"),
          alpha = 0.6,
          cex = 0.9,
          cat.cex = 1.0,
          cat.fontface = "bold",
          cat.pos = c(-20, 20),
          cat.dist = c(0.15, 0.15),
          cat.col = c("#f39c12", "#3498db"),
          margin = 0.2,
          print.mode = "raw",
          fontfamily = "sans",
          lwd = 2,
          force.unique = TRUE,
          na = "remove"
        )
        grid::grid.newpage()
        grid::grid.draw(venn.plot)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error creating Venn diagram:", e$message), cex = 1.2, col = "red")
      })
    } else if (length(sets) == 3) {
      tryCatch({
        # Check if all sets are identical
        all_intersect <- Reduce(intersect, sets)
        all_same <- all(vapply(sets, function(s) length(s) == length(all_intersect), logical(1)))
        
        if (all_same) {
          plot.new()
          text(0.5, 0.5, 
               paste0("All datasets have identical genes\n(", format(length(all_intersect), big.mark = ","), " genes)"),
               cex = 1.8, col = "gray40")
          return()
        }
        
        # Create category names with totals (like "Q = 16")
        cat_names <- paste0(names(sets), " = ", format(vapply(sets, length, integer(1)), big.mark = ","))
        
        # Create category names with totals
        cat_names <- paste0(names(sets), "\n", format(vapply(sets, length, integer(1)), big.mark = ","))
        
        venn.plot <- venn.diagram(
          x = sets,
          category.names = cat_names,
          filename = NULL,
          output = TRUE,
          disable.logging = TRUE,
          fill = c("#f1c40f", "#3498db", "#e74c3c"),
          alpha = 0.6,
          cex = 0.8,
          cat.cex = 0.9,
          cat.fontface = "bold",
          cat.pos = c(-40, 40, 180),
          cat.dist = c(0.15, 0.15, 0.10),
          cat.col = c("#f1c40f", "#3498db", "#e74c3c"),
          margin = 0.2,
          print.mode = "raw",
          fontfamily = "sans",
          lwd = 2,
          force.unique = TRUE,
          na = "remove"
        )
        grid.draw(venn.plot)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error creating Venn diagram:", e$message), cex = 1.2, col = "red")
      })
    } else if (length(sets) == 4) {
      tryCatch({
        # Check if all sets are identical
        all_intersect <- Reduce(intersect, sets)
        all_same <- all(vapply(sets, function(s) length(s) == length(all_intersect), logical(1)))
        
        if (all_same) {
          plot.new()
          text(0.5, 0.5, 
               paste0("All datasets have identical genes\n(", format(length(all_intersect), big.mark = ","), " genes)"),
               cex = 1.8, col = "gray40")
          return()
        }
        
        # Ensure all genes are unique within each set
        sets <- lapply(sets, unique)
        
        # Create category names with totals
        cat_names <- paste0(names(sets), "\n", format(vapply(sets, length, integer(1)), big.mark = ","))
        
        # Create a more detailed Venn diagram with better visibility
        # Calculate all intersections manually first to ensure they exist
        all_intersect_4 <- Reduce(intersect, sets)
        intersect_123 <- Reduce(intersect, sets[seq_len(3L)])
        intersect_124 <- Reduce(intersect, sets[c(1L, 2L, 4L)])
        intersect_134 <- Reduce(intersect, sets[c(1L, 3L, 4L)])
        intersect_234 <- Reduce(intersect, sets[c(2L, 3L, 4L)])
        
        venn.plot <- venn.diagram(
          x = sets,
          category.names = cat_names,
          filename = NULL,
          output = TRUE,
          disable.logging = TRUE,
          fill = c("#f1c40f", "#3498db", "#e74c3c", "#2ecc71"),
          alpha = 0.65,
          cex = 0.7,
          cat.cex = 0.85,
          cat.fontface = "bold",
          cat.col = c("#f1c40f", "#3498db", "#e74c3c", "#2ecc71"),
          margin = 0.12,
          print.mode = "raw",
          fontfamily = "sans",
          lwd = 2,
          force.unique = TRUE,
          na = "remove",
          scaled = TRUE,
          euler.d = FALSE,
          rotation.degree = 0,
          ext.text = FALSE,
          ext.percent = 0,
          ext.pos = 0,
          ext.dist = 0,
          ext.line.lwd = 0,
          ext.line.lty = 0
        )
        grid.draw(venn.plot)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error creating Venn diagram:", e$message), cex = 1.2, col = "red")
      })
    } else {
      # For 5+ datasets, check if all identical
      all_intersect <- Reduce(intersect, sets)
      all_same <- all(vapply(sets, function(s) length(s) == length(all_intersect), logical(1)))
      
      if (all_same) {
        plot.new()
        text(0.5, 0.5, 
             paste0("All datasets have identical genes\n(", format(length(all_intersect), big.mark = ","), " genes)"),
             cex = 1.8, col = "gray40")
      } else {
        # Show summary
        plot.new()
        text(0.5, 0.7, paste("Total Datasets:", length(sets)), cex = 2, font = 2)
        text(0.5, 0.5, paste("Common Genes:", format(length(all_intersect), big.mark = ",")), 
             cex = 3, col = "#2ecc71", font = 2)
        text(0.5, 0.3, "See UpSet plot for detailed intersections ->", 
             cex = 1.3, col = "gray40")
      }
    }
  }

  output$venn_plot <- renderPlot({
    req(rv$all_genes_list)
    draw_qc_venn_to_dev()
  })
  
  draw_qc_upset_to_dev <- function(gene_list = rv$all_genes_list) {
    upset_prep <- gexp_qc_prepare_upset_data(gene_list)
    if (!isTRUE(upset_prep$ok)) {
      plot.new()
      text(0.5, 0.5, upset_prep$message, cex = 1.8, col = "gray40")
      return(invisible())
    }
    upset_df <- upset_prep$upset_df
    max_set_size <- upset_prep$max_set_size
    tryCatch({
      # upset() returns an object that only draws when shown (file devices do not auto-print)
      p <- UpSetR::upset(
        upset_df,
        sets = colnames(upset_df),
        keep.order = TRUE,
        order.by = "freq",
        main.bar.color = "#3498db",
        sets.bar.color = "#e74c3c",
        matrix.color = "#2ecc71",
        point.size = 3.5,
        line.size = 1,
        text.scale = c(1.4, 1.1, 1.1, 1, 1.4, 1.2),
        mb.ratio = c(0.7, 0.3),
        set_size.show = TRUE,
        set_size.scale_max = max_set_size * 1.1
      )
      methods::show(p)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating UpSet plot:", e$message), cex = 1.2, col = "red")
    })
  }

  output$upset_plot <- renderPlot({
    req(rv$all_genes_list)
    draw_qc_upset_to_dev()
  })
  
  # ==============================================================================
  # QC PLOTS
  # ==============================================================================
  
  output$qc_boxplot <- renderPlot({
    expr <- .qc_expr()
    req(expr)
    df <- gexp_qc_prepare_boxplot_data(
      combined_expr_raw = expr,
      micro_expr_list = rv$micro_expr_list,
      rna_counts_list = rv$rna_counts_list,
      max_points = 500000L
    )
    
    ggplot(df, aes(x = Sample, y = Expression, fill = Platform)) +
      geom_boxplot(outlier.size = 0.5) +
      theme_bw() +
      labs(title = if (isTRUE(rv$normalization_complete)) {
        "Expression Distribution - After Normalization"
      } else {
        "Expression Distribution - Raw Data (run Step 2 first)"
      }, y = "Expression") +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            plot.title = element_text(face = "bold", size = 16))
  })
  
  output$qc_density <- renderPlot({
    expr <- .qc_expr()
    req(expr)
    dd <- gexp_qc_prepare_density_data(expr, max_samples = 50L)
    plot(dd$first,
         main = if (isTRUE(rv$normalization_complete)) {
           "Expression Density - After Normalization"
         } else {
           "Expression Density - Raw Data (run Step 2 first)"
         },
         xlab = "Expression", col = "#3498db", lwd = 2,
         ylim = c(0, max(dd$first$y) * 1.2))
    if (length(dd$others) > 0) {
      for (i in seq_along(dd$others)) {
        lines(dd$others[[i]], col = dd$colors[i + 1L], lwd = 1)
      }
    }
  })

  .qc_platform_boxplot <- function(expr, title, fill) {
    if (is.null(expr) || !is.matrix(expr) || ncol(expr) < 1L) {
      plot.new()
      text(0.5, 0.5, "Complete Step 2 to see this plot.", cex = 1.1, col = "gray40")
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

  .qc_platform_density <- function(expr, title, color) {
    if (is.null(expr) || !is.matrix(expr) || ncol(expr) < 1L) {
      plot.new()
      text(0.5, 0.5, "Complete Step 2 to see this plot.", cex = 1.1, col = "gray40")
      return(invisible(NULL))
    }
    dd <- gexp_qc_prepare_density_data(expr, max_samples = 50L)
    plot(dd$first, main = title, xlab = "Expression", col = color, lwd = 2,
         ylim = c(0, max(dd$first$y, na.rm = TRUE) * 1.2))
    if (length(dd$others) > 0L) {
      for (i in seq_along(dd$others)) {
        graphics::lines(dd$others[[i]], col = dd$colors[i + 1L], lwd = 1)
      }
    }
  }

  output$qc_boxplot_rna <- renderPlot({
    .qc_platform_boxplot(rv$expr_rna, "RNA-seq QC boxplot", "#93c5fd")
  })
  output$qc_density_rna <- renderPlot({
    .qc_platform_density(rv$expr_rna, "RNA-seq QC density", "#2563eb")
  })
  output$qc_boxplot_micro <- renderPlot({
    .qc_platform_boxplot(rv$expr_micro, "Microarray QC boxplot", "#fde68a")
  })
  output$qc_density_micro <- renderPlot({
    .qc_platform_density(rv$expr_micro, "Microarray QC density", "#d97706")
  })

  # ---- Parallel mode: gene overlap / common genes identified SEPARATELY per platform ----
  .platform_gene_sets <- function(platform) {
    ds <- if (identical(platform, "rna")) names(rv$rna_counts_list) else names(rv$micro_expr_list)
    rv$all_genes_list[intersect(names(rv$all_genes_list), ds)]
  }
  .platform_label <- function(platform) if (identical(platform, "rna")) "RNA-seq" else "Microarray"

  for (.pl in c("rna", "micro")) local({
    pl <- .pl
    output[[paste0("venn_plot_", pl)]] <- renderPlot({
      req(rv$all_genes_list)
      draw_qc_venn_to_dev(.platform_gene_sets(pl))
    })
    output[[paste0("upset_plot_", pl)]] <- renderPlot({
      req(rv$all_genes_list)
      draw_qc_upset_to_dev(.platform_gene_sets(pl))
    })
    output[[paste0("common_genes_summary_", pl)]] <- renderUI({
      req(rv$all_genes_list)
      sets <- .platform_gene_sets(pl)
      lab <- .platform_label(pl)
      if (length(sets) == 0L) {
        return(tags$p(style = "color:#7f8c8d;", icon("info-circle"), paste0(" No ", lab, " datasets loaded.")))
      }
      common <- Reduce(intersect, lapply(sets, unique))
      tags$div(
        style = "padding: 8px 12px; background: #e8f5e9; border-radius: 6px; margin-bottom: 8px; font-size: 13px;",
        icon("check-circle", style = "color:#2e7d32;"),
        tags$strong(paste0(" ", lab, " common genes: ", format(length(common), big.mark = ","))),
        paste0(" (across ", length(sets), " dataset", if (length(sets) != 1L) "s" else "", ": ", paste(names(sets), collapse = ", "), ")"),
        if (length(sets) == 1L) tags$span(style = "color:#555;", " - one dataset, so all its genes are used.") else NULL
      )
    })
    gexp_register_plot_downloads(output, paste0("venn_plot_", pl), function() { req(rv$all_genes_list); draw_qc_venn_to_dev(.platform_gene_sets(pl)) }, 7, 7,
                                 paste0("QC_Venn_Diagram_", .platform_label(pl)))
    gexp_register_plot_downloads(output, paste0("upset_plot_", pl), function() { req(rv$all_genes_list); draw_qc_upset_to_dev(.platform_gene_sets(pl)) }, 8, 6,
                                 paste0("QC_UpSet_Plot_", .platform_label(pl)))
  })

  # Parallel-mode plots had no image download buttons at all (merged mode
  # already did, via gexp_ui_plot_download_bar) - add PNG/JPG/PDF for each.
  gexp_register_plot_downloads(output, "qc_boxplot_rna", function() .qc_platform_boxplot(rv$expr_rna, "RNA-seq QC boxplot", "#93c5fd"), 7, 5, "QC_Boxplot_RNAseq")
  gexp_register_plot_downloads(output, "qc_density_rna", function() .qc_platform_density(rv$expr_rna, "RNA-seq QC density", "#2563eb"), 7, 5, "QC_Density_RNAseq")
  gexp_register_plot_downloads(output, "qc_boxplot_micro", function() .qc_platform_boxplot(rv$expr_micro, "Microarray QC boxplot", "#fde68a"), 7, 5, "QC_Boxplot_Microarray")
  gexp_register_plot_downloads(output, "qc_density_micro", function() .qc_platform_density(rv$expr_micro, "Microarray QC density", "#d97706"), 7, 5, "QC_Density_Microarray")

  output$qc_process_summary_ui <- renderUI({
    if (is.null(.qc_expr()) || ncol(.qc_expr()) == 0) {
      return(tags$p(style = "color: #6c757d; margin: 0;", icon("info-circle"), " Complete Step 2 (Normalization) and view QC plots to see process summary."))
    }
    n_samp <- ncol(.qc_expr())
    n_genes <- nrow(.qc_expr())
    tags$div(
      style = "font-size: 14px; line-height: 1.6; color: #333;",
      tags$p(tags$strong("Step 3 summary."), " ", format(n_genes, big.mark = ","), " genes, ", format(n_samp, big.mark = ","), " samples after normalization. Venn/UpSet show common genes; QC plots show normalized expression."))
  })

  output$dl_qc_venn_png <- downloadHandler(
    filename = function() "QC_Venn_Diagram.png",
    content = function(file) {
      req(rv$all_genes_list)
      png(file, width = 8, height = 6, res = IMAGE_DPI, units = "in", bg = "white")
      on.exit(dev.off())
      draw_qc_venn_to_dev()
    }
  )
  output$dl_qc_venn_jpg <- downloadHandler(
    filename = function() "QC_Venn_Diagram.jpg",
    content = function(file) {
      req(rv$all_genes_list)
      jpeg(file, width = 8, height = 6, res = IMAGE_DPI, units = "in", bg = "white", quality = 95)
      on.exit(dev.off())
      draw_qc_venn_to_dev()
    }
  )
  output$dl_qc_venn_pdf <- downloadHandler(
    filename = function() "QC_Venn_Diagram.pdf",
    content = function(file) {
      req(rv$all_genes_list)
      pdf(file, width = 8, height = 6, bg = "white")
      on.exit(dev.off())
      draw_qc_venn_to_dev()
    }
  )
  output$dl_qc_upset_png <- downloadHandler(
    filename = function() "QC_UpSet_Plot.png",
    content = function(file) {
      req(rv$all_genes_list)
      png(file, width = 10, height = 6, res = IMAGE_DPI, units = "in", bg = "white")
      on.exit(dev.off())
      draw_qc_upset_to_dev()
    }
  )
  output$dl_qc_upset_jpg <- downloadHandler(
    filename = function() "QC_UpSet_Plot.jpg",
    content = function(file) {
      req(rv$all_genes_list)
      jpeg(file, width = 10, height = 6, res = IMAGE_DPI, units = "in", bg = "white", quality = 95)
      on.exit(dev.off())
      draw_qc_upset_to_dev()
    }
  )
  output$dl_qc_upset_pdf <- downloadHandler(
    filename = function() "QC_UpSet_Plot.pdf",
    content = function(file) {
      req(rv$all_genes_list)
      pdf(file, width = 10, height = 6, bg = "white")
      on.exit(dev.off())
      draw_qc_upset_to_dev()
    }
  )
  output$dl_qc_boxplot_png <- downloadHandler(
    filename = function() "QC_Boxplot.png",
    content = function(file) {
      req(rv$combined_expr_raw)
      df <- gexp_qc_prepare_boxplot_data(
        combined_expr_raw = rv$combined_expr_raw,
        micro_expr_list = rv$micro_expr_list,
        rna_counts_list = rv$rna_counts_list,
        max_points = 500000L
      )
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Sample, y = .data$Expression, fill = .data$Platform)) +
        ggplot2::geom_boxplot(outlier.size = 0.5) +
        ggplot2::theme_bw() +
        ggplot2::labs(title = "Expression Distribution - Raw Data", y = "Expression") +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(), plot.title = ggplot2::element_text(face = "bold", size = 16))
      ggplot2::ggsave(file, p, width = 10, height = 5, dpi = IMAGE_DPI, units = "in", bg = "white", device = "png")
    }
  )
  output$dl_qc_boxplot_jpg <- downloadHandler(
    filename = function() "QC_Boxplot.jpg",
    content = function(file) {
      req(rv$combined_expr_raw)
      df <- gexp_qc_prepare_boxplot_data(
        combined_expr_raw = rv$combined_expr_raw,
        micro_expr_list = rv$micro_expr_list,
        rna_counts_list = rv$rna_counts_list,
        max_points = 500000L
      )
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Sample, y = .data$Expression, fill = .data$Platform)) +
        ggplot2::geom_boxplot(outlier.size = 0.5) +
        ggplot2::theme_bw() +
        ggplot2::labs(title = "Expression Distribution - Raw Data", y = "Expression") +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(), plot.title = ggplot2::element_text(face = "bold", size = 16))
      ggplot2::ggsave(file, p, width = 10, height = 5, dpi = IMAGE_DPI, units = "in", bg = "white", device = "jpeg")
    }
  )
  output$dl_qc_boxplot_pdf <- downloadHandler(
    filename = function() "QC_Boxplot.pdf",
    content = function(file) {
      req(rv$combined_expr_raw)
      df <- gexp_qc_prepare_boxplot_data(
        combined_expr_raw = rv$combined_expr_raw,
        micro_expr_list = rv$micro_expr_list,
        rna_counts_list = rv$rna_counts_list,
        max_points = 500000L
      )
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Sample, y = .data$Expression, fill = .data$Platform)) +
        ggplot2::geom_boxplot(outlier.size = 0.5) +
        ggplot2::theme_bw() +
        ggplot2::labs(title = "Expression Distribution - Raw Data", y = "Expression") +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(), plot.title = ggplot2::element_text(face = "bold", size = 16))
      ggplot2::ggsave(file, p, width = 10, height = 5, device = "pdf", bg = "white")
    }
  )
  output$dl_qc_density_png <- downloadHandler(
    filename = function() "QC_Density.png",
    content = function(file) {
      req(rv$combined_expr_raw)
      png(file, width = 8, height = 5, res = IMAGE_DPI, units = "in", bg = "white")
      on.exit(dev.off())
      dd <- gexp_qc_prepare_density_data(rv$combined_expr_raw, max_samples = 50L)
      plot(dd$first, main = "Expression Density - Raw Data", xlab = "Expression", col = "#3498db", lwd = 2, ylim = c(0, max(dd$first$y) * 1.2))
      if (length(dd$others) > 0) {
        for (i in seq_along(dd$others)) lines(dd$others[[i]], col = dd$colors[i + 1L], lwd = 1)
      }
    }
  )
  output$dl_qc_density_jpg <- downloadHandler(
    filename = function() "QC_Density.jpg",
    content = function(file) {
      req(rv$combined_expr_raw)
      jpeg(file, width = 8, height = 5, res = IMAGE_DPI, units = "in", bg = "white", quality = 95)
      on.exit(dev.off())
      dd <- gexp_qc_prepare_density_data(rv$combined_expr_raw, max_samples = 50L)
      plot(dd$first, main = "Expression Density - Raw Data", xlab = "Expression", col = "#3498db", lwd = 2, ylim = c(0, max(dd$first$y) * 1.2))
      if (length(dd$others) > 0) {
        for (i in seq_along(dd$others)) lines(dd$others[[i]], col = dd$colors[i + 1L], lwd = 1)
      }
    }
  )
  output$dl_qc_density_pdf <- downloadHandler(
    filename = function() "QC_Density.pdf",
    content = function(file) {
      req(rv$combined_expr_raw)
      pdf(file, width = 8, height = 5, bg = "white")
      on.exit(dev.off())
      dd <- gexp_qc_prepare_density_data(rv$combined_expr_raw, max_samples = 50L)
      plot(dd$first, main = "Expression Density - Raw Data", xlab = "Expression", col = "#3498db", lwd = 2, ylim = c(0, max(dd$first$y) * 1.2))
      if (length(dd$others) > 0) {
        for (i in seq_along(dd$others)) lines(dd$others[[i]], col = dd$colors[i + 1L], lwd = 1)
      }
    }
  )

  # Next to normalize
}
