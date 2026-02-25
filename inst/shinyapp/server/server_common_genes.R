# ==============================================================================
# SERVER_COMMON_GENES.R - Common Genes (DEG vs WGCNA) + GO & KEGG
# ==============================================================================

server_common_genes <- function(input, output, session, rv) {
  
  # ---------- COMMON GENES ----------
  observeEvent(input$compute_common_genes, {
    # Check prerequisites with clear messages (instead of silent req)
    if (is.null(rv$sig_genes) || !is.data.frame(rv$sig_genes) || nrow(rv$sig_genes) == 0) {
      showNotification("Complete Step 6 (Differential Expression) first and run DE analysis to get significant genes.", type = "warning", duration = 8)
      return()
    }
    if (is.null(rv$gene_metrics) || !is.data.frame(rv$gene_metrics) || nrow(rv$gene_metrics) == 0) {
      showNotification("Complete Step 7 (WGCNA): run Module-Trait correlation and 'Calculate Correlations & GS/MM' to get gene metrics.", type = "warning", duration = 8)
      return()
    }
    if (is.null(rv$significant_modules) || !is.data.frame(rv$significant_modules) || nrow(rv$significant_modules) == 0) {
      showNotification("Complete Step 7 (WGCNA): click 'Identify Significant Modules' to get trait-associated modules.", type = "warning", duration = 8)
      return()
    }

    # Get DEG gene column (allow "Gene" or "gene" or first column)
    sg <- rv$sig_genes
    deg_col <- if ("Gene" %in% names(sg)) "Gene" else if ("gene" %in% names(sg)) "gene" else names(sg)[1]
    deg_genes <- unique(trimws(as.character(sg[[deg_col]])))
    if (length(deg_genes) == 0) {
      showNotification("No significant DEGs found. Adjust DE cutoffs in Step 6 or re-run DE analysis.", type = "warning", duration = 6)
      return()
    }

    sig_mod_names <- as.character(rv$significant_modules$Module)
    # Module column is "MEblue" etc.; gene_metrics$Module is "blue"
    module_colors <- unique(sub("^ME", "", sig_mod_names))
    wgcna_genes <- unique(trimws(rv$gene_metrics$Gene[rv$gene_metrics$Module %in% module_colors]))
    if (length(wgcna_genes) == 0) {
      showNotification("No genes in significant WGCNA modules. Check module-trait thresholds in Step 7.", type = "warning", duration = 6)
      return()
    }

    common <- intersect(deg_genes, wgcna_genes)
    rv$common_genes_de_wgcna <- common
    rv$common_genes_df <- data.frame(Gene = common, stringsAsFactors = FALSE)
    rv$common_genes_deg_n <- length(deg_genes)
    rv$common_genes_wgcna_n <- length(wgcna_genes)

    if (length(common) == 0) {
      showNotification("Computation done: no overlap between DEGs and WGCNA significant module genes. Try relaxing DE or module-trait cutoffs.", type = "warning", duration = 8)
    } else {
      showNotification(paste("Found", length(common), "common genes (DEG ∩ WGCNA)."), type = "message", duration = 5)
    }
  })
  
  output$common_genes_placeholder_ui <- renderUI({
    if (!is.null(rv$common_genes_de_wgcna)) return(NULL)
    tags$div(
      class = "alert alert-warning",
      icon("hand-point-right"),
      " Click 'Compute Common Genes' to find overlap between DEGs and WGCNA significant module genes. Requires Step 6 (DE) and Step 7 (Identify Significant Modules) completed."
    )
  })
  
  output$common_genes_summary_ui <- renderUI({
    req(rv$common_genes_de_wgcna)
    n_common <- length(rv$common_genes_de_wgcna)
    n_deg <- rv$common_genes_deg_n
    n_wgcna <- rv$common_genes_wgcna_n
    pct_deg <- if (n_deg > 0) round(100 * n_common / n_deg, 1) else 0
    pct_wgcna <- if (n_wgcna > 0) round(100 * n_common / n_wgcna, 1) else 0
    tags$div(
      class = "alert alert-success",
      icon("check-circle"),
      tags$strong("Common genes: ", n_common),
      tags$br(),
      "DEGs: ", n_deg, " | WGCNA (sig. modules): ", n_wgcna,
      tags$br(),
      "Overlap: ", pct_deg, "% of DEGs, ", pct_wgcna, "% of WGCNA genes."
    )
  })
  
  output$common_genes_table <- DT::renderDataTable({
    req(rv$common_genes_df)
    DT::datatable(rv$common_genes_df, options = list(pageLength = 25), rownames = FALSE, filter = "top")
  })
  
  output$download_common_genes <- downloadHandler(
    filename = function() "common_genes_DEG_WGCNA.csv",
    content = function(file) {
      req(rv$common_genes_df)
      write.csv(rv$common_genes_df, file, row.names = FALSE)
      write.csv(rv$common_genes_df, file.path(CSV_EXPORT_DIR(), "common_genes_DEG_WGCNA.csv"), row.names = FALSE)
    }
  )
  
  # ---------- GO ENRICHMENT ----------
  observeEvent(input$run_go_enrichment, {
    req(rv$common_genes_de_wgcna)
    if (length(rv$common_genes_de_wgcna) == 0) {
      showNotification("No common genes. Compute common genes first and ensure there is overlap.", type = "warning", duration = 6)
      return()
    }
    genes <- unique(rv$common_genes_de_wgcna)
    entrez <- tryCatch(
      clusterProfiler::bitr(genes, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = org.Hs.eg.db::org.Hs.eg.db),
      error = function(e) data.frame(SYMBOL = character(), ENTREZID = character())
    )
    if (nrow(entrez) == 0) {
      showNotification("No ENTREZ IDs found for common genes. Check gene symbols.", type = "error", duration = 8)
      return()
    }
    
    pcut <- input$go_pvalue_cutoff
    qcut <- input$go_qvalue_cutoff
    
    withProgress(message = "GO enrichment...", value = 0.3, {
      rv$go_bp <- tryCatch({
        clusterProfiler::enrichGO(gene = entrez$ENTREZID, OrgDb = org.Hs.eg.db::org.Hs.eg.db,
                                  ont = "BP", pAdjustMethod = "BH", pvalueCutoff = pcut, qvalueCutoff = qcut)
      }, error = function(e) NULL)
      incProgress(0.3)
      rv$go_mf <- tryCatch({
        clusterProfiler::enrichGO(gene = entrez$ENTREZID, OrgDb = org.Hs.eg.db::org.Hs.eg.db,
                                  ont = "MF", pAdjustMethod = "BH", pvalueCutoff = pcut, qvalueCutoff = qcut)
      }, error = function(e) NULL)
      incProgress(0.2)
      rv$go_cc <- tryCatch({
        clusterProfiler::enrichGO(gene = entrez$ENTREZID, OrgDb = org.Hs.eg.db::org.Hs.eg.db,
                                  ont = "CC", pAdjustMethod = "BH", pvalueCutoff = pcut, qvalueCutoff = qcut)
      }, error = function(e) NULL)
      incProgress(0.2)
    })
    showNotification("GO enrichment complete.", type = "message", duration = 4)
  })
  
  output$go_enrichment_status_ui <- renderUI({
    if (is.null(rv$go_bp) && is.null(rv$go_mf) && is.null(rv$go_cc)) return(NULL)
    n_bp <- if (!is.null(rv$go_bp) && inherits(rv$go_bp, "enrichResult")) nrow(as.data.frame(rv$go_bp)) else 0
    n_mf <- if (!is.null(rv$go_mf) && inherits(rv$go_mf, "enrichResult")) nrow(as.data.frame(rv$go_mf)) else 0
    n_cc <- if (!is.null(rv$go_cc) && inherits(rv$go_cc, "enrichResult")) nrow(as.data.frame(rv$go_cc)) else 0
    tags$div(class = "alert alert-info",
             "BP: ", n_bp, " terms | MF: ", n_mf, " terms | CC: ", n_cc, " terms")
  })
  
  plot_go_dot <- function(go_obj, title) {
    if (is.null(go_obj) || !inherits(go_obj, "enrichResult")) {
      plot.new()
      text(0.5, 0.5, "No enrichment or not run yet", cex = 1.2)
      return(invisible(NULL))
    }
    df <- as.data.frame(go_obj)
    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, paste("No significant terms for", title), cex = 1.2)
      return(invisible(NULL))
    }
    n_show <- min(20, nrow(df))
    p <- enrichplot::dotplot(go_obj, showCategory = n_show, x = "GeneRatio", color = "p.adjust", size = "Count") +
      ggplot2::scale_color_continuous(low = "#E64B35", high = "#4DBBD5", name = "p.adjust") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 10),
        plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
        legend.position = "right"
      ) +
      ggplot2::ggtitle(title)
    print(p)
  }
  
  output$go_bp_plot <- renderPlot({
    tryCatch({
      plot_go_dot(rv$go_bp, "GO Biological Process")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", conditionMessage(e)), cex = 1, col = "red")
    })
  })
  
  output$go_mf_plot <- renderPlot({
    tryCatch({
      plot_go_dot(rv$go_mf, "GO Molecular Function")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", conditionMessage(e)), cex = 1, col = "red")
    })
  })
  
  output$go_cc_plot <- renderPlot({
    tryCatch({
      plot_go_dot(rv$go_cc, "GO Cellular Component")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", conditionMessage(e)), cex = 1, col = "red")
    })
  })
  
  output$download_go_results <- downloadHandler(
    filename = function() "GO_Enrichment_Common_Genes.csv",
    content = function(file) {
      req(rv$go_bp, rv$go_mf, rv$go_cc)
      out <- list()
      if (!is.null(rv$go_bp) && inherits(rv$go_bp, "enrichResult")) {
        df <- as.data.frame(rv$go_bp)
        if (nrow(df) > 0) { df$Ontology <- "BP"; out[[length(out) + 1]] <- df }
      }
      if (!is.null(rv$go_mf) && inherits(rv$go_mf, "enrichResult")) {
        df <- as.data.frame(rv$go_mf)
        if (nrow(df) > 0) { df$Ontology <- "MF"; out[[length(out) + 1]] <- df }
      }
      if (!is.null(rv$go_cc) && inherits(rv$go_cc, "enrichResult")) {
        df <- as.data.frame(rv$go_cc)
        if (nrow(df) > 0) { df$Ontology <- "CC"; out[[length(out) + 1]] <- df }
      }
      data_out <- if (length(out) == 0) data.frame() else do.call(rbind, out)
      if (length(out) == 0) write.csv(data_out, file) else write.csv(data_out, file, row.names = FALSE)
      if (length(out) == 0) write.csv(data_out, file.path(CSV_EXPORT_DIR(), "GO_Enrichment_Common_Genes.csv")) else write.csv(data_out, file.path(CSV_EXPORT_DIR(), "GO_Enrichment_Common_Genes.csv"), row.names = FALSE)
    }
  )
  
  # ---------- KEGG ENRICHMENT ----------
  observeEvent(input$run_kegg_enrichment, {
    req(rv$common_genes_de_wgcna)
    if (length(rv$common_genes_de_wgcna) == 0) {
      showNotification("No common genes. Compute common genes first and ensure there is overlap.", type = "warning", duration = 6)
      return()
    }
    genes <- unique(rv$common_genes_de_wgcna)
    entrez <- tryCatch(
      clusterProfiler::bitr(genes, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = org.Hs.eg.db::org.Hs.eg.db),
      error = function(e) data.frame(SYMBOL = character(), ENTREZID = character())
    )
    if (nrow(entrez) == 0) {
      showNotification("No ENTREZ IDs found for common genes.", type = "error", duration = 8)
      return()
    }
    
    withProgress(message = "KEGG enrichment...", value = 0.5, {
      rv$kegg_enrichment <- tryCatch({
        clusterProfiler::enrichKEGG(gene = entrez$ENTREZID, organism = "hsa",
                                     pvalueCutoff = input$kegg_pvalue_cutoff)
      }, error = function(e) NULL)
    })
    showNotification("KEGG enrichment complete.", type = "message", duration = 4)
  })
  
  output$kegg_enrichment_status_ui <- renderUI({
    if (is.null(rv$kegg_enrichment)) return(NULL)
    df_k <- kegg_result_df(rv$kegg_enrichment)
    n <- if (is.null(df_k)) 0 else nrow(df_k)
    tags$div(class = "alert alert-info", "KEGG pathways found: ", n)
  })

  # Safely get KEGG enrichment as a plain data frame (handles enrichResult, keggResult, different package versions)
  kegg_result_df <- function(obj) {
    if (is.null(obj)) return(NULL)
    ok_class <- inherits(obj, "enrichResult") || inherits(obj, "keggResult") ||
      (isS4(obj) && "result" %in% methods::slotNames(obj))
    if (!ok_class) return(NULL)
    df <- tryCatch(
      as.data.frame(obj, stringsAsFactors = FALSE),
      error = function(e) {
        if (isS4(obj) && "result" %in% methods::slotNames(obj))
          as.data.frame(methods::slot(obj, "result"), stringsAsFactors = FALSE)
        else
          NULL
      }
    )
    if (is.null(df) || nrow(df) == 0) return(df)
    # Normalize column names for different clusterProfiler versions
    nms <- names(df)
    nms_l <- tolower(nms)
    if (!"Description" %in% nms && "description" %in% nms_l)
      df$Description <- df[[nms[which(nms_l == "description")[1]]]]
    if (!"GeneRatio" %in% nms && "generatio" %in% nms_l)
      df$GeneRatio <- df[[nms[which(nms_l == "generatio")[1]]]]
    if (!"geneID" %in% nms && "geneid" %in% nms_l)
      df$geneID <- df[[nms[which(nms_l == "geneid")[1]]]]
    if (!"p.adjust" %in% nms && "padjust" %in% nms_l)
      df$p.adjust <- df[[nms[which(nms_l == "padjust")[1]]]]
    if (!"ID" %in% nms && "id" %in% nms_l)
      df$ID <- df[[nms[which(nms_l == "id")[1]]]]
    df
  }

  kegg_geneid_count <- function(geneID) {
    if (is.null(geneID)) return(integer(0))
    sep <- if (any(grepl("/", as.character(geneID), fixed = TRUE))) "/" else ","
    lengths(strsplit(as.character(geneID), sep, fixed = TRUE))
  }

  # KEGG bar plot - manual ggplot from result dataframe
  output$kegg_barplot <- renderPlot(
    {
      tryCatch({
        df <- kegg_result_df(rv$kegg_enrichment)
        if (is.null(df) || nrow(df) == 0) {
          plot.new()
          text(0.5, 0.5, if (is.null(rv$kegg_enrichment)) "Run KEGG enrichment first" else "No significant KEGG pathways", cex = 1.2)
          return(invisible(NULL))
        }
        n_show <- min(20, nrow(df))
        o <- order(as.numeric(df$p.adjust))
        df <- df[o, ][seq_len(n_show), , drop = FALSE]
        if (!"Count" %in% names(df) && "geneID" %in% names(df))
          df$Count <- kegg_geneid_count(df$geneID)
        if (!"Count" %in% names(df)) df$Count <- 1L
        df$Description <- factor(df$Description, levels = rev(unique(as.character(df$Description))))
        p <- ggplot2::ggplot(df, ggplot2::aes(x = Description, y = Count, fill = p.adjust)) +
          ggplot2::geom_col() +
          ggplot2::coord_flip() +
          ggplot2::scale_fill_continuous(low = "#E64B35", high = "#4DBBD5", name = "p.adjust") +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(
            axis.text.y = ggplot2::element_text(size = 10),
            plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
            legend.position = "right"
          ) +
          ggplot2::labs(title = "KEGG Pathway Enrichment (Bar)", x = NULL, y = "Gene count")
        print(p)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", conditionMessage(e)), cex = 0.9, col = "red")
      })
    },
    width = 550,
    height = 500,
    res = 96,
    alt = "KEGG bar plot"
  )

  # KEGG chord diagram: pathways (right) <-> genes (left), colored by pathway
  # Returns chord_df, pathways (IDs), pathway_info (ID, Description, p.adjust) for list and legend
  kegg_chord_data <- function() {
    kdf <- kegg_result_df(rv$kegg_enrichment)
    if (is.null(kdf) || nrow(kdf) == 0) return(NULL)
    if (!"ID" %in% names(kdf) || !"geneID" %in% names(kdf)) return(NULL)
    top_n <- tryCatch(min(max(2, as.integer(input$kegg_chord_top_n)), nrow(kdf), 30), error = function(e) min(10, nrow(kdf)))
    top_pathways <- head(kdf, top_n)
    pathway_info <- top_pathways[, c("ID", "Description", "p.adjust")]
    pathway_info$Description <- as.character(pathway_info$Description)
    sep <- if (any(grepl("/", top_pathways$geneID, fixed = TRUE))) "/" else ","
    pathway_genes <- data.frame(
      Pathway = rep(top_pathways$ID, lengths(strsplit(as.character(top_pathways$geneID), sep, fixed = TRUE))),
      ENTREZID = unlist(strsplit(as.character(top_pathways$geneID), sep, fixed = TRUE)),
      stringsAsFactors = FALSE
    )
    entrez_to_sym <- tryCatch(
      clusterProfiler::bitr(pathway_genes$ENTREZID, fromType = "ENTREZID", toType = "SYMBOL", OrgDb = org.Hs.eg.db::org.Hs.eg.db),
      error = function(e) data.frame(ENTREZID = character(), SYMBOL = character())
    )
    pathway_genes <- merge(pathway_genes, entrez_to_sym, by = "ENTREZID", all.x = TRUE)
    pathway_genes <- pathway_genes[!is.na(pathway_genes$SYMBOL) & pathway_genes$SYMBOL != "", ]
    if (nrow(pathway_genes) == 0) return(NULL)
    chord_df <- data.frame(Pathway = pathway_genes$Pathway, Gene = pathway_genes$SYMBOL, stringsAsFactors = FALSE)
    pathways <- unique(chord_df$Pathway)
    list(
      chord_df = chord_df,
      pathways = pathways,
      pathway_info = pathway_info
    )
  }

  # Pathway list table (same pathways as chord) for UI
  output$kegg_pathway_list_table <- DT::renderDataTable({
    dat <- kegg_chord_data()
    if (is.null(dat) || !"pathway_info" %in% names(dat) || nrow(dat$pathway_info) == 0)
      return(data.frame(Pathway = character(), Description = character(), `p.adjust` = numeric(), stringsAsFactors = FALSE))
    df <- dat$pathway_info
    names(df) <- c("Pathway ID", "Description", "p.adjust")
    DT::datatable(df, options = list(pageLength = 12, scrollX = TRUE, scrollY = "560px"), rownames = FALSE, filter = "top", class = "compact stripe")
  })

  output$kegg_chord_plot <- renderPlot(
    {
      circlize::circos.clear()
      tryCatch({
        dat <- kegg_chord_data()
        if (is.null(dat)) {
          plot.new()
          text(0.5, 0.5, "Run KEGG enrichment first or no pathway–gene links.", cex = 1.1)
          return(invisible(NULL))
        }
        chord_df <- dat$chord_df
        pathways <- dat$pathways
        genes <- unique(chord_df$Gene)
        n_path <- length(pathways)
        n_genes <- length(genes)
        if (n_path < 1 || n_genes < 1) {
          plot.new()
          text(0.5, 0.5, "No pathway–gene links to draw.", cex = 1.1)
          return(invisible(NULL))
        }
        pathway_colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(min(8, max(3, n_path)), "Set2"))(n_path)
        pathway_cols <- setNames(pathway_colors, pathways)
        gene_to_path <- chord_df[match(genes, chord_df$Gene), "Pathway"]
        gene_cols <- setNames(pathway_colors[match(gene_to_path, pathways)], genes)
        grid_colors <- c(pathway_cols, gene_cols)
        circlize::circos.par(gap.degree = c(rep(3, n_path), rep(0.3, n_genes)), start.degree = 90)
        circlize::chordDiagram(
          chord_df,
          annotationTrack = "grid",
          grid.col = grid_colors,
          transparency = 0.2,
          directional = 0,
          order = c(pathways, genes),
          preAllocateTracks = list(track.height = 0.12)
        )
        circlize::circos.track(
          track.index = 1,
          panel.fun = function(x, y) {
            sector.index <- circlize::get.cell.meta.data("sector.index")
            if (sector.index %in% pathways) {
              circlize::circos.text(
                circlize::get.cell.meta.data("xcenter"),
                circlize::get.cell.meta.data("ylim")[1] + 0.5,
                sector.index,
                facing = "clockwise",
                niceFacing = TRUE,
                adj = c(0, 0.5),
                cex = 0.85
              )
            }
          },
          bg.border = NA
        )
        circlize::circos.clear()
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Chord error:", conditionMessage(e)), cex = 1, col = "red")
      })
      circlize::circos.clear()
    },
    width = 700,
    height = 600,
    res = 96,
    alt = "KEGG chord diagram"
  )
  
  kegg_chord_to_file <- function(file, dev_fun) {
    dat <- kegg_chord_data()
    if (is.null(dat)) {
      grDevices::png(file, width = 800, height = 800)
      plot.new()
      text(0.5, 0.5, "No chord data. Run KEGG enrichment first.")
      grDevices::dev.off()
      return()
    }
    chord_df <- dat$chord_df
    pathways <- dat$pathways
    genes <- unique(chord_df$Gene)
    n_path <- length(pathways)
    n_genes <- length(genes)
    pathway_colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(min(8, max(3, n_path)), "Set2"))(n_path)
    pathway_cols <- setNames(pathway_colors, pathways)
    gene_to_path <- chord_df[match(genes, chord_df$Gene), "Pathway"]
    gene_cols <- setNames(pathway_colors[match(gene_to_path, pathways)], genes)
    grid_colors <- c(pathway_cols, gene_cols)
    dev_fun(file)
    circlize::circos.clear()
    circlize::circos.par(gap.degree = c(rep(3, n_path), rep(0.3, n_genes)), start.degree = 90)
    circlize::chordDiagram(chord_df, annotationTrack = "grid", grid.col = grid_colors, transparency = 0.2, directional = 0, order = c(pathways, genes), preAllocateTracks = list(track.height = 0.12))
    circlize::circos.track(track.index = 1, panel.fun = function(x, y) {
      sector.index <- circlize::get.cell.meta.data("sector.index")
      if (sector.index %in% pathways) circlize::circos.text(circlize::get.cell.meta.data("xcenter"), circlize::get.cell.meta.data("ylim")[1] + 0.5, sector.index, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 0.9)
    }, bg.border = NA)
    circlize::circos.clear()
    grDevices::dev.off()
  }

  output$download_kegg_chord <- downloadHandler(
    filename = function() "KEGG_Chord_Diagram.png",
    content = function(file) {
      kegg_chord_to_file(file, function(f) grDevices::png(f, width = 9 * IMAGE_DPI, height = 8.67 * IMAGE_DPI, res = IMAGE_DPI))
    }
  )
  output$download_kegg_chord_jpg <- downloadHandler(
    filename = function() "KEGG_Chord_Diagram.jpg",
    content = function(file) {
      kegg_chord_to_file(file, function(f) jpeg(f, width = 9, height = 8.67, res = IMAGE_DPI, units = "in", bg = "white", quality = 95))
    }
  )
  output$download_kegg_chord_pdf <- downloadHandler(
    filename = function() "KEGG_Chord_Diagram.pdf",
    content = function(file) {
      kegg_chord_to_file(file, function(f) pdf(f, width = 9, height = 8.67, bg = "white"))
    }
  )
  
  output$download_kegg_results <- downloadHandler(
    filename = function() "KEGG_Enrichment_Common_Genes.csv",
    content = function(file) {
      df <- kegg_result_df(rv$kegg_enrichment)
      if (is.null(df) || nrow(df) == 0) {
        write.csv(data.frame(), file)
        write.csv(data.frame(), file.path(CSV_EXPORT_DIR(), "KEGG_Enrichment_Common_Genes.csv"))
      } else {
        write.csv(df, file, row.names = FALSE)
        write.csv(df, file.path(CSV_EXPORT_DIR(), "KEGG_Enrichment_Common_Genes.csv"), row.names = FALSE)
      }
    }
  )

  # ---------- Extract Data for ML (Common Genes) - ready for ML path ----------
  observeEvent(input$extract_ml_data_common_genes, {
    if (is.null(rv$common_genes_de_wgcna) || length(rv$common_genes_de_wgcna) == 0) {
      showNotification("Compute common genes first.", type = "warning", duration = 5)
      return()
    }
    if (is.null(rv$datExpr)) {
      showNotification("WGCNA data (Step 7 → Data Preparation) required. Run WGCNA first.", type = "warning", duration = 6)
      return()
    }
    datExpr <- as.matrix(rv$datExpr)
    genes_in_expr <- colnames(datExpr)
    common <- unique(trimws(rv$common_genes_de_wgcna))
    keep <- intersect(common, genes_in_expr)
    if (length(keep) == 0) {
      showNotification("No common genes found in datExpr. Check gene names.", type = "error", duration = 5)
      return()
    }
    rv$extracted_data_ml <- datExpr[, keep, drop = FALSE]
    showNotification(paste("Extracted", nrow(rv$extracted_data_ml), "samples ×", ncol(rv$extracted_data_ml), "genes for ML."), type = "message", duration = 5)
  })

  output$common_genes_extracted_ml_status_ui <- renderUI({
    if (is.null(rv$extracted_data_ml)) return(NULL)
    tags$div(
      class = "alert alert-success",
      icon("check-circle"),
      " Extracted data ready for ML: ", nrow(rv$extracted_data_ml), " samples × ", ncol(rv$extracted_data_ml), " genes."
    )
  })

  output$common_genes_download_extracted_ml_ui <- renderUI({
    if (is.null(rv$extracted_data_ml)) return(NULL)
    downloadButton("download_common_genes_extracted_ml",
                  tagList(icon("download"), " Download Extracted Data (CSV)"),
                  class = "btn-info")
  })

  output$download_common_genes_extracted_ml <- downloadHandler(
    filename = function() "extracted_common_genes_data.csv",
    content = function(file) {
      req(rv$extracted_data_ml)
      m <- as.matrix(rv$extracted_data_ml)
      if (nrow(m) != length(rownames(m))) rownames(m) <- paste0("S", seq_len(nrow(m)))
      if (ncol(m) != length(colnames(m))) colnames(m) <- paste0("G", seq_len(ncol(m)))
      write.csv(m, file, row.names = TRUE)
      write.csv(m, file.path(CSV_EXPORT_DIR(), "extracted_common_genes_data.csv"), row.names = TRUE)
    }
  )
}
