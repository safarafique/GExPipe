# ==============================================================================
# SERVER_GSEA.R - GSEA Analysis per Gene (separate plot + list for each gene)
# ==============================================================================
# Uses rv$ml_common_genes (or common_genes_de_wgcna), rv$batch_corrected.
# For each target gene: ranks genes by correlation with that gene, runs GSEA,
# stores in rv$gsea_results_by_gene. UI shows one section per gene (plot | list).
# ==============================================================================

server_gsea <- function(input, output, session, rv) {

  output$gsea_process_summary_ui <- renderUI({
    n_gene <- length(rv$gsea_results_by_gene)
    has_result <- !is.null(rv$gsea_result) && inherits(rv$gsea_result, "enrichResult") && nrow(rv$gsea_result@result) > 0
    if (n_gene == 0 && !has_result) {
      return(tags$p(style = "color: #6c757d; margin: 0;", icon("info-circle"), " Run GSEA to see process summary."))
    }
    msg <- if (n_gene > 0) paste0("GSEA run for ", n_gene, " target gene(s).") else "GSEA complete (pathway enrichment)."
    tags$div(
      style = "font-size: 14px; line-height: 1.6; color: #333;",
      tags$p(tags$strong("Step 14 complete."), " ", msg, " Enrichment plots and pathway lists above."))
  })

  output$gsea_placeholder_ui <- renderUI({
    expr <- rv$batch_corrected
    if (!is.null(expr) && (is.matrix(expr) || is.data.frame(expr)) && nrow(expr) > 0) return(NULL)
    tags$div(
      class = "alert alert-warning",
      icon("hand-point-right"),
      " Run Batch Correction (Step 5) first so batch-corrected expression is available. Then run ML (Step 10) or have common genes from Step 8."
    )
  })

  output$gsea_status_ui <- renderUI({
    if (!isTRUE(rv$gsea_complete)) return(NULL)
    n <- length(rv$gsea_target_genes)
    colls <- rv$gsea_collections_used
    coll_text <- if (!is.null(colls) && length(colls) > 0) paste0(" Collections: ", paste(colls, collapse = ", "), ".") else ""
    tags$div(
      class = "alert alert-success",
      icon("check-circle"),
      paste0(" GSEA complete for ", n, " gene(s).", coll_text, " View each gene's plot and pathway list below.")
    )
  })

  # ---- Gene set collection info panel ----
  output$gsea_collection_info_ui <- renderUI({
    selected <- input$gsea_collection
    if (is.null(selected) || length(selected) == 0) {
      return(tags$div(
        class = "alert alert-warning", style = "margin-top: 30px; padding: 10px;",
        icon("exclamation-triangle"), " Select at least one gene set collection."
      ))
    }
    labels <- c(H = "Hallmark", C5_BP = "GO:BP", C5_MF = "GO:MF", C2_KEGG = "KEGG",
                C2_REACTOME = "Reactome", C7 = "Immunologic", C6 = "Oncogenic")
    approx_sizes <- c(H = "~50", C5_BP = "~7,700", C5_MF = "~1,700", C2_KEGG = "~186",
                      C2_REACTOME = "~1,600", C7 = "~5,200", C6 = "~189")
    tags$div(
      style = "margin-top: 28px; padding: 10px; background: #eaf4fc; border-radius: 8px; border-left: 3px solid #3498db;",
      tags$p(tags$strong(icon("layer-group"), paste0(" ", length(selected), " collection(s) selected:")),
             style = "margin-bottom: 6px; font-size: 13px;"),
      tags$ul(style = "margin: 0; padding-left: 20px; font-size: 12px;",
        lapply(selected, function(s) {
          tags$li(tags$strong(labels[s]), tags$span(paste0(" (", approx_sizes[s], " gene sets)"), style = "color: #666;"))
        })
      ),
      if (length(selected) > 2) tags$p(
        icon("info-circle", style = "color: #f39c12;"),
        " Multiple large collections may increase computation time.",
        style = "margin-top: 6px; margin-bottom: 0; font-size: 11px; color: #856404;")
    )
  })

  observeEvent(input$run_gsea, {
    if (is.null(rv$batch_corrected)) {
      showNotification(
        tags$div(icon("exclamation-triangle"), tags$strong(" Step 5 required:"),
                 " Complete batch correction (Step 5) before running GSEA."),
        type = "error", duration = 6)
      return()
    }
    expr_mat <- as.matrix(rv$batch_corrected)
    if (nrow(expr_mat) < 10 || ncol(expr_mat) < 3) {
      showNotification("Batch-corrected expression too small (need at least 10 genes and 3 samples).", type = "error", duration = 6)
      return()
    }
    custom <- trimws(unlist(strsplit(gsub("[,;\n]+", ",", input$gsea_target_genes), ",")))
    custom <- custom[nzchar(custom)]
    if (length(custom) > 0) {
      target_genes <- custom
    } else if (!is.null(rv$roc_selected_genes) && length(rv$roc_selected_genes) > 0) {
      target_genes <- rv$roc_selected_genes
    } else if (!is.null(rv$ml_common_genes) && length(rv$ml_common_genes) > 0) {
      target_genes <- rv$ml_common_genes
    } else if (!is.null(rv$common_genes_de_wgcna) && length(rv$common_genes_de_wgcna) > 0) {
      target_genes <- rv$common_genes_de_wgcna
    } else {
      showNotification("No target genes. Select genes in Step 12 (ROC), run ML (Step 10), or type custom gene symbols.", type = "warning", duration = 8)
      return()
    }
    target_in_data <- intersect(target_genes, rownames(expr_mat))
    if (length(target_in_data) == 0) {
      showNotification("None of the target genes found in batch-corrected expression. Check gene symbols.", type = "error", duration = 8)
      return()
    }
    withProgress(message = "Running GSEA per gene...", value = 0.1, {
      tryCatch({
        # ---- Fetch selected gene set collections ----
        gsea_coll_map <- list(
          "H"           = list(cat = "H",  sub = NULL,          label = "Hallmark"),
          "C5_BP"       = list(cat = "C5", sub = "GO:BP",       label = "GO:BP"),
          "C5_MF"       = list(cat = "C5", sub = "GO:MF",       label = "GO:MF"),
          "C2_KEGG"     = list(cat = "C2", sub = "CP:KEGG",     label = "KEGG"),
          "C2_REACTOME" = list(cat = "C2", sub = "CP:REACTOME", label = "Reactome"),
          "C7"          = list(cat = "C7", sub = NULL,          label = "Immunologic"),
          "C6"          = list(cat = "C6", sub = NULL,          label = "Oncogenic")
        )
        selected_colls <- input$gsea_collection
        if (is.null(selected_colls) || length(selected_colls) == 0) selected_colls <- "H"

        gene_sets_list <- list()
        collection_lookup <- data.frame(gs_name = character(0), collection = character(0), stringsAsFactors = FALSE)

        for (coll_id in selected_colls) {
          params <- gsea_coll_map[[coll_id]]
          if (is.null(params)) next
          incProgress(0, detail = paste("Fetching", params$label, "gene sets..."))
          gs <- if (is.null(params$sub)) {
            msigdbr::msigdbr(species = "Homo sapiens", category = params$cat)
          } else {
            msigdbr::msigdbr(species = "Homo sapiens", category = params$cat, subcategory = params$sub)
          }
          gs_df <- gs %>% dplyr::select(.data$gs_name, .data$gene_symbol)
          gene_sets_list[[coll_id]] <- as.data.frame(gs_df)
          unique_gs <- unique(gs$gs_name)
          collection_lookup <- rbind(collection_lookup,
            data.frame(gs_name = unique_gs, collection = params$label, stringsAsFactors = FALSE))
        }

        gene_sets <- unique(do.call(rbind, gene_sets_list))
        rv$gsea_collection_lookup <- unique(collection_lookup)
        rv$gsea_collections_used <- unname(sapply(selected_colls, function(c) gsea_coll_map[[c]]$label))

        results_by_gene <- list()
        n_genes <- length(target_in_data)
        for (idx in seq_along(target_in_data)) {
          gene <- target_in_data[idx]
          incProgress(0.1 + 0.8 * (idx - 1) / n_genes, detail = paste("GSEA for", gene, "..."))
          if (!gene %in% rownames(expr_mat)) next
          signature_expr <- as.numeric(expr_mat[gene, ])
          expr_t <- t(expr_mat)
          cor_vec <- apply(expr_t, 2, function(x) {
            stats::cor(x, signature_expr, method = "pearson", use = "complete.obs")
          })
          gene_list <- sort(cor_vec, decreasing = TRUE)
          gsea_result <- clusterProfiler::GSEA(
            geneList = gene_list,
            TERM2GENE = gene_sets,
            pvalueCutoff = 1,
            verbose = FALSE,
            minGSSize = 10,
            maxGSSize = 500
          )
          results_by_gene[[gene]] <- gsea_result
        }
        rv$gsea_results_by_gene <- results_by_gene
        rv$gsea_target_genes <- target_in_data
        rv$gsea_result <- if (length(results_by_gene) > 0) results_by_gene[[1]] else NULL
        rv$gsea_complete <- TRUE
        incProgress(1, detail = "Done")
        showNotification(paste("GSEA complete for", length(results_by_gene), "gene(s)."), type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("GSEA error:", e$message), type = "error", duration = 10)
      })
    })
  })

  # Dynamic UI: one section per gene (plot left, pathway list right)
  output$gsea_per_gene_container <- renderUI({
    res_by_gene <- rv$gsea_results_by_gene
    if (is.null(res_by_gene) || length(res_by_gene) == 0) return(NULL)
    genes <- names(res_by_gene)
    pieces <- list()
    for (i in seq_along(genes)) {
      gene <- genes[i]
      id_plot <- paste0("gsea_plot_", i)
      id_table <- paste0("gsea_table_", i)
      left_col <- column(8,
        tags$p(tags$strong("Enrichment plot"), style = "margin-bottom: 6px;"),
        plotOutput(id_plot, height = "640px"))
      right_col <- column(4,
        tags$p(tags$strong("Enriched pathways (complete table)"), style = "margin-bottom: 6px; font-size: 12px;"),
        DT::dataTableOutput(id_table, height = "500px"))
      inner_row <- fluidRow(left_col, right_col)
      dl_btns <- tags$div(style = "margin-top: 10px;",
        downloadButton(paste0("download_gsea_plot_jpg_", i), tagList(icon("download"), " JPG (300 DPI)"), class = "btn-primary btn-sm", style = "margin-right: 4px;"),
        downloadButton(paste0("download_gsea_plot_pdf_", i), tagList(icon("download"), " PDF"), class = "btn-primary btn-sm", style = "margin-right: 8px;"),
        downloadButton(paste0("download_gsea_table_", i), tagList(icon("download"), " Table (", gene, ")"), class = "btn-success btn-sm"))
      b <- box(
        title = tags$span(icon("dna"), " GSEA for ", gene),
        width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        inner_row,
        dl_btns
      )
      pieces[[i]] <- fluidRow(b)
    }
    do.call(tagList, pieces)
  })

  # Dynamic outputs for each gene (plot, table, downloads)
  observe({
    res_by_gene <- rv$gsea_results_by_gene
    if (is.null(res_by_gene) || length(res_by_gene) == 0) return()
    genes <- names(res_by_gene)
    for (i in seq_along(genes)) {
      local({
        ii <- i
        g <- genes[ii]
        result <- res_by_gene[[g]]
        res_df <- result@result
        coll_lookup <- rv$gsea_collection_lookup
        top_terms <- if (nrow(res_df) > 0) {
          res_df %>% dplyr::arrange(.data$p.adjust) %>% head(10) %>% dplyr::pull(.data$ID)
        } else character(0)
        top_terms <- top_terms[top_terms %in% res_df$ID]

        output[[paste0("gsea_plot_", ii)]] <- renderPlot({
          if (nrow(res_df) == 0 || length(top_terms) == 0) {
            plot.new()
            text(0.5, 0.5, "No enriched pathways.", cex = 1.2, col = "gray40")
            return()
          }
          tryCatch({
            p <- enrichplot::gseaplot2(
              result,
              geneSetID = top_terms,
              title = paste("GSEA for", g),
              base_size = 11,
              subplots = 1:3,
              pvalue_table = FALSE,
              rel_heights = c(1.5, 0.3, 0.5)
            )
            if (inherits(p, "ggplot")) {
              p <- p + ggplot2::theme(plot.margin = ggplot2::margin(t = 50, r = 12, b = 12, l = 12, unit = "pt"))
              print(p)
            } else if (inherits(p, "list")) {
              do.call(gridExtra::grid.arrange, p)
            } else {
              grid::grid.draw(p)
            }
          }, error = function(e) {
            plot.new()
            text(0.5, 0.5, paste("Plot error:", conditionMessage(e)), cex = 0.9, col = "red")
          })
        }, width = 700, height = 640, res = 96)

        output[[paste0("gsea_table_", ii)]] <- DT::renderDataTable({
          out <- as.data.frame(res_df)
          if (nrow(out) == 0) return(DT::datatable(data.frame(Message = "No enriched pathways."), rownames = FALSE))
          # Add collection column if lookup available
          if (!is.null(coll_lookup) && nrow(coll_lookup) > 0 && "ID" %in% names(out)) {
            coll_match <- coll_lookup$collection[match(out$ID, coll_lookup$gs_name)]
            if (!all(is.na(coll_match))) out <- cbind(Collection = coll_match, out)
          }
          num_cols <- c("enrichmentScore", "NES", "pvalue", "p.adjust", "qvalue", "setSize", "rank")
          for (col in num_cols) if (col %in% names(out)) out[[col]] <- round(as.numeric(out[[col]]), 6)
          if ("NES" %in% names(out)) out$NES <- round(out$NES, 4)
          dt_opts <- list(pageLength = 15, scrollX = TRUE, scrollY = "420px", dom = "tpi", paging = TRUE)
          padj_idx <- which(names(out) == "p.adjust")
          if (length(padj_idx) > 0) dt_opts$order <- list(list(padj_idx - 1, "asc"))
          DT::datatable(out, options = dt_opts, rownames = FALSE, filter = "top", style = "bootstrap", class = "compact stripe")
        })

        output[[paste0("download_gsea_plot_jpg_", ii)]] <- downloadHandler(
          filename = function() paste0("GSEA_plot_", g, ".jpg"),
          content = function(file) {
            if (nrow(res_df) == 0 || length(top_terms) == 0) return()
            tryCatch({
              p <- enrichplot::gseaplot2(result, geneSetID = top_terms, title = paste("GSEA for", g),
                base_size = 14, subplots = 1:3, pvalue_table = FALSE, rel_heights = c(1.2, 0.3, 0.5))
              if (inherits(p, "ggplot")) {
                ggplot2::ggsave(file, plot = p, width = 10, height = 8, dpi = IMAGE_DPI, units = "in", bg = "white", device = "jpeg")
              } else if (inherits(p, "list")) {
                jpeg(file, width = 10, height = 8, res = IMAGE_DPI, units = "in", bg = "white", quality = 95)
                do.call(gridExtra::grid.arrange, p)
                dev.off()
              } else {
                jpeg(file, width = 10, height = 8, res = IMAGE_DPI, units = "in", bg = "white", quality = 95)
                grid::grid.draw(p)
                dev.off()
              }
            }, error = function(e) NULL)
          }
        )
        output[[paste0("download_gsea_plot_pdf_", ii)]] <- downloadHandler(
          filename = function() paste0("GSEA_plot_", g, ".pdf"),
          content = function(file) {
            if (nrow(res_df) == 0 || length(top_terms) == 0) return()
            tryCatch({
              p <- enrichplot::gseaplot2(result, geneSetID = top_terms, title = paste("GSEA for", g),
                base_size = 14, subplots = 1:3, pvalue_table = FALSE, rel_heights = c(1.2, 0.3, 0.5))
              if (inherits(p, "ggplot")) {
                ggplot2::ggsave(file, plot = p, width = 10, height = 8, device = "pdf", bg = "white")
              } else if (inherits(p, "list")) {
                pdf(file, width = 10, height = 8, bg = "white")
                do.call(gridExtra::grid.arrange, p)
                dev.off()
              } else {
                pdf(file, width = 10, height = 8, bg = "white")
                grid::grid.draw(p)
                dev.off()
              }
            }, error = function(e) NULL)
          }
        )

        output[[paste0("download_gsea_table_", ii)]] <- downloadHandler(
          filename = function() paste0("GSEA_results_", g, ".csv"),
          content = function(file) {
            out <- as.data.frame(res_df)
            if (!is.null(coll_lookup) && nrow(coll_lookup) > 0 && "ID" %in% names(out)) {
              coll_match <- coll_lookup$collection[match(out$ID, coll_lookup$gs_name)]
              if (!all(is.na(coll_match))) out <- cbind(Collection = coll_match, out)
            }
            write.csv(out, file, row.names = FALSE)
            write.csv(out, file.path(CSV_EXPORT_DIR(), paste0("GSEA_results_", g, ".csv")), row.names = FALSE)
          }
        )
      })
    }
  })

  output$gsea_results_table <- DT::renderDataTable({
    req(rv$gsea_result)
    res <- as.data.frame(rv$gsea_result@result)
    # Add collection column
    coll_lookup <- rv$gsea_collection_lookup
    if (!is.null(coll_lookup) && nrow(coll_lookup) > 0 && "ID" %in% names(res)) {
      coll_match <- coll_lookup$collection[match(res$ID, coll_lookup$gs_name)]
      if (!all(is.na(coll_match))) res <- cbind(Collection = coll_match, res)
    }
    res$pvalue <- round(res$pvalue, 6)
    res$p.adjust <- round(res$p.adjust, 6)
    res$NES <- round(res$NES, 4)
    DT::datatable(res, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE, filter = "top")
  })

  output$download_gsea_results <- downloadHandler(
    filename = function() "GSEA_results.csv",
    content = function(file) {
      req(rv$gsea_result)
      out <- as.data.frame(rv$gsea_result@result)
      coll_lookup <- rv$gsea_collection_lookup
      if (!is.null(coll_lookup) && nrow(coll_lookup) > 0 && "ID" %in% names(out)) {
        coll_match <- coll_lookup$collection[match(out$ID, coll_lookup$gs_name)]
        if (!all(is.na(coll_match))) out <- cbind(Collection = coll_match, out)
      }
      write.csv(out, file, row.names = FALSE)
      write.csv(out, file.path(CSV_EXPORT_DIR(), "GSEA_results.csv"), row.names = FALSE)
    }
  )
}
