# ==============================================================================
# SERVER_PPI.R - PPI Network (Common Genes) + Hub Gene Identification
# ==============================================================================

server_ppi <- function(input, output, session, rv) {

  # Applied gene set (updated only when user clicks "Run"); graphs use this
  app_ppi <- reactiveValues(applied_mode = NULL, applied_top_n = NULL, applied_manual_genes = NULL)

  observeEvent(input$ppi_apply_gene_set, {
    mode <- input$ppi_gene_select_mode
    if (is.null(mode)) mode <- "topn"
    app_ppi$applied_mode <- mode
    app_ppi$applied_top_n <- tryCatch(as.integer(input$ppi_top_hubs), error = function(e) 15L)
    app_ppi$applied_manual_genes <- if (mode == "manual" && length(input$ppi_manual_genes) > 0) input$ppi_manual_genes else NULL
    showNotification("Gene set applied. Network graphs updated.", type = "message", duration = 3)
  })

  # When PPI completes, apply current selection so graphs show without requiring Run first
  observeEvent(rv$ppi_complete, {
    if (!isTRUE(rv$ppi_complete)) return()
    mode <- input$ppi_gene_select_mode
    if (is.null(mode)) mode <- "topn"
    app_ppi$applied_mode <- mode
    app_ppi$applied_top_n <- tryCatch(as.integer(input$ppi_top_hubs), error = function(e) 15L)
    app_ppi$applied_manual_genes <- if (mode == "manual" && length(input$ppi_manual_genes) > 0) input$ppi_manual_genes else NULL
  })

  output$ppi_placeholder_ui <- renderUI({
    if (!is.null(rv$common_genes_de_wgcna) && length(rv$common_genes_de_wgcna) > 0) return(NULL)
    tags$div(
      class = "alert alert-warning",
      icon("hand-point-right"),
      " Go to Step 8 (Common Genes), click 'Compute Common Genes', then return here to run PPI analysis on those genes."
    )
  })

  observeEvent(input$run_ppi, {
    req(rv$common_genes_de_wgcna)
    if (length(rv$common_genes_de_wgcna) == 0) {
      showNotification("No common genes. Run Step 8 and compute common genes first.", type = "warning", duration = 6)
      return()
    }

    genes <- unique(trimws(rv$common_genes_de_wgcna))
    valid_genes <- genes[genes %in% keys(org.Hs.eg.db::org.Hs.eg.db, keytype = "SYMBOL")]
    if (length(valid_genes) == 0) {
      showNotification("No valid gene symbols found for STRINGdb.", type = "error", duration = 8)
      return()
    }

    st <- tryCatch(as.numeric(input$ppi_score_threshold), error = function(e) NA)
    score_threshold <- max(150, min(900, if (is.na(st)) 400 else st))
    nt <- tryCatch(as.integer(input$ppi_top_hubs), error = function(e) NA)
    n_top_hubs <- max(5, min(50, if (is.na(nt)) 15 else nt))

    withProgress(message = "PPI analysis (STRINGdb + hub genes)...", value = 0.1, {
      old_timeout <- getOption("timeout")
      options(timeout = 600)

      tryCatch({
        incProgress(0.15, detail = "Mapping to STRING...")
        string_db <- STRINGdb::STRINGdb$new(
          version = "11.5",
          species = 9606,
          score_threshold = score_threshold,
          input_directory = ""
        )
        mapped <- string_db$map(data.frame(SYMBOL = valid_genes), "SYMBOL", removeUnmappedRows = TRUE)
        if (nrow(mapped) == 0) stop("No genes mapped to STRING IDs.")

        incProgress(0.3, detail = "Fetching interactions...")
        id_col <- if ("STRING_id" %in% colnames(mapped)) "STRING_id" else if ("string_id" %in% colnames(mapped)) "string_id" else colnames(mapped)[1]
        valid_ids <- as.character(na.omit(mapped[[id_col]]))
        interactions <- string_db$get_interactions(valid_ids)
        options(timeout = old_timeout)

        if (is.null(interactions)) interactions <- data.frame()
        if (!is.data.frame(interactions)) interactions <- as.data.frame(interactions, stringsAsFactors = FALSE)
        if (nrow(interactions) == 0) stop("No PPI interactions found. Try lowering the score threshold.")

        incProgress(0.5, detail = "Building network...")
        from_col <- if ("from" %in% colnames(interactions)) "from" else if ("protein1" %in% colnames(interactions)) "protein1" else names(interactions)[1]
        to_col   <- if ("to" %in% colnames(interactions)) "to" else if ("protein2" %in% colnames(interactions)) "protein2" else names(interactions)[2]
        edge_list <- interactions[, c(from_col, to_col), drop = FALSE]
        colnames(edge_list) <- c("from", "to")
        # Vertex names must match edge endpoints (STRING IDs). First column = vertex name (STRING_id).
        # igraph requires unique vertex names: deduplicate by STRING_id (keep first row per ID).
        sym_col <- if ("SYMBOL" %in% colnames(mapped)) "SYMBOL" else if ("gene" %in% colnames(mapped)) "gene" else colnames(mapped)[min(2, ncol(mapped))]
        if (sym_col == id_col || is.na(sym_col)) sym_col <- id_col
        vertices_df <- data.frame(STRING_id = mapped[[id_col]], SYMBOL = mapped[[sym_col]], stringsAsFactors = FALSE)
        vertices_df <- vertices_df[!duplicated(vertices_df$STRING_id), ]
        if (nrow(vertices_df) == 0) stop("No unique vertices after removing duplicates.")
        g <- igraph::graph_from_data_frame(d = edge_list, directed = FALSE, vertices = vertices_df)
        if ("combined_score" %in% colnames(interactions)) E(g)$weight <- interactions$combined_score
        g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

        incProgress(0.6, detail = "Hub scores...")
        hub_scores <- data.frame(
          gene = igraph::V(g)$name,
          SYMBOL = if (is.null(igraph::V(g)$SYMBOL)) igraph::V(g)$name else igraph::V(g)$SYMBOL,
          Degree = igraph::degree(g),
          Betweenness = igraph::betweenness(g, normalized = TRUE),
          Closeness = suppressWarnings(igraph::closeness(g, normalized = TRUE)),
          Eigenvector = tryCatch(igraph::eigen_centrality(g)$vector, error = function(e) rep(0, igraph::vcount(g))),
          PageRank = igraph::page_rank(g)$vector,
          stringsAsFactors = FALSE
        )
        hub_scores$Closeness[is.nan(hub_scores$Closeness)] <- 0
        hub_scores$ClusterCoef <- igraph::transitivity(g, type = "local")
        hub_scores$ClusterCoef[is.nan(hub_scores$ClusterCoef)] <- 0

        incProgress(0.75, detail = "Consensus hubs...")
        n_top <- min(n_top_hubs, nrow(hub_scores))
        hub_rankings <- list(
          Degree = head(hub_scores[order(-hub_scores$Degree), ], n_top),
          Betweenness = head(hub_scores[order(-hub_scores$Betweenness), ], n_top),
          Closeness = head(hub_scores[order(-hub_scores$Closeness), ], n_top),
          Eigenvector = head(hub_scores[order(-hub_scores$Eigenvector), ], n_top),
          PageRank = head(hub_scores[order(-hub_scores$PageRank), ], n_top)
        )
        all_hub_genes <- unlist(lapply(hub_rankings, function(x) x$SYMBOL))
        hub_gene_counts <- table(all_hub_genes)
        consensus_hubs <- names(hub_gene_counts)[hub_gene_counts >= 3]

        igraph::V(g)$degree <- hub_scores$Degree
        igraph::V(g)$betweenness <- hub_scores$Betweenness
        igraph::V(g)$pagerank <- hub_scores$PageRank
        igraph::V(g)$is_hub <- igraph::V(g)$SYMBOL %in% consensus_hubs

        sym_attr <- igraph::V(g)$SYMBOL
        if (is.null(sym_attr)) sym_attr <- igraph::V(g)$name
        deg_vec <- igraph::degree(g)
        interactive_genes <- as.character(na.omit(unique(sym_attr[deg_vec > 0])))
        non_interactive_in_network <- as.character(na.omit(unique(sym_attr[deg_vec == 0])))
        unmapped_genes <- setdiff(valid_genes, mapped[[sym_col]])
        non_interactive_genes <- c(unmapped_genes, non_interactive_in_network)
        non_interactive_genes <- unique(non_interactive_genes)
        deg_for_status <- deg_vec[match(interactive_genes, sym_attr)]
        deg_for_status[is.na(deg_for_status)] <- 0
        status_df <- data.frame(
          Gene = c(interactive_genes, non_interactive_genes),
          Status = c(rep("Interactive", length(interactive_genes)), rep("Non-interactive", length(non_interactive_genes))),
          Degree = c(deg_for_status, rep(0, length(non_interactive_genes))),
          stringsAsFactors = FALSE
        )
        status_df <- status_df[order(-status_df$Degree, status_df$Gene), ]

        rv$ppi_graph <- g
        rv$ppi_hub_scores <- hub_scores
        rv$ppi_consensus_hubs <- consensus_hubs
        rv$ppi_hub_rankings <- hub_rankings
        rv$ppi_interactive_genes <- interactive_genes
        rv$ppi_non_interactive_genes <- non_interactive_genes
        rv$ppi_gene_status_table <- status_df
        rv$ppi_complete <- TRUE
        incProgress(1)
        n_int <- length(interactive_genes)
        n_non <- length(non_interactive_genes)
        showNotification(
          paste0("PPI complete: ", n_int, " interacting, ", n_non, " non-interacting. See list below; graphs use interacting genes."),
          type = "message", duration = 8
        )
      }, error = function(e) {
        options(timeout = old_timeout)
        showNotification(paste("PPI error:", e$message), type = "error", duration = 10)
      })
    })
  })

  output$ppi_status_ui <- renderUI({
    if (is.null(rv$ppi_complete) || !rv$ppi_complete) return(NULL)
    g <- rv$ppi_graph
    tags$div(
      class = "alert alert-success",
      icon("check-circle"),
      " Network: ", igraph::vcount(g), " nodes, ", igraph::ecount(g), " edges. ",
      "Interactive: ", length(rv$ppi_interactive_genes), " | Non-interactive: ", length(rv$ppi_non_interactive_genes), ". ",
      "Consensus hubs: ", length(rv$ppi_consensus_hubs), "."
    )
  })

  output$ppi_interactive_summary_ui <- renderUI({
    req(rv$ppi_gene_status_table)
    n_int <- sum(rv$ppi_gene_status_table$Status == "Interactive")
    n_non <- sum(rv$ppi_gene_status_table$Status == "Non-interactive")
    tags$div(
      class = "alert alert-info",
      tags$span(icon("link"), " Interactive: ", n_int, " genes (have PPI edges)"),
      tags$br(),
      tags$span(icon("unlink"), " Non-interactive: ", n_non, " genes (unmapped or no interactions)")
    )
  })

  output$ppi_gene_status_table <- DT::renderDataTable({
    req(rv$ppi_gene_status_table)
    DT::datatable(rv$ppi_gene_status_table, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE, filter = "top")
  })

  observe({
    req(rv$ppi_interactive_genes)
    n <- length(rv$ppi_interactive_genes)
    tryCatch(updateNumericInput(session, "ppi_top_hubs", max = min(50, max(1, n))), error = function(e) NULL)
  })

  output$ppi_manual_select_ui <- renderUI({
    req(rv$ppi_interactive_genes)
    if (is.null(input$ppi_gene_select_mode) || input$ppi_gene_select_mode != "manual") return(NULL)
    selectizeInput("ppi_manual_genes",
                   "Select genes (manual):",
                   choices = rv$ppi_interactive_genes,
                   selected = head(rv$ppi_interactive_genes, min(15, length(rv$ppi_interactive_genes))),
                   multiple = TRUE,
                   width = "100%")
  })

  ppi_selected_genes <- reactive({
    if (is.null(rv$ppi_graph) || is.null(rv$ppi_hub_scores) || is.null(rv$ppi_interactive_genes) || length(rv$ppi_interactive_genes) == 0)
      return(character(0))
    mode <- app_ppi$applied_mode
    if (is.null(mode)) return(character(0))
    if (mode == "hub") {
      hubs <- rv$ppi_consensus_hubs
      if (is.null(hubs) || length(hubs) == 0) return(character(0))
      return(intersect(hubs, rv$ppi_interactive_genes))
    }
    if (mode == "manual" && length(app_ppi$applied_manual_genes) > 0) {
      return(intersect(app_ppi$applied_manual_genes, rv$ppi_interactive_genes))
    }
    if (mode == "manual") return(character(0))
    n <- app_ppi$applied_top_n
    if (is.null(n)) n <- 15L
    n <- min(50, max(1, n), length(rv$ppi_interactive_genes))
    top <- rv$ppi_hub_scores[order(-rv$ppi_hub_scores$Degree), ]
    top <- top[top$SYMBOL %in% rv$ppi_interactive_genes, ]
    head(top$SYMBOL, n)
  })

  # Applied mode/n_val for plot titles and summary (depends on Run click)
  ppi_applied_mode_n <- reactive({
    list(mode = app_ppi$applied_mode, n_val = if (identical(app_ppi$applied_mode, "topn")) app_ppi$applied_top_n else NA_integer_)
  })

  ppi_subgraph <- reactive({
    if (is.null(rv$ppi_graph)) return(NULL)
    sel <- ppi_selected_genes()
    if (length(sel) == 0) return(NULL)
    g <- rv$ppi_graph
    sym_attr <- igraph::V(g)$SYMBOL
    if (is.null(sym_attr)) sym_attr <- igraph::V(g)$name
    vids <- igraph::V(g)[sym_attr %in% sel]
    if (length(vids) == 0) return(NULL)
    igraph::induced_subgraph(g, vids)
  })

  # Hub-only subgraph (consensus hubs in selected set); no Hub/Other legend
  ppi_subgraph_hub_only <- reactive({
    g_sub <- ppi_subgraph()
    if (is.null(g_sub)) return(NULL)
    is_hub <- igraph::V(g_sub)$is_hub
    if (is.null(is_hub) || !any(is_hub, na.rm = TRUE)) return(NULL)
    vids <- igraph::V(g_sub)[is_hub]
    if (length(vids) < 1) return(NULL)
    igraph::induced_subgraph(g_sub, vids)
  })

  # Other-only subgraph (top N connected genes that are not hubs); no Hub/Other legend
  ppi_subgraph_other_only <- reactive({
    g_sub <- ppi_subgraph()
    if (is.null(g_sub)) return(NULL)
    is_hub <- igraph::V(g_sub)$is_hub
    if (is.null(is_hub)) return(NULL)
    vids <- igraph::V(g_sub)[!is_hub]
    if (length(vids) < 1) return(NULL)
    igraph::induced_subgraph(g_sub, vids)
  })

  output$ppi_topn_ui <- renderUI({
    if (is.null(input$ppi_gene_select_mode) || input$ppi_gene_select_mode != "topn") return(NULL)
    n <- if (is.null(rv$ppi_interactive_genes)) 50L else length(rv$ppi_interactive_genes)
    numericInput("ppi_top_hubs", "Top N (by degree)", value = 15, min = 5, max = min(50, max(1, n)), step = 1)
  })

  # Placeholder message for network plots: run PPI first, or no interacting genes, or click Run
  ppi_plot_placeholder_msg <- reactive({
    if (is.null(rv$ppi_complete) || !rv$ppi_complete)
      return("Run PPI analysis first (Step 1). Then check interacting/non-interacting counts and list above.")
    if (length(rv$ppi_interactive_genes) == 0)
      return("No interacting genes. Lower STRING score threshold or check common genes; then re-run PPI.")
    if (is.null(app_ppi$applied_mode))
      return("Select gene set above (Hub genes only, Top N by degree, or manual) and click Run to generate network graphs.")
    mode <- app_ppi$applied_mode
    if (mode == "hub") return("Choose 'Hub genes only' above and click Run. If no hubs, run PPI and check consensus hubs.")
    "Choose gene set above (Hub genes only or Top N by degree) and click Run. Graphs and list below use the applied set."
  })

  output$ppi_view_summary_ui <- renderUI({
    g_sub <- ppi_subgraph()
    if (is.null(g_sub)) return(tags$div(class = "alert alert-warning", "Select gene set above and click Run to view the interaction network."))
    sel <- ppi_applied_mode_n()
    mode <- sel$mode
    n_val <- sel$n_val
    desc <- if (identical(mode, "hub")) paste0("Hub genes only: ", igraph::vcount(g_sub), " genes, ", igraph::ecount(g_sub), " edges.")
    else if (identical(mode, "topn") && !is.na(n_val)) paste0("Top ", n_val, " by degree: ", igraph::vcount(g_sub), " genes, ", igraph::ecount(g_sub), " edges.")
    else paste0(igraph::vcount(g_sub), " selected genes, ", igraph::ecount(g_sub), " edges.")
    tags$div(class = "alert alert-success", icon("chart-line"), " ", desc)
  })

  # Bright red/orange palette; edges and label spacing to reduce overlap
  ppi_col_hub    <- "#E53935"
  ppi_col_other  <- "#FF6F00"
  ppi_edge_col   <- grDevices::adjustcolor("#FF9800", alpha.f = 0.45)
  ppi_label_cex  <- 0.68
  ppi_label_dist <- 1.35
  ppi_frame_col  <- "#37474F"

  # Title and legend depend on selection: hub-only vs top N by degree
  ppi_plot_title <- function(prefix, mode, n_val) {
    if (identical(mode, "hub")) paste0(prefix, " (Hub genes only)")
    else if (identical(mode, "topn") && !is.na(n_val)) paste0(prefix, " (Top ", n_val, " by degree)")
    else paste0(prefix, " (Top genes by degree)")
  }

  ppi_plot_common <- function(g_sub) {
    if (is.null(g_sub) || igraph::vcount(g_sub) == 0) return(NULL)
    deg <- igraph::V(g_sub)$degree
    if (length(deg) == 0 || all(is.na(deg))) deg <- rep(1, igraph::vcount(g_sub))
    node_sizes <- scales::rescale(deg, to = c(5, 14))
    is_hub <- igraph::V(g_sub)$is_hub
    if (is.null(is_hub)) is_hub <- rep(FALSE, igraph::vcount(g_sub))
    all_hubs <- all(is_hub, na.rm = TRUE)
    node_colors <- if (all_hubs) rep(ppi_col_hub, igraph::vcount(g_sub)) else ifelse(is_hub, ppi_col_hub, ppi_col_other)
    legend_show <- if (all_hubs) list(legend = "Hub genes", col = ppi_col_hub) else list(legend = c("Hub", "Other"), col = c(ppi_col_hub, ppi_col_other))
    list(sizes = node_sizes, colors = node_colors, legend_show = legend_show)
  }

  ppi_safe_layout <- function(g_sub, method = "fr") {
    n <- igraph::vcount(g_sub)
    if (n <= 0) return(NULL)
    set.seed(123)
    if (n == 1) return(matrix(c(0, 0), 1, 2))
    if (n == 2) return(igraph::layout_in_circle(g_sub))
    if (method == "fr") return(igraph::layout_with_fr(g_sub, niter = 3500, start.temp = sqrt(n)))
    if (method == "kk") return(igraph::layout_with_kk(g_sub, maxiter = 500))
    igraph::layout_with_fr(g_sub, niter = 3500, start.temp = sqrt(n))
  }

  output$ppi_plot_traditional <- renderPlot(
    {
      tryCatch({
        g_sub <- ppi_subgraph()
        cm <- ppi_plot_common(g_sub)
        if (is.null(cm)) {
          plot.new()
          msg <- ppi_plot_placeholder_msg()
          text(0.5, 0.5, msg, cex = 0.95, col = "gray40", xpd = NA)
          return()
        }
        sel <- ppi_applied_mode_n()
        main_title <- ppi_plot_title("1. Fruchterman–Reingold", sel$mode, sel$n_val)
        layout_fr <- ppi_safe_layout(g_sub, "fr")
        if (is.null(layout_fr) || nrow(layout_fr) != igraph::vcount(g_sub)) { plot.new(); text(0.5, 0.5, "Layout failed.", cex = 1.2); return() }
        op <- par(mar = c(1.5, 1.5, 2.5, 1.5), bg = "#FAFAFA", cex.main = 1.1, col.main = "#37474F", xpd = NA)
        on.exit(par(op), add = TRUE)
        igraph::plot.igraph(g_sub,
                            layout = layout_fr,
                            vertex.color = cm$colors,
                            vertex.size = cm$sizes,
                            vertex.label = igraph::V(g_sub)$SYMBOL,
                            vertex.label.cex = ppi_label_cex,
                            vertex.label.dist = ppi_label_dist,
                            vertex.label.color = "#263238",
                            vertex.label.family = "sans",
                            vertex.frame.color = ppi_frame_col,
                            vertex.frame.width = 0.8,
                            edge.color = ppi_edge_col,
                            edge.width = 0.65,
                            main = main_title)
        legend("bottomright", legend = cm$legend_show$legend, pch = 21, pt.bg = cm$legend_show$col, pt.cex = 2, bty = "n", text.col = "#37474F")
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Plot error:", conditionMessage(e)), cex = 0.9, col = "red")
      })
    },
    width = 560, height = 520, res = 96
  )

  output$ppi_plot_circular <- renderPlot(
    {
      tryCatch({
        g_sub <- ppi_subgraph()
        cm <- ppi_plot_common(g_sub)
        if (is.null(cm)) { plot.new(); text(0.5, 0.5, ppi_plot_placeholder_msg(), cex = 0.95, col = "gray40"); return() }
        sel <- ppi_applied_mode_n()
        main_title <- ppi_plot_title("2. Circular by Degree", sel$mode, sel$n_val)
        degree_order <- order(igraph::V(g_sub)$degree, decreasing = TRUE)
        layout_circle <- igraph::layout_in_circle(g_sub, order = degree_order)
        op <- par(mar = c(1.5, 1.5, 2.5, 1.5), bg = "#FAFAFA", cex.main = 1.1, col.main = "#37474F", xpd = NA)
        on.exit(par(op), add = TRUE)
        igraph::plot.igraph(g_sub,
                            layout = layout_circle,
                            vertex.color = cm$colors,
                            vertex.size = cm$sizes,
                            vertex.label = igraph::V(g_sub)$SYMBOL,
                            vertex.label.cex = ppi_label_cex,
                            vertex.label.dist = ppi_label_dist,
                            vertex.label.color = "#263238",
                            vertex.label.family = "sans",
                            vertex.frame.color = ppi_frame_col,
                            vertex.frame.width = 0.8,
                            edge.color = ppi_edge_col,
                            edge.width = 0.65,
                            main = main_title)
        legend("bottomright", legend = cm$legend_show$legend, pch = 21, pt.bg = cm$legend_show$col, pt.cex = 2, bty = "n", text.col = "#37474F")
      }, error = function(e) { plot.new(); text(0.5, 0.5, paste("Error:", conditionMessage(e)), cex = 0.9, col = "red") })
    },
    width = 560, height = 520, res = 96
  )

  output$ppi_plot_ggraph <- renderPlot(
    {
      tryCatch({
        g_sub <- ppi_subgraph()
        if (is.null(g_sub) || igraph::vcount(g_sub) == 0) {
          plot.new()
          text(0.5, 0.5, ppi_plot_placeholder_msg(), cex = 0.95, col = "gray40")
          return()
        }
        sel <- ppi_applied_mode_n()
        main_title <- ppi_plot_title("3. ggraph", sel$mode, sel$n_val)
        tg <- tidygraph::as_tbl_graph(g_sub)
        tg <- tg %>% tidygraph::activate(nodes) %>% dplyr::mutate(hub_status = ifelse(is_hub, "Hub", "Other"))
        set.seed(123)
        p <- ggraph::ggraph(tg, layout = "fr", niter = 3500) +
          ggraph::geom_edge_link(alpha = 0.4, color = "#FF9800", width = 0.55) +
          ggraph::geom_node_point(aes(size = degree, color = hub_status), alpha = 0.92, stroke = 0.8) +
          ggraph::geom_node_text(aes(label = SYMBOL), repel = TRUE, size = 3, fontface = "bold", color = "#263238", box.padding = 0.7, point.padding = 0.45, max.overlaps = 25) +
          ggplot2::scale_color_manual(values = c("Hub" = ppi_col_hub, "Other" = ppi_col_other)) +
          ggplot2::scale_size_continuous(range = c(5, 14)) +
          ggplot2::labs(title = main_title, subtitle = paste0(igraph::vcount(g_sub), " nodes, ", igraph::ecount(g_sub), " edges"), color = "Type", size = "Degree") +
          ggraph::theme_graph(base_family = "sans", background = "#FAFAFA") +
          ggplot2::theme(
            legend.position = "right",
            plot.title = ggplot2::element_text(face = "bold", size = 13, color = "#37474F"),
            plot.subtitle = ggplot2::element_text(size = 9, color = "#546E7A"),
            plot.margin = ggplot2::margin(4, 4, 4, 4, "pt")
          )
        print(p)
      }, error = function(e) { plot.new(); text(0.5, 0.5, paste("Error:", conditionMessage(e)), cex = 0.9, col = "red") })
    },
    width = 560, height = 520, res = 96
  )

  output$ppi_plot_kamada <- renderPlot(
    {
      tryCatch({
        g_sub <- ppi_subgraph()
        cm <- ppi_plot_common(g_sub)
        if (is.null(cm)) { plot.new(); text(0.5, 0.5, ppi_plot_placeholder_msg(), cex = 0.95, col = "gray40"); return() }
        sel <- ppi_applied_mode_n()
        main_title <- ppi_plot_title("4. Kamada–Kawai", sel$mode, sel$n_val)
        layout_kk <- ppi_safe_layout(g_sub, "kk")
        if (is.null(layout_kk) || nrow(layout_kk) != igraph::vcount(g_sub)) { plot.new(); text(0.5, 0.5, "Layout failed.", cex = 1.2); return() }
        op <- par(mar = c(1.5, 1.5, 2.5, 1.5), bg = "#FAFAFA", cex.main = 1.1, col.main = "#37474F", xpd = NA)
        on.exit(par(op), add = TRUE)
        igraph::plot.igraph(g_sub,
                            layout = layout_kk,
                            vertex.color = cm$colors,
                            vertex.size = cm$sizes,
                            vertex.label = igraph::V(g_sub)$SYMBOL,
                            vertex.label.cex = ppi_label_cex,
                            vertex.label.dist = ppi_label_dist,
                            vertex.label.color = "#263238",
                            vertex.label.family = "sans",
                            vertex.frame.color = ppi_frame_col,
                            vertex.frame.width = 0.8,
                            edge.color = ppi_edge_col,
                            edge.width = 0.65,
                            main = main_title)
        legend("bottomright", legend = cm$legend_show$legend, pch = 21, pt.bg = cm$legend_show$col, pt.cex = 2, bty = "n", text.col = "#37474F")
      }, error = function(e) { plot.new(); text(0.5, 0.5, paste("Error:", conditionMessage(e)), cex = 0.9, col = "red") })
    },
    width = 560, height = 520, res = 96
  )

  # Single plot: Hub-only or Other-only network depending on applied gene set
  output$ppi_hub_or_other_title_ui <- renderUI({
    mode <- app_ppi$applied_mode
    if (mode == "hub") {
      tags$h5(icon("circle-nodes"), " Hub genes only (consensus hubs)", style = "margin-top: 4px; margin-bottom: 8px; color: #B71C1C; font-size: 13px;")
    } else if (mode == "topn") {
      tags$h5(icon("circle"), " Other genes only (top N connected, non-hub)", style = "margin-top: 4px; margin-bottom: 8px; color: #1565C0; font-size: 13px;")
    } else {
      tags$p(icon("info-circle"), " Choose 'Hub genes only' or 'Top N by degree' above and click Run to see this network.", style = "margin-bottom: 8px; font-size: 12px; color: #666;")
    }
  })

  output$ppi_plot_hub_or_other <- renderPlot(
    {
      mode <- app_ppi$applied_mode
      tryCatch({
        if (mode == "hub") {
          g <- ppi_subgraph_hub_only()
          main <- "Hub genes only (consensus hubs)"
          empty_msg <- "No hub genes in selected set.\nSelect more genes (Top N) or run PPI to get consensus hubs."
          node_col <- ppi_col_hub
        } else if (mode == "topn") {
          g <- ppi_subgraph_other_only()
          main <- "Other genes only (top N connected, non-hub)"
          empty_msg <- "No non-hub genes in selected set.\nAll selected genes are consensus hubs."
          node_col <- ppi_col_other
        } else {
          plot.new()
          text(0.5, 0.5, "Choose 'Hub genes only' or 'Top N by degree' above to see this network.", cex = 1, col = "gray40", xpd = NA)
          return()
        }
        if (is.null(g) || igraph::vcount(g) == 0) {
          plot.new()
          text(0.5, 0.5, empty_msg, cex = 0.95, col = "gray40", xpd = NA)
          return()
        }
        deg <- igraph::V(g)$degree
        if (length(deg) == 0 || all(is.na(deg))) deg <- rep(1, igraph::vcount(g))
        node_sizes <- scales::rescale(deg, to = c(6, 18))
        layout_fr <- ppi_safe_layout(g, "fr")
        if (is.null(layout_fr) || nrow(layout_fr) != igraph::vcount(g)) { plot.new(); text(0.5, 0.5, "Layout failed.", cex = 1.2); return() }
        op <- par(mar = c(1.5, 1.5, 2.5, 1.5), bg = "#FAFAFA", cex.main = 1.1, col.main = "#37474F", xpd = NA)
        on.exit(par(op), add = TRUE)
        igraph::plot.igraph(g,
                            layout = layout_fr,
                            vertex.color = node_col,
                            vertex.size = node_sizes,
                            vertex.label = igraph::V(g)$SYMBOL,
                            vertex.label.cex = ppi_label_cex,
                            vertex.label.dist = ppi_label_dist,
                            vertex.label.color = "#263238",
                            vertex.label.family = "sans",
                            vertex.frame.color = ppi_frame_col,
                            vertex.frame.width = 0.8,
                            edge.color = ppi_edge_col,
                            edge.width = 0.65,
                            main = main)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Plot error:", conditionMessage(e)), cex = 0.9, col = "red")
      })
    },
    width = 560, height = 520, res = 96
  )

  output$ppi_hub_table <- DT::renderDataTable({
    req(rv$ppi_hub_scores)
    df <- rv$ppi_hub_scores
    df$Betweenness <- round(df$Betweenness, 4)
    df$Closeness <- round(df$Closeness, 4)
    df$Eigenvector <- round(df$Eigenvector, 4)
    df$PageRank <- round(df$PageRank, 4)
    df$ClusterCoef <- round(df$ClusterCoef, 4)
    DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE, filter = "top")
  })

  output$ppi_consensus_ui <- renderUI({
    req(rv$ppi_consensus_hubs)
    hubs <- rv$ppi_consensus_hubs
    if (length(hubs) == 0) return(tags$p("No consensus hub genes (none appear in 3+ methods)."))
    tags$p(tags$strong(paste(hubs, collapse = ", ")))
  })

  output$download_ppi_gene_status <- downloadHandler(
    filename = function() "PPI_Interactive_NonInteractive_Genes.csv",
    content = function(file) {
      req(rv$ppi_gene_status_table)
      write.csv(rv$ppi_gene_status_table, file, row.names = FALSE)
      write.csv(rv$ppi_gene_status_table, file.path(CSV_EXPORT_DIR(), "PPI_Interactive_NonInteractive_Genes.csv"), row.names = FALSE)
    }
  )

  output$download_ppi_hub_scores <- downloadHandler(
    filename = function() "PPI_Hub_Gene_Scores.csv",
    content = function(file) {
      req(rv$ppi_hub_scores)
      write.csv(rv$ppi_hub_scores, file, row.names = FALSE)
      write.csv(rv$ppi_hub_scores, file.path(CSV_EXPORT_DIR(), "PPI_Hub_Gene_Scores.csv"), row.names = FALSE)
    }
  )

  output$download_ppi_consensus <- downloadHandler(
    filename = function() "Consensus_Hub_Genes.csv",
    content = function(file) {
      req(rv$ppi_consensus_hubs)
      hub_rankings <- rv$ppi_hub_rankings
      all_hub_genes <- unlist(lapply(hub_rankings, function(x) x$SYMBOL))
      counts <- table(all_hub_genes)
      consensus <- rv$ppi_consensus_hubs
      df <- data.frame(Gene = consensus, Frequency = as.numeric(counts[consensus]))
      write.csv(df, file, row.names = FALSE)
      write.csv(df, file.path(CSV_EXPORT_DIR(), "Consensus_Hub_Genes.csv"), row.names = FALSE)
    }
  )

  ppi_network_plot_to_file <- function(file, dev_fun, ...) {
    req(rv$ppi_graph)
    g <- rv$ppi_graph
    dev_fun(file, ...)
    op <- par(mar = c(2, 2, 3, 2), cex.main = 1.2, col.main = "#37474F")
    on.exit(par(op), add = TRUE)
    set.seed(123)
    layout_fr <- igraph::layout_with_fr(g, niter = 1500)
    node_colors <- ifelse(igraph::V(g)$is_hub, ppi_col_hub, ppi_col_other)
    node_sizes <- scales::rescale(igraph::V(g)$degree, to = c(6, 18))
    igraph::plot.igraph(g, layout = layout_fr, vertex.color = node_colors, vertex.size = node_sizes,
                        vertex.label = ifelse(igraph::V(g)$is_hub, igraph::V(g)$SYMBOL, NA),
                        vertex.label.cex = ppi_label_cex, vertex.label.dist = ppi_label_dist,
                        vertex.label.color = "#263238", vertex.frame.color = ppi_frame_col, vertex.frame.width = 0.8,
                        edge.color = ppi_edge_col, edge.width = 0.65, main = "PPI Network (Hub genes)")
    legend("bottomright", legend = c("Hub", "Other"), pch = 21, pt.bg = c(ppi_col_hub, ppi_col_other), pt.cex = 2, bty = "n", text.col = "#37474F")
    dev.off()
  }

  output$download_ppi_network_png <- downloadHandler(
    filename = function() "PPI_Network.png",
    content = function(file) {
      ppi_network_plot_to_file(file, function(f, ...) png(f, width = 7, height = 7, res = IMAGE_DPI, units = "in", bg = "#FAFAFA"))
    }
  )
  output$download_ppi_network_jpg <- downloadHandler(
    filename = function() "PPI_Network.jpg",
    content = function(file) {
      ppi_network_plot_to_file(file, function(f, ...) jpeg(f, width = 7, height = 7, res = IMAGE_DPI, units = "in", bg = "#FAFAFA", quality = 95))
    }
  )
  output$download_ppi_network_pdf <- downloadHandler(
    filename = function() "PPI_Network.pdf",
    content = function(file) {
      ppi_network_plot_to_file(file, function(f, ...) pdf(f, width = 7, height = 7, bg = "#FAFAFA"))
    }
  )

  # ---------- Interactive gene list (end of page, for ML / downstream) — uses selected set (hub or top N) ----------
  ppi_interactive_list_df <- reactive({
    req(rv$ppi_hub_scores)
    sel <- ppi_selected_genes()
    if (length(sel) == 0) return(data.frame(Gene = character(), Degree = numeric(), Betweenness = numeric(), Closeness = numeric(), Eigenvector = numeric(), PageRank = numeric(), ClusterCoef = numeric()))
    df <- rv$ppi_hub_scores[rv$ppi_hub_scores$SYMBOL %in% sel, ]
    df <- df[order(-df$Degree), c("SYMBOL", "Degree", "Betweenness", "Closeness", "Eigenvector", "PageRank", "ClusterCoef")]
    colnames(df)[1] <- "Gene"
    df
  })

  output$ppi_interactive_list_summary_ui <- renderUI({
    sel <- ppi_selected_genes()
    n <- length(sel)
    mode <- app_ppi$applied_mode
    mode_txt <- if (mode == "hub") "Hub genes" else if (mode == "topn") "Top N by degree" else if (mode == "manual") "Manual selection" else "—"
    tags$div(
      class = "alert alert-success",
      icon("link"),
      tags$strong(" Genes for next step (", mode_txt, "): ", n),
      " — This list is used for Extract Data for ML and downstream. Graphs above show the same set."
    )
  })

  output$ppi_interactive_list_table <- DT::renderDataTable({
    req(ppi_interactive_list_df())
    df <- ppi_interactive_list_df()
    df$Betweenness <- round(df$Betweenness, 4)
    df$Closeness <- round(df$Closeness, 4)
    df$Eigenvector <- round(df$Eigenvector, 4)
    df$PageRank <- round(df$PageRank, 4)
    df$ClusterCoef <- round(df$ClusterCoef, 4)
    DT::datatable(df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE, filter = "top")
  })

  output$download_ppi_interactive_genes <- downloadHandler(
    filename = function() "Interactive_Genes_List.csv",
    content = function(file) {
      req(ppi_interactive_list_df())
      write.csv(ppi_interactive_list_df(), file, row.names = FALSE)
      write.csv(ppi_interactive_list_df(), file.path(CSV_EXPORT_DIR(), "Interactive_Genes_List.csv"), row.names = FALSE)
    }
  )

  output$ppi_extract_expr_ui <- renderUI({
    sel <- ppi_selected_genes()
    if (length(sel) == 0 && is.null(rv$ppi_interactive_genes)) return(NULL)
    if (is.null(rv$datExpr)) {
      return(tags$span(
        style = "margin-left: 10px; color: #6c757d;",
        tags$small("(Run Step 7 (WGCNA) → Data Preparation to get expression data, then you can extract samples × genes matrix here.)")
      ))
    }
    datExpr <- as.matrix(rv$datExpr)
    genes_in_expr <- colnames(datExpr)
    inter <- if (length(sel) > 0) sel else rv$ppi_interactive_genes
    found <- intersect(inter, genes_in_expr)
    n_found <- length(found)
    if (n_found == 0) {
      return(tags$span(style = "margin-left: 10px; color: #856404;", tags$small("No interactive genes found in WGCNA expression matrix (check gene names).")))
    }
    tags$span(
      style = "margin-left: 10px;",
      downloadButton("download_ppi_extracted_expr",
                     tagList(icon("download"), " Expression data (interacting genes, ", n_found, " genes × samples)"),
                     class = "btn-info")
    )
  })

  output$download_ppi_extracted_expr <- downloadHandler(
    filename = function() "extracted_interacting_genes_data.csv",
    content = function(file) {
      req(rv$datExpr, rv$ppi_interactive_genes)
      datExpr <- as.matrix(rv$datExpr)
      genes_in_expr <- colnames(datExpr)
      inter <- rv$ppi_interactive_genes
      keep <- intersect(inter, genes_in_expr)
      if (length(keep) == 0) {
        write.csv(data.frame(message = "No matching genes"), file)
        write.csv(data.frame(message = "No matching genes"), file.path(CSV_EXPORT_DIR(), "extracted_interacting_genes_data.csv"))
        return()
      }
      extracted <- datExpr[, keep, drop = FALSE]
      m <- as.matrix(extracted)
      if (nrow(m) != length(rownames(m))) rownames(m) <- paste0("S", seq_len(nrow(m)))
      if (ncol(m) != length(colnames(m))) colnames(m) <- paste0("G", seq_len(ncol(m)))
      write.csv(m, file, row.names = TRUE)
      write.csv(m, file.path(CSV_EXPORT_DIR(), "extracted_interacting_genes_data.csv"), row.names = TRUE)
    }
  )

  # ---------- Network Centrality–Weighted ML ----------
  observeEvent(input$ppi_apply_centrality, {
    req(rv$ppi_hub_scores)
    df <- rv$ppi_hub_scores
    mode <- input$ppi_centrality_mode
    if (is.null(mode)) mode <- "filter"
    if (mode == "hub") {
      hubs <- rv$ppi_consensus_hubs
      if (is.null(hubs) || length(hubs) == 0) {
        showNotification("No consensus hub genes. Run PPI and ensure hub analysis completed.", type = "warning", duration = 5)
        return()
      }
      inter <- rv$ppi_interactive_genes
      if (!is.null(inter) && length(inter) > 0) hubs <- intersect(hubs, inter)
      rv$ppi_centrality_filtered_genes <- hubs
      rv$ppi_centrality_weights <- NULL
      tab <- df[df$SYMBOL %in% hubs, ]
      tab <- tab[order(-tab$Degree), ]
      tab$Rank <- seq_len(nrow(tab))
      tab$Weight <- NA
      rv$ppi_centrality_table <- tab
      showNotification(paste("Hub genes only (", length(hubs), " genes) selected for ML. Click 'Extract Data for ML' to use them."), type = "message", duration = 4)
      return()
    }
    n_top <- max(5, min(100, as.integer(input$ppi_centrality_top_n)))
    metric <- input$ppi_centrality_metric
    if (is.null(metric)) metric <- "Degree"
    if (metric == "Composite") {
      df$Rank_Degree <- rank(-df$Degree)
      df$Rank_Betweenness <- rank(-df$Betweenness)
      df$Rank_Closeness <- rank(-df$Closeness)
      df$Composite_Rank <- (df$Rank_Degree + df$Rank_Betweenness + df$Rank_Closeness) / 3
      ord <- order(df$Composite_Rank)
    } else {
      ord <- order(-df[[metric]])
    }
    df_ord <- df[ord, ]
    top_genes <- head(df_ord$SYMBOL, n_top)
    if (mode == "filter") {
      rv$ppi_centrality_filtered_genes <- top_genes
      rv$ppi_centrality_weights <- NULL
    } else {
      rv$ppi_centrality_filtered_genes <- NULL
      deg_norm <- (df_ord$Degree - min(df_ord$Degree)) / (max(df_ord$Degree) - min(df_ord$Degree) + 1e-10)
      bet_norm <- (df_ord$Betweenness - min(df_ord$Betweenness)) / (max(df_ord$Betweenness) - min(df_ord$Betweenness) + 1e-10)
      close_norm <- (df_ord$Closeness - min(df_ord$Closeness)) / (max(df_ord$Closeness) - min(df_ord$Closeness) + 1e-10)
      w <- (deg_norm + bet_norm + close_norm) / 3
      rv$ppi_centrality_weights <- setNames(w, df_ord$SYMBOL)
    }
    tab <- df_ord
    tab$Rank <- seq_len(nrow(tab))
    if (mode == "weight") {
      tab$Weight <- (tab$Degree - min(tab$Degree)) / (max(tab$Degree) - min(tab$Degree) + 1e-10)
      bet_n <- (tab$Betweenness - min(tab$Betweenness)) / (max(tab$Betweenness) - min(tab$Betweenness) + 1e-10)
      close_n <- (tab$Closeness - min(tab$Closeness)) / (max(tab$Closeness) - min(tab$Closeness) + 1e-10)
      tab$Weight <- (tab$Weight + bet_n + close_n) / 3
    } else {
      tab$Weight <- NA
    }
    rv$ppi_centrality_table <- tab
    msg <- if (mode == "filter") paste("Top", length(top_genes), "genes by", metric, "selected for ML. Click 'Extract Data for ML' to use them.") else paste("Centrality weights computed for", nrow(tab), "genes.")
    showNotification(msg, type = "message", duration = 4)
  })

  output$ppi_centrality_status_ui <- renderUI({
    if (is.null(rv$ppi_centrality_table)) return(NULL)
    n <- nrow(rv$ppi_centrality_table)
    mode_txt <- if (!is.null(rv$ppi_centrality_filtered_genes) && length(rv$ppi_centrality_filtered_genes) > 0)
      paste0("Selected for ML: ", length(rv$ppi_centrality_filtered_genes), " genes (filter or hub).") else "Weights computed (no gene filter)."
    tags$div(
      class = "alert alert-success",
      icon("check-circle"),
      " ", mode_txt, " Use 'Extract Data for ML' below to send this set to the ML tab."
    )
  })

  output$ppi_centrality_table <- DT::renderDataTable({
    req(rv$ppi_centrality_table)
    df <- rv$ppi_centrality_table[, c("SYMBOL", "Degree", "Betweenness", "Closeness", "Weight", "Rank")]
    df$Betweenness <- round(df$Betweenness, 4)
    df$Closeness <- round(df$Closeness, 4)
    if (!all(is.na(df$Weight))) df$Weight <- round(df$Weight, 4)
    colnames(df)[1] <- "Gene"
    DT::datatable(df, options = list(pageLength = 15), rownames = FALSE)
  })

  output$download_ppi_centrality_weighted <- downloadHandler(
    filename = function() "PPI_centrality_weighted_genes.csv",
    content = function(file) {
      req(rv$ppi_centrality_table)
      df <- rv$ppi_centrality_table[, c("SYMBOL", "Degree", "Betweenness", "Closeness", "Weight", "Rank")]
      colnames(df)[1] <- "Gene"
      write.csv(df, file, row.names = FALSE)
      write.csv(df, file.path(CSV_EXPORT_DIR(), "PPI_centrality_weighted_genes.csv"), row.names = FALSE)
    }
  )

  # ---------- Extract Data for ML (stores rv$extracted_data_ml for ML tab) ----------
  # Uses the gene set from "5. Network Centrality-Weighted ML" when user has applied it (filter top N or hub); otherwise graph-section selection or all interactive.
  observeEvent(input$extract_ml_data, {
    if (is.null(rv$datExpr)) {
      showNotification("Run PPI analysis and ensure WGCNA data (Step 7) is prepared.", type = "warning", duration = 6)
      return()
    }
    genes_for_ml <- if (!is.null(rv$ppi_centrality_filtered_genes) && length(rv$ppi_centrality_filtered_genes) > 0) {
      rv$ppi_centrality_filtered_genes
    } else {
      ppi_selected_genes()
    }
    if (length(genes_for_ml) == 0) {
      genes_for_ml <- rv$ppi_interactive_genes
    }
    if (is.null(genes_for_ml) || length(genes_for_ml) == 0) {
      showNotification("Run PPI analysis first. Choose Hub genes only or Top N by degree above.", type = "warning", duration = 6)
      return()
    }
    datExpr <- as.matrix(rv$datExpr)
    genes_in_expr <- colnames(datExpr)
    inter <- unique(trimws(genes_for_ml))
    keep <- intersect(inter, genes_in_expr)
    if (length(keep) == 0) {
      showNotification("No interacting genes found in datExpr. Check gene names.", type = "error", duration = 6)
      return()
    }
    rv$extracted_data_ml <- datExpr[, keep, drop = FALSE]
    showNotification(paste("Extracted", nrow(rv$extracted_data_ml), "samples ×", ncol(rv$extracted_data_ml), "genes for ML."), type = "message", duration = 5)
  })

  output$ppi_extracted_ml_status_ui <- renderUI({
    if (is.null(rv$extracted_data_ml)) return(NULL)
    tags$div(
      class = "alert alert-success",
      icon("check-circle"),
      " Extracted data ready: ", nrow(rv$extracted_data_ml), " samples × ", ncol(rv$extracted_data_ml), " genes."
    )
  })

  output$ppi_extracted_ml_genes_ui <- renderUI({
    if (is.null(rv$extracted_data_ml)) return(NULL)
    genes <- colnames(rv$extracted_data_ml)
    tags$div(
      tags$h5("Genes in extracted matrix (for ML)", style = "margin-top: 15px;"),
      tags$p(paste(genes, collapse = ", "), style = "font-size: 12px; word-break: break-all; max-height: 120px; overflow-y: auto;")
    )
  })

  output$ppi_download_extracted_ml_ui <- renderUI({
    if (is.null(rv$extracted_data_ml)) return(NULL)
    downloadButton("download_extracted_ml_csv",
                   tagList(icon("download"), " Download Extracted Data (CSV)"),
                   class = "btn-info")
  })

  output$download_extracted_ml_csv <- downloadHandler(
    filename = function() "extracted_interacting_genes_data.csv",
    content = function(file) {
      req(rv$extracted_data_ml)
      m <- as.matrix(rv$extracted_data_ml)
      if (nrow(m) != length(rownames(m))) rownames(m) <- paste0("S", seq_len(nrow(m)))
      if (ncol(m) != length(colnames(m))) colnames(m) <- paste0("G", seq_len(ncol(m)))
      write.csv(m, file, row.names = TRUE)
      write.csv(m, file.path(CSV_EXPORT_DIR(), "extracted_interacting_genes_data.csv"), row.names = TRUE)
    }
  )
}
