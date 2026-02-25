# ==============================================================================
# SERVER_QC.R - Step 2: QC & Visualization Module
# ==============================================================================

server_qc <- function(input, output, session, rv) {
  
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
    n <- if (!is.null(rv$combined_expr_raw)) ncol(rv$combined_expr_raw) else 0
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
    total_genes_per_dataset <- sapply(rv$all_genes_list, length)
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
          lapply(names(rv$all_genes_list), function(gse) {
            total <- length(rv$all_genes_list[[gse]])
            pct <- round(100 * common_count / total, 1)
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
  # VENN & UPSET
  # ==============================================================================
  
  output$venn_plot <- renderPlot({
    req(rv$all_genes_list)
    if (length(rv$all_genes_list) < 2) {
      plot.new()
      text(0.5, 0.5, "Need 2+ datasets for Venn diagram", cex = 1.8, col = "gray40")
      return()
    }
    
    # Use original gene lists (before filtering to common genes)
    sets <- rv$all_genes_list[1:min(5, length(rv$all_genes_list))]
    
    # Ensure sets are lists of character vectors and remove duplicates
    sets <- lapply(sets, function(x) {
      if (is.null(x)) return(character(0))
      x <- as.character(x)
      x <- x[!is.na(x) & x != ""]  # Remove NA and empty strings
      unique(x)  # Remove duplicates within each set
    })
    
    # Remove any empty sets
    sets <- sets[sapply(sets, length) > 0]
    
    # Ensure all sets have names
    if (is.null(names(sets)) || any(names(sets) == "")) {
      names(sets) <- paste0("Dataset_", seq_along(sets))
    }
    
    if (length(sets) < 2) {
      plot.new()
      text(0.5, 0.5, "Need 2+ datasets with genes for Venn diagram", cex = 1.8, col = "gray40")
      return()
    }
    
    if (length(sets) == 2) {
      tryCatch({
        # Calculate intersection manually to ensure correct count
        intersection <- intersect(sets[[1]], sets[[2]])
        only_set1 <- setdiff(sets[[1]], sets[[2]])
        only_set2 <- setdiff(sets[[2]], sets[[1]])
        
        # Create category names with totals
        cat_names <- paste0(names(sets), " = ", format(sapply(sets, length), big.mark = ","))
        
        # If all genes are the same, show a simple message instead
        if (length(intersection) == length(sets[[1]]) && length(intersection) == length(sets[[2]])) {
          plot.new()
          text(0.5, 0.5, 
               paste0("All datasets have identical genes\n(", format(length(intersection), big.mark = ","), " genes)"),
               cex = 1.8, col = "gray40")
          return()
        }
        
        # Create category names with totals displayed clearly
        cat_names <- paste0(names(sets), "\n", format(sapply(sets, length), big.mark = ","))
        
        venn.plot <- venn.diagram(
          x = sets,
          category.names = cat_names,
          filename = NULL,
          output = TRUE,
          fill = c("#f39c12", "#3498db"),
          alpha = 0.6,
          cex = 0.85,
          cat.cex = 0.95,
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
        grid.draw(venn.plot)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error creating Venn diagram:", e$message), cex = 1.2, col = "red")
      })
    } else if (length(sets) == 3) {
      tryCatch({
        # Check if all sets are identical
        all_intersect <- Reduce(intersect, sets)
        all_same <- all(sapply(sets, function(s) length(s) == length(all_intersect)))
        
        if (all_same) {
          plot.new()
          text(0.5, 0.5, 
               paste0("All datasets have identical genes\n(", format(length(all_intersect), big.mark = ","), " genes)"),
               cex = 1.8, col = "gray40")
          return()
        }
        
        # Create category names with totals (like "Q = 16")
        cat_names <- paste0(names(sets), " = ", format(sapply(sets, length), big.mark = ","))
        
        # Create category names with totals
        cat_names <- paste0(names(sets), "\n", format(sapply(sets, length), big.mark = ","))
        
        venn.plot <- venn.diagram(
          x = sets,
          category.names = cat_names,
          filename = NULL,
          output = TRUE,
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
        all_same <- all(sapply(sets, function(s) length(s) == length(all_intersect)))
        
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
        cat_names <- paste0(names(sets), "\n", format(sapply(sets, length), big.mark = ","))
        
        # Create a more detailed Venn diagram with better visibility
        # Calculate all intersections manually first to ensure they exist
        all_intersect_4 <- Reduce(intersect, sets)
        intersect_123 <- Reduce(intersect, sets[1:3])
        intersect_124 <- Reduce(intersect, sets[c(1,2,4)])
        intersect_134 <- Reduce(intersect, sets[c(1,3,4)])
        intersect_234 <- Reduce(intersect, sets[2:4])
        
        venn.plot <- venn.diagram(
          x = sets,
          category.names = cat_names,
          filename = NULL,
          output = TRUE,
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
      all_same <- all(sapply(sets, function(s) length(s) == length(all_intersect)))
      
      if (all_same) {
        plot.new()
        text(0.5, 0.5, 
             paste0("All datasets have identical genes\n(", format(length(all_intersect), big.mark = ","), " genes)"),
             cex = 1.8, col = "gray40")
      } else {
        # Show summary
        plot.new()
        text(0.5, 0.7, paste("Total Datasets:", length(sets)), cex = 2, font = 2)
        text(0.5, 0.5, paste("Common Genes:", format(length(rv$common_genes), big.mark = ",")), 
             cex = 3, col = "#2ecc71", font = 2)
        text(0.5, 0.3, "See UpSet plot for detailed intersections →", 
             cex = 1.3, col = "gray40")
      }
    }
  })
  
  output$upset_plot <- renderPlot({
    req(rv$all_genes_list)
    if (length(rv$all_genes_list) < 2) {
      plot.new()
      text(0.5, 0.5, "Need 2+ datasets for UpSet plot", cex = 1.8, col = "gray40")
      return()
    }
    
    # Use original gene lists (before filtering to common genes)
    gene_lists <- rv$all_genes_list
    
    # Ensure all are character vectors
    gene_lists <- lapply(gene_lists, function(x) {
      if (is.null(x)) return(character(0))
      as.character(x)
    })
    
    # Remove any empty lists
    gene_lists <- gene_lists[sapply(gene_lists, length) > 0]
    
    if (length(gene_lists) < 2) {
      plot.new()
      text(0.5, 0.5, "Need 2+ datasets with genes for UpSet plot", cex = 1.8, col = "gray40")
      return()
    }
    
    # Get all unique genes across all datasets
    all_genes <- unique(unlist(gene_lists))
    
    if (length(all_genes) == 0) {
      plot.new()
      text(0.5, 0.5, "No genes found in datasets", cex = 1.8, col = "gray40")
      return()
    }
    
    # Create binary matrix: rows = genes, columns = datasets
    upset_matrix <- matrix(0, nrow = length(all_genes), ncol = length(gene_lists))
    rownames(upset_matrix) <- all_genes
    colnames(upset_matrix) <- names(gene_lists)
    
    # Fill matrix: 1 if gene is present in dataset, 0 otherwise
    for (i in seq_along(gene_lists)) {
      genes_in_set <- gene_lists[[i]]
      # Find which rows (genes) are in this set
      matching_genes <- intersect(all_genes, genes_in_set)
      if (length(matching_genes) > 0) {
        upset_matrix[matching_genes, i] <- 1
      }
    }
    
    # Convert to data frame for UpSetR
    upset_df <- as.data.frame(upset_matrix)
    
    # Calculate max set size for scaling
    max_set_size <- max(sapply(gene_lists, length))
    
    tryCatch({
      upset(upset_df, 
            sets = colnames(upset_df),
            keep.order = TRUE, 
            order.by = "freq",
            main.bar.color = "#3498db",
            sets.bar.color = "#e74c3c",
            matrix.color = "#2ecc71",
            point.size = 4,
            line.size = 1.2,
            text.scale = c(1.8, 1.5, 1.5, 1.3, 1.8, 1.5),
            mb.ratio = c(0.6, 0.4),
            set_size.show = TRUE,
            set_size.scale_max = max_set_size * 1.1
      )
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating UpSet plot:", e$message), cex = 1.2, col = "red")
    })
  })
  
  # ==============================================================================
  # QC PLOTS
  # ==============================================================================
  
  output$qc_boxplot <- renderPlot({
    req(rv$combined_expr_raw)
    
    # Prepare data
    micro_n <- if (length(rv$micro_expr_list) > 0) sum(vapply(rv$micro_expr_list, ncol, integer(1))) else 0L
    rna_n <- if (length(rv$rna_counts_list) > 0) sum(vapply(rv$rna_counts_list, ncol, integer(1))) else 0L
    platform_per_sample <- c(rep("Microarray", micro_n), rep("RNAseq", rna_n))
    platform_labels <- rep(platform_per_sample, each = nrow(rv$combined_expr_raw))
    
    df <- data.frame(
      Expression = as.vector(rv$combined_expr_raw),
      Sample = rep(colnames(rv$combined_expr_raw), each = nrow(rv$combined_expr_raw)),
      Platform = platform_labels[1:length(as.vector(rv$combined_expr_raw))]
    )
    
    if (nrow(df) > 500000) {
      set.seed(123)
      df <- df[sample(nrow(df), 500000), ]
    }
    
    ggplot(df, aes(x = Sample, y = Expression, fill = Platform)) +
      geom_boxplot(outlier.size = 0.5) +
      theme_bw() +
      labs(title = "Expression Distribution - Raw Data", y = "Expression") +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            plot.title = element_text(face = "bold", size = 16))
  })
  
  output$qc_density <- renderPlot({
    req(rv$combined_expr_raw)
    
    plot(density(rv$combined_expr_raw[, 1], na.rm = TRUE), main = "Expression Density - Raw Data",
         xlab = "Expression", col = "#3498db", lwd = 2, 
         ylim = c(0, max(density(rv$combined_expr_raw[, 1])$y) * 1.2))
    
    colors <- rainbow(ncol(rv$combined_expr_raw))
    for (i in 2:min(ncol(rv$combined_expr_raw), 50)) {
      lines(density(rv$combined_expr_raw[, i], na.rm = TRUE), col = colors[i], lwd = 1)
    }
  })
  
  # ==============================================================================
  # SAMPLE OUTLIER DETECTION
  # ==============================================================================

  # ---- Previously excluded samples banner ----
  output$qc_excluded_info_ui <- renderUI({
    excluded <- rv$qc_excluded_samples
    if (is.null(excluded) || length(excluded) == 0) return(NULL)
    tags$div(
      class = "alert alert-info", style = "margin-bottom: 12px;",
      icon("info-circle"),
      tags$strong(paste0(" ", length(excluded), " sample(s) previously excluded: ")),
      tags$span(paste(excluded, collapse = ", "), style = "font-size: 12px;")
    )
  })

  # ---- Run outlier detection ----
  observeEvent(input$run_outlier_detection, {
    req(rv$combined_expr_raw)
    expr <- rv$combined_expr_raw

    if (ncol(expr) < 5) {
      showNotification("Need at least 5 samples for outlier detection.", type = "warning", duration = 5)
      return()
    }

    withProgress(message = "Detecting sample outliers...", value = 0, {
      tryCatch({
        # Use top variable genes for efficiency
        n_top <- min(5000, nrow(expr))
        gene_vars <- apply(expr, 1, var, na.rm = TRUE)
        top_genes <- names(sort(gene_vars, decreasing = TRUE))[seq_len(n_top)]
        expr_sub <- expr[top_genes, , drop = FALSE]

        # Remove genes with zero/NA variance
        good_var <- apply(expr_sub, 1, function(x) { v <- var(x, na.rm = TRUE); !is.na(v) && v > 0 })
        expr_sub <- expr_sub[good_var, , drop = FALSE]

        # Replace NAs with row means
        for (i in seq_len(nrow(expr_sub))) {
          na_idx <- is.na(expr_sub[i, ])
          if (any(na_idx)) expr_sub[i, na_idx] <- mean(expr_sub[i, !na_idx], na.rm = TRUE)
        }

        incProgress(0.2, detail = "PCA analysis...")

        # ---- PCA-based outlier detection (Mahalanobis distance) ----
        pca <- prcomp(t(expr_sub), scale. = TRUE, center = TRUE)
        n_pc <- min(2, ncol(pca$x))
        scores <- pca$x[, seq_len(n_pc), drop = FALSE]

        center <- colMeans(scores)
        cov_mat <- tryCatch(cov(scores), error = function(e) diag(n_pc))
        if (any(is.na(cov_mat)) || det(cov_mat) < 1e-10) {
          cov_mat <- diag(apply(scores, 2, var, na.rm = TRUE))
        }
        distances <- tryCatch(
          mahalanobis(scores, center, cov_mat),
          error = function(e) apply(scores, 1, function(x) sum((x - center)^2))
        )
        pca_threshold <- qchisq(0.975, df = n_pc)
        pca_outliers <- names(which(distances > pca_threshold))

        incProgress(0.4, detail = "Connectivity analysis...")

        # ---- Sample connectivity (WGCNA-style signed network, power=6) ----
        cor_mat <- cor(expr_sub, use = "pairwise.complete.obs", method = "pearson")
        cor_mat[is.na(cor_mat)] <- 0
        adj_mat <- ((1 + cor_mat) / 2)^6
        diag(adj_mat) <- 0
        k <- rowSums(adj_mat)
        conn_threshold <- mean(k) - 2 * sd(k)
        conn_outliers <- names(which(k < conn_threshold))

        incProgress(0.3, detail = "Preparing results...")

        all_outliers <- union(pca_outliers, conn_outliers)

        # Store results
        rv$qc_pca_scores <- scores
        rv$qc_pca_distances <- distances
        rv$qc_pca_threshold <- pca_threshold
        rv$qc_pca_outliers <- pca_outliers
        rv$qc_pca_var_explained <- summary(pca)$importance[2, seq_len(n_pc)]
        rv$qc_conn_k <- k
        rv$qc_conn_threshold <- conn_threshold
        rv$qc_conn_outliers <- conn_outliers
        rv$qc_all_outliers <- all_outliers
        rv$qc_outlier_detection_complete <- TRUE

        incProgress(0.1, detail = "Done!")

        n_flagged <- length(all_outliers)
        showNotification(
          tags$div(icon(if (n_flagged > 0) "exclamation-triangle" else "check-circle"),
                   tags$strong(paste0(" Outlier detection complete. ", n_flagged, " sample(s) flagged."))),
          type = if (n_flagged > 0) "warning" else "message", duration = 6)

      }, error = function(e) {
        showNotification(paste("Outlier detection error:", e$message), type = "error", duration = 8)
      })
    })
  })

  # ---- Summary badges ----
  output$qc_outlier_summary_ui <- renderUI({
    if (!isTRUE(rv$qc_outlier_detection_complete)) return(NULL)
    n_pca <- length(rv$qc_pca_outliers)
    n_conn <- length(rv$qc_conn_outliers)
    n_total <- length(rv$qc_all_outliers)
    n_samples <- ncol(rv$combined_expr_raw)

    tags$div(
      style = "display: flex; gap: 12px; flex-wrap: wrap; align-items: center;",
      tags$div(
        style = paste0("padding: 8px 14px; border-radius: 20px; font-weight: bold; font-size: 13px; ",
                       "background: ", if (n_pca > 0) "#fce4ec" else "#e8f5e9", "; ",
                       "color: ", if (n_pca > 0) "#c62828" else "#2e7d32", ";"),
        icon("chart-area"), paste0(" PCA: ", n_pca, " outlier(s)")
      ),
      tags$div(
        style = paste0("padding: 8px 14px; border-radius: 20px; font-weight: bold; font-size: 13px; ",
                       "background: ", if (n_conn > 0) "#fff3e0" else "#e8f5e9", "; ",
                       "color: ", if (n_conn > 0) "#e65100" else "#2e7d32", ";"),
        icon("project-diagram"), paste0(" Connectivity: ", n_conn, " outlier(s)")
      ),
      tags$div(
        style = paste0("padding: 8px 14px; border-radius: 20px; font-weight: bold; font-size: 14px; ",
                       "background: ", if (n_total > 0) "#ffebee" else "#c8e6c9", "; ",
                       "color: ", if (n_total > 0) "#b71c1c" else "#1b5e20", ";"),
        icon(if (n_total > 0) "exclamation-triangle" else "check-circle"),
        paste0(" Total: ", n_total, " / ", n_samples, " flagged")
      )
    )
  })

  # ---- Results panel (plots + table + selector) ----
  output$qc_outlier_results_ui <- renderUI({
    if (!isTRUE(rv$qc_outlier_detection_complete)) return(NULL)

    tagList(
      tags$hr(style = "margin: 20px 0;"),
      # Row 1: PCA plot | Connectivity plot
      fluidRow(
        column(6,
          tags$p(tags$strong(icon("chart-area"), " PCA Outlier Detection"), style = "margin-bottom: 6px;"),
          plotOutput("qc_pca_outlier_plot", height = "420px"),
          tags$div(style = "margin-top: 6px;",
            downloadButton("dl_qc_pca_plot_jpg", tagList(icon("download"), " JPG"), class = "btn-danger btn-xs", style = "margin-right: 4px;"),
            downloadButton("dl_qc_pca_plot_pdf", tagList(icon("download"), " PDF"), class = "btn-danger btn-xs"))
        ),
        column(6,
          tags$p(tags$strong(icon("project-diagram"), " Sample Connectivity"), style = "margin-bottom: 6px;"),
          plotOutput("qc_connectivity_plot", height = "420px"),
          tags$div(style = "margin-top: 6px;",
            downloadButton("dl_qc_conn_plot_jpg", tagList(icon("download"), " JPG"), class = "btn-danger btn-xs", style = "margin-right: 4px;"),
            downloadButton("dl_qc_conn_plot_pdf", tagList(icon("download"), " PDF"), class = "btn-danger btn-xs"))
        )
      ),
      # Row 2: Outlier table + selector
      tags$hr(style = "margin: 15px 0;"),
      fluidRow(
        column(7,
          tags$p(tags$strong(icon("table"), " All Samples -- Outlier Summary"), style = "margin-bottom: 6px;"),
          DT::dataTableOutput("qc_outlier_table"),
          tags$div(style = "margin-top: 6px;",
            downloadButton("dl_qc_outlier_csv", tagList(icon("download"), " CSV"), class = "btn-default btn-xs"))
        ),
        column(5,
          uiOutput("qc_outlier_selector_ui")
        )
      )
    )
  })

  # ---- Plot helper: PCA scatter ----
  make_qc_pca_plot <- function() {
    scores <- as.data.frame(rv$qc_pca_scores)
    scores$Sample <- rownames(scores)
    scores$Distance <- rv$qc_pca_distances[scores$Sample]
    scores$IsOutlier <- scores$Distance > rv$qc_pca_threshold

    if (!is.null(rv$unified_metadata) && "Dataset" %in% names(rv$unified_metadata) &&
        "SampleID" %in% names(rv$unified_metadata)) {
      scores$Dataset <- rv$unified_metadata$Dataset[match(scores$Sample, rv$unified_metadata$SampleID)]
      if (all(is.na(scores$Dataset))) scores$Dataset <- "Dataset"
    } else {
      scores$Dataset <- "Dataset"
    }

    var_exp <- rv$qc_pca_var_explained * 100
    n_out <- sum(scores$IsOutlier)

    p <- ggplot2::ggplot(scores, ggplot2::aes(x = PC1, y = PC2)) +
      ggplot2::geom_point(ggplot2::aes(color = Dataset, shape = IsOutlier, size = IsOutlier), alpha = 0.8) +
      ggplot2::scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17),
                                   labels = c("FALSE" = "Normal", "TRUE" = "Outlier"), name = "Status") +
      ggplot2::scale_size_manual(values = c("FALSE" = 3, "TRUE" = 5),
                                  labels = c("FALSE" = "Normal", "TRUE" = "Outlier"), name = "Status") +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::labs(
        title = "PCA-based Outlier Detection",
        subtitle = paste0("Mahalanobis threshold: ", round(rv$qc_pca_threshold, 2), " | Outliers: ", n_out),
        x = paste0("PC1 (", round(var_exp[1], 1), "%)"),
        y = if (length(var_exp) > 1) paste0("PC2 (", round(var_exp[2], 1), "%)") else "PC2"
      ) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14),
                     legend.position = "right")

    if (n_out > 0) {
      p <- p + ggrepel::geom_text_repel(
        data = scores[scores$IsOutlier, , drop = FALSE],
        ggplot2::aes(label = Sample), size = 3, fontface = "bold",
        color = "#c62828", max.overlaps = 20, box.padding = 0.5)
    }
    if (sum(!scores$IsOutlier) >= 3) {
      p <- p + ggplot2::stat_ellipse(
        data = scores[!scores$IsOutlier, , drop = FALSE],
        ggplot2::aes(x = PC1, y = PC2), color = "gray40",
        linetype = "dashed", level = 0.975, inherit.aes = FALSE)
    }
    p
  }

  # ---- Plot helper: Connectivity bar chart ----
  make_qc_conn_plot <- function() {
    k <- rv$qc_conn_k
    threshold <- rv$qc_conn_threshold
    df <- data.frame(Sample = names(k), Connectivity = as.numeric(k),
                     IsOutlier = k < threshold, stringsAsFactors = FALSE)
    df <- df[order(df$Connectivity), , drop = FALSE]
    df$Sample <- factor(df$Sample, levels = df$Sample)

    n_show <- min(nrow(df), 40)
    if (nrow(df) > n_show) {
      df_plot <- df[seq_len(n_show), , drop = FALSE]
      tsuffix <- paste0(" (bottom ", n_show, "/", nrow(df), ")")
    } else {
      df_plot <- df
      tsuffix <- ""
    }

    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = Sample, y = Connectivity, fill = IsOutlier)) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.85, width = 0.7) +
      ggplot2::scale_fill_manual(values = c("FALSE" = "#27ae60", "TRUE" = "#e74c3c"),
                                  labels = c("FALSE" = "Normal", "TRUE" = "Low Connectivity"), name = "Status") +
      ggplot2::geom_hline(yintercept = threshold, linetype = "dashed", color = "#e74c3c", size = 0.8) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::labs(
        title = paste0("Sample Connectivity", tsuffix),
        subtitle = paste0("Signed network (power=6) | Threshold: mean-2*SD = ", round(threshold, 1)),
        x = "", y = "Connectivity (sum of adjacency)", fill = "Status"
      ) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13),
                     legend.position = "top")
    p
  }

  # ---- Render plots ----
  output$qc_pca_outlier_plot <- renderPlot({
    req(rv$qc_pca_scores)
    print(make_qc_pca_plot())
  }, height = 420, res = 96)

  output$qc_connectivity_plot <- renderPlot({
    req(rv$qc_conn_k)
    print(make_qc_conn_plot())
  }, height = 420, res = 96)

  # ---- Download handlers for plots ----
  output$dl_qc_pca_plot_jpg <- downloadHandler(
    filename = function() "QC_PCA_Outlier_Detection.jpg",
    content = function(file) {
      req(rv$qc_pca_scores)
      ggplot2::ggsave(file, make_qc_pca_plot(), width = 8, height = 6, dpi = IMAGE_DPI, units = "in", bg = "white", device = "jpeg")
    }
  )
  output$dl_qc_pca_plot_pdf <- downloadHandler(
    filename = function() "QC_PCA_Outlier_Detection.pdf",
    content = function(file) {
      req(rv$qc_pca_scores)
      ggplot2::ggsave(file, make_qc_pca_plot(), width = 8, height = 6, device = "pdf", bg = "white")
    }
  )
  output$dl_qc_conn_plot_jpg <- downloadHandler(
    filename = function() "QC_Sample_Connectivity.jpg",
    content = function(file) {
      req(rv$qc_conn_k)
      ggplot2::ggsave(file, make_qc_conn_plot(), width = 8, height = 6, dpi = IMAGE_DPI, units = "in", bg = "white", device = "jpeg")
    }
  )
  output$dl_qc_conn_plot_pdf <- downloadHandler(
    filename = function() "QC_Sample_Connectivity.pdf",
    content = function(file) {
      req(rv$qc_conn_k)
      ggplot2::ggsave(file, make_qc_conn_plot(), width = 8, height = 6, device = "pdf", bg = "white")
    }
  )

  # ---- Outlier summary table ----
  output$qc_outlier_table <- DT::renderDataTable({
    req(rv$qc_outlier_detection_complete)
    all_samples <- colnames(rv$combined_expr_raw)

    df <- data.frame(
      Sample = all_samples,
      Mahal_Distance = round(rv$qc_pca_distances[all_samples], 2),
      PCA_Outlier = ifelse(all_samples %in% rv$qc_pca_outliers, "Yes", ""),
      Connectivity = round(rv$qc_conn_k[all_samples], 2),
      Conn_Outlier = ifelse(all_samples %in% rv$qc_conn_outliers, "Yes", ""),
      Flagged = ifelse(all_samples %in% rv$qc_all_outliers, "OUTLIER", ""),
      stringsAsFactors = FALSE
    )
    df <- df[order(-nchar(df$Flagged), -df$Mahal_Distance), , drop = FALSE]

    dt <- DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE, dom = "frtip"),
                        rownames = FALSE, selection = "none")
    dt <- DT::formatStyle(dt, "Flagged",
                          backgroundColor = DT::styleEqual("OUTLIER", "#ffebee"),
                          color = DT::styleEqual("OUTLIER", "#c62828"),
                          fontWeight = DT::styleEqual("OUTLIER", "bold"))
    dt <- DT::formatStyle(dt, "PCA_Outlier",
                          color = DT::styleEqual("Yes", "#e74c3c"),
                          fontWeight = DT::styleEqual("Yes", "bold"))
    dt <- DT::formatStyle(dt, "Conn_Outlier",
                          color = DT::styleEqual("Yes", "#e65100"),
                          fontWeight = DT::styleEqual("Yes", "bold"))
    dt
  })

  output$dl_qc_outlier_csv <- downloadHandler(
    filename = function() "QC_Sample_Outlier_Summary.csv",
    content = function(file) {
      req(rv$qc_outlier_detection_complete)
      all_samples <- colnames(rv$combined_expr_raw)
      df <- data.frame(
        Sample = all_samples,
        Mahal_Distance = round(rv$qc_pca_distances[all_samples], 4),
        PCA_Outlier = all_samples %in% rv$qc_pca_outliers,
        Connectivity = round(rv$qc_conn_k[all_samples], 4),
        Conn_Outlier = all_samples %in% rv$qc_conn_outliers,
        Flagged = all_samples %in% rv$qc_all_outliers,
        stringsAsFactors = FALSE
      )
      write.csv(df, file, row.names = FALSE)
      write.csv(df, file.path(CSV_EXPORT_DIR(), "QC_Sample_Outlier_Summary.csv"), row.names = FALSE)
    }
  )

  # ---- Outlier selector (checkboxes for flagged samples) ----
  output$qc_outlier_selector_ui <- renderUI({
    req(rv$qc_outlier_detection_complete)
    outliers <- rv$qc_all_outliers

    if (length(outliers) == 0) {
      return(tags$div(
        style = "padding: 25px; text-align: center;",
        tags$div(
          style = "padding: 25px; background: #e8f5e9; border-radius: 12px; border: 2px solid #4caf50;",
          icon("check-circle", style = "color: #4caf50; font-size: 40px;"),
          tags$h4("No outliers detected!", style = "color: #2e7d32; margin-top: 10px;"),
          tags$p("All samples pass both PCA and connectivity checks. Proceed to normalization.",
                 style = "color: #388e3c; font-size: 13px; margin: 0;")
        )
      ))
    }

    # Build labels with detection method info
    checkbox_choices <- setNames(outliers, sapply(outliers, function(s) {
      methods <- c()
      if (s %in% rv$qc_pca_outliers) methods <- c(methods, "PCA")
      if (s %in% rv$qc_conn_outliers) methods <- c(methods, "Connectivity")
      dist_val <- round(rv$qc_pca_distances[s], 1)
      conn_val <- round(rv$qc_conn_k[s], 1)
      paste0(s, "  [", paste(methods, collapse = "+"), " | Dist:", dist_val, " | Conn:", conn_val, "]")
    }))

    tags$div(
      style = "padding: 15px; background: #fff3cd; border: 2px solid #ffc107; border-radius: 10px;",
      tags$h4(icon("exclamation-triangle", style = "color: #e74c3c;"),
              tags$strong(paste0(" ", length(outliers), " Outlier(s) Detected")),
              style = "margin-top: 0; margin-bottom: 10px; color: #c62828;"),
      tags$p("Select samples to exclude. Unchecked samples will be kept.",
             style = "font-size: 13px; margin-bottom: 10px; color: #495057;"),
      tags$div(
        style = "max-height: 220px; overflow-y: auto; padding: 8px; background: #fff; border-radius: 6px; border: 1px solid #ddd;",
        checkboxGroupInput("qc_outlier_checkboxes", NULL,
          choices = checkbox_choices,
          selected = outliers,
          width = "100%")
      ),
      tags$div(
        style = "margin-top: 8px; display: flex; gap: 6px;",
        actionButton("qc_select_all_outliers", tagList(icon("check-double"), " All"),
          class = "btn-default btn-xs"),
        actionButton("qc_deselect_all_outliers", tagList(icon("square"), " None"),
          class = "btn-default btn-xs")
      ),
      tags$hr(style = "margin: 10px 0;"),
      actionButton("exclude_outliers_btn",
        tagList(icon("trash"), " Exclude Selected Outliers"),
        class = "btn-danger btn-block",
        style = "font-weight: bold; font-size: 14px; padding: 10px;")
    )
  })

  # Quick selection buttons
  observeEvent(input$qc_select_all_outliers, {
    outliers <- rv$qc_all_outliers
    if (!is.null(outliers)) updateCheckboxGroupInput(session, "qc_outlier_checkboxes", selected = outliers)
  })
  observeEvent(input$qc_deselect_all_outliers, {
    updateCheckboxGroupInput(session, "qc_outlier_checkboxes", selected = character(0))
  })

  # ---- Exclude selected outliers ----
  observeEvent(input$exclude_outliers_btn, {
    req(rv$combined_expr_raw)

    samples_to_exclude <- input$qc_outlier_checkboxes
    if (is.null(samples_to_exclude) || length(samples_to_exclude) == 0) {
      showNotification("No samples selected for exclusion. Check at least one sample.", type = "warning", duration = 4)
      return()
    }

    keep_cols <- setdiff(colnames(rv$combined_expr_raw), samples_to_exclude)
    if (length(keep_cols) < 3) {
      showNotification("Cannot exclude: would leave fewer than 3 samples.", type = "error", duration = 5)
      return()
    }

    # Exclude from combined expression matrix
    rv$combined_expr_raw <- rv$combined_expr_raw[, keep_cols, drop = FALSE]

    # Exclude from per-dataset microarray lists
    for (gse in names(rv$micro_expr_list)) {
      cur <- rv$micro_expr_list[[gse]]
      keep <- setdiff(colnames(cur), samples_to_exclude)
      if (length(keep) > 0) {
        rv$micro_expr_list[[gse]] <- cur[, keep, drop = FALSE]
      } else {
        rv$micro_expr_list[[gse]] <- NULL
      }
    }
    rv$micro_expr_list <- rv$micro_expr_list[!sapply(rv$micro_expr_list, is.null)]

    # Exclude from per-dataset RNA-seq lists
    for (gse in names(rv$rna_counts_list)) {
      cur <- rv$rna_counts_list[[gse]]
      keep <- setdiff(colnames(cur), samples_to_exclude)
      if (length(keep) > 0) {
        rv$rna_counts_list[[gse]] <- cur[, keep, drop = FALSE]
      } else {
        rv$rna_counts_list[[gse]] <- NULL
      }
    }
    rv$rna_counts_list <- rv$rna_counts_list[!sapply(rv$rna_counts_list, is.null)]

    # Exclude from unified metadata if available
    if (!is.null(rv$unified_metadata) && "SampleID" %in% names(rv$unified_metadata)) {
      rv$unified_metadata <- rv$unified_metadata[!rv$unified_metadata$SampleID %in% samples_to_exclude, , drop = FALSE]
    }

    # Track excluded samples
    if (is.null(rv$qc_excluded_samples)) rv$qc_excluded_samples <- character(0)
    rv$qc_excluded_samples <- unique(c(rv$qc_excluded_samples, samples_to_exclude))

    # Reset outlier detection so user can re-run on reduced data
    rv$qc_outlier_detection_complete <- FALSE
    rv$qc_all_outliers <- character(0)

    showNotification(
      tags$div(icon("check-circle"),
               tags$strong(paste0(" ", length(samples_to_exclude), " sample(s) excluded: ")),
               tags$span(paste(samples_to_exclude, collapse = ", "), style = "font-size: 12px;"),
               tags$br(),
               tags$span(paste0("Remaining: ", length(keep_cols), " samples. QC plots updated. You may re-run detection to verify."),
                         style = "font-size: 12px; color: #27ae60;")),
      type = "message", duration = 8)
  })

  # Next to normalize
}
