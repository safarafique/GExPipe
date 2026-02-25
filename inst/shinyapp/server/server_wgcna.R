# ==============================================================================
# SERVER_WGCNA.R - Step 7: WGCNA Analysis Module
# ==============================================================================
#
# WGCNA best practices (signed network):
#   - Use batch-corrected expression; samples = rows, genes = columns in datExpr.
#   - Soft threshold: aim for scale-free R² > 0.8; typical power 6–12.
#   - minModuleSize: 30–50 for discovery; 20 if few genes.
#   - mergeCutHeight: 0.25 merges modules with eigengene cor > 0.75.
#   - Grey module = unassigned genes; exclude from trait correlation.
#
# ==============================================================================

server_wgcna <- function(input, output, session, rv) {
  
  output$wgcna_timer <- renderText({
    wgcna_running <- isTRUE(rv$wgcna_running)
    wgcna_start <- rv$wgcna_start
    
    if (!wgcna_running || is.null(wgcna_start)) return("00:00")
    invalidateLater(1000, session)
    elapsed <- as.integer(difftime(Sys.time(), wgcna_start, units = "secs"))
    sprintf("%02d:%02d", elapsed %/% 60, elapsed %% 60)
  })
  
  # Helper function to add log messages
  add_wgcna_log <- function(message) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    rv$wgcna_log_messages <- c(rv$wgcna_log_messages, paste0("[", timestamp, "] ", message))
  }
  
  # ---------- STEP 1: DATA PREPARATION ----------
  observeEvent(input$prepare_wgcna, {
    # Require batch correction to be complete
    if (!isTRUE(rv$batch_complete) || is.null(rv$batch_corrected) || is.null(rv$unified_metadata)) {
      showNotification(
        tags$div(icon("exclamation-triangle"), tags$strong(" Step 5 required:"),
                 " Complete batch correction (Step 5) and ensure group labels are applied before running WGCNA."),
        type = "error", duration = 6)
      return()
    }
    batch_complete <- TRUE
    batch_corrected <- rv$batch_corrected
    unified_metadata <- rv$unified_metadata
    
    # Check if DE analysis has been run (optional but recommended)
    de_results <- rv$de_results
    if (is.null(de_results)) {
      showNotification(
        tags$div(
          icon("info-circle"),
          tags$strong("Note:"),
          " DE analysis is recommended before WGCNA, but not required.",
          tags$br(),
          "Proceeding with WGCNA data preparation...",
          style = "font-size: 13px;"
        ),
        type = "warning", duration = 5
      )
    }
    
    # Disable button and show loading
    shinyjs::disable("prepare_wgcna")
    shinyjs::html("prepare_wgcna", 
                  HTML('<i class="fa fa-spinner fa-spin"></i> Preparing...'))
    
    rv$wgcna_start <- Sys.time()
    rv$wgcna_running <- TRUE
    
    showNotification(
      tags$div(
        tags$div(class = "status-indicator processing"),
        tags$strong("WGCNA data preparation..."),
        tags$br(),
        tags$span("Preparing expression matrix for network analysis. Please wait..."),
        style = "font-size: 13px;"
      ),
      type = "message",
      duration = NULL,
      id = "wgcna_prepare_processing"
    )
    
    withProgress(message = "Preparing WGCNA data", value = 0.5, {
      tryCatch({
        # Use batch corrected data if available
        expr_mat <- rv$batch_corrected
        
        if (is.null(expr_mat)) {
          expr_mat <- rv$combined_expr
        }
        
        if (is.null(expr_mat)) {
          stop("No expression data available. Please complete batch correction first.")
        }
        
        add_wgcna_log("Starting WGCNA data preparation...")
        
        # Remove samples with too many NAs
        good_samples <- colSums(is.na(expr_mat)) < nrow(expr_mat) * 0.5
        expr_mat <- expr_mat[, good_samples, drop = FALSE]
        
        # Match samples with metadata (by rownames or SampleID column)
        meta <- rv$unified_metadata
        samp_ids <- if ("SampleID" %in% names(meta)) meta$SampleID else rownames(meta)
        if (is.null(samp_ids)) samp_ids <- rownames(meta)
        common_samples <- intersect(colnames(expr_mat), as.character(samp_ids))
        if (length(common_samples) == 0) {
          common_samples <- intersect(colnames(expr_mat), rownames(meta))
        }
        if (length(common_samples) == 0) {
          stop("No matching samples between expression data and metadata. Check that batch correction and group selection used the same sample IDs.")
        }
        expr_mat <- expr_mat[, common_samples, drop = FALSE]
        # Subset metadata to common_samples; use SampleID match if rownames don't match
        if (all(common_samples %in% rownames(meta))) {
          sample_info <- meta[common_samples, , drop = FALSE]
        } else if ("SampleID" %in% names(meta)) {
          sample_info <- meta[match(common_samples, meta$SampleID), , drop = FALSE]
          rownames(sample_info) <- common_samples
        } else {
          sample_info <- meta[match(common_samples, rownames(meta)), , drop = FALSE]
          rownames(sample_info) <- common_samples
        }
        
        # Check for good samples and genes
        if (!requireNamespace("WGCNA", quietly = TRUE)) {
          stop("WGCNA package not installed. Please install it: BiocManager::install('WGCNA')")
        }
        
        gsg <- WGCNA::goodSamplesGenes(t(expr_mat), verbose = 3)
        if (!gsg$allOK) {
          expr_mat <- t(expr_mat)[gsg$goodSamples, gsg$goodGenes]
          expr_mat <- t(expr_mat)
          add_wgcna_log(paste("Filtered:", sum(!gsg$goodSamples), "samples,", 
                            sum(!gsg$goodGenes), "genes removed"))
        }
        
        gene_mode <- if (!is.null(input$wgcna_gene_mode)) input$wgcna_gene_mode else "top_variable"
        if (gene_mode == "all_common") {
          expr_top <- expr_mat
          add_wgcna_log(paste("Using all common genes:", nrow(expr_top), "genes"))
        } else {
          # Select top variable genes
          vars <- apply(expr_mat, 1, var, na.rm = TRUE)
          top_n <- min(input$wgcna_top_genes, length(vars))
          keep_genes <- names(sort(vars, decreasing = TRUE))[1:top_n]
          expr_top <- expr_mat[keep_genes, , drop = FALSE]
          add_wgcna_log(paste("Selected top", top_n, "most variable genes"))
        }
        
        # Filter by minimum samples
        min_samples <- ceiling(input$wgcna_min_samples * ncol(expr_top))
        keep <- rowSums(!is.na(expr_top)) >= min_samples
        expr_top <- expr_top[keep, , drop = FALSE]
        
        # Require minimum dimensions for meaningful WGCNA
        if (nrow(expr_top) < 20L) {
          stop("Too few genes (", nrow(expr_top), ") after filtering. Use 'All genes' or larger 'Top Variable Genes' (e.g. 3000–5000) or lower 'Min Samples' fraction.")
        }
        if (ncol(expr_top) < 3L) {
          stop("Too few samples (", ncol(expr_top), "). Need at least 3 samples for WGCNA.")
        }
        
        # Transpose for WGCNA (samples as rows, genes as columns)
        datExpr <- t(expr_top)
        rv$datExpr <- datExpr
        # Store gene list and selection scores (variance = basis for "top variable" selection; rank 1 = highest variance)
        final_genes <- colnames(datExpr)
        rv$wgcna_top_variable_genes <- final_genes
        gene_var <- vars[final_genes]
        rv$wgcna_gene_variance_table <- data.frame(
          Rank = seq_along(final_genes),
          Gene = final_genes,
          Variance = round(as.numeric(gene_var), 6),
          stringsAsFactors = FALSE
        )
        rv$wgcna_sample_info <- sample_info[rownames(datExpr), , drop = FALSE]
        rv$wgcna_prepared <- TRUE
        rv$wgcna_datExpr_before_exclude <- NULL
        rv$wgcna_sample_info_before_exclude <- NULL

        # Sample clustering tree (for outlier check: one bad sample can ruin the network)
        st <- hclust(dist(datExpr), method = "average")
        rv$wgcna_sample_tree <- st
        rv$wgcna_excluded_samples <- character(0)  # reset exclusion when re-preparing
        rv$wgcna_suggested_outliers <- character(0)

        rv$wgcna_complete <- FALSE
        
        add_wgcna_log(paste("WGCNA data prepared:", nrow(datExpr), "samples x", 
                          ncol(datExpr), "genes"))
        
        # Re-enable button
        shinyjs::enable("prepare_wgcna")
        shinyjs::html("prepare_wgcna", 
                      HTML('<i class="fa fa-check-circle"></i> Prepare WGCNA Data'))
        
        removeNotification("wgcna_prepare_processing")
        
        showNotification(
          tags$div(
            tags$strong("✓ WGCNA data prepared!"),
            tags$br(),
            tags$span("Samples: ", nrow(datExpr), " | Genes: ", ncol(datExpr)),
            style = "font-size: 13px;"
          ),
          type = "message", duration = 6
        )
        
      }, error = function(e) {
        rv$wgcna_running <- FALSE
        shinyjs::enable("prepare_wgcna")
        shinyjs::html("prepare_wgcna", 
                      HTML('<i class="fa fa-exclamation-triangle"></i> Prepare WGCNA Data'))
        removeNotification("wgcna_prepare_processing")
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
        add_wgcna_log(paste("ERROR:", e$message))
      })
    })
    
    rv$wgcna_running <- FALSE
  })
  
  output$wgcna_qc_status <- renderUI({
    if (!isTRUE(rv$wgcna_prepared)) {
      tags$div(
        class = "alert alert-warning",
        icon("exclamation-triangle"),
        "Click 'Prepare WGCNA Data' to construct datExpr matrix."
      )
    } else if (is.null(rv$datExpr)) {
      tags$div(
        class = "alert alert-warning",
        icon("exclamation-triangle"),
        "WGCNA data not available. Please run Step 1 (Data Preparation) above again."
      )
    } else {
      tags$div(
        class = "alert alert-success",
        icon("check-circle"),
        "WGCNA datExpr ready: ",
        nrow(rv$datExpr), " samples × ",
        ncol(rv$datExpr), " genes."
      )
    }
  })
  
  output$wgcna_gene_list_ui <- renderUI({
    if (!isTRUE(rv$wgcna_prepared) || is.null(rv$wgcna_top_variable_genes)) return(NULL)
    genes <- rv$wgcna_top_variable_genes
    n <- length(genes)
    tags$div(
      class = "alert alert-info",
      style = "margin-bottom: 0;",
      tags$strong(icon("list"), " Genes used in WGCNA: ", n, " genes"),
      " (top variable genes you selected, after min-samples filter). Selection is by ",
      tags$strong("variance"), " (higher = more variable).",
      tags$br(),
      tags$div(
        style = "margin-top: 10px; display: flex; flex-wrap: wrap; gap: 10px; align-items: center;",
        downloadButton("download_wgcna_gene_list",
                       tagList(icon("download"), " Gene list (Rank & Variance)"),
                       class = "btn-sm btn-info"),
        downloadButton("download_wgcna_scaled_expr",
                       tagList(icon("download"), " Scaled expression (z-score)"),
                       class = "btn-sm btn-success")
      )
    )
  })
  
  output$download_wgcna_gene_list <- downloadHandler(
    filename = function() {
      n <- length(rv$wgcna_top_variable_genes)
      paste0("WGCNA_top", n, "_genes_rank_variance.csv")
    },
    content = function(file) {
      req(rv$wgcna_gene_variance_table)
      df <- rv$wgcna_gene_variance_table
      write.csv(df, file, row.names = FALSE)
      write.csv(df, file.path(CSV_EXPORT_DIR(), basename(file)), row.names = FALSE)
    }
  )
  
  output$download_wgcna_scaled_expr <- downloadHandler(
    filename = function() {
      n <- length(rv$wgcna_top_variable_genes)
      paste0("WGCNA_top", n, "_genes_scaled_expression.csv")
    },
    content = function(file) {
      req(rv$datExpr, rv$wgcna_top_variable_genes)
      # datExpr: samples x genes; scale each gene (column) -> z-score per gene across samples
      scaled <- scale(rv$datExpr)
      # Export genes x samples (genes as rows)
      mat <- t(scaled)
      df <- data.frame(Gene = rownames(mat), mat, check.names = FALSE)
      fname <- paste0("WGCNA_top", length(rv$wgcna_top_variable_genes), "_genes_scaled_expression.csv")
      write.csv(df, file, row.names = FALSE)
      write.csv(df, file.path(CSV_EXPORT_DIR(), fname), row.names = FALSE)
    }
  )
  
  # Per-sample merge height from hclust (height at which sample joins the tree)
  get_sample_merge_heights <- function(ht) {
    n <- length(ht$labels)
    if (n < 2) return(setNames(numeric(n), ht$labels))
    merge <- ht$merge
    height <- ht$height
    join_height <- numeric(n)
    for (k in seq_len(n - 1)) {
      for (side in 1:2) {
        if (merge[k, side] < 0) {
          leaf <- -merge[k, side]
          join_height[leaf] <- height[k]
        }
      }
    }
    if (!is.null(ht$labels)) names(join_height) <- ht$labels
    join_height
  }

  # Detect potential outliers: samples that join the dendrogram at unusually high height
  observeEvent(input$wgcna_detect_outliers, {
    req(rv$wgcna_sample_tree, rv$datExpr)
    ht <- rv$wgcna_sample_tree
    h <- get_sample_merge_heights(ht)
    med <- median(h)
    mad_val <- mad(h, na.rm = TRUE)
    if (mad_val < 1e-10) mad_val <- sd(h, na.rm = TRUE)
    # Flag samples with join height > median + 3*MAD (conservative)
    threshold <- med + 3 * mad_val
    suggested <- names(h)[h > threshold]
    rv$wgcna_suggested_outliers <- suggested
    if (length(suggested) > 0) {
      add_wgcna_log(paste("Outlier detection: suggested", length(suggested), "sample(s) (height > median + 3*MAD):", paste(suggested, collapse = ", ")))
      showNotification(paste("Suggested ", length(suggested), " potential outlier(s). Exclude suggested or dismiss.", sep = ""), type = "message", duration = 5)
    } else {
      add_wgcna_log("Outlier detection: no samples above height threshold (median + 3*MAD).")
      showNotification("No outliers detected — you can proceed to Step 2.", type = "message", duration = 4)
    }
  })

  # Exclude suggested outliers (from Detect button)
  observeEvent(input$wgcna_exclude_suggested, {
    req(rv$wgcna_suggested_outliers, length(rv$wgcna_suggested_outliers) > 0, rv$datExpr)
    suggested <- rv$wgcna_suggested_outliers
    sample_ids <- rownames(rv$datExpr)
    found <- intersect(suggested, sample_ids)
    if (length(found) == 0) return()
    if (is.null(rv$wgcna_datExpr_before_exclude)) {
      rv$wgcna_datExpr_before_exclude <- rv$datExpr
      rv$wgcna_sample_info_before_exclude <- rv$wgcna_sample_info
    }
    keep <- setdiff(sample_ids, found)
    rv$datExpr <- rv$datExpr[keep, , drop = FALSE]
    rv$wgcna_sample_info <- rv$wgcna_sample_info[keep, , drop = FALSE]
    rv$wgcna_excluded_samples <- found
    rv$wgcna_sample_tree <- hclust(dist(rv$datExpr), method = "average")
    rv$wgcna_complete <- FALSE
    rv$wgcna_suggested_outliers <- character(0)
    add_wgcna_log(paste("Excluded suggested outliers:", paste(found, collapse = ", ")))
    showNotification(paste("Excluded ", length(found), " suggested outlier(s). Re-run Step 3 if needed.", sep = ""), type = "message", duration = 5)
  })

  # Skip outlier step (no outliers)
  observeEvent(input$wgcna_skip_outliers, {
    rv$wgcna_suggested_outliers <- character(0)
    showNotification("No outliers excluded. Proceed to Step 2 (Pick Soft Threshold).", type = "message", duration = 4)
  })

  output$wgcna_suggested_outliers_ui <- renderUI({
    if (!isTRUE(rv$wgcna_prepared)) return(NULL)
    suggested <- rv$wgcna_suggested_outliers
    if (is.null(suggested)) suggested <- character(0)
    if (length(suggested) == 0) return(NULL)
    tags$div(
      style = "margin-top: 10px; padding: 8px; background: #fff3cd; border-radius: 4px; border: 1px solid #ffc107;",
      tags$p(tags$small(tags$strong("Suggested outliers:"), paste(suggested, collapse = ", ")), style = "margin: 0 0 6px 0;"),
      tags$div(
        actionButton("wgcna_exclude_suggested", tagList(icon("user-minus"), " Exclude suggested"), class = "btn-warning btn-sm", style = "margin-right: 6px;"),
        actionButton("wgcna_dismiss_suggested", "Dismiss", class = "btn-default btn-sm")
      )
    )
  })

  observeEvent(input$wgcna_dismiss_suggested, {
    rv$wgcna_suggested_outliers <- character(0)
    showNotification("Suggestion dismissed. Exclude manually or proceed to Step 2.", type = "message", duration = 3)
  })

  # Outlier check: optional exclusion of samples (comma-separated IDs)
  observeEvent(input$wgcna_apply_exclude, {
    req(rv$wgcna_prepared, rv$datExpr)
    txt <- trimws(input$wgcna_exclude_samples)
    if (txt == "") {
      # Restore all samples if user cleared the box and clicked update
      if (length(rv$wgcna_excluded_samples) > 0 && !is.null(rv$wgcna_datExpr_before_exclude)) {
        rv$datExpr <- rv$wgcna_datExpr_before_exclude
        rv$wgcna_sample_info <- rv$wgcna_sample_info_before_exclude
        rv$wgcna_excluded_samples <- character(0)
        rv$wgcna_sample_tree <- hclust(dist(rv$datExpr), method = "average")
        rv$wgcna_complete <- FALSE
        add_wgcna_log("Outlier exclusion cleared; all samples restored.")
        showNotification("All samples restored.", type = "message", duration = 3)
      } else {
        showNotification("Enter sample IDs to exclude (comma-separated), or leave empty to keep all.", type = "message", duration = 4)
      }
      return()
    }
    to_exclude <- trimws(strsplit(txt, split = "[,]")[[1]])
    to_exclude <- to_exclude[nzchar(to_exclude)]
    if (length(to_exclude) == 0) return()
    sample_ids <- rownames(rv$datExpr)
    found <- intersect(to_exclude, sample_ids)
    not_found <- setdiff(to_exclude, sample_ids)
    if (length(found) == 0) {
      showNotification(
        paste("None of the given IDs match current samples. Current sample IDs: ", paste(head(sample_ids, 10), collapse = ", "), if (length(sample_ids) > 10) " ..." else ""),
        type = "warning", duration = 6)
      return()
    }
    # Store backup on first exclusion so we can restore if user clears the box
    if (is.null(rv$wgcna_datExpr_before_exclude)) {
      rv$wgcna_datExpr_before_exclude <- rv$datExpr
      rv$wgcna_sample_info_before_exclude <- rv$wgcna_sample_info
    }
    keep <- setdiff(sample_ids, found)
    rv$datExpr <- rv$datExpr[keep, , drop = FALSE]
    rv$wgcna_sample_info <- rv$wgcna_sample_info[keep, , drop = FALSE]
    rv$wgcna_excluded_samples <- found
    rv$wgcna_sample_tree <- hclust(dist(rv$datExpr), method = "average")
    rv$wgcna_complete <- FALSE
    add_wgcna_log(paste("Excluded", length(found), "samples (outlier check):", paste(found, collapse = ", ")))
    if (length(not_found) > 0) {
      add_wgcna_log(paste("Sample IDs not found (ignored):", paste(not_found, collapse = ", ")))
    }
    showNotification(
      paste("Excluded ", length(found), " sample(s). Re-run 'Build Network' (Step 3) if you had already run it.", sep = ""),
      type = "message", duration = 5)
  })

  output$wgcna_exclude_status <- renderUI({
    if (!isTRUE(rv$wgcna_prepared)) return(NULL)
    excl <- rv$wgcna_excluded_samples
    if (length(excl) == 0) {
      tags$p(tags$small(icon("check"), " No samples excluded.", style = "color: #28a745; margin-top: 8px;"))
    } else {
      tags$p(
        tags$small(
          icon("user-minus"), " Excluded: ", length(excl), " sample(s) — ",
          paste(excl, collapse = ", "),
          style = "color: #856404; margin-top: 8px;"
        )
      )
    }
  })

  output$wgcna_sample_tree <- renderPlot({
    req(rv$wgcna_sample_tree, rv$datExpr)
    ht <- rv$wgcna_sample_tree
    n_samp <- nrow(rv$datExpr)
    n_genes <- ncol(rv$datExpr)
    d <- as.dendrogram(ht)
    # Style branches
    set_edge_par <- function(node) {
      if (is.leaf(node)) return(node)
      attr(node, "edgePar") <- list(col = "#2980b9", lwd = 1.4)
      node
    }
    d <- dendrapply(d, set_edge_par)
    # Small leaf labels so sample IDs fit (scale by number of samples to limit overlap)
    leaf_cex <- max(0.25, min(0.55, 28 / n_samp))
    set_leaf_par <- function(node) {
      if (!is.leaf(node)) return(node)
      attr(node, "nodePar") <- list(cex = leaf_cex, pch = NA)
      node
    }
    d <- dendrapply(d, set_leaf_par)
    # Extra bottom margin for sample ID labels
    op <- par(mar = c(6, 4.5, 5, 1.5), bg = "white", fg = "#2c3e50", xpd = NA,
              plt = c(0.14, 0.98, 0.22, 0.88))
    on.exit(par(op), add = TRUE)
    plot(d, main = "", xlab = "", ylab = "Height", sub = "", leaflab = "perpendicular", axes = FALSE)
    title(main = "Sample clustering (outlier check)", line = 2.8, col.main = "#1a252f", font.main = 2, cex.main = 1.2)
    mtext(side = 3, text = sprintf("%s samples × %s genes", format(n_samp, big.mark = ","), format(n_genes, big.mark = ",")), line = 1.4, cex = 0.9, col = "#5d6d7e")
    h_max <- max(ht$height)
    y_ticks <- pretty(c(0, h_max), n = 5)
    axis(2, at = y_ticks, col = "#5d6d7e", col.axis = "#2c3e50", cex.axis = 0.9, las = 1)
    box(col = "gray85", lwd = 1)
  }, width = 960, height = 420, res = 96)
  
  # ---------- STEP 2: SOFT THRESHOLD ----------
  observeEvent(input$pick_soft_threshold, {
    req(rv$datExpr)
    
    # Clear any stale connections from download/other steps to avoid "invalid connection" errors
    closeAllConnections()
    
    # Disable button
    shinyjs::disable("pick_soft_threshold")
    shinyjs::html("pick_soft_threshold", 
                  HTML('<i class="fa fa-spinner fa-spin"></i> Calculating...'))
    
    showNotification(
      tags$div(
        tags$div(class = "status-indicator processing"),
        tags$strong("Calculating soft threshold..."),
        tags$br(),
        tags$span("This may take a few minutes. Please wait..."),
        style = "font-size: 13px;"
      ),
      type = "message",
      duration = NULL,
      id = "wgcna_threshold_processing"
    )
    
    tryCatch({
      if (!requireNamespace("WGCNA", quietly = TRUE)) {
        stop("WGCNA package not installed.")
      }
      
      power_max <- max(12L, min(30L, as.integer(input$wgcna_power_max)))
      powers <- c(1:10, seq(12, power_max, by = 2))
      add_wgcna_log(paste("Calculating soft threshold for powers:", paste(powers, collapse = ", ")))
      
      # Use a local copy of datExpr to avoid reactive access during long computation
      datExpr <- as.matrix(rv$datExpr)
      n_genes <- ncol(datExpr)
      # Always use blockSize to avoid connection/memory errors (especially on Windows with threading)
      block_size <- if (n_genes > 10000) 2000L else if (n_genes > 5000) 4000L else 5000L
      # Disable WGCNA multi-threading to avoid "invalid connection" (parallel backend can fail in Shiny/Windows)
      tryCatch(
        if (is.function(get0("disableWGCNAThreads", envir = getNamespace("WGCNA"), inherits = FALSE)))
          WGCNA::disableWGCNAThreads(),
        error = function(e) NULL
      )
      # verbose = 0 to avoid writing to stdout (can trigger connection errors in Shiny)
      sft <- WGCNA::pickSoftThreshold(datExpr, powerVector = powers, verbose = 0,
                                      networkType = "signed", blockSize = block_size)
      
      rv$soft_threshold <- sft
      rv$soft_threshold_powers <- powers
      # Auto-fill Step 3 power so user can go straight to Build Network
      rec_power <- sft$powerEstimate
      if ((is.na(rec_power) || length(rec_power) == 0) && !is.null(sft$fitIndices) && nrow(sft$fitIndices) > 0) {
        signedR2 <- -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2]
        rec_power <- sft$fitIndices[which.max(signedR2), 1]
      }
      if (length(rec_power) && !is.na(rec_power)) {
        rec_power <- max(1L, min(30L, as.integer(rec_power)))
        tryCatch(updateNumericInput(session, "wgcna_power", value = rec_power), error = function(e) NULL)
      }
      add_wgcna_log("Soft-threshold calculation complete")
      
      shinyjs::enable("pick_soft_threshold")
      shinyjs::html("pick_soft_threshold", 
                    HTML('<i class="fa fa-check-circle"></i> Calculate Soft Threshold'))
      
      removeNotification("wgcna_threshold_processing")
      
      showNotification("Soft-threshold calculation complete.", type = "message", duration = 5)
      
    }, error = function(e) {
      closeAllConnections()
      shinyjs::enable("pick_soft_threshold")
      shinyjs::html("pick_soft_threshold", 
                    HTML('<i class="fa fa-exclamation-triangle"></i> Calculate Soft Threshold'))
      removeNotification("wgcna_threshold_processing")
      msg <- conditionMessage(e)
      add_wgcna_log(paste("ERROR:", msg))
      if (grepl("connection|invalid", msg, ignore.case = TRUE)) {
        showNotification(
          "Soft threshold failed (connection/memory). Connections were reset. If you ran Download earlier, try again; otherwise reduce genes or power range.",
          type = "error", duration = 12
        )
      } else {
        showNotification(paste("Error:", msg), type = "error", duration = 10)
      }
    })
  })
  
  output$soft_threshold_plot <- renderPlot({
    req(rv$soft_threshold)
    sft <- rv$soft_threshold
    if (!is.null(rv$soft_threshold_powers)) {
      powers <- rv$soft_threshold_powers
    } else {
      powers <- sft$fitIndices[, 1]
    }
    op <- par(mfrow = c(1, 2), bg = "white", fg = "#2c3e50", col.main = "#1a252f", font.main = 2, col.axis = "#34495e", col.lab = "#34495e")
    on.exit(par(op), add = TRUE)
    # Scale independence plot
    x1 <- sft$fitIndices[, 1]
    y1 <- -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2]
    plot(x1, y1, xlab = "Soft Threshold (power)", ylab = "Scale Free Topology Model Fit, signed R²",
         type = "n", main = "Scale independence", col.main = "#1a252f")
    points(x1, y1, pch = 19, col = "#2980b9", cex = 1.2)
    text(x1, y1, labels = powers, cex = 0.85, col = "#1a252f", pos = 4, offset = 0.3)
    abline(h = 0.8, col = "#e74c3c", lty = 2, lwd = 1.5)
    box(col = "gray85", lwd = 1)
    # Mean connectivity plot
    y2 <- sft$fitIndices[, 5]
    plot(x1, y2, xlab = "Soft Threshold (power)", ylab = "Mean Connectivity",
         type = "n", main = "Mean connectivity", col.main = "#1a252f")
    points(x1, y2, pch = 19, col = "#27ae60", cex = 1.2)
    text(x1, y2, labels = powers, cex = 0.85, col = "#1a252f", pos = 4, offset = 0.3)
    box(col = "gray85", lwd = 1)
  })
  
  output$soft_threshold_result <- renderUI({
    req(rv$soft_threshold)
    
    sft <- rv$soft_threshold
    best_power <- if (length(sft$powerEstimate)) sft$powerEstimate else NA
    fitIndices <- sft$fitIndices
    if (is.null(fitIndices) || nrow(fitIndices) < 1) {
      return(tags$div(class = "alert alert-warning", icon("info-circle"), "No fit indices. Choose power 6–12 in Step 3."))
    }
    signedR2 <- -sign(fitIndices[, 3]) * fitIndices[, 2]
    max_r2 <- max(signedR2, na.rm = TRUE)
    power_at_max <- fitIndices[which.max(signedR2), 1]
    if (length(power_at_max) > 1) power_at_max <- power_at_max[1]
    
    if (is.na(best_power)) {
      best_hint <- if (length(power_at_max) && !is.na(power_at_max))
        paste0("Best R² = ", round(max_r2, 2), " at power ", power_at_max, ". ") else ""
      tags$div(
        class = "alert alert-warning",
        icon("info-circle"),
        "No power reached R² > 0.8. ", best_hint,
        "Choose power manually (typically 6–12); higher power = fewer connections."
      )
    } else {
      tags$div(
        class = "alert alert-success",
        icon("check-circle"),
        tags$strong("Recommended power: ", best_power),
        tags$br(),
        "Use this value in Step 3 for network construction."
      )
    }
  })
  
  # ---------- STEP 3: NETWORK CONSTRUCTION ----------
  observeEvent(input$run_wgcna, {
    req(rv$datExpr)
    
    # Disable button
    shinyjs::disable("run_wgcna")
    shinyjs::html("run_wgcna", 
                  HTML('<i class="fa fa-spinner fa-spin"></i> Building Network...'))
    
    rv$wgcna_start <- Sys.time()
    rv$wgcna_running <- TRUE
    
    showNotification(
      tags$div(
        tags$div(class = "status-indicator processing"),
        tags$strong("WGCNA network construction..."),
        tags$br(),
        tags$span("This may take several minutes. Please wait..."),
        style = "font-size: 13px;"
      ),
      type = "message",
      duration = NULL,
      id = "wgcna_network_processing"
    )
    
    withProgress(message = "Running WGCNA network construction", value = 0.5, {
      tryCatch({
        if (!requireNamespace("WGCNA", quietly = TRUE)) {
          stop("WGCNA package not installed.")
        }
        # Avoid threading issues on some systems
        if (exists("disableWGCNAThreads", mode = "function", where = asNamespace("WGCNA"))) {
          WGCNA::disableWGCNAThreads()
        }
        
        # Ensure matrix: samples as rows, genes as columns
        datExpr <- as.matrix(rv$datExpr)
        if (ncol(datExpr) < 10 || nrow(datExpr) < 3) {
          stop("datExpr too small: need at least 10 genes and 3 samples. Re-run Step 1 with more genes/samples.")
        }
        
        # Safe defaults so run completes and graph appears (no error from missing/invalid inputs)
        power <- tryCatch(max(1L, min(30L, as.integer(input$wgcna_power))), error = function(e) 6L)
        if (length(power) == 0 || is.na(power)) power <- 6L
        minModuleSize <- tryCatch(max(10L, min(200L, as.integer(input$wgcna_min_module_size))), error = function(e) 30L)
        if (length(minModuleSize) == 0 || is.na(minModuleSize)) minModuleSize <- 30L
        mergeCutHeight <- 0.25  # fixed default: merge modules with eigengene cor > 0.75
        deepSplit <- tryCatch(max(0L, min(4L, as.integer(input$wgcna_deep_split))), error = function(e) 2L)
        if (length(deepSplit) == 0 || is.na(deepSplit)) deepSplit <- 2L
        
        add_wgcna_log(paste("Building network with power =", power, 
                          ", minModuleSize =", minModuleSize,
                          ", mergeCutHeight =", mergeCutHeight))
        
        # Build adjacency matrix
        incProgress(0.2, detail = "Building adjacency matrix...")
        adjacency <- WGCNA::adjacency(datExpr, power = power, type = "signed")
        
        # Calculate TOM
        incProgress(0.3, detail = "Calculating TOM...")
        TOM <- WGCNA::TOMsimilarity(adjacency, TOMType = "signed")
        dissTOM <- 1 - TOM
        rm(adjacency)
        gc(verbose = FALSE)
        
        # Gene tree
        incProgress(0.4, detail = "Building gene tree...")
        geneTree <- hclust(as.dist(dissTOM), method = "average")
        # Store tree early so dendrogram can render even if later steps fail
        rv$geneTree <- geneTree
        
        # Dynamic module detection (deepSplit: 0=conservative, 4=more modules)
        # cutreeDynamic is from dynamicTreeCut (WGCNA dependency), not exported by WGCNA
        incProgress(0.6, detail = "Detecting modules...")
        dynamicMods <- dynamicTreeCut::cutreeDynamic(
          dendro = geneTree,
          distM = dissTOM,
          deepSplit = deepSplit,
          pamRespectsDendro = FALSE,
          minClusterSize = minModuleSize
        )
        # Ensure integer module labels; NA -> 0 (grey) so labels2colors never fails
        dynamicMods <- as.integer(dynamicMods)
        dynamicMods[is.na(dynamicMods)] <- 0L
        dynamicColors <- WGCNA::labels2colors(dynamicMods)
        if (length(dynamicColors) != ncol(datExpr)) {
          stop("Module assignment length (", length(dynamicColors), ") != number of genes (", ncol(datExpr), ").")
        }
        
        # Module eigengenes
        incProgress(0.8, detail = "Calculating module eigengenes...")
        MEList <- tryCatch(
          WGCNA::moduleEigengenes(datExpr, colors = dynamicColors),
          error = function(e) { add_wgcna_log(paste("moduleEigengenes warning:", e$message)); NULL }
        )
        MEs <- if (!is.null(MEList)) MEList$eigengenes else NULL
        
        # Merge similar modules (or use unmerged / single-module fallback)
        incProgress(0.9, detail = "Merging modules...")
        mergedColors <- dynamicColors
        mergedMEs <- MEs
        if (is.null(MEs) || ncol(MEs) < 1) {
          me_vec <- as.numeric(scale(rowMeans(datExpr, na.rm = TRUE)))
          mergedMEs <- data.frame(MEgrey = me_vec, row.names = rownames(datExpr))
          add_wgcna_log("Single module (e.g. all grey); storing as-is.")
        } else {
          merge_result <- tryCatch({
            MEDiss <- 1 - cor(MEs, use = "pairwise.complete.obs")
            MEDiss[is.na(MEDiss)] <- 1
            WGCNA::mergeCloseModules(datExpr, dynamicColors,
                                     cutHeight = mergeCutHeight, verbose = 3, trapErrors = TRUE)
          }, error = function(e) { add_wgcna_log(paste("mergeCloseModules:", e$message)); NULL })
          if (!is.null(merge_result) && !is.null(merge_result$colors) && isTRUE(merge_result$allOK)) {
            mergedColors <- merge_result$colors
            mergedMEs <- if (!is.null(merge_result$newMEs)) merge_result$newMEs else MEs
          } else {
            add_wgcna_log("Using unmerged module colors.")
          }
        }
        
        # Always store results so UI updates (figures and data appear)
        rv$moduleColors <- mergedColors
        rv$dynamicColors <- mergedColors
        rv$MEs <- mergedMEs
        rv$wgcna_complete <- TRUE
        
        n_modules <- length(unique(mergedColors))
        add_wgcna_log(paste("WGCNA network construction complete. Detected", n_modules, "modules"))
        
        # Re-enable button and update UI
        shinyjs::enable("run_wgcna")
        shinyjs::html("run_wgcna", 
                      HTML('<i class="fa fa-check-circle"></i> Build Network'))
        removeNotification("wgcna_network_processing")
        showNotification(
          tags$div(
            tags$strong("✓ Network construction complete!"),
            tags$br(),
            tags$span("Modules detected: ", n_modules),
            style = "font-size: 13px;"
          ),
          type = "message", duration = 6
        )
        
    }, error = function(e) {
      rv$wgcna_running <- FALSE
      shinyjs::enable("run_wgcna")
      shinyjs::html("run_wgcna", 
                    HTML('<i class="fa fa-exclamation-triangle"></i> Build Network'))
      removeNotification("wgcna_network_processing")
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
      add_wgcna_log(paste("ERROR:", e$message))
    })
  })
  
  rv$wgcna_running <- FALSE
})
  
  output$wgcna_module_info <- renderUI({
    wgcna_complete <- isTRUE(rv$wgcna_complete)
    module_colors <- rv$moduleColors
    
    if (!wgcna_complete) {
      tags$div(
        class = "alert alert-warning",
        icon("exclamation-triangle"),
        "Run 'Build Network' to compute modules."
      )
    } else {
      n_modules <- length(unique(module_colors))
      module_table <- table(module_colors)
      
      tags$div(
        class = "alert alert-success",
        icon("check-circle"),
        tags$strong("Modules detected: ", n_modules),
        tags$br(),
        tags$small("Module sizes: ", paste(names(module_table), "=", module_table, collapse = ", "))
      )
    }
  })
  
  output$wgcna_dendrogram <- renderPlot({
    req(rv$geneTree, rv$moduleColors)
    
    if (!requireNamespace("WGCNA", quietly = TRUE)) {
      par(bg = "white"); plot.new(); text(0.5, 0.5, "WGCNA package required", cex = 1.2)
      return()
    }
    op <- par(bg = "white", fg = "#2c3e50")
    on.exit(par(op), add = TRUE)
    WGCNA::plotDendroAndColors(rv$geneTree,
                               rv$moduleColors,
                               "Module Colors",
                               main = "Gene Dendrogram and Module Colors",
                               dendroLabels = FALSE, hang = 0.03,
                               addGuide = TRUE, guideHang = 0.05)
  })
  
  # ---------- STEP 4: MODULE-TRAIT & GS/MM ----------
  observeEvent(input$calculate_module_trait, {
    req(rv$datExpr, rv$dynamicColors, rv$wgcna_sample_info)
    
    # Disable button
    shinyjs::disable("calculate_module_trait")
    shinyjs::html("calculate_module_trait", 
                  HTML('<i class="fa fa-spinner fa-spin"></i> Calculating...'))
    
    showNotification(
      tags$div(
        tags$div(class = "status-indicator processing"),
        tags$strong("Calculating module-trait correlations..."),
        tags$br(),
        tags$span("Computing GS, MM, and module-trait relationships. Please wait..."),
        style = "font-size: 13px;"
      ),
      type = "message",
      duration = NULL,
      id = "wgcna_module_trait_processing"
    )
    
    withProgress(message = "Calculating GS, MM & module-trait correlations", value = 0.3, {
      tryCatch({
        if (!requireNamespace("WGCNA", quietly = TRUE)) {
          stop("WGCNA package not installed.")
        }
        
        if (length(rv$dynamicColors) != ncol(rv$datExpr)) {
          stop("ERROR: dynamicColors and datExpr mismatch. Rebuild network.")
        }
        
        add_wgcna_log("Calculating module eigengenes...")
        MEs <- WGCNA::moduleEigengenes(rv$datExpr,
                                      rv$dynamicColors)$eigengenes
        rv$MEs <- MEs
        
        # Prepare trait data (use Condition if present, else first factor/numeric)
        sample_info <- rv$wgcna_sample_info
        cond_col <- if ("Condition" %in% names(sample_info)) "Condition" else NULL
        if (is.null(cond_col)) {
          fac_cols <- names(sample_info)[vapply(sample_info, function(x) is.factor(x) || is.character(x), logical(1))]
          if (length(fac_cols) > 0) cond_col <- fac_cols[1]
        }
        if (!is.null(cond_col)) {
          unique_groups <- unique(sample_info[[cond_col]])
          unique_groups <- unique_groups[!is.na(unique_groups) & as.character(unique_groups) != ""]
        } else {
          unique_groups <- character(0)
        }
        if (length(unique_groups) <= 1) {
          trait_data <- data.frame(Trait1 = rnorm(nrow(sample_info)))
          rownames(trait_data) <- rownames(sample_info)
        } else {
          trait_data <- data.frame(row.names = rownames(sample_info))
          for (g in unique_groups) {
            trait_data[[as.character(g)]] <- ifelse(sample_info[[cond_col]] == g, 1, 0)
          }
        }
        rv$trait_data <- trait_data
        
        add_wgcna_log("Calculating module-trait correlations...")
        moduleTraitCor <- cor(MEs, trait_data, use = "pairwise.complete.obs")
        moduleTraitPvalue <- WGCNA::corPvalueStudent(moduleTraitCor,
                                                    nSamples = nrow(MEs))
        
        rv$moduleTraitCor <- moduleTraitCor
        rv$moduleTraitPvalue <- moduleTraitPvalue
        
        add_wgcna_log("Calculating Gene Significance (GS) and Module Membership (MM)...")
        trait_vector <- trait_data[, 1]
        
        geneTraitSignificance <- as.data.frame(
          cor(rv$datExpr, trait_vector, use = "p")
        )
        names(geneTraitSignificance) <- "GS"
        GSPvalue <- as.data.frame(
          WGCNA::corPvalueStudent(as.matrix(geneTraitSignificance),
                                 nrow(rv$datExpr))
        )
        names(GSPvalue) <- "GS.pvalue"
        
        modNames <- substring(names(MEs), 3)
        geneModuleMembership <- as.data.frame(
          cor(rv$datExpr, MEs, use = "p")
        )
        MMPvalue <- as.data.frame(
          WGCNA::corPvalueStudent(as.matrix(geneModuleMembership),
                                 nrow(rv$datExpr))
        )
        names(geneModuleMembership) <- paste("MM", modNames, sep = "")
        names(MMPvalue) <- paste("p.MM", modNames, sep = "")
        
        # Create gene metrics dataframe
        gene_metrics <- data.frame(
          Gene = colnames(rv$datExpr),
          Module = rv$dynamicColors,
          GS = geneTraitSignificance$GS,
          GS.pvalue = GSPvalue$GS.pvalue,
          stringsAsFactors = FALSE
        )
        
        gene_metrics$MM <- NA
        gene_metrics$MM.pvalue <- NA
        
        for (i in seq_len(nrow(gene_metrics))) {
          module_col <- paste0("MM", gene_metrics$Module[i])
          p_col <- paste0("p.MM", gene_metrics$Module[i])
          if (module_col %in% names(geneModuleMembership)) {
            gene_metrics$MM[i] <- geneModuleMembership[i, module_col]
            gene_metrics$MM.pvalue[i] <- MMPvalue[i, p_col]
          }
        }
        
        rv$gene_metrics <- gene_metrics
        rv$geneModuleMembership <- geneModuleMembership
        rv$MMPvalue <- MMPvalue
        
        add_wgcna_log(paste("✓ GS/MM calculated for", nrow(gene_metrics), "genes"))
        add_wgcna_log("Module-trait correlation complete")
        
        # Re-enable button
        shinyjs::enable("calculate_module_trait")
        shinyjs::html("calculate_module_trait", 
                      HTML('<i class="fa fa-check-circle"></i> Calculate Module-Trait'))
        
        removeNotification("wgcna_module_trait_processing")
        
        showNotification("Module-trait & GS/MM calculated", type = "message", duration = 5)
        
      }, error = function(e) {
        shinyjs::enable("calculate_module_trait")
        shinyjs::html("calculate_module_trait", 
                      HTML('<i class="fa fa-exclamation-triangle"></i> Calculate Module-Trait'))
        removeNotification("wgcna_module_trait_processing")
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
        add_wgcna_log(paste("ERROR:", e$message))
      })
    })
  })
  
  output$module_trait_heatmap <- renderPlot({
    req(rv$moduleTraitCor)
    
    if (!requireNamespace("WGCNA", quietly = TRUE)) {
      par(bg = "white"); plot.new(); text(0.5, 0.5, "WGCNA package required", cex = 1.2)
      return()
    }
    op <- par(bg = "white", fg = "#2c3e50")
    on.exit(par(op), add = TRUE)
    moduleTraitCor <- rv$moduleTraitCor
    WGCNA::labeledHeatmap(
      Matrix = moduleTraitCor,
      xLabels = colnames(rv$trait_data),
      yLabels = rownames(moduleTraitCor),
      ySymbols = rownames(moduleTraitCor),
      colorLabels = FALSE,
      colors = WGCNA::blueWhiteRed(50),
      textMatrix = NULL,
      main = "Module-Trait Relationships"
    )
  })
  
  output$significant_module_count_ui <- renderUI({
    req(rv$moduleTraitCor, rv$moduleTraitPvalue)
    
    p_threshold <- input$gs_pval_threshold
    cor_threshold <- input$mm_cor_threshold
    n_pos <- 0
    n_neg <- 0
    
    for (module in rownames(rv$moduleTraitPvalue)) {
      pval <- rv$moduleTraitPvalue[module, 1]
      corval <- rv$moduleTraitCor[module, 1]
      if (!is.na(pval) && !is.na(corval) && pval < p_threshold) {
        if (corval > cor_threshold) n_pos <- n_pos + 1
        if (corval < -cor_threshold) n_neg <- n_neg + 1
      }
    }
    
    tags$div(
      class = "alert alert-info",
      tags$p(tags$b("Significant Modules Total:"), n_pos + n_neg),
      tags$p(icon("arrow-up", class = "text-success"), " Positive: ", n_pos,
             " | ",
             icon("arrow-down", class = "text-danger"), " Negative: ", n_neg)
    )
  })
  
  wgcna_module_trait_to_file <- function(file, dev_fun) {
    req(rv$moduleTraitCor)
    if (!requireNamespace("WGCNA", quietly = TRUE)) stop("WGCNA package required")
    moduleTraitCor <- rv$moduleTraitCor
    dev_fun(file)
    par(bg = "white")
    WGCNA::labeledHeatmap(
      Matrix = moduleTraitCor,
      xLabels = colnames(rv$trait_data),
      yLabels = rownames(moduleTraitCor),
      ySymbols = rownames(moduleTraitCor),
      colorLabels = FALSE,
      colors = WGCNA::blueWhiteRed(50),
      textMatrix = NULL,
      main = "Module-Trait Relationships"
    )
    dev.off()
  }

  output$download_module_trait <- downloadHandler(
    filename = function() "module_trait_heatmap.png",
    content = function(file) {
      wgcna_module_trait_to_file(file, function(f) png(f, width = 6 * IMAGE_DPI, height = 8 * IMAGE_DPI, res = IMAGE_DPI, bg = "white"))
    }
  )
  output$download_module_trait_jpg <- downloadHandler(
    filename = function() "module_trait_heatmap.jpg",
    content = function(file) {
      wgcna_module_trait_to_file(file, function(f) jpeg(f, width = 6, height = 8, res = IMAGE_DPI, units = "in", bg = "white", quality = 95))
    }
  )
  output$download_module_trait_pdf <- downloadHandler(
    filename = function() "module_trait_heatmap.pdf",
    content = function(file) {
      wgcna_module_trait_to_file(file, function(f) pdf(f, width = 6, height = 8, bg = "white"))
    }
  )
  
  # ========== STEP 5: ME RELATIONSHIPS ==========
  observeEvent(input$calculate_me_relationships, {
    req(rv$MEs)
    
    shinyjs::disable("calculate_me_relationships")
    shinyjs::html("calculate_me_relationships", 
                  HTML('<i class="fa fa-spinner fa-spin"></i> Calculating...'))
    
    tryCatch({
      add_wgcna_log("Calculating module eigengene relationships...")
      
      # ME correlation
      ME_cor <- cor(rv$MEs, use = "pairwise.complete.obs")
      rv$ME_correlation <- ME_cor
      
      # ME dendrogram (handle NA from constant eigengenes)
      ME_cor[is.na(ME_cor)] <- 0
      ME_dist <- as.dist(1 - ME_cor)
      ME_tree <- hclust(ME_dist, method = "average")
      rv$ME_tree <- ME_tree
      
      add_wgcna_log("Module eigengene relationships calculated")
      
      shinyjs::enable("calculate_me_relationships")
      shinyjs::html("calculate_me_relationships", 
                    HTML('<i class="fa fa-link"></i> Calculate Module Relationships'))
      
      showNotification("Module eigengene relationships calculated", type = "message", duration = 5)
      
    }, error = function(e) {
      shinyjs::enable("calculate_me_relationships")
      shinyjs::html("calculate_me_relationships", 
                    HTML('<i class="fa fa-link"></i> Calculate Module Relationships'))
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
      add_wgcna_log(paste("ERROR:", e$message))
    })
  })
  
  output$me_correlation_heatmap <- renderPlot({
    req(rv$ME_correlation)
    
    if (!requireNamespace("WGCNA", quietly = TRUE)) {
      par(bg = "white"); plot.new(); text(0.5, 0.5, "WGCNA package required", cex = 1.2)
      return()
    }
    op <- par(bg = "white", fg = "#2c3e50")
    on.exit(par(op), add = TRUE)
    WGCNA::labeledHeatmap(
      Matrix = rv$ME_correlation,
      xLabels = names(rv$MEs),
      yLabels = names(rv$MEs),
      ySymbols = names(rv$MEs),
      colorLabels = FALSE,
      colors = WGCNA::blueWhiteRed(50),
      textMatrix = round(rv$ME_correlation, 2),
      main = "Module Eigengene Correlation"
    )
  })
  
  output$me_dendrogram_plot <- renderPlot({
    req(rv$ME_tree)
    ht <- rv$ME_tree
    d <- as.dendrogram(ht)
    set_edge_par <- function(node) {
      if (is.leaf(node)) return(node)
      attr(node, "edgePar") <- list(col = "#8e44ad", lwd = 1.4)
      node
    }
    d <- dendrapply(d, set_edge_par)
    op <- par(mar = c(4, 4, 4, 2), bg = "white", fg = "#2c3e50")
    on.exit(par(op), add = TRUE)
    plot(d, main = "Module Eigengene Dendrogram", xlab = "", ylab = "Height", leaflab = "perpendicular", cex = 0.8, col.main = "#1a252f")
    axis(2, col = "#5d6d7e", col.axis = "#2c3e50", cex.axis = 0.9, las = 1)
    box(col = "gray85", lwd = 1)
  })
  
  output$me_scatter_plot <- renderPlot({
    req(rv$MEs, ncol(rv$MEs) >= 2)
    
    df <- data.frame(
      ME1 = rv$MEs[, 1],
      ME2 = rv$MEs[, 2]
    )
    
    ggplot(df, aes(x = ME1, y = ME2)) +
      geom_point(color = "#3498db", alpha = 0.7, size = 3) +
      theme_bw(base_size = 14) +
      labs(
        title = "Module Eigengene Scatter Plot",
        x = names(rv$MEs)[1],
        y = names(rv$MEs)[2]
      ) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        panel.grid.major = element_line(color = "gray92"),
        panel.grid.minor = element_line(color = "gray97")
      )
  })
  
  output$eigengene_distance_heatmap <- renderPlot({
    req(rv$ME_correlation)
    
    if (!requireNamespace("pheatmap", quietly = TRUE)) {
      par(bg = "white"); plot.new(); text(0.5, 0.5, "pheatmap package required", cex = 1.2)
      return()
    }
    op <- par(bg = "white")
    on.exit(par(op), add = TRUE)
    dist_matrix <- as.matrix(as.dist(1 - rv$ME_correlation))
    
    pheatmap::pheatmap(
      dist_matrix,
      color = colorRampPalette(c("#3498db", "white", "#e74c3c"))(100),
      main = "Eigengene Distance Heatmap",
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      border_color = "gray90",
      fontsize = 10
    )
  })
  
  # ========== STEP 6: SIGNIFICANT MODULE ANALYSIS ==========
  observeEvent(input$identify_significant_modules, {
    req(rv$moduleTraitCor, rv$moduleTraitPvalue)
    
    shinyjs::disable("identify_significant_modules")
    shinyjs::html("identify_significant_modules", 
                  HTML('<i class="fa fa-spinner fa-spin"></i> Identifying...'))
    
    tryCatch({
      p_threshold <- input$sig_module_pval_threshold
      cor_threshold <- input$sig_module_cor_threshold
      
      add_wgcna_log(paste("Identifying significant modules with p <", p_threshold, 
                        "and |cor| >", cor_threshold))
      
      sig_modules <- data.frame(
        Module = character(),
        Correlation = numeric(),
        Pvalue = numeric(),
        Size = integer(),
        stringsAsFactors = FALSE
      )
      
      for (module in rownames(rv$moduleTraitPvalue)) {
        pval <- rv$moduleTraitPvalue[module, 1]
        corval <- rv$moduleTraitCor[module, 1]
        # Row names are ME names (e.g. MEblue); moduleColors are colors (e.g. blue)
        module_color <- if (grepl("^ME", module)) substring(module, 3) else module
        if (!is.na(pval) && !is.na(corval) && 
            pval < p_threshold && abs(corval) > cor_threshold) {
          module_size <- sum(rv$moduleColors == module_color, na.rm = TRUE)
          sig_modules <- rbind(sig_modules, data.frame(
            Module = module,
            Correlation = corval,
            Pvalue = pval,
            Size = module_size,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      rv$significant_modules <- sig_modules
      
      add_wgcna_log(paste("Found", nrow(sig_modules), "significant modules"))
      
      shinyjs::enable("identify_significant_modules")
      shinyjs::html("identify_significant_modules", 
                    HTML('<i class="fa fa-search"></i> Identify Significant Modules'))
      
      showNotification(
        paste("Identified", nrow(sig_modules), "significant modules"),
        type = "message", duration = 5
      )
      
    }, error = function(e) {
      shinyjs::enable("identify_significant_modules")
      shinyjs::html("identify_significant_modules", 
                    HTML('<i class="fa fa-search"></i> Identify Significant Modules'))
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
      add_wgcna_log(paste("ERROR:", e$message))
    })
  })
  
  output$significant_modules_summary_ui <- renderUI({
    req(rv$significant_modules)
    
    sig_mods <- rv$significant_modules
    n_pos <- sum(sig_mods$Correlation > 0)
    n_neg <- sum(sig_mods$Correlation < 0)
    
    tags$div(
      class = "alert alert-info",
      tags$p(tags$b("Significant Modules Found: "), nrow(sig_mods)),
      tags$p(icon("arrow-up", class = "text-success"), " Positive: ", n_pos,
             " | ",
             icon("arrow-down", class = "text-danger"), " Negative: ", n_neg)
    )
  })
  
  output$module_significance_barplot <- renderPlot({
    req(rv$significant_modules)
    
    sig_mods <- rv$significant_modules
    sig_mods$Module <- factor(sig_mods$Module, 
                             levels = sig_mods$Module[order(abs(sig_mods$Correlation), decreasing = TRUE)])
    # Original module color (strip): positive = upper side, negative = lower side
    sig_mods$orig_color <- sapply(as.character(sig_mods$Module), function(m) {
      x <- sub("^ME", "", m)
      if (x %in% colors()) x else "gray50"
    })
    y_range <- max(sig_mods$Correlation, na.rm = TRUE) - min(sig_mods$Correlation, na.rm = TRUE)
    strip_height <- if (y_range > 0) 0.02 * y_range else 0.02
    # Bar segments: green (positive) or red (negative)
    bar_df <- data.frame(
      Module = sig_mods$Module,
      ymin = 0,
      ymax = sig_mods$Correlation,
      fill_val = ifelse(sig_mods$Correlation > 0, "#2ecc71", "#e74c3c"),
      stringsAsFactors = FALSE
    )
    # Strip segments: original module color on upper (positive) or lower (negative) side
    strip_df <- data.frame(
      Module = sig_mods$Module,
      ymin = ifelse(sig_mods$Correlation > 0, sig_mods$Correlation, sig_mods$Correlation - strip_height),
      ymax = ifelse(sig_mods$Correlation > 0, sig_mods$Correlation + strip_height, sig_mods$Correlation),
      fill_val = sig_mods$orig_color,
      stringsAsFactors = FALSE
    )
    rect_df <- rbind(bar_df, strip_df)
    ggplot(rect_df, aes(x = Module)) +
      geom_rect(aes(xmin = as.numeric(Module) - 0.45, xmax = as.numeric(Module) + 0.45,
                    ymin = ymin, ymax = ymax, fill = fill_val)) +
      scale_fill_identity() +
      geom_hline(yintercept = 0, linewidth = 0.5, color = "gray30") +
      theme_bw(base_size = 14) +
      labs(
        title = "Module Significance Barplot",
        x = "Module",
        y = "Correlation with Trait"
      ) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(color = "gray92"),
        panel.grid.minor = element_line(color = "gray97")
      )
  })
  
  output$module_size_correlation_plot <- renderPlot({
    req(rv$significant_modules)
    
    sig_mods <- rv$significant_modules
    sig_mods$Direction <- ifelse(sig_mods$Correlation > 0, "Positive", "Negative")
    
    ggplot(sig_mods, aes(x = Size, y = abs(Correlation), color = Direction)) +
      geom_point(size = 4, alpha = 0.7) +
      scale_color_manual(values = c("Positive" = "#2ecc71", "Negative" = "#e74c3c"), name = "Direction") +
      theme_bw(base_size = 14) +
      labs(
        title = "Module Size vs Correlation",
        x = "Module Size (number of genes)",
        y = "Absolute Correlation"
      ) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        panel.grid.major = element_line(color = "gray92"),
        panel.grid.minor = element_line(color = "gray97")
      )
  })
  
  output$significant_modules_table <- DT::renderDataTable({
    req(rv$significant_modules)
    
    sig_mods <- rv$significant_modules
    sig_mods$Correlation <- round(sig_mods$Correlation, 3)
    sig_mods$Pvalue <- format(sig_mods$Pvalue, scientific = TRUE, digits = 3)
    
    DT::datatable(
      sig_mods,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      filter = "top"
    )
  })
  
  # ========== MODULE GENES TABLE ==========
  observe({
    req(rv$moduleColors)
    
    unique_modules <- sort(unique(rv$moduleColors))
    updateSelectInput(session, "select_module",
                     choices = unique_modules,
                     selected = unique_modules[1])
  })
  
  output$module_gene_stats <- renderUI({
    req(input$select_module, rv$gene_metrics)
    
    module_genes <- rv$gene_metrics[rv$gene_metrics$Module == input$select_module, ]
    n_total <- nrow(module_genes)
    
    if (input$filter_sig_genes == "sig_only") {
      sig_genes <- module_genes[module_genes$GS.pvalue < input$gs_pval_threshold & 
                               abs(module_genes$MM) > input$mm_cor_threshold, ]
      n_sig <- nrow(sig_genes)
    } else {
      n_sig <- n_total
    }
    
    tags$div(
      class = "alert alert-info",
      style = "margin-top: 25px; padding: 10px;",
      tags$p(tags$strong("Module: ", input$select_module)),
      tags$p("Total genes: ", n_total),
      if (input$filter_sig_genes == "sig_only") {
        tags$p("Significant genes: ", n_sig)
      }
    )
  })
  
  output$module_genes_table <- DT::renderDataTable({
    req(input$select_module, rv$gene_metrics)
    
    module_genes <- rv$gene_metrics[rv$gene_metrics$Module == input$select_module, ]
    
    if (input$filter_sig_genes == "sig_only") {
      module_genes <- module_genes[
        module_genes$GS.pvalue < input$gs_pval_threshold & 
        abs(module_genes$MM) > input$mm_cor_threshold, 
      ]
    }
    
    # Format columns
    module_genes$GS <- round(module_genes$GS, 4)
    module_genes$GS.pvalue <- format(module_genes$GS.pvalue, scientific = TRUE, digits = 3)
    module_genes$MM <- round(module_genes$MM, 4)
    module_genes$MM.pvalue <- format(module_genes$MM.pvalue, scientific = TRUE, digits = 3)
    
    DT::datatable(
      module_genes,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "400px"
      ),
      rownames = FALSE,
      filter = "top"
    )
  })
  
  # ========== DOWNLOAD HANDLERS ==========
  output$download_all_sig_genes <- downloadHandler(
    filename = function() "wgcna_significant_genes.csv",
    content = function(file) {
      req(rv$gene_metrics)
      sig_genes <- rv$gene_metrics[
        rv$gene_metrics$GS.pvalue < input$gs_pval_threshold &
        abs(rv$gene_metrics$MM) > input$mm_cor_threshold,
      ]
      write.csv(sig_genes, file, row.names = FALSE)
      write.csv(sig_genes, file.path(CSV_EXPORT_DIR(), "wgcna_significant_genes.csv"), row.names = FALSE)
    }
  )
  
  output$download_sig_modules_table <- downloadHandler(
    filename = function() "wgcna_significant_modules.csv",
    content = function(file) {
      req(rv$significant_modules)
      write.csv(rv$significant_modules, file, row.names = FALSE)
      write.csv(rv$significant_modules, file.path(CSV_EXPORT_DIR(), "wgcna_significant_modules.csv"), row.names = FALSE)
    }
  )
  
  output$download_sig_modules_genes <- downloadHandler(
    filename = function() "wgcna_module_genes.zip",
    content = function(file) {
      req(rv$gene_metrics, rv$significant_modules)
      export_dir <- CSV_EXPORT_DIR()
      sig_modules <- rv$significant_modules$Module
      out_files <- character(0)
      for (mod in sig_modules) {
        mod_color <- if (grepl("^ME", mod)) substring(mod, 3) else mod
        mod_genes <- rv$gene_metrics[rv$gene_metrics$Module == mod_color, ]
        fname <- file.path(export_dir, paste0("module_", mod, "_genes.csv"))
        write.csv(mod_genes, fname, row.names = FALSE)
        out_files <- c(out_files, fname)
      }
      if (length(out_files) > 0) zip(file, files = out_files)
    }
  )
  
  output$download_module_analysis_plots <- downloadHandler(
    filename = function() "wgcna_analysis_plots.pdf",
    content = function(file) {
      req(rv$geneTree, rv$moduleColors)
      
      pdf(file, width = 12, height = 10, bg = "white")
      par(bg = "white", fg = "#2c3e50")
      
      # Dendrogram
      if (requireNamespace("WGCNA", quietly = TRUE)) {
        WGCNA::plotDendroAndColors(rv$geneTree, rv$moduleColors, "Module Colors",
                                   dendroLabels = FALSE, hang = 0.03)
      }
      
      # Module-trait heatmap
      if (!is.null(rv$moduleTraitCor)) {
        par(bg = "white")
        WGCNA::labeledHeatmap(Matrix = rv$moduleTraitCor,
                              xLabels = colnames(rv$trait_data),
                              yLabels = rownames(rv$moduleTraitCor),
                              colors = WGCNA::blueWhiteRed(50),
                              main = "Module-Trait Relationships")
      }
      
      dev.off()
    }
  )
  
  output$download_module_genes <- downloadHandler(
    filename = function() {
      paste0("module_", input$select_module, "_genes.csv")
    },
    content = function(file) {
      req(input$select_module, rv$gene_metrics)
      fn <- paste0("module_", input$select_module, "_genes.csv")
      module_genes <- rv$gene_metrics[rv$gene_metrics$Module == input$select_module, ]
      write.csv(module_genes, file, row.names = FALSE)
      write.csv(module_genes, file.path(CSV_EXPORT_DIR(), fn), row.names = FALSE)
    }
  )
  
  output$download_all_modules <- downloadHandler(
    filename = function() "wgcna_all_module_assignments.csv",
    content = function(file) {
      req(rv$gene_metrics)
      write.csv(rv$gene_metrics, file, row.names = FALSE)
      write.csv(rv$gene_metrics, file.path(CSV_EXPORT_DIR(), "wgcna_all_module_assignments.csv"), row.names = FALSE)
    }
  )
  
  output$download_module_eigengenes <- downloadHandler(
    filename = function() "wgcna_module_eigengenes.csv",
    content = function(file) {
      req(rv$MEs)
      write.csv(rv$MEs, file)
      write.csv(rv$MEs, file.path(CSV_EXPORT_DIR(), "wgcna_module_eigengenes.csv"))
    }
  )
  
  # WGCNA Processing Log
  output$wgcna_log <- renderText({
    if (!is.null(rv$wgcna_log_messages) && length(rv$wgcna_log_messages) > 0) {
      if (length(rv$wgcna_log_messages) > 300) {
        paste(c("... (showing last 300 lines) ...", 
                tail(rv$wgcna_log_messages, 300)), collapse = "\n")
      } else {
        paste(rv$wgcna_log_messages, collapse = "\n")
      }
    } else {
      "No WGCNA logs yet. Run WGCNA analysis steps to see logs."
    }
  })
  
  observeEvent(input$clear_wgcna_log_btn, {
    showNotification("Log display refreshed.", type = "message", duration = 3)
  })
}

