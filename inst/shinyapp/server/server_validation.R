# ==============================================================================
# SERVER_VALIDATION.R - Step 11: Validation Setup (External / Internal)
# ==============================================================================
# Lets user choose External (GEO dataset) or Internal (70/30 split) validation.
# External: download → phenodata → categorize → DE on validation data.
# Stores rv$validation_mode and all external validation data in rv$.
# ==============================================================================

server_validation <- function(input, output, session, rv) {

  # ---- Observe: store validation mode into rv ----
  observeEvent(input$validation_mode, {
    rv$validation_mode <- input$validation_mode
  })

  # ---- Mode description panel ----
  output$validation_mode_info_ui <- renderUI({
    mode <- input$validation_mode
    if (is.null(mode)) mode <- "external"
    if (mode == "external") {
      tags$div(
        style = "padding: 20px; background: linear-gradient(135deg, #d5f5e3 0%, #abebc6 100%); border-radius: 12px; border: 2px solid #27ae60;",
        tags$h4(icon("globe", style = "color: #27ae60;"), tags$strong(" External Validation"), style = "margin-top: 0;"),
        tags$ul(
          tags$li("Download an independent GEO dataset (GSE)"),
          tags$li("Browse phenodata and select group column"),
          tags$li("Categorize values as Normal / Disease / Exclude"),
          tags$li("Run DE analysis on the validation dataset"),
          tags$li("ROC will compare training vs external validation"),
          tags$li("Nomogram will use external dataset for validation (no 70/30 split)")
        ),
        tags$p(icon("star", style = "color: #f39c12;"), tags$strong(" Recommended for publication-quality analysis."),
               style = "color: #1e8449; margin-top: 8px; margin-bottom: 0;")
      )
    } else {
      tags$div(
        style = "padding: 20px; background: linear-gradient(135deg, #d6eaf8 0%, #aed6f1 100%); border-radius: 12px; border: 2px solid #3498db;",
        tags$h4(icon("random", style = "color: #3498db;"), tags$strong(" Internal Validation (70/30 Split)"), style = "margin-top: 0;"),
        tags$ul(
          tags$li("No additional dataset needed"),
          tags$li("ROC is computed on training (test) data"),
          tags$li("Nomogram uses 70% training / 30% validation split"),
          tags$li("Suitable for exploratory or preliminary analysis")
        ),
        tags$p(icon("check-circle", style = "color: #3498db;"), tags$strong(" Ready to proceed. Click 'Continue to ROC' below."),
               style = "color: #2471a3; margin-top: 8px; margin-bottom: 0;")
      )
    }
  })

  # ---- External Validation Config UI (conditional) ----
  output$ext_val_config_ui <- renderUI({
    if (is.null(input$validation_mode) || input$validation_mode != "external") return(NULL)

    fluidRow(
      box(
        title = tags$span(icon("globe"), " External Validation -- GEO Dataset"),
        width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        tags$p(
          icon("lightbulb", style = "color: #f39c12; margin-right: 5px;"),
          tags$strong("External validation workflow:"),
          " 1) Enter GSE IDs & select platform/DE method  2) Download data  3) Browse phenodata & select group column  4) Categorize & Run DE.",
          style = "font-size: 13px; color: #495057; margin-bottom: 14px; padding: 10px 12px; background: #fef9e7; border-left: 4px solid #f39c12; border-radius: 4px;"
        ),

        # Step A: GSE IDs, platform, DE method
        tags$h4(icon("step-forward"), " Step A: Configure & Download", style = "color: #e67e22; margin-bottom: 10px;"),
        fluidRow(
          column(4,
            textAreaInput("ext_val_gse_ids", "Validation GSE IDs:",
              value = "", rows = 2, placeholder = "e.g. GSE12345, GSE67890")
          ),
          column(3,
            radioButtons("ext_val_platform", "Platform Type:",
              choices = c("RNA-seq" = "rnaseq", "Microarray" = "microarray", "Merged (Both)" = "merged"),
              selected = "rnaseq")
          ),
          column(3,
            radioButtons("ext_val_de_method", "DE Method:",
              choices = c("limma" = "limma", "DESeq2" = "deseq2", "edgeR" = "edger"),
              selected = "limma")
          ),
          column(2,
            tags$div(style = "margin-top: 30px;",
              actionButton("ext_val_download_btn",
                tagList(icon("download"), " Download"),
                class = "btn-warning btn-block", style = "font-size: 14px;"),
              tags$div(style = "margin-top: 6px;",
                actionButton("clear_ext_validation",
                  tagList(icon("trash"), " Clear All"),
                  class = "btn-default btn-sm btn-block"))
            )
          )
        ),
        uiOutput("ext_val_log_ui"),

        # Step B: Phenodata browser & column selection (after download)
        uiOutput("ext_val_phenodata_ui"),

        # Step C: Run validation (after column selected)
        uiOutput("ext_val_run_ui"),

        # Status
        uiOutput("ext_val_status_ui")
      )
    )
  })

  # ============================================================================
  # STEP A: Download GEO validation data
  # ============================================================================
  observeEvent(input$ext_val_download_btn, {
    gse_text <- gsub("\\s+", ",", input$ext_val_gse_ids)
    gse_ids <- trimws(unlist(strsplit(gse_text, ",")))
    gse_ids <- gse_ids[nzchar(gse_ids)]
    if (length(gse_ids) == 0) {
      showNotification(tags$div(icon("exclamation-triangle"), tags$strong(" Enter at least one GSE ID.")), type = "warning", duration = 5)
      return()
    }

    platform <- input$ext_val_platform
    shinyjs::disable("ext_val_download_btn")

    withProgress(message = "Downloading validation data...", value = 0, {
      tryCatch({
        ext_log <- ""
        all_expr_list <- list()
        all_metadata_list <- list()
        rnaseq_ids <- character(0)
        micro_ids <- character(0)

        if (platform == "rnaseq") { rnaseq_ids <- gse_ids }
        else if (platform == "microarray") { micro_ids <- gse_ids }
        else { rnaseq_ids <- gse_ids; micro_ids <- gse_ids }

        # ---- Download RNA-seq ----
        for (gse_id in rnaseq_ids) {
          incProgress(0.2 / max(1, length(gse_ids)), detail = paste0(gse_id, " (RNA-seq)"))
          ext_log <- paste0(ext_log, gse_id, " (RNA-seq)... ")
          rna_dir <- file.path(getwd(), "ext_val_rna")
          dir.create(rna_dir, showWarnings = FALSE, recursive = TRUE)
          gse_dir <- file.path(rna_dir, gse_id)
          dir.create(gse_dir, showWarnings = FALSE)
          count_file <- tryCatch(download_ncbi_raw_counts(gse_id, gse_dir), error = function(e) NULL)
          if (is.null(count_file)) {
            tryCatch({
              suppressMessages(invisible(capture.output(getGEOSuppFiles(gse_id, baseDir = dirname(gse_dir), makeDirectory = FALSE, fetch_files = TRUE), file = nullfile())))
              files <- list.files(gse_dir, full.names = TRUE, recursive = TRUE)
              tar_files <- files[grepl("\\.tar$", files, ignore.case = TRUE)]
              for (tf in tar_files) tryCatch(untar(tf, exdir = gse_dir, tar = "internal"), error = function(e) NULL)
              files <- list.files(gse_dir, full.names = TRUE, recursive = TRUE)
              for (pat in c("count", "raw", "matrix")) {
                matches <- files[grepl(pat, basename(files), ignore.case = TRUE)]
                matches <- matches[!grepl("series_matrix", basename(matches), ignore.case = TRUE)]
                for (cand in matches) {
                  tryCatch({
                    df_test <- suppressWarnings(data.table::fread(cand, data.table = FALSE, nrows = 1e6))
                    if (ncol(df_test) >= 2 && nrow(df_test) >= 10) { count_file <- cand; break }
                  }, error = function(e) NULL)
                }
                if (!is.null(count_file)) break
              }
            }, error = function(e) NULL)
          }
          if (is.null(count_file)) { ext_log <- paste0(ext_log, "FAILED\n"); next }
          count_df <- tryCatch(read_count_matrix(count_file), error = function(e) suppressWarnings(data.table::fread(count_file, data.table = FALSE)))
          if (is.null(count_df) || ncol(count_df) < 2) { ext_log <- paste0(ext_log, "FAILED\n"); next }
          gene_ids <- as.character(count_df[[1]])
          count_matrix <- as.matrix(count_df[, -1, drop = FALSE]); mode(count_matrix) <- "numeric"
          rownames(count_matrix) <- gene_ids
          gene_symbols <- suppressMessages(convert_rnaseq_ids(gene_ids, gse_id))
          rownames(count_matrix) <- gene_symbols
          vld <- !is.na(gene_symbols) & trimws(gene_symbols) != ""
          count_matrix <- count_matrix[vld, , drop = FALSE]
          if (any(duplicated(rownames(count_matrix)))) count_matrix <- limma::avereps(count_matrix, ID = rownames(count_matrix))
          all_expr_list[[gse_id]] <- count_matrix
          rna_meta <- tryCatch({
            suppressMessages(invisible(capture.output(gl <- getGEO(gse_id, GSEMatrix = TRUE), file = nullfile())))
            pData(if (is.list(gl)) gl[[1]] else gl)
          }, error = function(e) { sm <- fetch_geo_series_matrix_metadata(gse_id); if (!is.null(sm)) sm else data.frame(title = colnames(count_matrix), row.names = colnames(count_matrix)) })
          all_metadata_list[[gse_id]] <- rna_meta
          ext_log <- paste0(ext_log, "OK (", nrow(count_matrix), "g x ", ncol(count_matrix), "s)\n")
        }

        # ---- Download Microarray ----
        for (gse_id in micro_ids) {
          if (gse_id %in% names(all_expr_list)) next
          incProgress(0.2 / max(1, length(gse_ids)), detail = paste0(gse_id, " (Microarray)"))
          ext_log <- paste0(ext_log, gse_id, " (Microarray)... ")
          micro_data <- tryCatch({
            suppressMessages(invisible(capture.output(md <- getGEO(gse_id, GSEMatrix = TRUE, getGPL = TRUE), file = nullfile()))); md
          }, error = function(e) NULL)
          if (is.null(micro_data)) { ext_log <- paste0(ext_log, "FAILED\n"); next }
          known_platforms <- names(platform_to_annot); micro_eset <- NULL
          if (is.list(micro_data) && length(micro_data) >= 1) {
            plats <- vapply(micro_data, function(x) annotation(x), character(1))
            idx <- which(plats %in% known_platforms)[1]
            if (!is.na(idx)) micro_eset <- micro_data[[idx]]
          } else { if (annotation(micro_data) %in% known_platforms) micro_eset <- micro_data }
          if (is.null(micro_eset)) { ext_log <- paste0(ext_log, "SKIPPED (platform)\n"); next }
          micro_expr <- exprs(micro_eset); fdata <- fData(micro_eset)
          gene_symbols <- suppressMessages(map_microarray_ids(micro_expr, fdata, micro_eset, gse_id))
          rownames(micro_expr) <- gene_symbols
          vld <- !is.na(gene_symbols) & trimws(gene_symbols) != ""
          micro_expr <- micro_expr[vld, , drop = FALSE]
          if (any(duplicated(rownames(micro_expr)))) micro_expr <- limma::avereps(micro_expr, ID = rownames(micro_expr))
          all_expr_list[[gse_id]] <- micro_expr
          all_metadata_list[[gse_id]] <- pData(micro_eset)
          ext_log <- paste0(ext_log, "OK (", nrow(micro_expr), "g x ", ncol(micro_expr), "s)\n")
        }

        if (length(all_expr_list) == 0) {
          showNotification(tags$div(icon("times-circle"), tags$strong(" No datasets downloaded.")), type = "error", duration = 8)
          rv$ext_val_log <- ext_log; return()
        }

        incProgress(0.3, detail = "Combining genes...")

        common_genes_val <- Reduce(intersect, lapply(all_expr_list, rownames))
        for (gse in names(all_expr_list)) all_expr_list[[gse]] <- all_expr_list[[gse]][common_genes_val, , drop = FALSE]
        combined_ext_expr <- do.call(cbind, all_expr_list)

        ext_meta <- tryCatch(
          do.call(rbind, lapply(names(all_metadata_list), function(gse) {
            md <- all_metadata_list[[gse]]
            common_samp <- intersect(colnames(all_expr_list[[gse]]), rownames(md))
            if (length(common_samp) > 0) md <- md[common_samp, , drop = FALSE]
            md$Dataset <- gse; md
          })),
          error = function(e) {
            all_cols <- unique(unlist(lapply(all_metadata_list, colnames)))
            do.call(rbind, lapply(names(all_metadata_list), function(gse) {
              md <- all_metadata_list[[gse]]
              common_samp <- intersect(colnames(all_expr_list[[gse]]), rownames(md))
              if (length(common_samp) > 0) md <- md[common_samp, , drop = FALSE]
              md$Dataset <- gse
              for (cc in setdiff(all_cols, colnames(md))) md[[cc]] <- NA
              md[, union(colnames(md), all_cols), drop = FALSE]
            }))
          }
        )

        ext_log <- paste0(ext_log, "\nCombined: ", nrow(combined_ext_expr), " genes x ", ncol(combined_ext_expr), " samples\n")

        rv$ext_val_raw_expr <- combined_ext_expr
        rv$ext_val_metadata <- ext_meta
        rv$ext_val_log <- ext_log
        rv$ext_val_downloaded <- TRUE

        showNotification(
          tags$div(icon("check-circle"), tags$strong(" Download complete!"),
                   " Now browse phenodata below and select your group column."),
          type = "message", duration = 8)

      }, error = function(e) {
        showNotification(tags$div(icon("times-circle"), tags$strong(" Download failed: "), conditionMessage(e)), type = "error", duration = 10)
        rv$ext_val_log <- paste0("Error: ", conditionMessage(e))
      }, finally = { shinyjs::enable("ext_val_download_btn") })
    })
  })

  # ============================================================================
  # STEP B: Phenodata browser + column selector
  # ============================================================================
  output$ext_val_phenodata_ui <- renderUI({
    if (!isTRUE(rv$ext_val_downloaded) || is.null(rv$ext_val_metadata)) return(NULL)
    meta <- rv$ext_val_metadata
    col_choices <- colnames(meta)
    preselect <- NULL
    for (candidate in c("Condition", "condition", "Group", "group", "disease state:ch1",
                        "disease status:ch1", "diagnosis:ch1", "source_name_ch1")) {
      if (candidate %in% col_choices) {
        vals <- as.character(trimws(meta[[candidate]]))
        u <- unique(vals[!is.na(vals) & vals != ""])
        if (length(u) >= 2 && length(u) <= 5) { preselect <- candidate; break }
      }
    }
    tagList(
      tags$hr(),
      tags$h4(icon("table"), " Step B: Browse Phenodata & Select Group Column", style = "color: #2980b9; margin-bottom: 10px;"),
      tags$p("Examine the phenodata below. Choose the column that contains your group labels (e.g. Normal vs Disease).", style = "font-size: 13px; margin-bottom: 8px;"),
      DT::dataTableOutput("ext_val_phenodata_table"),
      tags$div(style = "margin-top: 12px;",
        fluidRow(
          column(6,
            selectInput("ext_val_group_col", "Select Group Column:",
              choices = col_choices, selected = preselect, width = "100%")
          ),
          column(6,
            uiOutput("ext_val_column_preview_ui")
          )
        )
      )
    )
  })

  output$ext_val_phenodata_table <- DT::renderDataTable({
    req(rv$ext_val_metadata)
    meta <- rv$ext_val_metadata
    if (ncol(meta) > 20) meta <- meta[, seq_len(20), drop = FALSE]
    DT::datatable(meta, options = list(pageLength = 8, scrollX = TRUE, scrollY = "300px"),
                  rownames = TRUE, selection = "none")
  })

  # Column preview with categorization dropdowns
  output$ext_val_column_preview_ui <- renderUI({
    req(rv$ext_val_metadata, input$ext_val_group_col)
    col <- input$ext_val_group_col
    if (!col %in% colnames(rv$ext_val_metadata)) return(NULL)
    vals <- as.character(trimws(rv$ext_val_metadata[[col]]))
    u <- unique(vals[!is.na(vals) & vals != ""])
    n_per <- table(vals[!is.na(vals) & vals != ""])

    auto_cat <- function(v) {
      vl <- tolower(as.character(v))
      if (grepl("normal|control|healthy|wild|non-?tumor|benign|unaffected|ctrl", vl)) return("Normal")
      if (grepl("disease|tumor|cancer|metastatic|patient|affected|malignant|asd|case|treatment", vl)) return("Disease")
      "None"
    }

    tags$div(
      style = "margin-top: 10px; padding: 12px; background: #f0f7ff; border-radius: 6px; border: 1px solid #b3d7ff;",
      tags$p(tags$strong("Column: "), tags$code(col),
             tags$span(paste0(" (", length(u), " unique values)"), style = "color: #6c757d;"),
             style = "margin-bottom: 8px;"),
      tags$p(icon("tags", style = "color: #2980b9; margin-right: 5px;"),
             tags$strong("Categorize each value as Normal, Disease, or None (exclude):"),
             style = "font-size: 13px; margin-bottom: 10px;"),
      lapply(seq_along(u), function(i) {
        v <- u[i]
        input_id <- paste0("ext_val_cat_", i)
        cat_guess <- auto_cat(v)
        cat_color <- switch(cat_guess, Normal = "#2ecc71", Disease = "#e74c3c", "#95a5a6")
        n_samp <- if (v %in% names(n_per)) as.integer(n_per[[v]]) else 0L
        tags$div(
          style = paste0("display: flex; align-items: center; gap: 12px; margin-bottom: 8px; padding: 8px 12px; background: #fff; border-left: 4px solid ", cat_color, "; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.08);"),
          tags$div(
            style = "flex: 1; min-width: 150px;",
            tags$strong(v, style = "font-size: 14px;"),
            tags$span(paste0(" (", n_samp, " samples)"), style = "color: #6c757d; font-size: 12px; margin-left: 6px;")
          ),
          tags$div(
            style = "width: 150px;",
            selectInput(input_id, label = NULL,
              choices = c("Normal" = "Normal", "Disease" = "Disease", "None (exclude)" = "None"),
              selected = cat_guess, width = "100%")
          )
        )
      }),
      tags$p(
        icon("info-circle", style = "color: #17a2b8; margin-right: 4px;"),
        tags$em("Values marked 'None' will be excluded. You need at least one Normal and one Disease."),
        style = "font-size: 12px; color: #6c757d; margin-top: 8px;")
    )
  })

  # ============================================================================
  # STEP C: Run validation button
  # ============================================================================
  output$ext_val_run_ui <- renderUI({
    if (!isTRUE(rv$ext_val_downloaded) || is.null(rv$ext_val_metadata)) return(NULL)
    tagList(
      tags$hr(),
      tags$h4(icon("play-circle"), " Step C: Categorize Groups & Run DE", style = "color: #27ae60; margin-bottom: 10px;"),
      fluidRow(
        column(6,
          actionButton("ext_val_run_btn",
            tagList(icon("dna"), " Apply Groups & Run DE Analysis"),
            class = "btn-success btn-lg", style = "min-width: 280px;")
        ),
        column(6,
          tags$p("This will: use your Normal/Disease categorizations, run limma DE analysis on the validation data, and store results for ROC & Nomogram. Values marked 'None' will be excluded.",
                 style = "font-size: 12px; color: #6c757d; margin-top: 8px;")
        )
      )
    )
  })

  # ============================================================================
  # Run handler: categorize → DE
  # ============================================================================
  observeEvent(input$ext_val_run_btn, {
    req(rv$ext_val_raw_expr, rv$ext_val_metadata, input$ext_val_group_col)
    col <- input$ext_val_group_col
    meta <- rv$ext_val_metadata
    if (!col %in% colnames(meta)) {
      showNotification("Selected column not found.", type = "error", duration = 5); return()
    }

    vals <- as.character(trimws(meta[[col]]))
    vals[vals == ""] <- NA
    u <- unique(vals[!is.na(vals)])

    cat_map <- list()
    for (i in seq_along(u)) {
      input_id <- paste0("ext_val_cat_", i)
      cat_val <- input[[input_id]]
      if (is.null(cat_val)) cat_val <- "None"
      cat_map[[u[i]]] <- cat_val
    }

    normal_vals <- names(cat_map)[cat_map == "Normal"]
    disease_vals <- names(cat_map)[cat_map == "Disease"]
    if (length(normal_vals) == 0) {
      showNotification(tags$div(icon("exclamation-triangle"), tags$strong(" No values categorized as Normal.")), type = "error", duration = 6); return()
    }
    if (length(disease_vals) == 0) {
      showNotification(tags$div(icon("exclamation-triangle"), tags$strong(" No values categorized as Disease.")), type = "error", duration = 6); return()
    }

    outcome <- rep(NA_integer_, length(vals))
    outcome[vals %in% normal_vals] <- 0L
    outcome[vals %in% disease_vals] <- 1L

    valid <- !is.na(outcome)
    ext_expr_t <- t(rv$ext_val_raw_expr)[valid, , drop = FALSE]
    outcome <- outcome[valid]

    if (nrow(ext_expr_t) < 5) {
      showNotification("Too few valid samples after categorization.", type = "error", duration = 6); return()
    }
    if (sum(outcome == 1) < 2 || sum(outcome == 0) < 2) {
      showNotification(paste0("Need at least 2 samples per group. Disease: ", sum(outcome == 1), ", Normal: ", sum(outcome == 0), "."), type = "error", duration = 6); return()
    }

    # Store validation data
    rv$external_validation_expr <- ext_expr_t
    rv$external_validation_outcome <- outcome
    rv$external_validation_group_col <- col
    rv$external_validation_n_disease <- sum(outcome == 1)
    rv$external_validation_n_normal <- sum(outcome == 0)
    rv$external_validation_gene_names <- colnames(ext_expr_t)
    rv$external_validation_raw_expr <- rv$ext_val_raw_expr
    rv$external_validation_metadata <- rv$ext_val_metadata[valid, , drop = FALSE]

    n_excluded <- sum(!valid)

    # ==================================================================
    # Run DE analysis (limma) on validation data
    # ==================================================================
    tryCatch({
      withProgress(message = "Running DE on validation data...", value = 0, {
        val_expr <- rv$ext_val_raw_expr[, valid, drop = FALSE]

        max_val <- max(val_expr, na.rm = TRUE)
        if (max_val > 50) {
          min_val <- min(val_expr, na.rm = TRUE)
          if (min_val < 0) val_expr <- val_expr - min_val + 1
          val_expr <- log2(val_expr + 1)
        }
        val_expr <- limma::normalizeBetweenArrays(val_expr, method = "quantile")

        incProgress(0.3, detail = "Building design matrix...")

        condition <- factor(ifelse(outcome == 0, "Normal", "Disease"),
                            levels = c("Normal", "Disease"))
        design <- model.matrix(~ 0 + condition)
        colnames(design) <- levels(condition)

        contrast <- limma::makeContrasts(Disease - Normal, levels = design)

        incProgress(0.3, detail = "Fitting model...")

        fit <- limma::lmFit(val_expr, design)
        fit2 <- limma::contrasts.fit(fit, contrast)
        fit2 <- limma::eBayes(fit2)

        de_res <- limma::topTable(fit2, number = Inf, adjust.method = "BH")
        de_res$Gene <- rownames(de_res)
        de_res <- de_res[, c("Gene", "logFC", "AveExpr", "P.Value", "adj.P.Val")]

        padj_cut <- 0.05
        logfc_cut <- 0.5
        de_res$Significance <- "Not Significant"
        de_res$Significance[de_res$adj.P.Val < padj_cut & de_res$logFC > logfc_cut] <- "Up-regulated"
        de_res$Significance[de_res$adj.P.Val < padj_cut & de_res$logFC < -logfc_cut] <- "Down-regulated"

        rv$ext_val_de_results <- de_res
        rv$ext_val_sig_genes <- de_res[de_res$Significance != "Not Significant", ]

        incProgress(0.4, detail = "DE complete!")
      })
    }, error = function(e) {
      showNotification(
        tags$div(icon("exclamation-triangle"), tags$strong(" Validation DE failed: "), conditionMessage(e)),
        type = "warning", duration = 8)
      rv$ext_val_de_results <- NULL
      rv$ext_val_sig_genes <- NULL
    })

    n_overlap <- length(intersect(rv$ml_common_genes, colnames(ext_expr_t)))
    n_degs <- if (!is.null(rv$ext_val_sig_genes)) nrow(rv$ext_val_sig_genes) else 0L

    showNotification(
      tags$div(icon("check-circle"), tags$strong(" Validation complete!"),
               tags$br(),
               tags$span(paste0(nrow(ext_expr_t), " samples (Disease: ", sum(outcome == 1),
                                ", Normal: ", sum(outcome == 0),
                                if (n_excluded > 0) paste0(", Excluded: ", n_excluded) else "",
                                "). ML gene overlap: ", n_overlap, "/", length(rv$ml_common_genes))),
               if (n_degs > 0) tags$span(tags$br(), paste0("Validation DE: ", n_degs, " DEGs found."),
                                          style = "color: #27ae60; font-weight: bold;"),
               tags$br(),
               tags$span(paste0("Normal = [", paste(normal_vals, collapse = ", "), "]  |  Disease = [", paste(disease_vals, collapse = ", "), "]"),
                         style = "font-size: 12px; color: #6c757d;")),
      type = "message", duration = 10)
  })

  # ---- Clear all ----
  observeEvent(input$clear_ext_validation, {
    rv$ext_val_raw_expr <- NULL
    rv$ext_val_metadata <- NULL
    rv$ext_val_downloaded <- NULL
    rv$ext_val_log <- NULL
    rv$ext_val_de_results <- NULL
    rv$ext_val_sig_genes <- NULL
    rv$external_validation_expr <- NULL
    rv$external_validation_outcome <- NULL
    rv$external_validation_group_col <- NULL
    rv$external_validation_n_disease <- NULL
    rv$external_validation_n_normal <- NULL
    rv$external_validation_gene_names <- NULL
    rv$external_validation_raw_expr <- NULL
    rv$external_validation_metadata <- NULL
    rv$nomogram_ext_val_data <- NULL
    rv$nomogram_ext_val_metrics <- NULL
    rv$nomogram_ext_val_roc <- NULL
    showNotification("External validation data cleared.", type = "message", duration = 3)
  })

  # Status UI
  output$ext_val_status_ui <- renderUI({
    if (is.null(rv$external_validation_expr)) return(NULL)
    n_overlap <- length(intersect(rv$ml_common_genes, rv$external_validation_gene_names))
    tags$div(
      class = "alert alert-success", style = "margin-top: 10px;",
      icon("check-circle"),
      tags$strong(" External validation ready: "),
      paste0(nrow(rv$external_validation_expr), " samples (",
             rv$external_validation_n_disease, " Disease / ",
             rv$external_validation_n_normal, " Normal). "),
      tags$span(paste0("ML gene overlap: ", n_overlap, "/", length(rv$ml_common_genes), "."),
                style = "color: #1e8449; font-weight: bold;")
    )
  })

  # Log output
  output$ext_val_log_ui <- renderUI({
    if (is.null(rv$ext_val_log) || !nzchar(rv$ext_val_log)) return(NULL)
    tags$div(
      style = "margin-top: 10px;",
      tags$p(tags$strong("Download Log:"), style = "font-size: 13px; margin-bottom: 4px;"),
      tags$pre(rv$ext_val_log, style = "max-height: 180px; overflow-y: auto; font-size: 12px; background: #f8f9fa; border: 1px solid #dee2e6; padding: 10px; border-radius: 4px;")
    )
  })

  # ---- Validation status panel ----
  output$val_status_ui <- renderUI({
    mode <- input$validation_mode
    if (is.null(mode)) return(NULL)
    if (mode == "internal") {
      tags$div(
        class = "alert alert-info", style = "margin-top: 10px;",
        icon("check-circle"),
        tags$strong(" Internal Validation selected. "),
        "Proceed to ROC and Nomogram. The Nomogram will use 70/30 split-sample validation."
      )
    } else if (mode == "external" && !is.null(rv$external_validation_expr)) {
      tags$div(
        class = "alert alert-success", style = "margin-top: 10px;",
        icon("check-circle"),
        tags$strong(" External validation data loaded and ready. "),
        paste0(nrow(rv$external_validation_expr), " samples. "),
        "Continue to ROC to see AUC comparison, then Nomogram for external validation."
      )
    } else {
      NULL
    }
  })

  # ============================================================================
  # VALIDATION DE RESULTS PANEL (volcano, table, gene overlap)
  # ============================================================================
  output$val_de_panel_ui <- renderUI({
    req(rv$ext_val_de_results)
    de <- rv$ext_val_de_results
    sig <- rv$ext_val_sig_genes
    n_total <- nrow(de)
    n_sig <- if (!is.null(sig)) nrow(sig) else 0
    n_up <- sum(de$Significance == "Up-regulated", na.rm = TRUE)
    n_down <- sum(de$Significance == "Down-regulated", na.rm = TRUE)

    tagList(
      fluidRow(
        box(
          title = tags$span(icon("dna"), " Validation DE Results",
                            tags$span("LIMMA", class = "label label-info",
                                      style = "margin-left: 8px; font-size: 11px;")),
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,

          fluidRow(
            column(4,
              tags$div(
                style = "text-align: center; padding: 15px; background: linear-gradient(135deg, #f39c12, #e67e22); border-radius: 10px; color: white; margin-bottom: 10px;",
                tags$h4(icon("star"), tags$strong("Total DEGs"), style = "margin: 0;"),
                tags$h2(n_sig, style = "margin: 5px 0;"),
                tags$small(paste0("of ", format(n_total, big.mark = ","), " genes tested"))
              )
            ),
            column(4,
              tags$div(
                style = "text-align: center; padding: 15px; background: linear-gradient(135deg, #e74c3c, #c0392b); border-radius: 10px; color: white; margin-bottom: 10px;",
                tags$h4(icon("arrow-up"), tags$strong("Up-regulated"), style = "margin: 0;"),
                tags$h2(n_up, style = "margin: 5px 0;"),
                tags$small("adj.P < 0.05, logFC > 0.5")
              )
            ),
            column(4,
              tags$div(
                style = "text-align: center; padding: 15px; background: linear-gradient(135deg, #3498db, #2980b9); border-radius: 10px; color: white; margin-bottom: 10px;",
                tags$h4(icon("arrow-down"), tags$strong("Down-regulated"), style = "margin: 0;"),
                tags$h2(n_down, style = "margin: 5px 0;"),
                tags$small("adj.P < 0.05, logFC < -0.5")
              )
            )
          ),

          fluidRow(
            column(6,
              tags$p(tags$strong("Volcano Plot -- Validation Dataset"), style = "margin-bottom: 6px;"),
              plotOutput("val_volcano_plot", height = "450px"),
              tags$div(style = "margin-top: 8px;",
                downloadButton("download_val_volcano_jpg", tagList(icon("download"), " JPG"), class = "btn-info btn-sm", style = "margin-right: 6px;"),
                downloadButton("download_val_volcano_pdf", tagList(icon("download"), " PDF"), class = "btn-info btn-sm"))
            ),
            column(6,
              tags$p(tags$strong("Top Significant DEGs -- Validation"), style = "margin-bottom: 6px;"),
              DT::dataTableOutput("val_de_table"),
              tags$div(style = "margin-top: 8px;",
                downloadButton("download_val_de_csv", tagList(icon("download"), " Full DE Results (CSV)"), class = "btn-info btn-sm"))
            )
          ),

          uiOutput("val_gene_overlap_ui")
        )
      )
    )
  })

  # Volcano plot
  output$val_volcano_plot <- renderPlot({
    req(rv$ext_val_de_results)
    tryCatch({
      vd <- rv$ext_val_de_results
      vd$Significance <- factor(vd$Significance, levels = c("Not Significant", "Down-regulated", "Up-regulated"))
      min_padj <- min(vd$adj.P.Val[vd$adj.P.Val > 0], na.rm = TRUE)
      if (is.infinite(min_padj) || is.na(min_padj)) min_padj <- 1e-300
      vd$adj.P.Val[vd$adj.P.Val == 0] <- min_padj
      vd$neg_log10_padj <- -log10(vd$adj.P.Val)
      max_finite <- max(vd$neg_log10_padj[is.finite(vd$neg_log10_padj)], na.rm = TRUE)
      if (is.finite(max_finite)) vd$neg_log10_padj[!is.finite(vd$neg_log10_padj)] <- max_finite + 1
      vd <- vd[is.finite(vd$logFC) & is.finite(vd$neg_log10_padj), ]

      vd$Label <- ""
      top_genes_to_label <- rbind(head(vd[order(vd$adj.P.Val), ], 10), head(vd[order(-abs(vd$logFC)), ], 10))
      ml_genes <- if (!is.null(rv$ml_common_genes)) intersect(rv$ml_common_genes, vd$Gene) else character(0)
      vd$Label[vd$Gene %in% c(top_genes_to_label$Gene, ml_genes)] <- vd$Gene[vd$Gene %in% c(top_genes_to_label$Gene, ml_genes)]
      vd$IsMLGene <- vd$Gene %in% ml_genes

      n_up <- sum(vd$Significance == "Up-regulated", na.rm = TRUE)
      n_down <- sum(vd$Significance == "Down-regulated", na.rm = TRUE)

      p <- ggplot2::ggplot(vd, ggplot2::aes(x = logFC, y = neg_log10_padj, color = Significance)) +
        ggplot2::geom_point(alpha = 0.5, size = 1.8) +
        ggplot2::geom_point(data = vd[vd$IsMLGene, , drop = FALSE],
                            ggplot2::aes(x = logFC, y = neg_log10_padj),
                            color = "#8E24AA", size = 3.5, shape = 17, alpha = 0.9) +
        ggplot2::scale_color_manual(
          values = c("Up-regulated" = "#e74c3c", "Down-regulated" = "#3498db", "Not Significant" = "gray70")) +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::labs(
          title = "Validation Volcano Plot",
          subtitle = paste0("DEGs: Up=", n_up, ", Down=", n_down,
                            if (length(ml_genes) > 0) paste0(" | ML genes (purple triangles): ", length(ml_genes)) else ""),
          x = "Log2 Fold Change", y = "-Log10 Adjusted P-value") +
        ggplot2::geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "gray40", alpha = 0.7) +
        ggplot2::geom_vline(xintercept = c(-0.5, 0.5), linetype = "dashed", color = "gray40", alpha = 0.7) +
        ggrepel::geom_text_repel(ggplot2::aes(label = Label), size = 3, max.overlaps = 20,
                                  box.padding = 0.5, segment.color = "gray50") +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 15),
                       legend.position = "right")
      print(p)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Volcano plot error:", conditionMessage(e)), cex = 0.9, col = "gray40")
    })
  }, height = 450, res = 96)

  # DE table
  output$val_de_table <- DT::renderDataTable({
    req(rv$ext_val_sig_genes)
    sig <- rv$ext_val_sig_genes
    top <- head(sig[order(sig$adj.P.Val), c("Gene", "logFC", "adj.P.Val", "Significance")], 30)
    top$logFC <- round(top$logFC, 4)
    top$adj.P.Val <- signif(top$adj.P.Val, 4)
    DT::datatable(top, options = list(pageLength = 15, dom = 't', scrollX = TRUE), rownames = FALSE)
  })

  # Gene overlap panel
  output$val_gene_overlap_ui <- renderUI({
    req(rv$ext_val_de_results, rv$ml_common_genes)
    de <- rv$ext_val_de_results
    sig <- rv$ext_val_sig_genes
    ml_genes <- rv$ml_common_genes
    val_sig_genes <- if (!is.null(sig)) sig$Gene else character(0)
    val_all_genes <- de$Gene

    overlap_sig <- intersect(ml_genes, val_sig_genes)
    overlap_all <- intersect(ml_genes, val_all_genes)
    not_found <- setdiff(ml_genes, val_all_genes)

    tags$div(
      style = "margin-top: 20px; padding: 15px; background: #f0f7ff; border-radius: 8px; border: 1px solid #b3d7ff;",
      tags$h5(icon("exchange-alt", style = "color: #8E24AA;"), tags$strong(" ML Test Genes in Validation DE"),
              style = "color: #2c3e50; margin-bottom: 10px;"),
      fluidRow(
        column(3,
          tags$div(
            style = "text-align: center; padding: 12px; background: #fff; border-radius: 8px; border: 2px solid #8E24AA;",
            tags$h4(length(ml_genes), style = "color: #8E24AA; margin: 0;"),
            tags$small("ML Test Genes")
          )
        ),
        column(3,
          tags$div(
            style = "text-align: center; padding: 12px; background: #fff; border-radius: 8px; border: 2px solid #27ae60;",
            tags$h4(length(overlap_all), style = "color: #27ae60; margin: 0;"),
            tags$small("Found in Validation")
          )
        ),
        column(3,
          tags$div(
            style = "text-align: center; padding: 12px; background: #fff; border-radius: 8px; border: 2px solid #e74c3c;",
            tags$h4(length(overlap_sig), style = "color: #e74c3c; margin: 0;"),
            tags$small("Also DE in Validation")
          )
        ),
        column(3,
          tags$div(
            style = "text-align: center; padding: 12px; background: #fff; border-radius: 8px; border: 2px solid #95a5a6;",
            tags$h4(length(not_found), style = "color: #95a5a6; margin: 0;"),
            tags$small("Not in Validation")
          )
        )
      ),
      if (length(overlap_sig) > 0) {
        tags$div(
          style = "margin-top: 12px;",
          tags$p(tags$strong("ML genes also significant in validation DE:"), style = "font-size: 13px; margin-bottom: 6px;"),
          DT::dataTableOutput("val_overlap_de_table")
        )
      } else {
        tags$p(tags$em("No ML test genes are significantly DE in the validation dataset."),
               style = "color: #6c757d; margin-top: 8px; font-size: 13px;")
      }
    )
  })

  output$val_overlap_de_table <- DT::renderDataTable({
    req(rv$ext_val_sig_genes, rv$ml_common_genes)
    sig <- rv$ext_val_sig_genes
    ml_genes <- rv$ml_common_genes
    overlap <- sig[sig$Gene %in% ml_genes, c("Gene", "logFC", "adj.P.Val", "Significance"), drop = FALSE]
    if (nrow(overlap) == 0) return(NULL)
    overlap$logFC <- round(overlap$logFC, 4)
    overlap$adj.P.Val <- signif(overlap$adj.P.Val, 4)
    DT::datatable(overlap, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  })

  # ---- Download handlers ----
  make_volcano_plot <- function() {
    vd <- rv$ext_val_de_results
    vd$Significance <- factor(vd$Significance, levels = c("Not Significant", "Down-regulated", "Up-regulated"))
    min_padj <- min(vd$adj.P.Val[vd$adj.P.Val > 0], na.rm = TRUE)
    if (is.infinite(min_padj) || is.na(min_padj)) min_padj <- 1e-300
    vd$adj.P.Val[vd$adj.P.Val == 0] <- min_padj
    vd$neg_log10_padj <- -log10(vd$adj.P.Val)
    max_finite <- max(vd$neg_log10_padj[is.finite(vd$neg_log10_padj)], na.rm = TRUE)
    if (is.finite(max_finite)) vd$neg_log10_padj[!is.finite(vd$neg_log10_padj)] <- max_finite + 1
    vd <- vd[is.finite(vd$logFC) & is.finite(vd$neg_log10_padj), ]
    vd$Label <- ""
    top_genes_to_label <- rbind(head(vd[order(vd$adj.P.Val), ], 10), head(vd[order(-abs(vd$logFC)), ], 10))
    ml_genes <- if (!is.null(rv$ml_common_genes)) intersect(rv$ml_common_genes, vd$Gene) else character(0)
    vd$Label[vd$Gene %in% c(top_genes_to_label$Gene, ml_genes)] <- vd$Gene[vd$Gene %in% c(top_genes_to_label$Gene, ml_genes)]
    vd$IsMLGene <- vd$Gene %in% ml_genes
    ggplot2::ggplot(vd, ggplot2::aes(x = logFC, y = neg_log10_padj, color = Significance)) +
      ggplot2::geom_point(alpha = 0.5, size = 1.8) +
      ggplot2::geom_point(data = vd[vd$IsMLGene, , drop = FALSE], ggplot2::aes(x = logFC, y = neg_log10_padj),
                          color = "#8E24AA", size = 3.5, shape = 17, alpha = 0.9) +
      ggplot2::scale_color_manual(values = c("Up-regulated" = "#e74c3c", "Down-regulated" = "#3498db", "Not Significant" = "gray70")) +
      ggplot2::theme_bw(base_size = 13) +
      ggplot2::labs(title = "Validation Volcano Plot", x = "Log2 Fold Change", y = "-Log10 Adjusted P-value") +
      ggplot2::geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "gray40") +
      ggplot2::geom_vline(xintercept = c(-0.5, 0.5), linetype = "dashed", color = "gray40") +
      ggrepel::geom_text_repel(ggplot2::aes(label = Label), size = 3, max.overlaps = 20) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }

  output$download_val_volcano_jpg <- downloadHandler(
    filename = function() "Validation_Volcano_Plot.jpg",
    content = function(file) {
      req(rv$ext_val_de_results)
      p <- make_volcano_plot()
      ggplot2::ggsave(file, plot = p, width = 8, height = 6, dpi = IMAGE_DPI, units = "in", bg = "white", device = "jpeg")
    }
  )

  output$download_val_volcano_pdf <- downloadHandler(
    filename = function() "Validation_Volcano_Plot.pdf",
    content = function(file) {
      req(rv$ext_val_de_results)
      p <- make_volcano_plot()
      ggplot2::ggsave(file, plot = p, width = 8, height = 6, device = "pdf", bg = "white")
    }
  )

  output$download_val_de_csv <- downloadHandler(
    filename = function() "Validation_DE_Results.csv",
    content = function(file) {
      req(rv$ext_val_de_results)
      write.csv(rv$ext_val_de_results, file, row.names = FALSE)
      write.csv(rv$ext_val_de_results, file.path(CSV_EXPORT_DIR(), "Validation_DE_Results.csv"), row.names = FALSE)
    }
  )

  # ---- Navigation ----
  observeEvent(input$next_page_validation_to_roc, {
    updateTabItems(session, "sidebar_menu", "roc")
  })
}
