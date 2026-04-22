# ==============================================================================
# SERVER_DOWNLOAD.R - Step 1: Data Download (Multi-Platform Pipeline)
# ==============================================================================
# Matches: PHASE 1 - STEP 1 (Download) + STEP 2 (Gene ID Mapping) + common genes
# Pipeline: Microarray getGEO → store raw; RNA-seq NCBI raw counts / supp → store raw;
#           then map to symbols (remove NA, avereps); then common genes & combined matrix
# ==============================================================================

server_download <- function(input, output, session, rv) {

  output$download_timer <- renderText({
    if (!isTRUE(rv$download_running) || is.null(rv$download_start)) return("00:00")
    invalidateLater(1000, session)
    elapsed <- as.integer(difftime(Sys.time(), rv$download_start, units = "secs"))
    sprintf("%02d:%02d", elapsed %/% 60, elapsed %% 60)
  })

  output$download_process_summary_ui <- renderUI({
    expr <- rv$combined_expr_raw
    if (is.null(expr) || nrow(expr) == 0 || ncol(expr) == 0) {
      return(tags$p(style = "color: #6c757d; margin: 0;", icon("info-circle"), " Run download to see process summary (datasets, samples, genes)."))
    }
    n_genes <- nrow(expr)
    n_samples <- ncol(expr)
    n_rna <- length(rv$rna_counts_list)
    n_micro <- length(rv$micro_expr_list)
    tags$div(
      style = "font-size: 14px; line-height: 1.6; color: #333;",
      tags$p(tags$strong("Step 1 complete."), " Combined matrix: ", format(n_genes, big.mark = ","), " genes \u00d7 ", format(n_samples, big.mark = ","), " samples."),
      tags$p("RNA-seq datasets: ", n_rna, ". Microarray datasets: ", n_micro, ". Common genes (intersection) retained. See log below for details."))
  })

  observeEvent(input$start_processing, {
    shinyjs::disable("start_processing")
    shinyjs::html("start_processing",
                  HTML('<i class="fa fa-spinner fa-spin"></i> Processing...'))

    rv$download_start <- Sys.time()
    rv$download_running <- TRUE
    log_text <- "Starting download...\n"

    showNotification(
      tags$div(
        tags$div(class = "status-indicator processing"),
        tags$strong("Processing started..."),
        tags$br(),
        tags$span("Downloading and processing datasets. Please wait..."),
        style = "font-size: 13px;"
      ),
      type = "message",
      duration = NULL,
      id = "download_processing"
    )

    tryCatch({
      withProgress(message = "Downloading data...", value = 0, {

        parsed <- gexp_parse_gse_inputs(
          analysis_type = input$analysis_type,
          rnaseq_gses = input$rnaseq_gses,
          microarray_gses = input$microarray_gses,
          dataset_mode = input$dataset_mode
        )
        rnaseq_ids <- parsed$rnaseq_ids
        micro_ids <- parsed$micro_ids
        rv$dataset_mode <- parsed$dataset_mode
        if (length(parsed$messages) > 0) {
          for (msg in parsed$messages) {
            showNotification(msg, type = "warning", duration = 6)
          }
        }

      log_text <- paste0("Starting download...\n")
      disease <- trimws(if (is.null(input$disease_name)) "" else input$disease_name)
      if (nzchar(disease)) {
        log_text <- paste0(log_text, "Disease/Condition: ", disease, "\n")
        rv$disease_name <- disease
      }
      log_text <- paste0(log_text, "Mode: ", if (identical(rv$dataset_mode, "single")) "Single dataset" else "Multiple datasets", "\n")
      log_text <- paste0(log_text, "RNA-seq: ", length(rnaseq_ids), " | Microarray: ", length(micro_ids), "\n\n")

      if (length(rnaseq_ids) == 0 && length(micro_ids) == 0) {
        log_text <- paste0(log_text, "Please specify at least one GSE ID in RNA-seq or Microarray.\n")
        rv$download_running <- FALSE
        output$download_log <- renderText({ log_text })
        shinyjs::enable("start_processing")
        shinyjs::html("start_processing", HTML('<i class="fa fa-play-circle"></i> Start Processing'))
        removeNotification("download_processing")
        return()
      }

      # --------------------------------------------------------------------------
      # MANAGE STORED FILES: clear download dirs at start of each run so multiple
      # runs don't accumulate old GSE data. Only current run's bulk/microarray
      # files are kept.
      # --------------------------------------------------------------------------
      prep_logs <- gexp_prepare_download_dirs(
        base_dir = getwd(),
        has_micro = length(micro_ids) > 0,
        has_rna = length(rnaseq_ids) > 0
      )
      if (length(prep_logs) > 0) {
        log_text <- paste0(log_text, paste0(prep_logs, collapse = "\n"), "\n")
      }

      # Clear stale data from any previous run so we start completely fresh.
      # Without this, a successful prior run leaves old matrices in rv; when a
      # later run fails (e.g. no count file), STEP 2 re-processes the old data
      # with newly computed (mismatched-length) gene symbol vectors, causing
      # "length of 'dimnames' [1] not equal to array extent".
      rv$rna_counts_list     <- list()
      rv$micro_expr_list     <- list()
      rv$rna_metadata_list   <- list()
      rv$micro_metadata_list <- list()
      rv$micro_eset_list     <- list()
      rv$all_genes_list      <- list()
      rv$platform_per_gse    <- list()
      rv$common_genes        <- character(0)
      rv$combined_expr_raw   <- NULL
      rv$download_complete   <- FALSE
      rv$download_complete_at <- NULL

      # --------------------------------------------------------------------------
      # STEP 1: AUTOMATED DATA DOWNLOAD (pipeline: store raw, no mapping yet)
      # --------------------------------------------------------------------------

      # --- Download Microarray (pipeline: getGEO, exprs, pData, store; platform auto-detected) ---
      skip_fail_reasons <- list()
      if (length(micro_ids) > 0) {
        micro_dir <- file.path(getwd(), "micro_data")
        dir.create(micro_dir, showWarnings = FALSE, recursive = TRUE)
        if (is.null(rv$micro_cel_paths)) rv$micro_cel_paths <- list()
        log_text <- paste0(log_text, "Downloading Microarray Datasets...\n")
        for (i in seq_along(micro_ids)) {
          incProgress(1 / (length(rnaseq_ids) + length(micro_ids)))
          gse_id <- micro_ids[i]
          log_text <- paste0(log_text, "[", i, "/", length(micro_ids), "] ", gse_id, "... ")
          res <- gexp_download_one_microarray_gse(gse_id, micro_dir)
          if (!isTRUE(res$ok)) {
            log_text <- paste0(log_text, "FAILED (", res$reason, "). Skipped.\n")
            skip_fail_reasons[[gse_id]] <- paste0("Microarray: ", res$reason)
            next
          }
          rv$micro_expr_list[[gse_id]] <- res$micro_expr
          rv$micro_metadata_list[[gse_id]] <- res$metadata
          rv$micro_eset_list[[gse_id]] <- res$micro_eset
          if (is.null(rv$platform_per_gse)) rv$platform_per_gse <- list()
          rv$platform_per_gse[[gse_id]] <- res$platform_id
          if (length(res$cel_paths) > 0) rv$micro_cel_paths[[gse_id]] <- res$cel_paths
          log_text <- paste0(log_text, res$log, "\n")
        }
      }

      # --- Download RNA-seq: try GEO supplementary first (full gene set), then NCBI raw counts ---
      # GEO supp often has full matrix (~39k rows -> ~37k symbols); NCBI raw can be filtered (~24k).
      if (length(rnaseq_ids) > 0) {
        log_text <- paste0(log_text, "\nDownloading RNA-seq Datasets...\n")
        rna_dir <- file.path(getwd(), "rna_data")
        if (!dir.exists(rna_dir)) dir.create(rna_dir, showWarnings = FALSE, recursive = TRUE)
        for (i in seq_along(rnaseq_ids)) {
          incProgress(1 / (length(rnaseq_ids) + length(micro_ids)))
          gse_id <- rnaseq_ids[i]
          log_text <- paste0(log_text, "[", i, "/", length(rnaseq_ids), "] ", gse_id, "... ")
          res <- gexp_download_one_rnaseq_gse(gse_id, rna_dir)
          if (!isTRUE(res$ok)) {
            log_text <- paste0(log_text, " FAILED (", res$reason, "). Skipped.\n")
            skip_fail_reasons[[gse_id]] <- paste0("RNA-seq: ", res$reason)
            next
          }
          rv$rna_counts_list[[gse_id]] <- res$count_matrix
          rv$rna_metadata_list[[gse_id]] <- res$metadata
          rv$all_genes_list[[gse_id]] <- rownames(res$count_matrix)
          log_text <- paste0(log_text, res$log, " OK (", nrow(res$count_matrix), " genes)\n")
        }
      }

      # --------------------------------------------------------------------------
      # Guard: if every download failed there is nothing to process — stop here
      # with a clear message rather than crashing inside the gene ID mapping code.
      # --------------------------------------------------------------------------
      if (length(rv$rna_counts_list) == 0 && length(rv$micro_expr_list) == 0) {
        log_text <- paste0(
          log_text,
          "\nAll downloads failed — no data to process.\n",
          "Check the per-GSE reasons above, fix the GSE ID(s) or internet connection, and try again.\n"
        )
        rv$download_running <- FALSE
        output$download_log <- renderText({ log_text })
        showNotification(
          tags$div(
            icon("exclamation-triangle"),
            tags$strong("All downloads failed"),
            tags$p(
              "No data was retrieved. See the log above for the reason per GSE.",
              style = "font-size: 12px; margin-top: 6px;"
            )
          ),
          type = "error", duration = 12
        )
        stop("__gexpipe_all_downloads_failed__")
      }

      # --------------------------------------------------------------------------
      # STEP 2: GENE IDENTIFIER MAPPING & STANDARDIZATION (pipeline: map, remove NA, avereps)
      # --------------------------------------------------------------------------

      log_text <- paste0(log_text, "\nSTEP 2: Gene ID mapping...\n")

      if (length(rv$micro_expr_list) > 0) {
        for (gse_id in names(rv$micro_expr_list)) {
          micro_expr <- rv$micro_expr_list[[gse_id]]
          micro_eset <- rv$micro_eset_list[[gse_id]]
          if (is.null(micro_eset)) {
            micro_data <- tryCatch({
              suppressMessages(invisible(capture.output(
                md <- GEOquery::getGEO(gse_id, GSEMatrix = TRUE, getGPL = TRUE),
                file = nullfile()
              )))
              md
            }, error = function(e) NULL)
            micro_eset <- if (!is.null(micro_data) && is.list(micro_data) && length(micro_data) >= 1) {
              micro_data[[1]]
            } else {
              micro_data
            }
          }
          if (is.null(micro_eset)) {
            log_text <- paste0(log_text, "  ", gse_id, ": skipped mapping (GEO object unavailable during remap)\n")
            next
          }
          fdata <- Biobase::fData(micro_eset)
          gene_symbols <- suppressMessages(map_microarray_ids(micro_expr, fdata, micro_eset, gse_id))
          rownames(micro_expr) <- gene_symbols
          valid <- !is.na(gene_symbols) & trimws(gene_symbols) != ""
          micro_expr <- micro_expr[valid, , drop = FALSE]
          if (nrow(micro_expr) == 0) next
          if (any(duplicated(rownames(micro_expr)))) {
            micro_expr <- limma::avereps(micro_expr, ID = rownames(micro_expr))
          }
          rv$micro_expr_list[[gse_id]] <- micro_expr
          log_text <- paste0(log_text, "  ", gse_id, ": ", nrow(micro_expr), " unique gene symbols\n")
        }
      }

      # Build all_genes_list from mapped rownames (RNA-seq already set in download loop; add microarray)
      rv$all_genes_list <- list()
      for (gse in names(rv$micro_expr_list)) {
        rv$all_genes_list[[gse]] <- rownames(rv$micro_expr_list[[gse]])
      }
      for (gse in names(rv$rna_counts_list)) {
        rv$all_genes_list[[gse]] <- rownames(rv$rna_counts_list[[gse]])
      }

      # Dataset count for downstream steps (e.g. skip batch correction when only 1 dataset)
      rv$dataset_count <- length(rv$all_genes_list)
      rv$single_dataset <- isTRUE(rv$dataset_count == 1)

      # --------------------------------------------------------------------------
      # NORMALIZE TO GENE SYMBOLS so overlap is by symbol (probe/Entrez/Ensembl -> symbol)
      # --------------------------------------------------------------------------
      normalized <- gexp_download_normalize_ids_for_overlap(
        micro_expr_list = rv$micro_expr_list,
        rna_counts_list = rv$rna_counts_list,
        platform_per_gse = rv$platform_per_gse,
        all_genes_list = rv$all_genes_list
      )
      rv$micro_expr_list <- normalized$micro_expr_list
      rv$rna_counts_list <- normalized$rna_counts_list
      rv$all_genes_list <- normalized$all_genes_list
      log_text <- paste0(log_text, normalized$log_text)

      # --------------------------------------------------------------------------
      # COMMON GENES & COMBINED MATRIX (pipeline: intersection, subset, cbind)
      # --------------------------------------------------------------------------

      if (length(rv$all_genes_list) > 0) {
        finalized <- gexp_download_finalize_common_genes(
          micro_expr_list = rv$micro_expr_list,
          rna_counts_list = rv$rna_counts_list,
          all_genes_list = rv$all_genes_list
        )
        rv$micro_expr_list <- finalized$micro_expr_list
        rv$rna_counts_list <- finalized$rna_counts_list
        rv$common_genes <- finalized$common_genes

        if (!isTRUE(finalized$ok) || length(rv$common_genes) == 0) {
            log_text <- paste0(log_text, gexp_no_common_genes_diagnostic_log(rv$all_genes_list))
            showNotification(
              tags$div(
                icon("exclamation-triangle"),
                tags$strong("0 common genes — ID conversion may have failed"),
                tags$p("Ensure both datasets use gene symbols. RNA-seq Entrez IDs are converted via org.Hs.eg.db and biomaRt.", style = "margin-top: 8px; font-size: 12px;"),
                tags$ul(
                  style = "margin: 4px 0 0 0; padding-left: 18px; font-size: 12px; color: #333;",
                  tags$li("Install biomaRt for Entrez->symbol fallback: BiocManager::install(\"biomaRt\")"),
                  tags$li("Check internet (biomaRt needs Ensembl access)"),
                  tags$li("See download log for sample row IDs per dataset")
                ),
                tags$p("See download log for details.", style = "margin-top: 6px; font-size: 11px; opacity: 0.9;")
              ),
              type = "warning",
              duration = 15
            )
        } else {
          rv$combined_expr_raw <- finalized$combined_expr_raw

          n_genes <- nrow(rv$combined_expr_raw)
          n_samples <- ncol(rv$combined_expr_raw)
          log_text <- paste0(log_text, "\nCommon genes (rows): ", n_genes, "\n")
          log_text <- paste0(log_text, "Total samples (columns): ", n_samples, "\n")
          log_text <- paste0(log_text, "Combined matrix: ", n_genes, " genes x ", n_samples, " samples\n")
          if (length(rv$common_genes) < 1000) {
            log_text <- paste0(log_text, "Few common genes - check ID mapping if needed.\n")
          }
          log_text <- paste0(log_text, "Proceed to QC tab.\n")

          rv$download_complete <- TRUE
          if (is.null(rv$download_complete_at)) rv$download_complete_at <- Sys.time()
        }
      }

      # --- Skip/Fail summary: show actual reason per GSE (no generic message) ---
      if (length(skip_fail_reasons) > 0) {
        log_text <- paste0(log_text, "\n--- Skip/Fail summary (reasons so you can fix or remove GSEs) ---\n")
        for (gse in names(skip_fail_reasons)) {
          log_text <- paste0(log_text, "  ", gse, ": ", skip_fail_reasons[[gse]], "\n")
        }
        # Build one line per GSE with its actual reason (no generic text)
        reason_lines <- vapply(names(skip_fail_reasons), function(gse) {
          paste0(gse, ": ", skip_fail_reasons[[gse]])
        }, character(1))
        showNotification(
          tags$div(
            icon("info-circle"),
            tags$strong("Some GSE(s) skipped or failed"),
            tags$p("Actual reasons:", style = "margin-top: 6px; font-size: 12px; font-weight: bold;"),
            tags$ul(
              style = "margin: 6px 0 0 0; padding-left: 18px; font-size: 12px; color: #333;",
              lapply(reason_lines, function(line) tags$li(line))
            )
          ),
          type = "warning",
          duration = 12
        )
      }

      rv$download_running <- FALSE
      output$download_log <- renderText({ log_text })
    })
    }, error = function(e) {
      closeAllConnections()
      msg <- conditionMessage(e)
      # Sentinel thrown by the "all downloads failed" guard — notification was
      # already shown; do not overwrite the log or show a second error popup.
      if (identical(msg, "__gexpipe_all_downloads_failed__")) return()
      err_log <- paste0(log_text, "\n\nError: ", msg, "\nConnections were reset. Please check your network and try again.")
      output$download_log <- renderText({ err_log })
      rv$download_running <- FALSE
      if (grepl("connection|timeout|hostname|resolve", msg, ignore.case = TRUE)) {
        showNotification("Download failed: network or connection limit. Connections were reset. Please try again in a moment.", type = "error", duration = 12)
      } else {
        showNotification(paste("Download failed:", msg), type = "error", duration = 10)
      }
    }, finally = {
      closeAllConnections()
      shinyjs::enable("start_processing")
      shinyjs::html("start_processing", HTML('<i class="fa fa-play-circle"></i> Start Processing'))
      removeNotification("download_processing")
      shinyjs::runjs("
        $('#next_button_container').slideDown(300);
        $('.box[data-widget=\"collapse\"]').first().removeClass('collapsed-box');
        $('.box[data-widget=\"collapse\"]').first().find('.box-body').show();
      ")
    })
  })
}
