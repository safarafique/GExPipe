# ==============================================================================
# SERVER_DOWNLOAD.R - Step 1: Data Download (Multi-Platform Pipeline)
# ==============================================================================
# Matches: PHASE 1 - STEP 1 (Download) + STEP 2 (Gene ID Mapping) + common genes
# Pipeline: Microarray getGEO → store raw; RNA-seq NCBI raw counts / supp → store raw;
#           then map to symbols (remove NA, avereps); then common genes & combined matrix
# ==============================================================================

# Helper: fetch phenotype columns from GEO series matrix when getGEO fails (all !sample_* lines)
# Uses explicit connection cleanup to avoid "all connections in use" when network fails.
fetch_geo_series_matrix_metadata <- function(gse_id) {
  conn <- NULL
  tryCatch({
    url_str <- sprintf("https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/matrix/%s_series_matrix.txt.gz",
                       substr(gse_id, 1, 3), gse_id, gse_id)
    conn <- url(url_str, open = "rb")
    raw_lines <- readLines(conn, warn = FALSE, encoding = "UTF-8")
    if (length(raw_lines) == 0) return(NULL)
    idx <- grep("^!sample_", raw_lines, ignore.case = TRUE)
    if (length(idx) == 0) return(NULL)
    lines <- strsplit(raw_lines[idx], "\t", fixed = TRUE)
    attr_names <- vapply(lines, function(x) sub("^!sample_", "", x[1], ignore.case = TRUE), character(1))
    n_samples <- max(vapply(lines, length, integer(1))) - 1L
    if (n_samples < 1) return(NULL)
    sample_ids <- lines[[1]][-1]
    sample_ids <- head(sample_ids, n_samples)
    out <- as.data.frame(matrix(NA_character_, nrow = length(sample_ids), ncol = length(attr_names)),
                         stringsAsFactors = FALSE)
    colnames(out) <- make.names(attr_names, unique = TRUE)
    rownames(out) <- sample_ids
    for (j in seq_along(lines)) {
      vals <- lines[[j]][-1]
      n <- min(length(vals), nrow(out))
      if (n > 0) out[seq_len(n), j] <- vals[seq_len(n)]
    }
    out
  }, error = function(e) NULL,
  finally = {
    if (!is.null(conn)) try(close(conn), silent = TRUE)
  })
}

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

        # Parse GSE IDs (pipeline: micro_gse_ids, rna_gse_ids)
        rnaseq_ids <- c()
        micro_ids <- c()
        if (input$analysis_type %in% c("rnaseq", "merged")) {
          rnaseq_text <- gsub("\\s+", ",", input$rnaseq_gses)
          rnaseq_ids <- trimws(unlist(strsplit(rnaseq_text, ",")))
          rnaseq_ids <- rnaseq_ids[nzchar(rnaseq_ids)]
        }
        if (input$analysis_type %in% c("microarray", "merged")) {
          micro_text <- gsub("\\s+", ",", input$microarray_gses)
          micro_ids <- trimws(unlist(strsplit(micro_text, ",")))
          micro_ids <- micro_ids[nzchar(micro_ids)]
        }

        # Single-dataset mode: keep only the first ID (skip batch correction later)
        rv$dataset_mode <- if (is.null(input$dataset_mode) || !nzchar(input$dataset_mode)) "multi" else input$dataset_mode
        if (identical(rv$dataset_mode, "single")) {
          if (length(rnaseq_ids) > 1) {
            showNotification("Single dataset mode: using only the first RNA-seq GSE ID.", type = "warning", duration = 6)
            rnaseq_ids <- rnaseq_ids[1]
          }
          if (length(micro_ids) > 1) {
            showNotification("Single dataset mode: using only the first microarray GSE ID.", type = "warning", duration = 6)
            micro_ids <- micro_ids[1]
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
      if (length(micro_ids) > 0) {
        micro_dir <- file.path(getwd(), "micro_data")
        if (dir.exists(micro_dir)) {
          tryCatch({ unlink(micro_dir, recursive = TRUE, force = TRUE) }, error = function(e) NULL)
          log_text <- paste0(log_text, "Cleared previous microarray cache (micro_data).\n")
        }
        dir.create(micro_dir, showWarnings = FALSE, recursive = TRUE)
      }
      if (length(rnaseq_ids) > 0) {
        rna_dir <- file.path(getwd(), "rna_data")
        if (dir.exists(rna_dir)) {
          tryCatch({ unlink(rna_dir, recursive = TRUE, force = TRUE) }, error = function(e) NULL)
          log_text <- paste0(log_text, "Cleared previous RNA-seq cache (rna_data).\n")
        }
        dir.create(rna_dir, showWarnings = FALSE, recursive = TRUE)
      }

      # --------------------------------------------------------------------------
      # STEP 1: AUTOMATED DATA DOWNLOAD (pipeline: store raw, no mapping yet)
      # --------------------------------------------------------------------------

      # --- Download Microarray (pipeline: getGEO, exprs, pData, store; require platform in platform_to_annot) ---
      missing_platform_gses <- character(0)
      skip_fail_reasons <- list()
      if (length(micro_ids) > 0) {
        micro_dir <- file.path(getwd(), "micro_data")
        dir.create(micro_dir, showWarnings = FALSE, recursive = TRUE)
        if (is.null(rv$micro_cel_paths)) rv$micro_cel_paths <- list()
        log_text <- paste0(log_text, "Downloading Microarray Datasets...\n")
        known_platforms <- names(platform_to_annot)
        for (i in seq_along(micro_ids)) {
          incProgress(1 / (length(rnaseq_ids) + length(micro_ids)))
          gse_id <- micro_ids[i]
          log_text <- paste0(log_text, "[", i, "/", length(micro_ids), "] ", gse_id, "... ")

          micro_data <- tryCatch({
            suppressMessages(invisible(capture.output(
              md <- GEOquery::getGEO(gse_id, GSEMatrix = TRUE, getGPL = TRUE),
              file = nullfile()
            )))
            md
          }, error = function(e) structure(list(error = conditionMessage(e)), class = "geo_error"))

          if (inherits(micro_data, "geo_error")) {
            err_msg <- micro_data$error
            if (grepl("connection|timeout|hostname|resolve|HTTP|ssl|could not resolve|Unable to", err_msg, ignore.case = TRUE)) {
              reason <- "network/HTTP - check internet connection"
            } else {
              reason <- substr(gsub("\n", " ", err_msg), 1L, 80L)
            }
            log_text <- paste0(log_text, "FAILED (", reason, "). Skipped.\n")
            skip_fail_reasons[[gse_id]] <- paste0("Microarray: ", reason)
            next
          }

          # When GSE has multiple platforms, use first that matches platform_to_annot (global.R)
          if (is.list(micro_data) && length(micro_data) >= 1) {
            platforms <- vapply(micro_data, function(x) Biobase::annotation(x), character(1))
            idx <- which(platforms %in% known_platforms)[1]
            if (!is.na(idx)) {
              micro_eset <- micro_data[[idx]]
              platform_id <- Biobase::annotation(micro_eset)
              plat_type <- if (!is.null(platform_id_to_type) && platform_id %in% names(platform_id_to_type)) platform_id_to_type[[platform_id]] else "supported"
              log_text <- paste0(log_text, "Platform ", platform_id, " - ", plat_type, ". ")
            } else {
              # No known platform for this GSE: record and skip
              platform_id <- platforms[1]
              missing_platform_gses <- c(missing_platform_gses, gse_id)
              reason <- paste0("platform ID missing (", platform_id, " not in app list)")
              log_text <- paste0(log_text, "SKIPPED: ", reason, ".\n")
              skip_fail_reasons[[gse_id]] <- paste0("Microarray: ", reason)
              next
            }
          } else {
            micro_eset <- micro_data
            platform_id <- Biobase::annotation(micro_eset)
            if (!(platform_id %in% known_platforms)) {
              missing_platform_gses <- c(missing_platform_gses, gse_id)
              reason <- paste0("platform ID missing (", platform_id, " not in app list)")
              log_text <- paste0(log_text, "SKIPPED: ", reason, ".\n")
              skip_fail_reasons[[gse_id]] <- paste0("Microarray: ", reason)
              next
            }
            plat_type <- if (!is.null(platform_id_to_type) && platform_id %in% names(platform_id_to_type)) platform_id_to_type[[platform_id]] else "supported"
            log_text <- paste0(log_text, "Platform ", platform_id, " - ", plat_type, ". ")
          }
          micro_expr <- Biobase::exprs(micro_eset)
          pdata <- Biobase::pData(micro_eset)

          rv$micro_expr_list[[gse_id]] <- micro_expr
          rv$micro_metadata_list[[gse_id]] <- pdata
          rv$micro_eset_list[[gse_id]] <- micro_eset
          if (is.null(rv$platform_per_gse)) rv$platform_per_gse <- list()
          rv$platform_per_gse[[gse_id]] <- platform_id

          log_text <- paste0(log_text, "Downloaded: ", nrow(micro_expr), " genes x ", ncol(micro_expr), " samples")
          # Optional: try to fetch CEL files from GEO supplementary for RMA normalization
          tryCatch({
            suppressMessages(invisible(capture.output(
              GEOquery::getGEOSuppFiles(gse_id, baseDir = micro_dir, makeDirectory = TRUE, fetch_files = TRUE),
              file = nullfile()
            )))
            supp_dir <- file.path(micro_dir, gse_id)
            if (!dir.exists(supp_dir)) supp_dir <- micro_dir
            files <- list.files(supp_dir, full.names = TRUE, recursive = TRUE)
            tar_files <- files[grepl("\\.tar$|\\.zip$", files, ignore.case = TRUE)]
            for (tf in tar_files) {
              tryCatch({
                if (grepl("\\.zip$", tf, ignore.case = TRUE)) utils::unzip(tf, exdir = supp_dir) else untar(tf, exdir = supp_dir, tar = "internal")
              }, error = function(e) NULL)
            }
            files <- list.files(supp_dir, full.names = TRUE, recursive = TRUE)
            cel <- files[grepl("\\.cel$", files, ignore.case = TRUE)]
            if (length(cel) > 0) {
              rv$micro_cel_paths[[gse_id]] <- cel
              log_text <<- paste0(log_text, ". CEL: ", length(cel), " files (RMA available)\n")
            } else {
              log_text <<- paste0(log_text, "\n")
            }
          }, error = function(e) { log_text <<- paste0(log_text, "\n") })
        }
        # Show error notification if any microarray GSEs had missing platform
        if (length(missing_platform_gses) > 0) {
          showNotification(
            tags$div(
              icon("exclamation-circle"),
              tags$strong("Platform ID missing"),
              tags$p(paste(missing_platform_gses, collapse = ", "), style = "margin-top: 6px;"),
              tags$p("These GSE(s) were skipped. Add the platform in global.R or remove them.", style = "margin-top: 4px; font-size: 12px;")
            ),
            type = "error",
            duration = 10
          )
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

          gse_dir <- file.path(rna_dir, gse_id)
          dir.create(gse_dir, showWarnings = FALSE)

          supp_err <- NULL
          count_file <- NULL
          tryCatch({
              suppressMessages(invisible(capture.output(
                GEOquery::getGEOSuppFiles(gse_id, baseDir = dirname(gse_dir), makeDirectory = FALSE, fetch_files = TRUE),
                file = nullfile()
              )))
              files <- list.files(gse_dir, full.names = TRUE, recursive = TRUE)
              if (length(files) == 0) {
                files <- list.files(rna_dir, full.names = TRUE, recursive = TRUE)
                files <- files[grepl(gse_id, basename(files), ignore.case = TRUE)]
              }

              # 1) Prefer explicit bulk RNA-seq raw count text files in supplements (e.g. *_bulkRNAseq_raw_counts.txt.gz)
              supp_files <- files[!grepl("\\.tar$", files, ignore.case = TRUE)]
              bulk_candidates <- supp_files[
                grepl("bulk", basename(supp_files), ignore.case = TRUE) &
                  grepl("count", basename(supp_files), ignore.case = TRUE)
              ]
              if (length(bulk_candidates) > 0) {
                best_nrow <- 0L
                for (cand in bulk_candidates) {
                  tryCatch({
                    df <- suppressWarnings(data.table::fread(cand, data.table = FALSE, nrows = 1e6))
                    # Heuristic: at least 2 columns and at least 10 rows; choose the file with the most rows
                    if (ncol(df) >= 2 && nrow(df) >= 10 && nrow(df) > best_nrow) {
                      best_nrow <- nrow(df)
                      count_file <- cand
                    }
                  }, error = function(e) NULL)
                }
              }

              # 2) If no bulk-specific file found, fall back to searching inside any TAR archives and other count-like files
              if (is.null(count_file)) {
                tar_files <- files[grepl("\\.tar$", files, ignore.case = TRUE)]
                for (tar_file in tar_files) {
                  tryCatch({
                    # Use R's internal untar to avoid Windows tar.exe issues with truncated/corrupted archives
                    untar(tar_file, exdir = gse_dir, tar = "internal")
                  }, error = function(e) {
                    msg <- conditionMessage(e)
                    if (grepl("truncated|corrupt|error|invalid", msg, ignore.case = TRUE)) {
                      supp_err <<- paste0("Truncated or corrupted tar archive (", basename(tar_file), "). Re-download or try another GSE.")
                    } else {
                      supp_err <<- paste0("Untar failed: ", substr(msg, 1L, 120L))
                    }
                  }, warning = function(w) {
                    # tar often warns on truncated archive; treat as failure so we skip this GSE
                    supp_err <<- paste0("Tar archive problem (", basename(tar_file), "). File may be truncated or corrupted.")
                  })
                }
                files <- list.files(gse_dir, full.names = TRUE, recursive = TRUE)
                if (length(files) == 0) files <- list.files(rna_dir, full.names = TRUE)
                for (pattern in c("count", "raw", "matrix")) {
                  matches <- files[grepl(pattern, basename(files), ignore.case = TRUE)]
                  matches <- matches[!grepl("series_matrix", basename(matches), ignore.case = TRUE)]
                  if (length(matches) > 0) {
                    best_nrow <- 0L
                    for (cand in matches) {
                      tryCatch({
                        df <- suppressWarnings(data.table::fread(cand, data.table = FALSE, nrows = 1e6))
                        # Relaxed threshold: allow typical gene counts (~20k) instead of requiring 30k+
                        if (ncol(df) >= 2 && nrow(df) >= 10 && nrow(df) > best_nrow) {
                          best_nrow <- nrow(df)
                          count_file <- cand
                        }
                      }, error = function(e) NULL)
                    }
                    if (!is.null(count_file)) break
                    break
                  }
                }
              }
            }, error = function(e) { supp_err <<- conditionMessage(e); NULL })

          # Try NCBI (all genome versions) and keep the file with the most rows; then use best of GEO supp vs NCBI
          ncbi_best <- tryCatch(download_ncbi_raw_counts_best(gse_id, gse_dir), error = function(e) NULL)
          nrow_supp <- 0L
          if (!is.null(count_file)) {
            nrow_supp <- tryCatch({
              df <- suppressWarnings(data.table::fread(count_file, data.table = FALSE, nrows = 500000L))
              nrow(df)
            }, error = function(e) 0L)
          }
          nrow_ncbi <- 0L
          if (!is.null(ncbi_best)) {
            nrow_ncbi <- tryCatch({
              df <- suppressWarnings(data.table::fread(ncbi_best, data.table = FALSE, nrows = 500000L))
              nrow(df)
            }, error = function(e) 0L)
          }
          if (nrow_ncbi > nrow_supp && !is.null(ncbi_best)) {
            count_file <- ncbi_best
            log_text <- paste0(log_text, "(NCBI ", nrow_ncbi, " rows) ")
          } else if (!is.null(count_file)) {
            log_text <- paste0(log_text, "(GEO supp ", nrow_supp, " rows) ")
          }
          if (is.null(count_file) && !is.null(ncbi_best)) count_file <- ncbi_best
          if (is.null(count_file)) {
            reason <- "no count file (check internet or GSE may not have GEO supp or NCBI counts)"
            if (!is.null(supp_err) && nzchar(supp_err)) {
              if (grepl("connection|timeout|hostname|resolve|HTTP|ssl", supp_err, ignore.case = TRUE)) {
                reason <- "network/HTTP - check internet connection"
              } else if (grepl("truncated|corrupt|tar archive", supp_err, ignore.case = TRUE)) {
                reason <- "truncated/corrupted supplementary tar - try re-download or remove this GSE"
              } else {
                reason <- supp_err
              }
            }
            log_text <- paste0(log_text, " FAILED (", reason, "). Skipped.\n")
            skip_fail_reasons[[gse_id]] <- paste0("RNA-seq: ", reason)
            next
          }

          count_df <- tryCatch(read_count_matrix(count_file), error = function(e) {
            suppressWarnings(data.table::fread(count_file, data.table = FALSE))
          })
          if (is.null(count_df) || ncol(count_df) < 2 || nrow(count_df) < 10) {
            reason <- "count file format invalid or too small"
            log_text <- paste0(log_text, " FAILED (", reason, "). Skipped.\n")
            skip_fail_reasons[[gse_id]] <- paste0("RNA-seq: ", reason)
            next
          }

          gene_ids <- as.character(count_df[[1]])
          count_matrix <- as.matrix(count_df[, -1, drop = FALSE])
          mode(count_matrix) <- "numeric"
          rownames(count_matrix) <- gene_ids

          # General GSE phenodata extraction: getGEO + pData so all GSE phenodata columns appear
          rna_metadata <- tryCatch({
            suppressMessages(invisible(capture.output(
              gse_list <- GEOquery::getGEO(gse_id, GSEMatrix = TRUE),
              file = nullfile()
            )))
            # If multiple platforms, select the first one
            gse <- if (inherits(gse_list, "list") && length(gse_list) > 1) gse_list[[1]] else if (inherits(gse_list, "list")) gse_list[[1]] else gse_list
            # Extract phenotype/sample metadata (all columns from GSE)
            pheno <- Biobase::pData(gse)
            if (is.null(pheno) || nrow(pheno) == 0) stop("empty pData")
            pheno
          }, error = function(e) {
            # Fallback 1: try GEO series matrix to get ALL !sample_* columns
            sm <- fetch_geo_series_matrix_metadata(gse_id)
            count_cols <- colnames(count_matrix)
            if (!is.null(sm) && nrow(sm) > 0 && ncol(sm) > 0) {
              # Align to count_matrix samples (rows); fill missing with NA
              out <- as.data.frame(matrix(NA_character_, nrow = length(count_cols), ncol = ncol(sm)),
                                    stringsAsFactors = FALSE)
              colnames(out) <- colnames(sm)
              rownames(out) <- count_cols
              for (sid in count_cols) {
                if (sid %in% rownames(sm)) out[sid, ] <- sm[sid, , drop = TRUE]
              }
              return(out)
            }
            # Fallback 2: minimal metadata so group step still works
            data.frame(title = colnames(count_matrix), row.names = colnames(count_matrix), stringsAsFactors = FALSE)
          })

          # Keep phenotype rows only for samples that have bulk RNA-seq counts
          if (!is.null(rna_metadata) && nrow(rna_metadata) > 0) {
            common_samples <- intersect(colnames(count_matrix), rownames(rna_metadata))
            if (length(common_samples) > 0) {
              rna_metadata <- rna_metadata[common_samples, , drop = FALSE]
            }
          }

          gene_symbols <- suppressMessages(convert_rnaseq_ids(gene_ids, gse_id))
          rownames(count_matrix) <- gene_symbols
          valid <- !is.na(gene_symbols) & trimws(gene_symbols) != ""
          count_matrix <- count_matrix[valid, , drop = FALSE]
          if (nrow(count_matrix) == 0) {
            reason <- "no genes after ID mapping"
            log_text <- paste0(log_text, " FAILED (", reason, "). Skipped.\n")
            skip_fail_reasons[[gse_id]] <- paste0("RNA-seq: ", reason)
            next
          }
          if (any(duplicated(rownames(count_matrix)))) {
            count_matrix <- limma::avereps(count_matrix, ID = rownames(count_matrix))
          }

          rv$rna_counts_list[[gse_id]] <- count_matrix
          rv$rna_metadata_list[[gse_id]] <- rna_metadata
          rv$all_genes_list[[gse_id]] <- rownames(count_matrix)
          log_text <- paste0(log_text, " OK (", nrow(count_matrix), " genes)\n")
        }
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
            micro_eset <- if (!is.null(micro_data) && is.list(micro_data)) micro_data[[1]] else micro_data
          }
          if (is.null(micro_eset)) next
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
      log_text <- paste0(log_text, "\nSTEP 2b: Normalize IDs to gene symbols for overlap...\n")
      for (gse in names(rv$micro_expr_list)) {
        micro_expr <- rv$micro_expr_list[[gse]]
        rn <- rownames(micro_expr)
        sample_rn <- head(rn[!is.na(rn) & rn != ""], min(200, length(rn)))
        looks_like_symbol <- length(sample_rn) > 0 && mean(grepl("^[A-Za-z]", sample_rn), na.rm = TRUE) > 0.6 && mean(grepl("^ENSG|_at$|_st$|^[0-9]+$", sample_rn), na.rm = TRUE) < 0.5
        gpl <- if (!is.null(rv$platform_per_gse)) rv$platform_per_gse[[gse]] else NULL
        if (!looks_like_symbol) {
          fmt <- detect_gene_id_format(rn)
          log_text <- paste0(log_text, "  ", gse, ": format ", fmt, " -> converting to symbols...\n")
          sym <- any_id_to_symbol(rn, gpl_id = gpl)
          valid <- !is.na(sym) & trimws(sym) != ""
          if (sum(valid) > 0) {
            rownames(micro_expr) <- sym
            micro_expr <- micro_expr[valid, , drop = FALSE]
            if (any(duplicated(rownames(micro_expr)))) micro_expr <- limma::avereps(micro_expr, ID = rownames(micro_expr))
            rv$micro_expr_list[[gse]] <- micro_expr
            rv$all_genes_list[[gse]] <- rownames(micro_expr)
            log_text <- paste0(log_text, "  ", gse, ": converted to ", nrow(micro_expr), " gene symbols\n")
          }
        }
      }
      for (gse in names(rv$rna_counts_list)) {
        cnt <- rv$rna_counts_list[[gse]]
        rn <- rownames(cnt)
        sample_rn <- head(rn[!is.na(rn) & rn != ""], min(200, length(rn)))
        looks_like_symbol <- length(sample_rn) > 0 && mean(grepl("^[A-Za-z]", sample_rn), na.rm = TRUE) > 0.6 && mean(grepl("^ENSG|_at$|_st$|^[0-9]+$", sample_rn), na.rm = TRUE) < 0.5
        if (!looks_like_symbol) {
          sym <- any_id_to_symbol(rn, gpl_id = NULL)
          valid <- !is.na(sym) & trimws(sym) != ""
          if (sum(valid) > 0) {
            rownames(cnt) <- sym
            cnt <- cnt[valid, , drop = FALSE]
            if (any(duplicated(rownames(cnt)))) cnt <- limma::avereps(cnt, ID = rownames(cnt))
            rv$rna_counts_list[[gse]] <- cnt
            rv$all_genes_list[[gse]] <- rownames(cnt)
            log_text <- paste0(log_text, "  ", gse, ": converted to ", nrow(cnt), " gene symbols\n")
          }
        }
      }

      # Force Entrez -> symbol for RNA-seq datasets still with numeric row IDs (e.g. org.Hs.eg.db failed; biomaRt fallback)
      for (gse in names(rv$rna_counts_list)) {
        rn <- rownames(rv$rna_counts_list[[gse]])
        sample_rn <- head(rn[!is.na(rn) & nzchar(trimws(rn))], min(300, length(rn)))
        if (length(sample_rn) > 0 && mean(grepl("^[0-9]+$", sample_rn), na.rm = TRUE) > 0.7) {
          log_text <- paste0(log_text, "  ", gse, ": row IDs still Entrez-like -> trying biomaRt Entrez->symbol...\n")
          sym <- entrez_to_symbol_biomart(rn)
          if (!is.null(sym)) {
            valid <- !is.na(sym) & trimws(sym) != ""
            if (sum(valid) > 0) {
              cnt <- rv$rna_counts_list[[gse]]
              rownames(cnt) <- sym
              cnt <- cnt[valid, , drop = FALSE]
              if (any(duplicated(rownames(cnt)))) cnt <- limma::avereps(cnt, ID = rownames(cnt))
              rv$rna_counts_list[[gse]] <- cnt
              rv$all_genes_list[[gse]] <- rownames(cnt)
              log_text <- paste0(log_text, "  ", gse, ": biomaRt converted to ", nrow(cnt), " gene symbols\n")
            }
          }
        }
      }

      # Force convert Affymetrix HuGene _st probe IDs (e.g. 2824546_st) to gene symbols if still present
      for (gse in names(rv$all_genes_list)) {
        rn <- rv$all_genes_list[[gse]]
        sample_rn <- head(rn[!is.na(rn) & nzchar(trimws(rn))], min(500, length(rn)))
        if (length(sample_rn) > 0 && mean(grepl("^[0-9]+_st$", sample_rn), na.rm = TRUE) > 0.5) {
          log_text <- paste0(log_text, "  ", gse, ": detected Affymetrix HuGene probe (_st) format -> converting...\n")
          gpl <- if (!is.null(rv$platform_per_gse)) rv$platform_per_gse[[gse]] else NULL
          sym <- probe_ids_to_symbol_hugene_db(rn, gpl)
          if (is.null(sym) || sum(!is.na(sym)) <= length(rn) * 0.1)
            sym <- probe_ids_to_symbol_gpl(rn, gpl)
          if (is.null(sym) || sum(!is.na(sym)) <= length(rn) * 0.1)
            sym <- probe_ids_to_symbol_biomart(rn, gpl)
          if (!is.null(sym) && sum(!is.na(sym)) > length(rn) * 0.1) {
            valid <- !is.na(sym) & trimws(sym) != ""
            if (gse %in% names(rv$micro_expr_list)) {
              micro_expr <- rv$micro_expr_list[[gse]]
              rownames(micro_expr) <- sym
              micro_expr <- micro_expr[valid, , drop = FALSE]
              if (any(duplicated(rownames(micro_expr)))) micro_expr <- limma::avereps(micro_expr, ID = rownames(micro_expr))
              rv$micro_expr_list[[gse]] <- micro_expr
            } else if (gse %in% names(rv$rna_counts_list)) {
              cnt <- rv$rna_counts_list[[gse]]
              rownames(cnt) <- sym
              cnt <- cnt[valid, , drop = FALSE]
              if (any(duplicated(rownames(cnt)))) cnt <- limma::avereps(cnt, ID = rownames(cnt))
              rv$rna_counts_list[[gse]] <- cnt
            }
            rv$all_genes_list[[gse]] <- if (gse %in% names(rv$micro_expr_list)) rownames(rv$micro_expr_list[[gse]]) else rownames(rv$rna_counts_list[[gse]])
            n_after <- if (gse %in% names(rv$micro_expr_list)) nrow(rv$micro_expr_list[[gse]]) else nrow(rv$rna_counts_list[[gse]])
            log_text <- paste0(log_text, "  ", gse, ": _st probe IDs converted to ", n_after, " gene symbols (HuGene/GEO GPL/biomaRt)\n")
          }
        }
      }

      # Log gene symbols extracted from each GSE (format detected + conversion method)
      log_text <- paste0(log_text, "\nGene symbols extracted per GSE (format detected + sample):\n")
      for (gse in names(rv$all_genes_list)) {
        rn <- rv$all_genes_list[[gse]]
        n <- length(rn)
        sample_rn <- head(rn[!is.na(rn) & nzchar(trimws(rn))], 10)
        sample_str <- if (length(sample_rn) > 0) paste(sQuote(sample_rn), collapse = ", ") else "(none)"
        fmt <- detect_gene_id_format(rn)
        log_text <- paste0(log_text, "  ", gse, ": ", n, " genes; format: ", fmt, "; sample: ", sample_str, "\n")
      }

      # --------------------------------------------------------------------------
      # COMMON GENES & COMBINED MATRIX (pipeline: intersection, subset, cbind)
      # --------------------------------------------------------------------------

      if (length(rv$all_genes_list) > 0) {
        rv$common_genes <- Reduce(intersect, rv$all_genes_list)

        for (gse in names(rv$micro_expr_list)) {
          keep <- intersect(rv$common_genes, rownames(rv$micro_expr_list[[gse]]))
          rv$micro_expr_list[[gse]] <- rv$micro_expr_list[[gse]][keep, , drop = FALSE]
        }
        for (gse in names(rv$rna_counts_list)) {
          keep <- intersect(rv$common_genes, rownames(rv$rna_counts_list[[gse]]))
          rv$rna_counts_list[[gse]] <- rv$rna_counts_list[[gse]][keep, , drop = FALSE]
        }
        rv$common_genes <- Reduce(intersect, c(
          if (length(rv$micro_expr_list) > 0) lapply(rv$micro_expr_list, rownames) else list(),
          if (length(rv$rna_counts_list) > 0) lapply(rv$rna_counts_list, rownames) else list()
        ))

          if (length(rv$common_genes) == 0) {
            log_text <- paste0(log_text, "\nNo common genes across datasets after converting to gene symbols.\n")
            log_text <- paste0(log_text, "Diagnostic: sample row IDs per dataset (to see if mapping to symbols failed):\n")
            for (gse in names(rv$all_genes_list)) {
              rn <- rv$all_genes_list[[gse]]
              sample_rn <- head(rn[!is.na(rn) & nzchar(trimws(rn))], 5)
              log_text <- paste0(log_text, "  ", gse, " (", length(rn), " rows): ", paste(sQuote(sample_rn), collapse = ", "), "\n")
            }
            log_text <- paste0(log_text, "\n--- Gene ID conversion (probe / Entrez -> symbols) ---\n")
            log_text <- paste0(log_text, "If you see probe IDs (2824546_st, 200000_s_at) or numeric IDs (Entrez) instead of symbols (e.g. BRCA1):\n")
            log_text <- paste0(log_text, "  1. Check internet (biomaRt needs Ensembl); install biomaRt: BiocManager::install(\"biomaRt\")\n")
            log_text <- paste0(log_text, "  2. RNA-seq with Entrez row IDs is converted via org.Hs.eg.db then biomaRt fallback\n")
            log_text <- paste0(log_text, "  3. Microarray: GEO GPL annotation is used when biomaRt is offline\n")
            log_text <- paste0(log_text, "  4. For testing merged RNA+microarray, use GSEs that map to symbols (e.g. GSE62646)\n")
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
          for (gse in names(rv$micro_expr_list)) {
            rv$micro_expr_list[[gse]] <- rv$micro_expr_list[[gse]][rv$common_genes, , drop = FALSE]
          }
          for (gse in names(rv$rna_counts_list)) {
            rv$rna_counts_list[[gse]] <- rv$rna_counts_list[[gse]][rv$common_genes, , drop = FALSE]
          }
          rv$combined_expr_raw <- do.call(cbind, c(rv$micro_expr_list, rv$rna_counts_list))
          rownames(rv$combined_expr_raw) <- rv$common_genes

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
