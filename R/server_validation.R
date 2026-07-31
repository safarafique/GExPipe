# ==============================================================================
# SERVER_VALIDATION.R - Step 11: Validation Setup (External / Internal)
# ==============================================================================
# Lets user choose External (GEO dataset) or Internal (70/30 split) validation.
# External: download -> phenodata -> categorize -> DE on validation data.
# Stores rv$validation_mode and all external validation data in rv$.
# ==============================================================================

server_validation <- function(input, output, session, rv) {

  # ---- Observe: store validation mode into rv ----
  observeEvent(input$validation_mode, {
    rv$validation_mode <- input$validation_mode
  })

  # ---- Mode description panel ----
  output$validation_process_summary_ui <- renderUI({
    mode <- input$validation_mode
    if (is.null(mode)) mode <- "external"
    if (mode == "external" && (is.null(rv$external_validation_expr) || nrow(rv$external_validation_expr) == 0)) {
      return(tags$p(style = "color: #6c757d; margin: 0;", icon("info-circle"), " Complete validation setup (download GSE, select groups, run DE) to see process summary."))
    }
    if (mode == "internal") {
      return(tags$p(style = "font-size: 14px; color: #333; margin: 0;", tags$strong("Step 11 (Internal)."), " 70/30 split will be used for nomogram. Proceed to ROC/Nomogram."))
    }
    n_samp <- ncol(rv$external_validation_expr)
    n_genes <- nrow(rv$external_validation_expr)
    tags$div(
      style = "font-size: 14px; line-height: 1.6; color: #333;",
      tags$p(tags$strong("Step 11 complete (External)."), " Validation data: ", format(n_genes, big.mark = ","), " genes \u00d7 ", n_samp, " samples. DE and ROC/Nomogram use this cohort."))
  })

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
          " 1) Enter any GEO GSE ID(s) & select platform/DE method  2) Download ",
          "(full phenodata columns are fetched for every GSE, same as Step 1)  ",
          "3) Browse phenodata & select the Normal/Disease column  4) Categorize & Run DE.",
          style = "font-size: 13px; color: #495057; margin-bottom: 14px; padding: 10px 12px; background: #fef9e7; border-left: 4px solid #f39c12; border-radius: 4px;"
        ),

        # Step A: GSE IDs, platform, DE method
        tags$h4(icon("step-forward"), " Step A: Configure & Download", style = "color: #e67e22; margin-bottom: 10px;"),
        fluidRow(
          column(
            4,
            textAreaInput(
              "ext_val_gse_ids",
              label = tags$span(
                "Validation GSE IDs ",
                tags$span(
                  "(any GEO series — one or more, comma/space separated)",
                  style = "font-weight: normal; color: #999; font-size: 12px;"
                )
              ),
              value = "",
              rows = 2,
              placeholder = "GSE114007, GSE50760"
            )
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
  # Build one phenodata table for the browser: union columns across GSEs,
  # expand characteristics_*, and pin rows to expression sample order.
  .gexpipe_ext_val_combine_metadata <- function(all_expr_list, all_metadata_list) {
    meta_parts <- lapply(names(all_expr_list), function(gse) {
      md <- all_metadata_list[[gse]]
      ids <- colnames(all_expr_list[[gse]])
      if (is.null(md) || !is.data.frame(md) || ncol(md) == 0L || nrow(md) == 0L) {
        md <- data.frame(title = ids, row.names = ids, stringsAsFactors = FALSE)
      }
      md <- as.data.frame(md, stringsAsFactors = FALSE, check.names = FALSE)
      # AnnotatedDataFrame list-columns break DT / rbind; flatten to character
      md[] <- lapply(md, function(x) {
        if (is.factor(x)) {
          as.character(x)
        } else if (is.list(x)) {
          vapply(x, function(v) paste(as.character(unlist(v)), collapse = "; "), character(1))
        } else {
          x
        }
      })
      md <- tryCatch(gexp_expand_geo_characteristics(md), error = function(e) md)
      # Keep matrix sample IDs even when GEO rownames differ
      stub <- data.frame(title = ids, row.names = ids, stringsAsFactors = FALSE)
      md <- .gexpipe_align_pdata_to_primary(stub, md)
      keep <- intersect(ids, rownames(md))
      if (length(keep) > 0L) md <- md[keep, , drop = FALSE]
      md$Dataset <- gse
      md
    })
    all_meta_cols <- unique(unlist(lapply(meta_parts, colnames)))
    meta_parts <- lapply(meta_parts, function(md) {
      for (cc in setdiff(all_meta_cols, colnames(md))) md[[cc]] <- NA_character_
      md[, all_meta_cols, drop = FALSE]
    })
    do.call(rbind, meta_parts)
  }

  # Always pull full GEO phenotype columns for EVERY GSE the user entered
  # (same path as Step 1 / Groups). Never leave a series on title-only stubs.
  .gexpipe_ext_val_enrich_metadata <- function(all_expr_list, all_metadata_list, log_cb = NULL) {
    for (gse in names(all_expr_list)) {
      ids <- colnames(all_expr_list[[gse]])
      primary <- if (!is.null(all_metadata_list[[gse]]) &&
                     is.data.frame(all_metadata_list[[gse]]) &&
                     nrow(all_metadata_list[[gse]]) > 0L) {
        as.data.frame(all_metadata_list[[gse]], stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        data.frame(title = ids, row.names = ids, stringsAsFactors = FALSE)
      }
      # Drop our own Dataset label before judging / merging GEO columns
      if ("Dataset" %in% colnames(primary)) {
        primary$Dataset <- NULL
      }

      n_before <- ncol(primary)
      enriched <- tryCatch(
        gexp_fetch_full_pdata(gse, sample_ids = ids),
        error = function(e) NULL
      )
      if (is.null(enriched) || !is.data.frame(enriched) || ncol(enriched) <= 1L) {
        enriched <- tryCatch(
          gexp_enrich_pdata_columns(primary, gse, force = TRUE),
          error = function(e) NULL
        )
      }
      if (!is.null(enriched) && is.data.frame(enriched) && ncol(enriched) > 0L) {
        stub <- data.frame(title = ids, row.names = ids, stringsAsFactors = FALSE)
        # Prefer the richer table, keep matrix sample IDs as rownames
        if (ncol(enriched) >= n_before) {
          md <- .gexpipe_align_pdata_to_primary(stub, enriched)
        } else {
          md <- .gexpipe_merge_pdata_columns(
            .gexpipe_align_pdata_to_primary(stub, primary),
            enriched
          )
        }
        md <- tryCatch(gexp_expand_geo_characteristics(md), error = function(e) md)
        all_metadata_list[[gse]] <- md
      } else {
        md <- tryCatch(gexp_expand_geo_characteristics(primary), error = function(e) primary)
        all_metadata_list[[gse]] <- md
      }

      n_after <- ncol(all_metadata_list[[gse]])
      if (is.function(log_cb)) {
        log_cb(sprintf(
          "  %s phenodata: %d \u2192 %d columns%s\n",
          gse, n_before, n_after,
          if (n_after <= 2L) " (WARNING: GEO returned few columns — check network)" else ""
        ))
      }
    }
    all_metadata_list
  }

  # Accept any mix of spaces / commas / newlines; normalize to GSE######
  .gexpipe_parse_gse_ids <- function(text) {
    if (is.null(text) || length(text) == 0L) return(character(0))
    text <- paste(as.character(text), collapse = " ")
    parts <- unlist(strsplit(text, "[,;\\s]+", perl = TRUE), use.names = FALSE)
    parts <- toupper(trimws(parts))
    parts <- parts[nzchar(parts)]
    # Allow bare digits -> GSE prefix
    parts <- ifelse(grepl("^[0-9]+$", parts), paste0("GSE", parts), parts)
    parts <- gsub("^GSE0+", "GSE", parts)
    unique(parts[grepl("^GSE[0-9]+$", parts)])
  }

  observeEvent(input$ext_val_download_btn, {
    gse_ids <- .gexpipe_parse_gse_ids(input$ext_val_gse_ids)
    if (length(gse_ids) == 0) {
      showNotification(
        tags$div(
          icon("exclamation-triangle"),
          tags$strong(" Enter at least one GSE ID."),
          " Example: GSE114007, GSE50760"
        ),
        type = "warning", duration = 5
      )
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

        # Clear previous external validation cache so this run doesn't mix with old files
        rna_dir <- file.path(getwd(), "ext_val_rna")
        micro_dir <- file.path(getwd(), "ext_val_micro")
        if (dir.exists(rna_dir) && length(rnaseq_ids) > 0) {
          tryCatch({ unlink(rna_dir, recursive = TRUE, force = TRUE) }, error = function(e) NULL)
        }
        if (dir.exists(micro_dir) && length(micro_ids) > 0) {
          tryCatch({ unlink(micro_dir, recursive = TRUE, force = TRUE) }, error = function(e) NULL)
        }
        if (length(rnaseq_ids) > 0) dir.create(rna_dir, showWarnings = FALSE, recursive = TRUE)
        if (length(micro_ids) > 0) dir.create(micro_dir, showWarnings = FALSE, recursive = TRUE)

        # ---- Download RNA-seq (same helper as Step 1 Download) ----
        for (gse_id in rnaseq_ids) {
          incProgress(0.2 / max(1, length(gse_ids)), detail = paste0(gse_id, " (RNA-seq)"))
          ext_log <- paste0(ext_log, gse_id, " (RNA-seq)... ")
          res <- tryCatch(
            gexp_download_one_rnaseq_gse(gse_id, rna_dir),
            error = function(e) list(ok = FALSE, reason = conditionMessage(e), log = "")
          )
          if (!isTRUE(res$ok) || is.null(res$count_matrix)) {
            ext_log <- paste0(ext_log, "FAILED",
              if (!is.null(res$reason) && nzchar(res$reason)) paste0(" (", res$reason, ")") else "",
              "\n")
            next
          }
          all_expr_list[[gse_id]] <- res$count_matrix
          md <- res$metadata
          if (is.null(md) || !is.data.frame(md) || ncol(md) <= 1L) {
            md <- tryCatch(
              gexp_fetch_full_pdata(gse_id, sample_ids = colnames(res$count_matrix)),
              error = function(e) md
            )
          }
          all_metadata_list[[gse_id]] <- md
          n_pd <- if (!is.null(md) && is.data.frame(md)) ncol(md) else 0L
          ext_log <- paste0(
            ext_log, "OK (", nrow(res$count_matrix), "g x ", ncol(res$count_matrix),
            "s, phenodata columns: ", n_pd, ")",
            if (!is.null(res$log) && nzchar(res$log)) paste0(" ", trimws(res$log)) else "",
            "\n"
          )
        }

        # ---- Download Microarray (same helper as Step 1 Download) ----
        for (gse_id in micro_ids) {
          if (gse_id %in% names(all_expr_list)) next
          incProgress(0.2 / max(1, length(gse_ids)), detail = paste0(gse_id, " (Microarray)"))
          ext_log <- paste0(ext_log, gse_id, " (Microarray)... ")
          res <- tryCatch(
            gexp_download_one_microarray_gse(gse_id, micro_dir),
            error = function(e) list(ok = FALSE, reason = conditionMessage(e), log = "")
          )
          if (!isTRUE(res$ok) || is.null(res$micro_expr)) {
            ext_log <- paste0(ext_log, "FAILED",
              if (!is.null(res$reason) && nzchar(res$reason)) paste0(" (", res$reason, ")") else "",
              "\n")
            next
          }
          all_expr_list[[gse_id]] <- res$micro_expr
          md <- res$metadata
          if (is.null(md) || !is.data.frame(md) || ncol(md) <= 1L) {
            md <- tryCatch(
              gexp_fetch_full_pdata(gse_id, sample_ids = colnames(res$micro_expr)),
              error = function(e) md
            )
          }
          all_metadata_list[[gse_id]] <- md
          n_pd <- if (!is.null(md) && is.data.frame(md)) ncol(md) else 0L
          ext_log <- paste0(
            ext_log, "OK (", nrow(res$micro_expr), "g x ", ncol(res$micro_expr),
            "s, phenodata columns: ", n_pd, ")",
            if (!is.null(res$log) && nzchar(res$log)) paste0(" ", trimws(res$log)) else "",
            "\n"
          )
        }

        if (length(all_expr_list) == 0) {
          showNotification(tags$div(icon("times-circle"), tags$strong(" No datasets downloaded.")), type = "error", duration = 8)
          rv$ext_val_log <- ext_log; return()
        }

        incProgress(0.25, detail = "Fetching full GEO phenodata for every GSE...")
        enrich_log <- ""
        all_metadata_list <- .gexpipe_ext_val_enrich_metadata(
          all_expr_list,
          all_metadata_list,
          log_cb = function(msg) { enrich_log <<- paste0(enrich_log, msg) }
        )
        if (nzchar(enrich_log)) {
          ext_log <- paste0(ext_log, "\nPhenodata enrichment (all entered GSEs):\n", enrich_log)
        }

        incProgress(0.15, detail = "Combining genes...")
        common_genes_val <- Reduce(intersect, lapply(all_expr_list, rownames))
        if (length(common_genes_val) == 0L) {
          showNotification(
            tags$div(icon("times-circle"), tags$strong(" No common genes across validation datasets.")),
            type = "error", duration = 8
          )
          rv$ext_val_log <- paste0(ext_log, "\nNo common genes after ID mapping.\n")
          return()
        }
        for (gse in names(all_expr_list)) {
          all_expr_list[[gse]] <- all_expr_list[[gse]][common_genes_val, , drop = FALSE]
        }
        combined_ext_expr <- do.call(cbind, all_expr_list)

        ext_meta <- .gexpipe_ext_val_combine_metadata(all_expr_list, all_metadata_list)
        expr_cols <- colnames(combined_ext_expr)
        if (!anyDuplicated(expr_cols) && all(expr_cols %in% rownames(ext_meta))) {
          ext_meta <- ext_meta[expr_cols, , drop = FALSE]
        } else {
          ext_log <- paste0(
            ext_log,
            "WARNING: phenodata rows could not be matched 1:1 to expression columns; ",
            "using series order.\n"
          )
        }

        # Cache per-GSE pieces so "Refresh phenodata" can re-fetch without re-download
        rv$ext_val_expr_list <- all_expr_list
        rv$ext_val_meta_list <- all_metadata_list

        n_pd_final <- ncol(ext_meta)
        geo_cols_final <- setdiff(colnames(ext_meta), "Dataset")
        thin_final <- length(geo_cols_final) <= 2L ||
          .gexpipe_pdata_is_thin(ext_meta[, geo_cols_final, drop = FALSE])
        ext_log <- paste0(
          ext_log,
          "\nCombined: ", nrow(combined_ext_expr), " genes x ", ncol(combined_ext_expr),
          " samples | phenodata columns: ", n_pd_final,
          " (", paste(head(geo_cols_final, 12), collapse = ", "),
          if (length(geo_cols_final) > 12) ", ..." else "", ")\n",
          "GExPipe ", as.character(utils::packageVersion("GExPipe")), "\n"
        )

        rv$ext_val_raw_expr <- combined_ext_expr
        rv$ext_val_metadata <- ext_meta
        rv$ext_val_log <- ext_log
        rv$ext_val_downloaded <- TRUE

        if (isTRUE(thin_final)) {
          showNotification(
            tags$div(
              icon("exclamation-triangle"),
              tags$strong(" Download OK, but phenodata is still thin."),
              paste0(" Only ", n_pd_final, " columns. Click 'Re-fetch full GEO phenodata' below (needs NCBI access).")
            ),
            type = "warning", duration = 12
          )
        } else {
          showNotification(
            tags$div(
              icon("check-circle"), tags$strong(" Download complete!"),
              paste0(" Phenodata columns: ", n_pd_final, ". Browse below and select Normal/Disease.")
            ),
            type = "message", duration = 8
          )
        }

      }, error = function(e) {
        showNotification(tags$div(icon("times-circle"), tags$strong(" Download failed: "), conditionMessage(e)), type = "error", duration = 10)
        rv$ext_val_log <- paste0("Error: ", conditionMessage(e))
      }, finally = { shinyjs::enable("ext_val_download_btn") })
    })
  })

  # Re-fetch full GEO phenodata without re-downloading counts/expression
  observeEvent(input$ext_val_refresh_pdata_btn, {
    req(isTRUE(rv$ext_val_downloaded), rv$ext_val_raw_expr)
    expr_list <- rv$ext_val_expr_list
    if (is.null(expr_list) || length(expr_list) == 0L) {
      # Rebuild one-GSE list from the combined matrix + Dataset column
      meta <- rv$ext_val_metadata
      if (is.null(meta) || !"Dataset" %in% colnames(meta)) {
        showNotification("No validation datasets cached. Re-run Download.", type = "warning", duration = 6)
        return()
      }
      expr_list <- list()
      meta_list <- list()
      for (gse in unique(as.character(meta$Dataset))) {
        samp <- rownames(meta)[as.character(meta$Dataset) == gse]
        samp <- intersect(samp, colnames(rv$ext_val_raw_expr))
        if (length(samp) == 0L) next
        expr_list[[gse]] <- rv$ext_val_raw_expr[, samp, drop = FALSE]
        meta_list[[gse]] <- meta[samp, , drop = FALSE]
      }
    } else {
      meta_list <- if (is.null(rv$ext_val_meta_list)) list() else rv$ext_val_meta_list
    }
    if (length(expr_list) == 0L) {
      showNotification("No validation expression data found. Re-run Download.", type = "warning", duration = 6)
      return()
    }

    shinyjs::disable("ext_val_refresh_pdata_btn")
    withProgress(message = "Re-fetching full GEO phenodata...", value = 0.2, {
      tryCatch({
        # Force thin so enrich always re-hits NCBI
        for (gse in names(expr_list)) {
          ids <- colnames(expr_list[[gse]])
          meta_list[[gse]] <- data.frame(
            title = ids, row.names = ids, stringsAsFactors = FALSE
          )
        }
        meta_list <- .gexpipe_ext_val_enrich_metadata(
          expr_list,
          meta_list,
          log_cb = function(msg) {
            rv$ext_val_log <- paste0(
              if (is.null(rv$ext_val_log)) "" else rv$ext_val_log,
              msg
            )
          }
        )
        ext_meta <- .gexpipe_ext_val_combine_metadata(expr_list, meta_list)
        expr_cols <- colnames(rv$ext_val_raw_expr)
        if (!anyDuplicated(expr_cols) && all(expr_cols %in% rownames(ext_meta))) {
          ext_meta <- ext_meta[expr_cols, , drop = FALSE]
        }
        rv$ext_val_meta_list <- meta_list
        rv$ext_val_expr_list <- expr_list
        rv$ext_val_metadata <- ext_meta
        n_pd <- ncol(ext_meta)
        geo_cols <- setdiff(colnames(ext_meta), "Dataset")
        rv$ext_val_log <- paste0(
          if (is.null(rv$ext_val_log)) "" else rv$ext_val_log,
          "\nPhenodata refresh: ", n_pd, " columns (",
          paste(head(geo_cols, 15), collapse = ", "),
          if (length(geo_cols) > 15) ", ..." else "", ")\n"
        )
        showNotification(
          tags$div(
            icon("check-circle"),
            tags$strong(paste0(" Phenodata refreshed: ", n_pd, " columns.")),
            if (length(geo_cols) <= 2L)
              " Still thin — check NCBI GEO network access."
            else
              " Select the disease/normal column below."
          ),
          type = if (length(geo_cols) <= 2L) "warning" else "message",
          duration = 8
        )
      }, error = function(e) {
        showNotification(
          tags$div(icon("times-circle"), tags$strong(" Phenodata refresh failed: "), conditionMessage(e)),
          type = "error", duration = 10
        )
      }, finally = {
        shinyjs::enable("ext_val_refresh_pdata_btn")
      })
    })
  })

  # ============================================================================
  # STEP B: Phenodata browser + column selector
  # ============================================================================
  output$ext_val_phenodata_ui <- renderUI({
    if (!isTRUE(rv$ext_val_downloaded) || is.null(rv$ext_val_metadata)) return(NULL)
    meta <- rv$ext_val_metadata
    all_cols <- colnames(meta)
    # Biological columns first, GEO bookkeeping (status, dates, contacts) last
    col_choices <- gexp_phenotype_column_choices(all_cols)
    preselect <- NULL
    for (candidate in c("Condition", "condition", "Group", "group", "disease state:ch1",
                        "disease status:ch1", "diagnosis:ch1", "source_name_ch1")) {
      if (candidate %in% all_cols) {
        vals <- as.character(trimws(meta[[candidate]]))
        u <- unique(vals[!is.na(vals) & vals != ""])
        if (length(u) >= 2 && length(u) <= 5) { preselect <- candidate; break }
      }
    }
    if (is.null(preselect)) {
      suggested <- tryCatch(gexp_suggest_group_column(all_cols), error = function(e) "")
      if (length(suggested) == 1L && nzchar(suggested)) preselect <- suggested
    }
    # Dataset is added by the combine step above, so ignore it when judging
    # whether GEO actually returned usable phenotype columns.
    meta_geo_cols <- setdiff(all_cols, "Dataset")
    thin_note <- if (.gexpipe_pdata_is_thin(meta[, meta_geo_cols, drop = FALSE])) {
      tags$div(
        class = "alert alert-warning",
        style = "margin: 8px 0; padding: 8px 12px; font-size: 12px;",
        icon("exclamation-triangle"),
        paste0(
          " Only ", length(all_cols), " phenodata column(s) were returned by GEO. ",
          "Group labels cannot be derived from this. Check internet access to NCBI GEO, ",
          "then re-run the download above."
        )
      )
    } else {
      NULL
    }
    tagList(
      tags$hr(),
      tags$h4(icon("table"), " Step B: Browse Phenodata & Select Group Column", style = "color: #2980b9; margin-bottom: 10px;"),
      tags$p(
        tags$span(
          class = "badge",
          style = "background: #8e44ad; font-size: 11px; padding: 4px 8px; margin-right: 8px;",
          paste0("GExPipe ", as.character(utils::packageVersion("GExPipe")))
        ),
        "Full GEO phenodata is extracted for every GSE you entered (same as Step 1). ",
        "Choose the column with group labels (e.g. Normal vs Disease / OA), then categorize below.",
        style = "font-size: 13px; margin-bottom: 8px;"
      ),
      tags$div(
        style = "display: flex; gap: 12px; flex-wrap: wrap; margin-bottom: 10px;",
        tags$span(class = "badge", style = "background: #3498db; font-size: 13px; padding: 6px 12px;",
                  icon("vial"), paste0(" Samples: ", nrow(meta))),
        tags$span(class = "badge", style = "background: #2ecc71; font-size: 13px; padding: 6px 12px;",
                  icon("columns"), paste0(" Columns: ", length(all_cols)))
      ),
      thin_note,
      tags$div(
        style = "margin-bottom: 10px; padding: 8px 10px; background: #f8f9fa; border-radius: 5px; max-height: 90px; overflow-y: auto;",
        tags$strong(icon("list"), " Available columns: ", style = "color: #495057; font-size: 12px;"),
        tags$span(paste(all_cols, collapse = ", "),
                  style = "font-size: 12px; color: #2c3e50; word-break: break-word;")
      ),
      DT::dataTableOutput("ext_val_phenodata_table"),
      tags$div(style = "margin-top: 8px; display: flex; gap: 8px; flex-wrap: wrap; align-items: center;",
        downloadButton("download_ext_val_phenodata_csv", tagList(icon("download"), " Phenodata (CSV)"), class = "btn-info btn-sm"),
        actionButton(
          "ext_val_refresh_pdata_btn",
          tagList(icon("sync"), " Re-fetch full GEO phenodata"),
          class = "btn-warning btn-sm"
        ),
        tags$span(
          "Use Re-fetch if only title/Dataset columns appear.",
          style = "font-size: 12px; color: #6c757d;"
        )
      ),
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
    DT::datatable(
      meta,
      options = list(
        pageLength = 8,
        lengthMenu = c(8, 25, 50, 100),
        scrollX = TRUE,
        scrollY = "350px",
        autoWidth = FALSE,
        columnDefs = list(list(className = "dt-left", targets = "_all"))
      ),
      class = "display compact stripe hover",
      filter = "top",
      rownames = TRUE,
      selection = "none"
    )
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
  # Run handler: categorize -> DE
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
    rv$ext_val_expr_list <- NULL
    rv$ext_val_meta_list <- NULL
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
      p
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
          DT::dataTableOutput("val_overlap_de_table"),
          tags$div(style = "margin-top: 8px;",
            downloadButton("download_val_overlap_de_csv", tagList(icon("download"), " Overlap table (CSV)"), class = "btn-success btn-sm"))
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

  output$download_ext_val_phenodata_csv <- downloadHandler(
    filename = function() "External_Validation_Phenodata.csv",
    content = function(file) {
      req(rv$ext_val_metadata)
      write.csv(rv$ext_val_metadata, file, row.names = TRUE)
      write.csv(rv$ext_val_metadata, file.path(CSV_EXPORT_DIR(), "External_Validation_Phenodata.csv"), row.names = TRUE)
    }
  )

  output$download_val_overlap_de_csv <- downloadHandler(
    filename = function() "Validation_Overlap_ML_DE.csv",
    content = function(file) {
      req(rv$ext_val_sig_genes, rv$ml_common_genes)
      sig <- rv$ext_val_sig_genes
      ml_genes <- rv$ml_common_genes
      overlap <- sig[sig$Gene %in% ml_genes, c("Gene", "logFC", "adj.P.Val", "Significance"), drop = FALSE]
      if (nrow(overlap) == 0) overlap <- data.frame(Gene = character(), logFC = numeric(), adj.P.Val = numeric(), Significance = character())
      write.csv(overlap, file, row.names = FALSE)
      write.csv(overlap, file.path(CSV_EXPORT_DIR(), "Validation_Overlap_ML_DE.csv"), row.names = FALSE)
    }
  )

  # ---- Navigation ----
  observeEvent(input$next_page_validation_to_roc, {
    updateTabItems(session, "sidebar_menu", "roc")
    shinyjs::runjs("window.scrollTo(0, 0);")
  })
}
