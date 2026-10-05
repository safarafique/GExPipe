# ==============================================================================
# SERVER_NOMOGRAM.R - Diagnostic Nomogram
# ==============================================================================
# External mode: train on ALL data, validate on external dataset from Step 11.
# Internal mode: 70/30 split-sample validation (stratified).
# Uses rv$batch_corrected, rv$ml_common_genes / rv$common_genes_de_wgcna,
# rv$wgcna_sample_info / rv$unified_metadata.
# ==============================================================================

server_nomogram <- function(input, output, session, rv) {

  # ---- Validation mode indicator ----
  output$nomogram_validation_mode_ui <- renderUI({
    mode <- rv$validation_mode
    if (is.null(mode)) mode <- "internal"
    if (mode == "external") {
      has_ext <- !is.null(rv$external_validation_expr)
      tags$div(
        class = if (has_ext) "alert alert-success" else "alert alert-warning",
        style = "margin-bottom: 15px;",
        icon(if (has_ext) "check-circle" else "exclamation-triangle"),
        tags$strong(" External Validation Mode. "),
        if (has_ext) {
          paste0("Model trained on ALL training data. External dataset (",
                 nrow(rv$external_validation_expr), " samples) used for validation.")
        } else {
          "Go back to Step 11 to load an external validation dataset."
        }
      )
    } else {
      tags$div(
        class = "alert alert-info",
        style = "margin-bottom: 15px;",
        icon("info-circle"),
        tags$strong(" Internal Validation Mode. "),
        "70/30 stratified split-sample validation will be used."
      )
    }
  })

  output$nomogram_run_info_ui <- renderUI({
    mode <- rv$validation_mode
    if (is.null(mode)) mode <- "internal"
    if (mode == "external") {
      tags$p(icon("globe", style = "color: #27ae60;"),
             " Train on ALL samples, validate on external dataset.",
             style = "margin-bottom: 10px; font-size: 13px; color: #1e8449; font-weight: bold;")
    } else {
      tags$p(icon("random", style = "color: #3498db;"),
             " Stratified 70% training / 30% internal validation.",
             style = "margin-bottom: 10px; font-size: 13px; color: #2471a3;")
    }
  })

  output$nomogram_process_summary_ui <- renderUI({
    if (!isTRUE(rv$nomogram_complete)) {
      return(tags$p(style = "color: #6c757d; margin: 0;", icon("info-circle"), " Run diagnostic nomogram to see process summary."))
    }
    train_auc <- if (!is.null(rv$nomogram_train_metrics) && "AUC" %in% names(rv$nomogram_train_metrics)) round(rv$nomogram_train_metrics$AUC, 3) else NA
    val_auc <- if (!is.null(rv$nomogram_val_metrics) && "AUC" %in% names(rv$nomogram_val_metrics)) round(rv$nomogram_val_metrics$AUC, 3) else NA
    tags$div(
      style = "font-size: 14px; line-height: 1.6; color: #333;",
      tags$p(tags$strong("Step 13 complete."), " Nomogram built. Training AUC: ", train_auc, "; Validation AUC: ", val_auc, ". Calibration and DCA plots above."))
  })

  output$nomogram_placeholder_ui <- renderUI({
    if (!is.null(rv$batch_corrected) && (is.matrix(rv$batch_corrected) || is.data.frame(rv$batch_corrected)) && nrow(rv$batch_corrected) > 0) return(NULL)
    tags$div(
      class = "alert alert-warning",
      icon("hand-point-right"),
      " Run Batch Correction (Step 5) first. Then run ML (Step 10) or have common genes from Step 8."
    )
  })

  output$nomogram_status_ui <- renderUI({
    if (!isTRUE(rv$nomogram_complete)) return(NULL)
    n_pred <- length(if (!is.null(rv$nomogram_available_genes)) rv$nomogram_available_genes else character(0))
    n_train <- nrow(if (!is.null(rv$nomogram_train_data)) rv$nomogram_train_data else data.frame())
    n_val <- nrow(if (!is.null(rv$nomogram_validation_data)) rv$nomogram_validation_data else data.frame())
    mode <- rv$validation_mode
    if (is.null(mode)) mode <- "internal"
    val_label <- if (mode == "external") "External Validation" else "Internal Validation (30%)"
    tags$div(
      class = "alert alert-success",
      icon("check-circle"),
      paste0(" Nomogram complete. Training: ", n_train, " samples, ", val_label, ": ", n_val, " samples. ", n_pred, " predictors.")
    )
  })

  # ============================================================================
  # RESULT TABLES + AUTO-EXPORT (new figures/tables -> existing export folder)
  # ============================================================================
  nomogram_result_tables <- function() {
    d <- rv$nomogram_model_diagnostics
    perf <- rv$nomogram_performance_comparison
    std_cols <- c("Predictor", "Coefficient", "Std_Error", "OR", "OR_Lower", "OR_Upper",
                  "P_Value", "VIF", "VIF_Status", "Fit_Type")
    firth_cols <- c("Predictor", "Coefficient_Firth", "Std_Error_Firth", "OR_Firth",
                    "OR_Firth_Lower", "OR_Firth_Upper", "P_Firth")
    note_df <- function(msg) data.frame(Note = msg, stringsAsFactors = FALSE)
    list(
      Nomogram_Model_Coefficients = if (is.null(d)) NULL else d[, intersect(std_cols, names(d)), drop = FALSE],
      Nomogram_Firth_Coefficients = if (is.null(d)) NULL else if (any(firth_cols[-1] %in% names(d))) {
        d[, intersect(firth_cols, names(d)), drop = FALSE]
      } else {
        note_df("Firth regression unavailable (logistf not installed or did not converge).")
      },
      Nomogram_Bootstrap_Optimism_Correction = if (is.null(rv$nomogram_optimism_summary)) {
        note_df("Bootstrap optimism correction was not available for this run.")
      } else rv$nomogram_optimism_summary,
      Nomogram_Performance_Training = if (is.null(perf)) NULL else perf[perf$Dataset == "Training", , drop = FALSE],
      Nomogram_Performance_Validation = if (is.null(perf)) NULL else perf[perf$Dataset != "Training", , drop = FALSE],
      Nomogram_Performance_Comparison = perf,
      Nomogram_Calibration_Statistics = rv$nomogram_calibration_stats,
      Nomogram_Confusion_Matrices = rv$nomogram_confusion_table,
      Nomogram_Model_Diagnostics = d,
      Nomogram_Outcome_Coding_and_Settings = rv$nomogram_run_settings
    )
  }

  nomogram_export_all <- function() {
    dir <- tryCatch(CSV_EXPORT_DIR(), error = function(e) NULL)
    if (is.null(dir)) return(invisible(NULL))
    n_fig <- 0L; n_tab <- 0L
    figs <- list(
      list("Nomogram_ROC_Training", function() plot_nomogram_roc_train(), 6.5, 6.5),
      list("Nomogram_ROC_Validation", function() plot_nomogram_roc_val(), 6.5, 6.5),
      list("Nomogram_ROC_Training_vs_Validation", function() plot_nomogram_roc_combined(), 6.5, 6.5),
      list("Nomogram_Calibration_Training", function() plot_nomogram_cal_train(), 6.5, 6.5),
      list("Nomogram_Calibration_Validation", function() plot_nomogram_cal_val(), 6.5, 6.5),
      list("Nomogram_ConfusionMatrix_Training", function() plot_nomogram_conf_train(), 6.5, 5.5),
      list("Nomogram_ConfusionMatrix_Validation", function() plot_nomogram_conf_val(), 6.5, 5.5)
    )
    for (f in figs) {
      for (type in c("png", "pdf")) {
        saved <- tryCatch({
          gexp_plot_device_open(file.path(dir, paste0(f[[1L]], ".", type)), width = f[[3L]], height = f[[4L]], type = type)
          tryCatch(f[[2L]](), finally = grDevices::dev.off())
          TRUE
        }, error = function(e) FALSE)
        if (isTRUE(saved)) n_fig <- n_fig + 1L
      }
    }
    for (nm in names(nomogram_result_tables())) {
      tab <- nomogram_result_tables()[[nm]]
      if (is.null(tab)) next
      ok <- tryCatch({ utils::write.csv(tab, file.path(dir, paste0(nm, ".csv")), row.names = FALSE); TRUE }, error = function(e) FALSE)
      if (isTRUE(ok)) n_tab <- n_tab + 1L
    }
    rv$nomogram_export_dir <- dir
    showNotification(paste0("Saved ", n_fig, " figure files (300 DPI PNG + PDF) and ", n_tab, " CSV tables to: ", dir),
                     type = "message", duration = 10)
    invisible(dir)
  }

  # ============================================================================
  # RUN NOMOGRAM
  # ============================================================================
  observeEvent(input$run_nomogram, {
    if (is.null(rv$batch_corrected)) {
      showNotification(
        tags$div(icon("exclamation-triangle"), tags$strong(" Step 5 required:"),
                 " Complete batch correction (Step 5) before building the nomogram."),
        type = "error", duration = 6)
      return()
    }
    if ((is.null(rv$roc_selected_genes) || length(rv$roc_selected_genes) == 0) &&
        (is.null(rv$ml_common_genes) || length(rv$ml_common_genes) == 0) &&
        (is.null(rv$common_genes_de_wgcna) || length(rv$common_genes_de_wgcna) == 0)) {
      showNotification(
        tags$div(icon("exclamation-triangle"), tags$strong(" Gene list required:"),
                 " Select genes in Step 12 (ROC), or run ML (Step 10) for common genes, or compute common DEG+WGCNA genes (Step 8) first."),
        type = "error", duration = 8)
      return()
    }

    mode <- rv$validation_mode
    if (is.null(mode)) mode <- "internal"

    # User-configurable bootstrap repetitions (default 1000) and a fixed seed:
    # the internal split, bootstrap optimism and calibration curve are all
    # reproducible for the same seed.
    boot_B <- suppressWarnings(as.integer(input$nomogram_boot_B))
    if (length(boot_B) != 1L || is.na(boot_B)) boot_B <- 1000L
    boot_B <- max(100L, min(5000L, boot_B))
    run_seed <- suppressWarnings(as.integer(input$nomogram_seed))
    if (length(run_seed) != 1L || is.na(run_seed)) run_seed <- 123L

    # Check external validation data if mode is external
    if (mode == "external" && is.null(rv$external_validation_expr)) {
      showNotification(
        tags$div(icon("exclamation-triangle"), tags$strong(" External validation data required:"),
                 " Go to Step 11 and load an external validation dataset first, or switch to Internal Validation."),
        type = "error", duration = 8)
      return()
    }

    expr_mat <- as.matrix(rv$batch_corrected)
    if (nrow(expr_mat) < 10 || ncol(expr_mat) < 3) {
      showNotification("Batch-corrected data too small (need >= 10 genes, >= 3 samples).", type = "error", duration = 6)
      return()
    }
    # Priority: user-selected genes from ROC > ML common genes > DEG+WGCNA common genes.
    # Guard against a STALE confirmed ROC selection left over from an earlier,
    # unrelated analysis in the same session - rv$roc_selected_genes is never
    # cleared when a new analysis starts, and ordinary gene symbols from a
    # previous run will often still be "present" in a new expression matrix
    # even though they were never chosen as candidates for THIS run. Only
    # trust it if it overlaps the current run's own candidate pool.
    current_gene_pool <- unique(c(rv$ml_common_genes, rv$common_genes_de_wgcna))
    common_genes <- rv$roc_selected_genes
    if (!is.null(common_genes) && length(common_genes) > 0 && length(current_gene_pool) > 0 &&
        length(intersect(common_genes, current_gene_pool)) == 0) {
      common_genes <- NULL  # stale selection from a different analysis run - discard
    }
    if (is.null(common_genes) || length(common_genes) == 0) common_genes <- rv$ml_common_genes
    if (is.null(common_genes) || length(common_genes) == 0) common_genes <- rv$common_genes_de_wgcna
    if (is.null(common_genes) || length(common_genes) == 0) {
      showNotification("No genes selected. Go to Step 12 (ROC) and select genes, or run ML (Step 10) first.", type = "warning", duration = 6)
      return()
    }
    sample_info <- rv$wgcna_sample_info
    if (is.null(sample_info)) sample_info <- rv$unified_metadata
    if (is.null(sample_info) || nrow(sample_info) == 0) {
      showNotification("No sample/group metadata. Run WGCNA or groups first.", type = "error", duration = 6)
      return()
    }

    available_genes <- common_genes[common_genes %in% rownames(expr_mat)]
    if (length(available_genes) == 0) {
      showNotification("None of the common genes found in expression data.", type = "error", duration = 6)
      return()
    }

    # Parallel mode's rv$batch_corrected is a UNION of RNA-seq and
    # Microarray genes (each platform corrected on its own), with NA
    # filled in for samples on the platform that never measured a given
    # gene. Being present by NAME in that union does not mean a gene is
    # actually measured across every sample - a gene from only one
    # platform would be fit as a real predictor for samples that have no
    # value for it, and rms::lrm silently drops those samples entirely.
    # Merged mode never hits this because its matrix is intersected up
    # front. Restrict to genes with a real value in every training
    # sample here so parallel mode gets the same guarantee, and tell the
    # user which genes (if any) were excluded and why.
    platform_incomplete <- available_genes[
      vapply(available_genes, function(g) anyNA(expr_mat[g, ]), logical(1))
    ]
    if (length(platform_incomplete) > 0) {
      available_genes <- setdiff(available_genes, platform_incomplete)
      showNotification(
        tags$div(
          icon("exclamation-triangle"),
          tags$strong(paste0(" Removed ", length(platform_incomplete), " gene(s) not measured on both platforms:")),
          tags$br(), paste(platform_incomplete, collapse = ", "),
          tags$br(), tags$small(
            "This training data comes from parallel mode (RNA-seq and Microarray corrected separately). ",
            "A gene measured on only one platform would be fit against samples that never actually measured ",
            "it, and those samples would be silently dropped from the model. Only genes present on both ",
            "platforms are kept."
          )
        ),
        type = "warning", duration = 18
      )
      if (length(available_genes) == 0) {
        showNotification("No genes are measured on both platforms - cannot build a nomogram from this parallel-mode training data.", type = "error", duration = 10)
        return()
      }
    }

    # External mode: the trained model can only be applied to genes the
    # validation dataset actually contains. Restrict the predictor pool to the
    # shared genes BEFORE fitting (never impute missing genes), and say so.
    if (mode == "external") {
      ext_cols <- colnames(rv$external_validation_expr)
      not_in_ext <- setdiff(available_genes, ext_cols)
      if (length(not_in_ext) > 0) {
        available_genes <- intersect(available_genes, ext_cols)
        showNotification(
          tags$div(
            icon("exclamation-triangle"),
            tags$strong(paste0(" ", length(not_in_ext), " gene(s) absent from the external dataset were excluded from the model:")),
            tags$br(), paste(not_in_ext, collapse = ", "),
            tags$br(), tags$small("The training model is applied unchanged to the validation data, so it can only use genes both datasets contain (no imputation).")
          ),
          type = "warning", duration = 15
        )
      }
      if (length(available_genes) == 0) {
        showNotification(
          tags$div(icon("exclamation-triangle"),
                   tags$strong(" No overlapping genes between the model predictors and the external dataset."),
                   tags$br(),
                   tags$small(paste0("Model genes: ", paste(head(not_in_ext, 5), collapse = ", "),
                                     " ... External genes (first 5): ", paste(head(ext_cols, 5), collapse = ", ")))),
          type = "error", duration = 12)
        return()
      }
    }

    expr_nomogram <- t(expr_mat[available_genes, , drop = FALSE])
    expr_nomogram <- as.data.frame(expr_nomogram)
    if (!is.null(sample_info$SampleID)) rownames(sample_info) <- as.character(sample_info$SampleID)
    else if (!is.null(sample_info$sample)) rownames(sample_info) <- as.character(sample_info$sample)
    if (is.null(rownames(sample_info))) rownames(sample_info) <- paste0("S", seq_len(nrow(sample_info)))
    common_samples <- intersect(rownames(expr_nomogram), rownames(sample_info))
    if (length(common_samples) < 10) {
      showNotification("Too few common samples between expression and metadata.", type = "error", duration = 6)
      return()
    }
    expr_nomogram <- expr_nomogram[common_samples, , drop = FALSE]
    sample_info <- sample_info[common_samples, , drop = FALSE]

    group_col <- NULL
    if ("Condition" %in% names(sample_info)) group_col <- "Condition"
    else if ("Group" %in% names(sample_info)) group_col <- "Group"
    else if ("group" %in% names(sample_info)) group_col <- "group"
    if (is.null(group_col)) {
      for (col in names(sample_info)) {
        if (grepl("^(sample|id|sampleid|gsm)", col, ignore.case = TRUE)) next
        vals <- as.character(trimws(sample_info[[col]]))
        vals[vals == ""] <- NA
        u <- unique(vals[!is.na(vals)])
        if (length(u) == 2L) { group_col <- col; break }
      }
      if (is.null(group_col)) group_col <- names(sample_info)[1]
    }
    # OUTCOME CODING (explicit, never reversed silently): 1 = Disease
    # (positive class), 0 = Normal. See gexp_diag_resolve_outcome().
    outcome_coding <- tryCatch(
      gexp_diag_resolve_outcome(sample_info[[group_col]], group_col),
      error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 10); NULL }
    )
    if (is.null(outcome_coding)) return()
    outcome <- outcome_coding$outcome
    if (isTRUE(outcome_coding$heuristic)) {
      showNotification(
        tags$div(
          icon("exclamation-triangle"),
          tags$strong(" Outcome coding was inferred: "),
          paste0("'", outcome_coding$disease_label, "' = 1 (Disease), '", outcome_coding$normal_label, "' = 0 (Normal)."),
          tags$br(), tags$small(outcome_coding$rule, " Check this is the intended direction.")
        ),
        type = "warning", duration = 15
      )
    }
    valid <- !is.na(outcome)
    if (sum(valid) < 10) {
      showNotification("Too many missing group values.", type = "error", duration = 6)
      return()
    }
    expr_nomogram <- expr_nomogram[valid, , drop = FALSE]
    sample_info   <- sample_info[valid, , drop = FALSE]
    outcome       <- outcome[valid]

    # ---- Training-data quality checks (warnings only; never stop the run) ----
    qc_notes <- character(0)
    # (1) Dataset/Condition confounding: if some GSE contains only one class, a gene's
    #     "AUC" can reflect which study a sample came from rather than disease.
    tryCatch({
      ds_vec <- if ("Dataset" %in% names(sample_info)) as.character(sample_info$Dataset) else NULL
      if (!is.null(ds_vec) && length(unique(ds_vec)) >= 2L) {
        tab <- table(ds_vec, factor(outcome, levels = c(0, 1), labels = c("Normal", "Disease")))
        one_class <- rownames(tab)[rowSums(tab > 0) < 2L]
        if (length(one_class) > 0L) {
          tab_txt <- paste(apply(cbind(rownames(tab), tab), 1, function(r) paste0(r[1], " (Normal ", r[2], ", Disease ", r[3], ")")), collapse = "; ")
          qc_notes <- c(qc_notes, paste0("Dataset/Condition confounded: ", tab_txt))
          showNotification(
            tags$div(icon("exclamation-triangle"), tags$strong(" Batch confounding in the training data: "),
                     paste0(length(one_class), " of ", nrow(tab), " dataset(s) contain only one outcome class. "),
                     tags$br(), tags$small(tab_txt),
                     tags$br(), tags$small("A study-specific difference (batch) cannot be separated from disease here, so training AUCs - for every gene, including genes unrelated to the disease - can be inflated. Treat these results as unreliable until Normal and Disease samples are present in the same study.")),
            type = "warning", duration = 25)
        }
      }
    }, error = function(e) NULL)
    # (2) Negative control: housekeeping genes should NOT separate disease from normal.
    tryCatch({
      hk_names <- c("GAPDH", "ACTB", "B2M", "PPIA", "RPLP0", "TBP", "HPRT1", "PGK1")
      hk_in <- intersect(hk_names, rownames(expr_mat))
      if (length(hk_in) > 0L) {
        hk_df <- as.data.frame(t(expr_mat[hk_in, rownames(expr_nomogram), drop = FALSE]))
        hk <- gexp_diag_housekeeping_auc(hk_df, outcome)
        if (!is.null(hk)) {
          hk_txt <- paste0(hk$Gene, " AUC ", sprintf("%.2f", hk$AUC), collapse = ", ")
          qc_notes <- c(qc_notes, paste0("Housekeeping negative control: ", hk_txt))
          if (any(hk$AUC > 0.80, na.rm = TRUE)) {
            showNotification(
              tags$div(icon("exclamation-triangle"), tags$strong(" Housekeeping genes separate the groups: "), hk_txt,
                       tags$br(), tags$small("These genes are expected to be near AUC 0.5. A high value suggests batch or sample-composition differences between Normal and Disease (or a real biological effect on that gene) - check Dataset/Condition balance before trusting the panel's AUCs.")),
              type = "warning", duration = 20)
          }
        }
      }
    }, error = function(e) NULL)
    # (3) Step 12 (ROC) and this step should score a gene identically; a mismatch means they use different data.
    tryCatch({
      ex_ml <- rv$extracted_data_ml
      if (!is.null(ex_ml) && (is.matrix(ex_ml) || is.data.frame(ex_ml))) {
        ex_ml <- as.data.frame(ex_ml)
        cs <- intersect(rownames(ex_ml), rownames(expr_nomogram))
        cg <- intersect(colnames(ex_ml), colnames(expr_nomogram))
        if (length(cs) >= 10L && length(cg) > 0L) {
          y_named <- setNames(outcome, rownames(expr_nomogram))
          d <- vapply(cg, function(g) {
            a1 <- gexp_diag_directional_roc(y_named[cs], as.numeric(ex_ml[cs, g]))
            a2 <- gexp_diag_directional_roc(y_named[cs], as.numeric(expr_nomogram[cs, g]))
            if (is.null(a1) || is.null(a2)) NA_real_ else abs(max(a1$auc, 1 - a1$auc) - max(a2$auc, 1 - a2$auc))
          }, numeric(1))
          overlap <- length(cs) / nrow(expr_nomogram)
          if (overlap < 0.9 || any(d > 0.02, na.rm = TRUE)) {
            qc_notes <- c(qc_notes, sprintf("Step 12 vs Step 14 data differ (sample overlap %.0f%%, max per-gene AUC difference %.3f)", 100 * overlap, max(d, na.rm = TRUE)))
            showNotification(
              tags$div(icon("exclamation-triangle"), tags$strong(" Step 12 and this step score genes on different data: "),
                       sprintf("%.0f%% sample overlap; per-gene AUC differs by up to %.3f (%s).", 100 * overlap, max(d, na.rm = TRUE), cg[which.max(d)]),
                       tags$br(), tags$small("Single-gene AUCs shown in Step 12 will not match this model's AUC because the expression matrix or sample set differs.")),
              type = "warning", duration = 15)
          }
        }
      }
    }, error = function(e) NULL)
    min_events <- min(sum(outcome == 1), sum(outcome == 0))
    epv <- min_events / length(available_genes)
    # If sample size or panel size forces a reduction, rank by each gene's
    # actual association with the disease outcome (point-biserial
    # correlation) - NOT raw expression variance, which is blind to
    # relevance and could silently drop a gene the user specifically
    # validated in Step 12 (ROC) in favor of an unvalidated, merely
    # high-variance one. Any trim is also reported to the user, never silent.
    .gexpipe_nomogram_trim <- function(genes, keep_n) {
      if (length(genes) <= keep_n) return(genes)
      assoc <- vapply(genes, function(g) {
        # Use expr_nomogram (samples x genes), already filtered to `valid`
        # and aligned with `outcome` - NOT expr_mat, which is still the
        # full, unfiltered batch-corrected matrix and would mismatch
        # outcome's length whenever samples were dropped above.
        r <- suppressWarnings(stats::cor(expr_nomogram[[g]], outcome, use = "pairwise.complete.obs"))
        if (is.na(r)) 0 else abs(r)
      }, numeric(1))
      kept <- names(sort(assoc, decreasing = TRUE))[seq_len(keep_n)]
      dropped <- setdiff(genes, kept)
      showNotification(
        tags$div(
          icon("exclamation-triangle"),
          tags$strong(paste0(" Sample size limits the nomogram to ", keep_n, " gene(s).")),
          tags$br(), "Kept (highest association with outcome): ", tags$strong(paste(kept, collapse = ", ")),
          tags$br(), "Dropped: ", paste(dropped, collapse = ", ")
        ),
        type = "warning", duration = 12
      )
      kept
    }
    if (epv < 10) {
      max_predictors <- max(3, floor(min_events / 10))
      available_genes <- .gexpipe_nomogram_trim(available_genes, min(max_predictors, length(available_genes)))
      epv <- min_events / length(available_genes)
    } else if (length(available_genes) > 15) {
      available_genes <- .gexpipe_nomogram_trim(available_genes, 15L)
      epv <- min_events / length(available_genes)
    }
    # Drop redundant, highly-correlated genes (e.g. co-expressed cell-cycle
    # markers like CHEK1/CCNB2) BEFORE fitting - two near-collinear
    # predictors in a multivariable logistic model inflate coefficient
    # variance and drive the kind of unstable, exploding ORs seen under
    # separation. Keep the more outcome-associated gene of each
    # highly-correlated pair; never silent.
    .gexpipe_nomogram_corr_prune <- function(genes, corr_cutoff = 0.8) {
      if (length(genes) < 2) return(genes)
      assoc <- vapply(genes, function(g) {
        r <- suppressWarnings(stats::cor(expr_nomogram[[g]], outcome, use = "pairwise.complete.obs"))
        if (is.na(r)) 0 else abs(r)
      }, numeric(1))
      ordered_genes <- names(sort(assoc, decreasing = TRUE))
      kept <- character(0)
      dropped_info <- character(0)
      for (g in ordered_genes) {
        redundant_with <- NA_character_
        for (k in kept) {
          r <- suppressWarnings(stats::cor(expr_nomogram[[g]], expr_nomogram[[k]], use = "pairwise.complete.obs"))
          if (!is.na(r) && abs(r) > corr_cutoff) { redundant_with <- k; break }
        }
        if (is.na(redundant_with)) kept <- c(kept, g)
        else dropped_info[g] <- redundant_with
      }
      if (length(dropped_info) > 0) {
        msg_lines <- paste0(names(dropped_info), " (redundant with ", dropped_info, ")")
        showNotification(
          tags$div(
            icon("exclamation-triangle"),
            tags$strong(paste0(" Removed ", length(dropped_info), " highly correlated gene(s) (|r| > ", corr_cutoff, "):")),
            tags$br(), paste(msg_lines, collapse = "; "),
            tags$br(), tags$small("Keeping the more outcome-associated gene from each correlated pair avoids collinear, unstable coefficients.")
          ),
          type = "warning", duration = 12
        )
      }
      kept
    }
    available_genes <- .gexpipe_nomogram_corr_prune(available_genes)

    # Pairwise correlation only catches two genes that move together. It
    # misses a gene that is a near-linear COMBINATION of several others
    # (multi-way collinearity), which shows up as a high VIF (Variance
    # Inflation Factor) even when every individual pairwise correlation
    # looks fine. A VIF above 10 means that gene's coefficient/SE cannot
    # be trusted. Iteratively drop the worst offender and refit until
    # every remaining gene's VIF is acceptable, same transparency as the
    # correlation pruning above.
    .gexpipe_nomogram_vif_prune <- function(genes, vif_cutoff = 10, min_genes = 2) {
      if (length(genes) < min_genes || !requireNamespace("car", quietly = TRUE)) return(genes)
      dropped <- character(0)
      repeat {
        if (length(genes) < min_genes) break
        fit_data <- cbind(expr_nomogram[, genes, drop = FALSE], Outcome = outcome)
        glm_fit <- tryCatch(
          glm(as.formula(paste("Outcome ~", paste(genes, collapse = " + "))), data = fit_data, family = binomial()),
          error = function(e) NULL
        )
        if (is.null(glm_fit)) break
        vifs <- tryCatch(car::vif(glm_fit), error = function(e) NULL)
        if (is.null(vifs) || length(vifs) != length(genes) || all(vifs <= vif_cutoff, na.rm = TRUE)) break
        worst <- names(vifs)[which.max(vifs)]
        dropped[worst] <- sprintf("VIF = %.1f", max(vifs, na.rm = TRUE))
        genes <- setdiff(genes, worst)
      }
      if (length(dropped) > 0) {
        msg_lines <- paste0(names(dropped), " (", dropped, ")")
        showNotification(
          tags$div(
            icon("exclamation-triangle"),
            tags$strong(paste0(" Removed ", length(dropped), " gene(s) with severe multi-way collinearity (VIF > ", vif_cutoff, "):")),
            tags$br(), paste(msg_lines, collapse = "; "),
            tags$br(), tags$small(
              "These genes are a near-linear combination of the others in the panel, not just pairwise correlated. ",
              "Their coefficient and standard error cannot be trusted while they remain in the model."
            )
          ),
          type = "warning", duration = 15
        )
      }
      genes
    }
    available_genes <- .gexpipe_nomogram_vif_prune(available_genes)

    if (sum(outcome == 1) < 10 || sum(outcome == 0) < 10) {
      showNotification("Need at least 10 samples per group.", type = "error", duration = 6)
      return()
    }

    nomogram_data <- cbind(expr_nomogram[, available_genes, drop = FALSE], Outcome = outcome)
    nomogram_data$SampleID <- rownames(nomogram_data)
    nd_drop <- gexp_diag_drop_nonfinite(nomogram_data, available_genes)
    if (nd_drop$n_dropped > 0) {
      showNotification(paste0(nd_drop$n_dropped, " training sample(s) with NA/Inf in a predictor were removed."),
                       type = "warning", duration = 10)
      nomogram_data <- nd_drop$df
    }
    nd_chk <- gexp_diag_check_frame(nomogram_data, available_genes, "Outcome", "Training", min_per_class = 10L)
    if (length(nd_chk) > 0) {
      showNotification(tags$div(icon("exclamation-triangle"), tags$strong(" Cannot build the model: "), paste(nd_chk, collapse = " ")),
                       type = "error", duration = 10)
      return()
    }

    # ==================================================================
    # Split based on validation mode
    # ==================================================================
    if (mode == "external") {
      # EXTERNAL MODE: train on ALL data, validate on external dataset
      train_data <- nomogram_data

      # Build external validation data.frame (strict checks, no imputation)
      ext_expr <- rv$external_validation_expr
      ext_outcome <- rv$external_validation_outcome
      ext_problem <- NULL
      if (is.null(ext_expr) || nrow(ext_expr) == 0) {
        ext_problem <- "The external validation dataset has no samples."
      } else if (is.null(ext_outcome) || length(ext_outcome) != nrow(ext_expr)) {
        ext_problem <- paste0("External outcome labels (", length(ext_outcome), ") do not match the number of external samples (",
                              nrow(ext_expr), "). Re-run the Validation step.")
      } else if (!all(ext_outcome[!is.na(ext_outcome)] %in% c(0, 1))) {
        ext_problem <- "External outcome must be coded 0 = Normal, 1 = Disease. Re-run the Validation step."
      }
      if (is.null(ext_problem)) {
        miss_ext <- setdiff(available_genes, colnames(ext_expr))
        if (length(miss_ext) > 0) {
          ext_problem <- paste0("Model predictor(s) missing from the external dataset: ", paste(miss_ext, collapse = ", "), ".")
        }
      }
      if (!is.null(ext_problem)) {
        showNotification(tags$div(icon("exclamation-triangle"), tags$strong(" External validation cannot run: "), ext_problem),
                         type = "error", duration = 12)
        return()
      }
      ext_df <- as.data.frame(ext_expr[, available_genes, drop = FALSE])
      ext_df[] <- lapply(ext_df, function(v) suppressWarnings(as.numeric(v)))
      ext_df$Outcome <- as.integer(ext_outcome)
      ext_df$SampleID <- if (!is.null(rownames(ext_expr))) rownames(ext_expr) else paste0("ExtS", seq_len(nrow(ext_df)))
      dropped_ext <- gexp_diag_drop_nonfinite(ext_df, available_genes)
      ext_df <- dropped_ext$df
      ext_df <- ext_df[!is.na(ext_df$Outcome), , drop = FALSE]
      if (dropped_ext$n_dropped > 0) {
        showNotification(
          paste0(dropped_ext$n_dropped, " external sample(s) with NA/Inf in a predictor were removed before validation."),
          type = "warning", duration = 10)
      }
      ext_chk <- gexp_diag_check_frame(ext_df, available_genes, "Outcome", "External validation", min_per_class = 2L)
      if (length(ext_chk) > 0) {
        showNotification(tags$div(icon("exclamation-triangle"), tags$strong(" External validation cannot run: "), paste(ext_chk, collapse = " ")),
                         type = "error", duration = 12)
        return()
      }
      validation_data <- ext_df

      # Validation cohort eligibility check - an N=7 external set (or one
      # with only 1-2 events in the smaller class) can produce a perfect-
      # looking but statistically meaningless AUC/ROC curve. Warn instead
      # of silently presenting it as if it were reliable.
      n_val_total <- nrow(validation_data)
      n_val_events <- sum(validation_data$Outcome == 1, na.rm = TRUE)
      n_val_nonevents <- sum(validation_data$Outcome == 0, na.rm = TRUE)
      min_val_class <- min(n_val_events, n_val_nonevents)
      if (n_val_total < 20 || min_val_class < 5) {
        showNotification(
          tags$div(
            icon("exclamation-triangle"),
            tags$strong(" Small validation cohort: "),
            paste0(n_val_total, " total samples (", n_val_events, " disease, ", n_val_nonevents, " normal)."),
            tags$br(),
            tags$small(
              "Below ~20 total samples or 5 events per class, validation AUC/ROC estimates are unstable ",
              "and can look artificially perfect or artificially poor by chance. Treat this run as ",
              "exploratory, not confirmatory. Consider Internal Validation (70/30 split) instead if you ",
              "don't have a larger external cohort."
            )
          ),
          type = "warning", duration = 15
        )
      }

    } else {
      # INTERNAL MODE: 70/30 stratified split
      withr::local_seed(run_seed)
      train_idx <- caret::createDataPartition(nomogram_data$Outcome, p = 0.7, list = FALSE)
      train_data <- nomogram_data[train_idx, ]
      validation_data <- nomogram_data[-train_idx, ]
    }

    # ==================================================================
    # Cross-cohort harmonization: Z-score each gene independently within
    # its own dataset, BEFORE fitting/predicting. Raw expression scale
    # (platform, batch, normalization method) differs between an external
    # validation cohort and the training cohort; feeding raw values into
    # coefficients learned on a different scale is what skews validation
    # predictions to extreme 0/1 values. For external mode, train and
    # validation are genuinely different batches, so each is standardized
    # using its OWN mean/SD. For internal mode (70/30 split of the SAME
    # cohort), there's no real batch shift to correct, so the held-out 30%
    # is standardized using the TRAINING split's mean/SD instead of its
    # own - independently re-centering a small subsample would just add
    # noise, not remove a real batch effect.
    .gexpipe_zscore_apply <- function(df, cols, ref_mean = NULL, ref_sd = NULL) {
      out_mean <- ref_mean; out_sd <- ref_sd
      for (g in cols) {
        v <- df[[g]]
        m <- if (!is.null(ref_mean)) ref_mean[[g]] else mean(v, na.rm = TRUE)
        s <- if (!is.null(ref_sd)) ref_sd[[g]] else stats::sd(v, na.rm = TRUE)
        if (is.null(ref_mean)) { if (is.null(out_mean)) out_mean <- list(); out_mean[[g]] <- m }
        if (is.null(ref_sd)) { if (is.null(out_sd)) out_sd <- list(); out_sd[[g]] <- s }
        df[[g]] <- if (is.finite(s) && s > 0) (v - m) / s else rep(0, length(v))
      }
      list(df = df, mean = out_mean, sd = out_sd)
    }
    # Record each model gene's RAW variation in the validation data BEFORE
    # z-scoring hides it (a constant gene z-scores to all zeros), so a
    # collapsed validation prediction can be explained to the user below.
    val_raw_sd <- vapply(available_genes, function(g) {
      v <- suppressWarnings(as.numeric(validation_data[[g]]))
      if (length(v) == 0L || all(is.na(v))) NA_real_ else stats::sd(v, na.rm = TRUE)
    }, numeric(1))
    # Raw-scale comparison BEFORE standardizing (training vs validation).
    scale_check <- NULL
    if (mode == "external") {
      scale_check <- tryCatch(gexp_diag_scale_check(train_data, validation_data, available_genes), error = function(e) NULL)
    }
    train_z <- .gexpipe_zscore_apply(train_data, available_genes)
    train_data <- train_z$df

    # Validation standardization (external mode). Default = within the validation
    # cohort (needed when platforms differ). That assumes a similar case-mix: with a
    # different disease prevalence the cohort mean sits elsewhere, which shifts every
    # standardized value and therefore every predicted risk. Alternatives are offered
    # and the choice is recorded with the results.
    val_std <- input$nomogram_val_standardize
    if (length(val_std) != 1L || !val_std %in% c("cohort", "prevalence", "training")) val_std <- "cohort"
    prev_tr <- mean(train_data$Outcome == 1)
    prev_va <- mean(validation_data$Outcome == 1)
    if (mode == "external" && val_std == "training") {
      validation_data <- .gexpipe_zscore_apply(validation_data, available_genes, ref_mean = train_z$mean, ref_sd = train_z$sd)$df
    } else if (mode == "external" && val_std == "prevalence" && prev_va > 0 && prev_va < 1) {
      # Reference mean/SD from the validation cohort re-weighted so its class mix equals the
      # TRAINING prevalence. Validation labels are used only for these weights - the model is
      # not refit and the threshold is not changed.
      w_cls <- ifelse(validation_data$Outcome == 1, prev_tr / prev_va, (1 - prev_tr) / (1 - prev_va))
      for (g in available_genes) {
        v <- suppressWarnings(as.numeric(validation_data[[g]]))
        mo <- gexp_diag_weighted_moments(v, w_cls)
        validation_data[[g]] <- if (is.finite(mo[["sd"]]) && mo[["sd"]] > 0) (v - mo[["mean"]]) / mo[["sd"]] else rep(0, length(v))
      }
    } else if (mode == "external") {
      val_std <- "cohort"
      validation_data <- .gexpipe_zscore_apply(validation_data, available_genes)$df
    } else {
      validation_data <- .gexpipe_zscore_apply(validation_data, available_genes, ref_mean = train_z$mean, ref_sd = train_z$sd)$df
    }
    std_label <- if (mode != "external") "Validation split z-scored with the training split's mean/SD" else switch(
      val_std,
      cohort = "Each gene z-scored within its own cohort (no outcome information used)",
      prevalence = "Validation genes z-scored within the cohort with reference mean/SD re-weighted to the training prevalence (validation labels used only for these weights)",
      training = "Validation genes z-scored with the TRAINING mean/SD (valid only when both cohorts share the same platform and scale)"
    )
    if (mode == "external") {
      if (abs(prev_tr - prev_va) > 0.10 && val_std == "cohort") {
        showNotification(
          tags$div(icon("exclamation-triangle"), tags$strong(" Different disease prevalence: "),
                   sprintf("training %.0f%% vs validation %.0f%%.", 100 * prev_tr, 100 * prev_va),
                   tags$br(), tags$small("Standardizing within the validation cohort assumes a similar case-mix. With this difference all standardized values - and so all predicted risks - are shifted (toward Disease when validation has fewer cases than training), which inflates false positives even when the AUC is fine. If specificity or calibration look off, choose 'prevalence-matched' standardization in the Run box, or validate on the same platform with the training mean/SD.")),
          type = "warning", duration = 25)
      }
      if (!is.null(scale_check) && any(scale_check$Flag, na.rm = TRUE)) {
        fl <- scale_check[scale_check$Flag %in% TRUE, , drop = FALSE]
        showNotification(
          tags$div(icon("info-circle"), tags$strong(" Raw scales differ between training and validation: "),
                   paste0(fl$Gene, " (validation mean ", sprintf("%.1f", fl$Val_Mean), " vs training ", sprintf("%.1f", fl$Train_Mean), ")", collapse = "; "),
                   tags$br(), tags$small(if (val_std == "training") "You chose the training mean/SD, which is NOT appropriate when the scales differ this much - predictions will be unreliable. Use within-cohort standardization."
                                         else "Typical of different platforms or normalizations (e.g. microarray vs RNA-seq). Values are standardized within the validation cohort; cross-platform differences still limit how far the model transfers.")),
          type = if (val_std == "training") "error" else "warning", duration = 20)
      }
    }
    rv$nomogram_scale_check <- scale_check
    rv$nomogram_val_standardization <- val_std

    # ==================================================================
    # Fit nomogram model on training data
    # ==================================================================
    formula_obj <- as.formula(paste("Outcome ~", paste(available_genes, collapse = " + ")))
    old_dd <- getOption("datadist")
    dd <- rms::datadist(train_data[, available_genes, drop = FALSE])
    options(datadist = dd)
    on.exit({ options(datadist = old_dd) }, add = TRUE)

    nomogram_model <- tryCatch(
      rms::lrm(formula_obj, data = train_data, x = TRUE, y = TRUE),
      error = function(e) { showNotification(paste("Model failed:", e$message), type = "error", duration = 8); NULL }
    )
    if (is.null(nomogram_model) || (isTRUE(nomogram_model$fail))) {
      showNotification("Model did not converge. Try fewer predictors or more samples.", type = "error", duration = 8)
      return()
    }

    # Quasi-separation fallback: if any slope's SE blows up, the fitted
    # probabilities will be pinned near 0/1 and everything downstream
    # (nomogram, calibration, DCA) degenerates. Refit with a modest ridge
    # penalty (stays inside rms::lrm so nomogram/calibrate/predict all keep
    # working unchanged) instead of shipping an exploding, useless model.
    .gexpipe_lrm_se <- function(fit) {
      tryCatch(sqrt(diag(vcov(fit)))[-1], error = function(e) numeric(0))
    }
    initial_se <- .gexpipe_lrm_se(nomogram_model)
    initial_coef <- tryCatch(abs(coef(nomogram_model)[-1]), error = function(e) numeric(0))
    ridge_applied <- FALSE
    # Predictors are z-scored, so a slope is "log-odds per 1 SD". |slope| > 5
    # means an odds ratio above ~150 per SD - near-perfect separation even
    # when the SE (e.g. 3.2) stays under 5. Such a steep model saturates to
    # 0/1 on any cohort whose expression distribution differs slightly, so
    # penalize it too, not only when the SE explodes.
    if ((length(initial_se) > 0 && any(initial_se > 3, na.rm = TRUE)) ||
        (length(initial_coef) > 0 && any(initial_coef > 5, na.rm = TRUE))) {
      penalized_fit <- tryCatch(
        rms::lrm(formula_obj, data = train_data, x = TRUE, y = TRUE, penalty = 2),
        error = function(e) NULL
      )
      if (!is.null(penalized_fit) && !isTRUE(penalized_fit$fail)) {
        nomogram_model <- penalized_fit
        ridge_applied <- TRUE
        showNotification(
          tags$div(
            icon("exclamation-triangle"),
            tags$strong(" Quasi-complete separation detected - switched to a penalized (ridge) fit."),
            tags$br(),
            tags$small(
              "The unpenalized model was extremely steep (a slope above 5 per SD, or SE above 3), which would have pushed ",
              "predicted probabilities to extreme 0/1 values. A small ridge penalty (penalty=2) keeps ",
              "the nomogram/calibration/DCA usable. See Model Diagnostics for Firth-corrected coefficients too."
            )
          ),
          type = "warning", duration = 15
        )
      }
    }

    train_data$Predicted_Prob <- predict(nomogram_model, newdata = train_data, type = "fitted")

    # VALIDATION USES THE TRAINING MODEL EXACTLY AS FITTED. The linear
    # predictor is the training intercept plus the training coefficients times
    # the validation predictors. Nothing is refit or recalibrated on the
    # validation outcomes, and the classification threshold (below) comes from
    # the training data only.
    train_cf <- stats::coef(nomogram_model)
    cf_genes <- setdiff(names(train_cf), "Intercept")
    val_lp <- if (setequal(cf_genes, available_genes)) {
      as.numeric(train_cf[["Intercept"]] + as.matrix(validation_data[, cf_genes, drop = FALSE]) %*% train_cf[cf_genes])
    } else {
      as.numeric(predict(nomogram_model, newdata = validation_data, type = "lp"))
    }
    validation_data$Predicted_Prob <- stats::plogis(val_lp)

    # If every validation sample gets the same predicted risk, the validation
    # AUC is exactly 0.5 and calibration/DCA are meaningless. That is a data or
    # pipeline problem (genes missing or constant in the validation matrix),
    # NOT weak biology - weak biology gives AUCs like 0.55-0.65, never exactly
    # 0.5. Say so, and name the genes responsible.
    val_pred_unique <- length(unique(round(validation_data$Predicted_Prob[is.finite(validation_data$Predicted_Prob)], 6)))
    imputed_genes <- character(0)  # external genes are never imputed (see shared-gene check above)
    dead_genes <- available_genes[is.na(val_raw_sd) | val_raw_sd == 0]
    if (val_pred_unique <= 1L || length(dead_genes) > 0L || length(imputed_genes) > 0L) {
      showNotification(
        tags$div(
          icon("exclamation-triangle"),
          tags$strong(if (val_pred_unique <= 1L) " Validation predictions are constant - validation AUC will be 0.5." else " Some model genes are unusable in the validation data."),
          if (length(imputed_genes) > 0L) tags$div("Not found in the validation dataset (filled with 0): ", tags$strong(paste(imputed_genes, collapse = ", "))),
          if (length(dead_genes) > 0L) tags$div("No variation or all missing in the validation data: ", tags$strong(paste(dead_genes, collapse = ", "))),
          tags$small(
            "This is a data/pipeline problem, not weak biology. Check that the validation dataset uses the same ",
            "gene symbols and platform choice (Step 11), and that these genes appear in its expression matrix. ",
            "A gene with no variation contributes nothing to the prediction."
          )
        ),
        type = "error", duration = 20
      )
    }
    # Classification threshold: Youden index on the TRAINING ROC only. It is
    # applied unchanged to the validation data (never re-selected there).
    train_roc <- pROC::roc(train_data$Outcome, train_data$Predicted_Prob, levels = c(0, 1), direction = "<", quiet = TRUE)
    optimal_threshold <- suppressWarnings(as.numeric(pROC::coords(train_roc, "best", ret = "threshold", best.method = "youden"))[1L])
    if (!is.finite(optimal_threshold)) optimal_threshold <- 0.5
    train_data$Predicted_Class <- ifelse(train_data$Predicted_Prob > optimal_threshold, 1, 0)
    validation_data$Predicted_Class <- ifelse(validation_data$Predicted_Prob > optimal_threshold, 1, 0)

    glm_fit <- glm(formula_obj, data = train_data, family = binomial())
    vif_vals <- if (requireNamespace("car", quietly = TRUE)) {
      tryCatch(car::vif(glm_fit), error = function(e) setNames(rep(NA, length(available_genes)), available_genes))
    } else {
      setNames(rep(NA, length(available_genes)), available_genes)
    }
    if (length(vif_vals) != length(available_genes)) vif_vals <- setNames(rep(NA, length(available_genes)), available_genes)
    coefs <- coef(nomogram_model)[-1]
    se <- sqrt(diag(vcov(nomogram_model)))[-1]
    z_crit <- stats::qnorm(0.975)
    model_diagnostics <- data.frame(
      Predictor = available_genes,
      Coefficient = as.numeric(coefs),
      Std_Error = as.numeric(se),
      OR = exp(as.numeric(coefs)),
      OR_Lower = exp(as.numeric(coefs) - z_crit * as.numeric(se)),
      OR_Upper = exp(as.numeric(coefs) + z_crit * as.numeric(se)),
      P_Value = 2 * stats::pnorm(-abs(as.numeric(coefs) / as.numeric(se))),
      VIF = as.numeric(vif_vals[available_genes]),
      stringsAsFactors = FALSE
    )
    model_diagnostics$VIF_Status <- ifelse(is.na(model_diagnostics$VIF), "Unknown",
      ifelse(model_diagnostics$VIF > 10, "High (>10)", ifelse(model_diagnostics$VIF > 5, "Moderate (5-10)", "Low (<5)")))
    model_diagnostics$Fit_Type <- if (ridge_applied) "Ridge-penalized (penalty = 2); Wald CI/p" else "Maximum likelihood; Wald CI/p"

    # Firth's penalized-likelihood logistic regression: when a gene (or
    # combination) near-perfectly separates the two groups, ordinary MLE
    # (rms::lrm/glm) diverges to huge coefficients and SEs (OR in the
    # 1e50 range is a real failure mode, not just a display quirk). Firth
    # adds a small bias-correcting penalty and always returns finite,
    # realistic coefficients/CIs, so it is shown as a second, trustworthy
    # set of estimates alongside the standard ones rather than replacing
    # the plotting/prediction engine (rms::lrm) that the nomogram/
    # calibration/DCA plots below still depend on.
    firth_fit <- if (requireNamespace("logistf", quietly = TRUE)) {
      tryCatch(logistf::logistf(formula_obj, data = train_data), error = function(e) NULL)
    } else {
      NULL
    }
    if (!is.null(firth_fit) && any(!is.finite(coef(firth_fit)))) firth_fit <- NULL  # non-convergence
    if (requireNamespace("logistf", quietly = TRUE) && is.null(firth_fit)) {
      showNotification("Firth regression did not converge for this panel; Firth columns are omitted.",
                       type = "warning", duration = 10)
    }
    if (!is.null(firth_fit)) {
      firth_coefs_all <- coef(firth_fit)
      firth_se_all <- setNames(sqrt(diag(vcov(firth_fit))), names(firth_coefs_all))
      model_diagnostics$Coefficient_Firth <- as.numeric(firth_coefs_all[available_genes])
      model_diagnostics$Std_Error_Firth <- as.numeric(firth_se_all[available_genes])
      model_diagnostics$OR_Firth <- exp(model_diagnostics$Coefficient_Firth)
      # Profile penalized-likelihood CI and penalized likelihood-ratio p-value
      model_diagnostics$OR_Firth_Lower <- exp(as.numeric(firth_fit$ci.lower[available_genes]))
      model_diagnostics$OR_Firth_Upper <- exp(as.numeric(firth_fit$ci.upper[available_genes]))
      model_diagnostics$P_Firth <- as.numeric(firth_fit$prob[available_genes])
    }
    rv$nomogram_firth_available <- !is.null(firth_fit)

    separation_flagged <- se > 5 | abs(coefs) > 15
    if (any(separation_flagged, na.rm = TRUE)) {
      bad_genes <- available_genes[which(separation_flagged)]
      showNotification(
        tags$div(
          icon("exclamation-triangle"),
          tags$strong(" Quasi-complete separation detected"),
          tags$span(" for: ", paste(bad_genes, collapse = ", ")),
          tags$br(),
          tags$small(
            if (!is.null(firth_fit)) {
              paste0(
                "These gene(s) near-perfectly separate the two groups in this sample, so the standard ",
                "model's coefficient/OR for them is unstable (huge SE, extreme OR - not a real effect size). ",
                "Firth-corrected (bias-reduced) coefficients are shown alongside the standard ones in the ",
                "Model Diagnostics table below - trust those instead."
              )
            } else {
              paste0(
                "These gene(s) near-perfectly separate the two groups in this sample, so the standard ",
                "model's coefficient/OR for them is unstable (huge SE, extreme OR). Install the 'logistf' ",
                "package for bias-reduced (Firth) estimates."
              )
            }
          )
        ),
        type = "warning", duration = 18
      )
    }

    val_dataset_label <- if (mode == "external") "External Validation" else "Internal Validation (30%)"
    train_metrics <- tryCatch(
      gexp_diag_performance(train_data$Outcome, train_data$Predicted_Prob, optimal_threshold, "Training"),
      error = function(e) { showNotification(paste0("Training performance failed: ", conditionMessage(e)), type = "error", duration = 10); NULL }
    )
    val_metrics <- tryCatch(
      gexp_diag_performance(validation_data$Outcome, validation_data$Predicted_Prob, optimal_threshold, val_dataset_label),
      error = function(e) { showNotification(paste0("Validation performance failed: ", conditionMessage(e)), type = "error", duration = 10); NULL }
    )
    if (is.null(train_metrics) || is.null(val_metrics)) return()
    performance_comparison <- rbind(train_metrics, val_metrics)

    # BOOTSTRAP optimism correction + bias-corrected calibration curve.
    # B (default 1000) resamples with a fixed seed. The apparent training
    # C-index is measured on the data the model was fit on and is optimistic;
    # rms::validate() refits on each resample and averages the gap
    # ("optimism") between performance on the resample and on the original data.
    boot_validate <- NULL
    cal_train <- NULL
    withProgress(message = paste0("Bootstrap validation (B = ", boot_B, ")..."), value = 0.1, {
      boot_validate <- tryCatch(
        withr::with_seed(run_seed, rms::validate(nomogram_model, method = "boot", B = boot_B)),
        error = function(e) {
          showNotification(paste0("Bootstrap optimism correction failed: ", conditionMessage(e),
                                  " The rest of the results are still shown."), type = "warning", duration = 12)
          NULL
        }
      )
      incProgress(0.45)
      cal_train <- tryCatch(
        withr::with_seed(run_seed, rms::calibrate(nomogram_model, method = "boot", B = boot_B)),
        error = function(e) NULL
      )
      incProgress(0.45)
    })
    optimism_summary <- NULL
    boot_row <- function(row, label, to_c = FALSE) {
      if (is.null(boot_validate) || !row %in% rownames(boot_validate)) return(NULL)
      orig <- as.numeric(boot_validate[row, "index.orig"])
      corr <- as.numeric(boot_validate[row, "index.corrected"])
      if (to_c) { orig <- 0.5 + orig / 2; corr <- 0.5 + corr / 2 }  # Dxy -> C-index
      data.frame(
        Metric = label, Apparent = orig, Mean_Optimism = orig - corr, Bootstrap_Corrected = corr,
        B_Requested = boot_B, B_Successful = as.integer(boot_validate[row, "n"]), Seed = run_seed,
        stringsAsFactors = FALSE
      )
    }
    optimism_summary <- do.call(rbind, Filter(Negate(is.null), list(
      boot_row("Dxy", "C-index (AUC), training", to_c = TRUE),
      boot_row("Slope", "Calibration slope, training"),
      boot_row("B", "Brier score, training")
    )))
    if (!is.null(optimism_summary) && nrow(optimism_summary) > 0 && optimism_summary$Mean_Optimism[1] > 0.05) {
      showNotification(
        tags$div(
          icon("exclamation-triangle"),
          tags$strong(" Meaningful overfitting detected: "),
          paste0("apparent training C-index ", round(optimism_summary$Apparent[1], 3), " drops to ",
                 round(optimism_summary$Bootstrap_Corrected[1], 3), " after bootstrap optimism correction (B=", boot_B, ")."),
          tags$br(),
          tags$small("The apparent value overstates how well this panel will generalize; use the bootstrap-corrected value as the honest training estimate.")
        ),
        type = "warning", duration = 15
      )
    }

    # Calibration statistics (intercept, slope, Brier) - computed only where the
    # sample size permits; otherwise reported as "not estimated", never invented.
    cal_stats <- rbind(
      gexp_diag_calibration(train_data$Outcome, train_data$Predicted_Prob, "Training (apparent)"),
      gexp_diag_calibration(validation_data$Outcome, validation_data$Predicted_Prob, val_dataset_label)
    )
    if (!is.null(boot_validate) && all(c("Intercept", "Slope", "B") %in% rownames(boot_validate))) {
      cal_stats <- rbind(cal_stats[1, , drop = FALSE], data.frame(
        Dataset = "Training (bootstrap-corrected)", N = nrow(train_data),
        N_Disease = sum(train_data$Outcome == 1), N_Normal = sum(train_data$Outcome == 0),
        Intercept = as.numeric(boot_validate["Intercept", "index.corrected"]),
        Intercept_Lower = NA_real_, Intercept_Upper = NA_real_,
        Slope = as.numeric(boot_validate["Slope", "index.corrected"]),
        Slope_Lower = NA_real_, Slope_Upper = NA_real_,
        Brier = as.numeric(boot_validate["B", "index.corrected"]),
        Intercept_Joint = NA_real_,
        Note = paste0("Optimism-corrected (rms::validate, ", as.integer(boot_validate["Slope", "n"]),
                      " successful resamples); CIs not estimated."),
        stringsAsFactors = FALSE
      ), cal_stats[2, , drop = FALSE])
    }
    rownames(cal_stats) <- NULL

    confusion_tbl <- rbind(gexp_diag_confusion_table(train_metrics), gexp_diag_confusion_table(val_metrics))

    # Methods / run settings recorded with the results (also downloadable)
    run_settings <- data.frame(
      Setting = c(
        "Validation mode", "Outcome column", "Outcome coding", "Predictors (n)", "Predictors",
        "Model fit", "Classification threshold", "Validation predictions", "Predictor standardization",
        "Disease prevalence (training / validation)", "Training-data checks",
        "Bootstrap repetitions requested", "Bootstrap repetitions successful", "Random seed",
        "Confidence intervals", "N training", "N validation", "GExPipe version", "Run time"
      ),
      Value = c(
        val_dataset_label, outcome_coding$group_col,
        paste0("1 = Disease ('", outcome_coding$disease_label, "'); 0 = Normal ('", outcome_coding$normal_label, "'). ", outcome_coding$rule),
        length(available_genes), paste(available_genes, collapse = ", "),
        if (ridge_applied) "Ridge-penalized logistic (rms::lrm, penalty = 2) after separation was detected" else "Logistic regression, maximum likelihood (rms::lrm)",
        paste0("Youden index on the TRAINING ROC = ", signif(optimal_threshold, 5), " (applied unchanged to validation)"),
        "Training intercept + coefficients applied directly; no refit and no recalibration on validation data",
        std_label,
        sprintf("%.1f%% / %.1f%%", 100 * prev_tr, 100 * prev_va),
        if (length(qc_notes) == 0L) "No problems flagged" else paste(qc_notes, collapse = " | "),
        boot_B, if (is.null(boot_validate)) "not available" else as.integer(boot_validate["Dxy", "n"]), run_seed,
        gexp_diag_ci_methods(), nrow(train_data), nrow(validation_data),
        as.character(utils::packageVersion("GExPipe")), format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      stringsAsFactors = FALSE
    )

    validation_data$Pred_Decile <- dplyr::ntile(validation_data$Predicted_Prob, min(10, nrow(validation_data) %/% 2))
    cal_validation <- validation_data %>%
      dplyr::group_by(.data$Pred_Decile) %>%
      dplyr::summarise(Predicted = mean(.data$Predicted_Prob), Observed = mean(.data$Outcome), N = dplyr::n(),
        SE = sqrt(mean(.data$Outcome) * (1 - mean(.data$Outcome)) / dplyr::n()), .groups = "drop") %>%
      dplyr::filter(!is.na(.data$Predicted) & .data$N >= 2)

    dca_thresholds <- seq(0.01, 0.99, by = 0.02)
    # Decision-curve analysis uses Suggests package dcurves only.
    dca_engine <- if (requireNamespace("dcurves", quietly = TRUE)) {
      "dcurves"
    } else {
      NA_character_
    }

    compute_dca <- function(dat) {
      if (identical(dca_engine, "dcurves")) {
        tryCatch(
          dcurves::dca(Outcome ~ Predicted_Prob, data = dat, thresholds = dca_thresholds),
          error = function(e) NULL)
      } else {
        NULL
      }
    }

    dca_train <- compute_dca(train_data)
    dca_val   <- compute_dca(validation_data)

    thresholds <- seq(0.01, 0.99, by = 0.02)
    n_train <- nrow(train_data)
    n_val <- nrow(validation_data)
    ci_train <- data.frame(
      threshold = thresholds,
      high_risk = as.numeric(colSums(outer(train_data$Predicted_Prob, thresholds, ">=")) / n_train * 1000),
      high_risk_with_outcome = vapply(thresholds, function(t) sum(train_data$Outcome[train_data$Predicted_Prob >= t]) / n_train * 1000, 0)
    )
    ci_val <- data.frame(
      threshold = thresholds,
      high_risk = as.numeric(colSums(outer(validation_data$Predicted_Prob, thresholds, ">=")) / n_val * 1000),
      high_risk_with_outcome = vapply(thresholds, function(t) sum(validation_data$Outcome[validation_data$Predicted_Prob >= t]) / n_val * 1000, 0)
    )

    rv$nomogram_model <- nomogram_model
    rv$nomogram_train_data <- train_data
    rv$nomogram_validation_data <- validation_data
    rv$nomogram_available_genes <- available_genes
    rv$nomogram_optimal_threshold <- optimal_threshold
    rv$nomogram_train_metrics <- train_metrics
    rv$nomogram_val_metrics <- val_metrics
    rv$nomogram_train_roc <- train_roc
    rv$nomogram_val_roc <- pROC::roc(validation_data$Outcome, validation_data$Predicted_Prob, levels = c(0, 1), direction = "<", quiet = TRUE)
    rv$nomogram_model_diagnostics <- model_diagnostics
    rv$nomogram_ridge_applied <- ridge_applied
    rv$nomogram_performance_comparison <- performance_comparison
    rv$nomogram_cal_train <- cal_train
    rv$nomogram_cal_validation <- cal_validation
    rv$nomogram_optimism_summary <- optimism_summary
    rv$nomogram_outcome_coding <- outcome_coding$table
    rv$nomogram_calibration_stats <- cal_stats
    rv$nomogram_confusion_table <- confusion_tbl
    rv$nomogram_run_settings <- run_settings
    rv$nomogram_boot_B <- boot_B
    rv$nomogram_seed <- run_seed
    rv$nomogram_dca_train <- dca_train
    rv$nomogram_dca_val <- dca_val
    rv$nomogram_dca_engine <- dca_engine
    rv$nomogram_ci_train <- ci_train
    rv$nomogram_ci_val <- ci_val
    rv$nomogram_complete <- TRUE

    # Save the new figures and tables to the existing export folder (300 DPI PNG + PDF)
    nomogram_export_all()

    val_label <- if (mode == "external") "External" else "Internal (30%)"
    msg <- paste0("Nomogram analysis complete. ",
                  "Training: ", n_train, " samples. ",
                  val_label, " Validation: ", n_val, " samples.")
    showNotification(msg, type = "message", duration = 5)
  })

  # ============================================================================
  # PLOTS
  # ============================================================================
  output$nomogram_plot_panel_a <- renderPlot({
    req(rv$nomogram_model, rv$nomogram_available_genes)
    dd <- rms::datadist(rv$nomogram_train_data[, rv$nomogram_available_genes, drop = FALSE])
    options(datadist = dd)
    on.exit(options(datadist = NULL), add = TRUE)
    np <- rms::nomogram(rv$nomogram_model, fun = plogis, fun.at = c(0.001, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.999), funlabel = "Risk of Disease", lp = FALSE)
    plot(np)
    title(main = "Diagnostic Nomogram", cex.main = 1.3, font.main = 2)
  }, height = 500)

  output$nomogram_plot_roc_train <- renderPlot({ plot_nomogram_roc_train() }, height = 440)
  output$nomogram_plot_roc_val <- renderPlot({ plot_nomogram_roc_val() }, height = 440)
  output$nomogram_plot_roc_combined <- renderPlot({ plot_nomogram_roc_combined() }, height = 480)
  output$nomogram_plot_cal_train <- renderPlot({ plot_nomogram_cal_train() }, height = 440)
  output$nomogram_plot_cal_val <- renderPlot({ plot_nomogram_cal_val() }, height = 440)
  output$nomogram_plot_conf_train <- renderPlot({ plot_nomogram_conf_train() }, height = 380)
  output$nomogram_plot_conf_val <- renderPlot({ plot_nomogram_conf_val() }, height = 380)

  render_dca <- function(dca_obj, model_col, title = NULL) {
    p <- plot(dca_obj, smooth = FALSE)
    p <- p + ggplot2::scale_color_manual(values = c("grey60", model_col, "grey60")) +
      ggplot2::theme(legend.position = c(0.99, 0.99), legend.justification = c(1, 1))
    if (!is.null(title)) {
      p <- p + ggplot2::labs(title = title) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
    }
    p
  }

  output$nomogram_plot_dca_train <- renderPlot({
    req(rv$nomogram_dca_train)
    render_dca(rv$nomogram_dca_train, "#E74C3C", "Training DCA")
  }, height = 320)

  output$nomogram_plot_dca_val <- renderPlot({
    req(rv$nomogram_dca_val)
    mode <- rv$validation_mode
    if (is.null(mode)) mode <- "internal"
    col <- if (mode == "external") "#27AE60" else "#3498DB"
    render_dca(rv$nomogram_dca_val, col, if (mode == "external") "External Validation DCA" else "Validation DCA")
  }, height = 320)

  output$nomogram_plot_impact_train <- renderPlot({
    req(rv$nomogram_ci_train)
    ci <- rv$nomogram_ci_train
    plot(ci$threshold, ci$high_risk, type = "l", lwd = 2, col = "#E74C3C", xlab = "Threshold", ylab = "Per 1000", main = "Training Clinical Impact", ylim = c(0, max(ci$high_risk) * 1.1))
    lines(ci$threshold, ci$high_risk_with_outcome, lwd = 2, col = "#E74C3C", lty = 2)
    legend("topright", legend = c("Classified High Risk", "High Risk with Outcome"), col = "#E74C3C", lty = c(1, 2), lwd = 2, bty = "n")
  }, height = 320)

  output$nomogram_plot_impact_val <- renderPlot({
    req(rv$nomogram_ci_val)
    mode <- rv$validation_mode
    if (is.null(mode)) mode <- "internal"
    col <- if (mode == "external") "#27AE60" else "#3498DB"
    label <- if (mode == "external") "External Validation Clinical Impact" else "Validation Clinical Impact"
    ci <- rv$nomogram_ci_val
    plot(ci$threshold, ci$high_risk, type = "l", lwd = 2, col = col, xlab = "Threshold", ylab = "Per 1000", main = label, ylim = c(0, max(ci$high_risk) * 1.1))
    lines(ci$threshold, ci$high_risk_with_outcome, lwd = 2, col = col, lty = 2)
    legend("topright", legend = c("Classified High Risk", "High Risk with Outcome"), col = col, lty = c(1, 2), lwd = 2, bty = "n")
  }, height = 320)

  output$nomogram_diagnostics_note_ui <- renderUI({
    # When quasi-complete separation was detected, "Coefficient/Std_Error/OR"
    # is ALREADY the ridge-penalized refit (see the fit block above) - NOT
    # the raw, exploding MLE. Firth is a separate, unpenalized-by-ridge fit,
    # so in that specific case Firth is the LESS corrected of the two and
    # can show a more extreme OR than the already-fixed standard columns
    # (e.g. a small ridge penalty already tamed the standard OR, while
    # Firth still reflects how extreme the raw separation was). Saying
    # "trust Firth when they disagree" would be backwards here, so the
    # note only makes that claim when ridge did NOT already fire.
    if (isTRUE(rv$nomogram_ridge_applied)) {
      tags$p(
        style = "font-size: 12px; color: #666;",
        tags$strong("Note: "),
        "quasi-complete separation was detected for this panel, so the Coefficient/Std_Error/OR columns already ",
        "come from a ridge-penalized refit (not the raw, unstable fit). The _Firth columns are a SEPARATE fit ",
        "without that ridge penalty, so they can look more extreme here - they show how unstable the raw ",
        "coefficients were, not a more trustworthy correction of the standard columns in this case."
      )
    } else {
      tags$p(
        style = "font-size: 12px; color: #666;",
        "Coefficient/Std_Error/OR come from the standard model. When a gene near-perfectly separates ",
        "the two groups, those can blow up to unrealistic values (huge SE, extreme OR) - the ",
        "_Firth columns are Firth's bias-reduced estimates, which stay finite and realistic under ",
        "separation; trust those when the two disagree."
      )
    }
  })

  output$nomogram_diagnostics_table <- DT::renderDataTable({
    req(rv$nomogram_model_diagnostics)
    df <- rv$nomogram_model_diagnostics
    for (j in names(df)) if (is.numeric(df[[j]])) df[[j]] <- signif(df[[j]], 4)
    DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  output$nomogram_performance_table <- DT::renderDataTable({
    req(rv$nomogram_performance_comparison)
    df <- rv$nomogram_performance_comparison
    count_cols <- c("N_Total", "N_Disease", "N_Normal", "TP", "TN", "FP", "FN")
    for (j in names(df)) if (is.numeric(df[[j]]) && !j %in% count_cols) df[[j]] <- round(df[[j]], 4)
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$nomogram_outcome_coding_ui <- renderUI({
    req(rv$nomogram_outcome_coding)
    oc <- rv$nomogram_outcome_coding
    tags$div(
      class = "alert alert-info", style = "font-size: 13px; margin-bottom: 10px;",
      icon("exchange-alt"), tags$strong(" Outcome coding: "),
      paste0("1 = Disease ('", oc$Source_Label[1], "', n = ", oc$N[1], "); 0 = Normal ('", oc$Source_Label[2], "', n = ", oc$N[2], "). "),
      tags$small(oc$Rule[1], " Source column: ", oc$Source_Column[1], ". External validation labels use the same coding (Normal = 0, Disease = 1).")
    )
  })

  output$nomogram_calibration_table <- DT::renderDataTable({
    req(rv$nomogram_calibration_stats)
    df <- rv$nomogram_calibration_stats
    for (j in names(df)) if (is.numeric(df[[j]]) && !j %in% c("N", "N_Disease", "N_Normal")) df[[j]] <- round(df[[j]], 4)
    DT::datatable(df, options = list(pageLength = 5, scrollX = TRUE, dom = "t"), rownames = FALSE)
  })

  output$nomogram_confusion_table <- DT::renderDataTable({
    req(rv$nomogram_confusion_table)
    DT::datatable(rv$nomogram_confusion_table, options = list(pageLength = 8, scrollX = TRUE, dom = "t"), rownames = FALSE)
  })

  output$nomogram_settings_table <- DT::renderDataTable({
    req(rv$nomogram_run_settings)
    DT::datatable(rv$nomogram_run_settings, options = list(pageLength = 20, scrollX = TRUE, dom = "t"), rownames = FALSE)
  })

  output$nomogram_optimism_note_ui <- renderUI({
    b <- rv$nomogram_boot_B
    tags$p(
      style = "font-size: 12px; color: #666;",
      "The apparent training C-index is measured on the same data the model was fit on and is always optimistic. ",
      "This refits the model on ", if (is.null(b)) "B" else b, " bootstrap resamples of the training data (seed ",
      if (is.null(rv$nomogram_seed)) "-" else rv$nomogram_seed,
      ") and averages how much each resample's performance drops when applied back to the original data. ",
      "Mean_Optimism = Apparent - Bootstrap_Corrected (rms convention, so for the Brier score a negative value means the apparent score was too good). ",
      "Bootstrap_Corrected is the more honest estimate of performance on new samples."
    )
  })

  output$nomogram_optimism_table <- DT::renderDataTable({
    req(rv$nomogram_optimism_summary)
    DT::datatable(rv$nomogram_optimism_summary, options = list(dom = "t"), rownames = FALSE)
  })
  output$nomogram_optimism_available <- reactive({ !is.null(rv$nomogram_optimism_summary) })
  outputOptions(output, "nomogram_optimism_available", suspendWhenHidden = FALSE)

  # ============================================================================
  # DOWNLOAD HANDLERS
  # ============================================================================
  nomogram_plot_to_file <- function(file, dev_open, dev_close = function() dev.off()) {
    req(rv$nomogram_model, rv$nomogram_available_genes)
    dd <- rms::datadist(rv$nomogram_train_data[, rv$nomogram_available_genes, drop = FALSE])
    options(datadist = dd)
    on.exit(options(datadist = NULL), add = TRUE)
    dev_open()
    np <- rms::nomogram(rv$nomogram_model, fun = plogis, fun.at = c(0.001, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.999), funlabel = "Risk of Disease", lp = FALSE)
    plot(np)
    title(main = "Diagnostic Nomogram", cex.main = 1.5, font.main = 2)
    dev_close()
  }

  output$download_nomogram_panel_a <- downloadHandler(
    filename = function() "Nomogram_Panel_A.png",
    content = function(file) {
      nomogram_plot_to_file(file,
        dev_open = function() png(file, width = 10 * IMAGE_DPI, height = 7.5 * IMAGE_DPI, res = IMAGE_DPI, bg = "white"))
    }
  )
  output$download_nomogram_panel_a_jpg <- downloadHandler(
    filename = function() "Nomogram_Panel_A.jpg",
    content = function(file) {
      nomogram_plot_to_file(file,
        dev_open = function() jpeg(file, width = 10, height = 7.5, res = IMAGE_DPI, units = "in", bg = "white", quality = 95))
    }
  )
  output$download_nomogram_panel_a_pdf <- downloadHandler(
    filename = function() "Nomogram_Panel_A.pdf",
    content = function(file) {
      nomogram_plot_to_file(file,
        dev_open = function() pdf(file, width = 10, height = 7.5, bg = "white"))
    }
  )

  # One CSV download per result table (also copied to the existing export folder)
  nomo_csv_downloads <- c(
    download_nomogram_coefficients = "Nomogram_Model_Coefficients",
    download_nomogram_firth = "Nomogram_Firth_Coefficients",
    download_nomogram_optimism = "Nomogram_Bootstrap_Optimism_Correction",
    download_nomogram_perf_training = "Nomogram_Performance_Training",
    download_nomogram_perf_validation = "Nomogram_Performance_Validation",
    download_nomogram_performance = "Nomogram_Performance_Comparison",
    download_nomogram_calibration = "Nomogram_Calibration_Statistics",
    download_nomogram_confusion = "Nomogram_Confusion_Matrices",
    download_nomogram_diagnostics = "Nomogram_Model_Diagnostics",
    download_nomogram_settings = "Nomogram_Outcome_Coding_and_Settings"
  )
  for (out_id in names(nomo_csv_downloads)) {
    local({
      id <- out_id
      nm <- nomo_csv_downloads[[id]]
      output[[id]] <- downloadHandler(
        filename = function() paste0(nm, ".csv"),
        content = function(file) {
          req(rv$nomogram_complete)
          tab <- nomogram_result_tables()[[nm]]
          if (is.null(tab)) tab <- data.frame(Note = "Not available for this run.")
          utils::write.csv(tab, file, row.names = FALSE)
          try(utils::write.csv(tab, file.path(CSV_EXPORT_DIR(), paste0(nm, ".csv")), row.names = FALSE), silent = TRUE)
        }
      )
    })
  }

  nomogram_panel_to_file <- function(file, plotfun, width, height) {
    ext <- tolower(sub(".*\\.", "", basename(file)))
    if (ext == "pdf") {
      pdf(file, width = width, height = height, bg = "white")
    } else if (ext %in% c("jpg", "jpeg")) {
      jpeg(file, width = width, height = height, res = IMAGE_DPI, units = "in", bg = "white", quality = 95)
    } else {
      png(file, width = width * IMAGE_DPI, height = height * IMAGE_DPI, res = IMAGE_DPI, bg = "white")
    }
    on.exit(dev.off(), add = TRUE)
    plotfun()
  }

  nomo_train_col <- "#C0392B"
  nomo_val_col <- function() if (identical(rv$validation_mode, "external")) "#1E8449" else "#2471A3"
  nomo_val_label <- function() if (identical(rv$validation_mode, "external")) "External validation" else "Internal validation (30%)"

  plot_nomogram_roc_train <- function() {
    req(rv$nomogram_train_roc, rv$nomogram_train_metrics)
    gexp_diag_plot_roc(list(rv$nomogram_train_roc), list(rv$nomogram_train_metrics),
                       "Training", nomo_train_col, "Training ROC curve")
  }
  plot_nomogram_roc_val <- function() {
    req(rv$nomogram_val_roc, rv$nomogram_val_metrics)
    gexp_diag_plot_roc(list(rv$nomogram_val_roc), list(rv$nomogram_val_metrics),
                       nomo_val_label(), nomo_val_col(), paste0(nomo_val_label(), " ROC curve"))
  }
  plot_nomogram_roc_combined <- function() {
    req(rv$nomogram_train_roc, rv$nomogram_val_roc, rv$nomogram_train_metrics, rv$nomogram_val_metrics)
    gexp_diag_plot_roc(
      list(rv$nomogram_train_roc, rv$nomogram_val_roc),
      list(rv$nomogram_train_metrics, rv$nomogram_val_metrics),
      c("Training", nomo_val_label()), c(nomo_train_col, nomo_val_col()),
      "Training vs validation ROC")
  }
  plot_nomogram_cal_train <- function() {
    req(rv$nomogram_train_data, rv$nomogram_calibration_stats)
    cs <- rv$nomogram_calibration_stats
    row <- cs[cs$Dataset == "Training (apparent)", , drop = FALSE]
    corr <- cs[cs$Dataset == "Training (bootstrap-corrected)", , drop = FALSE]
    extra <- if (nrow(corr) > 0 && is.finite(corr$Slope)) {
      c(sprintf("Corrected slope %.2f, Brier %.3f", corr$Slope, corr$Brier), "(dotted = bias-corrected)")
    } else NULL
    curve <- tryCatch({
      m <- as.data.frame(unclass(rv$nomogram_cal_train))
      cv <- data.frame(m$predy, m$calibrated.corrected)
      cv[stats::complete.cases(cv), , drop = FALSE]
    }, error = function(e) NULL)
    gexp_diag_plot_calibration(rv$nomogram_train_data$Predicted_Prob, rv$nomogram_train_data$Outcome,
                               row, nomo_train_col, "Training calibration (apparent)", curve = curve, extra = extra)
  }
  plot_nomogram_cal_val <- function() {
    req(rv$nomogram_validation_data, rv$nomogram_calibration_stats)
    cs <- rv$nomogram_calibration_stats
    row <- cs[!grepl("^Training", cs$Dataset), , drop = FALSE][1, , drop = FALSE]
    gexp_diag_plot_calibration(rv$nomogram_validation_data$Predicted_Prob, rv$nomogram_validation_data$Outcome,
                               row, nomo_val_col(), paste0(nomo_val_label(), " calibration"))
  }
  plot_nomogram_conf_train <- function() {
    req(rv$nomogram_train_metrics)
    gexp_diag_plot_confusion(rv$nomogram_train_metrics, "Training: confusion matrix", nomo_train_col)
  }
  plot_nomogram_conf_val <- function() {
    req(rv$nomogram_val_metrics)
    gexp_diag_plot_confusion(rv$nomogram_val_metrics, paste0(nomo_val_label(), ": confusion matrix"), nomo_val_col())
  }
  plot_nomogram_dca_train <- function() {
    req(rv$nomogram_dca_train)
    render_dca(rv$nomogram_dca_train, "#E74C3C", "Training DCA")
  }
  plot_nomogram_dca_val <- function() {
    req(rv$nomogram_dca_val)
    mode <- rv$validation_mode; if (is.null(mode)) mode <- "internal"
    col <- if (mode == "external") "#27AE60" else "#3498DB"
    render_dca(rv$nomogram_dca_val, col, if (mode == "external") "External Validation DCA" else "Validation DCA")
  }
  plot_nomogram_impact_train <- function() {
    req(rv$nomogram_ci_train)
    ci <- rv$nomogram_ci_train
    plot(ci$threshold, ci$high_risk, type = "l", lwd = 2, col = "#E74C3C", xlab = "Threshold", ylab = "Per 1000", main = "Training Clinical Impact", ylim = c(0, max(ci$high_risk) * 1.1))
    lines(ci$threshold, ci$high_risk_with_outcome, lwd = 2, col = "#E74C3C", lty = 2)
    legend("topright", legend = c("Classified High Risk", "High Risk with Outcome"), col = "#E74C3C", lty = c(1, 2), lwd = 2, bty = "n")
  }
  plot_nomogram_impact_val <- function() {
    req(rv$nomogram_ci_val)
    mode <- rv$validation_mode; if (is.null(mode)) mode <- "internal"
    col <- if (mode == "external") "#27AE60" else "#3498DB"
    label <- if (mode == "external") "External Validation Clinical Impact" else "Validation Clinical Impact"
    ci <- rv$nomogram_ci_val
    plot(ci$threshold, ci$high_risk, type = "l", lwd = 2, col = col, xlab = "Threshold", ylab = "Per 1000", main = label, ylim = c(0, max(ci$high_risk) * 1.1))
    lines(ci$threshold, ci$high_risk_with_outcome, lwd = 2, col = col, lty = 2)
    legend("topright", legend = c("Classified High Risk", "High Risk with Outcome"), col = col, lty = c(1, 2), lwd = 2, bty = "n")
  }

  add_nomogram_panel_downloads <- function(stem, plotfun, width, height, prefix) {
    output[[paste0("download_nomogram_", stem, "_png")]] <- downloadHandler(
      filename = function() paste0(prefix, ".png"),
      content = function(file) nomogram_panel_to_file(file, plotfun, width, height)
    )
    output[[paste0("download_nomogram_", stem, "_jpg")]] <- downloadHandler(
      filename = function() paste0(prefix, ".jpg"),
      content = function(file) nomogram_panel_to_file(file, plotfun, width, height)
    )
    output[[paste0("download_nomogram_", stem, "_pdf")]] <- downloadHandler(
      filename = function() paste0(prefix, ".pdf"),
      content = function(file) nomogram_panel_to_file(file, plotfun, width, height)
    )
  }

  add_nomogram_panel_downloads("roc_train", plot_nomogram_roc_train, 6.5, 6.5, "Nomogram_ROC_Training")
  add_nomogram_panel_downloads("roc_val", plot_nomogram_roc_val, 6.5, 6.5, "Nomogram_ROC_Validation")
  add_nomogram_panel_downloads("roc_combined", plot_nomogram_roc_combined, 6.5, 6.5, "Nomogram_ROC_Training_vs_Validation")
  add_nomogram_panel_downloads("cal_train", plot_nomogram_cal_train, 6.5, 6.5, "Nomogram_Calibration_Training")
  add_nomogram_panel_downloads("cal_val", plot_nomogram_cal_val, 6.5, 6.5, "Nomogram_Calibration_Validation")
  add_nomogram_panel_downloads("conf_train", plot_nomogram_conf_train, 6.5, 5.5, "Nomogram_ConfusionMatrix_Training")
  add_nomogram_panel_downloads("conf_val", plot_nomogram_conf_val, 6.5, 5.5, "Nomogram_ConfusionMatrix_Validation")
  add_nomogram_panel_downloads("dca_train", plot_nomogram_dca_train, 7, 4.5, "Nomogram_DCA_Training")
  add_nomogram_panel_downloads("dca_val", plot_nomogram_dca_val, 7, 4.5, "Nomogram_DCA_Validation")
  add_nomogram_panel_downloads("impact_train", plot_nomogram_impact_train, 7, 4.5, "Nomogram_Impact_Training")
  add_nomogram_panel_downloads("impact_val", plot_nomogram_impact_val, 7, 4.5, "Nomogram_Impact_Validation")
}
