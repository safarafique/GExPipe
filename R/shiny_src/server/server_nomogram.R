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
    # Priority: user-selected genes from ROC > ML common genes > DEG+WGCNA common genes
    common_genes <- rv$roc_selected_genes
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
    grp <- sample_info[[group_col]]
    grp <- as.character(trimws(grp))
    grp[grp == ""] <- NA
    u <- unique(grp[!is.na(grp)])
    if (length(u) < 2) {
      msg <- if (length(u) == 0) paste0("No group values in column '", group_col, "'.")
      else paste0("Only one group found: '", u[1], "'. Need exactly two groups.")
      showNotification(msg, type = "error", duration = 8)
      return()
    }
    if (length(u) > 2) {
      showNotification(paste0("Column '", group_col, "' has ", length(u), " values. Nomogram needs exactly two."), type = "error", duration = 10)
      return()
    }
    disease_like <- grepl("disease|case|asd|treatment|2|tumor|patient", u, ignore.case = TRUE)
    normal_like  <- grepl("normal|control|healthy|1|non|ctrl", u, ignore.case = TRUE)
    if (sum(disease_like) >= 1 && sum(normal_like) >= 1) {
      disease_val <- u[which(disease_like)[1]]
      outcome <- as.integer(grp == disease_val)
    } else {
      outcome <- as.integer(factor(grp, levels = u)) - 1L
    }
    valid <- !is.na(outcome)
    if (sum(valid) < 10) {
      showNotification("Too many missing group values.", type = "error", duration = 6)
      return()
    }
    expr_nomogram <- expr_nomogram[valid, , drop = FALSE]
    sample_info   <- sample_info[valid, , drop = FALSE]
    outcome       <- outcome[valid]
    min_events <- min(sum(outcome == 1), sum(outcome == 0))
    epv <- min_events / length(available_genes)
    if (epv < 10) {
      max_predictors <- max(3, floor(min_events / 10))
      gene_var <- apply(expr_mat[available_genes, ], 1, var, na.rm = TRUE)
      available_genes <- names(sort(gene_var, decreasing = TRUE))[seq_len(min(max_predictors, length(available_genes)))]
      epv <- min_events / length(available_genes)
    } else if (length(available_genes) > 15) {
      gene_var <- apply(expr_mat[available_genes, ], 1, var, na.rm = TRUE)
      available_genes <- names(sort(gene_var, decreasing = TRUE))[1:15]
      epv <- min_events / length(available_genes)
    }
    if (sum(outcome == 1) < 10 || sum(outcome == 0) < 10) {
      showNotification("Need at least 10 samples per group.", type = "error", duration = 6)
      return()
    }

    nomogram_data <- cbind(expr_nomogram[, available_genes, drop = FALSE], Outcome = outcome)
    nomogram_data$SampleID <- rownames(nomogram_data)

    # ==================================================================
    # Split based on validation mode
    # ==================================================================
    if (mode == "external") {
      # EXTERNAL MODE: train on ALL data, validate on external dataset
      train_data <- nomogram_data

      # Build external validation data.frame
      ext_expr <- rv$external_validation_expr
      ext_outcome <- rv$external_validation_outcome
      ext_genes_available <- intersect(available_genes, colnames(ext_expr))

      if (length(ext_genes_available) < 1) {
        showNotification(
          tags$div(icon("exclamation-triangle"),
                   tags$strong(" No overlapping genes between model predictors and external dataset."),
                   tags$br(),
                   tags$small(paste0("Model genes: ", paste(head(available_genes, 5), collapse = ", "),
                                     " ... External genes (first 5): ", paste(head(colnames(ext_expr), 5), collapse = ", ")))),
          type = "error", duration = 10)
        return()
      }

      ext_df <- as.data.frame(ext_expr[, ext_genes_available, drop = FALSE])
      missing_genes <- setdiff(available_genes, ext_genes_available)
      if (length(missing_genes) > 0) {
        for (mg in missing_genes) ext_df[[mg]] <- 0
        showNotification(
          tags$div(icon("info-circle"),
                   paste0(length(missing_genes), " model gene(s) not found in external dataset -- imputed with 0.")),
          type = "warning", duration = 8)
      }
      ext_df <- ext_df[, available_genes, drop = FALSE]
      ext_df$Outcome <- ext_outcome
      ext_df$SampleID <- paste0("ExtS", seq_len(nrow(ext_df)))
      validation_data <- ext_df

    } else {
      # INTERNAL MODE: 70/30 stratified split
      set.seed(123)
      train_idx <- caret::createDataPartition(nomogram_data$Outcome, p = 0.7, list = FALSE)
      train_data <- nomogram_data[train_idx, ]
      validation_data <- nomogram_data[-train_idx, ]
    }

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

    train_data$Predicted_Prob <- predict(nomogram_model, newdata = train_data, type = "fitted")

    # Validation predictions: use same (training) datadist so the model is applied consistently to new data
    validation_data$Predicted_Prob <- tryCatch(
      predict(nomogram_model, newdata = validation_data, type = "fitted"),
      error = function(e) {
        glm_model <- glm(formula_obj, data = train_data, family = binomial())
        predict(glm_model, newdata = validation_data, type = "response")
      }
    )

    train_roc <- pROC::roc(train_data$Outcome, train_data$Predicted_Prob, quiet = TRUE)
    optimal_threshold <- as.numeric(pROC::coords(train_roc, "best", ret = "threshold", best.method = "youden"))
    if (!is.finite(optimal_threshold)) optimal_threshold <- 0.5
    train_data$Predicted_Class <- ifelse(train_data$Predicted_Prob > optimal_threshold, 1, 0)
    validation_data$Predicted_Class <- ifelse(validation_data$Predicted_Prob > optimal_threshold, 1, 0)

    glm_fit <- glm(formula_obj, data = train_data, family = binomial())
    vif_vals <- tryCatch(car::vif(glm_fit), error = function(e) setNames(rep(NA, length(available_genes)), available_genes))
    if (length(vif_vals) != length(available_genes)) vif_vals <- setNames(rep(NA, length(available_genes)), available_genes)
    coefs <- coef(nomogram_model)[-1]
    se <- sqrt(diag(vcov(nomogram_model)))[-1]
    model_diagnostics <- data.frame(
      Predictor = available_genes,
      Coefficient = coefs,
      Std_Error = se,
      OR = exp(coefs),
      VIF = as.numeric(vif_vals[available_genes]),
      stringsAsFactors = FALSE
    )
    model_diagnostics$VIF_Status <- ifelse(is.na(model_diagnostics$VIF), "Unknown",
      ifelse(model_diagnostics$VIF > 10, "High (>10)", ifelse(model_diagnostics$VIF > 5, "Moderate (5-10)", "Low (<5)")))

    calc_metrics <- function(actual, pred_prob, pred_class, thresh) {
      cm <- caret::confusionMatrix(factor(pred_class, levels = c(0, 1)), factor(actual, levels = c(0, 1)))
      roc_obj <- pROC::roc(actual, pred_prob, quiet = TRUE, ci = TRUE)
      auc_ci <- pROC::ci(roc_obj, of = "auc")
      data.frame(
        N_Total = length(actual), N_Disease = sum(actual == 1), N_Normal = sum(actual == 0),
        Accuracy = as.numeric(cm$overall["Accuracy"]),
        Sensitivity = as.numeric(cm$byClass["Sensitivity"]),
        Specificity = as.numeric(cm$byClass["Specificity"]),
        PPV = as.numeric(cm$byClass["Pos Pred Value"]),
        NPV = as.numeric(cm$byClass["Neg Pred Value"]),
        AUC = as.numeric(pROC::auc(roc_obj)),
        AUC_Lower = as.numeric(auc_ci)[1], AUC_Upper = as.numeric(auc_ci)[3],
        Threshold = thresh,
        stringsAsFactors = FALSE
      )
    }
    train_metrics <- calc_metrics(train_data$Outcome, train_data$Predicted_Prob, train_data$Predicted_Class, optimal_threshold)
    val_metrics <- calc_metrics(validation_data$Outcome, validation_data$Predicted_Prob, validation_data$Predicted_Class, optimal_threshold)
    train_metrics$Dataset <- "Training"
    val_metrics$Dataset <- if (mode == "external") "External Validation" else "Internal Validation (30%)"
    performance_comparison <- rbind(train_metrics, val_metrics)

    cal_train <- tryCatch(rms::calibrate(nomogram_model, method = "boot", B = 200), error = function(e) NULL)
    validation_data$Pred_Decile <- dplyr::ntile(validation_data$Predicted_Prob, min(10, nrow(validation_data) %/% 2))
    cal_validation <- validation_data %>%
      dplyr::group_by(.data$Pred_Decile) %>%
      dplyr::summarise(Predicted = mean(.data$Predicted_Prob), Observed = mean(.data$Outcome), N = dplyr::n(),
        SE = sqrt(mean(.data$Outcome) * (1 - mean(.data$Outcome)) / dplyr::n()), .groups = "drop") %>%
      dplyr::filter(!is.na(.data$Predicted) & .data$N >= 2)

    dca_train <- tryCatch({
      rmda::decision_curve(Outcome ~ Predicted_Prob, data = train_data, fitted.risk = TRUE, thresholds = seq(0.01, 0.99, by = 0.02), bootstraps = 25)
    }, error = function(e) tryCatch({
      rmda::decision_curve(Outcome ~ Predicted_Prob, data = train_data, fitted.risk = TRUE, thresholds = seq(0.01, 0.99, by = 0.02), bootstraps = 0)
    }, error = function(e2) NULL))
    dca_val <- tryCatch({
      rmda::decision_curve(Outcome ~ Predicted_Prob, data = validation_data, fitted.risk = TRUE, thresholds = seq(0.01, 0.99, by = 0.02), bootstraps = 25)
    }, error = function(e) tryCatch({
      rmda::decision_curve(Outcome ~ Predicted_Prob, data = validation_data, fitted.risk = TRUE, thresholds = seq(0.01, 0.99, by = 0.02), bootstraps = 0)
    }, error = function(e2) NULL))

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
    rv$nomogram_val_roc <- pROC::roc(validation_data$Outcome, validation_data$Predicted_Prob, quiet = TRUE)
    rv$nomogram_model_diagnostics <- model_diagnostics
    rv$nomogram_performance_comparison <- performance_comparison
    rv$nomogram_cal_train <- cal_train
    rv$nomogram_cal_validation <- cal_validation
    rv$nomogram_dca_train <- dca_train
    rv$nomogram_dca_val <- dca_val
    rv$nomogram_ci_train <- ci_train
    rv$nomogram_ci_val <- ci_val
    rv$nomogram_complete <- TRUE

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
    np <- rms::nomogram(rv$nomogram_model, fun = plogis, fun.at = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), funlabel = "Risk of Disease", lp = FALSE)
    plot(np)
    title(main = "Diagnostic Nomogram", cex.main = 1.3, font.main = 2)
  }, height = 500)

  output$nomogram_plot_roc_train <- renderPlot({
    req(rv$nomogram_train_roc, rv$nomogram_train_metrics)
    plot(rv$nomogram_train_roc, col = "#E74C3C", lwd = 2, main = "Training ROC", legacy.axes = TRUE)
    abline(0, 1, lty = 2, col = "grey60")
    m <- rv$nomogram_train_metrics
    text(0.6, 0.25, sprintf("AUC = %.3f\n(95%% CI: %.3f-%.3f)", m$AUC, m$AUC_Lower, m$AUC_Upper), cex = 1, col = "#E74C3C", font = 2)
  }, height = 360)

  output$nomogram_plot_roc_val <- renderPlot({
    req(rv$nomogram_val_roc, rv$nomogram_val_metrics)
    mode <- rv$validation_mode
    if (is.null(mode)) mode <- "internal"
    col <- if (mode == "external") "#27AE60" else "#3498DB"
    label <- if (mode == "external") "External Validation ROC" else "Validation ROC"
    plot(rv$nomogram_val_roc, col = col, lwd = 2, main = label, legacy.axes = TRUE)
    abline(0, 1, lty = 2, col = "grey60")
    m <- rv$nomogram_val_metrics
    text(0.6, 0.25, sprintf("AUC = %.3f\n(95%% CI: %.3f-%.3f)", m$AUC, m$AUC_Lower, m$AUC_Upper), cex = 1, col = col, font = 2)
  }, height = 360)

  output$nomogram_plot_cal_train <- renderPlot({
    req(rv$nomogram_cal_train)
    plot(rv$nomogram_cal_train, lwd = 2, col = "#E74C3C", xlab = "Predicted", ylab = "Observed")
  }, height = 320)

  output$nomogram_plot_cal_val <- renderPlot({
    req(rv$nomogram_cal_validation)
    mode <- rv$validation_mode
    if (is.null(mode)) mode <- "internal"
    col <- if (mode == "external") "#27AE60" else "#3498DB"
    label <- if (mode == "external") "External Validation Calibration" else "Validation Calibration"
    cv <- rv$nomogram_cal_validation
    plot(cv$Predicted, cv$Observed, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "Predicted", ylab = "Observed", main = label)
    abline(0, 1, lty = 2, col = "grey60")
    arrows(cv$Predicted, cv$Observed - 1.96 * cv$SE, cv$Predicted, cv$Observed + 1.96 * cv$SE, angle = 90, code = 3, length = 0.05, col = col)
    points(cv$Predicted, cv$Observed, pch = 19, cex = 1.2, col = col)
    lines(cv$Predicted, cv$Observed, col = col, lwd = 2)
  }, height = 320)

  output$nomogram_plot_dca_train <- renderPlot({
    req(rv$nomogram_dca_train)
    rmda::plot_decision_curve(rv$nomogram_dca_train, col = c("grey60", "#E74C3C", "grey60"), legend.position = "topright", lwd = 2)
  }, height = 320)

  output$nomogram_plot_dca_val <- renderPlot({
    req(rv$nomogram_dca_val)
    mode <- rv$validation_mode
    if (is.null(mode)) mode <- "internal"
    col <- if (mode == "external") "#27AE60" else "#3498DB"
    rmda::plot_decision_curve(rv$nomogram_dca_val, col = c("grey60", col, "grey60"), legend.position = "topright", lwd = 2)
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

  output$nomogram_diagnostics_table <- DT::renderDataTable({
    req(rv$nomogram_model_diagnostics)
    df <- rv$nomogram_model_diagnostics
    df$Coefficient <- round(df$Coefficient, 4)
    df$Std_Error <- round(df$Std_Error, 4)
    df$OR <- round(df$OR, 4)
    df$VIF <- round(df$VIF, 2)
    DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  output$nomogram_performance_table <- DT::renderDataTable({
    req(rv$nomogram_performance_comparison)
    df <- rv$nomogram_performance_comparison
    for (j in c("Accuracy", "Sensitivity", "Specificity", "PPV", "NPV", "AUC", "AUC_Lower", "AUC_Upper", "Threshold"))
      if (j %in% names(df)) df[[j]] <- round(df[[j]], 4)
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  # ============================================================================
  # DOWNLOAD HANDLERS
  # ============================================================================
  nomogram_plot_to_file <- function(file, dev_open, dev_close = function() dev.off()) {
    req(rv$nomogram_model, rv$nomogram_available_genes)
    dd <- rms::datadist(rv$nomogram_train_data[, rv$nomogram_available_genes, drop = FALSE])
    options(datadist = dd)
    on.exit(options(datadist = NULL), add = TRUE)
    dev_open()
    np <- rms::nomogram(rv$nomogram_model, fun = plogis, fun.at = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), funlabel = "Risk of Disease", lp = FALSE)
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

  output$download_nomogram_diagnostics <- downloadHandler(
    filename = function() "Nomogram_Model_Diagnostics.csv",
    content = function(file) {
      req(rv$nomogram_model_diagnostics)
      write.csv(rv$nomogram_model_diagnostics, file, row.names = FALSE)
      write.csv(rv$nomogram_model_diagnostics, file.path(CSV_EXPORT_DIR(), "Nomogram_Model_Diagnostics.csv"), row.names = FALSE)
    }
  )

  output$download_nomogram_performance <- downloadHandler(
    filename = function() "Nomogram_Performance_Comparison.csv",
    content = function(file) {
      req(rv$nomogram_performance_comparison)
      write.csv(rv$nomogram_performance_comparison, file, row.names = FALSE)
      write.csv(rv$nomogram_performance_comparison, file.path(CSV_EXPORT_DIR(), "Nomogram_Performance_Comparison.csv"), row.names = FALSE)
    }
  )
}
