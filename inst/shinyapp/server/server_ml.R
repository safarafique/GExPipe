# ==============================================================================
# SERVER_ML.R - Machine Learning (LASSO, Random Forest, SVM-RFE)
# ==============================================================================
# Uses rv$extracted_data_ml (samples × genes) and rv$wgcna_sample_info for labels.
# ==============================================================================

server_ml <- function(input, output, session, rv) {

  output$ml_placeholder_ui <- renderUI({
    if (!is.null(rv$extracted_data_ml)) return(NULL)
    tags$div(
      class = "alert alert-warning",
      icon("hand-point-right"),
      " Go to Step 9 (PPI Interaction), run PPI analysis, then click 'Extract Data for ML'. Return here and click 'Run ML Analysis'."
    )
  })

  output$ml_status_ui <- renderUI({
    if (!isTRUE(rv$ml_complete)) return(NULL)
    methods_run <- rv$ml_methods_run
    if (is.null(methods_run)) methods_run <- c("LASSO", "Elastic Net", "Ridge", "Random Forest", "SVM-RFE", "Boruta", "sPLS-DA", "XGBoost+SHAP")
    tags$div(
      class = "alert alert-success",
      icon("check-circle"),
      " ML analysis complete. Results and Venn diagram for: ", paste(methods_run, collapse = ", "), "."
    )
  })

  output$ml_process_summary_ui <- renderUI({
    if (!isTRUE(rv$ml_complete)) {
      return(tags$p(style = "color: #6c757d; margin: 0;", icon("info-circle"), " Extract data for ML and run ML analysis to see process summary."))
    }
    n_genes <- length(rv$ml_common_genes)
    methods_run <- rv$ml_methods_run
    tags$div(
      style = "font-size: 14px; line-height: 1.6; color: #333;",
      tags$p(tags$strong("Step 10 complete."), " Common genes across selected methods: ", n_genes, ". Methods run: ", paste(methods_run, collapse = ", "), "."))
  })

  # SVM-RFE helper (recursive feature elimination)
  svmRFE <- function(X, y, k_fold = 5) {
    X <- as.matrix(X)
    mode(X) <- "numeric"
    n_features <- ncol(X)
    ranked_list <- c()
    remaining_features <- 1:n_features
    feature_names <- colnames(X)
    iteration <- 0
    while (length(remaining_features) > 1) {
      iteration <- iteration + 1
      folds <- caret::createFolds(y, k = k_fold)
      feature_scores <- rep(0, length(remaining_features))
      for (fold in folds) {
        train_X <- X[-fold, remaining_features, drop = FALSE]
        train_y <- y[-fold]
        model <- tryCatch(
          kernlab::ksvm(train_X, train_y, kernel = "vanilladot", C = 1, scaled = FALSE),
          error = function(e) NULL
        )
        if (is.null(model) || is.null(model@coef) || length(model@coef[[1]]) == 0) next
        weights <- tryCatch(
          t(model@coef[[1]]) %*% model@xmatrix[[1]],
          error = function(e) rep(0, length(remaining_features))
        )
        feature_scores <- feature_scores + (weights^2)
      }
      avg_scores <- feature_scores / k_fold
      worst_feature <- which.min(avg_scores)
      ranked_list <- c(feature_names[remaining_features[worst_feature]], ranked_list)
      remaining_features <- remaining_features[-worst_feature]
    }
    ranked_list <- c(feature_names[remaining_features], ranked_list)
    data.frame(Gene = ranked_list, Rank = seq_along(ranked_list), stringsAsFactors = FALSE)
  }

  observeEvent(input$run_ml, {
    if (is.null(rv$extracted_data_ml)) {
      showNotification(
        tags$div(icon("exclamation-triangle"), tags$strong(" Data not ready:"),
                 " Go to Step 9 (PPI) or Step 8 (Common Genes), then click 'Extract Data for ML' before running ML."),
        type = "error", duration = 8)
      return()
    }
    if (is.null(rv$wgcna_sample_info)) {
      showNotification(
        tags$div(icon("exclamation-triangle"), tags$strong(" Sample info missing:"),
                 " Complete WGCNA (Step 7) first — sample metadata with group labels is required for ML."),
        type = "error", duration = 8)
      return()
    }
    methods_sel <- input$ml_methods
    if (is.null(methods_sel) || length(methods_sel) == 0) {
      showNotification("Select at least one method (e.g. LASSO, RF, SVM-RFE, Boruta, sPLS-DA, XGBoost+SHAP).", type = "warning", duration = 6)
      return()
    }
    x_raw <- as.matrix(rv$extracted_data_ml)
    mode(x_raw) <- "numeric"
    sample_info <- rv$wgcna_sample_info
    if (nrow(x_raw) < 3 || ncol(x_raw) < 2) {
      showNotification("Extracted data too small (need at least 3 samples and 2 genes).", type = "error", duration = 6)
      return()
    }
    common_samples <- intersect(rownames(x_raw), rownames(sample_info))
    if (length(common_samples) == 0) {
      showNotification("No common samples between expression data and sample info.", type = "error", duration = 6)
      return()
    }
    x <- x_raw[common_samples, , drop = FALSE]
    sample_info <- sample_info[common_samples, , drop = FALSE]
    if (!is.null(rv$ppi_centrality_weights) && length(rv$ppi_centrality_weights) > 0) {
      w <- rv$ppi_centrality_weights
      for (j in colnames(x)) {
        if (j %in% names(w)) x[, j] <- x[, j] * as.numeric(w[j])
      }
    }
    cond_col <- if ("Condition" %in% names(sample_info)) "Condition" else NULL
    if (is.null(cond_col)) {
      fac_cols <- names(sample_info)[vapply(sample_info, function(z) is.factor(z) || is.character(z), logical(1))]
      if (length(fac_cols) > 0) cond_col <- fac_cols[1]
    }
    if (is.null(cond_col)) {
      showNotification("No group/Condition column in sample info.", type = "error", duration = 6)
      return()
    }
    grp <- sample_info[[cond_col]]
    y <- as.factor(ifelse(grp %in% c("Disease", "disease", "2"), 1, 0))
    if (length(unique(y)) < 2) {
      showNotification("Need at least two classes (e.g. Disease vs Normal).", type = "error", duration = 6)
      return()
    }
    n_rf <- max(5, min(100, as.integer(input$ml_rf_top_genes)))
    n_svm <- max(5, min(100, as.integer(input$ml_svm_top_genes)))
    n_elastic <- max(5, min(100, as.integer(input$ml_elastic_top_genes)))
    n_ridge <- max(5, min(100, as.integer(input$ml_ridge_top_genes)))
    n_boruta <- max(5, min(100, as.integer(input$ml_boruta_top_genes)))
    n_splsda <- max(5, min(100, as.integer(input$ml_splsda_top_genes)))
    n_xgboost <- max(5, min(100, as.integer(input$ml_xgboost_top_genes)))

    # Clear results for methods not selected
    if (!"lasso" %in% methods_sel) { rv$ml_lasso_df <- NULL; rv$ml_lasso_genes <- NULL; rv$ml_cv_fit_lasso <- NULL }
    if (!"elastic" %in% methods_sel) { rv$ml_elastic_df <- NULL; rv$ml_elastic_top_genes <- NULL; rv$ml_cv_fit_elastic <- NULL }
    if (!"ridge" %in% methods_sel) { rv$ml_ridge_df <- NULL; rv$ml_ridge_top_genes <- NULL; rv$ml_cv_fit_ridge <- NULL }
    if (!"rf" %in% methods_sel) { rv$ml_rf_importance <- NULL; rv$ml_rf_top_genes <- NULL; rv$ml_rf_model <- NULL }
    if (!"svm" %in% methods_sel) { rv$ml_svm_ranking <- NULL; rv$ml_svm_top_genes <- NULL }
    if (!"boruta" %in% methods_sel) { rv$ml_boruta_df <- NULL; rv$ml_boruta_top_genes <- NULL }
    if (!"splsda" %in% methods_sel) { rv$ml_splsda_df <- NULL; rv$ml_splsda_top_genes <- NULL }
    if (!"xgboost" %in% methods_sel) { rv$ml_xgboost_df <- NULL; rv$ml_xgboost_top_genes <- NULL }

    n_steps <- length(methods_sel)
    step_inc <- 0.8 / max(1, n_steps)
    prog <- 0.1

    withProgress(message = "Running ML analysis...", value = 0.1, {
      tryCatch({
        set.seed(123)
        gene_lists <- list()
        method_names <- character(0)

        if ("lasso" %in% methods_sel) {
          incProgress(step_inc, detail = "LASSO...")
          cv_fit <- glmnet::cv.glmnet(x, y, alpha = 1, family = "binomial")
          coef_min <- as.matrix(coef(cv_fit, s = cv_fit$lambda.min))
          lasso_df <- data.frame(Gene = rownames(coef_min), Coefficient = coef_min[, 1], stringsAsFactors = FALSE)
          lasso_df <- lasso_df[lasso_df$Gene != "(Intercept)" & lasso_df$Coefficient != 0, ]
          lasso_df$AbsCoefficient <- abs(lasso_df$Coefficient)
          lasso_df <- lasso_df[order(-lasso_df$AbsCoefficient), ]
          lasso_df$Rank_LASSO <- seq_len(nrow(lasso_df))
          rv$ml_lasso_df <- lasso_df
          rv$ml_cv_fit_lasso <- cv_fit
          gene_lists$LASSO <- rv$ml_lasso_df$Gene
          method_names <- c(method_names, "LASSO")
          prog <- prog + step_inc
        }
        if ("elastic" %in% methods_sel) {
          incProgress(step_inc, detail = "Elastic Net...")
          set.seed(123)
          cv_elastic <- glmnet::cv.glmnet(x, y, alpha = 0.5, family = "binomial")
          coef_elastic <- as.matrix(coef(cv_elastic, s = cv_elastic$lambda.min))
          elastic_df <- data.frame(Gene = rownames(coef_elastic), Coefficient = coef_elastic[, 1], stringsAsFactors = FALSE)
          elastic_df <- elastic_df[elastic_df$Gene != "(Intercept)" & elastic_df$Coefficient != 0, ]
          elastic_df$AbsCoefficient <- abs(elastic_df$Coefficient)
          elastic_df <- elastic_df[order(-elastic_df$AbsCoefficient), ]
          elastic_df$Rank_ElasticNet <- seq_len(nrow(elastic_df))
          rv$ml_elastic_df <- elastic_df
          rv$ml_cv_fit_elastic <- cv_elastic
          gene_lists[["Elastic Net"]] <- head(rv$ml_elastic_df$Gene, n_elastic)
          method_names <- c(method_names, "Elastic Net")
          prog <- prog + step_inc
        }
        if ("ridge" %in% methods_sel) {
          incProgress(step_inc, detail = "Ridge...")
          set.seed(123)
          cv_ridge <- glmnet::cv.glmnet(x, y, alpha = 0, family = "binomial")
          coef_ridge <- as.matrix(coef(cv_ridge, s = cv_ridge$lambda.min))
          ridge_df <- data.frame(Gene = rownames(coef_ridge), Coefficient = coef_ridge[, 1], stringsAsFactors = FALSE)
          ridge_df <- ridge_df[ridge_df$Gene != "(Intercept)" & ridge_df$Coefficient != 0, ]
          ridge_df$AbsCoefficient <- abs(ridge_df$Coefficient)
          ridge_df <- ridge_df[order(-ridge_df$AbsCoefficient), ]
          ridge_df$Rank_Ridge <- seq_len(nrow(ridge_df))
          rv$ml_ridge_df <- ridge_df
          rv$ml_cv_fit_ridge <- cv_ridge
          gene_lists$Ridge <- head(rv$ml_ridge_df$Gene, n_ridge)
          method_names <- c(method_names, "Ridge")
          prog <- prog + step_inc
        }
        if ("rf" %in% methods_sel) {
          incProgress(step_inc, detail = "Random Forest...")
          set.seed(123)
          rf_model <- randomForest::randomForest(x = x, y = y, ntree = 500, importance = TRUE)
          imp <- randomForest::importance(rf_model)
          rf_df <- data.frame(
            Gene = rownames(imp),
            MeanDecreaseAccuracy = imp[, "MeanDecreaseAccuracy"],
            MeanDecreaseGini = imp[, "MeanDecreaseGini"],
            stringsAsFactors = FALSE
          )
          rf_df$Rank_RF_Accuracy <- rank(-rf_df$MeanDecreaseAccuracy)
          rf_df$Rank_RF_Gini <- rank(-rf_df$MeanDecreaseGini)
          rv$ml_rf_importance <- rf_df
          rv$ml_rf_model <- rf_model
          gene_lists[["Random Forest"]] <- head(rv$ml_rf_importance[order(-rv$ml_rf_importance$MeanDecreaseAccuracy), "Gene"], n_rf)
          method_names <- c(method_names, "Random Forest")
          prog <- prog + step_inc
        }
        if ("svm" %in% methods_sel) {
          incProgress(step_inc, detail = "SVM-RFE...")
          set.seed(123)
          svm_ranking <- svmRFE(x, y)
          rv$ml_svm_ranking <- svm_ranking
          gene_lists[["SVM-RFE"]] <- head(rv$ml_svm_ranking$Gene, n_svm)
          method_names <- c(method_names, "SVM-RFE")
          prog <- prog + step_inc
        }
        if ("boruta" %in% methods_sel) {
          incProgress(step_inc, detail = "Boruta...")
          set.seed(123)
          x_df <- as.data.frame(x)
          boruta_fit <- tryCatch(
            Boruta::Boruta(x_df, y, maxRuns = 100, doTrace = 0),
            error = function(e) NULL
          )
          if (!is.null(boruta_fit)) {
            att <- Boruta::attStats(boruta_fit)
            att$Gene <- rownames(att)
            att <- att[att$decision %in% c("Confirmed", "Tentative"), ]
            att <- att[order(-att$medianImp), ]
            boruta_df <- data.frame(Gene = att$Gene, medianImp = att$medianImp, meanImp = att$meanImp, decision = att$decision, Rank_Boruta = seq_len(nrow(att)), stringsAsFactors = FALSE)
            rv$ml_boruta_df <- boruta_df
            gene_lists[["Boruta"]] <- head(boruta_df$Gene, n_boruta)
            method_names <- c(method_names, "Boruta")
          } else {
            rv$ml_boruta_df <- NULL
          }
          prog <- prog + step_inc
        }
        if ("splsda" %in% methods_sel) {
          incProgress(step_inc, detail = "sPLS-DA...")
          set.seed(123)
          n_keep <- min(n_splsda, ncol(x), 50)
          splsda_fit <- tryCatch({
            mixOmics::splsda(X = x, Y = y, ncomp = 2, keepX = c(n_keep, n_keep))
          }, error = function(e) NULL)
          if (!is.null(splsda_fit)) {
            vip_scores <- tryCatch(mixOmics::vip(splsda_fit), error = function(e) NULL)
            if (is.null(vip_scores)) {
              ld <- splsda_fit$loadings$X
              vip_scores <- matrix(abs(ld), nrow = nrow(ld), dimnames = list(rownames(ld), NULL))
              if (ncol(vip_scores) > 1) vip_scores <- apply(vip_scores, 1, max) else vip_scores <- setNames(as.numeric(vip_scores), rownames(ld))
            } else {
              if (NCOL(vip_scores) > 1) vip_scores <- apply(vip_scores, 1, max) else vip_scores <- setNames(as.numeric(vip_scores), rownames(vip_scores))
            }
            vip_ord <- order(-vip_scores)
            genes_ord <- names(vip_scores)[vip_ord]
            splsda_df <- data.frame(Gene = genes_ord, VIP = as.numeric(vip_scores[genes_ord]), Rank_sPLSDA = seq_along(genes_ord), stringsAsFactors = FALSE)
            rv$ml_splsda_df <- splsda_df
            gene_lists[["sPLS-DA"]] <- head(genes_ord, n_splsda)
            method_names <- c(method_names, "sPLS-DA")
          } else {
            rv$ml_splsda_df <- NULL
          }
          prog <- prog + step_inc
        }
        if ("xgboost" %in% methods_sel) {
          incProgress(step_inc, detail = "XGBoost+SHAP...")
          set.seed(123)
          y_num <- as.numeric(y) - 1
          dtrain <- xgboost::xgb.DMatrix(data = x, label = y_num)
          xgb_fit <- tryCatch(
            xgboost::xgb.train(params = list(objective = "binary:logistic", eval_metric = "auc"), data = dtrain, nrounds = 100, verbose = 0),
            error = function(e) NULL
          )
          if (!is.null(xgb_fit)) {
            imp_df <- NULL
            if (requireNamespace("SHAPforxgboost", quietly = TRUE)) {
              shap_res <- tryCatch(SHAPforxgboost::shap.values(xgb_model = xgb_fit, X_train = x), error = function(e) NULL)
              if (!is.null(shap_res) && !is.null(shap_res$mean_shap_score)) {
                ms <- shap_res$mean_shap_score
                genes_ord <- names(ms)[order(-ms)]
                imp_df <- data.frame(Gene = genes_ord, mean_abs_SHAP = as.numeric(ms[genes_ord]), Rank_XGBoostSHAP = seq_along(genes_ord), stringsAsFactors = FALSE)
              }
            }
            if (is.null(imp_df)) {
              imp <- tryCatch(xgboost::xgb.importance(model = xgb_fit), error = function(e) NULL)
              if (!is.null(imp) && nrow(imp) > 0) {
                imp_df <- data.frame(Gene = imp$Feature, Gain = imp$Gain, Rank_XGBoost = seq_len(nrow(imp)), stringsAsFactors = FALSE)
                imp_df <- imp_df[order(-imp_df$Gain), ]
                imp_df$Rank_XGBoost <- seq_len(nrow(imp_df))
              }
            }
            if (!is.null(imp_df)) {
              rv$ml_xgboost_df <- imp_df
              gene_lists[["XGBoost+SHAP"]] <- head(imp_df$Gene, n_xgboost)
              method_names <- c(method_names, "XGBoost+SHAP")
            } else {
              rv$ml_xgboost_df <- NULL
            }
          } else {
            rv$ml_xgboost_df <- NULL
          }
          prog <- prog + step_inc
        }

        # Common genes = intersection of selected methods' gene lists
        common_all <- if (length(gene_lists) == 0) character(0) else Reduce(intersect, gene_lists)
        rv$ml_common_genes <- common_all
        rv$ml_venn_sets <- gene_lists
        rv$ml_methods_run <- method_names
        # Keep per-method top genes for combined list / display
        rv$ml_lasso_genes <- if (!is.null(rv$ml_lasso_df)) rv$ml_lasso_df$Gene else character(0)
        rv$ml_elastic_top_genes <- if (!is.null(rv$ml_elastic_df)) head(rv$ml_elastic_df$Gene, n_elastic) else character(0)
        rv$ml_ridge_top_genes <- if (!is.null(rv$ml_ridge_df)) head(rv$ml_ridge_df$Gene, n_ridge) else character(0)
        rv$ml_rf_top_genes <- if (!is.null(rv$ml_rf_importance)) head(rv$ml_rf_importance[order(-rv$ml_rf_importance$MeanDecreaseAccuracy), "Gene"], n_rf) else character(0)
        rv$ml_svm_top_genes <- if (!is.null(rv$ml_svm_ranking)) head(rv$ml_svm_ranking$Gene, n_svm) else character(0)
        rv$ml_boruta_top_genes <- if (!is.null(rv$ml_boruta_df)) head(rv$ml_boruta_df$Gene, n_boruta) else character(0)
        rv$ml_splsda_top_genes <- if (!is.null(rv$ml_splsda_df)) head(rv$ml_splsda_df$Gene, n_splsda) else character(0)
        rv$ml_xgboost_top_genes <- if (!is.null(rv$ml_xgboost_df)) head(rv$ml_xgboost_df$Gene, n_xgboost) else character(0)
        rv$ml_x <- x
        rv$ml_y <- y
        rv$ml_complete <- TRUE
        incProgress(1 - prog, detail = "Done")
        showNotification(paste0("ML analysis complete (", paste(method_names, collapse = ", "), "). Venn and common genes use selected methods only."), type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("ML error:", e$message), type = "error", duration = 10)
      })
    })
  })

  output$ml_lasso_table <- DT::renderDataTable({
    req(rv$ml_lasso_df)
    DT::datatable(rv$ml_lasso_df, options = list(pageLength = 15), rownames = FALSE)
  })
  output$download_ml_lasso <- downloadHandler(
    filename = function() "lasso_coefficients_ranking.csv",
    content = function(file) {
      req(rv$ml_lasso_df)
      write.csv(rv$ml_lasso_df, file, row.names = FALSE)
      write.csv(rv$ml_lasso_df, file.path(CSV_EXPORT_DIR(), "lasso_coefficients_ranking.csv"), row.names = FALSE)
    }
  )

  output$ml_rf_table <- DT::renderDataTable({
    req(rv$ml_rf_importance)
    DT::datatable(rv$ml_rf_importance, options = list(pageLength = 15), rownames = FALSE)
  })
  output$download_ml_rf <- downloadHandler(
    filename = function() "random_forest_importance_ranking.csv",
    content = function(file) {
      req(rv$ml_rf_importance)
      write.csv(rv$ml_rf_importance, file, row.names = FALSE)
      write.csv(rv$ml_rf_importance, file.path(CSV_EXPORT_DIR(), "random_forest_importance_ranking.csv"), row.names = FALSE)
    }
  )

  output$ml_svm_table <- DT::renderDataTable({
    req(rv$ml_svm_ranking)
    DT::datatable(rv$ml_svm_ranking, options = list(pageLength = 15), rownames = FALSE)
  })
  output$download_ml_svm <- downloadHandler(
    filename = function() "svm_rfe_ranking.csv",
    content = function(file) {
      req(rv$ml_svm_ranking)
      write.csv(rv$ml_svm_ranking, file, row.names = FALSE)
      write.csv(rv$ml_svm_ranking, file.path(CSV_EXPORT_DIR(), "svm_rfe_ranking.csv"), row.names = FALSE)
    }
  )

  output$ml_elastic_table <- DT::renderDataTable({
    req(rv$ml_elastic_df)
    DT::datatable(rv$ml_elastic_df, options = list(pageLength = 15), rownames = FALSE)
  })
  output$download_ml_elastic <- downloadHandler(
    filename = function() "elastic_net_ranking.csv",
    content = function(file) {
      req(rv$ml_elastic_df)
      write.csv(rv$ml_elastic_df, file, row.names = FALSE)
      write.csv(rv$ml_elastic_df, file.path(CSV_EXPORT_DIR(), "elastic_net_ranking.csv"), row.names = FALSE)
    }
  )

  output$ml_ridge_table <- DT::renderDataTable({
    req(rv$ml_ridge_df)
    DT::datatable(rv$ml_ridge_df, options = list(pageLength = 15), rownames = FALSE)
  })
  output$download_ml_ridge <- downloadHandler(
    filename = function() "ridge_ranking.csv",
    content = function(file) {
      req(rv$ml_ridge_df)
      write.csv(rv$ml_ridge_df, file, row.names = FALSE)
      write.csv(rv$ml_ridge_df, file.path(CSV_EXPORT_DIR(), "ridge_ranking.csv"), row.names = FALSE)
    }
  )

  output$ml_boruta_table <- DT::renderDataTable({
    req(rv$ml_boruta_df)
    DT::datatable(rv$ml_boruta_df, options = list(pageLength = 15), rownames = FALSE)
  })
  output$download_ml_boruta <- downloadHandler(
    filename = function() "boruta_importance.csv",
    content = function(file) {
      req(rv$ml_boruta_df)
      write.csv(rv$ml_boruta_df, file, row.names = FALSE)
      write.csv(rv$ml_boruta_df, file.path(CSV_EXPORT_DIR(), "boruta_importance.csv"), row.names = FALSE)
    }
  )

  output$ml_splsda_table <- DT::renderDataTable({
    req(rv$ml_splsda_df)
    DT::datatable(rv$ml_splsda_df, options = list(pageLength = 15), rownames = FALSE)
  })
  output$download_ml_splsda <- downloadHandler(
    filename = function() "splsda_vip_ranking.csv",
    content = function(file) {
      req(rv$ml_splsda_df)
      write.csv(rv$ml_splsda_df, file, row.names = FALSE)
      write.csv(rv$ml_splsda_df, file.path(CSV_EXPORT_DIR(), "splsda_vip_ranking.csv"), row.names = FALSE)
    }
  )

  output$ml_xgboost_table <- DT::renderDataTable({
    req(rv$ml_xgboost_df)
    DT::datatable(rv$ml_xgboost_df, options = list(pageLength = 15), rownames = FALSE)
  })
  output$download_ml_xgboost <- downloadHandler(
    filename = function() "xgboost_shap_ranking.csv",
    content = function(file) {
      req(rv$ml_xgboost_df)
      write.csv(rv$ml_xgboost_df, file, row.names = FALSE)
      write.csv(rv$ml_xgboost_df, file.path(CSV_EXPORT_DIR(), "xgboost_shap_ranking.csv"), row.names = FALSE)
    }
  )

  output$ml_common_genes_ui <- renderUI({
    req(rv$ml_common_genes)
    n <- length(rv$ml_common_genes)
    n_m <- if (is.null(rv$ml_venn_sets)) 0L else length(rv$ml_venn_sets)
    if (n_m == 0) n_m <- 1L
    tags$p(tags$strong("Common genes (", n_m, " selected method(s)): ", n), style = "margin-bottom: 10px;")
  })
  output$ml_common_genes_table <- DT::renderDataTable({
    req(rv$ml_common_genes)
    df <- data.frame(Gene = rv$ml_common_genes, stringsAsFactors = FALSE)
    DT::datatable(df, options = list(pageLength = 20), rownames = FALSE)
  })
  output$download_ml_common_genes <- downloadHandler(
    filename = function() "common_genes_ML_methods.csv",
    content = function(file) {
      req(rv$ml_common_genes)
      write.csv(data.frame(Gene = rv$ml_common_genes), file, row.names = FALSE)
      write.csv(data.frame(Gene = rv$ml_common_genes), file.path(CSV_EXPORT_DIR(), "common_genes_ML_methods.csv"), row.names = FALSE)
    }
  )

  # Final summary list (common to selected methods)
  output$ml_final_list_ui <- renderUI({
    req(rv$ml_common_genes)
    n <- length(rv$ml_common_genes)
    n_m <- if (is.null(rv$ml_venn_sets)) 0L else length(rv$ml_venn_sets)
    if (n_m == 0) n_m <- 1L
    tags$p(tags$strong("Number of genes in final list (common to ", n_m, " selected method(s)): ", n), style = "margin-bottom: 10px;")
  })
  output$ml_final_list_table <- DT::renderDataTable({
    req(rv$ml_common_genes)
    df <- data.frame(Gene = rv$ml_common_genes, stringsAsFactors = FALSE)
    DT::datatable(df, options = list(pageLength = 20), rownames = FALSE)
  })
  output$download_ml_final_list <- downloadHandler(
    filename = function() "final_list_common_genes_ML.csv",
    content = function(file) {
      req(rv$ml_common_genes)
      write.csv(data.frame(Gene = rv$ml_common_genes), file, row.names = FALSE)
      write.csv(data.frame(Gene = rv$ml_common_genes), file.path(CSV_EXPORT_DIR(), "final_list_common_genes_ML.csv"), row.names = FALSE)
    }
  )

  # ---------------------------------------------------------------------------
  # Correlation & co-expression for final biomarker panel (common ML genes)
  # ---------------------------------------------------------------------------
  output$ml_biomarker_cor_heatmap <- renderPlot({
    req(rv$extracted_data_ml, rv$ml_common_genes)
    expr_mat <- as.matrix(rv$extracted_data_ml)          # samples x genes
    genes <- intersect(rv$ml_common_genes, colnames(expr_mat))
    if (length(genes) < 2) {
      plot.new()
      text(0.5, 0.5, "Need at least 2 final biomarker genes with expression data.", cex = 0.9, col = "gray40")
      return()
    }
    expr_sub <- expr_mat[, genes, drop = FALSE]          # samples x final genes
    cor_mat <- suppressWarnings(cor(expr_sub, method = "spearman", use = "pairwise.complete.obs"))

    tryCatch({
      pheatmap::pheatmap(
        cor_mat,
        display_numbers = TRUE,
        number_color = "black",
        clustering_method = "complete",
        main = "Biomarker Co-expression (Spearman)",
        fontsize = 10
      )
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Heatmap error:", conditionMessage(e)), cex = 0.9, col = "gray40")
    })
  }, height = 380)

  output$ml_biomarker_coexp_network <- renderPlot({
    req(rv$extracted_data_ml, rv$ml_common_genes)
    expr_mat <- as.matrix(rv$extracted_data_ml)          # samples x genes
    genes <- intersect(rv$ml_common_genes, colnames(expr_mat))
    if (length(genes) < 2) {
      plot.new()
      text(0.5, 0.5, "Need at least 2 final biomarker genes with expression data.", cex = 0.9, col = "gray40")
      return()
    }
    expr_sub <- expr_mat[, genes, drop = FALSE]
    cor_mat <- suppressWarnings(cor(expr_sub, method = "spearman", use = "pairwise.complete.obs"))
    diag(cor_mat) <- 0

    thr <- 0.7
    adj <- cor_mat
    adj[abs(adj) < thr] <- 0
    if (all(adj == 0)) {
      plot.new()
      text(0.5, 0.5, paste0("No edges with |Spearman| \u2265 ", thr, "."), cex = 0.9, col = "gray40")
      return()
    }

    g <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE, diag = FALSE)
    layout <- igraph::layout_with_fr(g)
    edge_cols <- ifelse(igraph::E(g)$weight >= 0, "#2ECC71", "#E74C3C")
    edge_width <- 1 + 4 * abs(igraph::E(g)$weight)
    vertex_size <- 8 + 4 * igraph::degree(g)

    plot(g,
         layout = layout,
         vertex.label = igraph::V(g)$name,
         vertex.label.cex = 0.8,
         vertex.color = "#3498DB",
         vertex.size = vertex_size,
         edge.color = edge_cols,
         edge.width = edge_width,
         main = paste0("Co-expression Network (|Spearman| \u2265 ", thr, ")"))
  }, height = 380)

  # Combined list: all genes from selected methods with rank per method and In_common
  output$download_ml_combined_list <- downloadHandler(
    filename = function() "combined_ML_gene_list.csv",
    content = function(file) {
      sets <- rv$ml_venn_sets
      if (is.null(sets) || length(sets) == 0) return()
      all_genes <- unique(unlist(sets, use.names = FALSE))
      if (length(all_genes) == 0) return()
      common <- rv$ml_common_genes
      if (is.null(common)) common <- character(0)
      df <- data.frame(Gene = all_genes, stringsAsFactors = FALSE)
      for (nm in names(sets)) {
        g <- sets[[nm]]
        r <- setNames(seq_along(g), g)
        col_nm <- gsub(" ", "_", paste0(nm, "_rank"))
        df[[col_nm]] <- as.integer(r[all_genes])
      }
      df$In_common_selected <- all_genes %in% common
      rank_cols <- setdiff(names(df), c("Gene", "In_common_selected"))
      ord <- order(-as.integer(df$In_common_selected), rowSums(!is.na(df[, rank_cols, drop = FALSE])), decreasing = TRUE)
      df <- df[ord, , drop = FALSE]
      write.csv(df, file, row.names = FALSE)
      write.csv(df, file.path(CSV_EXPORT_DIR(), "combined_ML_gene_list.csv"), row.names = FALSE)
    }
  )

  # Diagnostic plots UI: show only plots for methods that were run
  output$ml_diagnostic_plots_ui <- renderUI({
    if (!isTRUE(rv$ml_complete)) return(NULL)
    out <- list()
    # Random Forest
    if (!is.null(rv$ml_rf_model) && !is.null(rv$ml_x) && !is.null(rv$ml_y)) {
      out <- c(out, list(
        fluidRow(
          column(6, box(title = "RF: Training / Test / OOB Error", width = NULL, status = "success", solidHeader = TRUE,
                       plotOutput("ml_plot_rf_error", height = "380px"),
                       tags$div(style = "margin-top:6px;",
                                downloadButton("dl_ml_rf_error_png", tagList(icon("download"), " PNG (300 DPI)"), class = "btn-success btn-xs", style = "margin-right:6px;"),
                                downloadButton("dl_ml_rf_error_pdf", tagList(icon("download"), " PDF"), class = "btn-success btn-xs")))),
          column(6, box(title = "RF: Top 10 importance (MeanDecreaseAccuracy)", width = NULL, status = "success", solidHeader = TRUE,
                       plotOutput("ml_plot_rf_importance", height = "380px"),
                       tags$div(style = "margin-top:6px;",
                                downloadButton("dl_ml_rf_imp_png", tagList(icon("download"), " PNG (300 DPI)"), class = "btn-success btn-xs", style = "margin-right:6px;"),
                                downloadButton("dl_ml_rf_imp_pdf", tagList(icon("download"), " PDF"), class = "btn-success btn-xs"))))
        )
      ))
    }
    # LASSO
    if (!is.null(rv$ml_cv_fit_lasso)) {
      out <- c(out, list(
        fluidRow(
          column(6, box(title = "LASSO: Coefficient path", width = NULL, status = "danger", solidHeader = TRUE,
                       plotOutput("ml_plot_lasso_path", height = "380px"),
                       tags$div(style = "margin-top:6px;",
                                downloadButton("dl_ml_lasso_path_png", tagList(icon("download"), " PNG (300 DPI)"), class = "btn-danger btn-xs", style = "margin-right:6px;"),
                                downloadButton("dl_ml_lasso_path_pdf", tagList(icon("download"), " PDF"), class = "btn-danger btn-xs")))),
          column(6, box(title = "LASSO: Binomial deviance vs lambda", width = NULL, status = "danger", solidHeader = TRUE,
                       plotOutput("ml_plot_lasso_deviance", height = "380px"),
                       tags$div(style = "margin-top:6px;",
                                downloadButton("dl_ml_lasso_dev_png", tagList(icon("download"), " PNG (300 DPI)"), class = "btn-danger btn-xs", style = "margin-right:6px;"),
                                downloadButton("dl_ml_lasso_dev_pdf", tagList(icon("download"), " PDF"), class = "btn-danger btn-xs"))))
        )
      ))
    }
    # Elastic Net
    if (!is.null(rv$ml_cv_fit_elastic)) {
      out <- c(out, list(
        fluidRow(
          column(6, box(title = "Elastic Net: Coefficient path", width = NULL, status = "info", solidHeader = TRUE,
                       plotOutput("ml_plot_elastic_path", height = "380px"),
                       tags$div(style = "margin-top:6px;",
                                downloadButton("dl_ml_elastic_path_png", tagList(icon("download"), " PNG (300 DPI)"), class = "btn-info btn-xs", style = "margin-right:6px;"),
                                downloadButton("dl_ml_elastic_path_pdf", tagList(icon("download"), " PDF"), class = "btn-info btn-xs")))),
          column(6, box(title = "Elastic Net: Binomial deviance vs lambda", width = NULL, status = "info", solidHeader = TRUE,
                       plotOutput("ml_plot_elastic_deviance", height = "380px"),
                       tags$div(style = "margin-top:6px;",
                                downloadButton("dl_ml_elastic_dev_png", tagList(icon("download"), " PNG (300 DPI)"), class = "btn-info btn-xs", style = "margin-right:6px;"),
                                downloadButton("dl_ml_elastic_dev_pdf", tagList(icon("download"), " PDF"), class = "btn-info btn-xs"))))
        )
      ))
    }
    # Ridge
    if (!is.null(rv$ml_cv_fit_ridge)) {
      out <- c(out, list(
        fluidRow(
          column(6, box(title = "Ridge: Coefficient path", width = NULL, status = "primary", solidHeader = TRUE,
                       plotOutput("ml_plot_ridge_path", height = "380px"),
                       tags$div(style = "margin-top:6px;",
                                downloadButton("dl_ml_ridge_path_png", tagList(icon("download"), " PNG (300 DPI)"), class = "btn-primary btn-xs", style = "margin-right:6px;"),
                                downloadButton("dl_ml_ridge_path_pdf", tagList(icon("download"), " PDF"), class = "btn-primary btn-xs")))),
          column(6, box(title = "Ridge: Binomial deviance vs lambda", width = NULL, status = "primary", solidHeader = TRUE,
                       plotOutput("ml_plot_ridge_deviance", height = "380px"),
                       tags$div(style = "margin-top:6px;",
                                downloadButton("dl_ml_ridge_dev_png", tagList(icon("download"), " PNG (300 DPI)"), class = "btn-primary btn-xs", style = "margin-right:6px;"),
                                downloadButton("dl_ml_ridge_dev_pdf", tagList(icon("download"), " PDF"), class = "btn-primary btn-xs"))))
        )
      ))
    }
    if (length(out) == 0) return(tags$p(icon("info-circle"), " No diagnostic plots (run LASSO, Elastic Net, Ridge, or Random Forest).", style = "color: #666;"))
    do.call(tagList, out)
  })

  # ---------- Plot helpers (used by renderPlot and download) ----------
  plot_rf_error <- function() {
    req(rv$ml_x, rv$ml_y)
    x <- rv$ml_x
    y <- rv$ml_y
    set.seed(123)
    train_idx <- sample(seq_len(nrow(x)), size = round(0.8 * nrow(x)))
    x_train <- x[train_idx, , drop = FALSE]
    y_train <- y[train_idx]
    x_test <- x[-train_idx, , drop = FALSE]
    y_test <- y[-train_idx]
    rf_train <- randomForest::randomForest(x_train, y_train, ntree = 500)
    train_pred <- predict(rf_train, x_train)
    test_pred <- predict(rf_train, x_test)
    train_err <- mean(train_pred != y_train)
    test_err <- mean(test_pred != y_test)
    oob_err <- rf_train$err.rate[, 1]
    ntrees <- seq_along(oob_err)
    par(mar = c(4.5, 4.5, 3.5, 1.5), cex.main = 1.0, cex.lab = 0.9, cex.axis = 0.8, mgp = c(2.5, 0.8, 0))
    plot(ntrees, oob_err, type = "l", col = "green", lwd = 2, xlab = "Number of Trees", ylab = "Error Rate",
         main = "Random Forest Error Rates", ylim = c(0, max(c(oob_err, train_err, test_err)) * 1.1))
    lines(ntrees, rep(train_err, length(ntrees)), col = "black", lty = 2, lwd = 2)
    lines(ntrees, rep(test_err, length(ntrees)), col = "red", lty = 3, lwd = 2)
    legend("topright", legend = c("OOB", "Training", "Test"), col = c("green", "black", "red"), lwd = 2, lty = c(1, 2, 3), cex = 0.85)
  }

  plot_rf_importance <- function() {
    req(rv$ml_rf_importance)
    df <- rv$ml_rf_importance[order(-rv$ml_rf_importance$MeanDecreaseAccuracy), ][seq_len(min(10L, nrow(rv$ml_rf_importance))), ]
    par(mar = c(4.5, 8, 3.5, 1.5), cex.main = 1.0, cex.lab = 0.9, cex.axis = 0.8, mgp = c(2.5, 0.8, 0))
    barplot(rev(df$MeanDecreaseAccuracy), names.arg = rev(df$Gene), horiz = TRUE, las = 1, col = "steelblue",
            main = "Top 10 Genes (MeanDecreaseAccuracy)", xlab = "Importance", cex.names = 0.8)
  }

  plot_lasso_path <- function() {
    req(rv$ml_cv_fit_lasso)
    par(mar = c(4.5, 4.5, 4, 1.5), cex.main = 1.0, cex.lab = 0.9, cex.axis = 0.8, mgp = c(2.5, 0.8, 0))
    plot(rv$ml_cv_fit_lasso$glmnet.fit, xvar = "lambda", label = FALSE)
    title("LASSO: Coefficient Path", line = 2.8, cex.main = 1.0)
  }
  plot_lasso_dev <- function() {
    req(rv$ml_cv_fit_lasso)
    par(mar = c(4.5, 4.5, 5.5, 1.5), cex.main = 1.0, cex.lab = 0.9, cex.axis = 0.8, mgp = c(2.5, 0.8, 0))
    plot(rv$ml_cv_fit_lasso, main = "")
    title("LASSO: Binomial Deviance vs log(lambda)", line = 4.2, cex.main = 0.95)
    abline(v = log(rv$ml_cv_fit_lasso$lambda.min), col = "red", lty = 2, lwd = 2)
  }
  plot_elastic_path <- function() {
    req(rv$ml_cv_fit_elastic)
    par(mar = c(4.5, 4.5, 4, 1.5), cex.main = 1.0, cex.lab = 0.9, cex.axis = 0.8, mgp = c(2.5, 0.8, 0))
    plot(rv$ml_cv_fit_elastic$glmnet.fit, xvar = "lambda", label = FALSE)
    title("Elastic Net: Coefficient Path", line = 2.8, cex.main = 1.0)
  }
  plot_elastic_dev <- function() {
    req(rv$ml_cv_fit_elastic)
    par(mar = c(4.5, 4.5, 5.5, 1.5), cex.main = 1.0, cex.lab = 0.9, cex.axis = 0.8, mgp = c(2.5, 0.8, 0))
    plot(rv$ml_cv_fit_elastic, main = "")
    title("Elastic Net: Binomial Deviance vs log(lambda)", line = 4.2, cex.main = 0.95)
    abline(v = log(rv$ml_cv_fit_elastic$lambda.min), col = "red", lty = 2, lwd = 2)
  }
  plot_ridge_path <- function() {
    req(rv$ml_cv_fit_ridge)
    par(mar = c(4.5, 4.5, 4, 1.5), cex.main = 1.0, cex.lab = 0.9, cex.axis = 0.8, mgp = c(2.5, 0.8, 0))
    plot(rv$ml_cv_fit_ridge$glmnet.fit, xvar = "lambda", label = FALSE)
    title("Ridge: Coefficient Path", line = 2.8, cex.main = 1.0)
  }
  plot_ridge_dev <- function() {
    req(rv$ml_cv_fit_ridge)
    par(mar = c(4.5, 4.5, 5.5, 1.5), cex.main = 1.0, cex.lab = 0.9, cex.axis = 0.8, mgp = c(2.5, 0.8, 0))
    plot(rv$ml_cv_fit_ridge, main = "")
    title("Ridge: Binomial Deviance vs log(lambda)", line = 4.2, cex.main = 0.95)
    abline(v = log(rv$ml_cv_fit_ridge$lambda.min), col = "red", lty = 2, lwd = 2)
  }

  # ---------- Render plots ----------
  output$ml_plot_rf_error <- renderPlot({ plot_rf_error() }, height = 380)

  # RF: Top 10 importance (MeanDecreaseAccuracy)
  output$ml_plot_rf_importance <- renderPlot({ plot_rf_importance() }, height = 380)

  # LASSO coefficient path
  output$ml_plot_lasso_path <- renderPlot({ plot_lasso_path() }, height = 380)

  # LASSO deviance vs lambda
  output$ml_plot_lasso_deviance <- renderPlot({ plot_lasso_dev() }, height = 380)

  # Elastic Net coefficient path
  output$ml_plot_elastic_path <- renderPlot({ plot_elastic_path() }, height = 380)

  # Elastic Net deviance vs lambda
  output$ml_plot_elastic_deviance <- renderPlot({ plot_elastic_dev() }, height = 380)

  # Ridge coefficient path
  output$ml_plot_ridge_path <- renderPlot({ plot_ridge_path() }, height = 380)

  # Ridge deviance vs lambda
  output$ml_plot_ridge_deviance <- renderPlot({ plot_ridge_dev() }, height = 380)

  # ---------- Downloads (PNG 300 DPI and PDF) ----------
  download_plot <- function(file, type = c("png", "pdf"), plotfun) {
    type <- match.arg(type)
    if (type == "png") {
      png(file, width = 8 * IMAGE_DPI, height = 6 * IMAGE_DPI, res = IMAGE_DPI)
    } else {
      pdf(file, width = 8, height = 6)
    }
    on.exit(dev.off(), add = TRUE)
    plotfun()
  }

  output$dl_ml_rf_error_png <- downloadHandler(
    filename = function() "rf_error_rates.png",
    content = function(file) download_plot(file, "png", plot_rf_error)
  )
  output$dl_ml_rf_error_pdf <- downloadHandler(
    filename = function() "rf_error_rates.pdf",
    content = function(file) download_plot(file, "pdf", plot_rf_error)
  )
  output$dl_ml_rf_imp_png <- downloadHandler(
    filename = function() "rf_top10_importance.png",
    content = function(file) download_plot(file, "png", plot_rf_importance)
  )
  output$dl_ml_rf_imp_pdf <- downloadHandler(
    filename = function() "rf_top10_importance.pdf",
    content = function(file) download_plot(file, "pdf", plot_rf_importance)
  )
  output$dl_ml_lasso_path_png <- downloadHandler(
    filename = function() "lasso_coefficient_path.png",
    content = function(file) download_plot(file, "png", plot_lasso_path)
  )
  output$dl_ml_lasso_path_pdf <- downloadHandler(
    filename = function() "lasso_coefficient_path.pdf",
    content = function(file) download_plot(file, "pdf", plot_lasso_path)
  )
  output$dl_ml_lasso_dev_png <- downloadHandler(
    filename = function() "lasso_deviance_vs_lambda.png",
    content = function(file) download_plot(file, "png", plot_lasso_dev)
  )
  output$dl_ml_lasso_dev_pdf <- downloadHandler(
    filename = function() "lasso_deviance_vs_lambda.pdf",
    content = function(file) download_plot(file, "pdf", plot_lasso_dev)
  )
  output$dl_ml_elastic_path_png <- downloadHandler(
    filename = function() "elastic_net_coefficient_path.png",
    content = function(file) download_plot(file, "png", plot_elastic_path)
  )
  output$dl_ml_elastic_path_pdf <- downloadHandler(
    filename = function() "elastic_net_coefficient_path.pdf",
    content = function(file) download_plot(file, "pdf", plot_elastic_path)
  )
  output$dl_ml_elastic_dev_png <- downloadHandler(
    filename = function() "elastic_net_deviance_vs_lambda.png",
    content = function(file) download_plot(file, "png", plot_elastic_dev)
  )
  output$dl_ml_elastic_dev_pdf <- downloadHandler(
    filename = function() "elastic_net_deviance_vs_lambda.pdf",
    content = function(file) download_plot(file, "pdf", plot_elastic_dev)
  )
  output$dl_ml_ridge_path_png <- downloadHandler(
    filename = function() "ridge_coefficient_path.png",
    content = function(file) download_plot(file, "png", plot_ridge_path)
  )
  output$dl_ml_ridge_path_pdf <- downloadHandler(
    filename = function() "ridge_coefficient_path.pdf",
    content = function(file) download_plot(file, "pdf", plot_ridge_path)
  )
  output$dl_ml_ridge_dev_png <- downloadHandler(
    filename = function() "ridge_deviance_vs_lambda.png",
    content = function(file) download_plot(file, "png", plot_ridge_dev)
  )
  output$dl_ml_ridge_dev_pdf <- downloadHandler(
    filename = function() "ridge_deviance_vs_lambda.pdf",
    content = function(file) download_plot(file, "pdf", plot_ridge_dev)
  )

  output$ml_venn_message <- renderUI({
    sets <- rv$ml_venn_sets
    if (is.null(sets)) return(NULL)
    n_sets <- length(sets)
    if (n_sets == 1) {
      return(tags$p(icon("info-circle"), " Venn/UpSet needs 2 or more methods. You ran 1 method; Final List shows that method's genes.", style = "margin-bottom: 10px; font-size: 12px; color: #555;"))
    }
    if (n_sets > 5) {
      return(tags$p(icon("info-circle"), " Showing UpSet plot for all ", n_sets, " selected methods (Venn supports up to 5 sets).", style = "margin-bottom: 10px; font-size: 12px; color: #555;"))
    }
    NULL
  })

  # Bright colors for Venn (up to 8 methods)
  ml_venn_fill <- c("#E53935", "#1E88E5", "#43A047", "#FB8C00", "#8E24AA", "#009688", "#795548", "#607D8B")
  ml_venn_cats <- c("LASSO", "Elastic Net", "Ridge", "Random Forest", "SVM-RFE", "Boruta", "sPLS-DA", "XGBoost+SHAP")

  output$ml_venn_plot <- renderPlot({
    req(rv$ml_venn_sets)
    sets <- rv$ml_venn_sets
    n_sets <- length(sets)
    if (n_sets < 2) return()
    if (n_sets > 5) {
      # UpSet plot for 6+ methods (shows all selected methods)
      all_genes <- unique(unlist(sets, use.names = FALSE))
      if (length(all_genes) == 0) return()
      upset_matrix <- matrix(0L, nrow = length(all_genes), ncol = n_sets)
      rownames(upset_matrix) <- all_genes
      colnames(upset_matrix) <- names(sets)
      for (i in seq_len(n_sets)) {
        g <- sets[[i]]
        upset_matrix[intersect(all_genes, g), i] <- 1L
      }
      upset_df <- as.data.frame(upset_matrix)
      max_set_size <- max(sapply(sets, length))
      tryCatch({
        UpSetR::upset(upset_df,
                      sets = colnames(upset_df),
                      keep.order = TRUE,
                      order.by = "freq",
                      main.bar.color = "#3498db",
                      sets.bar.color = ml_venn_fill[seq_len(n_sets)],
                      matrix.color = "#2ecc71",
                      point.size = 3.5,
                      line.size = 1,
                      text.scale = c(1.5, 1.2, 1.2, 1.1, 1.5, 1.2),
                      mb.ratio = c(0.6, 0.4),
                      set_size.show = TRUE,
                      set_size.scale_max = max(1, max_set_size * 1.1),
                      mainbar.y.label = "Gene set intersections",
                      sets.x.label = "Genes per method")
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("UpSet error:", conditionMessage(e)), cex = 1, col = "red")
      })
      return(invisible(NULL))
    }
    grid::grid.newpage()
    fill_colors <- if (n_sets <= 8) ml_venn_fill[seq_len(n_sets)] else rep(ml_venn_fill, length.out = n_sets)
    cat_names <- names(sets)
    if (is.null(cat_names)) cat_names <- ml_venn_cats[seq_len(n_sets)]
    vp <- VennDiagram::venn.diagram(
      x = sets,
      category.names = cat_names,
      filename = NULL,
      output = TRUE,
      disable.logging = TRUE,
      imagetype = "png",
      height = 2200,
      width = 2200,
      resolution = 200,
      compression = "lzw",
      lwd = 2.5,
      lty = "blank",
      fill = fill_colors,
      alpha = 0.65,
      cex = 1.4,
      fontface = "bold",
      cat.cex = 1.3,
      cat.fontface = "bold",
      cat.col = fill_colors,
      margin = 0.08,
      main = "Common Genes Across ML Methods",
      main.cex = 1.4,
      main.fontface = "bold"
    )
    grid::grid.draw(vp)
  }, height = 500)
  output$download_ml_venn <- downloadHandler(
    filename = function() {
      sets <- rv$ml_venn_sets
      n <- if (is.null(sets)) 0 else length(sets)
      if (n > 5) "upset_plot_ML_methods.png" else "venn_diagram_ML_methods.png"
    },
    content = function(file) {
      req(rv$ml_venn_sets)
      sets <- rv$ml_venn_sets
      n_sets <- length(sets)
      if (n_sets < 2) return()
      if (n_sets > 5) {
        all_genes <- unique(unlist(sets, use.names = FALSE))
        if (length(all_genes) == 0) return()
        upset_matrix <- matrix(0L, nrow = length(all_genes), ncol = n_sets)
        rownames(upset_matrix) <- all_genes
        colnames(upset_matrix) <- names(sets)
        for (i in seq_len(n_sets)) {
          g <- sets[[i]]
          upset_matrix[intersect(all_genes, g), i] <- 1L
        }
        upset_df <- as.data.frame(upset_matrix)
        max_set_size <- max(sapply(sets, length))
        png(file, width = 10 * IMAGE_DPI, height = 6.67 * IMAGE_DPI, res = IMAGE_DPI)
        tryCatch({
          UpSetR::upset(upset_df,
                        sets = colnames(upset_df),
                        keep.order = TRUE,
                        order.by = "freq",
                        main.bar.color = "#3498db",
                        sets.bar.color = ml_venn_fill[seq_len(n_sets)],
                        matrix.color = "#2ecc71",
                        point.size = 3.5,
                        line.size = 1,
                        text.scale = c(1.5, 1.2, 1.2, 1.1, 1.5, 1.2),
                        mb.ratio = c(0.6, 0.4),
                        set_size.show = TRUE,
                        set_size.scale_max = max(1, max_set_size * 1.1),
                        mainbar.y.label = "Gene set intersections",
                        sets.x.label = "Genes per method")
        }, error = function(e) {
          plot.new()
          text(0.5, 0.5, paste("UpSet error:", conditionMessage(e)), cex = 1, col = "red")
        })
        dev.off()
        return()
      }
      fill_colors <- if (n_sets <= 8) ml_venn_fill[seq_len(n_sets)] else rep(ml_venn_fill, length.out = n_sets)
      cat_names <- names(sets)
      if (is.null(cat_names)) cat_names <- ml_venn_cats[seq_len(n_sets)]
      png(file, width = 8 * IMAGE_DPI, height = 8 * IMAGE_DPI, res = IMAGE_DPI)
      grid::grid.newpage()
      vp <- VennDiagram::venn.diagram(
        x = sets,
        category.names = cat_names,
        filename = NULL,
        output = TRUE,
        disable.logging = TRUE,
        imagetype = "png",
        height = 8 * IMAGE_DPI,
        width = 8 * IMAGE_DPI,
        resolution = IMAGE_DPI,
        compression = "lzw",
        lwd = 2.5,
        lty = "blank",
        fill = fill_colors,
        alpha = 0.65,
        cex = 1.4,
        fontface = "bold",
        cat.cex = 1.3,
        cat.fontface = "bold",
        cat.col = fill_colors,
        margin = 0.08,
        main = "Common Genes Across ML Methods",
        main.cex = 1.4,
        main.fontface = "bold"
      )
      grid::grid.draw(vp)
      dev.off()
    }
  )

  output$download_ml_venn_pdf <- downloadHandler(
    filename = function() {
      sets <- rv$ml_venn_sets
      n <- if (is.null(sets)) 0 else length(sets)
      if (n > 5) "upset_plot_ML_methods.pdf" else "venn_diagram_ML_methods.pdf"
    },
    content = function(file) {
      req(rv$ml_venn_sets)
      sets <- rv$ml_venn_sets
      n_sets <- length(sets)
      if (n_sets < 2) return()
      if (n_sets > 5) {
        all_genes <- unique(unlist(sets, use.names = FALSE))
        if (length(all_genes) == 0) return()
        upset_matrix <- matrix(0L, nrow = length(all_genes), ncol = n_sets)
        rownames(upset_matrix) <- all_genes
        colnames(upset_matrix) <- names(sets)
        for (i in seq_len(n_sets)) {
          g <- sets[[i]]
          upset_matrix[intersect(all_genes, g), i] <- 1L
        }
        upset_df <- as.data.frame(upset_matrix)
        max_set_size <- max(sapply(sets, length))
        pdf(file, width = 10, height = 6.67)
        tryCatch({
          UpSetR::upset(upset_df,
                        sets = colnames(upset_df),
                        keep.order = TRUE,
                        order.by = "freq",
                        main.bar.color = "#3498db",
                        sets.bar.color = ml_venn_fill[seq_len(n_sets)],
                        matrix.color = "#2ecc71",
                        point.size = 3.5,
                        line.size = 1,
                        text.scale = c(1.5, 1.2, 1.2, 1.1, 1.5, 1.2),
                        mb.ratio = c(0.6, 0.4),
                        set_size.show = TRUE,
                        set_size.scale_max = max(1, max_set_size * 1.1),
                        mainbar.y.label = "Gene set intersections",
                        sets.x.label = "Genes per method")
        }, error = function(e) {
          plot.new()
          text(0.5, 0.5, paste("UpSet error:", conditionMessage(e)), cex = 1, col = "red")
        })
        dev.off()
        return()
      }
      fill_colors <- if (n_sets <= 8) ml_venn_fill[seq_len(n_sets)] else rep(ml_venn_fill, length.out = n_sets)
      cat_names <- names(sets)
      if (is.null(cat_names)) cat_names <- ml_venn_cats[seq_len(n_sets)]
      pdf(file, width = 8, height = 8)
      grid::grid.newpage()
      vp <- VennDiagram::venn.diagram(
        x = sets,
        category.names = cat_names,
        filename = NULL,
        output = TRUE,
        disable.logging = TRUE,
        imagetype = "png",
        height = 8,
        width = 8,
        resolution = 72,
        fill = fill_colors,
        alpha = 0.65,
        cex = 1.4,
        fontface = "bold",
        cat.cex = 1.3,
        cat.fontface = "bold",
        cat.col = fill_colors,
        margin = 0.08,
        main = "Common Genes Across ML Methods",
        main.cex = 1.4,
        main.fontface = "bold"
      )
      grid::grid.draw(vp)
      dev.off()
    }
  )
}
