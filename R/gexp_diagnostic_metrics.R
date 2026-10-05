# ==============================================================================
# GEXP_DIAGNOSTIC_METRICS.R - helpers for the diagnostic (logistic) model step
# ==============================================================================
# Pure functions (no Shiny state) used by server_nomogram.R: outcome coding,
# input checks, performance metrics with 95% CIs, calibration statistics,
# confusion matrices and publication-style base-graphics figures.
# Conventions: outcome 1 = Disease (positive class), 0 = Normal.
# ==============================================================================

#' Methods text reported alongside every confidence interval
#' @noRd
gexp_diag_ci_methods <- function() {
  paste0(
    "Accuracy, sensitivity, specificity, PPV, NPV: exact Clopper-Pearson 95% CI (binomial). ",
    "AUC: DeLong 95% CI. Calibration intercept/slope: Wald 95% CI from logistic recalibration."
  )
}

#' Exact (Clopper-Pearson) binomial estimate and 95% CI
#' @return named numeric c(est, lower, upper); NA when n is 0
#' @noRd
gexp_diag_binom_ci <- function(x, n, conf.level = 0.95) {
  if (!is.finite(n) || n <= 0) {
    return(c(est = NA_real_, lower = NA_real_, upper = NA_real_))
  }
  ci <- stats::binom.test(as.integer(x), as.integer(n), conf.level = conf.level)$conf.int
  c(est = x / n, lower = ci[[1L]], upper = ci[[2L]])
}

#' Decide which class is coded 1 (Disease) and which 0 (Normal)
#'
#' Exact labels "Disease"/"Normal" (any case) are used first. Otherwise a
#' keyword heuristic is applied and the result is flagged `heuristic = TRUE`
#' so the caller can show it to the user; the coding is never reversed
#' silently.
#' @param grp Character/factor vector of group labels (NA allowed).
#' @return list(outcome, disease_label, normal_label, rule, heuristic, group_col, table)
#' @noRd
gexp_diag_resolve_outcome <- function(grp, group_col = "Condition") {
  grp <- trimws(as.character(grp))
  grp[grp == ""] <- NA_character_
  u <- unique(grp[!is.na(grp)])
  if (length(u) == 0L) {
    stop("No group values in column '", group_col, "'.", call. = FALSE)
  }
  if (length(u) == 1L) {
    stop("Only one group found: '", u[1L], "'. Need exactly two groups.", call. = FALSE)
  }
  if (length(u) > 2L) {
    stop("Column '", group_col, "' has ", length(u), " values. The diagnostic model needs exactly two.", call. = FALSE)
  }
  lu <- tolower(u)
  heuristic <- FALSE
  if (all(c("disease", "normal") %in% lu)) {
    disease_label <- u[lu == "disease"][1L]
    rule <- "Exact labels: 'Disease' = 1, 'Normal' = 0"
  } else {
    disease_like <- grepl("disease|case|asd|treatment|2|tumor|patient", u, ignore.case = TRUE)
    normal_like <- grepl("normal|control|healthy|1|non|ctrl", u, ignore.case = TRUE)
    if (any(disease_like) && any(normal_like)) {
      disease_label <- u[which(disease_like)[1L]]
      rule <- "Keyword heuristic (label matched disease/case/tumor/patient wording): that class = 1"
    } else {
      disease_label <- u[2L]
      rule <- "No Disease/Normal wording found: the second group in data order = 1"
    }
    heuristic <- TRUE
  }
  normal_label <- setdiff(u, disease_label)[1L]
  outcome <- ifelse(is.na(grp), NA_integer_, as.integer(grp == disease_label))
  list(
    outcome = outcome,
    disease_label = disease_label,
    normal_label = normal_label,
    rule = rule,
    heuristic = heuristic,
    group_col = group_col,
    table = data.frame(
      Class = c("Disease (positive)", "Normal (negative)"),
      Coded_As = c(1L, 0L),
      Source_Label = c(disease_label, normal_label),
      N = c(sum(grp == disease_label, na.rm = TRUE), sum(grp == normal_label, na.rm = TRUE)),
      Source_Column = group_col,
      Rule = rule,
      stringsAsFactors = FALSE
    )
  )
}

#' Validate a modelling data frame; returns a character vector of problems
#' @noRd
gexp_diag_check_frame <- function(df, predictors, outcome_col = "Outcome", what = "Data", min_per_class = 1L) {
  problems <- character(0)
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
    return(paste0(what, ": no samples."))
  }
  miss <- setdiff(c(predictors, outcome_col), names(df))
  if (length(miss) > 0L) {
    problems <- c(problems, paste0(what, ": missing column(s): ", paste(miss, collapse = ", "), "."))
    return(problems)
  }
  non_num <- predictors[!vapply(predictors, function(g) is.numeric(df[[g]]), logical(1))]
  if (length(non_num) > 0L) {
    problems <- c(problems, paste0(what, ": non-numeric predictor(s): ", paste(non_num, collapse = ", "), "."))
  }
  y <- df[[outcome_col]]
  if (anyNA(y)) problems <- c(problems, paste0(what, ": missing outcome values."))
  if (!all(y[!is.na(y)] %in% c(0, 1))) problems <- c(problems, paste0(what, ": outcome must be coded 0/1."))
  n1 <- sum(y == 1, na.rm = TRUE)
  n0 <- sum(y == 0, na.rm = TRUE)
  if (n1 == 0L || n0 == 0L) {
    problems <- c(problems, paste0(what, ": only one outcome class present (Disease = ", n1, ", Normal = ", n0, ")."))
  } else if (min(n1, n0) < min_per_class) {
    problems <- c(problems, paste0(what, ": fewer than ", min_per_class, " samples in the smaller class (Disease = ", n1, ", Normal = ", n0, ")."))
  }
  problems
}

#' Drop rows with NA/NaN/Inf in any predictor; reports how many were removed
#' @return list(df, n_dropped)
#' @noRd
gexp_diag_drop_nonfinite <- function(df, predictors) {
  bad <- rep(FALSE, nrow(df))
  for (g in predictors) {
    v <- suppressWarnings(as.numeric(df[[g]]))
    bad <- bad | !is.finite(v)
  }
  list(df = df[!bad, , drop = FALSE], n_dropped = sum(bad))
}

#' Performance of fixed predicted probabilities at a FIXED threshold
#'
#' The threshold is an input: it is never re-estimated here, so the same
#' training-derived threshold can be applied unchanged to validation data.
#' @return One-row data.frame (columns in the order used by the app's
#'   Performance Comparison table) with 95% CIs and counts.
#' @noRd
gexp_diag_performance <- function(actual, pred_prob, threshold, dataset = NA_character_) {
  actual <- as.integer(actual)
  pred_prob <- as.numeric(pred_prob)
  if (length(actual) != length(pred_prob)) stop("Outcome and prediction lengths differ.", call. = FALSE)
  if (anyNA(actual) || !all(actual %in% 0:1)) stop("Outcome must be coded 0/1 with no missing values.", call. = FALSE)
  if (any(!is.finite(pred_prob))) stop("Predicted probabilities contain NA/Inf.", call. = FALSE)
  n1 <- sum(actual == 1L)
  n0 <- sum(actual == 0L)
  if (n1 == 0L || n0 == 0L) stop("Only one outcome class present.", call. = FALSE)
  if (!is.finite(threshold)) stop("Classification threshold is not finite.", call. = FALSE)

  pred_class <- as.integer(pred_prob > threshold)
  TP <- sum(pred_class == 1L & actual == 1L)
  TN <- sum(pred_class == 0L & actual == 0L)
  FP <- sum(pred_class == 1L & actual == 0L)
  FN <- sum(pred_class == 0L & actual == 1L)
  N <- length(actual)
  acc <- gexp_diag_binom_ci(TP + TN, N)
  sens <- gexp_diag_binom_ci(TP, n1)
  spec <- gexp_diag_binom_ci(TN, n0)
  ppv <- gexp_diag_binom_ci(TP, TP + FP)
  npv <- gexp_diag_binom_ci(TN, TN + FN)

  roc_obj <- pROC::roc(actual, pred_prob, levels = c(0, 1), direction = "<", quiet = TRUE)
  auc_ci <- as.numeric(suppressWarnings(pROC::ci.auc(roc_obj, method = "delong")))
  # Caveats shown next to the numbers (not just in the console)
  note <- character(0)
  if (is.finite(auc_ci[1L]) && is.finite(auc_ci[3L]) && auc_ci[1L] == auc_ci[3L]) {
    note <- c(note, "AUC CI is degenerate (the DeLong interval collapses when AUC = 1); judge it by the sample size")
  }
  if (min(n1, n0) < 10L) {
    note <- c(note, sprintf("Small class (n = %d): all intervals are very wide", min(n1, n0)))
  }

  data.frame(
    N_Total = N, N_Disease = n1, N_Normal = n0,
    Accuracy = acc[["est"]], Accuracy_Lower = acc[["lower"]], Accuracy_Upper = acc[["upper"]],
    Sensitivity = sens[["est"]], Sensitivity_Lower = sens[["lower"]], Sensitivity_Upper = sens[["upper"]],
    Specificity = spec[["est"]], Specificity_Lower = spec[["lower"]], Specificity_Upper = spec[["upper"]],
    PPV = ppv[["est"]], PPV_Lower = ppv[["lower"]], PPV_Upper = ppv[["upper"]],
    NPV = npv[["est"]], NPV_Lower = npv[["lower"]], NPV_Upper = npv[["upper"]],
    AUC = auc_ci[2L], AUC_Lower = auc_ci[1L], AUC_Upper = auc_ci[3L],
    Threshold = threshold,
    Dataset = as.character(dataset),
    TP = TP, TN = TN, FP = FP, FN = FN,
    Sensitivity_n = paste0(TP, "/", n1),
    Specificity_n = paste0(TN, "/", n0),
    PPV_n = if (TP + FP > 0L) paste0(TP, "/", TP + FP) else NA_character_,
    NPV_n = if (TN + FN > 0L) paste0(TN, "/", TN + FN) else NA_character_,
    Note = paste(note, collapse = "; "),
    stringsAsFactors = FALSE
  )
}

#' Long-format confusion matrix (counts and percentages) from a performance row
#' @noRd
gexp_diag_confusion_table <- function(perf) {
  N <- perf$N_Total
  cells <- data.frame(
    Dataset = perf$Dataset,
    Cell = c("TP", "TN", "FP", "FN"),
    Description = c("Disease predicted Disease", "Normal predicted Normal",
                    "Normal predicted Disease", "Disease predicted Normal"),
    Count = c(perf$TP, perf$TN, perf$FP, perf$FN),
    stringsAsFactors = FALSE
  )
  cells$Percent_of_Total <- round(100 * cells$Count / N, 2)
  denom <- c(perf$N_Disease, perf$N_Normal, perf$N_Normal, perf$N_Disease)
  cells$Percent_of_Actual_Class <- round(100 * cells$Count / denom, 2)
  cells$Threshold <- perf$Threshold
  cells
}

#' Calibration statistics (intercept = calibration-in-the-large, slope, Brier)
#'
#' Intercept: logistic model with the linear predictor as a fixed offset
#' (calibration-in-the-large, slope fixed at 1); `Intercept_Joint` is the
#' intercept of the slope fit and is only used to draw the curve.
#' Slope: logistic regression of the outcome on the linear predictor. Both are
#' returned as NA with an explanatory Note when the smaller class has fewer than
#' `min_per_class` samples or the fit is unstable (near-perfect separation) -
#' nothing is estimated from too little data.
#' @noRd
gexp_diag_calibration <- function(actual, pred_prob, dataset = NA_character_, min_per_class = 10L) {
  actual <- as.integer(actual)
  pred_prob <- as.numeric(pred_prob)
  ok <- !is.na(actual) & is.finite(pred_prob)
  actual <- actual[ok]
  pred_prob <- pred_prob[ok]
  n1 <- sum(actual == 1L)
  n0 <- sum(actual == 0L)
  out <- data.frame(
    Dataset = as.character(dataset), N = length(actual), N_Disease = n1, N_Normal = n0,
    Intercept = NA_real_, Intercept_Lower = NA_real_, Intercept_Upper = NA_real_,
    Slope = NA_real_, Slope_Lower = NA_real_, Slope_Upper = NA_real_,
    Brier = if (length(actual) > 0L) mean((pred_prob - actual)^2) else NA_real_,
    Intercept_Joint = NA_real_,
    Note = "", stringsAsFactors = FALSE
  )
  if (min(n1, n0) < min_per_class) {
    out$Note <- paste0("Intercept/slope not estimated: fewer than ", min_per_class,
                       " samples in the smaller class (Disease = ", n1, ", Normal = ", n0, ").")
    return(out)
  }
  eps <- 1e-8
  lp <- stats::qlogis(pmin(pmax(pred_prob, eps), 1 - eps))
  fit_state <- new.env(parent = emptyenv())
  fit_state$warned <- FALSE
  fit_quiet <- function(expr) {
    withCallingHandlers(
      tryCatch(expr, error = function(e) NULL),
      warning = function(w) { fit_state$warned <- TRUE; invokeRestart("muffleWarning") }
    )
  }
  fit_s <- fit_quiet(stats::glm(actual ~ lp, family = stats::binomial()))
  fit_i <- fit_quiet(stats::glm(actual ~ 1, offset = lp, family = stats::binomial()))
  # A saturated / constant linear predictor leaves the slope NA (no "lp" row): treat as unstable.
  se_s <- tryCatch(summary(fit_s)$coefficients["lp", "Std. Error"], error = function(e) NA_real_)
  se_i <- tryCatch(summary(fit_i)$coefficients[1L, "Std. Error"], error = function(e) NA_real_)
  unstable <- fit_state$warned || is.null(fit_s) || is.null(fit_i) || anyNA(stats::coef(fit_s)) || anyNA(stats::coef(fit_i)) ||
    !is.finite(se_s) || !is.finite(se_i) || se_s > 10 || se_i > 10
  if (unstable) {
    out$Note <- "Intercept/slope unstable (near-perfect separation or non-convergence): not reported."
    return(out)
  }
  z <- stats::qnorm(0.975)
  b <- unname(stats::coef(fit_s)["lp"])
  a <- unname(stats::coef(fit_i)[1L])
  out$Slope <- b; out$Slope_Lower <- b - z * se_s; out$Slope_Upper <- b + z * se_s
  out$Intercept <- a; out$Intercept_Lower <- a - z * se_i; out$Intercept_Upper <- a + z * se_i
  # Intercept of the SAME fit that produced the slope: logit(P(Y=1)) = a_joint + b * LP.
  # The plotted recalibration curve must use (a_joint, b) together; mixing the
  # calibration-in-the-large intercept (slope fixed at 1) with the fitted slope
  # draws a curve that matches neither fit.
  out$Intercept_Joint <- unname(stats::coef(fit_s)[1L])
  out$Note <- "Apparent estimates on this dataset (Wald 95% CI)."
  out
}

# ------------------------------------------------------------------------------
# Figures (base graphics; sized for manuscripts, 300 DPI is set by the device)
# ------------------------------------------------------------------------------

#' Publication-style ROC curve(s) with AUC, 95% CI and the fixed threshold point
#' @param rocs List of pROC objects.
#' @param perf List of one-row performance data.frames (same order as `rocs`).
#' @noRd
gexp_diag_plot_roc <- function(rocs, perf, labels, cols, title = "ROC curve") {
  op <- graphics::par(mar = c(6.6, 5.2, 3.6, 1.5), cex.axis = 1.15, cex.lab = 1.35,
                      cex.main = 1.5, las = 1, mgp = c(3.2, 0.9, 0))
  on.exit(graphics::par(op), add = TRUE)
  graphics::plot(NA, xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i", asp = 1,
                 xlab = "1 - Specificity (false-positive rate)",
                 ylab = "Sensitivity (true-positive rate)", main = title, font.main = 2)
  graphics::abline(0, 1, lty = 2, col = "grey55", lwd = 1.5)
  leg <- character(0)
  for (i in seq_along(rocs)) {
    cc <- pROC::coords(rocs[[i]], x = "all", ret = c("specificity", "sensitivity"), transpose = FALSE)
    graphics::lines(1 - cc$specificity, cc$sensitivity, col = cols[i], lwd = 3.2)
    p <- perf[[i]]
    graphics::points(1 - p$Specificity, p$Sensitivity, pch = 21, bg = cols[i], col = "white", cex = 2.1, lwd = 1.5)
    leg[i] <- sprintf("%s: AUC %.3f (95%% CI %.3f-%.3f)", labels[i], p$AUC, p$AUC_Lower, p$AUC_Upper)
  }
  graphics::legend("bottomright", legend = leg, col = cols, lwd = 3.2, bty = "n", cex = 1.05, seg.len = 1.6)
  thr <- unique(round(vapply(perf, function(p) p$Threshold, numeric(1)), 3))
  graphics::mtext(sprintf("Circle = operating point at the training-derived threshold (%s)", paste(thr, collapse = ", ")),
                  side = 1, line = 4.6, cex = 0.85, col = "grey30")
  invisible(NULL)
}

#' Publication-style calibration plot (binned observed vs predicted + fitted line)
#' @param cal_row One-row output of gexp_diag_calibration().
#' @param curve Optional data.frame(predy, calibrated) for a bias-corrected curve.
#' @noRd
gexp_diag_plot_calibration <- function(pred, actual, cal_row, col, title, curve = NULL, n_bins = 10L, extra = NULL) {
  op <- graphics::par(mar = c(5, 5.2, 3.6, 1.5), cex.axis = 1.15, cex.lab = 1.35,
                      cex.main = 1.5, las = 1, mgp = c(3.2, 0.9, 0))
  on.exit(graphics::par(op), add = TRUE)
  graphics::plot(NA, xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i", asp = 1,
                 xlab = "Predicted probability of Disease",
                 ylab = "Observed proportion with Disease", main = title, font.main = 2)
  graphics::abline(0, 1, lty = 2, col = "grey55", lwd = 1.5)
  pred <- as.numeric(pred); actual <- as.integer(actual)
  ok <- is.finite(pred) & !is.na(actual)
  pred <- pred[ok]; actual <- actual[ok]
  # about >= 10 samples per bin so the per-bin CIs are informative
  k <- max(2L, min(as.integer(n_bins), length(pred) %/% 10L))
  brk <- unique(stats::quantile(pred, probs = seq(0, 1, length.out = k + 1L), na.rm = TRUE))
  op_xpd <- graphics::par(xpd = TRUE)  # keep markers/bars at 0 and 1 from being clipped
  on.exit(graphics::par(op_xpd), add = TRUE)
  if (length(brk) >= 3L) {
    bin <- cut(pred, breaks = brk, include.lowest = TRUE)
    for (b in levels(bin)) {
      idx <- which(bin == b)
      if (length(idx) < 2L) next
      ci <- gexp_diag_binom_ci(sum(actual[idx]), length(idx))
      x <- mean(pred[idx])
      graphics::segments(x, ci[["lower"]], x, ci[["upper"]], col = col, lwd = 1.8)
      graphics::points(x, ci[["est"]], pch = 21, bg = col, col = "white", cex = 1.7 + 0.04 * length(idx))
    }
  }
  if (!is.null(curve) && nrow(curve) > 1L) {
    graphics::lines(curve[[1L]], curve[[2L]], col = grDevices::adjustcolor("black", 0.7), lty = 3, lwd = 2.2)
  }
  if (is.finite(cal_row$Slope) && is.finite(cal_row$Intercept_Joint)) {
    xs <- seq(0.001, 0.999, length.out = 200)
    graphics::lines(xs, stats::plogis(cal_row$Intercept_Joint + cal_row$Slope * stats::qlogis(xs)), col = col, lwd = 3)
  }
  graphics::rug(pred, col = grDevices::adjustcolor(col, 0.45), ticksize = 0.03)
  fmt_ci <- function(est, lo, up) {
    if (is.finite(est)) sprintf("%.2f (%.2f to %.2f)", est, lo, up) else "not estimated"
  }
  txt <- c(
    sprintf("N = %d (Disease %d, Normal %d)", cal_row$N, cal_row$N_Disease, cal_row$N_Normal),
    paste0("Intercept: ", fmt_ci(cal_row$Intercept, cal_row$Intercept_Lower, cal_row$Intercept_Upper)),
    paste0("Slope: ", fmt_ci(cal_row$Slope, cal_row$Slope_Lower, cal_row$Slope_Upper)),
    sprintf("Brier score: %.3f", cal_row$Brier),
    extra
  )
  graphics::legend("topleft", legend = txt, bty = "n", cex = 0.98, text.col = "grey15")
  invisible(NULL)
}

#' Publication-style confusion-matrix figure (counts and % of total)
#' @noRd
gexp_diag_plot_confusion <- function(perf, title, col) {
  op <- graphics::par(mar = c(2.2, 6.5, 1, 1))
  on.exit(graphics::par(op), add = TRUE)
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 2), ylim = c(-0.1, 2.55), asp = 1)
  N <- perf$N_Total
  # rows = actual (Normal top, Disease bottom); columns = predicted (Normal, Disease)
  cells <- list(
    list(x = 0, y = 1, n = perf$TN, lab = "True negative (TN)", ok = TRUE),
    list(x = 1, y = 1, n = perf$FP, lab = "False positive (FP)", ok = FALSE),
    list(x = 0, y = 0, n = perf$FN, lab = "False negative (FN)", ok = FALSE),
    list(x = 1, y = 0, n = perf$TP, lab = "True positive (TP)", ok = TRUE)
  )
  for (cl in cells) {
    fill <- if (cl$ok) grDevices::adjustcolor(col, 0.25 + 0.55 * cl$n / max(1, N)) else
      grDevices::adjustcolor("grey60", 0.15 + 0.35 * cl$n / max(1, N))
    graphics::rect(cl$x, cl$y, cl$x + 1, cl$y + 1, col = fill, border = "white", lwd = 3)
    graphics::text(cl$x + 0.5, cl$y + 0.62, cl$n, cex = 2.6, font = 2)
    graphics::text(cl$x + 0.5, cl$y + 0.36, sprintf("%.1f%% of total", 100 * cl$n / N), cex = 1.0)
    graphics::text(cl$x + 0.5, cl$y + 0.16, cl$lab, cex = 0.95, col = "grey25")
  }
  graphics::text(0.5, 2.15, "Predicted\nNormal", cex = 1.2, font = 2)
  graphics::text(1.5, 2.15, "Predicted\nDisease", cex = 1.2, font = 2)
  graphics::text(-0.05, 1.5, "Actual\nNormal", cex = 1.2, font = 2, adj = 1, xpd = NA)
  graphics::text(-0.05, 0.5, "Actual\nDisease", cex = 1.2, font = 2, adj = 1, xpd = NA)
  graphics::text(1, 2.45, title, font = 2, cex = 1.6, xpd = NA)
  graphics::mtext(sprintf("Threshold %.3f | Sensitivity %s | Specificity %s",
                          perf$Threshold, perf$Sensitivity_n, perf$Specificity_n),
                  side = 1, line = 0.8, cex = 0.95, col = "grey30")
  invisible(NULL)
}


# ------------------------------------------------------------------------------
# Cross-cohort checks and standardization helpers
# ------------------------------------------------------------------------------

#' Per-gene AUC with the direction FIXED (never re-chosen per dataset)
#'
#' pROC's default `direction = "auto"` picks, in every dataset separately, the
#' direction that gives the larger AUC, so a gene that goes UP in disease in
#' training but DOWN in validation would still show a high validation AUC.
#' `direction = "<"` means higher values = Disease; ">" means lower = Disease.
#' When `direction` is NULL it is determined from this data (training).
#' @return list(roc, auc, direction)
#' @noRd
gexp_diag_directional_roc <- function(y, x, direction = NULL) {
  y <- as.integer(y)
  ok <- !is.na(y) & is.finite(x)
  y <- y[ok]; x <- x[ok]
  if (length(unique(y)) < 2L) return(NULL)
  if (is.null(direction)) {
    direction <- if (stats::median(x[y == 1L]) >= stats::median(x[y == 0L])) "<" else ">"
  }
  roc_obj <- pROC::roc(y, x, levels = c(0, 1), direction = direction, quiet = TRUE)
  list(roc = roc_obj, auc = as.numeric(pROC::auc(roc_obj)), direction = direction)
}

#' DeLong 95% CI of an AUC from a pROC object (c(lower, upper); NA when not estimable)
#' @noRd
gexp_diag_auc_ci <- function(roc_obj, conf.level = 0.95) {
  if (is.null(roc_obj)) return(c(NA_real_, NA_real_))
  ci <- tryCatch(suppressWarnings(as.numeric(pROC::ci.auc(roc_obj, conf.level = conf.level, method = "delong"))),
                 error = function(e) rep(NA_real_, 3))
  c(max(0, ci[1]), min(1, ci[3]))
}

#' AUC of a housekeeping/negative-control gene panel (two-sided, 0.5 = no signal)
#' @param expr_df samples x genes; @param outcome 0/1 aligned to rows
#' @return data.frame(Gene, AUC) for the controls found, or NULL
#' @noRd
gexp_diag_housekeeping_auc <- function(expr_df, outcome,
                                       controls = c("GAPDH", "ACTB", "B2M", "PPIA", "RPLP0", "TBP", "HPRT1", "PGK1")) {
  g <- intersect(controls, colnames(expr_df))
  if (length(g) == 0L || length(unique(outcome)) < 2L) return(NULL)
  aucs <- vapply(g, function(k) {
    r <- gexp_diag_directional_roc(outcome, as.numeric(expr_df[[k]]))
    if (is.null(r)) NA_real_ else max(r$auc, 1 - r$auc)
  }, numeric(1))
  data.frame(Gene = g, AUC = aucs, stringsAsFactors = FALSE)
}

#' Weighted mean and (n - 1)-style standard deviation with frequency weights
#' @noRd
gexp_diag_weighted_moments <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  x <- x[ok]; w <- w[ok]
  sw <- sum(w)
  m <- sum(w * x) / sw
  v <- sum(w * (x - m)^2) / sw
  n <- length(x)
  c(mean = m, sd = sqrt(v * n / max(1, n - 1)))
}

#' Compare each model gene's raw scale in training vs validation
#' @return data.frame(Gene, Train_Mean, Train_SD, Val_Mean, Val_SD, Shift_in_Train_SD, Flag)
#' @noRd
gexp_diag_scale_check <- function(train_df, val_df, genes, flag_at = 2) {
  do.call(rbind, lapply(genes, function(g) {
    tm <- mean(train_df[[g]], na.rm = TRUE); ts <- stats::sd(train_df[[g]], na.rm = TRUE)
    vm <- mean(val_df[[g]], na.rm = TRUE); vs <- stats::sd(val_df[[g]], na.rm = TRUE)
    shift <- if (is.finite(ts) && ts > 0) (vm - tm) / ts else NA_real_
    data.frame(Gene = g, Train_Mean = tm, Train_SD = ts, Val_Mean = vm, Val_SD = vs,
               Shift_in_Train_SD = shift, Flag = is.finite(shift) && abs(shift) > flag_at,
               stringsAsFactors = FALSE)
  }))
}
