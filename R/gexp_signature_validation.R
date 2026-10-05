# ==============================================================================
# GEXP_SIGNATURE_VALIDATION.R - validate a biomarker SIGNATURE (not single genes)
# ==============================================================================
# A signature score = mean of signed, standardized expression of the selected
# genes (sign = direction learned on the TRAINING datasets). Its AUC in an
# independent cohort is compared with random gene sets, and the same procedure
# is run leave-one-dataset-out across the training datasets.
# Gene-by-gene significance and fold-change cutoffs do not transfer between
# platforms/tissues (different scales); a rank-based signature AUC does.
# Outcome coding: 1 = Disease, 0 = Normal. Expression: genes x samples.
# ==============================================================================

#' Per-gene Welch t between Disease (1) and Normal (0); returns signed z
#' @return list(z, logFC, n) named by gene (NA when a group has < 2 values)
#' @noRd
gexp_sig_gene_z <- function(expr, y) {
  y <- as.integer(y)
  x1 <- expr[, y == 1L, drop = FALSE]; x0 <- expr[, y == 0L, drop = FALSE]
  n1 <- rowSums(!is.na(x1)); n0 <- rowSums(!is.na(x0))
  m1 <- rowMeans(x1, na.rm = TRUE); m0 <- rowMeans(x0, na.rm = TRUE)
  v1 <- rowSums((x1 - m1)^2, na.rm = TRUE) / pmax(n1 - 1, 1)
  v0 <- rowSums((x0 - m0)^2, na.rm = TRUE) / pmax(n0 - 1, 1)
  se2 <- v1 / pmax(n1, 1) + v0 / pmax(n0, 1)
  tt <- (m1 - m0) / sqrt(se2)
  df <- se2^2 / ((v1 / pmax(n1, 1))^2 / pmax(n1 - 1, 1) + (v0 / pmax(n0, 1))^2 / pmax(n0 - 1, 1))
  ok <- n1 >= 2L & n0 >= 2L & is.finite(tt) & is.finite(df) & df > 0
  z <- rep(NA_real_, length(tt)); z[ok] <- stats::qnorm(stats::pt(tt[ok], df[ok]))
  z[ok & is.infinite(z)] <- NA_real_
  list(z = stats::setNames(z, rownames(expr)), logFC = stats::setNames(m1 - m0, rownames(expr)),
       n = stats::setNames(n1 + n0, rownames(expr)))
}

#' Cross-dataset meta-analysis (Stouffer, sqrt(n)-weighted) of Disease vs Normal
#'
#' Each dataset is analysed on its own (so platform and batch scale never mix);
#' only datasets with >= 2 samples per group contribute. A gene is "consistent"
#' when every dataset that measures it agrees on the direction.
#' @return data.frame(Gene, Z, P, FDR, Direction, N_Datasets, Consistent, <dataset>_Z...)
#' @noRd
gexp_sig_meta <- function(expr, y, dataset) {
  dataset <- as.character(dataset)
  ds <- unique(dataset)
  zs <- list(); ns <- list()
  for (d in ds) {
    ix <- which(dataset == d)
    yy <- y[ix]
    if (sum(yy == 1L) < 2L || sum(yy == 0L) < 2L) next
    r <- gexp_sig_gene_z(expr[, ix, drop = FALSE], yy)
    zs[[d]] <- r$z; ns[[d]] <- length(ix)
  }
  if (length(zs) < 1L) stop("No dataset has at least 2 Normal and 2 Disease samples.", call. = FALSE)
  Z <- do.call(cbind, zs)
  w <- sqrt(unlist(ns))
  num <- rowSums(Z * rep(w, each = nrow(Z)), na.rm = TRUE)
  den <- sqrt(rowSums((!is.na(Z)) * rep(w^2, each = nrow(Z))))
  zm <- num / den
  zm[den == 0] <- NA_real_
  p <- 2 * stats::pnorm(-abs(zm))
  nd <- rowSums(!is.na(Z))
  consistent <- apply(sign(Z), 1L, function(r) { r <- r[!is.na(r) & r != 0]; length(r) > 0L && length(unique(r)) == 1L })
  out <- data.frame(Gene = rownames(Z), Z = zm, P = p, FDR = stats::p.adjust(p, "BH"),
                    Direction = ifelse(zm >= 0, "Up in Disease", "Down in Disease"),
                    N_Datasets = nd, Consistent = consistent, stringsAsFactors = FALSE)
  colnames(Z) <- paste0(colnames(Z), "_Z")
  cbind(out, as.data.frame(Z), row.names = NULL)
}

#' Genes passing the training criteria, strongest first
#' @noRd
gexp_sig_select <- function(meta, top_n = 100L, fdr = 0.05, min_datasets = 2L, require_consistent = TRUE) {
  ok <- !is.na(meta$FDR) & meta$FDR < fdr & meta$N_Datasets >= min_datasets & (!require_consistent | meta$Consistent)
  m <- meta[ok, , drop = FALSE]
  m <- m[order(-abs(m$Z)), , drop = FALSE]
  head(m$Gene, top_n)
}

#' Signature score per sample = mean of sign * z-scored expression
#' @param expr genes x samples
#' @noRd
gexp_sig_score <- function(expr, genes, signs) {
  genes <- intersect(genes, rownames(expr))
  z <- t(scale(t(expr[genes, , drop = FALSE])))
  z[!is.finite(z)] <- NA_real_
  colMeans(z * signs[genes], na.rm = TRUE)
}

# rank-based AUC (Mann-Whitney); fast enough for thousands of permutations
.gexp_sig_auc <- function(y, s) {
  ok <- is.finite(s); y <- y[ok]; s <- s[ok]
  n1 <- sum(y == 1L); n0 <- sum(y == 0L)
  if (n1 == 0L || n0 == 0L) return(NA_real_)
  (sum(rank(s)[y == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

.gexp_sig_auc_ci <- function(y, s) {
  r <- tryCatch(pROC::roc(y, s, levels = c(0, 1), direction = "<", quiet = TRUE), error = function(e) NULL)
  if (is.null(r)) return(c(NA_real_, NA_real_, NA_real_))
  ci <- tryCatch(as.numeric(suppressWarnings(pROC::ci.auc(r, method = "delong"))), error = function(e) rep(NA_real_, 3))
  c(ci[2L], ci[1L], ci[3L])
}

#' Validate a signature in one cohort
#'
#' Observed AUC (DeLong 95% CI) versus two nulls: (A) random genes with random
#' signs - "is there any signal?"; (B) random genes drawn from the pool that
#' passed the same training criteria, with their own training signs - "is this
#' selection better than other training-significant genes?".
#' @param pool_genes Optional genes passing the training criteria (null B).
#' @return list(auc, ci, n_used, n_missing, p_a, p_b, null_a, null_b, scores, per_gene)
#' @noRd
gexp_sig_validate <- function(expr_val, y_val, genes, signs, pool_genes = NULL, n_perm = 1000L, seed = 123L) {
  y_val <- as.integer(y_val)
  if (length(unique(y_val)) < 2L) stop("Validation data has only one outcome class.", call. = FALSE)
  used <- intersect(genes, rownames(expr_val))
  if (length(used) < 3L) stop("Fewer than 3 signature genes are present in the validation data.", call. = FALSE)
  sc <- gexp_sig_score(expr_val, used, signs)
  obs <- .gexp_sig_auc(y_val, sc)
  ci <- .gexp_sig_auc_ci(y_val, sc)
  Z <- t(scale(t(expr_val))); Z[!is.finite(Z)] <- NA_real_
  all_g <- rownames(expr_val); n <- length(used)
  run_null <- function(draw) {
    withr::with_seed(seed, vapply(seq_len(n_perm), function(i) {
      g <- draw(); s <- colMeans(Z[g, , drop = FALSE] * attr(g, "signs"), na.rm = TRUE); .gexp_sig_auc(y_val, s)
    }, numeric(1)))
  }
  null_a <- run_null(function() { g <- sample(all_g, n); attr(g, "signs") <- sample(c(-1, 1), n, TRUE); g })
  pool <- intersect(pool_genes, all_g); pool <- pool[pool %in% names(signs)]
  null_b <- NULL
  if (length(pool) >= 2L * n) {
    null_b <- run_null(function() { g <- sample(pool, n); attr(g, "signs") <- unname(signs[g]); g })
  }
  p_of <- function(nl) if (is.null(nl)) NA_real_ else (1 + sum(nl >= obs, na.rm = TRUE)) / (sum(!is.na(nl)) + 1)
  # per-gene replication table
  gz <- gexp_sig_gene_z(expr_val[used, , drop = FALSE], y_val)
  pv <- 2 * stats::pnorm(-abs(gz$z))
  tr_dir <- signs[used]
  val_dir <- sign(gz$z)
  per_gene <- data.frame(
    Gene = used, Training_Direction = ifelse(tr_dir > 0, "Up in Disease", "Down in Disease"),
    Validation_Direction = ifelse(val_dir > 0, "Up in Disease", ifelse(val_dir < 0, "Down in Disease", NA)),
    Validation_Z = as.numeric(gz$z), Validation_P = as.numeric(pv), Validation_FDR = stats::p.adjust(pv, "BH"),
    Same_Direction = val_dir == tr_dir, stringsAsFactors = FALSE)
  per_gene$Replicated <- per_gene$Same_Direction %in% TRUE & per_gene$Validation_FDR < 0.05
  list(auc = obs, ci = ci, n_used = n, n_missing = length(setdiff(genes, rownames(expr_val))),
       p_a = p_of(null_a), p_b = p_of(null_b), null_a = null_a, null_b = null_b,
       scores = sc, per_gene = per_gene, n_perm = n_perm)
}

#' Leave-one-dataset-out validation across the training datasets
#'
#' For each dataset with both groups: select genes on the OTHER datasets only
#' (same rule as the main signature), score the held-out dataset, compare with
#' random gene sets of the same size.
#' @return data.frame, one row per held-out dataset
#' @noRd
gexp_sig_lodo <- function(expr, y, dataset, top_n = 100L, fdr = 0.05, n_perm = 500L, seed = 123L) {
  dataset <- as.character(dataset)
  ds <- unique(dataset)
  rows <- lapply(ds, function(h) {
    ix <- which(dataset == h); yh <- y[ix]
    base <- data.frame(Held_Out = h, N = length(ix), N_Disease = sum(yh == 1L), N_Normal = sum(yh == 0L),
                       Genes_Selected = NA_integer_, AUC = NA_real_, AUC_Lower = NA_real_, AUC_Upper = NA_real_,
                       Null_Mean = NA_real_, Null_95th = NA_real_, P_Permutation = NA_real_, Note = "", stringsAsFactors = FALSE)
    if (sum(yh == 1L) < 2L || sum(yh == 0L) < 2L) { base$Note <- "Skipped: needs >= 2 samples in each group"; return(base) }
    others <- setdiff(ds, h); oi <- which(dataset %in% others)
    meta <- tryCatch(gexp_sig_meta(expr[, oi, drop = FALSE], y[oi], dataset[oi]), error = function(e) NULL)
    if (is.null(meta)) { base$Note <- "Skipped: no other dataset has both groups"; return(base) }
    meta <- meta[meta$Gene %in% rownames(expr)[rowSums(!is.na(expr[, ix, drop = FALSE])) > 1L], , drop = FALSE]
    sel <- gexp_sig_select(meta, top_n, fdr, min_datasets = min(2L, length(unique(dataset[oi]))))
    if (length(sel) < 3L) { base$Note <- "Skipped: fewer than 3 genes pass the selection on the other datasets"; return(base) }
    signs <- stats::setNames(sign(meta$Z), meta$Gene)
    res <- tryCatch(gexp_sig_validate(expr[, ix, drop = FALSE], yh, sel, signs, pool_genes = NULL, n_perm = n_perm, seed = seed), error = function(e) NULL)
    if (is.null(res)) { base$Note <- "Skipped: validation failed"; return(base) }
    base$Genes_Selected <- res$n_used; base$AUC <- res$auc; base$AUC_Lower <- res$ci[2L]; base$AUC_Upper <- res$ci[3L]
    base$Null_Mean <- mean(res$null_a, na.rm = TRUE); base$Null_95th <- unname(stats::quantile(res$null_a, 0.95, na.rm = TRUE))
    base$P_Permutation <- res$p_a
    if (min(sum(yh == 1L), sum(yh == 0L)) < 5L) base$Note <- "Small group: interval and p-value are unstable"
    base
  })
  do.call(rbind, rows)
}

# ------------------------------------------------------------------------------
# Figures (base graphics)
# ------------------------------------------------------------------------------

#' Observed signature AUC against the random-gene-set null distributions
#' @noRd
gexp_sig_plot_null <- function(res, title = "Signature validation") {
  op <- graphics::par(mar = c(5, 5.2, 9.0, 1.5), cex.axis = 1.15, cex.lab = 1.3, cex.main = 1.4, las = 1)
  on.exit(graphics::par(op), add = TRUE)
  nulls <- list(res$null_a, res$null_b); nulls <- nulls[!vapply(nulls, is.null, logical(1))]
  all_v <- c(unlist(nulls), res$auc)
  xr <- range(c(0.2, 1, all_v), na.rm = TRUE)
  brk <- seq(floor(xr[1] * 40) / 40, ceiling(xr[2] * 40) / 40, by = 0.025)
  cols <- c("#7f8c8d", "#3498db")
  h <- lapply(nulls, function(v) graphics::hist(v, breaks = brk, plot = FALSE))
  graphics::plot(NA, xlim = range(brk), ylim = c(0, max(vapply(h, function(z) max(z$density), numeric(1))) * 1.08),
                 xlab = "Signature AUC in the validation cohort", ylab = "Density", main = "", font.main = 2)
  for (i in seq_along(h)) graphics::rect(h[[i]]$breaks[-length(h[[i]]$breaks)], 0, h[[i]]$breaks[-1], h[[i]]$density,
                                          col = grDevices::adjustcolor(cols[i], 0.45), border = "white")
  graphics::abline(v = res$auc, col = "#c0392b", lwd = 3.5)
  graphics::abline(v = 0.5, lty = 2, col = "grey40")
  graphics::mtext(title, side = 3, line = 7.2, font = 2, cex = 1.35)
  leg <- c(sprintf("Random genes, random signs (p = %s)", format.pval(res$p_a, digits = 2, eps = 1e-3)),
           if (!is.null(res$null_b)) sprintf("Random training-significant genes (p = %s)", format.pval(res$p_b, digits = 2, eps = 1e-3)),
           sprintf("Observed AUC %.3f (95%% CI %.3f-%.3f)", res$auc, res$ci[2L], res$ci[3L]))
  usr <- graphics::par("usr")
  graphics::legend(x = mean(usr[c(1L, 2L)]), y = usr[4] + 0.03 * (usr[4] - usr[3]), xjust = 0.5, yjust = 0, xpd = NA,
                   legend = leg, fill = c(grDevices::adjustcolor(cols[seq_along(h)], 0.45), NA),
                   border = c(rep("white", length(h)), NA), lty = c(rep(NA, length(h)), 1), lwd = c(rep(NA, length(h)), 3.5),
                   col = c(rep(NA, length(h)), "#c0392b"), bty = "n", cex = 1.0)
  invisible(NULL)
}

#' Leave-one-dataset-out AUCs with confidence intervals
#' @noRd
gexp_sig_plot_lodo <- function(lodo, title = "Leave-one-dataset-out validation") {
  d <- lodo[is.finite(lodo$AUC), , drop = FALSE]
  op <- graphics::par(mar = c(5.6, 10, 3.8, 6.5), cex.axis = 1.1, cex.lab = 1.3, cex.main = 1.4, las = 1)
  on.exit(graphics::par(op), add = TRUE)
  if (nrow(d) == 0L) { graphics::plot.new(); graphics::text(0.5, 0.5, "No dataset could be held out"); return(invisible(NULL)) }
  y <- rev(seq_len(nrow(d)))
  graphics::plot(NA, xlim = c(0.3, 1.02), ylim = c(0.5, nrow(d) + 0.5), yaxt = "n", xlab = "Signature AUC in the held-out dataset",
                 ylab = "", main = title, font.main = 2)
  graphics::abline(v = 0.5, lty = 2, col = "grey40")
  graphics::segments(d$AUC_Lower, y, d$AUC_Upper, y, lwd = 2.5, col = "grey35")
  sig <- !is.na(d$P_Permutation) & d$P_Permutation < 0.05
  graphics::points(d$AUC, y, pch = 21, bg = ifelse(sig, "#27ae60", "#bdc3c7"), col = "white", cex = 2.4)
  graphics::axis(2, at = y, labels = sprintf("%s (n=%d)", d$Held_Out, d$N), tick = FALSE)
  usr <- graphics::par("usr")
  graphics::text(usr[2] + 0.02 * (usr[2] - usr[1]), y, pos = 4, xpd = NA, cex = 0.95, col = "grey20",
                 labels = ifelse(is.na(d$P_Permutation), "", paste0("p = ", format.pval(d$P_Permutation, digits = 2, eps = 1e-3))))
  graphics::mtext("Green = permutation p < 0.05 against random gene sets", side = 1, line = 4.4, cex = 0.85, col = "grey30")
  invisible(NULL)
}

#' AUC of the signature for several signature sizes (shown openly; fix N in advance)
#' @param meta Output of gexp_sig_meta() on the training data.
#' @return data.frame(N_Requested, N_Used, AUC, AUC_Lower, AUC_Upper, Same_Direction_Pct)
#' @noRd
gexp_sig_sweep <- function(expr_val, y_val, meta, ns = c(25L, 50L, 100L, 300L, 1000L, 100000L), min_datasets = 2L) {
  y_val <- as.integer(y_val)
  signs <- stats::setNames(sign(meta$Z), meta$Gene)
  rows <- lapply(ns, function(n) {
    sel <- intersect(gexp_sig_select(meta, n, min_datasets = min_datasets), rownames(expr_val))
    if (length(sel) < 3L) return(NULL)
    sc <- gexp_sig_score(expr_val, sel, signs)
    ci <- .gexp_sig_auc_ci(y_val, sc)
    gz <- gexp_sig_gene_z(expr_val[sel, , drop = FALSE], y_val)
    data.frame(N_Requested = if (n >= 100000L) "all passing" else as.character(n), N_Used = length(sel),
               AUC = .gexp_sig_auc(y_val, sc), AUC_Lower = ci[2L], AUC_Upper = ci[3L],
               Same_Direction_Pct = 100 * mean(sign(gz$z) == signs[sel], na.rm = TRUE), stringsAsFactors = FALSE)
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}
