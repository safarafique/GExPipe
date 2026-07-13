##' BIOS-Rank — limma-style ranking of disease-linked genes
##'
##' Use like limma: fit DE once, then rank genes with BIOS for a compact
##' disease-linked panel. Works on **any** expression matrix with a binary
##' Condition (Disease vs Normal). Not limited to specific GSE accessions.
##'
##' @examples
##' ## Minimal (any dataset):
##' ##   fit  <- eBayes(lmFit(expr, model.matrix(~ Condition)))
##' ##   bios <- biosRank(fit)
##' ##   topBIOS(bios, n = 20)
##'
##' ## With platform (merged microarray + RNA-seq):
##' ##   fit  <- eBayes(lmFit(expr, model.matrix(~ Condition + Platform)))
##' ##   bios <- biosRank(fit, coef_condition = 2, coef_platform = 3)
##' ##   topBIOS(bios, n = 20)

## ---------------------------------------------------------------------------
## Helpers
## ---------------------------------------------------------------------------

bios_scale01 <- function(x) {
  x <- as.numeric(x)
  r <- range(x, na.rm = TRUE)
  if (!all(is.finite(r)) || diff(r) < .Machine$double.eps) {
    return(rep(0, length(x)))
  }
  (x - r[1]) / (r[2] - r[1])
}

## ---------------------------------------------------------------------------
## Main API (limma-like)
## ---------------------------------------------------------------------------

##' Rank genes with BIOS after a limma fit
##'
##' @param fit limma MArrayLM object from \code{eBayes(lmFit(...))}
##' @param coef_condition integer; coefficient for Disease vs Normal (default 2)
##' @param coef_platform integer or NULL; Platform coefficient for Ep.
##'   NULL => single-platform mode (Ep = 1 for all genes).
##' @param module_genes optional character vector of trait-module genes (Em = 1).
##'   If NULL, a DE-based proxy is used (not true WGCNA).
##' @param stable_genes optional character vector of multi-selector / consensus
##'   genes (Es = 1). If NULL, a DE-based proxy is used.
##' @param weights named numeric vector for channels
##'   \code{c(Ec=, Ep=, Em=, Es=)}. Default equal 1/4.
##' @return A data.frame of class \code{BIOSRank}, one row per gene, sorted by
##'   BIOS score (descending). Use \code{topBIOS()} to slice top-n.
##' @export
biosRank <- function(fit,
                     coef_condition = 2L,
                     coef_platform = NULL,
                     module_genes = NULL,
                     stable_genes = NULL,
                     weights = c(Ec = 0.25, Ep = 0.25, Em = 0.25, Es = 0.25)) {
  if (!requireNamespace("limma", quietly = TRUE)) {
    stop("Package 'limma' is required. install with BiocManager::install('limma')")
  }
  if (!inherits(fit, "MArrayLM")) {
    stop("'fit' must be a limma MArrayLM object (from eBayes(lmFit(...))).")
  }

  tt_c <- limma::topTable(fit, coef = coef_condition, number = Inf, sort.by = "none")
  genes <- rownames(tt_c)
  if (is.null(genes)) stop("fit has no gene rownames.")

  beta_c <- abs(tt_c$logFC)
  padj <- tt_c$adj.P.Val
  Ec_raw <- (-log10(pmax(padj, 1e-300))) * beta_c
  Ec <- bios_scale01(Ec_raw)

  if (is.null(coef_platform)) {
    Ep <- rep(1, length(genes))
  } else {
    tt_p <- limma::topTable(fit, coef = coef_platform, number = Inf, sort.by = "none")
    beta_p <- abs(tt_p$logFC[match(genes, rownames(tt_p))])
    beta_p[!is.finite(beta_p)] <- 0
    Ep_raw <- beta_c / (beta_c + beta_p + 1e-6)
    Ep <- bios_scale01(Ep_raw)
  }

  ## Em: trait module membership (user list) or DE proxy
  if (!is.null(module_genes)) {
    Em <- as.numeric(genes %in% as.character(module_genes))
  } else {
    sig <- is.finite(padj) & padj < 0.05
    thr <- if (any(sig)) stats::quantile(beta_c[sig], 0.8, na.rm = TRUE) else Inf
    Em <- as.numeric(sig & beta_c >= thr)
  }

  ## Es: multi-selector stability (user list) or DE proxy
  if (!is.null(stable_genes)) {
    Es <- ifelse(genes %in% as.character(stable_genes), 1,
                 ifelse(genes %in% as.character(module_genes), 1 / 3, 0))
  } else {
    strong <- is.finite(padj) & padj < 0.01 & beta_c > 1
    sig <- is.finite(padj) & padj < 0.05
    Es <- ifelse(strong, 1, ifelse(sig, 1 / 3, 0))
  }

  w <- weights / sum(weights)
  for (nm in c("Ec", "Ep", "Em", "Es")) {
    if (!nm %in% names(w)) stop("weights must include ", nm)
  }

  BIOS <- w["Ec"] * Ec + w["Ep"] * Ep + w["Em"] * Em + w["Es"] * Es

  plat_logFC <- if (is.null(coef_platform)) {
    rep(NA_real_, length(genes))
  } else {
    limma::topTable(fit, coef = coef_platform, number = Inf, sort.by = "none")$logFC[
      match(genes, rownames(limma::topTable(fit, coef = coef_platform, number = Inf, sort.by = "none")))
    ]
  }

  out <- data.frame(
    Gene = genes,
    logFC_Condition = tt_c$logFC,
    adjP_Condition = padj,
    logFC_Platform = plat_logFC,
    Ec = as.numeric(Ec),
    Ep = as.numeric(Ep),
    Em = as.numeric(Em),
    Es = as.numeric(Es),
    BIOS_Rank = as.numeric(BIOS),
    stringsAsFactors = FALSE
  )
  out <- out[order(-out$BIOS_Rank, out$adjP_Condition), , drop = FALSE]
  rownames(out) <- NULL
  class(out) <- c("BIOSRank", "data.frame")
  attr(out, "weights") <- w
  attr(out, "coef_condition") <- coef_condition
  attr(out, "coef_platform") <- coef_platform
  out
}

##' Top-n BIOS genes (like limma::topTable)
##'
##' @param bios object from \code{biosRank()}
##' @param n number of genes
##' @param sort.by column to sort (default BIOS_Rank)
##' @return data.frame of top genes
##' @export
topBIOS <- function(bios, n = 20L, sort.by = "BIOS_Rank") {
  if (!inherits(bios, "BIOSRank") && !is.data.frame(bios)) {
    stop("'bios' must be the data.frame returned by biosRank().")
  }
  n <- as.integer(n)
  if (!sort.by %in% names(bios)) stop("Unknown sort.by column: ", sort.by)
  o <- order(-bios[[sort.by]], bios$adjP_Condition)
  head(bios[o, , drop = FALSE], n)
}

##' Print method
##' @export
print.BIOSRank <- function(x, n = 10L, ...) {
  cat("BIOSRank ranking: ", nrow(x), " genes",
      " | weights: ", paste(names(attr(x, "weights")),
                            round(attr(x, "weights"), 3),
                            sep = "=", collapse = ", "),
      "\n", sep = "")
  print(utils::head(as.data.frame(x), n))
  invisible(x)
}
