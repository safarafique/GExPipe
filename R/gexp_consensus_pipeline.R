## Separate-platform DE consensus (RNA-seq ∩ microarray, same direction)

#' Default Parallel Step 7 rule
#'
#' Auto keeps genes significant on both platforms in the same direction.
#' That list is for Step 9 (DEG ∩ modules), not for WGCNA input.
#' @noRd
gexpipe_parallel_consensus_defaults <- function() {
  list(same_direction = TRUE)
}

#' Gene IDs from a DE or significant-gene table
#'
#' @param de_df data.frame with a `Gene` / `gene` column, or first column.
#' @return Character vector of unique trimmed gene symbols.
#' @noRd
.gexpipe_de_gene_ids <- function(de_df) {
  if (is.null(de_df) || !is.data.frame(de_df) || nrow(de_df) < 1L) {
    return(character(0))
  }
  col <- if ("Gene" %in% names(de_df)) {
    "Gene"
  } else if ("gene" %in% names(de_df)) {
    "gene"
  } else {
    names(de_df)[1L]
  }
  unique(trimws(as.character(de_df[[col]])))
}

#' Up / Down direction named by gene
#'
#' @param de_df data.frame with Gene and Significance or logFC.
#' @return Named character vector (`Up` / `Down`).
#' @noRd
.gexpipe_de_direction <- function(de_df) {
  genes <- .gexpipe_de_gene_ids(de_df)
  if (length(genes) < 1L) {
    return(stats::setNames(character(0), character(0)))
  }
  idx <- match(genes, if ("Gene" %in% names(de_df)) {
    as.character(de_df$Gene)
  } else if ("gene" %in% names(de_df)) {
    as.character(de_df$gene)
  } else {
    as.character(de_df[[1L]])
  })
  dir <- rep(NA_character_, length(genes))
  if ("Significance" %in% names(de_df)) {
    sig <- as.character(de_df$Significance[idx])
    dir[sig == "Up-regulated"] <- "Up"
    dir[sig == "Down-regulated"] <- "Down"
  }
  if ("logFC" %in% names(de_df)) {
    lfc <- suppressWarnings(as.numeric(de_df$logFC[idx]))
    miss <- is.na(dir) & !is.na(lfc) & lfc != 0
    dir[miss] <- ifelse(lfc[miss] > 0, "Up", "Down")
  }
  stats::setNames(dir, genes)
}

#' Consensus DEGs from separate RNA-seq and microarray DE
#'
#' Keeps genes significant on **both** platforms. When
#' `require_same_direction` is TRUE (default), the logFC sign must match.
#'
#' Downstream tables use mean logFC and the more conservative (larger)
#' adjusted p-value so WGCNA / common-gene steps can reuse `sig_genes`.
#'
#' @param rna_sig Significant-gene table from RNA-seq DE (`Gene`, `logFC`,
#'   `adj.P.Val`, optional `Significance`).
#' @param micro_sig Significant-gene table from microarray DE (same columns).
#' @param require_same_direction logical; if TRUE, drop genes with opposite
#'   fold-change signs (only affects genes significant on both platforms;
#'   has no effect on a platform-exclusive gene under `combine = "union"`,
#'   since there's no second direction to conflict with).
#' @param combine `"intersection"` (default; a gene must be significant on
#'   BOTH platforms - the scientifically stronger, cross-platform-validated
#'   result) or `"union"` (a gene significant on EITHER platform passes,
#'   with no cross-platform confirmation - more exploratory, more false
#'   positives; use for hypothesis generation, not a final panel).
#' @return list with `table`, `genes`, `n_rna`, `n_micro`, `n_overlap`,
#'   `n_consensus`, `n_discordant`, `rna_only`, `micro_only`.
#' @examples
#' rna <- data.frame(
#'   Gene = c("A", "B", "C"),
#'   logFC = c(1.2, -0.8, 1.1),
#'   adj.P.Val = c(0.01, 0.02, 0.03),
#'   Significance = c("Up-regulated", "Down-regulated", "Up-regulated"),
#'   stringsAsFactors = FALSE
#' )
#' micro <- data.frame(
#'   Gene = c("A", "B", "D"),
#'   logFC = c(0.9, -1.0, 1.4),
#'   adj.P.Val = c(0.02, 0.01, 0.04),
#'   Significance = c("Up-regulated", "Down-regulated", "Up-regulated"),
#'   stringsAsFactors = FALSE
#' )
#' out <- gexpipe_consensus_degs(rna, micro)
#' out$genes
#' @export
gexpipe_consensus_degs <- function(rna_sig, micro_sig, require_same_direction = TRUE,
                                    combine = c("intersection", "union")) {
  combine <- match.arg(combine)
  rna_genes <- .gexpipe_de_gene_ids(rna_sig)
  micro_genes <- .gexpipe_de_gene_ids(micro_sig)
  overlap <- intersect(rna_genes, micro_genes)
  rna_dir <- .gexpipe_de_direction(rna_sig)
  micro_dir <- .gexpipe_de_direction(micro_sig)

  # The same-direction filter only ever applies to genes found on BOTH
  # platforms - a platform-exclusive gene (union mode only) has no second
  # direction to conflict with, so it is never dropped by this filter.
  overlap_kept <- overlap
  n_discordant <- 0L
  if (isTRUE(require_same_direction) && length(overlap) > 0L) {
    d1 <- unname(rna_dir[overlap])
    d2 <- unname(micro_dir[overlap])
    keep <- !is.na(d1) & !is.na(d2) & d1 == d2
    n_discordant <- sum(!keep, na.rm = TRUE)
    overlap_kept <- overlap[keep]
  }
  same <- if (identical(combine, "union")) {
    platform_exclusive <- setdiff(union(rna_genes, micro_genes), overlap)
    union(overlap_kept, platform_exclusive)
  } else {
    overlap_kept
  }

  pick_row <- function(df, gene) {
    if (is.null(df) || nrow(df) < 1L) {
      return(NULL)
    }
    gcol <- if ("Gene" %in% names(df)) "Gene" else if ("gene" %in% names(df)) "gene" else names(df)[1L]
    df[match(gene, as.character(df[[gcol]])), , drop = FALSE]
  }

  if (length(same) < 1L) {
    empty <- data.frame(
      Gene = character(0),
      logFC_rna = numeric(0),
      logFC_micro = numeric(0),
      logFC = numeric(0),
      adj.P.Val_rna = numeric(0),
      adj.P.Val_micro = numeric(0),
      adj.P.Val = numeric(0),
      Direction = character(0),
      Significance = character(0),
      stringsAsFactors = FALSE
    )
    return(list(
      table = empty,
      genes = character(0),
      n_rna = length(rna_genes),
      n_micro = length(micro_genes),
      n_overlap = length(overlap),
      n_consensus = 0L,
      n_discordant = as.integer(n_discordant),
      rna_only = setdiff(rna_genes, micro_genes),
      micro_only = setdiff(micro_genes, rna_genes)
    ))
  }

  rna_rows <- pick_row(rna_sig, same)
  micro_rows <- pick_row(micro_sig, same)
  lfc_rna <- suppressWarnings(as.numeric(rna_rows$logFC))
  lfc_micro <- suppressWarnings(as.numeric(micro_rows$logFC))
  padj_rna <- suppressWarnings(as.numeric(rna_rows$adj.P.Val))
  padj_micro <- suppressWarnings(as.numeric(micro_rows$adj.P.Val))
  # A platform-exclusive gene (union mode only) has no RNA-seq direction
  # to read - fall back to the microarray direction for those.
  direction <- unname(rna_dir[same])
  direction[is.na(direction)] <- unname(micro_dir[same])[is.na(direction)]
  significance <- ifelse(
    is.na(direction),
    "Not Significant",
    ifelse(direction == "Up", "Up-regulated", "Down-regulated")
  )

  table <- data.frame(
    Gene = same,
    logFC_rna = lfc_rna,
    logFC_micro = lfc_micro,
    logFC = rowMeans(cbind(lfc_rna, lfc_micro), na.rm = TRUE),
    adj.P.Val_rna = padj_rna,
    adj.P.Val_micro = padj_micro,
    adj.P.Val = pmax(padj_rna, padj_micro, na.rm = TRUE),
    Direction = direction,
    Significance = significance,
    stringsAsFactors = FALSE
  )
  rownames(table) <- table$Gene

  list(
    table = table,
    genes = same,
    n_rna = length(rna_genes),
    n_micro = length(micro_genes),
    n_overlap = length(overlap),
    n_consensus = length(same),
    n_discordant = as.integer(n_discordant),
    rna_only = setdiff(rna_genes, micro_genes),
    micro_only = setdiff(micro_genes, rna_genes)
  )
}

#' Per-gene logFC of the genes found on BOTH platforms (input to the concordance scatter)
#' @param rna_sig,micro_sig Step 6 significant-gene tables (Gene, logFC, adj.P.Val).
#' @return data.frame(Gene, logFC_rna, logFC_micro, Class) or NULL when there is no overlap.
#'   Class is "Concordant (up)", "Concordant (down)" or "Discordant".
#' @noRd
gexp_consensus_concordance_data <- function(rna_sig, micro_sig) {
  ov <- intersect(.gexpipe_de_gene_ids(rna_sig), .gexpipe_de_gene_ids(micro_sig))
  if (length(ov) < 1L) return(NULL)
  pick <- function(df) {
    gcol <- if ("Gene" %in% names(df)) "Gene" else if ("gene" %in% names(df)) "gene" else names(df)[1L]
    suppressWarnings(as.numeric(df$logFC[match(ov, trimws(as.character(df[[gcol]])))]))
  }
  d <- data.frame(Gene = ov, logFC_rna = pick(rna_sig), logFC_micro = pick(micro_sig), stringsAsFactors = FALSE)
  d <- d[is.finite(d$logFC_rna) & is.finite(d$logFC_micro), , drop = FALSE]
  if (nrow(d) < 1L) return(NULL)
  d$Class <- ifelse(sign(d$logFC_rna) != sign(d$logFC_micro), "Discordant",
                    ifelse(d$logFC_rna > 0, "Concordant (up)", "Concordant (down)"))
  d
}

#' Scatter of RNA-seq vs microarray logFC for the common DEGs
#' @param d Output of gexp_consensus_concordance_data().
#' @noRd
gexp_consensus_concordance_plot <- function(d, n_label = 10L) {
  if (is.null(d) || nrow(d) < 1L) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::annotate("text", x = 0, y = 0, label = "No genes are significant on both platforms.", colour = "grey40", size = 5))
  }
  n <- nrow(d)
  pct <- 100 * mean(d$Class != "Discordant")
  r <- if (n >= 3L) suppressWarnings(stats::cor(d$logFC_rna, d$logFC_micro, method = "spearman")) else NA_real_
  r_p <- if (n >= 3L) suppressWarnings(stats::cor.test(d$logFC_rna, d$logFC_micro, method = "spearman")$p.value) else NA_real_
  lim <- max(abs(c(d$logFC_rna, d$logFC_micro))) * 1.1
  d$rank_ <- pmin(abs(d$logFC_rna), abs(d$logFC_micro))
  lab <- d[order(-d$rank_), , drop = FALSE]
  lab <- lab[seq_len(min(n_label, nrow(lab))), , drop = FALSE]
  cols <- c("Concordant (up)" = "#E53935", "Concordant (down)" = "#1E88E5", "Discordant" = "#9E9E9E")
  ggplot2::ggplot(d, ggplot2::aes(x = .data$logFC_rna, y = .data$logFC_micro, colour = .data$Class)) +
    ggplot2::annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf, fill = "#E53935", alpha = 0.04) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0, fill = "#1E88E5", alpha = 0.04) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60") + ggplot2::geom_vline(xintercept = 0, colour = "grey60") +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_point(size = 2.4, alpha = 0.8) +
    ggrepel::geom_text_repel(data = lab, ggplot2::aes(label = .data$Gene), size = 3, show.legend = FALSE, max.overlaps = 30, box.padding = 0.4) +
    ggplot2::scale_colour_manual(values = cols, drop = FALSE) +
    ggplot2::coord_equal(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
    ggplot2::labs(
      title = "Directional concordance of common DEGs",
      subtitle = sprintf("%d genes significant on both platforms | %.0f%% same direction%s", n, pct,
                         if (is.finite(r)) sprintf("
Spearman rho = %.2f (P = %s)", r, format.pval(r_p, digits = 2)) else ""),
      x = "log2 fold change, RNA-seq (Disease vs Normal)", y = "log2 fold change, microarray (Disease vs Normal)", colour = NULL,
      caption = "Dashed line: y = x. Upper-right and lower-left quadrants agree in direction across platforms."
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), legend.position = "bottom")
}

#' Expression matrix + annotation for the consensus-DEG heatmap
#'
#' Genes are the top consensus DEGs (ranked by the smaller |logFC| of the two platforms, so a gene
#' must be strong on BOTH). Each platform is z-scored per gene separately before the samples are
#' placed side by side, so platform scale differences do not drive the clustering.
#' @return list(mat, annot, genes) or NULL.
#' @noRd
gexp_consensus_heatmap_data <- function(consensus_table, expr_rna, expr_micro, metadata, top_n = 30L) {
  if (is.null(consensus_table) || nrow(consensus_table) < 1L) return(NULL)
  ct <- consensus_table
  ct$rank_ <- if (all(c("logFC_rna", "logFC_micro") %in% names(ct))) pmin(abs(ct$logFC_rna), abs(ct$logFC_micro)) else abs(ct$logFC)
  ct <- ct[order(-ct$rank_, na.last = TRUE), , drop = FALSE]
  mats <- Filter(Negate(is.null), list(RNA_seq = expr_rna, Microarray = expr_micro))
  if (length(mats) == 0L) return(NULL)
  genes <- Reduce(intersect, c(list(as.character(ct$Gene)), lapply(mats, rownames)))
  genes <- genes[seq_len(min(as.integer(top_n), length(genes)))]
  if (length(genes) < 2L) return(NULL)
  zs <- lapply(mats, function(m) {
    z <- t(scale(t(as.matrix(m[genes, , drop = FALSE]))))
    z[!is.finite(z)] <- 0
    pmax(pmin(z, 3), -3)
  })
  mat <- do.call(cbind, zs)
  plat <- rep(names(mats), vapply(zs, ncol, integer(1)))
  idx <- match(colnames(mat), if ("SampleID" %in% names(metadata)) as.character(metadata$SampleID) else rownames(metadata))
  annot <- data.frame(
    Condition = if ("Condition" %in% names(metadata)) as.character(metadata$Condition[idx]) else NA_character_,
    Platform = plat, row.names = colnames(mat), stringsAsFactors = FALSE)
  annot <- annot[, vapply(annot, function(x) !all(is.na(x)), logical(1)), drop = FALSE]
  list(mat = mat, annot = annot, genes = genes)
}

#' Draw the hierarchical-clustering heatmap of top consensus DEGs (pheatmap)
#' @noRd
gexp_consensus_heatmap_draw <- function(hd) {
  if (is.null(hd)) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "Needs at least 2 consensus DEGs present in the normalized\nexpression data (apply Step 6 and Step 7 first).", cex = 1.1, col = "gray40")
    return(invisible(NULL))
  }
  colors <- list(Platform = c(RNA_seq = "#8E24AA", Microarray = "#FB8C00"))
  if ("Condition" %in% names(hd$annot)) colors$Condition <- c(Normal = "#43A047", Disease = "#E53935")
  pheatmap::pheatmap(
    hd$mat, annotation_col = hd$annot, annotation_colors = colors,
    color = grDevices::colorRampPalette(c("#1E88E5", "white", "#E53935"))(100), breaks = seq(-3, 3, length.out = 101),
    clustering_distance_rows = "correlation", clustering_distance_cols = "euclidean", clustering_method = "ward.D2",
    show_colnames = FALSE, border_color = NA, fontsize_row = max(6, 11 - length(hd$genes) / 8),
    main = sprintf("Top %d consensus DEGs (hierarchical clustering; z-score within platform)", length(hd$genes)))
  invisible(NULL)
}
