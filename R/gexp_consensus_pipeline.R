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
#'   fold-change signs.
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
gexpipe_consensus_degs <- function(rna_sig, micro_sig, require_same_direction = TRUE) {
  rna_genes <- .gexpipe_de_gene_ids(rna_sig)
  micro_genes <- .gexpipe_de_gene_ids(micro_sig)
  overlap <- intersect(rna_genes, micro_genes)
  rna_dir <- .gexpipe_de_direction(rna_sig)
  micro_dir <- .gexpipe_de_direction(micro_sig)

  same <- overlap
  n_discordant <- 0L
  if (isTRUE(require_same_direction) && length(overlap) > 0L) {
    d1 <- unname(rna_dir[overlap])
    d2 <- unname(micro_dir[overlap])
    keep <- !is.na(d1) & !is.na(d2) & d1 == d2
    n_discordant <- sum(!keep, na.rm = TRUE)
    same <- overlap[keep]
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
  direction <- unname(rna_dir[same])
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
