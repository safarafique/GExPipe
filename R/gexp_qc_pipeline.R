## GExPipe QC helpers
##
## Reusable, testable QC logic extracted from Shiny server code.

#' Detect sample outliers from expression matrix
#'
#' Uses two methods on the top variable genes:
#' - PCA Mahalanobis distance (97.5% chi-square cutoff),
#' - Sample connectivity in signed network (power = 6; mean - 2*SD threshold).
#'
#' @param expr Numeric matrix with genes in rows and samples in columns.
#' @param top_n Integer number of top variable genes to use (default 5000).
#' @return List with outlier detection results.
#'
#' @examples
#' expr <- matrix(rnorm(400), nrow = 40, ncol = 10)
#' rownames(expr) <- paste0("Gene", seq_len(nrow(expr)))
#' colnames(expr) <- paste0("S", seq_len(ncol(expr)))
#' out <- gexp_qc_detect_outliers(expr, top_n = 20)
#' length(out$all_outliers)
#' @export
gexp_qc_detect_outliers <- function(expr, top_n = 5000L) {
  if (is.null(expr) || !is.matrix(expr)) {
    stop("expr must be a matrix (genes x samples).")
  }
  if (ncol(expr) < 5) {
    stop("Need at least 5 samples for outlier detection.")
  }

  n_top <- min(as.integer(top_n), nrow(expr))
  gene_vars <- apply(expr, 1, stats::var, na.rm = TRUE)
  top_genes <- names(sort(gene_vars, decreasing = TRUE))[seq_len(n_top)]
  expr_sub <- expr[top_genes, , drop = FALSE]

  good_var <- apply(expr_sub, 1, function(x) {
    v <- stats::var(x, na.rm = TRUE)
    !is.na(v) && v > 0
  })
  expr_sub <- expr_sub[good_var, , drop = FALSE]

  # Replace NAs with row means
  for (i in seq_len(nrow(expr_sub))) {
    na_idx <- is.na(expr_sub[i, ])
    if (any(na_idx)) {
      expr_sub[i, na_idx] <- mean(expr_sub[i, !na_idx], na.rm = TRUE)
    }
  }

  pca <- stats::prcomp(t(expr_sub), scale. = TRUE, center = TRUE)
  n_pc <- min(2, ncol(pca$x))
  scores <- pca$x[, seq_len(n_pc), drop = FALSE]

  center <- colMeans(scores)
  cov_mat <- tryCatch(stats::cov(scores), error = function(e) diag(n_pc))
  if (any(is.na(cov_mat)) || det(cov_mat) < 1e-10) {
    cov_mat <- diag(apply(scores, 2, stats::var, na.rm = TRUE))
  }
  distances <- tryCatch(
    stats::mahalanobis(scores, center, cov_mat),
    error = function(e) apply(scores, 1, function(x) sum((x - center)^2))
  )
  pca_threshold <- stats::qchisq(0.975, df = n_pc)
  pca_outliers <- names(which(distances > pca_threshold))

  cor_mat <- stats::cor(expr_sub, use = "pairwise.complete.obs", method = "pearson")
  cor_mat[is.na(cor_mat)] <- 0
  adj_mat <- ((1 + cor_mat) / 2)^6
  diag(adj_mat) <- 0
  k <- rowSums(adj_mat)
  conn_threshold <- mean(k) - 2 * stats::sd(k)
  conn_outliers <- names(which(k < conn_threshold))

  all_outliers <- union(pca_outliers, conn_outliers)

  list(
    scores = scores,
    distances = distances,
    pca_threshold = pca_threshold,
    pca_outliers = pca_outliers,
    pca_var_explained = summary(pca)$importance[2, seq_len(n_pc)],
    connectivity = k,
    conn_threshold = conn_threshold,
    conn_outliers = conn_outliers,
    all_outliers = all_outliers
  )
}

#' Exclude selected samples from download/QC state lists
#'
#' @param combined_expr_raw Matrix genes x samples.
#' @param micro_expr_list Named list of microarray matrices.
#' @param rna_counts_list Named list of RNA-seq matrices.
#' @param unified_metadata Optional metadata data.frame with SampleID column.
#' @param samples_to_exclude Character vector of sample IDs.
#' @return List containing updated matrices/lists/metadata.
#'
#' @examples
#' expr <- matrix(rnorm(300), nrow = 30, ncol = 10)
#' rownames(expr) <- paste0("Gene", seq_len(nrow(expr)))
#' colnames(expr) <- paste0("S", seq_len(ncol(expr)))
#' meta <- data.frame(SampleID = colnames(expr), stringsAsFactors = FALSE)
#' res <- gexp_qc_exclude_samples(
#'   combined_expr_raw = expr,
#'   micro_expr_list = list(D1 = expr[, 1:5, drop = FALSE]),
#'   rna_counts_list = list(D2 = expr[, 6:10, drop = FALSE]),
#'   unified_metadata = meta,
#'   samples_to_exclude = c("S1", "S10")
#' )
#' ncol(res$combined_expr_raw)
#' @export
gexp_qc_exclude_samples <- function(
  combined_expr_raw,
  micro_expr_list,
  rna_counts_list,
  unified_metadata = NULL,
  samples_to_exclude
) {
  if (is.null(combined_expr_raw) || !is.matrix(combined_expr_raw)) {
    stop("combined_expr_raw must be a matrix.")
  }
  if (is.null(samples_to_exclude) || length(samples_to_exclude) == 0) {
    stop("samples_to_exclude cannot be empty.")
  }

  keep_cols <- setdiff(colnames(combined_expr_raw), samples_to_exclude)
  if (length(keep_cols) < 3) {
    stop("Cannot exclude samples: fewer than 3 samples would remain.")
  }

  combined_expr_raw <- combined_expr_raw[, keep_cols, drop = FALSE]

  for (gse in names(micro_expr_list)) {
    cur <- micro_expr_list[[gse]]
    keep <- setdiff(colnames(cur), samples_to_exclude)
    micro_expr_list[[gse]] <- if (length(keep) > 0) cur[, keep, drop = FALSE] else NULL
  }
  micro_expr_list <- micro_expr_list[!vapply(micro_expr_list, is.null, logical(1))]

  for (gse in names(rna_counts_list)) {
    cur <- rna_counts_list[[gse]]
    keep <- setdiff(colnames(cur), samples_to_exclude)
    rna_counts_list[[gse]] <- if (length(keep) > 0) cur[, keep, drop = FALSE] else NULL
  }
  rna_counts_list <- rna_counts_list[!vapply(rna_counts_list, is.null, logical(1))]

  if (!is.null(unified_metadata) && "SampleID" %in% names(unified_metadata)) {
    unified_metadata <- unified_metadata[!unified_metadata$SampleID %in% samples_to_exclude, , drop = FALSE]
  }

  list(
    combined_expr_raw = combined_expr_raw,
    micro_expr_list = micro_expr_list,
    rna_counts_list = rna_counts_list,
    unified_metadata = unified_metadata,
    remaining_samples = keep_cols
  )
}

#' Build gene-overlap summary table for QC UI
#'
#' @param all_genes_list Named list of gene vectors by dataset.
#' @param common_genes Character vector of common genes.
#' @return Data.frame with Dataset, Total_Genes, Pct_in_Common.
#'
#' @examples
#' all_genes <- list(
#'   D1 = c("A", "B", "C", "D"),
#'   D2 = c("B", "C", "E")
#' )
#' gexp_qc_gene_overlap_summary(all_genes, common_genes = c("B", "C"))
#' @export
gexp_qc_gene_overlap_summary <- function(all_genes_list, common_genes) {
  if (length(all_genes_list) == 0) {
    return(data.frame(
      Dataset = character(0),
      Total_Genes = integer(0),
      Pct_in_Common = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  common_count <- length(common_genes)
  data.frame(
    Dataset = names(all_genes_list),
    Total_Genes = vapply(all_genes_list, length, integer(1)),
    Pct_in_Common = vapply(all_genes_list, function(x) {
      if (length(x) == 0) return(0)
      round(100 * common_count / length(x), 1)
    }, numeric(1)),
    stringsAsFactors = FALSE
  )
}

#' Prepare cleaned gene sets for Venn plotting
#'
#' @param all_genes_list Named list of gene vectors by dataset.
#' @param max_sets Maximum number of sets to keep for Venn plotting.
#' @return List with `ok`, `sets`, and `message`.
#'
#' @examples
#' sets <- list(
#'   D1 = c("A", "B", "C"),
#'   D2 = c("B", "C", "D"),
#'   D3 = c("A", "D")
#' )
#' out <- gexp_qc_prepare_venn_sets(sets)
#' out$ok
#' @export
gexp_qc_prepare_venn_sets <- function(all_genes_list, max_sets = 5L) {
  if (length(all_genes_list) < 2) {
    return(list(ok = FALSE, sets = list(), message = "Need 2+ datasets for Venn diagram"))
  }
  n_use <- min(as.integer(max_sets), length(all_genes_list))
  sets <- all_genes_list[seq_len(n_use)]
  sets <- lapply(sets, function(x) {
    if (is.null(x)) return(character(0))
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(x)]
    unique(x)
  })
  sets <- sets[vapply(sets, length, integer(1)) > 0]
  if (is.null(names(sets)) || any(names(sets) == "")) {
    names(sets) <- paste0("Dataset_", seq_along(sets))
  }
  if (length(sets) < 2) {
    return(list(ok = FALSE, sets = sets, message = "Need 2+ datasets with genes for Venn diagram"))
  }
  list(ok = TRUE, sets = sets, message = "")
}

#' Prepare UpSet matrix data from per-dataset genes
#'
#' @param all_genes_list Named list of gene vectors by dataset.
#' @return List with `ok`, `upset_df`, `gene_lists`, `max_set_size`, and `message`.
#'
#' @examples
#' sets <- list(
#'   D1 = c("A", "B", "C"),
#'   D2 = c("B", "C", "D")
#' )
#' out <- gexp_qc_prepare_upset_data(sets)
#' names(out)
#' @export
gexp_qc_prepare_upset_data <- function(all_genes_list) {
  if (length(all_genes_list) < 2) {
    return(list(ok = FALSE, message = "Need 2+ datasets for UpSet plot"))
  }
  gene_lists <- lapply(all_genes_list, function(x) {
    if (is.null(x)) return(character(0))
    as.character(x)
  })
  gene_lists <- gene_lists[vapply(gene_lists, length, integer(1)) > 0]
  if (length(gene_lists) < 2) {
    return(list(ok = FALSE, message = "Need 2+ datasets with genes for UpSet plot"))
  }

  all_genes <- unique(unlist(gene_lists))
  if (length(all_genes) == 0) {
    return(list(ok = FALSE, message = "No genes found in datasets"))
  }

  upset_matrix <- matrix(0, nrow = length(all_genes), ncol = length(gene_lists))
  rownames(upset_matrix) <- all_genes
  colnames(upset_matrix) <- names(gene_lists)
  for (i in seq_along(gene_lists)) {
    matching_genes <- intersect(all_genes, gene_lists[[i]])
    if (length(matching_genes) > 0) upset_matrix[matching_genes, i] <- 1
  }

  list(
    ok = TRUE,
    upset_df = as.data.frame(upset_matrix),
    gene_lists = gene_lists,
    max_set_size = max(vapply(gene_lists, length, integer(1))),
    message = ""
  )
}

#' Prepare QC boxplot data from combined expression
#'
#' @param combined_expr_raw Matrix genes x samples.
#' @param micro_expr_list Named list of microarray matrices.
#' @param rna_counts_list Named list of RNA-seq matrices.
#' @param max_points Maximum rows to keep for plotting.
#' @return data.frame with Expression, Sample, Platform.
#'
#' @examples
#' expr <- matrix(rnorm(120), nrow = 12, ncol = 10)
#' rownames(expr) <- paste0("Gene", seq_len(nrow(expr)))
#' colnames(expr) <- paste0("S", seq_len(ncol(expr)))
#' df <- gexp_qc_prepare_boxplot_data(
#'   combined_expr_raw = expr,
#'   micro_expr_list = list(D1 = expr[, 1:5, drop = FALSE]),
#'   rna_counts_list = list(D2 = expr[, 6:10, drop = FALSE]),
#'   max_points = 1000
#' )
#' head(df)
#' @export
gexp_qc_prepare_boxplot_data <- function(
  combined_expr_raw,
  micro_expr_list,
  rna_counts_list,
  max_points = 500000L
) {
  micro_n <- if (length(micro_expr_list) > 0) sum(vapply(micro_expr_list, ncol, integer(1))) else 0L
  rna_n <- if (length(rna_counts_list) > 0) sum(vapply(rna_counts_list, ncol, integer(1))) else 0L
  platform_per_sample <- c(rep("Microarray", micro_n), rep("RNAseq", rna_n))
  platform_labels <- rep(platform_per_sample, each = nrow(combined_expr_raw))

  df <- data.frame(
    Expression = as.vector(combined_expr_raw),
    Sample = rep(colnames(combined_expr_raw), each = nrow(combined_expr_raw)),
    Platform = platform_labels[seq_along(as.vector(combined_expr_raw))],
    stringsAsFactors = FALSE
  )
  if (nrow(df) > max_points) {
    df <- df[sample(nrow(df), max_points), , drop = FALSE]
  }
  df
}

#' Prepare density curves for QC density plot
#'
#' @param combined_expr_raw Matrix genes x samples.
#' @param max_samples Maximum number of sample density lines to include.
#' @return List with first density, additional densities, and colors.
#'
#' @examples
#' expr <- matrix(rnorm(200), nrow = 20, ncol = 10)
#' d <- gexp_qc_prepare_density_data(expr, max_samples = 5)
#' length(d$others)
#' @export
gexp_qc_prepare_density_data <- function(combined_expr_raw, max_samples = 50L) {
  n_use <- min(ncol(combined_expr_raw), as.integer(max_samples))
  d1 <- stats::density(combined_expr_raw[, 1], na.rm = TRUE)
  others <- list()
  if (n_use >= 2) {
    for (i in 2:n_use) {
      others[[length(others) + 1L]] <- stats::density(combined_expr_raw[, i], na.rm = TRUE)
    }
  }
  list(
    first = d1,
    others = others,
    colors = grDevices::rainbow(n_use)
  )
}

