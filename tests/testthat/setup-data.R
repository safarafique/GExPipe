set.seed(123)

gexpipe_test_expr <- function(n_genes = 200L, n_samples = 12L) {
  n_genes <- as.integer(n_genes)
  n_samples <- as.integer(n_samples)
  expr <- matrix(stats::rnorm(n_genes * n_samples), nrow = n_genes, ncol = n_samples)
  rownames(expr) <- paste0("Gene", seq_len(n_genes))
  colnames(expr) <- paste0("S", seq_len(n_samples))
  expr
}

gexpipe_test_metadata <- function(sample_ids) {
  stopifnot(length(sample_ids) >= 6)
  sample_ids <- as.character(sample_ids)
  n <- length(sample_ids)
  # Avoid confounding Dataset and Condition (helps limma models be estimable).
  n1 <- n %/% 2
  n2 <- n - n1
  dataset <- c(rep("D1", n1), rep("D2", n2))

  cond_for_block <- function(m) {
    # Ensure both classes present in each dataset block.
    n_norm <- max(1L, m %/% 2)
    n_dis <- m - n_norm
    c(rep("Normal", n_norm), rep("Disease", n_dis))
  }
  condition <- c(cond_for_block(n1), cond_for_block(n2))

  out <- data.frame(
    Dataset = dataset,
    Condition = condition,
    row.names = sample_ids,
    stringsAsFactors = FALSE
  )
  # Shuffle within each dataset block to avoid identical ordering patterns.
  idx1 <- which(out$Dataset == "D1")
  idx2 <- which(out$Dataset == "D2")
  if (length(idx1) > 1) out[idx1, ] <- out[sample(idx1), , drop = FALSE]
  if (length(idx2) > 1) out[idx2, ] <- out[sample(idx2), , drop = FALSE]
  out
}

gexpipe_test_expr_small <- gexpipe_test_expr(150L, 10L)
gexpipe_test_meta_small <- gexpipe_test_metadata(colnames(gexpipe_test_expr_small))

