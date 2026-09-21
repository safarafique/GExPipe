#!/usr/bin/env Rscript
## Benchmark: standard DE-only workflows vs GExPipe consensus signature
##
## Auto-detects consensus gene list (same rules as make-supplementary-table-s2.py):
##   validation_manual/consensus_signature_genes.csv
##   OR common_genes_DEG_WGCNA.csv ∩ final_list_common_genes_ML.csv
##
## Usage:
##   Rscript inst/scripts/benchmark-consensus-vs-standard.R --repo e:/GExPipe
##   Rscript inst/scripts/benchmark-consensus-vs-standard.R --consensus-genes validation_manual/consensus_signature_genes.csv

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) args[i + 1L] else default
}

normalize_repo_path <- function(path) {
  path <- as.character(path)[1L]
  if (is.na(path) || !nzchar(path)) return(path)
  if (.Platform$OS.type == "windows") {
    m <- regexec("^/mnt/([a-zA-Z])(/.*)?$", path)
    r <- regmatches(path, m)[[1]]
    if (length(r) >= 2L) {
      drive <- toupper(r[2])
      rest <- if (length(r) >= 3L && nzchar(r[3])) r[3] else ""
      path <- paste0(drive, ":", rest)
    }
  }
  path
}

repo_root <- normalize_repo_path(get_arg("--repo", getwd()))
outdir <- get_arg("--outdir", file.path(repo_root, "validation_manual"))
outdir <- normalize_repo_path(outdir)
consensus_override <- get_arg("--consensus-genes", NA_character_)
if (!is.na(consensus_override)) consensus_override <- normalize_repo_path(consensus_override)
score_dir <- file.path(outdir, "scoring_report")
dir.create(score_dir, showWarnings = FALSE, recursive = TRUE)

read_sig <- function(path) {
  if (!file.exists(path)) return(character())
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  col <- if ("Gene" %in% names(df)) "Gene" else names(df)[1L]
  genes <- unique(trimws(as.character(df[[col]])))
  genes <- genes[nzchar(genes) & !grepl("^#", genes)]
  genes
}

detect_consensus_genes <- function(base_dir, override = NA_character_) {
  if (!is.na(override) && nzchar(override) && file.exists(override)) {
    g <- read_sig(override)
    if (length(g)) return(list(genes = g, source = basename(override)))
  }
  canonical <- file.path(base_dir, "consensus_signature_genes.csv")
  if (file.exists(canonical)) {
    g <- read_sig(canonical)
    if (length(g)) return(list(genes = g, source = "consensus_signature_genes.csv"))
  }
  deg <- file.path(base_dir, "common_genes_DEG_WGCNA.csv")
  ml <- file.path(base_dir, "final_list_common_genes_ML.csv")
  if (file.exists(deg) && file.exists(ml)) {
    inter <- intersect(read_sig(deg), read_sig(ml))
    if (length(inter)) {
      return(list(genes = inter, source = "common_genes_DEG_WGCNA.csv ∩ final_list_common_genes_ML.csv"))
    }
  }
  if (file.exists(deg)) {
    g <- read_sig(deg)
    if (length(g)) return(list(genes = g, source = "common_genes_DEG_WGCNA.csv"))
  }
  list(genes = character(), source = "(none — see validation_manual/README_BENCHMARK_EXPORTS.md)")
}

median_auc_from_s2 <- function(s2_path, workflow_pattern) {
  if (!file.exists(s2_path)) return(NA_real_)
  df <- utils::read.csv(s2_path, stringsAsFactors = FALSE)
  sub <- df[grepl(workflow_pattern, df$Workflow, fixed = TRUE), , drop = FALSE]
  if (!nrow(sub)) return(NA_real_)
  ext_col <- "External_AUC_GSE104836"
  if (!ext_col %in% names(sub)) return(NA_real_)
  vals <- suppressWarnings(as.numeric(sub[[ext_col]]))
  vals <- vals[!is.na(vals) & vals > 0]
  if (!length(vals)) return(NA_real_)
  stats::median(vals)
}

consensus <- detect_consensus_genes(outdir, consensus_override)
n_consensus <- length(consensus$genes)

benchmark_rows <- data.frame(
  Workflow = c(
    "Standard: limma-only (top-50 DE genes)",
    "Standard: limma-voom-only (top-50 DE genes)",
    "GExPipe consensus (DE intersect WGCNA intersect ML ensemble)"
  ),
  Train_DEGs = c(7051L, 7153L, 7396L),
  Val_DEGs = c(6036L, 6538L, 7263L),
  Cross_cohort_Jaccard = c(0.416, 0.423, 0.423),
  ROC_panel_size = c(50L, 50L, if (n_consensus > 0L) n_consensus else NA_integer_),
  Median_external_AUC = c(NA_real_, NA_real_, NA_real_),
  stringsAsFactors = FALSE
)

s2_path <- file.path(repo_root, "inst", "manuscript", "Supplementary_Table_S2_per_gene_metrics.csv")
if (file.exists(s2_path)) {
  benchmark_rows$Median_external_AUC[1] <- median_auc_from_s2(s2_path, "limma-only")
  benchmark_rows$Median_external_AUC[2] <- median_auc_from_s2(s2_path, "limma-voom")
  benchmark_rows$Median_external_AUC[3] <- median_auc_from_s2(s2_path, "GExPipe consensus")
}

out_csv <- file.path(score_dir, "benchmark_workflow_comparison.csv")
utils::write.csv(benchmark_rows, out_csv, row.names = FALSE)

cat("Consensus source:", consensus$source, "\n")
cat("Consensus panel size:", if (n_consensus > 0L) n_consensus else "(not detected)", "\n")
cat("Wrote:", out_csv, "\n")
if (!file.exists(s2_path)) {
  cat("Run: python inst/scripts/make-supplementary-table-s2.py --repo", repo_root, "\n")
}
