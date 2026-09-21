#!/usr/bin/env Rscript
## Merge manually timed GUI DEG lists into Table_competitor_efficiency_DE.csv
##
## Place significant-gene CSVs (column Gene, or first column) in:
##   validation_manual/competitor_benchmark/manual_exports/
##     GEOexplorer_DE_sig.csv
##     ShinySeq_DE_sig.csv
##     iGEAK_DE_sig.csv
##
## Optional timing file:
##   manual_competitor_timing_TEMPLATE.csv  (filled)
##
## Usage:
##   Rscript inst/scripts/merge-manual-competitor-degs.R --repo e:/GExPipe

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) args[i + 1L] else default
}

repo <- get_arg("--repo", getwd())
outdir <- get_arg("--outdir", file.path(repo, "validation_manual", "competitor_benchmark"))
man_dir <- file.path(outdir, "manual_exports")
dir.create(man_dir, recursive = TRUE, showWarnings = FALSE)

jaccard <- function(a, b) {
  a <- unique(a); b <- unique(b)
  u <- union(a, b)
  if (!length(u)) return(NA_real_)
  length(intersect(a, b)) / length(u)
}

read_genes <- function(path) {
  if (!file.exists(path)) return(character())
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  col <- if ("Gene" %in% names(df)) "Gene" else names(df)[1]
  unique(trimws(as.character(df[[col]])))
}

tbl_path <- file.path(outdir, "Table_competitor_efficiency_DE.csv")
if (!file.exists(tbl_path)) {
  stop("Run benchmark-competitor-efficiency-de.R first: missing ", tbl_path)
}
tbl <- utils::read.csv(tbl_path, stringsAsFactors = FALSE)

ref_path <- file.path(outdir, "GExPipe_deseq2_DE_sig.csv")
ref <- read_genes(ref_path)
if (!length(ref)) warning("No GExPipe_deseq2_DE_sig.csv — Jaccard columns may stay empty")

map <- list(
  GEOexplorer = file.path(man_dir, "GEOexplorer_DE_sig.csv"),
  `Shiny-Seq` = file.path(man_dir, "ShinySeq_DE_sig.csv"),
  iGEAK = file.path(man_dir, "iGEAK_DE_sig.csv")
)

# Optional filled timings
timing_path <- file.path(outdir, "manual_competitor_timing_TEMPLATE.csv")
timings <- if (file.exists(timing_path)) utils::read.csv(timing_path, stringsAsFactors = FALSE) else NULL

for (tool in names(map)) {
  genes <- read_genes(map[[tool]])
  i <- which(tbl$Tool == tool)[1]
  if (is.na(i)) next
  if (length(genes)) {
    tbl$n_DEGs[i] <- length(genes)
    if (length(ref)) tbl$Jaccard_vs_GExPipe_deseq2[i] <- round(jaccard(genes, ref), 3)
    cat(tool, ": n=", length(genes),
        " Jaccard vs GExPipe DESeq2=", tbl$Jaccard_vs_GExPipe_deseq2[i], "\n", sep = "")
  }
  if (!is.null(timings) && tool %in% timings$Tool) {
    sec <- timings$Seconds_to_DEG_or_main_result[timings$Tool == tool][1]
    if (!is.na(sec) && nzchar(as.character(sec))) {
      tbl$Seconds_to_DEG_table[i] <- as.numeric(sec)
    }
  }
}

utils::write.csv(tbl, tbl_path, row.names = FALSE)
cat("Updated:", tbl_path, "\n")
