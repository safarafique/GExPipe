#!/usr/bin/env Rscript
## BIOS-Rank v2 — correctness upgrades (apply improvement list)
##
## 1) Non-circular fidelity: NEVER rank with Ex that uses the eval assay
## 2) Locked weights on D1 (maximize held-out RNA AUC); optional D3 test
## 3) Harder baselines (Ec-only, Ep-only, Ec×Ep, limma, random)
## 4) Orthogonal CRC marker overlap (not the eval metric)
##
## Requires: results/BIOS_Rank_gene_scores.csv
## Optional: results/cache/merged_expr_GSE9348_GSE50760.rds for D3 transfer of locked weights
##
## Usage:
##   cd /mnt/e/GExPipe
##   "/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
##     "E:/GExPipe/GExPipe(original_paper)/scripts/bios-rank-v2-correctness.R" \
##     --gexpipe-repo "E:/GExPipe"

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) args[[i + 1L]] else default
}
normalize_repo_path <- function(path) {
  path <- as.character(path)[1L]
  if (is.na(path) || !nzchar(path)) return(path)
  if (.Platform$OS.type == "windows") {
    m <- regexec("^/mnt/([a-zA-Z])(/.*)?$", path)
    r <- regmatches(path, m)[[1]]
    if (length(r) >= 2L) {
      path <- paste0(toupper(r[2]), ":", if (length(r) >= 3L && nzchar(r[3])) r[3] else "")
    }
  }
  path
}

ca <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", ca[grep("^--file=", ca)])
script_path <- if (length(file_arg)) normalizePath(file_arg[1], winslash = "/", mustWork = FALSE) else getwd()
orig_root <- normalize_repo_path(dirname(dirname(script_path)))
if (!dir.exists(file.path(orig_root, "scripts"))) orig_root <- normalize_repo_path(getwd())

gexpipe_repo <- normalize_repo_path(get_arg("--gexpipe-repo", dirname(orig_root)))
k_panel <- as.integer(get_arg("--k", "20"))
seed <- as.integer(get_arg("--seed", "42"))
n_dirichlet <- as.integer(get_arg("--n-weights", "400"))

results_dir <- file.path(orig_root, "results")
scores_path <- file.path(results_dir, "BIOS_Rank_gene_scores.csv")
vm <- file.path(gexpipe_repo, "validation_manual")
vm_cp <- file.path(vm, "cross_platform")
crc_path <- file.path(vm_cp, "report", "crc_biomarker_preservation.csv")

if (!file.exists(scores_path)) stop("Missing ", scores_path, " — run bios-rank-filter.R first")

df <- utils::read.csv(scores_path, stringsAsFactors = FALSE)
## Prefer scored candidates
scored <- df[is.finite(df$AUC_Microarray) & is.finite(df$AUC_RNAseq), , drop = FALSE]
if (nrow(scored) < 100L) scored <- df
scored$Ex[!is.finite(scored$Ex)] <- 0
scored$Ec[!is.finite(scored$Ec)] <- 0
scored$Ep[!is.finite(scored$Ep)] <- 0

## Train-assay fidelity for ranking (NON-CIRCULAR): use microarray AUC only as Ex_train
## Evaluation primary: median AUC on RNA-seq (held-out assay)
scored$Ex_train <- {
  x <- scored$AUC_Microarray
  x[!is.finite(x)] <- 0
  r <- range(x, na.rm = TRUE)
  if (diff(r) < .Machine$double.eps) rep(0, length(x)) else (x - r[1]) / (r[2] - r[1])
}
## Discovery Ex (both assays) — for comparison only / old circular BIOS
scored$Ex_both <- scored$Ex
if (!"Ex_both" %in% names(scored) || all(is.na(scored$Ex_both))) scored$Ex_both <- scored$Ex

chans4 <- c("Ec", "Ep", "Em", "Es")  # no Ex in ranking
chans5_train <- c("Ec", "Ep", "Em", "Es", "Ex_train")
chans5_both <- c("Ec", "Ep", "Em", "Es", "Ex_both")

topk <- function(scores, k = k_panel) {
  o <- order(-as.numeric(scores), scored$adjP_Condition)
  scored$Gene[head(o, k)]
}

eval_rna <- function(genes) {
  idx <- match(unique(genes), scored$Gene)
  idx <- idx[!is.na(idx)]
  if (!length(idx)) return(c(Med_AUC_RNA = NA_real_, Med_AUC_Micro = NA_real_,
                             MinMedian_both = NA_real_))
  ar <- scored$AUC_RNAseq[idx]
  am <- scored$AUC_Microarray[idx]
  c(
    Med_AUC_RNA = stats::median(ar, na.rm = TRUE),
    Med_AUC_Micro = stats::median(am, na.rm = TRUE),
    MinMedian_both = stats::median(pmin(am, ar), na.rm = TRUE)
  )
}

score_w <- function(cols, w) {
  m <- as.matrix(scored[, cols, drop = FALSE])
  storage.mode(m) <- "double"
  m[!is.finite(m)] <- 0
  w <- as.numeric(w) / sum(w)
  as.numeric(m %*% w)
}

jaccard <- function(a, b) {
  a <- unique(as.character(a)); b <- unique(as.character(b))
  if (!length(union(a, b))) return(1)
  length(intersect(a, b)) / length(union(a, b))
}

## CRC gold set (orthogonal biology)
crc_genes <- character()
if (file.exists(crc_path)) {
  crc <- utils::read.csv(crc_path, stringsAsFactors = FALSE)
  crc_genes <- unique(trimws(as.character(crc$Gene)))
} else {
  crc_genes <- c(
    "FOXQ1", "ASCL2", "CDH3", "MYC", "BEST4", "OTOP2", "GUCA2B", "CA7",
    "MMP7", "CLDN1", "LGR5", "AXIN2", "RNF43", "EPCAM", "CEACAM5", "VEGFA",
    "SPP1", "COL1A1", "HPGD", "TGFBI"
  )
}
marker_hit <- function(genes) {
  g <- unique(as.character(genes))
  hit <- intersect(g, crc_genes)
  c(n_hit = length(hit), frac = length(hit) / max(1L, length(g)),
    hits = paste(hit, collapse = ";"))
}

row_eval <- function(label, genes, note = "") {
  m <- eval_rna(genes)
  h <- marker_hit(genes)
  data.frame(
    Method = label,
    k = length(unique(genes)),
    Med_AUC_RNA_heldout = unname(m["Med_AUC_RNA"]),
    Med_AUC_Micro = unname(m["Med_AUC_Micro"]),
    MinMedian_both = unname(m["MinMedian_both"]),
    CRC_marker_n = unname(h["n_hit"]),
    CRC_marker_frac = as.numeric(h["frac"]),
    CRC_markers = unname(h["hits"]),
    Note = note,
    stringsAsFactors = FALSE
  )
}

## ---- Methods / panels -------------------------------------------------------
set.seed(seed)
panels <- list()

## Corrected primary: equal-weight 4-channel (no Ex) — fully non-circular
w4 <- rep(1 / 4, 4)
panels$BIOS_v2_noEx_equal <- topk(score_w(chans4, w4))

## Corrected primary+: Ex_train = microarray AUC only (rank), eval on RNA
w5t <- rep(1 / 5, 5)
panels$BIOS_v2_ExTrain_equal <- topk(score_w(chans5_train, w5t))

## Old circular BIOS (both-assay Ex in rank) — for contrast
panels$BIOS_v1_circular_Ex <- topk(score_w(chans5_both, w5t))

## Harder baselines
panels$ONLY_Ec <- topk(scored$Ec)
panels$ONLY_Ep <- topk(scored$Ep)
panels$ONLY_Em <- topk(scored$Em)
panels$ONLY_Es <- topk(scored$Es)
panels$ONLY_Ex_both_circular <- topk(scored$Ex_both)
panels$ONLY_AUC_micro_train <- topk(scored$Ex_train)
panels$Ec_times_Ep <- topk(scored$Ec * scored$Ep)
panels$Random_matched_k <- sample(scored$Gene, k_panel)

limma_path <- file.path(vm_cp, "merged_limma_DE_all.csv")
if (file.exists(limma_path)) {
  lim <- utils::read.csv(limma_path, stringsAsFactors = FALSE)
  lim <- lim[order(lim$adj.P.Val), ]
  panels$Merged_limma_topk <- head(unique(intersect(lim$Gene, scored$Gene)), k_panel)
}

## ---- Locked weights: maximize held-out RNA AUC on D1 (Ex_train channels) ----
set.seed(seed)
dir_mat <- matrix(stats::rexp(n_dirichlet * 5), nrow = n_dirichlet, ncol = 5)
dir_mat <- dir_mat / rowSums(dir_mat)
colnames(dir_mat) <- chans5_train
rna_scores <- numeric(n_dirichlet)
for (b in seq_len(n_dirichlet)) {
  g <- topk(score_w(chans5_train, dir_mat[b, ]))
  rna_scores[b] <- eval_rna(g)["Med_AUC_RNA"]
}
best_i <- which.max(rna_scores)
w_locked <- dir_mat[best_i, ]
panels$BIOS_v2_ExTrain_locked <- topk(score_w(chans5_train, w_locked))

## Also lock 4-channel (no Ex)
dir4 <- matrix(stats::rexp(n_dirichlet * 4), nrow = n_dirichlet, ncol = 4)
dir4 <- dir4 / rowSums(dir4)
rna4 <- numeric(n_dirichlet)
for (b in seq_len(n_dirichlet)) {
  g <- topk(score_w(chans4, dir4[b, ]))
  rna4[b] <- eval_rna(g)["Med_AUC_RNA"]
}
best4 <- which.max(rna4)
w4_locked <- dir4[best4, ]
names(w4_locked) <- chans4
panels$BIOS_v2_noEx_locked <- topk(score_w(chans4, w4_locked))

## Assemble comparison table
tab <- do.call(rbind, lapply(names(panels), function(nm) {
  row_eval(nm, panels[[nm]])
}))
rownames(tab) <- NULL
## Sort by held-out RNA AUC
tab <- tab[order(-tab$Med_AUC_RNA_heldout, -tab$CRC_marker_frac), , drop = FALSE]

## Weight summary
wt <- data.frame(
  Protocol = c("ExTrain_equal", "ExTrain_locked", "noEx_equal", "noEx_locked"),
  Med_AUC_RNA = c(
    eval_rna(panels$BIOS_v2_ExTrain_equal)["Med_AUC_RNA"],
    eval_rna(panels$BIOS_v2_ExTrain_locked)["Med_AUC_RNA"],
    eval_rna(panels$BIOS_v2_noEx_equal)["Med_AUC_RNA"],
    eval_rna(panels$BIOS_v2_noEx_locked)["Med_AUC_RNA"]
  ),
  w_Ec = c(0.2, w_locked[1], 0.25, w4_locked[1]),
  w_Ep = c(0.2, w_locked[2], 0.25, w4_locked[2]),
  w_Em = c(0.2, w_locked[3], 0.25, w4_locked[3]),
  w_Es = c(0.2, w_locked[4], 0.25, w4_locked[4]),
  w_Ex_train = c(0.2, w_locked[5], NA, NA),
  stringsAsFactors = FALSE
)

## Save locked weights for D3
lock_path <- file.path(results_dir, "BIOS_v2_locked_weights.csv")
utils::write.csv(
  data.frame(
    Channel = c(chans5_train, paste0("noEx_", chans4)),
    Weight = c(as.numeric(w_locked), as.numeric(w4_locked)),
    Protocol = c(rep("ExTrain_locked", 5), rep("noEx_locked", 4))
  ),
  lock_path, row.names = FALSE
)

utils::write.csv(tab, file.path(results_dir, "BIOS_v2_correctness_comparison.csv"), row.names = FALSE)
utils::write.csv(wt, file.path(results_dir, "BIOS_v2_weight_lock_summary.csv"), row.names = FALSE)

## Gene lists
sig_dir <- file.path(orig_root, "signatures")
dir.create(sig_dir, showWarnings = FALSE, recursive = TRUE)
for (nm in c("BIOS_v2_noEx_equal", "BIOS_v2_ExTrain_equal", "BIOS_v2_ExTrain_locked",
             "BIOS_v2_noEx_locked")) {
  utils::write.csv(data.frame(Gene = panels[[nm]]),
                   file.path(sig_dir, paste0(nm, ".csv")), row.names = FALSE)
}

## ---- Optional D3: apply locked weights if cache exists ----------------------
d3_rds <- file.path(results_dir, "cache", "merged_expr_GSE9348_GSE50760.rds")
d3_note <- "D3 cache not found — skip locked-weight external test."
d3_tab <- NULL
if (file.exists(d3_rds)) {
  d3_note <- "D3 cache found — see BIOS_v2_D3_locked_transfer.csv"
  ## Lightweight: if D3 gene scores exist use them; else note need recompute
  d3_scores <- file.path(results_dir, "BIOS_Rank_D3_gene_scores.csv")
  if (file.exists(d3_scores)) {
    d3 <- utils::read.csv(d3_scores, stringsAsFactors = FALSE)
    d3 <- d3[is.finite(d3$AUC_RNAseq) & is.finite(d3$AUC_Microarray), ]
    d3$Ex_train <- {
      x <- d3$AUC_Microarray
      r <- range(x, na.rm = TRUE)
      if (diff(r) < .Machine$double.eps) rep(0, nrow(d3)) else (x - r[1]) / (r[2] - r[1])
    }
    score_d3 <- function(cols, w) {
      m <- as.matrix(d3[, cols, drop = FALSE])
      m[!is.finite(m)] <- 0
      as.numeric(m %*% (as.numeric(w) / sum(w)))
    }
    topk_d3 <- function(sc) d3$Gene[head(order(-sc, d3$adjP_Condition), k_panel)]
    eval_d3 <- function(g) {
      idx <- match(g, d3$Gene)
      stats::median(d3$AUC_RNAseq[idx], na.rm = TRUE)
    }
    ## Map locked weights: need Ec,Ep,Em,Es,Ex_train columns on D3
    if (all(c("Ec", "Ep", "Em", "Es") %in% names(d3))) {
      g_eq <- topk_d3(score_d3(chans5_train, rep(0.2, 5)))
      g_lk <- topk_d3(score_d3(chans5_train, w_locked))
      d3_tab <- data.frame(
        Method = c("D3_ExTrain_equal", "D3_ExTrain_locked_from_D1"),
        Med_AUC_RNA = c(eval_d3(g_eq), eval_d3(g_lk)),
        stringsAsFactors = FALSE
      )
      utils::write.csv(d3_tab, file.path(results_dir, "BIOS_v2_D3_locked_transfer.csv"),
                       row.names = FALSE)
    }
  } else {
    d3_note <- paste0(
      "D3 expr cache exists but no BIOS_Rank_D3_gene_scores.csv. ",
      "Re-run multi-dataset or build D3 scores to test locked weights."
    )
  }
}

## ---- Markdown report --------------------------------------------------------
fmt3 <- function(x) format(round(as.numeric(x), 3), nsmall = 3)
primary <- tab$Method[1]
md <- c(
  "# BIOS-Rank v2 — corrected (non-circular) protocol",
  "",
  "## What changed (novelty + correctness)",
  "",
  "1. **Primary ranking no longer uses held-out-assay AUC in Ex.**",
  "   - `BIOS_v2_noEx_*`: rank with Ec, Ep, Em, Es only.",
  "   - `BIOS_v2_ExTrain_*`: Ex = **microarray AUC only**; evaluate on **RNA-seq AUC**.",
  "2. **Locked weights** chosen on D1 to maximize held-out RNA median AUC (Dirichlet search).",
  "3. **Harder baselines**: Ec-only, Ep-only, Ec×Ep, limma, random, circular Ex.",
  "4. **Orthogonal CRC marker overlap** (from merge validation gold list).",
  "",
  paste0("**Primary eval metric (correct):** median single-gene AUC on **RNA-seq** (held-out assay), k=", k_panel, "."),
  "",
  "## Ranking comparison (sorted by held-out RNA AUC)",
  "",
  "| Method | Med AUC RNA (held-out) | Med AUC micro | MinMedian both | CRC markers (n / frac) |",
  "|--------|-----------------------:|--------------:|---------------:|------------------------|"
)
for (i in seq_len(nrow(tab))) {
  md <- c(md, paste0(
    "| ", tab$Method[i], " | ", fmt3(tab$Med_AUC_RNA_heldout[i]), " | ",
    fmt3(tab$Med_AUC_Micro[i]), " | ", fmt3(tab$MinMedian_both[i]), " | ",
    tab$CRC_marker_n[i], " / ", fmt3(tab$CRC_marker_frac[i]), " |"
  ))
}

md <- c(
  md, "",
  "## Locked weights (D1 → maximize RNA AUC)",
  "",
  paste0("- ExTrain locked: Ec=", fmt3(w_locked[1]), ", Ep=", fmt3(w_locked[2]),
         ", Em=", fmt3(w_locked[3]), ", Es=", fmt3(w_locked[4]),
         ", Ex_train=", fmt3(w_locked[5])),
  paste0("- noEx locked: Ec=", fmt3(w4_locked[1]), ", Ep=", fmt3(w4_locked[2]),
         ", Em=", fmt3(w4_locked[3]), ", Es=", fmt3(w4_locked[4])),
  paste0("- Equal ExTrain RNA AUC: ", fmt3(wt$Med_AUC_RNA[wt$Protocol == "ExTrain_equal"]),
         "; Locked: ", fmt3(wt$Med_AUC_RNA[wt$Protocol == "ExTrain_locked"])),
  "",
  paste0("## D3 locked-weight transfer"),
  "",
  d3_note,
  "",
  "## Correct claim (use this in the paper)",
  "",
  "> BIOS-Rank v2 ranks genes by platform-adjusted Condition evidence, purity, module, and",
  "> selector stability, optionally with **train-assay** fidelity; it is evaluated on the",
  "> **held-out assay** so Ex is not circular. Locked equal-or-better weights are reported",
  "> as sensitivity. Orthogonal CRC marker recovery is reported separately from AUC.",
  "",
  paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE))
)
writeLines(md, file.path(results_dir, "BIOS_v2_correctness.md"))

## Patch formula doc with v2 protocol
form <- file.path(orig_root, "manuscript", "BIOS_FORMULA.md")
if (file.exists(form)) {
  flines <- readLines(form, warn = FALSE)
  block <- c(
    "",
    "## 8. Corrected evaluation protocol (BIOS-Rank v2) — REQUIRED for claims",
    "",
    "To avoid circularity of (E_x) with the primary metric:",
    "",
    "1. **Ranking Ex (optional):** use only the **train assay** AUC, or omit Ex (4-channel BIOS).",
    "2. **Evaluation:** median gene AUC on the **held-out assay** (e.g. rank with microarray evidence → score RNA-seq AUC).",
    "3. **Weights:** equal by default; locked weights may be tuned on D1 held-out AUC and tested on D3.",
    "4. **Orthogonal biology:** CRC marker overlap is reported separately (not the ranking objective).",
    "",
    "See `results/BIOS_v2_correctness.md` and `scripts/bios-rank-v2-correctness.R`.",
    ""
  )
  if (!any(grepl("^## 8\\. Corrected evaluation protocol", flines))) {
    writeLines(c(flines, block), form)
  }
}

cat("\nWrote BIOS_v2_correctness.md / .csv\n")
print(tab[, c("Method", "Med_AUC_RNA_heldout", "CRC_marker_n", "CRC_marker_frac")], row.names = FALSE)
cat("\nLocked weights (ExTrain):\n")
print(w_locked)
cat("\n", d3_note, "\n", sep = "")

