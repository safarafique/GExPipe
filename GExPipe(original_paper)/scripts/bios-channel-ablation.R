#!/usr/bin/env Rscript
## BIOS-Rank channel ablation + weight sensitivity (D1 GSE89076 × GSE50760)
##
## Shows (1) each evidence channel is necessary (leave-one-out Δ metric),
##       (2) equal weights are near-optimal / robust (Dirichlet weight search).
##
## Requires: results/BIOS_Rank_gene_scores.csv  (from bios-rank-filter.R)
##
## Usage:
##   cd "/mnt/e/GExPipe/GExPipe(original_paper)"
##   "/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
##     scripts/bios-channel-ablation.R --gexpipe-repo "E:/GExPipe"

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
n_dirichlet <- as.integer(get_arg("--n-weights", "500"))

results_dir <- file.path(orig_root, "results")
scores_path <- file.path(results_dir, "BIOS_Rank_gene_scores.csv")
vm_cp <- file.path(gexpipe_repo, "validation_manual", "cross_platform")

if (!file.exists(scores_path)) {
  stop("Missing ", scores_path, "\nRun bios-rank-filter.R first.")
}

df <- utils::read.csv(scores_path, stringsAsFactors = FALSE)
chans <- c("Ec", "Ep", "Em", "Es", "Ex")
stopifnot(all(chans %in% names(df)))

## Prefer genes with finite Ex (scored candidates); fall back to all rows
scored <- df[is.finite(df$Ex) & is.finite(df$AUC_Microarray) & is.finite(df$AUC_RNAseq), , drop = FALSE]
if (nrow(scored) < 100L) scored <- df
cat("Universe for ranking: ", nrow(scored), " genes with finite AUCs\n", sep = "")

mat <- as.matrix(scored[, chans])
storage.mode(mat) <- "double"
mat[!is.finite(mat)] <- 0

## Primary metric on a gene panel (same as bios-rank-filter.R)
panel_metric <- function(genes) {
  genes <- unique(as.character(genes))
  idx <- match(genes, scored$Gene)
  idx <- idx[!is.na(idx)]
  if (!length(idx)) {
    return(c(
      MinMedian_cross_assay = NA_real_,
      Median_AUC_Microarray = NA_real_,
      Median_AUC_RNAseq = NA_real_,
      Mean_BIOS_full = NA_real_
    ))
  }
  am <- scored$AUC_Microarray[idx]
  ar <- scored$AUC_RNAseq[idx]
  c(
    MinMedian_cross_assay = stats::median(pmin(am, ar), na.rm = TRUE),
    Median_AUC_Microarray = stats::median(am, na.rm = TRUE),
    Median_AUC_RNAseq = stats::median(ar, na.rm = TRUE),
    Mean_BIOS_full = mean(scored$BIOS_Rank[idx], na.rm = TRUE)
  )
}

jaccard <- function(a, b) {
  a <- unique(as.character(a)); b <- unique(as.character(b))
  if (!length(a) && !length(b)) return(1)
  length(intersect(a, b)) / length(union(a, b))
}

score_with_weights <- function(w) {
  w <- as.numeric(w)
  w <- w / sum(w)
  as.numeric(mat %*% w)
}

topk_genes <- function(scores, k = k_panel) {
  o <- order(-scores, scored$adjP_Condition)
  scored$Gene[head(o, k)]
}

eval_weights <- function(label, w, kind = "weighted") {
  scores <- score_with_weights(w)
  g <- topk_genes(scores)
  m <- panel_metric(g)
  data.frame(
    Setting = label,
    Kind = kind,
    w_Ec = w[1], w_Ep = w[2], w_Em = w[3], w_Es = w[4], w_Ex = w[5],
    k = length(g),
    MinMedian_cross_assay = unname(m["MinMedian_cross_assay"]),
    Median_AUC_Microarray = unname(m["Median_AUC_Microarray"]),
    Median_AUC_RNAseq = unname(m["Median_AUC_RNAseq"]),
    Jaccard_vs_equal = NA_real_,
    stringsAsFactors = FALSE
  )
}

## ---- Equal-weight full BIOS -------------------------------------------------
w_eq <- rep(1 / 5, 5)
names(w_eq) <- chans
g_full <- topk_genes(score_with_weights(w_eq))
m_full <- panel_metric(g_full)
full_row <- eval_weights("BIOS_equal_weights", w_eq, "full")
full_row$Jaccard_vs_equal <- 1

## ---- Leave-one-out (set channel weight to 0; renormalize) -------------------
loo_rows <- list()
for (i in seq_along(chans)) {
  w <- w_eq
  w[i] <- 0
  if (sum(w) <= 0) next
  w <- w / sum(w)
  row <- eval_weights(paste0("LOO_drop_", chans[i]), w, "leave_one_out")
  row$Jaccard_vs_equal <- jaccard(topk_genes(score_with_weights(w)), g_full)
  loo_rows[[chans[i]]] <- row
}
loo <- do.call(rbind, loo_rows)
loo$Jaccard_vs_equal <- vapply(seq_along(chans), function(i) {
  w <- w_eq; w[i] <- 0; w <- w / sum(w)
  jaccard(topk_genes(score_with_weights(w)), g_full)
}, numeric(1))

## ---- Single-channel only ----------------------------------------------------
single_rows <- list()
for (i in seq_along(chans)) {
  w <- rep(0, 5); w[i] <- 1
  row <- eval_weights(paste0("ONLY_", chans[i]), w, "single_channel")
  row$Jaccard_vs_equal <- jaccard(topk_genes(score_with_weights(w)), g_full)
  single_rows[[chans[i]]] <- row
}
single <- do.call(rbind, single_rows)

## ---- Limma / random baselines (same metric, same scored universe) -----------
baseline_rows <- list()
limma_path <- file.path(vm_cp, "merged_limma_DE_all.csv")
if (file.exists(limma_path)) {
  lim <- utils::read.csv(limma_path, stringsAsFactors = FALSE)
  lim <- lim[order(lim$adj.P.Val), ]
  g_lim <- head(unique(intersect(lim$Gene, scored$Gene)), k_panel)
  m_lim <- panel_metric(g_lim)
  baseline_rows[["Merged_limma_topk"]] <- data.frame(
    Setting = "Merged_limma_topk", Kind = "baseline",
    w_Ec = NA, w_Ep = NA, w_Em = NA, w_Es = NA, w_Ex = NA,
    k = length(g_lim),
    MinMedian_cross_assay = unname(m_lim["MinMedian_cross_assay"]),
    Median_AUC_Microarray = unname(m_lim["Median_AUC_Microarray"]),
    Median_AUC_RNAseq = unname(m_lim["Median_AUC_RNAseq"]),
    Jaccard_vs_equal = jaccard(g_lim, g_full),
    Delta_vs_full = unname(m_lim["MinMedian_cross_assay"] - m_full["MinMedian_cross_assay"]),
    stringsAsFactors = FALSE
  )
}
set.seed(seed)
g_rand <- sample(scored$Gene, k_panel)
m_rand <- panel_metric(g_rand)
baseline_rows[["Random_matched_k"]] <- data.frame(
  Setting = "Random_matched_k", Kind = "baseline",
  w_Ec = NA, w_Ep = NA, w_Em = NA, w_Es = NA, w_Ex = NA,
  k = length(g_rand),
  MinMedian_cross_assay = unname(m_rand["MinMedian_cross_assay"]),
  Median_AUC_Microarray = unname(m_rand["Median_AUC_Microarray"]),
  Median_AUC_RNAseq = unname(m_rand["Median_AUC_RNAseq"]),
  Jaccard_vs_equal = jaccard(g_rand, g_full),
  Delta_vs_full = unname(m_rand["MinMedian_cross_assay"] - m_full["MinMedian_cross_assay"]),
  stringsAsFactors = FALSE
)

## ---- Dirichlet weight search (robustness + near-optimal weights) ------------
## Sample w ~ Dirichlet(1,...,1) on the simplex; score primary metric.
set.seed(seed)
dir_mat <- matrix(stats::rexp(n_dirichlet * 5), nrow = n_dirichlet, ncol = 5)
dir_mat <- dir_mat / rowSums(dir_mat)
colnames(dir_mat) <- chans

dir_metrics <- numeric(n_dirichlet)
dir_jacc <- numeric(n_dirichlet)
for (b in seq_len(n_dirichlet)) {
  sc <- score_with_weights(dir_mat[b, ])
  g <- topk_genes(sc)
  dir_metrics[b] <- panel_metric(g)["MinMedian_cross_assay"]
  dir_jacc[b] <- jaccard(g, g_full)
}
best_i <- which.max(dir_metrics)
w_best <- dir_mat[best_i, ]
best_row <- eval_weights("BIOS_best_Dirichlet", w_best, "weight_search")
best_row$Jaccard_vs_equal <- dir_jacc[best_i]

## Structured boost: put mass α on one channel, rest equal
boost_rows <- list()
alphas <- c(0.4, 0.6, 0.8)
for (ch in chans) {
  for (a in alphas) {
    w <- rep((1 - a) / 4, 5)
    names(w) <- chans
    w[ch] <- a
    lab <- sprintf("BOOST_%s_a%.1f", ch, a)
    row <- eval_weights(lab, w, "weight_boost")
    row$Jaccard_vs_equal <- jaccard(topk_genes(score_with_weights(w)), g_full)
    boost_rows[[lab]] <- row
  }
}
boost <- do.call(rbind, boost_rows)

## ---- Assemble tables --------------------------------------------------------
add_delta <- function(d, full_val) {
  d$Delta_vs_full <- d$MinMedian_cross_assay - full_val
  d
}
full_val <- as.numeric(m_full["MinMedian_cross_assay"])
full_row$Jaccard_vs_equal <- 1
full_row <- add_delta(full_row, full_val)  # → 0
loo <- add_delta(loo, full_val)
single <- add_delta(single, full_val)
best_row <- add_delta(best_row, full_val)
boost <- add_delta(boost, full_val)

## Baselines already include Delta_vs_full; ensure same column order
col_order <- c(
  "Setting", "Kind", "w_Ec", "w_Ep", "w_Em", "w_Es", "w_Ex",
  "k", "MinMedian_cross_assay", "Median_AUC_Microarray", "Median_AUC_RNAseq",
  "Jaccard_vs_equal", "Delta_vs_full"
)
align <- function(d) {
  for (nm in col_order) if (!nm %in% names(d)) d[[nm]] <- NA
  d[, col_order, drop = FALSE]
}
ablation <- rbind(
  align(full_row),
  align(loo),
  align(single),
  align(do.call(rbind, baseline_rows)),
  align(best_row),
  align(boost)
)
rownames(ablation) <- NULL

## Weight-search summary
wt_sum <- data.frame(
  n_draws = n_dirichlet,
  Equal_MinMedian = full_val,
  Best_MinMedian = max(dir_metrics, na.rm = TRUE),
  Median_random_w = stats::median(dir_metrics, na.rm = TRUE),
  P25 = as.numeric(stats::quantile(dir_metrics, 0.25, na.rm = TRUE)),
  P75 = as.numeric(stats::quantile(dir_metrics, 0.75, na.rm = TRUE)),
  Pct_draws_ge_equal = mean(dir_metrics >= full_val - 1e-12),
  Pct_draws_gt_equal = mean(dir_metrics > full_val + 1e-12),
  Best_w_Ec = w_best[1], Best_w_Ep = w_best[2], Best_w_Em = w_best[3],
  Best_w_Es = w_best[4], Best_w_Ex = w_best[5],
  Equal_vs_best_gap = full_val - max(dir_metrics, na.rm = TRUE),
  stringsAsFactors = FALSE
)

## Channel necessity summary
nec <- data.frame(
  Channel = chans,
  Meaning = c(
    "Condition strength (−log10 adjP × |βC|)",
    "Platform purity |βC|/(|βC|+|βP|)",
    "Trait-WGCNA module",
    "ML/consensus stability",
    "Cross-assay min AUC"
  ),
  LOO_MinMedian = loo$MinMedian_cross_assay[match(paste0("LOO_drop_", chans), loo$Setting)],
  Delta_vs_full = loo$Delta_vs_full[match(paste0("LOO_drop_", chans), loo$Setting)],
  Jaccard_panel_change = 1 - loo$Jaccard_vs_equal[match(paste0("LOO_drop_", chans), loo$Setting)],
  ONLY_MinMedian = single$MinMedian_cross_assay[match(paste0("ONLY_", chans), single$Setting)],
  stringsAsFactors = FALSE
)
nec$Hurts_when_dropped <- nec$Delta_vs_full < -1e-6
nec$Role <- ifelse(
  nec$Hurts_when_dropped, "necessary (drop ↓ metric)",
  ifelse(nec$Jaccard_panel_change > 0.05, "changes panel composition", "saturated at this k (panel unchanged)")
)

utils::write.csv(ablation, file.path(results_dir, "BIOS_channel_ablation.csv"), row.names = FALSE)
utils::write.csv(nec, file.path(results_dir, "BIOS_channel_necessity.csv"), row.names = FALSE)
utils::write.csv(wt_sum, file.path(results_dir, "BIOS_weight_sensitivity_summary.csv"), row.names = FALSE)
utils::write.csv(
  data.frame(draw = seq_len(n_dirichlet), MinMedian = dir_metrics, Jaccard_vs_equal = dir_jacc, dir_mat),
  file.path(results_dir, "BIOS_weight_dirichlet_draws.csv"),
  row.names = FALSE
)

## ---- Multi-k LOO (Em/Ep often saturated at k=20) ----------------------------
ks <- sort(unique(c(k_panel, 50L, 100L)))
multi_rows <- list()
for (kk in ks) {
  g0 <- topk_genes(score_with_weights(w_eq), k = kk)
  m0 <- panel_metric(g0)["MinMedian_cross_assay"]
  for (i in seq_along(chans)) {
    w <- w_eq; w[i] <- 0; w <- w / sum(w)
    g1 <- topk_genes(score_with_weights(w), k = kk)
    m1 <- panel_metric(g1)["MinMedian_cross_assay"]
    multi_rows[[length(multi_rows) + 1L]] <- data.frame(
      k = kk,
      Channel = chans[i],
      Full_MinMedian = unname(m0),
      LOO_MinMedian = unname(m1),
      Delta = unname(m1 - m0),
      Jaccard_vs_full = jaccard(g1, g0),
      stringsAsFactors = FALSE
    )
  }
}
multi_k <- do.call(rbind, multi_rows)
utils::write.csv(multi_k, file.path(results_dir, "BIOS_channel_loo_multik.csv"), row.names = FALSE)

## ---- Biology secondary metrics (not circular with Ex) -----------------------
bio_of <- function(genes) {
  idx <- match(unique(genes), scored$Gene)
  idx <- idx[!is.na(idx)]
  c(
    Mean_Ep = mean(scored$Ep[idx], na.rm = TRUE),
    Frac_Em = mean(scored$Em[idx] >= 1, na.rm = TRUE),
    Frac_Es = mean(scored$Es[idx] >= 1, na.rm = TRUE),
    Mean_Ec = mean(scored$Ec[idx], na.rm = TRUE)
  )
}
bio_rows <- list()
settings_bio <- list(
  BIOS_equal = g_full,
  ONLY_Ec = topk_genes(score_with_weights(c(1, 0, 0, 0, 0))),
  ONLY_Ep = topk_genes(score_with_weights(c(0, 1, 0, 0, 0))),
  ONLY_Em = topk_genes(score_with_weights(c(0, 0, 1, 0, 0))),
  ONLY_Es = topk_genes(score_with_weights(c(0, 0, 0, 1, 0))),
  ONLY_Ex = topk_genes(score_with_weights(c(0, 0, 0, 0, 1))),
  BIOS_best_Dirichlet = topk_genes(score_with_weights(w_best))
)
if (file.exists(limma_path)) {
  lim <- utils::read.csv(limma_path, stringsAsFactors = FALSE)
  lim <- lim[order(lim$adj.P.Val), ]
  settings_bio$Merged_limma_topk <- head(unique(intersect(lim$Gene, scored$Gene)), k_panel)
}
for (nm in names(settings_bio)) {
  g <- settings_bio[[nm]]
  m <- panel_metric(g)
  b <- bio_of(g)
  bio_rows[[nm]] <- data.frame(
    Setting = nm,
    MinMedian_cross_assay = unname(m["MinMedian_cross_assay"]),
    Mean_Ep = unname(b["Mean_Ep"]),
    Frac_Em = unname(b["Frac_Em"]),
    Frac_Es = unname(b["Frac_Es"]),
    Mean_Ec = unname(b["Mean_Ec"]),
    stringsAsFactors = FALSE
  )
}
bio_tab <- do.call(rbind, bio_rows)
rownames(bio_tab) <- NULL
utils::write.csv(bio_tab, file.path(results_dir, "BIOS_channel_biology_metrics.csv"), row.names = FALSE)

## Paper-facing verdict helpers
n_hurt <- sum(nec$Hurts_when_dropped)
n_comp <- sum(nec$Jaccard_panel_change > 0.05)
chans_hurt <- paste(nec$Channel[nec$Hurts_when_dropped], collapse = ", ")
chans_comp <- paste(nec$Channel[nec$Jaccard_panel_change > 0.05], collapse = ", ")
equal_near_best <- abs(wt_sum$Equal_vs_best_gap) < 0.03
ex_circular_note <- "ONLY_Ex can look best on MinMedian because Ex is both a ranking feature and the evaluation metric; prefer LOO + biology secondaries for necessity claims."

fmt3 <- function(x) format(round(as.numeric(x), 3), nsmall = 3)

md <- c(
  "# BIOS-Rank channel ablation & weight sensitivity (D1)",
  "",
  paste0("Cohort: GSE89076 × GSE50760. Default k=", k_panel,
         ". Primary metric: **min-median cross-assay gene AUC**."),
  paste0("Equal-weight BIOS MinMedian = **", fmt3(full_val), "**."),
  "",
  "## Claim (for the paper)",
  "",
  paste0("1. **Leave-one-out:** dropping **", if (nzchar(chans_hurt)) chans_hurt else "no channel",
         "** lowers MinMedian at k=", k_panel, "; **",
         if (nzchar(chans_comp)) chans_comp else "none",
         "** change top-*k* composition (Jaccard < 0.95)."),
  "2. **Saturation:** at small *k*, Em/Ep may show Δ≈0 and Jaccard=1 because top genes already have Em=Es=Ep≈1 — check multi-*k* table.",
  paste0("3. **Weights:** equal-weight MinMedian=", fmt3(wt_sum$Equal_MinMedian),
         "; best Dirichlet=", fmt3(wt_sum$Best_MinMedian),
         " (gap ", fmt3(wt_sum$Equal_vs_best_gap), "). ",
         if (equal_near_best) "**Equal weights are near-optimal** on this cohort. "
         else "Best weights improve the metric; treat as D1-tuned. ",
         format(round(100 * wt_sum$Pct_draws_ge_equal, 1), nsmall = 1),
         "% of random simplex weights ≥ equal."),
  paste0("4. **", ex_circular_note, "**"),
  "",
  paste0("## 1. Leave-one-out (k=", k_panel, ")"),
  "",
  "| Channel | Drop → MinMedian | Δ vs full | Panel Jaccard vs full | Verdict |",
  "|---------|------------------:|----------:|----------------------:|---------|"
)
for (i in seq_len(nrow(nec))) {
  md <- c(md, paste0(
    "| ", nec$Channel[i], " | ", fmt3(nec$LOO_MinMedian[i]), " | ",
    fmt3(nec$Delta_vs_full[i]), " | ",
    fmt3(1 - nec$Jaccard_panel_change[i]), " | ", nec$Role[i], " |"
  ))
}

md <- c(
  md, "",
  "## 1b. Multi-*k* leave-one-out",
  "",
  "| k | Channel | Full | LOO | Δ | Jaccard |",
  "|---|---------|-----:|----:|--:|--------:|"
)
for (i in seq_len(nrow(multi_k))) {
  md <- c(md, paste0(
    "| ", multi_k$k[i], " | ", multi_k$Channel[i], " | ",
    fmt3(multi_k$Full_MinMedian[i]), " | ", fmt3(multi_k$LOO_MinMedian[i]), " | ",
    fmt3(multi_k$Delta[i]), " | ", fmt3(multi_k$Jaccard_vs_full[i]), " |"
  ))
}

md <- c(
  md, "",
  "## 2. Single-channel only",
  "",
  "| Channel only | MinMedian | Δ vs full |",
  "|--------------|----------:|----------:|"
)
for (i in seq_len(nrow(single))) {
  md <- c(md, paste0(
    "| ", single$Setting[i], " | ", fmt3(single$MinMedian_cross_assay[i]), " | ",
    fmt3(single$Delta_vs_full[i]), " |"
  ))
}

md <- c(
  md, "",
  "## 2b. Biology secondaries (non-circular with Ex)",
  "",
  "| Setting | MinMedian | Mean Ep (purity) | Frac Em | Frac Es | Mean Ec |",
  "|---------|----------:|-----------------:|--------:|--------:|--------:|"
)
for (i in seq_len(nrow(bio_tab))) {
  md <- c(md, paste0(
    "| ", bio_tab$Setting[i], " | ", fmt3(bio_tab$MinMedian_cross_assay[i]), " | ",
    fmt3(bio_tab$Mean_Ep[i]), " | ", fmt3(bio_tab$Frac_Em[i]), " | ",
    fmt3(bio_tab$Frac_Es[i]), " | ", fmt3(bio_tab$Mean_Ec[i]), " |"
  ))
}

md <- c(
  md, "",
  paste0("## 3. Weight sensitivity (Dirichlet, n=", n_dirichlet, ")"),
  "",
  paste0("- Equal-weight MinMedian: **", fmt3(wt_sum$Equal_MinMedian), "**"),
  paste0("- Best random weights MinMedian: **", fmt3(wt_sum$Best_MinMedian),
         "** (gap vs equal: ", fmt3(wt_sum$Equal_vs_best_gap), ")"),
  paste0("- Median / IQR of random-weight MinMedian: ",
         fmt3(wt_sum$Median_random_w), " [", fmt3(wt_sum$P25), ", ", fmt3(wt_sum$P75), "]"),
  paste0("- Fraction of draws ≥ equal: ",
         format(round(100 * wt_sum$Pct_draws_ge_equal, 1), nsmall = 1), "%"),
  paste0("- Best weights (D1-tuned): Ec=", fmt3(wt_sum$Best_w_Ec),
         ", Ep=", fmt3(wt_sum$Best_w_Ep),
         ", Em=", fmt3(wt_sum$Best_w_Em),
         ", Es=", fmt3(wt_sum$Best_w_Es),
         ", Ex=", fmt3(wt_sum$Best_w_Ex)),
  "",
  "**Default recommendation:** keep **equal weights** in Methods; report best Dirichlet as sensitivity. ",
  "Do not replace the formula with D1-tuned weights unless they are locked and validated on D3.",
  "",
  paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE))
)
writeLines(md, file.path(results_dir, "BIOS_channel_ablation.md"))

## Patch Original Paper §4.5b
paper <- file.path(orig_root, "manuscript", "GExPipe_Original_Paper.md")
if (file.exists(paper)) {
  txt <- paste(readLines(paper, warn = FALSE), collapse = "\n")
  sec <- paste0(
    "\n### 4.5b Channel ablation & weight sensitivity\n\n",
    "Leave-one-out + Dirichlet weight search on D1 (see `results/BIOS_channel_ablation.md`). ",
    "Equal-weight MinMedian = ", fmt3(full_val), ". ",
    "At k=", k_panel, ", dropping **", chans_hurt, "** reduces the primary metric; **",
    chans_comp, "** alter panel membership. ",
    "Em/Ep can appear redundant at small *k* because top genes already saturate module/purity—multi-*k* LOO is reported. ",
    "Best Dirichlet MinMedian = ", fmt3(wt_sum$Best_MinMedian),
    " (gap vs equal ", fmt3(wt_sum$Equal_vs_best_gap), "); ",
    format(round(100 * wt_sum$Pct_draws_ge_equal, 1), nsmall = 1),
    "% of random weights ≥ equal. ",
    "**Methods default remains equal weights**; D1-tuned weights are sensitivity only. ",
    "ONLY_Ex can inflate MinMedian (circular with the metric)—biology secondaries (Ep/Em/Es membership) are reported alongside.\n"
  )
  if (grepl("### 4\\.5b Channel ablation", txt)) {
    txt <- sub(
      "### 4\\.5b Channel ablation[\\s\\S]*?(?=\\n### |\\n## |$)",
      trimws(sec),
      txt,
      perl = TRUE
    )
  } else if (grepl("### 4\\.5 BIOS-Rank", txt)) {
    txt <- sub(
      "(### 4\\.5 BIOS-Rank filter validation[\\s\\S]*?)(\\n### 4\\.[0-9])",
      paste0("\\1", sec, "\\2"),
      txt,
      perl = TRUE
    )
  }
  writeLines(strsplit(txt, "\n", fixed = TRUE)[[1]], paper)
  cat("Updated paper §4.5b\n")
}

## Update formula doc note
form <- file.path(orig_root, "manuscript", "BIOS_FORMULA.md")
if (file.exists(form)) {
  flines <- readLines(form, warn = FALSE)
  note <- c(
    "",
    "## 7. Empirical channel necessity (D1 ablation)",
    "",
    paste0("See `results/BIOS_channel_ablation.md` (generated ",
           format(Sys.time(), tz = "UTC", usetz = TRUE), ")."),
    "Equal weights are the **default**; LOO and Dirichlet search justify or refine them.",
    ""
  )
  if (!any(grepl("^## 7\\. Empirical channel necessity", flines))) {
    writeLines(c(flines, note), form)
  }
}

cat("\nWrote:\n")
cat("  ", file.path(results_dir, "BIOS_channel_ablation.csv"), "\n", sep = "")
cat("  ", file.path(results_dir, "BIOS_channel_necessity.csv"), "\n", sep = "")
cat("  ", file.path(results_dir, "BIOS_channel_loo_multik.csv"), "\n", sep = "")
cat("  ", file.path(results_dir, "BIOS_channel_biology_metrics.csv"), "\n", sep = "")
cat("  ", file.path(results_dir, "BIOS_weight_sensitivity_summary.csv"), "\n", sep = "")
cat("  ", file.path(results_dir, "BIOS_channel_ablation.md"), "\n", sep = "")
cat("\n=== Leave-one-out ===\n")
print(nec, row.names = FALSE)
cat("\n=== Weight summary ===\n")
print(wt_sum, row.names = FALSE)
cat("\n=== Biology secondaries ===\n")
print(bio_tab, row.names = FALSE)
