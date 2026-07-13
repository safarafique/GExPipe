#!/usr/bin/env Rscript
## Original Paper C4 — Simulation: joint limma vs separate∩ under known truth
## Lives only under GExPipe(original_paper)/; does not modify Application Note code.
##
## Design (ORIGINAL_PAPER_PLAN Work package E):
##   - 2 platforms, shared true DEGs, platform batch effect
##   - Scenario A: Platform ⊥ Condition (balanced)
##   - Scenario B: Platform partially confounded with Condition
##   - Compare: separate limma → ∩  vs  joint limma ~ Condition + Platform
##
## Usage (WSL):
##   cd "/mnt/e/GExPipe/GExPipe(original_paper)"
##   "/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
##     scripts/simulate-joint-vs-intersect.R --outdir "E:/GExPipe/GExPipe(original_paper)"

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
      drive <- toupper(r[2])
      rest <- if (length(r) >= 3L && nzchar(r[3])) r[3] else ""
      path <- paste0(drive, ":", rest)
    }
  }
  path
}

ca <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", ca[grep("^--file=", ca)])
script_path <- if (length(file_arg)) normalizePath(file_arg[1], winslash = "/", mustWork = FALSE) else getwd()
orig_root <- normalize_repo_path(dirname(dirname(script_path)))
if (!dir.exists(file.path(orig_root, "scripts"))) {
  orig_root <- normalize_repo_path(get_arg("--outdir", getwd()))
}

outdir <- normalize_repo_path(get_arg("--outdir", orig_root))
results_dir <- file.path(outdir, "results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

n_rep <- as.integer(get_arg("--n-rep", "50"))
seed0 <- as.integer(get_arg("--seed", "42"))
n_genes <- as.integer(get_arg("--n-genes", "2000"))
n_true <- as.integer(get_arg("--n-true", "100"))
fdr_cut <- as.numeric(get_arg("--fdr", "0.05"))
lfc_cut <- as.numeric(get_arg("--lfc", "0.25"))
## Intermediate–strong mix: ∩ recovers strong DEGs; joint recovers additional moderate ones
n_per_plat <- as.integer(get_arg("--n-per-plat", "30"))
beta_cond <- as.numeric(get_arg("--beta-cond", "1.5"))  # strong arm mean
beta_cond_weak <- as.numeric(get_arg("--beta-cond-weak", "0.9"))
frac_strong <- as.numeric(get_arg("--frac-strong", "0.4"))
beta_plat <- as.numeric(get_arg("--beta-plat", "3.0"))
sigma <- as.numeric(get_arg("--sigma", "0.85"))

suppressPackageStartupMessages({
  if (!requireNamespace("limma", quietly = TRUE)) {
    stop("Package limma is required")
  }
  library(limma)
})

## ---- Simulator ----
## expr: genes x samples (log-scale Gaussian, platform-scaled)
simulate_two_platform <- function(scenario = c("balanced", "confounded"),
                                  n_genes = 2000L, n_true = 100L,
                                  n_per_plat = 30L,
                                  beta_cond = 1.5, beta_cond_weak = 0.9,
                                  frac_strong = 0.4,
                                  beta_plat = 3.0,
                                  sigma = 0.85, seed = 1L) {
  scenario <- match.arg(scenario)
  set.seed(seed)
  true_idx <- sort(sample.int(n_genes, n_true))
  true_sign <- sample(c(-1, 1), n_true, replace = TRUE)
  n_strong <- max(1L, as.integer(round(n_true * frac_strong)))
  mag <- c(rep(beta_cond, n_strong), rep(beta_cond_weak, n_true - n_strong))
  mag <- sample(mag)  # shuffle which true genes are strong
  beta_g <- numeric(n_genes)
  beta_g[true_idx] <- true_sign * mag

  half <- as.integer(n_per_plat / 2L)
  if (scenario == "balanced") {
    cond_A <- rep(c("Disease", "Normal"), each = half)
    cond_B <- rep(c("Disease", "Normal"), each = half)
  } else {
    n_maj <- as.integer(round(n_per_plat * 0.8))
    n_min <- n_per_plat - n_maj
    cond_A <- c(rep("Disease", n_maj), rep("Normal", n_min))
    cond_B <- c(rep("Disease", n_min), rep("Normal", n_maj))
  }
  cond_A <- cond_A[seq_len(n_per_plat)]
  cond_B <- cond_B[seq_len(n_per_plat)]
  n_A <- length(cond_A)
  n_B <- length(cond_B)
  cond <- c(cond_A, cond_B)
  plat <- c(rep("A", n_A), rep("B", n_B))
  disease <- as.numeric(cond == "Disease")
  platB <- as.numeric(plat == "B")

  mu <- matrix(0, n_genes, n_A + n_B)
  plat_shift <- rnorm(n_genes, mean = beta_plat, sd = 0.25)
  for (j in seq_len(n_A + n_B)) {
    mu[, j] <- beta_g * disease[j] + plat_shift * platB[j]
  }
  expr <- mu + matrix(rnorm(n_genes * (n_A + n_B), 0, sigma), n_genes, n_A + n_B)
  expr[, plat == "B"] <- expr[, plat == "B"] * 1.1

  rownames(expr) <- paste0("G", seq_len(n_genes))
  colnames(expr) <- paste0(plat, "_", seq_len(n_A + n_B))
  meta <- data.frame(
    Sample = colnames(expr),
    Condition = factor(cond, levels = c("Normal", "Disease")),
    Platform = factor(plat, levels = c("A", "B")),
    stringsAsFactors = FALSE
  )
  rownames(meta) <- meta$Sample
  list(
    expr = expr, meta = meta,
    true_genes = rownames(expr)[true_idx],
    scenario = scenario
  )
}

## Harmonize: z-score genes within platform (simple stand-in for cross-platform prep)
harmonize_by_platform <- function(expr, meta) {
  out <- expr
  for (p in levels(meta$Platform)) {
    cols <- rownames(meta)[meta$Platform == p]
    block <- out[, cols, drop = FALSE]
    out[, cols] <- t(scale(t(block)))
  }
  out[!is.finite(out)] <- 0
  out
}

run_limma <- function(expr, condition, design_extra = NULL, fdr = 0.05, lfc = 0.5) {
  condition <- factor(condition, levels = c("Normal", "Disease"))
  if (is.null(design_extra)) {
    design <- model.matrix(~ condition)
  } else {
    design <- model.matrix(~ condition + design_extra)
  }
  fit <- limma::lmFit(expr, design)
  fit <- limma::eBayes(fit)
  ## Condition effect = second coefficient when design is ~ condition [+ ...]
  tt <- limma::topTable(fit, coef = 2, number = Inf, sort.by = "none")
  tt$Gene <- rownames(tt)
  sig <- tt$Gene[!is.na(tt$adj.P.Val) & tt$adj.P.Val < fdr & abs(tt$logFC) > lfc]
  list(all = tt, sig = unique(sig))
}

metrics <- function(called, true_genes, n_genes) {
  called <- unique(as.character(called))
  true_genes <- unique(as.character(true_genes))
  tp <- length(intersect(called, true_genes))
  fp <- length(setdiff(called, true_genes))
  fn <- length(setdiff(true_genes, called))
  sens <- if (length(true_genes)) tp / length(true_genes) else NA_real_
  fdr <- if (length(called)) fp / length(called) else NA_real_
  prec <- if (length(called)) tp / length(called) else NA_real_
  data.frame(
    n_called = length(called),
    TP = tp, FP = fp, FN = fn,
    Sensitivity = sens,
    FDR = fdr,
    Precision = prec,
    stringsAsFactors = FALSE
  )
}

one_replicate <- function(scenario, seed, n_genes, n_true, fdr_cut, lfc_cut,
                          n_per_plat, beta_cond, beta_cond_weak, frac_strong,
                          beta_plat, sigma) {
  sim <- simulate_two_platform(
    scenario = scenario, n_genes = n_genes, n_true = n_true, seed = seed,
    n_per_plat = n_per_plat, beta_cond = beta_cond,
    beta_cond_weak = beta_cond_weak, frac_strong = frac_strong,
    beta_plat = beta_plat, sigma = sigma
  )
  expr_raw <- sim$expr
  expr_h <- harmonize_by_platform(sim$expr, sim$meta)
  meta <- sim$meta
  true <- sim$true_genes

  idx_A <- meta$Platform == "A"
  idx_B <- meta$Platform == "B"

  ## --- Harmonized analyses (primary M1 comparison) ---
  sep_A <- run_limma(expr_h[, idx_A, drop = FALSE], meta$Condition[idx_A],
                     fdr = fdr_cut, lfc = lfc_cut)
  sep_B <- run_limma(expr_h[, idx_B, drop = FALSE], meta$Condition[idx_B],
                     fdr = fdr_cut, lfc = lfc_cut)
  inter <- intersect(sep_A$sig, sep_B$sig)
  union_sep <- union(sep_A$sig, sep_B$sig)
  joint_cond_h <- run_limma(expr_h, meta$Condition, fdr = fdr_cut, lfc = lfc_cut)
  joint_cp_h <- run_limma(
    expr_h, meta$Condition, design_extra = meta$Platform,
    fdr = fdr_cut, lfc = lfc_cut
  )

  ## --- Raw (no z-score): shows Platform covariate needed under confounding ---
  joint_cond_raw <- run_limma(expr_raw, meta$Condition, fdr = fdr_cut, lfc = lfc_cut)
  joint_cp_raw <- run_limma(
    expr_raw, meta$Condition, design_extra = meta$Platform,
    fdr = fdr_cut, lfc = lfc_cut
  )

  only_joint <- setdiff(joint_cp_h$sig, inter)
  only_inter <- setdiff(inter, joint_cp_h$sig)
  jaccard <- if (length(union(inter, joint_cp_h$sig))) {
    length(intersect(inter, joint_cp_h$sig)) / length(union(inter, joint_cp_h$sig))
  } else {
    NA_real_
  }

  methods <- c(
    "Separate_intersect",
    "Separate_union",
    "Joint_Condition_only_harmonized",
    "Joint_Condition_Platform_harmonized",
    "Joint_Condition_only_raw",
    "Joint_Condition_Platform_raw"
  )
  called <- list(inter, union_sep, joint_cond_h$sig, joint_cp_h$sig,
                 joint_cond_raw$sig, joint_cp_raw$sig)
  mets <- lapply(called, function(g) metrics(g, true, n_genes))
  tab <- cbind(
    data.frame(Scenario = scenario, Seed = seed, Method = methods, stringsAsFactors = FALSE),
    do.call(rbind, mets)
  )
  tab$n_sep_A <- NA_real_
  tab$n_sep_B <- NA_real_
  tab$n_sep_A[1] <- length(sep_A$sig)
  tab$n_sep_B[1] <- length(sep_B$sig)
  tab$Jaccard_vs_intersect <- NA_real_
  tab$Jaccard_vs_intersect[1] <- 1
  tab$Jaccard_vs_intersect[4] <- jaccard
  tab$n_only_in_joint_vs_intersect <- NA_real_
  tab$TP_only_in_joint_vs_intersect <- NA_real_
  tab$n_only_in_intersect_vs_joint <- NA_real_
  tab$TP_only_in_intersect_vs_joint <- NA_real_
  tab$n_only_in_joint_vs_intersect[4] <- length(only_joint)
  tab$TP_only_in_joint_vs_intersect[4] <- length(intersect(only_joint, true))
  tab$n_only_in_intersect_vs_joint[4] <- length(only_inter)
  tab$TP_only_in_intersect_vs_joint[4] <- length(intersect(only_inter, true))
  tab
}

summarize_reps <- function(df) {
  num_cols <- c(
    "n_called", "TP", "FP", "FN", "Sensitivity", "FDR", "Precision",
    "n_sep_A", "n_sep_B",
    "Jaccard_vs_intersect", "n_only_in_joint_vs_intersect", "TP_only_in_joint_vs_intersect",
    "n_only_in_intersect_vs_joint", "TP_only_in_intersect_vs_joint"
  )
  keys <- unique(df[, c("Scenario", "Method")])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    sub <- df[df$Scenario == keys$Scenario[i] & df$Method == keys$Method[i], , drop = FALSE]
    out <- data.frame(
      Scenario = keys$Scenario[i],
      Method = keys$Method[i],
      n_rep = nrow(sub),
      stringsAsFactors = FALSE
    )
    for (col in num_cols) {
      v <- suppressWarnings(as.numeric(sub[[col]]))
      out[[paste0(col, "_mean")]] <- mean(v, na.rm = TRUE)
      out[[paste0(col, "_sd")]] <- stats::sd(v, na.rm = TRUE)
    }
    out
  })
  do.call(rbind, rows)
}

cat("=== Simulation: joint limma vs separate∩ ===\n")
cat("n_rep=", n_rep, " n_genes=", n_genes, " n_true=", n_true,
    " n_per_plat=", n_per_plat,
    " beta_strong=", beta_cond, " beta_weak=", beta_cond_weak,
    " frac_strong=", frac_strong, " sigma=", sigma,
    " FDR<", fdr_cut, " |logFC|>", lfc_cut, "\n", sep = "")

all_rows <- list()
k <- 1L
for (sc in c("balanced", "confounded")) {
  for (r in seq_len(n_rep)) {
    seed <- seed0 + r + if (sc == "confounded") 10000L else 0L
    all_rows[[k]] <- one_replicate(
      sc, seed, n_genes, n_true, fdr_cut, lfc_cut,
      n_per_plat, beta_cond, beta_cond_weak, frac_strong, beta_plat, sigma
    )
    k <- k + 1L
    if (r %% 10L == 0L) cat("  ", sc, " rep ", r, "/", n_rep, "\n", sep = "")
  }
}
raw <- do.call(rbind, all_rows)
summary <- summarize_reps(raw)

raw_csv <- file.path(results_dir, "simulation_joint_vs_intersect_raw.csv")
sum_csv <- file.path(results_dir, "simulation_joint_vs_intersect_summary.csv")
utils::write.csv(raw, raw_csv, row.names = FALSE)
utils::write.csv(summary, sum_csv, row.names = FALSE)

fmt <- function(m, s, d = 3) {
  if (is.na(m)) return("—")
  if (is.na(s)) return(format(round(m, d), nsmall = d))
  paste0(format(round(m, d), nsmall = d), " ± ", format(round(s, d), nsmall = d))
}

md <- c(
  "# Table 4 (draft). Simulation — joint limma vs separate DEG ∩",
  "",
  paste0(
    "Replicates: ", n_rep, ". Genes: ", n_genes, " (true DEGs: ", n_true, "). ",
    "Samples/platform: ", n_per_plat, ". ",
    "Effect mix: ", round(100 * frac_strong), "% strong beta=", beta_cond,
    ", rest weak beta=", beta_cond_weak, ", sigma=", sigma, ". ",
    "Call: adj.P < ", fdr_cut, " and |logFC| > ", lfc_cut, ". ",
    "Seed base = ", seed0, "."
  ),
  "",
  "**Methods:** Separate_intersect = limma per platform → ∩; ",
  "Joint_*_harmonized = after within-platform z-score; ",
  "Joint_*_raw = no z-score (batch remains). ",
  "M1 = `~ Condition + Platform`.",
  "",
  "| Scenario | Method | Sensitivity | Empirical FDR | Precision | n called | Jaccard(∩, joint) |",
  "|----------|--------|-------------|-----------------|-----------|----------|-------------------|"
)

for (i in seq_len(nrow(summary))) {
  md <- c(md, paste0(
    "| ", summary$Scenario[i], " | ", summary$Method[i], " | ",
    fmt(summary$Sensitivity_mean[i], summary$Sensitivity_sd[i]), " | ",
    fmt(summary$FDR_mean[i], summary$FDR_sd[i]), " | ",
    fmt(summary$Precision_mean[i], summary$Precision_sd[i]), " | ",
    fmt(summary$n_called_mean[i], summary$n_called_sd[i], 1), " | ",
    fmt(summary$Jaccard_vs_intersect_mean[i], summary$Jaccard_vs_intersect_sd[i]), " |"
  ))
}

## Key contrast rows for prose
pick <- function(sc, method, col) {
  summary[[col]][summary$Scenario == sc & summary$Method == method][1]
}

md <- c(
  md,
  "",
  "## Key contrasts (mean)",
  "",
  paste0(
    "- **Balanced (harmonized):** joint(+Platform) sensitivity ",
    fmt(pick("balanced", "Joint_Condition_Platform_harmonized", "Sensitivity_mean"), NA),
    " vs ∩ ",
    fmt(pick("balanced", "Separate_intersect", "Sensitivity_mean"), NA),
    "; FDR ",
    fmt(pick("balanced", "Joint_Condition_Platform_harmonized", "FDR_mean"), NA),
    " vs ∩ ",
    fmt(pick("balanced", "Separate_intersect", "FDR_mean"), NA),
    "; Jaccard ",
    fmt(pick("balanced", "Joint_Condition_Platform_harmonized", "Jaccard_vs_intersect_mean"), NA),
    "."
  ),
  paste0(
    "- **Confounded (harmonized):** joint recovers TP missed by ∩: mean TP_only_joint=",
    fmt(pick("confounded", "Joint_Condition_Platform_harmonized", "TP_only_in_joint_vs_intersect_mean"), NA, 1),
    "."
  ),
  paste0(
    "- **Confounded (raw, no z-score):** Condition-only FDR ",
    fmt(pick("confounded", "Joint_Condition_only_raw", "FDR_mean"), NA),
    " vs joint(+Platform) FDR ",
    fmt(pick("confounded", "Joint_Condition_Platform_raw", "FDR_mean"), NA),
    " (Platform covariate required when batch remains)."
  ),
  paste0(
    "- Genes only in joint vs ∩ (balanced, harmonized): mean n=",
    fmt(pick("balanced", "Joint_Condition_Platform_harmonized", "n_only_in_joint_vs_intersect_mean"), NA, 1),
    " of which TP=",
    fmt(pick("balanced", "Joint_Condition_Platform_harmonized", "TP_only_in_joint_vs_intersect_mean"), NA, 1),
    "."
  ),
  "",
  "Accept C4-simulation if: (i) joint(+Platform) sensitivity ≥ ∩ with controlled FDR under harmonized data;",
  "(ii) lists are not identical (Jaccard < 1); (iii) on raw confounded data, Condition-only inflates FDR vs Platform-adjusted joint.",
  "",
  paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE))
)

md_path <- file.path(results_dir, "simulation_joint_vs_intersect.md")
writeLines(md, md_path)

## Update Original draft C4 section if present
draft <- file.path(outdir, "manuscript", "GExPipe_original_paper_draft.md")
if (file.exists(draft)) {
  draft_txt <- readLines(draft, warn = FALSE)
  marker <- "### 3. Generalization (C4)"
  end_marker <- "## Methods (evaluation protocol)"
  i0 <- grep(marker, draft_txt, fixed = TRUE)[1]
  i1 <- grep(end_marker, draft_txt, fixed = TRUE)[1]
  if (!is.na(i0) && !is.na(i1) && i1 > i0) {
    new_sec <- c(
      "### 3. Generalization (C4) — simulation (chosen)",
      "",
      paste0("Source: `results/simulation_joint_vs_intersect.md` (", format(Sys.Date()), ")."),
      "Chose **simulation with known truth** over a 2nd GEO external.",
      "",
      "#### Table 4 (draft)",
      "",
      md[grep("^\\|", md)],
      "",
      paste0(
        "- Balanced harmonized: joint sens=",
        fmt(pick("balanced", "Joint_Condition_Platform_harmonized", "Sensitivity_mean"), NA),
        " vs ∩ sens=",
        fmt(pick("balanced", "Separate_intersect", "Sensitivity_mean"), NA),
        "; Jaccard=",
        fmt(pick("balanced", "Joint_Condition_Platform_harmonized", "Jaccard_vs_intersect_mean"), NA),
        "."
      ),
      paste0(
        "- Confounded raw: Condition-only FDR=",
        fmt(pick("confounded", "Joint_Condition_only_raw", "FDR_mean"), NA),
        " vs joint(+Platform) FDR=",
        fmt(pick("confounded", "Joint_Condition_Platform_raw", "FDR_mean"), NA),
        "."
      ),
      ""
    )
    draft_txt <- c(draft_txt[seq_len(i0 - 1L)], new_sec, draft_txt[i1:length(draft_txt)])
    writeLines(draft_txt, draft)
    cat("Updated ", draft, "\n", sep = "")
  }
}

cat("\nWrote:\n  ", raw_csv, "\n  ", sum_csv, "\n  ", md_path, "\n", sep = "")
