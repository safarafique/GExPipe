#!/usr/bin/env Rscript
## Original Paper — ablation B0–B6 + matched-k + true B4 + nested CV
## Writes ONLY under GExPipe(original_paper)/; reads Application Note validation_manual/.
##
## Usage (WSL):
##   cd "/mnt/e/GExPipe/GExPipe(original_paper)"
##   "/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
##     scripts/ablation-consensus-panels.R --gexpipe-repo "E:/GExPipe"
##
## Signatures:
##   B0     limma top-50
##   B1     limma-voom top-50
##   B0k    limma top-k matched to |B5|   (fair size baseline)
##   B1k    limma-voom top-k matched to |B5|
##   B2     all limma DEGs (size only if huge)
##   B3     DE ∩ trait-WGCNA (capped for panel)
##   B4     TRUE: ≥2 ML on DE universe (no WGCNA)
##   B4leg  legacy export ML (was run on DEG∩WGCNA; often ≡ B5)
##   B5     full consensus DE ∩ WGCNA ∩ (≥2 ML)
##   B6     random genes of size |B5|
##
## Primary endpoint (small external n): RF panel AUC (+ nested CV on train).
## Secondary: glmnet panel AUC (can saturate at n≈20).

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
  orig_root <- normalize_repo_path(getwd())
}

gexpipe_repo <- normalize_repo_path(get_arg("--gexpipe-repo", dirname(orig_root)))
seed <- as.integer(get_arg("--seed", "42"))
n_boot <- as.integer(get_arg("--n-boot", "500"))
max_b3_for_panel <- as.integer(get_arg("--max-b3", "200"))
max_b4_input <- as.integer(get_arg("--max-b4-input", "200"))

vm <- file.path(gexpipe_repo, "validation_manual")
sig_dir <- file.path(orig_root, "signatures")
results_dir <- file.path(orig_root, "results")
dir.create(sig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
if (!dir.exists(vm)) stop("validation_manual not found at ", vm)

ORIGINAL_PAPER_HELPERS_ONLY <- TRUE
source(file.path(orig_root, "scripts", "panel-external-auc.R"), local = FALSE)
`%||%` <- function(a, b) if (!is.null(a)) a else b

write_sig <- function(genes, path) {
  genes <- unique(as.character(genes))
  genes <- genes[nzchar(genes)]
  utils::write.csv(data.frame(Gene = genes, stringsAsFactors = FALSE), path, row.names = FALSE)
  invisible(genes)
}

empty_row_template <- function() {
  data.frame(
    Signature = character(),
    Panel_size_requested = integer(),
    Panel_size_used = integer(),
    Median_gene_AUC_train = numeric(),
    Median_gene_AUC_external = numeric(),
    Panel_AUC_RF_external = numeric(),
    Panel_AUC_RF_CI95_lo = numeric(),
    Panel_AUC_RF_CI95_hi = numeric(),
    NestedCV_AUC_RF_train = numeric(),
    Panel_AUC_glmnet_external = numeric(),
    Panel_AUPRC_glmnet_external = numeric(),
    Panel_AUC_glmnet_CI95_lo = numeric(),
    Panel_AUC_glmnet_CI95_hi = numeric(),
    NestedCV_AUC_glmnet_train = numeric(),
    stringsAsFactors = FALSE
  )
}

## ---- Inputs (read-only from Application Note) ----
limma_all <- file.path(vm, "GSE50760_limma_DE_all.csv")
voom_all <- file.path(vm, "GSE50760_limma_voom_DE_all.csv")
limma_sig <- file.path(vm, "GSE50760_limma_DE_sig.csv")
deg_wgcna <- file.path(vm, "common_genes_DEG_WGCNA.csv")
ml_final <- file.path(vm, "final_list_common_genes_ML.csv")
consensus <- file.path(vm, "consensus_signature_genes.csv")
stopifnot(file.exists(limma_all), file.exists(voom_all))

B0 <- top_n_by_adjp(limma_all, 50L)
B1 <- top_n_by_adjp(voom_all, 50L)

if (file.exists(limma_sig)) {
  B2 <- read_gene_list(limma_sig)
} else {
  df <- utils::read.csv(limma_all, stringsAsFactors = FALSE)
  B2 <- unique(df$Gene[df$adj.P.Val < 0.05 & abs(df$logFC) > 1])
}

B3 <- read_gene_list(deg_wgcna)
if (!length(B3)) stop("Missing or empty ", deg_wgcna)

B5 <- read_gene_list(consensus)
if (!length(B5)) B5 <- read_gene_list(ml_final)
if (!length(B5) && length(B3) && file.exists(ml_final)) {
  B5 <- intersect(B3, read_gene_list(ml_final))
}
if (!length(B5)) stop("Could not resolve consensus genes (B5)")

k_match <- length(B5)
B0k <- top_n_by_adjp(limma_all, k_match)
B1k <- top_n_by_adjp(voom_all, k_match)

## Legacy B4 (export order: ML after WGCNA)
B4leg <- read_gene_list(ml_final)
if (!length(B4leg)) B4leg <- B5

B3_panel <- B3
if (length(B3_panel) > max_b3_for_panel) {
  df <- utils::read.csv(limma_all, stringsAsFactors = FALSE)
  df <- df[df$Gene %in% B3_panel, , drop = FALSE]
  df <- df[order(-abs(df$logFC)), , drop = FALSE]
  B3_panel <- head(unique(df$Gene), max_b3_for_panel)
  cat("B3 capped for panel AUC:", length(B3), "->", length(B3_panel),
      " (--max-b3=", max_b3_for_panel, ")\n", sep = "")
}

cat("Loading expression matrices...\n")
data <- load_train_val(vm)

## True B4: ML ≥2 on DE genes (no WGCNA)
## Prefer limma sig DEGs present in train matrix; cap by variance inside select_ml_ge2
de_for_ml <- intersect(B2, colnames(data$dat_tr))
if (length(de_for_ml) < 20L) {
  ## fall back to top-N by adj.P from limma_all
  de_for_ml <- intersect(top_n_by_adjp(limma_all, 500L), colnames(data$dat_tr))
}
cat("True B4: running ML ≥2 on DE universe (n_cand=", length(de_for_ml),
    ", max_input=", max_b4_input, ")...\n", sep = "")
b4_fit <- select_ml_ge2(
  data$dat_tr, data$y_tr, de_for_ml,
  seed = seed, max_input = max_b4_input
)
B4 <- b4_fit$genes
cat("  B4 genes:", length(B4), " (ML input capped to ", b4_fit$n_input, ")\n", sep = "")
cat("  LASSO:", length(b4_fit$per_method$LASSO %||% character()),
    " RF:", length(b4_fit$per_method$RF %||% character()),
    " SVM:", length(b4_fit$per_method$SVM %||% character()), "\n", sep = "")

note <- c(
  "B4 definition (updated)",
  "TRUE B4 = genes selected by ≥2 of LASSO / RF / SVM from the DE gene universe (NO WGCNA filter).",
  paste0("DE candidate pool size = ", length(de_for_ml), "; ML input after variance cap = ", b4_fit$n_input),
  paste0("length(B4 true)=", length(B4)),
  paste0("length(B4leg legacy export)=", length(B4leg)),
  paste0("length(B5 consensus)=", length(B5)),
  paste0("identical(B4,B5)=", identical(sort(B4), sort(B5))),
  paste0("identical(B4leg,B5)=", identical(sort(B4leg), sort(B5))),
  paste0("Jaccard(B4,B5)=", round(length(intersect(B4, B5)) / max(length(union(B4, B5)), 1L), 3)),
  paste0("Jaccard(B4,B4leg)=", round(length(intersect(B4, B4leg)) / max(length(union(B4, B4leg)), 1L), 3))
)
writeLines(note, file.path(sig_dir, "B4_note.txt"))
utils::write.csv(
  data.frame(
    Method = c("LASSO", "RF", "SVM"),
    N = c(
      length(b4_fit$per_method$LASSO %||% character()),
      length(b4_fit$per_method$RF %||% character()),
      length(b4_fit$per_method$SVM %||% character())
    )
  ),
  file.path(sig_dir, "B4_ml_method_counts.csv"),
  row.names = FALSE
)

## B6 random control matched to |B5|
universe <- setdiff(intersect(colnames(data$dat_tr), colnames(data$dat_va)), B5)
set.seed(seed)
if (length(universe) < length(B5)) {
  universe <- intersect(colnames(data$dat_tr), colnames(data$dat_va))
}
B6 <- sample(universe, size = min(length(B5), length(universe)))

write_sig(B0, file.path(sig_dir, "B0_limma_top50.csv"))
write_sig(B1, file.path(sig_dir, "B1_limma_voom_top50.csv"))
write_sig(B0k, file.path(sig_dir, "B0k_limma_top_matched_k.csv"))
write_sig(B1k, file.path(sig_dir, "B1k_limma_voom_top_matched_k.csv"))
write_sig(B2, file.path(sig_dir, "B2_all_DEGs_limma_sig.csv"))
write_sig(B3, file.path(sig_dir, "B3_DE_intersect_WGCNA.csv"))
write_sig(B3_panel, file.path(sig_dir, "B3_panel_capped.csv"))
write_sig(B4, file.path(sig_dir, "B4_DE_intersect_ML_true.csv"))
write_sig(B4leg, file.path(sig_dir, "B4leg_ML_after_WGCNA_export.csv"))
write_sig(B5, file.path(sig_dir, "B5_full_consensus.csv"))
write_sig(B6, file.path(sig_dir, "B6_random_matched_size.csv"))

sigs <- list(
  B0_limma_top50 = B0,
  B1_limma_voom_top50 = B1,
  B0k_limma_matched_k = B0k,
  B1k_limma_voom_matched_k = B1k,
  B3_DE_intersect_WGCNA = B3_panel,
  B4_DE_ML_true = B4,
  B4leg_ML_after_WGCNA = B4leg,
  B5_full_consensus = B5,
  B6_random_matched_size = B6
)
if (length(B2) <= 500L) {
  sigs <- c(list(B2_all_DEGs_limma_sig = B2), sigs)
} else {
  cat("Skipping panel AUC for B2 (size=", length(B2), "); size recorded in summary.\n", sep = "")
}

cat("Train n=", data$n_train, " Val n=", data$n_val, " matched k=", k_match, "\n", sep = "")
rows <- list()
for (nm in names(sigs)) {
  cat("Evaluating ", nm, " (k=", length(sigs[[nm]]), ")...\n", sep = "")
  rows[[nm]] <- evaluate_signature(
    nm, sigs[[nm]], data, seed = seed, n_boot = n_boot, do_nested = TRUE
  )
}
tab <- do.call(rbind, rows)
rownames(tab) <- NULL

## Size-only rows
size_extra <- data.frame(
  Signature = c("B2_all_DEGs_limma_sig_SIZE_ONLY", "B3_DE_intersect_WGCNA_FULL_SIZE"),
  Panel_size_requested = c(length(B2), length(B3)),
  Panel_size_used = c(NA_integer_, NA_integer_),
  Median_gene_AUC_train = c(NA_real_, NA_real_),
  Median_gene_AUC_external = c(NA_real_, NA_real_),
  Panel_AUC_RF_external = c(NA_real_, NA_real_),
  Panel_AUC_RF_CI95_lo = c(NA_real_, NA_real_),
  Panel_AUC_RF_CI95_hi = c(NA_real_, NA_real_),
  NestedCV_AUC_RF_train = c(NA_real_, NA_real_),
  Panel_AUC_glmnet_external = c(NA_real_, NA_real_),
  Panel_AUPRC_glmnet_external = c(NA_real_, NA_real_),
  Panel_AUC_glmnet_CI95_lo = c(NA_real_, NA_real_),
  Panel_AUC_glmnet_CI95_hi = c(NA_real_, NA_real_),
  NestedCV_AUC_glmnet_train = c(NA_real_, NA_real_),
  stringsAsFactors = FALSE
)
if (!"B2_all_DEGs_limma_sig" %in% tab$Signature) {
  tab <- rbind(tab, size_extra[1, , drop = FALSE])
}
tab <- rbind(tab, size_extra[2, , drop = FALSE])

out_csv <- file.path(results_dir, "ablation_panel_auc.csv")
utils::write.csv(tab, out_csv, row.names = FALSE)

med_csv <- file.path(results_dir, "ablation_gene_auc_median.csv")
utils::write.csv(
  tab[, c("Signature", "Panel_size_requested", "Panel_size_used",
          "Median_gene_AUC_train", "Median_gene_AUC_external",
          "Panel_AUC_RF_external", "NestedCV_AUC_RF_train")],
  med_csv, row.names = FALSE
)

fmt <- function(x, d = 3) {
  if (length(x) != 1L || is.na(x)) return("—")
  format(round(as.numeric(x), d), nsmall = d)
}

md <- c(
  "# Table 3 (draft). Ablation + matched-k — external panel AUC",
  "",
  paste0("Train: GSE50760 (n=", data$n_train, "). External: GSE104836 (n=", data$n_val, ")."),
  paste0("**Primary endpoint:** RF panel AUC (better behaved at small external n). Matched k = |B5| = ", k_match, "."),
  "Secondary: glmnet panel AUC (often saturates at n≈20). Nested CV = 5-fold on train only.",
  paste0("Bootstrap 95% CI: ", n_boot, " resamples on external RF scores. Seed = ", seed, "."),
  "",
  "| Signature | Definition | k | Med. gene AUC | **RF panel AUC (95% CI)** | NestedCV RF | glmnet panel |",
  "|-----------|------------|---|---------------|---------------------------|-------------|--------------|"
)

label <- c(
  B0_limma_top50 = "limma top-50",
  B1_limma_voom_top50 = "limma-voom top-50",
  B0k_limma_matched_k = paste0("limma top-", k_match, " (matched k)"),
  B1k_limma_voom_matched_k = paste0("limma-voom top-", k_match, " (matched k)"),
  B2_all_DEGs_limma_sig = "all limma DEGs",
  B3_DE_intersect_WGCNA = "DE ∩ trait-WGCNA (capped)",
  B4_DE_ML_true = "DE ∩ (≥2 ML), no WGCNA",
  B4leg_ML_after_WGCNA = "legacy ML after WGCNA",
  B5_full_consensus = "DE ∩ WGCNA ∩ (≥2 ML)",
  B6_random_matched_size = "random genes (|B5|)"
)

for (i in seq_len(nrow(tab))) {
  s <- tab$Signature[i]
  if (grepl("SIZE_ONLY|FULL_SIZE", s)) next
  def <- if (s %in% names(label)) label[[s]] else s
  ci <- paste0(fmt(tab$Panel_AUC_RF_external[i]), " (",
               fmt(tab$Panel_AUC_RF_CI95_lo[i]), "–",
               fmt(tab$Panel_AUC_RF_CI95_hi[i]), ")")
  md <- c(md, paste0(
    "| ", s, " | ", def, " | ",
    tab$Panel_size_used[i], " | ",
    fmt(tab$Median_gene_AUC_external[i]), " | ",
    ci, " | ",
    fmt(tab$NestedCV_AUC_RF_train[i]), " | ",
    fmt(tab$Panel_AUC_glmnet_external[i]), " |"
  ))
}

md <- c(
  md,
  "",
  paste0("True B4 size = ", length(B4), "; Jaccard(B4,B5) = ",
         round(length(intersect(B4, B5)) / max(length(union(B4, B5)), 1L), 3),
         ". Legacy B4≡B5: ", identical(sort(B4leg), sort(B5)), "."),
  paste0("Full B2 size = ", length(B2), "; full B3 size = ", length(B3), "."),
  "See `signatures/B4_note.txt`.",
  "",
  paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE))
)

out_md <- file.path(results_dir, "ablation_panel_auc.md")
writeLines(md, out_md)

## Refresh draft Results section numbers from this run
draft <- file.path(orig_root, "manuscript", "GExPipe_original_paper_draft.md")
if (file.exists(draft)) {
  draft_txt <- readLines(draft, warn = FALSE)
  marker <- "### 2. Ablation + panel AUC (C2–C3) — main table"
  end_marker <- "### 3. Generalization (C4)"
  i0 <- grep(marker, draft_txt, fixed = TRUE)[1]
  i1 <- grep(end_marker, draft_txt, fixed = TRUE)[1]
  if (!is.na(i0) && !is.na(i1) && i1 > i0) {
    new_sec <- c(
      marker,
      "",
      paste0("Source: `results/ablation_panel_auc.md` (", format(Sys.Date()), ")."),
      paste0("Train GSE50760 *n*=", data$n_train, "; external GSE104836 *n*=", data$n_val, "."),
      paste0("**Primary:** RF panel AUC. Matched *k* = ", k_match, ". Nested CV on train only."),
      "",
      "#### Table 3 (draft)",
      "",
      md[grep("^\\|", md)],
      "",
      paste0("- True B4 (ML on DE, no WGCNA): *k*=", length(B4),
             "; Jaccard with B5 = ",
             round(length(intersect(B4, B5)) / max(length(union(B4, B5)), 1L), 3), "."),
      paste0("- Legacy export B4≡B5: ", identical(sort(B4leg), sort(B5)), "."),
      "- glmnet external panel AUC often saturates at *n*=20 (including random); prefer RF + nested CV + matched-*k*.",
      "- C4 (2nd cohort / simulation) still pending.",
      ""
    )
    draft_txt <- c(draft_txt[seq_len(i0 - 1L)], new_sec, draft_txt[i1:length(draft_txt)])
    writeLines(draft_txt, draft)
    cat("Updated ", draft, "\n", sep = "")
  }
}

cat("\nWrote:\n  ", out_csv, "\n  ", med_csv, "\n  ", out_md, "\n", sep = "")
cat("Signatures in ", sig_dir, "\n", sep = "")
