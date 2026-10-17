# ==============================================================================
# check-gexpipe-run.R  -  audit one GExPipe "Parallel DE, then merge" run
#
# Point it at the folder where you saved the app's downloads. It finds every
# output file, then checks that the files come from the same run and agree with
# each other, recomputing what can be recomputed from the exported data.
# Nothing is guessed: a check that cannot run says SKIP and why.
#
# Usage (in R):
#   source(system.file("scripts", "check-gexpipe-run.R", package = "GExPipe"))
#   gexpipe_check_run("D:/my_run_folder")
#   # optional, to recompute training AUCs: a CSV with columns Sample, Condition
#   gexpipe_check_run("D:/my_run_folder", labels = "D:/my_run_folder/labels.csv")
#
# Usage (command line):
#   Rscript check-gexpipe-run.R D:/my_run_folder [labels.csv]
#
# Writes GExPipe_Run_Check.txt and GExPipe_Run_Check.csv into the folder (or out_dir).
# Base R only (packages jpeg/png are used for the blank-figure check if present).
# ==============================================================================

gexpipe_check_run <- function(dir, labels = NULL, stale_minutes = 30, out_dir = dir,
                              session_gap_hours = 1) {
  stopifnot(dir.exists(dir))
  res <- data.frame(Area = character(), Check = character(), Status = character(),
                    Detail = character(), stringsAsFactors = FALSE)
  add <- function(area, check, status, detail = "") {
    res[nrow(res) + 1L, ] <<- list(area, check, status, paste(detail, collapse = " "))
    invisible(status)
  }
  fp  <- function(f) file.path(dir, f)
  rd  <- function(f) utils::read.csv(fp(f), check.names = FALSE, stringsAsFactors = FALSE)
  latest <- function(pattern) {
    f <- list.files(dir, pattern = pattern, ignore.case = TRUE)
    if (!length(f)) return(NA_character_)
    f[which.max(file.mtime(fp(f)))]
  }
  have <- function(f) !is.na(f) && file.exists(fp(f))
  mt   <- function(f) if (have(f)) file.mtime(fp(f)) else as.POSIXct(NA)
  gset <- function(x) sort(unique(stats::na.omit(as.character(x))))
  show <- function(x, n = 12) {
    x <- as.character(x)
    if (!length(x)) return("{}")
    paste0("{", paste(utils::head(x, n), collapse = ", "), if (length(x) > n) ", ..." else "", "}")
  }
  same_set <- function(area, check, a, b, la, lb) {
    a <- gset(a); b <- gset(b)
    if (setequal(a, b)) add(area, check, "PASS", sprintf("%s = %s (%d genes)", la, lb, length(a)))
    else add(area, check, "FAIL", sprintf("%s has %d, %s has %d. Only in %s: %s. Only in %s: %s",
                                           la, length(a), lb, length(b), la, show(setdiff(a, b)),
                                           lb, show(setdiff(b, a))))
  }
  subset_of <- function(area, check, a, b, la, lb) {
    a <- gset(a); b <- gset(b); extra <- setdiff(a, b)
    if (!length(extra)) add(area, check, "PASS", sprintf("all %d %s are in %s", length(a), la, lb))
    else add(area, check, "FAIL", sprintf("%d of %d %s are NOT in %s: %s",
                                           length(extra), length(a), la, lb, show(extra)))
  }
  auc <- function(score, y) {          # Mann-Whitney AUC, y = 1 disease / 0 control
    ok <- is.finite(score) & !is.na(y); score <- score[ok]; y <- y[ok]
    n1 <- sum(y == 1); n0 <- sum(y == 0)
    if (!n1 || !n0) return(NA_real_)
    r <- rank(score); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  }

  # ---------------------------------------------------------------------------
  # 0. Inventory
  # ---------------------------------------------------------------------------
  F <- list(
    de_ma    = latest("^DE_Results_Microarray.*\\.csv$"),
    de_rna   = latest("^DE_Results_RNAseq.*\\.csv$"),
    sig_ma   = latest("^Significant_Genes_Microarray.*\\.csv$"),
    sig_rna  = latest("^Significant_Genes_RNAseq.*\\.csv$"),
    only_ma  = latest("^DEGs_Microarray_only\\.csv$"),
    only_rna = latest("^DEGs_RNA_only\\.csv$"),
    cons     = latest("^Consensus_DEGs_RNAseq_Microarray\\.csv$"),
    wgcna    = latest("^common_genes_ConsensusDE_WGCNA\\.csv$"),
    ppi      = latest("^PPI_Interactive_NonInteractive_Genes\\.csv$"),
    hubs     = latest("^Interactive_Genes_List\\.csv$"),
    hubs2    = latest("^Consensus_Hub_Genes\\.csv$"),
    ml_in    = latest("^extracted_common_genes_data\\.csv$"),
    ppi_data = latest("^extracted_interacting_genes_data\\.csv$"),
    final    = latest("^final_list_common_genes_ML\\.csv$"),
    combined = latest("^combined_ML_gene_list\\.csv$"),
    roc      = latest("^ROC_AUC_scores\\.csv$"),
    roc_pan  = latest("^ROC_Combined_Panel_Table\\.csv$"),
    roc_tv   = latest("^ROC_AUC_Training_vs_Validation\\.csv$"),
    val_de   = latest("^Validation_DE_Results\\.csv$"),
    val_ov   = latest("^Validation_Overlap_ML_DE\\.csv$"),
    nom_coef = latest("^Nomogram_Model_Coefficients\\.csv$"),
    nom_tr   = latest("^Nomogram_Performance_Training\\.csv$"),
    nom_va   = latest("^Nomogram_Performance_Validation\\.csv$"),
    nom_cm   = latest("^Nomogram_Confusion_Matrices\\.csv$"),
    venn_ml  = latest("^venn_diagram_ML_methods\\.(jpg|png|pdf)$")
  )
  rank_files <- c(LASSO = "lasso_coefficients_ranking.csv", Elastic_Net = "elastic_net_ranking.csv",
                  Ridge = "ridge_ranking.csv", Random_Forest = "random_forest_importance_ranking.csv",
                  SVM_RFE = "svm_rfe_ranking.csv", Boruta = "boruta_importance.csv",
                  XGBoost_SHAP = "xgboost_shap_ranking.csv", sPLSDA = "splsda_vip_ranking.csv")
  rank_files <- rank_files[file.exists(fp(rank_files))]
  file_label <- c(
    de_ma = "DE_Results_Microarray_*.csv", de_rna = "DE_Results_RNAseq_*.csv",
    sig_ma = "Significant_Genes_Microarray_*.csv", sig_rna = "Significant_Genes_RNAseq_*.csv",
    only_ma = "DEGs_Microarray_only.csv", only_rna = "DEGs_RNA_only.csv",
    cons = "Consensus_DEGs_RNAseq_Microarray.csv", wgcna = "common_genes_ConsensusDE_WGCNA.csv",
    ppi = "PPI_Interactive_NonInteractive_Genes.csv", hubs = "Interactive_Genes_List.csv",
    hubs2 = "Consensus_Hub_Genes.csv", ml_in = "extracted_common_genes_data.csv",
    ppi_data = "extracted_interacting_genes_data.csv", final = "final_list_common_genes_ML.csv",
    combined = "combined_ML_gene_list.csv", roc = "ROC_AUC_scores.csv",
    roc_pan = "ROC_Combined_Panel_Table.csv", roc_tv = "ROC_AUC_Training_vs_Validation.csv",
    val_de = "Validation_DE_Results.csv", val_ov = "Validation_Overlap_ML_DE.csv",
    nom_coef = "Nomogram_Model_Coefficients.csv", nom_tr = "Nomogram_Performance_Training.csv",
    nom_va = "Nomogram_Performance_Validation.csv", nom_cm = "Nomogram_Confusion_Matrices.csv",
    venn_ml = "venn_diagram_ML_methods.jpg")
  found <- vapply(F, have, logical(1))
  core  <- c("de_ma", "de_rna", "cons", "final")
  add("0 Inventory", "Output files found",
      if (all(found)) "PASS" else if (all(found[core])) "WARN" else "FAIL",
      sprintf("%d of %d expected files present%s", sum(found), length(F),
              if (all(found)) "" else paste0("; missing: ", paste(file_label[names(F)[!found]], collapse = ", "))))
  if (!any(found)) {
    lines <- c(sprintf("GExPipe run check - %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
               sprintf("Folder: %s", normalizePath(dir, winslash = "/")), "",
               "No GExPipe result files were found in this folder, so nothing was checked.",
               "Run the analysis in the app and use each step's Download buttons to save the",
               "CSV files and figures into this folder, then run gexpipe_check_run() again.", "",
               "VERDICT: NOT CHECKED - folder contains no GExPipe output files")
    writeLines(lines, file.path(out_dir, "GExPipe_Run_Check.txt"), useBytes = TRUE)
    cat(lines, sep = "\n")
    return(invisible(res))
  }

  # ---------------------------------------------------------------------------
  # 1. Same run? A step's file must not be OLDER than the files it was built from
  # ---------------------------------------------------------------------------
  stages <- list(
    "DE"           = c(F$de_ma, F$de_rna, F$sig_ma, F$sig_rna),
    "Consensus"    = F$cons,
    "WGCNA overlap" = F$wgcna,
    "PPI / hubs"   = c(F$ppi, F$hubs, F$hubs2, F$ppi_data),
    "ML"           = c(F$ml_in, F$final, F$combined, rank_files, F$venn_ml),
    "ROC"          = c(F$roc, F$roc_pan),
    "Validation"   = c(F$roc_tv, F$val_ov),
    "Nomogram"     = c(F$nom_coef, F$nom_tr, F$nom_va, F$nom_cm)
  )
  stale_any <- FALSE
  newest_before <- as.POSIXct(NA)
  for (s in names(stages)) {
    fs <- stats::na.omit(unname(stages[[s]])); fs <- fs[file.exists(fp(fs))]
    if (!length(fs)) next
    times <- file.mtime(fp(fs))
    if (!is.na(newest_before)) {
      old <- fs[times < newest_before - stale_minutes * 60]
      if (length(old)) {
        stale_any <- TRUE
        add("1 Same run", paste("Stale files at step:", s), "FAIL",
            sprintf("written BEFORE their inputs (newest input %s), so they come from an earlier run: %s",
                    format(newest_before, "%Y-%m-%d %H:%M"),
                    paste(sprintf("%s (%s)", old, format(file.mtime(fp(old)), "%Y-%m-%d %H:%M")),
                          collapse = "; ")))
      }
    }
    newest_before <- max(c(newest_before, times), na.rm = TRUE)
  }
  if (!stale_any) add("1 Same run", "Every step is newer than its inputs", "PASS",
                      "no file older than the step it depends on")
  # Run timeline: exports more than `session_gap_hours` apart are treated as separate sessions.
  key <- stats::na.omit(unlist(stages, use.names = FALSE)); key <- unique(key[file.exists(fp(key))])
  if (length(key)) {
    tt <- sort(file.mtime(fp(key))); names(tt) <- key[order(file.mtime(fp(key)))]
    sess <- cumsum(c(1, diff(as.numeric(tt)) > session_gap_hours * 3600))
    step_of <- function(f) names(stages)[vapply(stages, function(s) f %in% s, logical(1))][1]
    desc <- vapply(split(names(tt), sess), function(fs) sprintf("%s to %s: %s",
             format(min(file.mtime(fp(fs))), "%Y-%m-%d %H:%M"), format(max(file.mtime(fp(fs))), "%H:%M"),
             paste(unique(vapply(fs, step_of, character(1))), collapse = ", ")), character(1))
    add("1 Same run", "Export sessions found",
        if (length(desc) > 1) "WARN" else "PASS",
        if (length(desc) > 1) paste0(length(desc), " separate sessions (results from different sessions ",
                                     "may come from different runs): ", paste(desc, collapse = " | "))
        else desc)
  }
  # dated duplicates (several runs saved side by side)
  for (p in c("^DE_Results_Microarray", "^DE_Results_RNAseq", "^Expression_after_batch",
              "^Significant_Genes_RNAseq", "^Significant_Genes_Microarray")) {
    n <- length(list.files(dir, pattern = p))
    if (n > 1) add("1 Same run", paste("Several dated copies:", sub("^\\^", "", p)), "WARN",
                   sprintf("%d files; this check uses the newest one", n))
  }

  # ---------------------------------------------------------------------------
  # 2. Differential expression
  # ---------------------------------------------------------------------------
  sig_of <- function(de) de$Gene[!is.na(de$Significance) & de$Significance != "Not Significant"]
  de <- list()
  for (p in c("ma", "rna")) {
    lab <- if (p == "ma") "Microarray" else "RNA-seq"
    f_de <- F[[paste0("de_", p)]]; f_sig <- F[[paste0("sig_", p)]]
    if (!have(f_de)) { add("2 DE", paste(lab, "DE results"), "SKIP", "DE_Results file not found"); next }
    d <- rd(f_de); de[[p]] <- d
    s <- d[d$Gene %in% sig_of(d), ]
    up <- sum(s$Significance == "Up-regulated"); dn <- sum(s$Significance == "Down-regulated")
    add("2 DE", paste(lab, "DEGs"), "INFO", sprintf("%d significant (%d up, %d down) of %d genes",
                                                     nrow(s), up, dn, nrow(d)))
    if (nrow(s)) {
      bad_dir <- sum((s$Significance == "Up-regulated" & s$logFC <= 0) |
                     (s$Significance == "Down-regulated" & s$logFC >= 0), na.rm = TRUE)
      add("2 DE", paste(lab, "Up/Down label matches sign of logFC"),
          if (bad_dir) "FAIL" else "PASS", sprintf("%d mismatches", bad_dir))
      add("2 DE", paste(lab, "thresholds actually applied"), "INFO",
          sprintf("smallest |logFC| among DEGs = %.3f, largest adj.P among DEGs = %.3g",
                  min(abs(s$logFC), na.rm = TRUE), max(s$adj.P.Val, na.rm = TRUE)))
      ns <- d[!d$Gene %in% s$Gene, ]
      leak <- sum(abs(ns$logFC) > min(abs(s$logFC)) & ns$adj.P.Val < max(s$adj.P.Val), na.rm = TRUE)
      add("2 DE", paste(lab, "no non-significant gene passes the same cut-offs"),
          if (leak) "WARN" else "PASS", sprintf("%d genes labelled 'Not Significant' pass both cut-offs", leak))
    }
    if (have(f_sig)) same_set("2 DE", paste(lab, "Significant_Genes file = DEGs in DE_Results"),
                              rd(f_sig)$Gene, s$Gene, f_sig, f_de)
    f_only <- F[[paste0("only_", p)]]
    if (have(f_only) && have(f_sig) &&
        identical(unname(tools::md5sum(fp(f_only))), unname(tools::md5sum(fp(f_sig)))))
      add("2 DE", paste(f_only, "is platform-specific"), "WARN",
          sprintf("identical to %s: it holds ALL %s DEGs, not only those outside the consensus", f_sig, lab))
  }

  # ---------------------------------------------------------------------------
  # 3. Consensus (recomputed from the two DE tables)
  # ---------------------------------------------------------------------------
  cons <- if (have(F$cons)) rd(F$cons) else NULL
  if (!is.null(cons) && !is.null(de$ma) && !is.null(de$rna)) {
    a <- de$ma[de$ma$Gene %in% sig_of(de$ma), c("Gene", "logFC")]
    b <- de$rna[de$rna$Gene %in% sig_of(de$rna), c("Gene", "logFC")]
    m <- merge(a, b, by = "Gene", suffixes = c("_micro", "_rna"))
    recomputed <- m$Gene[sign(m$logFC_micro) == sign(m$logFC_rna)]
    add("3 Consensus", "Overlap of the two DEG lists", "INFO",
        sprintf("%d genes significant on both platforms, %d with the same direction",
                nrow(m), length(recomputed)))
    same_set("3 Consensus", "Consensus file = recomputed same-direction overlap",
             cons$Gene, recomputed, F$cons, "recomputed")
    if (all(c("logFC_rna", "logFC_micro") %in% names(cons))) {
      chk <- merge(cons[, c("Gene", "logFC_rna", "logFC_micro")], m, by = "Gene")
      dev <- max(abs(chk$logFC_rna.x - chk$logFC_rna.y), abs(chk$logFC_micro.x - chk$logFC_micro.y))
      add("3 Consensus", "logFC values copied correctly from the DE tables",
          if (is.finite(dev) && dev < 1e-6) "PASS" else "FAIL", sprintf("max difference %.2g", dev))
      flip <- sum(sign(cons$logFC_rna) != sign(cons$logFC_micro), na.rm = TRUE)
      add("3 Consensus", "Same direction on both platforms", if (flip) "FAIL" else "PASS",
          sprintf("%d genes change direction", flip))
    }
  } else add("3 Consensus", "Recompute consensus", "SKIP", "needs both DE_Results files and the consensus file")

  # ---------------------------------------------------------------------------
  # 4. Gene funnel: every step must be a subset of the step before
  # ---------------------------------------------------------------------------
  g_cons  <- if (!is.null(cons)) cons$Gene else NULL
  g_wgcna <- if (have(F$wgcna)) rd(F$wgcna)$Gene else NULL
  ppi     <- if (have(F$ppi)) rd(F$ppi) else NULL
  g_ppi   <- if (!is.null(ppi)) ppi$Gene[ppi$Status == "Interactive"] else NULL
  g_hubs  <- if (have(F$hubs)) rd(F$hubs)$Gene else if (have(F$hubs2)) rd(F$hubs2)$Gene else NULL
  g_final <- if (have(F$final)) rd(F$final)$Gene else NULL
  if (!is.null(g_wgcna) && !is.null(g_cons)) subset_of("4 Funnel", "Consensus-WGCNA genes are consensus DEGs", g_wgcna, g_cons, "overlap genes", "consensus")
  if (!is.null(ppi) && !is.null(g_wgcna)) same_set("4 Funnel", "PPI input = Consensus-WGCNA overlap", ppi$Gene, g_wgcna, F$ppi, F$wgcna)
  if (!is.null(g_hubs) && !is.null(g_ppi)) subset_of("4 Funnel", "Hub genes are STRING-connected genes", g_hubs, g_ppi, "hub genes", "Interactive genes")
  if (have(F$hubs) && have(F$hubs2)) same_set("4 Funnel", "Both hub-gene files agree", rd(F$hubs)$Gene, rd(F$hubs2)$Gene, F$hubs, F$hubs2)
  if (!is.null(g_final) && !is.null(g_hubs)) subset_of("4 Funnel", "Final ML panel genes are hub genes", g_final, g_hubs, "panel genes", "hub genes")
  add("4 Funnel", "Gene counts per step", "INFO", sprintf(
    "DEGs %s / %s -> consensus %s -> consensus & WGCNA %s -> STRING-connected %s -> hubs %s -> final panel %s",
    if (!is.null(de$ma)) length(sig_of(de$ma)) else "NA", if (!is.null(de$rna)) length(sig_of(de$rna)) else "NA",
    length(g_cons), length(g_wgcna), length(g_ppi), length(g_hubs), length(g_final)))

  # ---------------------------------------------------------------------------
  # 5. Machine learning: final panel = genes chosen by every method
  # ---------------------------------------------------------------------------
  X <- if (have(F$ml_in)) rd(F$ml_in) else NULL
  if (!is.null(X)) {
    samples <- as.character(X[[1]])
    add("5 ML", "ML input matrix", "INFO", sprintf("%d samples x %d genes: %s",
                                                    nrow(X), ncol(X) - 1L, show(names(X)[-1])))
    if (!is.null(g_hubs)) same_set("5 ML", "ML input genes = hub genes", names(X)[-1], g_hubs, F$ml_in, "hub genes")
    cohort <- character()
    for (p in c("Microarray", "RNAseq")) {
      fe <- latest(paste0("^Expression_(before|after)_batch_", p, ".*\\.csv$"))
      if (have(fe)) {
        cols <- names(utils::read.csv(fp(fe), nrows = 1, check.names = FALSE))
        k <- sum(samples %in% cols)
        if (k) cohort <- c(cohort, sprintf("%s (%d of %d samples)", p, k, length(samples)))
      }
    }
    add("5 ML", "Cohort the ML / nomogram models were trained on",
        if (length(cohort)) "INFO" else "SKIP",
        if (length(cohort)) paste(cohort, collapse = " + ") else "no Expression_*_batch_* file to match sample IDs")
  }
  # The *_ranking.csv files list every gene a method ranked; which genes each
  # method SELECTED is in combined_ML_gene_list.csv (a blank rank = not selected).
  ranked <- list()
  for (m in names(rank_files)) ranked[[m]] <- rd(rank_files[[m]])[[1]]
  if (length(ranked)) {
    add("5 ML", "Genes listed per ranking file (all ranked genes, not the selection)", "INFO",
        paste(sprintf("%s %d", names(ranked), lengths(ranked)), collapse = ", "))
    if (!is.null(g_hubs)) for (m in names(ranked))
      subset_of("5 ML", paste(m, "genes are ML-input genes"), ranked[[m]], g_hubs, m, "hub genes")
  }
  if (have(F$combined)) {
    cm <- rd(F$combined)
    rc <- grep("_rank$", names(cm), value = TRUE)
    sel <- lapply(rc, function(r) cm$Gene[!is.na(cm[[r]])]); names(sel) <- sub("_rank$", "", rc)
    add("5 ML", "Genes selected per method", "INFO",
        paste(sprintf("%s %d", names(sel), lengths(sel)), collapse = ", "))
    inter <- Reduce(intersect, lapply(sel, gset))
    if (!is.null(g_final)) {
      same_set("5 ML", "Final panel = genes selected by every method", g_final, inter, F$final,
               "recomputed intersection")
      if ("In_common_selected" %in% names(cm))
        same_set("5 ML", "combined_ML_gene_list 'In_common_selected' = final panel",
                 cm$Gene[as.logical(cm$In_common_selected)], g_final, F$combined, F$final)
    }
  } else add("5 ML", "Final panel = genes selected by every method", "SKIP",
             "combined_ML_gene_list.csv not found (it records which genes each method selected)")
  if (have(F$venn_ml) && have(F$final) && mt(F$venn_ml) < mt(F$final))
    add("5 ML", "ML Venn figure from the same run as the final panel", "FAIL",
        sprintf("%s (%s) is older than %s (%s)", F$venn_ml, format(mt(F$venn_ml), "%Y-%m-%d %H:%M"),
                F$final, format(mt(F$final), "%Y-%m-%d %H:%M")))

  # ---------------------------------------------------------------------------
  # 6. ROC / validation / nomogram use the final panel
  # ---------------------------------------------------------------------------
  if (!is.null(g_final)) {
    if (have(F$roc)) same_set("6 Panel use", "ROC genes = final panel", rd(F$roc)$Gene, g_final, F$roc, F$final)
    if (have(F$roc_pan)) {
      rp <- rd(F$roc_pan)
      same_set("6 Panel use", "ROC combined-panel genes = final panel",
               rp$Feature[rp$Type != "Multi-gene"], g_final, F$roc_pan, F$final)
      lab <- rp$Feature[rp$Type == "Multi-gene"]
      k <- suppressWarnings(as.integer(sub(".*\\((\\d+) genes?\\).*", "\\1", lab)))
      if (length(k) && !is.na(k[1]))
        add("6 Panel use", "'Combined panel (k genes)' label = panel size",
            if (k[1] == length(g_final)) "PASS" else "FAIL",
            sprintf("label says %d, final panel has %d", k[1], length(g_final)))
    }
    if (have(F$roc_tv)) same_set("6 Panel use", "Validation ROC genes = final panel", rd(F$roc_tv)$Gene, g_final, F$roc_tv, F$final)
    if (have(F$val_ov)) subset_of("6 Panel use", "Validation overlap genes are panel genes", rd(F$val_ov)$Gene, g_final, F$val_ov, "final panel")
    if (have(F$nom_coef)) {
      pred <- rd(F$nom_coef)$Predictor
      subset_of("6 Panel use", "Nomogram predictors are panel genes", pred, g_final, "nomogram predictors", "final panel")
      add("6 Panel use", "Nomogram predictors", "INFO", sprintf("%d of %d panel genes: %s",
                                                                 length(pred), length(g_final), show(pred)))
    }
  }

  # ---------------------------------------------------------------------------
  # 7. Nomogram numbers agree with each other
  # ---------------------------------------------------------------------------
  for (w in c("tr", "va")) {
    f <- F[[paste0("nom_", w)]]
    if (!have(f)) next
    p <- rd(f)[1, ]; lab <- if (w == "tr") "Training" else "Validation"
    ok <- with(p, TP + FN == N_Disease && TN + FP == N_Normal && N_Disease + N_Normal == N_Total)
    add("7 Nomogram", paste(lab, "confusion matrix adds up to n"), if (isTRUE(ok)) "PASS" else "FAIL",
        sprintf("n = %d (%d disease / %d control); TP/FP/FN/TN = %d/%d/%d/%d; AUC %.3f; threshold %.3f",
                p$N_Total, p$N_Disease, p$N_Normal, p$TP, p$FP, p$FN, p$TN, p$AUC, p$Threshold))
    sens <- p$TP / (p$TP + p$FN); spec <- p$TN / (p$TN + p$FP)
    add("7 Nomogram", paste(lab, "sensitivity / specificity match the counts"),
        if (abs(sens - p$Sensitivity) < 1e-6 && abs(spec - p$Specificity) < 1e-6) "PASS" else "FAIL",
        sprintf("from counts %.3f / %.3f, file says %.3f / %.3f", sens, spec, p$Sensitivity, p$Specificity))
    if (w == "tr" && !is.null(X))
      add("7 Nomogram", "Training n = rows of the ML input matrix",
          if (p$N_Total == nrow(X)) "PASS" else "FAIL", sprintf("nomogram n %d, ML matrix %d", p$N_Total, nrow(X)))
  }
  if (have(F$nom_tr) && have(F$roc) && have(F$nom_coef)) {
    pred <- rd(F$nom_coef)$Predictor
    if (length(pred) == 1L) {
      r <- rd(F$roc); a_gene <- r$AUC[r$Gene == pred]
      a_nom <- rd(F$nom_tr)$AUC[1]
      if (length(a_gene))
        add("7 Nomogram", "Single-gene nomogram AUC = that gene's ROC AUC",
            if (abs(a_gene - a_nom) < 1e-6) "PASS" else "WARN",
            sprintf("%s: ROC %.4f, nomogram training %.4f", pred, a_gene, a_nom))
    }
  }

  # ---------------------------------------------------------------------------
  # 8. Validation cohort: is each panel gene's direction replicated?
  # ---------------------------------------------------------------------------
  if (have(F$val_de) && !is.null(cons) && !is.null(g_final)) {
    v <- rd(F$val_de)
    for (g in g_final) {
      tr <- cons$logFC[cons$Gene == g]; vv <- v[v$Gene == g, ]
      if (!length(tr) || !nrow(vv)) { add("8 Validation", paste("Direction of", g), "SKIP", "gene missing in a file"); next }
      same <- sign(tr[1]) == sign(vv$logFC[1])
      sig  <- isTRUE(vv$adj.P.Val[1] < 0.05)
      add("8 Validation", paste("Replication of", g),
          if (same && sig) "PASS" else "WARN",
          sprintf("training logFC %+.2f, validation logFC %+.2f (adj.P %.3g) - %s", tr[1], vv$logFC[1],
                  vv$adj.P.Val[1],
                  if (!same) "direction NOT replicated"
                  else if (sig) "replicated" else "same direction but not significant in validation"))
    }
  }

  # ---------------------------------------------------------------------------
  # 9. Optional: recompute training AUCs from a labels file (Sample, Condition)
  # ---------------------------------------------------------------------------
  if (!is.null(labels)) {
    if (!file.exists(labels) || is.null(X)) {
      add("9 Recompute AUC", "Training AUC per gene", "SKIP", "labels file or ML input matrix not found")
    } else {
      L <- utils::read.csv(labels, stringsAsFactors = FALSE)
      y <- L$Condition[match(as.character(X[[1]]), L$Sample)]
      ctrl <- grepl("normal|control|healthy", y, ignore.case = TRUE)
      yy <- ifelse(is.na(y), NA, ifelse(ctrl, 0, 1))
      add("9 Recompute AUC", "Labels matched", if (anyNA(yy)) "WARN" else "PASS",
          sprintf("%d disease / %d control / %d unmatched", sum(yy == 1, na.rm = TRUE),
                  sum(yy == 0, na.rm = TRUE), sum(is.na(yy))))
      if (have(F$roc)) {
        r <- rd(F$roc)
        for (g in intersect(r$Gene, names(X))) {
          a <- auc(X[[g]], yy); a <- max(a, 1 - a)
          add("9 Recompute AUC", paste("AUC", g), if (abs(a - r$AUC[r$Gene == g]) < 1e-3) "PASS" else "FAIL",
              sprintf("recomputed %.4f, app %.4f", a, r$AUC[r$Gene == g]))
        }
      }
    }
  } else add("9 Recompute AUC", "Training AUC per gene", "SKIP",
             "give labels = 'file.csv' (columns Sample, Condition) to recompute; the app does not export training labels")

  # ---------------------------------------------------------------------------
  # 10. Batch correction applied? Blank or duplicated figures?
  # ---------------------------------------------------------------------------
  for (p in c("", "Microarray_", "RNAseq_")) {
    b <- latest(paste0("^Expression_before_batch_", p, "\\d"))
    a <- latest(paste0("^Expression_after_batch_", p, "\\d"))
    if (have(a) && have(b)) {
      same <- identical(unname(tools::md5sum(fp(a))), unname(tools::md5sum(fp(b))))
      add("10 Batch", paste("Batch correction changed the data", if (nzchar(p)) sub("_", "", p) else "(merged)"),
          if (same) "FAIL" else "PASS",
          if (same) sprintf("%s is byte-identical to %s: correction was NOT applied", a, b) else sprintf("%s differs from %s", a, b))
    }
  }
  imgs <- list.files(dir, pattern = "\\.(jpg|jpeg|png)$", ignore.case = TRUE)
  blank <- character()
  for (f in imgs) {
    img <- tryCatch(if (grepl("png$", f, ignore.case = TRUE)) png::readPNG(fp(f)) else jpeg::readJPEG(fp(f)),
                    error = function(e) NULL)
    if (is.null(img)) next
    ch <- if (length(dim(img)) == 3) img[, , 1:min(3, dim(img)[3])] else img
    if (mean(ch < 0.97) < 0.002) blank <- c(blank, f)      # < 0.2% non-white pixels
  }
  add("10 Figures", "Blank (all-white) images",
      if (length(blank)) "FAIL" else if (!length(imgs)) "SKIP" else "PASS",
      if (length(blank)) show(blank, 50) else sprintf("%d images checked", length(imgs)))
  allf <- list.files(dir, pattern = "\\.(csv|jpg|jpeg|png|pdf)$", ignore.case = TRUE)
  md5 <- tools::md5sum(fp(allf)); names(md5) <- allf
  dup <- split(names(md5), md5); dup <- dup[lengths(dup) > 1]
  expected <- function(x) all(grepl("^(common_genes_ML_methods|final_list_common_genes_ML)\\.csv$", x))
  dup <- dup[!vapply(dup, expected, logical(1))]
  if (length(dup)) for (d in dup) add("10 Figures", "Different files with identical content", "WARN", paste(d, collapse = " == "))
  else add("10 Figures", "Different files with identical content", "PASS", "none")

  # ---------------------------------------------------------------------------
  # Report
  # ---------------------------------------------------------------------------
  tab <- table(factor(res$Status, levels = c("PASS", "INFO", "WARN", "FAIL", "SKIP")))
  lines <- c(sprintf("GExPipe run check - %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
             sprintf("Folder: %s", normalizePath(dir, winslash = "/")),
             sprintf("PASS %d | INFO %d | WARN %d | FAIL %d | SKIP %d",
                     tab[["PASS"]], tab[["INFO"]], tab[["WARN"]], tab[["FAIL"]], tab[["SKIP"]]), "")
  for (a in unique(res$Area)) {
    lines <- c(lines, paste0("== ", a))
    s <- res[res$Area == a, ]
    lines <- c(lines, sprintf("  [%s] %s%s", s$Status, s$Check, ifelse(nzchar(s$Detail), paste0(": ", s$Detail), "")), "")
  }
  verdict <- if (!all(found[core])) paste("INCOMPLETE - key result files are missing:",
               paste(file_label[core[!found[core]]], collapse = ", "),
               "- download them from the app into this folder and run again")
             else if (tab[["FAIL"]] > 0) "NOT CONSISTENT - fix the FAIL lines (usually: re-export from ONE run into an EMPTY folder)"
             else if (tab[["WARN"]] > 0) "CONSISTENT, with warnings to review"
             else "CONSISTENT - all checks passed"
  lines <- c(lines, paste("VERDICT:", verdict))
  writeLines(lines, file.path(out_dir, "GExPipe_Run_Check.txt"), useBytes = TRUE)
  utils::write.csv(res, file.path(out_dir, "GExPipe_Run_Check.csv"), row.names = FALSE)
  cat(lines, sep = "\n")
  invisible(res)
}

if (!interactive() && length(commandArgs(TRUE)) >= 1L && sys.nframe() == 0L) {
  a <- commandArgs(TRUE)
  gexpipe_check_run(a[1], labels = if (length(a) >= 2L) a[2] else NULL)
}
