# GExPipe 0.99.109

- Download (Step 1): multi-platform GEO series (e.g. GSE18123, GPL570 + GPL6244) now download.
  The resumable pre-fetch reads the series' matrix/ folder and fetches every
  GSE-GPLxxx_series_matrix file instead of a non-existent single file, which previously
  reported "0 MB downloaded, in progress" forever. Small (< 1 MB) series matrices are now
  accepted as complete.
- Normalization (Step 2): new "Download Before / After Normalization Data" box (all analysis
  types). ZIP with the Step 1 input matrices, per-dataset / per-platform / combined normalized
  matrices, per-sample and per-dataset check tables (log scale, spread of sample medians) and a
  README; or the check table alone as CSV.
- Normalization (Step 2): RMA from CEL files now converts probe-set IDs to gene symbols with the
  GPL / annotation-package converter used in Step 1, so Affymetrix datasets overlap the others.
  "No genes remain ..." errors now name each dataset, its row count and example IDs.
- Batch Correction (Step 5): new quantitative batch-effect check (before vs after). Reports the %
  of top-PC variance explained by batch and by condition, PC1 batch R-squared, silhouette by
  batch, % genes with a significant batch effect (limma) and a verdict per corrected matrix.
  Downloads: check table (CSV) and ZIP with matched before/after matrices, sample metadata,
  check table and README. Parallel completion message now shows the real sample count.
- QC (Step 3), Parallel mode: per-platform outlier plots and tables appear inside Sample Outlier
  Detection only after it is run (no empty boxes); each outlier table has a CSV download.
- PPI: Consensus_Hub_Genes.csv now lists the methods that selected each gene and all centrality
  scores, and downloads (with headers) even when no consensus hub is found.
- Microarray example IDs corrected to GSE268456, GSE47927 (was GSE26856).
- QC (Step 3), Parallel mode: common genes are now shown in separate RNA-seq (blue) and
  microarray (orange) boxes, each with its own Venn and UpSet box, instead of one combined block.
  Two datasets with identical gene sets are drawn as one labelled circle ("100% overlap")
  instead of VennDiagram's "n n (Coincidental)".

# GExPipe 0.99.108

- Version bump for Bioconductor: devel branch synced with the GitHub repository (all changes
  from 0.99.52 to 0.99.107 are now on git.bioconductor.org).

# GExPipe 0.99.107

- Example GSE IDs: training examples are RNA-seq GSE144119, GSE100026 and microarray GSE268456, GSE47927; validation examples are RNA-seq GSE162462 and microarray GSE13159.
- Batch Correction page, Parallel mode: the expression export now gives a separate before and after
  CSV for RNA-seq and for microarray (four files), matching the per-platform batch correction.
  Single/Merged mode keeps the two combined files.
- Step 3 Quality Control, Parallel mode: the Venn and UpSet plots no longer pool all datasets.
  Common genes are now identified separately for the RNA-seq datasets and for the microarray
  datasets (own Venn, UpSet, common-gene count and PNG/JPG/PDF downloads for each platform).
- Step 7 (Consensus): two new figures with 300 DPI PNG/JPG/PDF downloads, also auto-saved on
  Apply: DEG_LogFC_Concordance_Scatter.png (RNA-seq vs microarray logFC of the common DEGs, % same
  direction, Spearman rho, top genes labelled) and Consensus_DEG_Hierarchical_Heatmap.png (top
  consensus DEGs, z-scored within platform, clustered, annotated by Condition and Platform).
- Step 3 Quality Control, Parallel mode: Sample Outlier Detection now shows its graphs. PCA
  (Mahalanobis) and sample-connectivity plots appear for RNA-seq and for microarray (PNG, JPG
  300 DPI and PDF downloads), and the "Exclude Selected Outliers" selector is available.
  Previously these were only drawn in Merged mode.
- Gene-expression boxplots (training and validation): Disease is now red and Normal green.
- ROC step (Step 12): per-gene AUC tables now include DeLong 95% confidence intervals
  (AUC_Lower_95CI / AUC_Upper_95CI) for training, and for validation in the Training vs
  Validation comparison table; the CIs are also in the CSV downloads.
- Results Summary: new "Where did my genes go?" gene funnel. It lists the number of genes
  left after each step (Step 6 DE, Step 7 consensus, Step 8 common genes, Step 10 ML,
  Step 12 AUC >= 0.8, nomogram) and names the first step that reached zero, with a hint on
  which cutoff to relax.
- Second filter (Groups step and External Validation): empty values in the filter column
  are now a tickable "(blank)" choice, ticked by default. Previously only non-empty values
  were listed, so samples with an empty value (e.g. controls in a "disease stage" column that
  only exists for patients, as in GSE47927) could never be kept and were silently dropped,
  leaving only one group. The filter panel also warns in red when a filter removes every
  sample of a group, and the validation run stops with the same hint.
- Validation (Step 12): new "Signature validation" panel. Single-gene significance and
  fold-change cutoffs do not transfer between platforms or tissues, so the whole signature is
  scored instead: each validation sample gets the mean of the signed, standardized expression
  of the signature genes (direction learned on the training datasets by a per-dataset
  Stouffer meta-analysis, so platforms and batches never mix). Reports the AUC with a DeLong
  95% CI, permutation p-values against random genes and against random genes that passed the
  same training filter (the second catches winner's-curse selection), per-gene direction
  replication, the AUC for several signature sizes, and a leave-one-dataset-out table and
  figure across the training datasets. Gene source: meta-analysis top N (default),
  all passing genes, Step 8/10/12 genes, or a custom list. Every run is appended to a
  "validation attempts log" so all validation sets tried can be reported. New helpers in
  R/gexp_signature_validation.R and R/server_signature_validation.R.
- ROC step (Step 12): the training and validation gene-expression panels are now built by
  one shared function, so they have identical gene order (training-AUC order, genes present
  in both), group order (Normal, Disease), colours, layout and size. Each panel shows a
  bracket with significance stars (Wilcoxon rank-sum per gene, Benjamini-Hochberg adjusted
  across the genes shown) and the group sizes. Previously the validation panel used the ML
  gene order, and group colours depended on which group appeared first in the data.
- Batch Correction and DE pages: removed the duplicate "Next" buttons. Each page now
  has a single Next button at the bottom (after the logs); the extra one directly under
  "Apply Batch Correction" / "Run DE Analysis" and its unused handlers are gone.
- Groups (Step 4): the optional second filter now shows its own result ("After this
  filter: CML = 48, Healthy = 17 (65 of 97 samples kept)"); the Column Preview below it
  counts before the filter, which was easy to misread.
- Diagnostic model + ROC step: fixes for misleading external-validation results.
  - Calibration plot: the validation curve combined the calibration-in-the-large
    intercept with the slope from a different fit, so it did not pass through the
    binned points. It now uses the intercept and slope of the same logistic
    recalibration fit (the reported calibration intercept is unchanged).
  - Step 12 per-gene ROC: AUCs used pROC's `direction = "auto"` separately in training
    and validation, so a gene that goes up in training but down in validation still
    looked validated. The direction is now fixed from the training data; a reversed
    gene shows validation AUC < 0.5 and `Direction_Replicated = FALSE`
    (`Direction_Training` is added to the tables).
  - External validation standardization: within-cohort z-scoring assumes a similar
    disease prevalence; with e.g. 81% disease in training vs 48% in validation every
    standardized value, and so every predicted risk, shifted toward Disease (low
    specificity, poor calibration, normal AUC). New Run-box option "matched to the
    training prevalence" (validation labels used only to weight the reference mean/SD;
    no refit, no new threshold) and "training mean/SD" (same-platform only); the default
    is unchanged but now warns about prevalence and raw-scale differences.
  - New training-data checks (warnings + recorded in the run settings): Dataset/Condition
    confounding, housekeeping-gene negative control (GAPDH, ACTB, ...), and agreement
    between the Step 12 and Step 14 expression data.
  - Calibration estimation no longer throws when predictions saturate (reported as unstable).
- Diagnostic model (Step 14, Nomogram): reporting and validation overhaul.
  - Coefficient table now reports Coefficient, Std_Error, OR, 95% CI for the OR,
    p-value, VIF and VIF status; the Firth fit adds coefficient, SE, OR, profile
    penalized-likelihood 95% CI and p-value (Firth failures are reported, not hidden).
  - Outcome coding is explicit and shown: Disease = 1, Normal = 0 (exact
    "Disease"/"Normal" labels first; any keyword-based inference is flagged to the user).
  - Bootstrap optimism correction: default B raised from 200 to 1000, user-configurable
    (100-5000) with a fixed, user-visible seed; reports apparent value, mean optimism,
    corrected value, B requested and B successful for the C-index, calibration slope
    and Brier score.
  - External/internal validation applies the training model exactly as fitted:
    the training intercept and coefficients are applied directly to the validation
    predictors, and the threshold is the training-derived Youden threshold. The previous
    intercept recalibration on validation outcomes was removed, and model genes missing
    from the external dataset are no longer imputed with 0 - the panel is restricted to
    shared genes before fitting.
  - Performance table now includes N, accuracy, sensitivity, specificity, PPV, NPV and
    AUC with 95% CIs (exact Clopper-Pearson; DeLong for AUC), the threshold,
    numerator/denominator counts (e.g. 42/52) and TP/TN/FP/FN, plus a Note for
    small classes or a degenerate AUC CI.
  - New: calibration statistics (intercept, slope, Brier) for training (apparent and
    bootstrap-corrected) and validation, estimated only when each class has >= 10
    samples; confusion-matrix figures/tables; combined training-vs-validation ROC;
    publication-style ROC/calibration figures.
  - New figures and tables are saved automatically (300 DPI PNG + PDF, CSV) to the
    existing export folder, and every table has a CSV download (coefficients, Firth,
    optimism, training/validation performance, calibration, confusion, settings).
  - Input checks with clear notifications: missing/non-numeric predictors, NA/Inf,
    single outcome class, tiny classes, label/sample mismatches, convergence failures.
  - Helpers live in the new R/gexp_diagnostic_metrics.R.
- PPI: the Run handler no longer crashes with "argument is of length zero" when
  the score-threshold / top-hubs inputs are missing (e.g. the settings panel
  has not rendered); it falls back to the defaults (400 / 15).
- Downloads: a GSE already on disk is now always checked FIRST and used, in
  Training and External Validation, for microarray and RNA-seq; a download only
  starts when nothing usable is found. Verified offline (network cut off):
  - Microarray: a complete cached series matrix is parsed directly from disk.
    Previously GEOquery contacted NCBI to list files before looking at the
    cache, so a downloaded series still failed without internet.
  - RNA-seq: a counts file placed directly in `rna_data`/`ext_val_rna` (not
    only in a per-GSE subfolder) is found; previously it was ignored and the
    app spent ~50 s on fallback attempts. The cache check no longer depends on
    a validation-only option.
- Batch correction (Parallel): added a "Same method for both platforms" mode
  next to Auto (best method per platform) and Manual (pick each platform).
  RNA-seq and microarray are still corrected separately, just with one
  chosen method (ComBat-ref, limma, ComBat or SVA).
- Groups (Step 4) and External Validation: added a manual group-assignment
  mode. Choose "Manual: tick samples in the phenodata table", tick rows
  (column search boxes narrow the table; "Tick all filtered rows" selects every
  match), then Assign ticked -> Normal / Disease (or Unassign / Clear all). An
  "Assigned" column shows each sample's group; unassigned samples are
  excluded. The default column-based method is unchanged.
- Downloads: fixed validation (and Groups) phenodata fetch ignoring an
  already-downloaded series matrix. `gexp_fetch_full_pdata()` called GEOquery
  with no cache folder, so it re-downloaded the whole GSE (886 MB for
  GSE13159) into a temp dir - over an hour. It now reads just the header of
  the local `*_series_matrix.txt.gz` (1.8 s for 2096 samples); the header
  reader also stops at the expression table instead of reading the whole
  file, and `.gexpipe_getgeo_series()` reuses a complete local copy when no
  cache dir is passed.
- Groups (Step 4): same optional second sample filter per dataset ("+ Add
  second filter"), applied at Extract Groups so skipped samples get no group.
- External Validation: added an optional second sample filter next to the
  group-column selector ("+ Add second filter"). Pick another phenodata
  column (e.g. tissue/source) and untick values to skip, so the validation
  cohort can be narrowed (e.g. CML + peripheral blood only). Hidden and
  off by default; "Remove filter" restores the unfiltered behaviour.

- Downloads (Training + External Validation): a manually-downloaded or
  previously-downloaded GSE is now reliably reused instead of re-downloaded,
  in both steps:
  - External Validation's download handler used to unconditionally wipe the
    entire `ext_val_rna`/`ext_val_micro` cache folder on every click, which
    forced a full re-download even when the exact same GSE had already been
    downloaded (or manually placed there). Now only files for GSE IDs NOT in
    the current request are cleared; a file matching a currently-requested
    GSE ID is kept and reused.
  - `gexp_download_one_microarray_gse`/`gexp_download_one_rnaseq_gse` (the
    shared functions used by both Training and Validation) now search
    several candidate cache locations for a complete, already-downloaded
    file for the requested GSE - including a package-bundled
    `inst/shinyapp/ext_val_micro`/`ext_val_rna` location - before attempting
    any download, and skip straight to using it when found.
  - A successful External Validation download is now also persisted into
    that canonical `ext_val_micro`/`ext_val_rna` store, so it stays
    available for reuse in later sessions, not just the current one.
- Downloads: fixed the resumable large-series pre-fetch throwing away
  real progress on every failed attempt. It used to delete the partial
  file once it ran out of retries (to stop GEOquery crashing on an
  incomplete leftover), which meant a user who reached, say, 50% before
  hitting the retry limit had to start over from 0% on every subsequent
  try, never actually finishing a large series like GSE13159 in
  practice. The partial file is now KEPT (it's still valid gzip, just
  incomplete), and the app stops with a clear message instead of
  handing it to GEOquery: "download in progress, not yet complete - 50%
  (443 of 886 MB). Progress is saved; click Download again to continue
  from here (not from the start)." Clicking Download again resumes from
  the saved byte offset rather than restarting.
- Downloads: added real download-progress reporting for the resumable
  large-series pre-fetch (e.g. GSE13159) - "GSEnnnnn: 47% (140 of 300
  MB)", updated every ~20 seconds (was: no progress at all, just a spinner
  for however long the download took). The total file size comes from a
  real HTTP HEAD-style request (Content-Length header), verified to
  match the final downloaded size exactly. Reported via
  `incProgress(amount = 0, detail = ...)`, which only updates the
  displayed text and never moves the bar's numeric value, so it can't
  conflict with the outer per-dataset progress bar that already owns
  that scale; this also means it's a safe no-op outside a Shiny session
  (the manual pipeline scripts), never an error.
- Manual pipeline scripts (`inst/scripts/manual_pipeline_rnaseq.R`,
  `manual_pipeline_microarray.R`): fixed a real gap - both single-GSE
  download convenience functions default to `fast = TRUE`, which skips
  gene-ID-to-symbol conversion entirely, and both scripts were calling
  them without overriding it. Row names stayed as raw Entrez IDs (RNA-seq)
  or raw probe IDs (microarray) all the way through the pipeline, which
  every later step (WGCNA, GO/KEGG, STRINGdb) silently mishandled since
  they all assume real gene symbols. Confirmed directly with real
  downloads before and after: RNA-seq now converts correctly with
  `fast = FALSE` alone; microarray needed an added explicit finishing
  step (`org.Hs.eg.db` against whatever intermediate ID format the
  platform's own conversion produces - probe ID, RefSeq, or Ensembl
  transcript, which vary by platform), since `fast = FALSE` alone was
  not sufficient there. Verified: 93% of probes (44,053 of 47,323)
  resolved to real symbols on a platform that previously returned every
  row as an unconverted probe ID.
  Also added a guardrail: if `group_col`/`normal_values`/`disease_values`
  don't match a dataset's real phenodata, every sample's Condition ends
  up NA and gets filtered out - which silently strips `colnames()` from
  the expression matrix entirely (confirmed: `matrix[, logical(0)]` loses
  its colnames even with `drop = FALSE`), surfacing several steps later
  as a confusing "Expression matrix must have colnames" error in Batch
  correction instead of a clear one where the real problem is.
- Downloads: fixed a crash ("argument is of length zero") introduced by
  the resumable pre-fetch below when it exhausts every retry attempt
  without completing - it left the large partial file sitting in
  GEOquery's cache folder, and GEOquery's own cache-reuse check only
  looks at whether the file exists, not whether it's complete, so it
  logged "Using locally cached version" and tried to parse the
  truncated gzip, crashing deep in its own parser. Reproduced directly
  against a real partial GSE13159 download to confirm the exact
  mechanism before fixing it. The partial file is now deleted if every
  retry attempt fails, so GEOquery always either finds a genuinely
  complete file or starts completely fresh.
- Downloads: very large series (e.g. GSE13159, ~2000+ samples, a
  multi-hundred-MB series matrix) now get a resumable, retried pre-fetch
  before GEOquery's own download runs. GEOquery's download is a single
  unresumed request; on a real run against GSE13159 we found the
  connection can be cut short partway through - sometimes as a clear
  timeout, but sometimes NCBI's server closes it early with no error at
  all, so a plain "did the request succeed" check isn't trustworthy for
  this file. The new pre-fetch uses curl::multi_download(resume = TRUE)
  (verified directly: a genuine HTTP 206 Range-resume, continuing from
  the exact byte a prior attempt stopped at, not restarting), retried up
  to 10 times, each attempt continuing from wherever the last stopped.
  Completeness is verified by checking the file actually ends with
  `!series_matrix_table_end`, not just "no error was raised" - the same
  check already used to detect a truncated cache from an earlier
  session. Deliberately conservative: only the plain
  "<GSE>_series_matrix.txt.gz" filename is handled (not the "-GPLxxxx"
  multi-platform variant); any failure here falls through unchanged to
  GEOquery's existing download, so this can only help, never make a
  download worse.
- Gene ID conversion: added automatic detection and conversion of circRNA
  IDs using circBase's own numbering (`hsa_circ_XXXXXXX`) to their host
  gene symbol, via a bundled, verified lookup table (4,189 circRNAs, from
  circbase.org's Salzman2013/Jeck2013/Memczak2013/Rybak2015/Zhang2013
  annotation files). This is a real, safe conversion - the table was
  downloaded and checked directly before bundling.
  A different, common case - a study's ORIGINAL discovery-cohort ID
  (e.g. Arraystar circRNA microarray IDs like "A-NT2RP7011570", which
  encode the tissue/cell line a circRNA was first found in) - has no
  public, generically-fetchable mapping to a gene symbol (verified: the
  exact IDs from GSE71008 do not appear anywhere in circBase's own
  downloadable tables). These are now correctly labeled "circRNA
  discovery ID - no public gene-symbol mapping available" in the
  download log instead of the previous, misleading generic "Microarray
  probe-like ID" guess, and are still safely kept as original IDs
  rather than converted to a wrong or guessed symbol.
- Step 7 (RNA-seq ∩ microarray): added a "Combination method" choice -
  Intersection (Consensus, the existing default and recommended for a
  final panel: a gene must be significant on both platforms) or Union
  (Exploratory, new: a gene significant on either platform passes, with
  no cross-platform confirmation). `gexpipe_consensus_degs()` gained a
  `combine = c("intersection", "union")` argument; the same-direction
  filter still only ever applies to genes found on both platforms.
- Nomogram: fixed Sensitivity/Specificity and PPV/NPV being swapped in the
  performance table (`caret::confusionMatrix()` treated "Normal" as the
  positive class); Disease is now the positive class.
- Nomogram: the gene panel now comes from the user's confirmed Step 12
  selection (a stale selection from an earlier run is ignored). When the
  events-per-variable rule forces a reduction, genes are ranked by
  association with the outcome instead of variance, and the user is told
  which genes were kept or dropped. Highly correlated genes (|r| > 0.8)
  are pruned before fitting.
- Nomogram: predictors are z-scored per cohort before fitting/prediction,
  and the external-validation intercept is recalibrated (offset model,
  slopes kept from training) so training and validation prevalence
  differences no longer collapse calibration/DCA/clinical-impact curves.
- Nomogram: near-separated models (slope above 5 per SD, or SE above 3)
  are refit with a ridge penalty; Firth (`logistf`) coefficients are
  reported alongside the standard ones (`logistf` added to Imports).
- Nomogram: added a bootstrap optimism-corrected C-index table, a warning
  for small validation cohorts, a note on minimum sample sizes, and a
  message naming the genes responsible when validation predictions are
  constant (AUC exactly 0.5). The Training calibration plot and both DCA
  plots now carry titles like the ROC and clinical-impact panels, in both
  merged and parallel modes.
- Validation (Step 11): the DE method chosen by the user (limma, DESeq2,
  edgeR) is now honoured; it was previously hard-coded to limma. RNA-seq
  counts are rounded after duplicate-symbol averaging so DESeq2/edgeR
  stay eligible. GSE examples added to the input box.
- ROC (Step 12): the Training vs Validation AUC comparison now uses each
  gene's true training AUC instead of dropping genes below the AUC cut-off.
- PPI (Step 9): fixed an "argument is of length zero" crash caused by an
  unrendered gene-set selector returning `character(0)`.
- Downloads: the GEO download timeout is raised to 1 hour for very large
  series (e.g. GSE13159). Truncated cached series-matrix files (from
  interrupted downloads) are now detected by checking for the closing
  `!series_matrix_table_end` line and re-downloaded instead of reused.
- Parallel mode: added PNG/JPG/PDF download buttons to all RNA-seq and
  microarray plots in QC, Normalization, Batch correction and Results.
- Removed the misleading "PDF" badge from the "16. Results Summary" menu
  item (the page has no whole-report PDF download).
- Parallel mode (Step 6 Results): added the top-N DE genes heatmap for
  each platform (RNA-seq and Microarray), with PNG/JPG/PDF downloads.
  The "Heatmap Genes:" input already existed per platform but had no
  heatmap wired to it; merged mode already had this heatmap.
- Nomogram: fixed a parallel-mode-only data integrity gap - training data
  in parallel mode is a UNION of RNA-seq and Microarray genes with NA
  filled in for the platform that never measured a given gene; a gene
  measured on only one platform was silently fit as a real predictor,
  causing every sample from the OTHER platform to be dropped from the
  model without warning. The model now only uses genes present on both
  platforms, and names any gene excluded for this reason. Merged mode
  was never affected (its matrices are intersected before training).
- Nomogram: added VIF-based collinearity pruning after the pairwise
  correlation check. Two genes can each have |r| < 0.8 individually
  while one is a near-linear combination of several others (multi-way
  collinearity), which pairwise correlation cannot see but shows up as
  a high VIF; such a gene's coefficient/SE cannot be trusted. The worst
  offender (VIF > 10) is dropped and the model refit, repeated until
  every remaining gene's VIF is acceptable, with the reason shown.
- Nomogram: fixed misleading guidance in the Model Diagnostics note when
  quasi-complete separation triggers the ridge-penalized refit - in that
  case, Coefficient/Std_Error/OR are already the ridge-corrected values,
  while the _Firth columns come from a separate, unpenalized fit and can
  look MORE extreme. The note used to always say "trust Firth when they
  disagree," which was backwards in this specific case; it now explains
  which columns are already corrected depending on whether ridge fired.
- Common Genes (Step 9): fixed a significant parallel-mode-only bug -
  right after Step 6's parallel DE finishes, `rv$sig_genes` is always
  overwritten with ONLY the RNA-seq significant genes (a leftover
  "default view" assignment unrelated to any user action), so every
  Common Genes computation (DEG intersect WGCNA module) silently
  excluded all Microarray DEGs from the candidate pool feeding ML, ROC
  and the Nomogram - in every parallel-mode run, not just some. Common
  Genes now unions both platforms' own significant-gene tables when
  Step 7 (consensus) hasn't been run, and still correctly uses Step 7's
  real combined result when it has. Merged mode was never affected.
- Every step's processing log now uses one shared dark-terminal style
  with a consistent closing summary block.

# GExPipe 0.99.106

- Fixed a metadata desync bug: applying group labels in Step 4 trimmed
  `combined_expr`/`expr_micro`/`expr_rna`/`unified_metadata` to the kept
  samples but left `combined_expr_before_global_norm` untouched, so any
  time Step 4 excluded a sample, Parallel-mode Step 5 batch correction
  crashed the app with a metadata-alignment error.
- Fixed `edgeR::filterByExpr()` (built for raw integer counts) being
  applied to already-normalized continuous log-intensity data in the
  limma DE path, which could silently filter out every gene
  ("Independent filtering removed all genes"). Independent filtering now
  detects whether the input looks like counts and uses an appropriate
  filter either way.
- Fixed `.gexpipe_factor_meta()` hardcoding `levels = c("Normal",
  "Disease")` when re-leveling `Condition`: after renaming groups in
  Step 4 (e.g. to custom labels), every sample's Condition silently
  became `NA`, destroying the DE design matrix. Group labels are now
  preserved regardless of what they're named.
- Fixed empty/zero-length platform IDs (`Biobase::annotation()`
  returning `character(0)`) silently breaking probe-to-symbol GPL
  lookups; added a `platform_id` phenodata fallback.
- Fixed corrupted/partial GEO series-matrix and GPL annotation cache
  files (from earlier interrupted downloads) being reused indefinitely
  instead of triggering a fresh download, including one path that could
  segfault R outright when a mislabeled `.gz` file was opened.
- Fixed the same class of corrupted-cache bug for STRINGdb's PPI step
  (alias/interaction files); GExPipe now manages its own validated
  STRINGdb cache directory instead of relying on STRINGdb's unmanaged
  default.
- Fixed stale `raw_counts_for_deseq2`/`rna_counts_list` from an earlier,
  unrelated download being treated as "available" for a later run with
  different samples; now requires real sample overlap with the current
  dataset before being trusted.
- Fixed a duplicate y-axis rendering bug in the WGCNA Module Eigengene
  Dendrogram (overlapping/garbled axis labels).
- External Validation now runs the same pipeline as the main analysis
  for the same GSE: probe/gene ID-to-symbol conversion uses the same
  GPL/fData information, per-dataset normalization uses the same
  function, gene filtering uses the same variance-percentile step, batch
  correction (real ComBat-ref/limma for 2+ datasets, or an optional
  technical-covariate diagnostic for a single dataset) mirrors Step 5,
  and DE uses the same shared engines with Dataset-aware covariates.
- Increased the browser idle-disconnect keep-alive window from 30 to
  60 minutes.
- GEO series-matrix downloads get a longer timeout (600s) so very large
  series (e.g. GSE13159, ~2000+ samples) don't time out on a normal
  connection and get misreported as a network/connectivity failure.
- Parallel-mode Step 5 now has separate variance-percentile sliders for
  RNA-seq and microarray (previously shared one value for both).
- Results Summary (Step 16) is now a text/table-only overview - all
  plots, JPG/PDF download buttons, and the "Cite this analysis" box
  were removed from this tab; figures remain available on each step's
  own tab.
- Added standalone, non-Shiny example scripts under `inst/scripts/`
  (`manual_pipeline_microarray.R`, `manual_pipeline_rnaseq.R`, and
  `_multi` variants for 2+ datasets) that run the full pipeline -
  download through PPI - step by step using the same package functions
  as the app.

# GExPipe 0.99.105

- Fixed RNA-seq download failing with "no usable RNA-seq count matrix"
  (or "unused argument (path = ...)" in the log): the internal count-file
  reader called `data.table::fread()` with a `path` argument that function
  does not have (it takes `input`). This broke reading of every downloaded
  RNA-seq count file, direct NCBI counts and GEO supplementary files alike.
- Fixed a related crash ("cannot allocate vector of size ...") in the
  GEO-supplementary per-sample-file merge fallback: files with a duplicate
  gene key (e.g. Excel-date-corrupted symbols like `MARCH1` -> `01-Mar`)
  were merged with `all = TRUE` without deduplicating first, so each
  duplicate's row count multiplied across every per-sample file merged in
  sequence. Both direct (NCBI) and supplementary RNA-seq download paths are
  now verified working end to end.

# GExPipe 0.99.104

- Step 1 no longer crashes after gene-symbol mapping when microarray and
  RNA-seq studies have different gene counts (`cbind` row mismatch). The
  combined matrix is aligned to shared symbols; Parallel keeps each GSE
  at full coverage. The download log no longer calls this a network reset.

# GExPipe 0.99.103

- DESCRIPTION, README, and vignette document the four analysis types
  (RNA-seq only, microarray only, Merged (Both), Parallel DE then merge)
  and that each RNA-seq and microarray box accepts one or more GSE IDs
  in the same run. Parallel is not coerced to Merged when both boxes
  have IDs. Parallel download keeps each platform's genes
  (`keep_platforms_separate`).

# GExPipe 0.99.102

- Merged and Parallel keep every GSE typed in each box (several RNA-seq
  and several microarray studies in one run). "Single dataset" only
  limits RNA-only or microarray-only.

# GExPipe 0.99.101

- Step 11 ML Venn for 5 selected methods draws again. The old rotation=
  argument crashed VennDiagram, so the panel stayed blank.

# GExPipe 0.99.100

- After Step 1 download, the app stays clickable. Parallel Apply Normalization
  runs itself (it no longer clicks a hidden button). Extra GEO phenodata
  fetches wait until Step 4. Pipeline progress no longer crashes if the DE
  method radio is empty.

# GExPipe 0.99.99

- Step 7 count cards keep full labels and a key: Common = Venn center
  (both platforms); Consensus = same-direction list that Apply stores.
  Step 9 uses the same icons so it is not confused with Step 7 Common.

# GExPipe 0.99.98

- Parallel Step 9 labels the overlap as Consensus DE ∩ WGCNA modules
  (not generic DEGs). The Venn circles use those names.

# GExPipe 0.99.97

- Parallel WGCNA always shows RNA-seq vs microarray (or Auto = more
  samples). Auto no longer hides that choice inside Manual.

# GExPipe 0.99.96

- Parallel Step 6 has separate LogFC, adj. P, and heatmap-gene cutoffs
  for RNA-seq (left) and microarray (right). One Run DE still starts both.

# GExPipe 0.99.95

- Parallel Step 6: Run DE is no longer greyed out if Step 5 was skipped.
  Clicking it uses each platform's normalized matrix (RNA-seq count DE
  still uses raw counts). Next: RNA-seq ∩ microarray is on the DE page.

# GExPipe 0.99.94

- Step 5 (batch) in Parallel now shows Apply and Next: Differential
  Expression. Those controls were trapped in the non-Parallel panel.

# GExPipe 0.99.93

- Next buttons now switch the sidebar tab. After Step 2, Next: QC &
  Visualization opens QC (including Parallel). The same Next click
  works on every later step.

# GExPipe 0.99.92

- Later steps (9-16) show which of the four analysis types is active and
  where DEGs come from. Step 7 is hidden unless Parallel. Welcome names
  the four types. GO / KEGG / volcano / Venn use a publication theme;
  Venn PNG exports at 300 dpi.

# GExPipe 0.99.91

- Step 7 Auto keeps same-direction RNA-seq ∩ microarray DEGs for Step 9
  (not WGCNA). Step 8 Auto builds one network on the platform with more
  samples (RNA VST or microarray after batch) using the top 5,000
  variable genes. Manual can change those choices.

# GExPipe 0.99.90

- Parallel Step 6 runs two DE engines with one click. Auto: microarray
  limma; RNA-seq DESeq2 (or the Step 1 choice). Manual picks the RNA
  engine. Count DE uses RNA genes/samples only (no microarray mix).

# GExPipe 0.99.89

- Parallel Step 5 uses a separate batch method per platform (one Apply).
  Auto: microarray ComBat-ref; RNA-seq limma if DESeq2/edgeR/voom, ComBat-ref
  if limma DE. Manual shows both radios. No joint ComBat.

# GExPipe 0.99.88

- Step 2 features now follow analysis type: RNA-seq skip vs TMM by DE
  method; microarray always platform Auto/Manual; Merged = per-study
  methods + common genes + global quantile on; Parallel stays separate
  with no intersection and no global quantile.

# GExPipe 0.99.87

- Step 2 Auto is the default and follows the Step 1 DE method (skip RNA-seq
  TMM for DESeq2/edgeR/voom; TMM or log2 for limma). Manual shows a details
  box and only the radios for the active platform so Apply cannot use the
  wrong RNA scale for count DE.

# GExPipe 0.99.86

- Step 2 Auto follows the Step 1 DE method (DESeq2/edgeR/voom skip RNA-seq
  TMM; limma uses TMM or log2). Manual shows a details box so the chosen
  scale matches the data. RNA-seq count DE stays on the same Apply page.

# GExPipe 0.99.85

- Step 7 is labeled RNA-seq ∩ microarray. Parallel About text now says
  WGCNA is one network on a processed platform matrix (top variable
  genes), then Step 9 overlaps those DEGs with modules.

# GExPipe 0.99.84

- RNA-seq normalization method choices are hidden when DESeq2, edgeR, or
  limma-voom is selected (those engines use raw counts). Microarray
  normalization stays available. RNA-seq methods appear only for limma.

# GExPipe 0.99.83

- Step 7 now shows Common DEGs (bulk RNA-seq ∩ microarray) as its own count
  and labels the Venn center as that overlap.

# GExPipe 0.99.82

- Parallel Step 5 shows two gene-variance histograms at the top (RNA-seq left,
  microarray right). The single combined variance plot stays on Merged only.

# GExPipe 0.99.81

- Parallel Step 4 shows the Groups success banner separately for RNA-seq and
  microarray (own sample, gene, Normal, and Disease counts). The combined
  134-sample card stays on Merged only.

# GExPipe 0.99.80

- Parallel Step 2 draws median/range and distribution-overlap plots separately
  for RNA-seq and microarray. Those diagnostics no longer put both platforms
  on one figure.

# GExPipe 0.99.79

- Parallel DE now has its own two-column UI from download through DE (RNA-seq
  left, microarray right). RNA-seq only, microarray only, and Merged keep the
  previous single-track pages.

# GExPipe 0.99.78

- Parallel Step 5 toast and logs report microarray and RNA-seq batch results
  separately instead of one combined gene × sample total.

# GExPipe 0.99.77

- Parallel DE now keeps platforms separate from download through DE and shows
  a microarray run log and an RNA-seq run log on each of those steps. Download
  no longer intersects gene lists before Step 2.

# GExPipe 0.99.76

- Parallel Step 2 shows two separate run logs (microarray and RNA-seq), each
  with its own method, gene count, and sample count. The combined toast is no
  longer presented as one joint normalization.

# GExPipe 0.99.75

- Parallel DE keeps microarray and RNA-seq separate through normalize, batch,
  and DE (own gene sets; no global quantile; no joint ComBat). Matrices meet
  at Step 7 consensus DEGs only.

# GExPipe 0.99.74

- Parallel DE Step 1 now has two method selectors: microarray is fixed to
  limma; RNA-seq is DESeq2 / edgeR / limma-voom / limma. Both run in Step 6.

# GExPipe 0.99.73

- WGCNA uses top-variable genes on a continuous matrix (RNA-seq VST, or
  microarray normalized values). It no longer subsets to consensus DEGs;
  DEG ∩ modules stays in Step 9. Parallel DE can pick one WGCNA platform.
- Parallel Step 2 keeps microarray normalization visible when DESeq2/edgeR
  is selected; RNA-seq counts stay raw for those engines.

# GExPipe 0.99.72

- Parallel DE now runs Step 5: RNA-seq and microarray are batch-corrected
  separately (platform-standard methods; a single GSE is only filtered).
  Datasets are not merged until common DEGs in Step 7. Merged (Both) still
  uses one shared batch correction then one limma DE.

# GExPipe 0.99.71

- New Step 1 option **Parallel DE, then merge**: RNA-seq and microarray run
  their own DE in parallel (RNA-seq = DESeq2 / edgeR / limma-voom / limma;
  microarray = limma), then Step 7 keeps common same-direction DEGs and
  merge/batch runs after that. **Merged (Both)** and single-platform runs are
  unchanged from the previous app (batch then one DE).

# GExPipe 0.99.70

- Merged RNA-seq + microarray: new Step 1 option **Separate DE first, then
  common genes, then merge**. After groups, Step 6 runs the two platform DEs,
  Step 7 keeps the overlap, and batch/merge runs after that for WGCNA onward.
  The previous merge-first path remains available.

# GExPipe 0.99.69

- Merged RNA-seq + microarray: Step 6 runs separate DE on each platform
  (RNA-seq uses the method chosen in Step 1; microarray always uses limma).
  New Step 7 keeps genes significant on both platforms with the same
  direction. WGCNA and later steps use that consensus list.

# GExPipe 0.99.68

- If port 3838 is already in use, stop leftover httpuv servers in this R
  session or switch to the next free port so runGExPipe() can start.

# GExPipe 0.99.67

- Reorder analysis tabs: Step 2 is per-dataset normalization (platform table:
  RMA, neqc, Agilent normexp, log2+quantile, TMM, log2(x+1)); Step 3 is QC,
  outlier removal with re-normalization, and common-gene checks.
- Merged RNA-seq + microarray: drop genes low on either platform, then optional
  global quantile (on by default).

# GExPipe 0.99.66

- Microarray Step 1: if GEOquery returns an ExpressionSet with an empty assay
  (GSE89076 on Windows), parse the NCBI series-matrix table into a real
  ExpressionSet so merged RNA-seq + microarray downloads keep both series.
- Fast download (default): NCBI RNA-seq counts and microarray series-matrix
  first; skip GEO RAW.tar, extra GPL HTTP, and phenodata enrich during Step 1.
  Keep NCBI `*_raw_counts_*_NCBI.tsv.gz` and microarray series-matrix files
  across runs.

# GExPipe 0.99.63

- RNA-seq: download NCBI `rnaseq_counts` TSV directly (skip GEOquery's broken
  `getRNASeqQuantResults()` row.names join). Reject HTML/captcha files saved
  with a `.gz` name. Fixes GSE50760 and similar series.
- Step 1 always shows RNA-seq and microarray GSE boxes. IDs in both boxes run
  merged analysis (normalize each study, then merge common genes). Default
  platform is Merged (Both).

# GExPipe 0.99.62

- Analysis order: keep full per-GSE matrices after download; normalize each
  study on its native scale; merge on common genes; then batch-correct.
  Global quantile is off by default and is skipped for mixed microarray +
  RNA-seq and for count-based DE. QC outliers are flagged within each dataset.

# GExPipe 0.99.61

- RNA-seq speed: fetch NCBI counts before metadata and RAW.tar; skip supplementary
  when NCBI matrix is valid; cap broad per-sample scan; silence fread quoting noise.

# GExPipe 0.99.60

- RNA-seq: restore 0.99.53 download order (metadata + NCBI before GEO supp), reject
  tiny supplementary tables, and skip HTML/captcha cached files. Fixes GSE50760
  regression while keeping GSE89076 microarray support.

# GExPipe 0.99.59

- RNA-seq: reject tiny GEO supplementary tables (<500 genes); always prefer NCBI
  `rnaseq_counts` via GEOquery. Fixes GSE50760 picking a 3-row metadata table.

# GExPipe 0.99.58

- RNA-seq: use GEOquery `getRNASeqQuantResults()` / NCBI `rnaseq_counts` first for
  any human/mouse GSE with SRA data (fixes GSE50760 and similar RAW.tar-only series).

# GExPipe 0.99.57

- RNA-seq download: support more GEO layouts for any GSE — broader supplementary
  file patterns, merge per-sample TXT/TSV from RAW.tar, always try NCBI
  `rnaseq_counts` fallback, and clearer errors when only FPKM/processed files exist.

# GExPipe 0.99.56

- Faster GEO downloads: reuse `micro_data`/`rna_data` cache by default (no wipe each run),
  skip redundant phenodata enrichment and CEL supplementary files during download,
  defer NCBI count fetch when GEO supplementary already has counts, and enrich
  phenodata on the Groups tab instead of blocking Step 1. Control via
  `options(gexpipe.fast_download = TRUE)` and `options(gexpipe.clear_download_cache = TRUE)`.

# GExPipe 0.99.55

- Microarray download: use `getGPL = FALSE` (via `.gexpipe_getgeo_series()`) so series
  like GSE89076 work when a corrupt/HTML GPL cache would otherwise break `getGEO()`.

# GExPipe 0.99.54

- `runGExPipe()`: do not `pkgload::load_all()` from an older source checkout when a
  newer GExPipe is already installed (fixes Shiny using stale code from `E:/GExPipe`).

# GExPipe 0.99.53

- Microarray GEO download: support `RangedSummarizedExperiment` from newer GEOquery
  (not only `ExpressionSet`); fix series-matrix fallback search path and tab separator
  when supplementary parsing is needed.

# GExPipe 0.99.52

- Microarray GEO download: handle SummarizedExperiment series matrices via
  `.gexpipe_geo_expr_matrix()`, `.gexpipe_geo_fdata()`, and `.gexpipe_geo_annotation()`
  helpers so expression, feature metadata, and platform IDs work for both
  ExpressionSet and SummarizedExperiment objects.

# GExPipe 0.99.49

- Step 4 Phenodata Browser: show the full GSE phenodata table immediately (with column list),
  and run GEO enrich only after the UI flush so the browser and column selector no longer
  stay blank while NCBI metadata is fetched. Column selection no longer waits on normalization.

# GExPipe 0.99.48

- Phenodata Browser: skip reactiveValues write-back when enrich does not change the table,
  and isolate thin-only enrich in renderUI/DT so badge/DT no longer re-enter on every flush
  (fixes stuck "Columns: 1" after successful enrich).

# GExPipe 0.99.47

- Phenodata enrich: case-insensitive GSE key write-back so enriched columns update the RNA/micro list the browser actually reads (not a mismatched micro stub).
- Replace whole metadata lists when storing enrich results (reactiveValues-safe).
- Treat title/geo_accession-only tables as thin; warn when enrich cannot add columns.
- Re-enrich thin phenodata lists before setting download_complete so Groups badge/DT see rich columns.

# GExPipe 0.99.44

- Remove all `requireNamespace("rmda")` / `rmda` code paths from the nomogram DCA module (fixes R CMD check WARNING about undeclared dependency).
- Normalize `NEWS.md` section titles to `# GExPipe x.y.z` so R can parse version history.
# GExPipe 0.99.43

- Correct co-author names to **Naeem Mahmood Ashraf** and **Prof. Dr. Muhammad Farooq Sabar**.
- Remove unavailable Suggests package `rmda` (Bioconductor CHECK ERROR). Nomogram DCA uses `dcurves` (already preferred in code).

# GExPipe 0.99.40
### BiocCheck
- Rename GSEA map field `cat` to `msig_category` so BiocCheck no longer flags a false `cat()` hit.
- Add maintainer ORCID (`0000-0003-2646-8106`) in Authors@R.

# GExPipe 0.99.39
### Authors
- Add co-authors Naeem Mahmood Ashraf and Prof. Dr. Muhammad Farooq Sabar; Safa Rafique remains maintainer (`cre`).

# GExPipe 0.99.38
### Bioconductor NOTES cleanup
- Move optional feature packages from Imports to Suggests: Boruta, car, cicerone,
  corrplot, dcurves, kernlab, mixOmics, SHAPforxgboost (use requireNamespace guards).
- Prefer seq_len/seq_along; replace cat()/redundant stop-warn prefixes in Shiny servers.
- Treat Suggests packages as optional during attach/bootstrap (core Imports remain required).

# GExPipe 0.99.37
### Package hygiene
- Exclude and untrack `GExPipe(original_paper)/` from the Bioconductor package tree (`.Rbuildignore` + `.gitignore`).

# GExPipe 0.99.36
### SPB NOTES cleanup (reviewer request)
- Expand NAMESPACE `importFrom` for grDevices/graphics/stats/utils/shiny/ggplot2/DT/grid.
- Expand `utils::globalVariables()` for NSE column names and Shiny symbols.
- Replace `sapply()` with `vapply()`; prefer `seq_len()` / `sample.int()`.
- Replace ggplot `print(p)` with returning `p` in `renderPlot`.
- Remove `<<-` via env boxes / reactiveValues assignment.
- Fix `gexp_fetch_geo_series_matrix_metadata` call in validation server.

# GExPipe 0.99.35
### Bioconductor check warnings
- Replace non-ASCII characters in R/ with ASCII equivalents.
- Declare Suggests: bslib, crosstalk, devtools, fontawesome, htmltools, htmlwidgets, rmda.
- Replace `set.seed()` with `withr::local_seed()` / `withr::with_seed()` (BiocCheck).

# GExPipe 0.99.34
### SPB / R CMD check
- Fix `gexp_qc_build_sample_dataset_map` man page example matrix dimensions.

# GExPipe 0.99.33
### Documentation
- Clarify `gexp_align_rnaseq_sample_names()` runs after GEO download (GSE ID workflow), not manual data entry; fix example matrix dimensions in man page.

# GExPipe 0.99.32
### SPB / R CMD check
- Regenerate `man/gexp_align_rnaseq_sample_names.Rd` example (fixes examples ERROR).

# GExPipe 0.99.31
### SPB / R CMD check fixes
- Fix `gexp_align_rnaseq_sample_names()` example matrix dimensions.
- Skip source-tree-only bioc-review tests when `R/` is not in installed layout.
- Harden server namespace test; avoid false match on `inst/shinyapp/server.R`.
- Remove `install.packages()` from GitHub bootstrap (BiocCheck compliance).

# GExPipe 0.99.30
### Tests
- Fix `test-bioc-review.R` shinytest2 readme path for `covr` / installed-package test runs.

# GExPipe 0.99.29
### Bioconductor second-review response
- Vignette: 30 end-user screenshots in `vignettes/images/`; maintainer-only notes removed.
- Step 4: `title` column fallback for poorly annotated GEO series; optional group rename at Group Summary.
- DE/ML contrasts respect custom reference/comparison labels.

# GExPipe 0.99.28
### Shinytest2 readiness signal
- Inject `shinytest2::use_shinytest2()` in test mode so `window.shinytest2.ready` is set for AppDriver.

# GExPipe 0.99.27
### Shinytest2 GEO download scenario
- Added tests: empty GSE validation and GSE ID + Start Processing (`GSE62646` by default).
- Helpers: `.gexpipe_shinytest2_poll_output()`, `.gexpipe_shinytest2_start_geo_download()`.
- Added `inst/scripts/record-shinytest2-geo.R` for interactive recording.

# GExPipe 0.99.26
### Shiny testing (Bioconductor review)
- Added `shinytest2` workflow tests (`tests/testthat/test-shiny-integration.R`) and
  `helper-shinytest2.R` for welcome → dashboard → QC navigation.
- Skip full Bioconductor attach in `shiny.testmode` so `shinytest2` sessions start quickly.
- Documented usage in `inst/scripts/README-shinytest2.md`.

# GExPipe 0.99.25
### Bioconductor review (second round)
- Moved Shiny bootstrap from `inst/shinyapp/global.R` into `R/gexpipe_shinyapp_bootstrap.R`.
- Replaced all `suppressWarnings()` / `suppressMessages()` in `R/` with targeted quiet I/O helpers.
- Added `tests/testthat/test-shiny-coverage.R` and expanded bioc-review / app-builder tests for UI tabs, `utils_shiny_app`, and `dummy_imports`.
- Fixed `.gexpipe_best_version()` for R 4.6+ (`package_version` comparison).

# GExPipe 0.99.24
### Vignette (Bioconductor review)
- Removed maintainer-only text from `vignettes/GExPipe.Rmd` (screenshot paths,
  internal vignette notes).
- Moved walkthrough screenshots to `vignettes/images/` with direct
  `knitr::include_graphics()` calls.
- Added five PNG figures referenced by the vignette; maintainer regeneration
  documented in `inst/scripts/README-vignette-screenshots.md`.

# GExPipe 0.99.23
### Bioconductor review (code organization and testing)
- `inst/shinyapp/server.R` and `ui.R` now delegate to `gexp_app_server()` / `gexp_app_ui()`
  instead of duplicating modular logic or calling `source()` on tab modules.
- Added `gexpipe_spearman_cor()` and removed `suppressWarnings(cor(...))` from ML plots.
- Added `tests/testthat/test-coverage-helpers.R` for normalization, ID detection, WGCNA prep,
  download overlap helpers, and UI/ML utilities.

# GExPipe 0.99.22
### Bioconductor review (testing and code organization)
- Added `tests/testthat/test-pipeline-helpers.R` for download/QC/classify helpers and
  namespace-based server wiring.
- Replaced scattered `suppressMessages(capture.output(getGEO...))` with
  `.gexpipe_geo_quiet()` and centralized count-file reads in `.gexpipe_fread_counts()`.
- Documented remaining suppression (STRINGdb ID mapping, optional biomaRt chatter).

### Shiny functional review
- Fix generic `V2` sample names from headerless GEO count files; per-GSE labels in QC
  outlier plots before normalization.

# GExPipe 0.99.21
### Bioconductor second review
- `BugReports` now points to GitHub Issues (`safarafique/GExPipe`).
- Shiny server and UI tab modules moved from `inst/shinyapp/` to `R/` (no runtime
  `source()` / custom caching for tab modules).
- Added `inst/scripts/make-vignette-extdata.R` documenting synthetic vignette data.
- Removed redundant `inst/pkg_versions.txt` (versions are in `DESCRIPTION`).
- Reduced `suppressWarnings()` around namespace unloads; added tests for UI/server
  builders, helpers, and pipeline wiring.

# GExPipe 0.99.20

