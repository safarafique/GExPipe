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
