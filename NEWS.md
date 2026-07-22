# GExPipe 0.99.43

- Correct co-author names to **Naeem Mahmood Ashraf** and **Prof. Dr. Muhammad Farooq Sabar**.
- Remove unavailable Suggests package `rmda` (Bioconductor CHECK ERROR). Nomogram DCA uses `dcurves` (already preferred in code).

# GExPipe NEWS

## Version 0.99.40

### BiocCheck
- Rename GSEA map field `cat` to `msig_category` so BiocCheck no longer flags a false `cat()` hit.
- Add maintainer ORCID (`0000-0003-2646-8106`) in Authors@R.

## Version 0.99.39

### Authors
- Add co-authors Naeem Mahmood Ashraf and Prof. Dr. Muhammad Farooq Sabar; Safa Rafique remains maintainer (`cre`).

## Version 0.99.38

### Bioconductor NOTES cleanup
- Move optional feature packages from Imports to Suggests: Boruta, car, cicerone,
  corrplot, dcurves, kernlab, mixOmics, SHAPforxgboost (use requireNamespace guards).
- Prefer seq_len/seq_along; replace cat()/redundant stop-warn prefixes in Shiny servers.
- Treat Suggests packages as optional during attach/bootstrap (core Imports remain required).

## Version 0.99.37

### Package hygiene
- Exclude and untrack `GExPipe(original_paper)/` from the Bioconductor package tree (`.Rbuildignore` + `.gitignore`).

## Version 0.99.36

### SPB NOTES cleanup (reviewer request)
- Expand NAMESPACE `importFrom` for grDevices/graphics/stats/utils/shiny/ggplot2/DT/grid.
- Expand `utils::globalVariables()` for NSE column names and Shiny symbols.
- Replace `sapply()` with `vapply()`; prefer `seq_len()` / `sample.int()`.
- Replace ggplot `print(p)` with returning `p` in `renderPlot`.
- Remove `<<-` via env boxes / reactiveValues assignment.
- Fix `gexp_fetch_geo_series_matrix_metadata` call in validation server.

## Version 0.99.35

### Bioconductor check warnings
- Replace non-ASCII characters in R/ with ASCII equivalents.
- Declare Suggests: bslib, crosstalk, devtools, fontawesome, htmltools, htmlwidgets, rmda.
- Replace `set.seed()` with `withr::local_seed()` / `withr::with_seed()` (BiocCheck).

## Version 0.99.34

### SPB / R CMD check
- Fix `gexp_qc_build_sample_dataset_map` man page example matrix dimensions.

## Version 0.99.33

### Documentation
- Clarify `gexp_align_rnaseq_sample_names()` runs after GEO download (GSE ID workflow), not manual data entry; fix example matrix dimensions in man page.

## Version 0.99.32

### SPB / R CMD check
- Regenerate `man/gexp_align_rnaseq_sample_names.Rd` example (fixes examples ERROR).

## Version 0.99.31

### SPB / R CMD check fixes
- Fix `gexp_align_rnaseq_sample_names()` example matrix dimensions.
- Skip source-tree-only bioc-review tests when `R/` is not in installed layout.
- Harden server namespace test; avoid false match on `inst/shinyapp/server.R`.
- Remove `install.packages()` from GitHub bootstrap (BiocCheck compliance).

## Version 0.99.30

### Tests
- Fix `test-bioc-review.R` shinytest2 readme path for `covr` / installed-package test runs.

## Version 0.99.29

### Bioconductor second-review response
- Vignette: 30 end-user screenshots in `vignettes/images/`; maintainer-only notes removed.
- Step 4: `title` column fallback for poorly annotated GEO series; optional group rename at Group Summary.
- DE/ML contrasts respect custom reference/comparison labels.

## Version 0.99.28

### Shinytest2 readiness signal
- Inject `shinytest2::use_shinytest2()` in test mode so `window.shinytest2.ready` is set for AppDriver.

## Version 0.99.27

### Shinytest2 GEO download scenario
- Added tests: empty GSE validation and GSE ID + Start Processing (`GSE62646` by default).
- Helpers: `.gexpipe_shinytest2_poll_output()`, `.gexpipe_shinytest2_start_geo_download()`.
- Added `inst/scripts/record-shinytest2-geo.R` for interactive recording.

## Version 0.99.26

### Shiny testing (Bioconductor review)
- Added `shinytest2` workflow tests (`tests/testthat/test-shiny-integration.R`) and
  `helper-shinytest2.R` for welcome → dashboard → QC navigation.
- Skip full Bioconductor attach in `shiny.testmode` so `shinytest2` sessions start quickly.
- Documented usage in `inst/scripts/README-shinytest2.md`.

## Version 0.99.25

### Bioconductor review (second round)
- Moved Shiny bootstrap from `inst/shinyapp/global.R` into `R/gexpipe_shinyapp_bootstrap.R`.
- Replaced all `suppressWarnings()` / `suppressMessages()` in `R/` with targeted quiet I/O helpers.
- Added `tests/testthat/test-shiny-coverage.R` and expanded bioc-review / app-builder tests for UI tabs, `utils_shiny_app`, and `dummy_imports`.
- Fixed `.gexpipe_best_version()` for R 4.6+ (`package_version` comparison).

## Version 0.99.24

### Vignette (Bioconductor review)
- Removed maintainer-only text from `vignettes/GExPipe.Rmd` (screenshot paths,
  internal vignette notes).
- Moved walkthrough screenshots to `vignettes/images/` with direct
  `knitr::include_graphics()` calls.
- Added five PNG figures referenced by the vignette; maintainer regeneration
  documented in `inst/scripts/README-vignette-screenshots.md`.

## Version 0.99.23

### Bioconductor review (code organization and testing)
- `inst/shinyapp/server.R` and `ui.R` now delegate to `gexp_app_server()` / `gexp_app_ui()`
  instead of duplicating modular logic or calling `source()` on tab modules.
- Added `gexpipe_spearman_cor()` and removed `suppressWarnings(cor(...))` from ML plots.
- Added `tests/testthat/test-coverage-helpers.R` for normalization, ID detection, WGCNA prep,
  download overlap helpers, and UI/ML utilities.

## Version 0.99.22

### Bioconductor review (testing and code organization)
- Added `tests/testthat/test-pipeline-helpers.R` for download/QC/classify helpers and
  namespace-based server wiring.
- Replaced scattered `suppressMessages(capture.output(getGEO...))` with
  `.gexpipe_geo_quiet()` and centralized count-file reads in `.gexpipe_fread_counts()`.
- Documented remaining suppression (STRINGdb ID mapping, optional biomaRt chatter).

### Shiny functional review
- Fix generic `V2` sample names from headerless GEO count files; per-GSE labels in QC
  outlier plots before normalization.

## Version 0.99.21

### Bioconductor second review
- `BugReports` now points to GitHub Issues (`safarafique/GExPipe`).
- Shiny server and UI tab modules moved from `inst/shinyapp/` to `R/` (no runtime
  `source()` / custom caching for tab modules).
- Added `inst/scripts/make-vignette-extdata.R` documenting synthetic vignette data.
- Removed redundant `inst/pkg_versions.txt` (versions are in `DESCRIPTION`).
- Reduced `suppressWarnings()` around namespace unloads; added tests for UI/server
  builders, helpers, and pipeline wiring.

## Version 0.99.20
