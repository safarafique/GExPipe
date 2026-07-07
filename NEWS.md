# GExPipe NEWS

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
