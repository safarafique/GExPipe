# GExPipe NEWS

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
