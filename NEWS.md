# GExPipe NEWS

## Version 0.99.6

- Vignette `GExPipe.Rmd`: GitHub install (`devtools::install_github`), Bioconductor install note for post-acceptance, bundled-data workflow fixes (WGCNA gene IDs, demo label balance, random Shiny port), and reproducibility notes for real vs bundled data.
- Additional vignettes: `gexpipe-getting-started.Rmd`, `gexpipe-user-guide-from-html.Rmd`, and related workflow documentation.

## Version 0.99.5

- Removed `inst/CITATION` until a publication/preprint DOI is available, resolving BiocCheck citation warning for missing DOI.
- Added robust Shiny navigation fix for "Continue to Results Summary" button and ensured pipeline toolbar includes step 15 ("Summary").
- Updated User Guideline contact details and applied code formatting cleanup in `R/` using `styler`.

## Version 0.99.3

- Shiny UI/server orchestration consolidated under `R/` per Bioconductor Shiny guidelines; `runGExPipe()` returns the app object only (no `runApp()` in package source).
- New vignette `gexpipe-shiny-architecture.Rmd` with runnable chunks documenting layout and testing expectations.
- `runGExPipe()` man-page examples now include a non-interactive check-friendly construction line.
- Added `tests/testthat/test-runGExPipe.R`; `tests/testthat.R` trimmed to standard `test_check("GExPipe")`.
- Suggests: `chromote`, `pkgload` (for shinytest2 smoke test and source-based checks).
- PPI/STRINGdb: extended download **timeout** only; do not set `download.file.method = "curl"` (avoids **`curl` nonzero exit status** on some Windows setups). Initialization tries STRING data versions **11.5**, then **11**, then **12** (override via `options(gexpipe.stringdb_try_versions = ...)`).
- Main vignette `GExPipe.Rmd`: expanded post–WGCNA sections (common genes, UpSet, GO BP/MF/CC, KEGG ORA + plots, STRING PPI, ML + importance, ROC, D1/D2 validation holdout, GSEA GO + KEGG, MsigDB).

## Version 0.99.0

- Initial release.
- Shiny app for bulk RNA-seq and microarray analysis (GEO download, QC, normalization, DE, WGCNA, pathways, PPI, ML, optional immune deconvolution).
- All dependencies declared in DESCRIPTION; install via `BiocManager::install("GExPipe")` (no runtime package installation once on Bioconductor).
