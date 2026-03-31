# GExPipe NEWS

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
