# GExPipe NEWS

## Version 0.99.7

### Compatibility
- Extended R version support from R 4.6-only to **R 4.4, 4.5, and 4.6**
  (`Depends: R (>= 4.4.0)`); `BiocManager` selects the correct Bioconductor
  release (3.19–3.22) automatically for the user's R version.

### Dependency fixes
- Fixed five impossible version constraints that blocked installation on all R
  versions: `caret (>= 6.0-94)`, `ggplot2 (>= 3.4.0)`, `msigdbr (>= 7.5.1)`,
  `shiny (>= 1.8.0)`, `data.table (>= 1.15.0)`.
- Lowered all Bioconductor package minimums from Bioc 3.22 to Bioc 3.19
  equivalents so R 4.4 users can install matching versions.
- Moved `corrplot` from `Suggests` to `Imports` (was used as a required package
  in `global.R` but could be skipped in the installed-package path).

### New feature — `gexpipe_setup()`
- Added exported function `gexpipe_setup()` (`R/setup.R`): detects R/Bioc
  version, batch-installs all required and optional packages in one
  `BiocManager::install()` call, reports a pass/fail summary, and optionally
  launches the app. Users on any R version run this once after install.

### Auto-install improvements
- `inst/shinyapp/global.R` rewritten: batch installs all missing packages in a
  single `BiocManager::install()` call (faster than one-by-one), shows a
  4-step progress banner, and reports loaded/missing counts.
- `gexp_app_attach_packages()` in `R/utils_shiny_app.R` updated to use the
  same batch install approach; added `.gexpipe_batch_install()` helper.

### Vignette (`vignettes/GExPipe.Rmd`)
- Fixed critical bug: `results='asis'` added to screenshots chunk (headings
  were rendering as literal text).
- Added GitHub install block as primary path (package not yet on Bioconductor).
- Replaced `require()` with `requireNamespace()` per Bioconductor guidelines.
- Removed redundant `for` loop rebuilding `micro_expr_list`.
- Removed `gsub("^X", "")` dead code on column names.
- Added variance filter before `prcomp()` to prevent `NaN` on zero-variance genes.
- Added `results='asis'`, human-readable screenshot labels, `BiocStyle::CRANpkg()`
  references, and `eval=FALSE` note for the Shiny launch chunk.
- Updated `VignetteIndexEntry` (removed number prefix), `toc_depth: 3`,
  expanded author block.

### Repository cleanup
- Removed dev scripts from root: `app.R`, `patch_nav.R`, `refactor_css.R`,
  `test_app.R`.
- Removed `app/` directory (RStudio project, session files, user data).
- Removed 246 MB of user/session data committed during development:
  `inst/shinyapp/saved_workspaces/` (170 MB),
  `inst/shinyapp/rna_data/` (18 MB),
  `inst/shinyapp/micro_data/` (736 KB),
  `vignettes/micro_data/` (57 MB),
  `vignettes/rna_data/` (496 KB).
- Added `vignettes/micro_data` and `vignettes/rna_data` to `.Rbuildignore`.
- Added `man/gexpipe_setup.Rd` for the new exported function.

## Version 0.99.5
- Vignette `GExPipe.Rmd`: GitHub install (`devtools::install_github`), Bioconductor install note for post-acceptance, bundled-data workflow fixes (WGCNA gene IDs, demo label balance, random Shiny port), and reproducibility notes for real vs bundled data.
- Vignette `GExPipe.Rmd` restructured as a user-facing Shiny walkthrough with clearer step hierarchy, Bioconductor-style app documentation emphasis, and simplified `system.file()` access for bundled `extdata`.
- Removed developer-focused vignette `gexpipe-shiny-testing-ci.Rmd` from package vignettes and removed generated intermediate files from `vignettes/`.
- Vignette `gexpipe-workflow.Rmd` removed; the main getting-started narrative is `GExPipe.Rmd`.
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
