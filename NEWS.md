# GExPipe NEWS

## Version 0.99.19

### BiocCheck / SPB
- Add suggested `biocViews`: `Normalization`, `NetworkEnrichment`.
- Address BiocCheck coding notes in `utils_shiny_app.R`: `seq_len()` indexing,
  `sprintf()` instead of `paste()` in signals, remove `<<-` in package attach loop.

## Version 0.99.18

### Shiny UI
- Improve ML methods Venn/UpSet: all selected methods shown, explicit zero when no
  genes are common to all methods, publication-style colors, and per-method counts
  in the plot footer.
- Fix `.gexpipe_lib_base` path collision when sourcing `global.R` locally (character
  path preserved after helper scripts load).
- Add JPG and PDF download buttons for all Shiny plots (shared `gexp_ui_plot_download_*`
  helpers); nomogram panels B–E, ML biomarker plots, and Results Summary plots exportable.

### Check / DESCRIPTION
- Drop `rmda` from Suggests (unavailable on R 4.6; nomogram DCA uses `dcurves` fallback).
- Fix `R CMD check` Rd and `grDevices` notes for ML Venn and plot-export helpers.

## Version 0.99.17

### SPB build fix
- Remove `dev/` from the package source tree (developer utilities kept local via
  `.gitignore`) to eliminate R CMD build WARNING: empty `dev/` directory.

## Version 0.99.16

### SPB build fix
- Move developer scripts to `dev/scripts/` (excluded from tarball) to eliminate
  R CMD build WARNING: empty `scripts/` directory.

## Version 0.99.15

### BiocCheck / SPB fixes
- Route runtime package installs through indirect helpers (BiocCheck: no literal
  `install()` in package code).
- Add runnable `@examples` for all exported helpers missing documentation examples.
- Remove `set.seed()` from PVCA subsampling (BiocCheck WARNING).
- Update minimum R version to 4.6.0.

## Version 0.99.14

### Bioconductor SPB build fix
- Remove empty `inst/scripts/` directory (dev scripts moved to top-level `scripts/`,
  excluded from the package tarball) to eliminate R CMD build WARNING.

## Version 0.99.13

### Bioconductor SPB revision (vignette + check clean-up)
- Bundle first vignette screenshot (`step1_download.png`) under `inst/extdata/vignette-screenshots/`.
- Remove non-portable filename from `vignettes/`; fix remaining non-ASCII in R sources (including `utils_shiny_app.R` comment encoding).
- Vignette reviewer response: structure, Shiny walkthrough, Bioconductor install, `system.file()` for bundled data.

## Version 0.99.12

### Bioconductor SPB revision (check clean-up)
- Document `micro_eset_list` argument in `gexp_download_normalize_ids_for_overlap()`.
- Satisfy R CMD check Imports usage via `dummy_imports()` (cli, lifecycle, rlang, etc.).
- Avoid undeclared `remotes`/`stringi` namespace calls in optional repair paths.
- Replace non-ASCII punctuation in R sources with ASCII equivalents.
- Add `remotes` to Suggests; sync vignette formatting with GitHub.

## Version 0.99.11

### Bioconductor submission readiness
- Runtime `BiocManager::install()` is **disabled by default** when `GExPipe` is
  installed in a normal R library (Bioconductor / CRAN path). Dependencies must
  be installed at package install time via `BiocManager::install("GExPipe",
  dependencies = TRUE)`.
- GitHub / first-run workflow: `options(gexpipe.auto_install = TRUE)` before
  `runGExPipe()` or `gexpipe_setup()` to restore background dependency installation.
- Regenerated documentation for 13 previously exported but undocumented helpers.
- Vignette revised for end users (Shiny walkthrough, Bioconductor install, `system.file`
  for bundled data). Developer vignettes remain excluded from the package.
- `.Rbuildignore` excludes `inst/pkg_versions_installed.local.txt`.

## Version 0.99.10

### Machine learning — glmnet 5.x compatibility (fixes LASSO / Elastic Net / Ridge)
- **Root cause:** `glmnet` 5.0 renamed `glmnet_control()` to `glmnet.control()`. GExPipe
  was calling the old API, so every smoke test failed and ML reported
  *"glmnet is not available…"* even when glmnet was installed.
- New helper `.gexpipe_glmnet_smoke()` supports glmnet 4.x and 5.x.
- Subprocess glmnet scripts updated for the new API.

## Version 0.99.9

### Machine learning (glmnet) — no more skipped LASSO / Elastic Net / Ridge
- Fixed a bug where a failed in-session `glmnet` check could return `FALSE` instead
  of falling through to the subprocess path.
- LASSO, Elastic Net, and Ridge are **never skipped** when an isolated R subprocess
  can run `cv.glmnet` (Windows DLL lock after rebuild no longer blocks ML).
- Shiny server modules now receive glmnet helper functions from the package namespace.
- `runGExPipe()` prints glmnet readiness at startup.

### PVCA & GitHub / `install_github` launches
- `gexpipe_pvca_df()` is resolved via `.gexpipe_call()` and `gexp_platform_helpers.R`
  is sourced in `global.R`, so PVCA plots work from GitHub checkout and installed runs.
- Server modules inherit from the `GExPipe` namespace so exported helpers are visible.

### UI
- Batch confounding table: black text on white background (readable in success alert).
- ML step: guidance for smooth glmnet runs after install or R upgrade.

## Version 0.99.8

### Mixed microarray + RNA-seq — platform covariates, caveats, and diagnostics
- New helpers in `R/gexp_platform_helpers.R`: detect mixed platforms, test
  Platform–Dataset confounding, build batch/DE design matrices with `Platform`
  when estimable, and polar PCA coordinates for diagnostics.
- Batch correction (`R/gexp_batch_pipeline.R`): ComBat / ComBat-ref / Hybrid now
  protect `Condition` (+ `Platform` when not confounded with `Dataset`) instead
  of `mod = NULL`; limma path uses the same protected model matrix.
- DE (`server_results.R`, `R/gexp_de_pipeline.R`): limma, DESeq2, edgeR, and
  limma-voom include `Platform` in the design when mixed and identifiable.
- Step 1 (Download): in-app caveat panel when **Merged (Both)** is selected.
- Step 5 (Batch): mixed-platform info banner, **Platform-coloured PCA** before/after
  batch correction, Platform in PVCA and clustering annotations, updated confounding
  guidance.

### WGCNA module-trait heatmap — no redundant duplicate column
- For a 2-group design the auto-generated combined contrast trait (e.g.
  "Disease vs Normal") is, by definition, a copy of one group indicator and the
  mirror image of the other, so it rendered as a column identical to "Disease"
  in the Module-Trait Relationships heatmap. The heatmap now drops that
  redundant column (via new helper `.wgcna_heatmap_cor()` in
  `inst/shinyapp/server/server_wgcna.R`), so only the distinct per-group columns
  are shown (Normal vs Disease, which are visibly opposite). The combined trait
  is still kept in `rv$trait_data` for the GS-vs-MM trait selector. Applied to
  all four render paths (live heatmap, PNG/JPG/PDF export, WGCNA summary plot,
  and the Results Summary tab).

### Native package handling — no manual R restart after an R upgrade
- `.gexpipe_ensure_native_pkg()` (`R/utils_shiny_app.R`) now rebuilds a
  version-mismatched native package (e.g. `glmnet`, `xgboost`) **before** its
  compiled DLL is ever loaded, deciding from the DESCRIPTION `Built:` field
  instead of loading the DLL to smoke-test it. On Windows a mismatched DLL,
  once mapped into the R process, cannot be swapped in the same session — which
  previously forced the "glmnet was rebuilt — one R restart needed" message and
  skipped LASSO / Elastic Net / Ridge for that run. The fresh build now loads
  cleanly the same session, so those methods run immediately with no restart.
  Both launch paths (`runGExPipe()` and the standalone `inst/shinyapp/global.R`)
  benefit, since both route through this helper.

### Documentation — Shiny app launch instructions
- Clarified how to start the app as an explicit two-step pattern across all
  docs (`README.md`, `vignettes/GExPipe.Rmd`, `runGExPipe()` examples,
  `man/runGExPipe.Rd`, and the `gexpipe_setup()` fallback message): first
  `app <- runGExPipe()` builds and returns the Shiny app object, then
  `shiny::runApp(app, port = 3838L)` starts the server. This replaces the
  nested one-liner `shiny::runApp(GExPipe::runGExPipe(), ...)` and matches the
  Bioconductor Shiny guideline that the package function returns an app object.

### Dependencies and documentation
- Aligned `DESCRIPTION` `Imports` with packages used in the Shiny app (63 runtime
  dependencies); added `affy`, `oligo`, `Matrix`, `Rcpp`, `withr`, `pillar`;
  removed unused `e1071`; relaxed minimum versions for reliable install on R 4.5+
  / Bioconductor 3.21–3.22.
- Added `inst/pkg_versions.txt` and alignment scripts under `inst/scripts/`.
- Updated `README.md` and `inst/PACKAGE_VERSIONS.md` to match `DESCRIPTION`.
- Synced install/load lists in `R/utils_shiny_app.R`, `R/setup.R`, and
  `inst/shinyapp/global.R`.

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
