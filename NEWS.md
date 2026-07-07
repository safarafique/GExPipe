# GExPipe NEWS

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
