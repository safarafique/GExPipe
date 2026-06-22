# GExPipe Package Versions (v0.99.13)

## Supported R versions

GExPipe requires **R >= 4.5.0** (stable release). `BiocManager` selects compatible package
versions for your R release:

- **R 4.5** → Bioconductor 3.21
- **R 4.6** (stable) → Bioconductor 3.22

Minimum versions in `DESCRIPTION` are floors for `BiocManager::install("GExPipe")`.
Installed versions may be newer on your system.

Machine-readable list: `inst/pkg_versions.txt` (tab-separated: `package`, `min_version`).

To compare with packages actually installed in your GExPipe library:

```r
Rscript inst/scripts/dump_installed_versions.R   # from package root; sets GEXPIPE_ROOT if needed
```

That writes `inst/pkg_versions_installed.local.txt` (gitignored). Run `Rscript inst/scripts/check_pkg_alignment.R` to verify `DESCRIPTION`, `pkg_versions.txt`, and install lists agree.

---

## Imports (required at runtime)

See `DESCRIPTION` for the authoritative list. All packages below are installed and
loaded when you run `GExPipe::runGExPipe()`, `gexpipe_setup()`, or `inst/shinyapp/global.R`.

Includes **affy** and **oligo** for microarray CEL RMA normalization, and **Matrix**,
**Rcpp**, **withr**, **pillar** (common version-conflict dependencies).

---

## Suggests (development / checks only)

| Package     | Use |
|-------------|-----|
| BiocCheck   | Bioconductor submission |
| BiocManager | Recommended for install (also auto-installed at app start) |
| BiocStyle   | Vignettes |
| chromote    | Headless testing |
| knitr       | Vignettes |
| pak         | Dev tooling |
| pkgload     | Dev / tests |
| rmarkdown   | Vignettes |
| shinytest2  | Shiny integration tests |
| testthat    | Unit tests |

---

## Report issues

```r
read.delim(system.file("pkg_versions.txt", package = "GExPipe"))
sessionInfo()
```
