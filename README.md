# GExPipe

**Gene Expression Pipeline** — a Shiny application for high-throughput genomic analysis of bulk RNA-seq and microarray data (e.g. from GEO).

| Resource      | Link |
|---------------|------|
| **Bioconductor** | [bioconductor.org/packages/GExPipe](https://bioconductor.org/packages/GExPipe) |
| **GitHub**       | [github.com/safarafique/GExPipe](https://github.com/safarafique/GExPipe) |
| **Bug reports** | [Bioconductor Support · tag GExPipe](https://support.bioconductor.org/tag/GExPipe) |

---

## Overview

GExPipe provides a single workflow for:

- **Download** — GEO datasets (GEOquery)
- **QC** — sample and gene filtering
- **Normalization** — count/microarray normalization
- **Batch correction** — ComBat, limma
- **Differential expression** — limma / DESeq2 / edgeR
- **WGCNA** — co-expression modules and module–trait correlation
- **Pathway enrichment** — GO, KEGG (clusterProfiler)
- **PPI** — protein–protein interaction (STRINGdb)
- **Machine learning** — LASSO, RF, SVM-RFE, XGBoost, etc.
- **Optional** — immune deconvolution, GSEA, nomogram, ROC

Dependencies are from **CRAN** and **Bioconductor** only.

---

## Requirements

| R version | Bioconductor | Supported |
|-----------|-------------|-----------|
| 4.4.x | 3.19 / 3.20 | ❌ (below minimum) |
| 4.5.x | 3.21 | ✅ (recommended) |
| 4.6.x stable | 3.22 | ✅ |
| 4.6.x alpha/beta | — | ❌ (no binaries available) |

> **Use a stable R release.** Download from
> https://cran.r-project.org/bin/windows/base/

---

## Installation & Run

### Option 1 — Run directly from GitHub (simplest, no install needed)

Paste **these 2 lines** into R or RStudio on any machine.
Everything else — BiocManager, all Bioconductor + CRAN packages, libraries — installs and loads automatically.

```r
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
shiny::runGitHub("GExPipe", "safarafique", destdir = tempfile())
```

> First run on a fresh machine takes **10–30 min** (downloading **63** dependencies from `DESCRIPTION`).
> Every run after that starts in seconds.

---

### Option 2 — Install once, run in seconds every time (Recommended)

**First time only** (installs GExPipe + all dependencies automatically):

```r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("safarafique/GExPipe", INSTALL_opts = "--no-staged-install")
shiny::runApp(GExPipe::runGExPipe(), port = 3838L)
```

**Every session after that** — just 1 line:

```r
shiny::runApp(GExPipe::runGExPipe(), port = 3838L)
```

Both options require **R >= 4.5.0** (stable). Bioconductor **3.21** (R 4.5) or **3.22** (R 4.6 stable) is selected automatically for your R version. R 4.4 and pre-release R builds are not supported.

### From Bioconductor (when accepted)

```r
BiocManager::install("GExPipe")
```

---

## Running in Google Colab (or other cloud notebooks)

In **Google Colab** (or any headless R session) there is no local browser, so you must expose the app on all interfaces and open the URL Colab provides.

Use Option 1 or Option 2 above to install, then start with:

```r
library(GExPipe)
library(shiny)
app <- runGExPipe(launch.browser = FALSE, host = "0.0.0.0", port = 3838L)
shiny::runApp(app, host = "0.0.0.0", port = 3838L)
```

Open port **3838** via Colab’s port-forwarding panel, or run a tunnel (`ngrok http 3838`) and open the HTTPS URL.

---

## Package versions

Runtime dependencies are declared in **`DESCRIPTION`** (`Imports`, minimum versions). The app also installs **affy** and **oligo** for microarray CEL RMA (see `DESCRIPTION`).

Shipped reference files:

| File | Contents |
|------|----------|
| **`inst/pkg_versions.txt`** | Tab-separated **minimum** versions (`package`, `min_version`) — mirrors `DESCRIPTION` |
| **`inst/PACKAGE_VERSIONS.md`** | Human-readable summary and R/Bioconductor notes |

After install, list **your** installed versions (GExPipe library on Windows: `%LOCALAPPDATA%\GExPipe\<R-major.minor>\`):

```r
read.delim(system.file("pkg_versions.txt", package = "GExPipe"))
# Installed versions on this machine:
pkgs <- sub("\\s*\\(.*", "", strsplit(read.dcf(system.file("DESCRIPTION", package = "GExPipe"))[1, "Imports"], ",\\s*")[[1]])
pkgs <- pkgs[pkgs != "parallel"]
lib <- file.path(Sys.getenv("LOCALAPPDATA"), "GExPipe", paste0(R.Version()$major, ".", sub("\\..*", "", R.Version()$minor)))
sapply(pkgs, function(p) as.character(packageVersion(p, lib.loc = lib)))
```

From a package checkout, `Rscript inst/scripts/check_pkg_alignment.R` verifies that `DESCRIPTION`, `pkg_versions.txt`, and the install lists in `R/utils_shiny_app.R` / `inst/shinyapp/global.R` match.

Use `sessionInfo()` and the tables above when reporting issues.

---

## Building for Bioconductor submission

The vignette is R Markdown and requires **Pandoc**. If Pandoc is not installed, build without vignettes:

```bash
R CMD build . --no-build-vignettes
```

The resulting `GExPipe_0.99.8.tar.gz` is valid for submission; vignette source is in `vignettes/`. To build the vignette locally, install [Pandoc](https://pandoc.org/installing.html) and run `R CMD build .` without `--no-build-vignettes`.

### Package check

From the package root:

```bash
R CMD build . --no-build-vignettes
R CMD check GExPipe_0.99.8.tar.gz --no-build-vignettes --no-manual
```

Before submission, fix any **ERROR**s and **WARNING**s from `R CMD check`, then run `BiocCheck("GExPipe_0.99.8.tar.gz")` and address any reported issues.

---

## Contributors

- **Safa Rafique** — author and maintainer
