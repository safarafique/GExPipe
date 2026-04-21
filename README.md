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
| 4.4.x | 3.19 / 3.20 | ✅ |
| 4.5.x | 3.21 | ✅ |
| 4.6.x | 3.22 | ✅ |

---

## Installation & Run

### Option 1 — Run directly from GitHub (simplest, no install needed)

Paste **these 2 lines** into R or RStudio on any machine.
Everything else — BiocManager, all Bioconductor + CRAN packages, libraries — installs and loads automatically.

```r
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
shiny::runGitHub("GExPipe", "safarafique", destdir = tempfile())
```

> First run on a fresh machine takes **10–30 min** (downloading ~50 packages).
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

Both options work on **R 4.4, 4.5, and 4.6** — the correct Bioconductor package
versions are selected automatically for your R version.

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

## Package versions (tested with)

A full list of package names and versions is shipped with the package:

- **`inst/pkg_versions.txt`** — tab-separated (package, version).
- **`inst/PACKAGE_VERSIONS.md`** — same info and how to get it from R:  
  `system.file("pkg_versions.txt", package = "GExPipe")`.

Use these when reporting issues or when you need exact versions.

---

## Building for Bioconductor submission

The vignette is R Markdown and requires **Pandoc**. If Pandoc is not installed, build without vignettes:

```bash
R CMD build . --no-build-vignettes
```

The resulting `GExPipe_0.99.7.tar.gz` is valid for submission; vignette source is in `vignettes/`. To build the vignette locally, install [Pandoc](https://pandoc.org/installing.html) and run `R CMD build .` without `--no-build-vignettes`.

### Package check

From the package root:

```bash
R CMD build . --no-build-vignettes
R CMD check GExPipe_0.99.7.tar.gz --no-build-vignettes --no-manual
```

Before submission, fix any **ERROR**s and **WARNING**s from `R CMD check`, then run `BiocCheck("GExPipe_0.99.7.tar.gz")` and address any reported issues.

---

## Contributors

- **Safa Rafique** — author and maintainer
