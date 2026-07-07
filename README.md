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

### Option 1 — Run directly from GitHub (always latest `main`)

Paste into R or RStudio. Each run clones the current GitHub `main` branch.

```r
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
shiny::runGitHub("safarafique/GExPipe", destdir = tempfile())
```

> First run on a fresh machine takes **10–30 min** (downloading dependencies).
> After an **R upgrade**, restart R once (`Ctrl+Shift+F10` in RStudio) before the first run.

---

### Option 2 — Install once, run every session (recommended)

**Install or update** (use `force = TRUE` when updating to the latest GitHub code):

```r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github(
  "safarafique/GExPipe",
  force = TRUE,
  upgrade = "always",
  INSTALL_opts = "--no-staged-install"
)
```

**Restart R once** after install or update (RStudio: **Ctrl+Shift+F10**).  
This avoids Windows `glmnet` DLL lock issues and ensures PVCA / ML helpers load correctly.

```r
app <- GExPipe::runGExPipe()      # Step 1: build app (check console for "glmnet OK")
shiny::runApp(app, port = 3838L)  # Step 2: start server
```

**Every later session** (no reinstall unless you want updates):

```r
app <- GExPipe::runGExPipe()
shiny::runApp(app, port = 3838L)
```

**To pull the latest fixes from GitHub** into an existing install:

```r
remotes::install_github("safarafique/GExPipe", force = TRUE, upgrade = "always",
                        INSTALL_opts = "--no-staged-install")
# then Ctrl+Shift+F10 and runGExPipe() again
```

Both options require **R >= 4.5.0** (stable). Bioconductor **3.21** (R 4.5) or **3.22** (R 4.6 stable) is selected automatically.

### From Bioconductor (when accepted)

```r
BiocManager::install("GExPipe", dependencies = TRUE)
app <- GExPipe::runGExPipe()
shiny::runApp(app, port = 3838L)
```

Dependencies are installed at package install time. Runtime auto-install is **off** by default for Bioconductor installs.

For GitHub installs into your normal R library, enable first-run auto-install if needed:

```r
options(gexpipe.auto_install = TRUE)
app <- GExPipe::runGExPipe()
shiny::runApp(app, port = 3838L)
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

Runtime dependencies are declared in **`DESCRIPTION`** (`Imports` / `Suggests`, with minimum versions where specified). The app also uses **affy** and **oligo** for microarray CEL RMA when supplementary CEL files are available.

To list installed versions for packages declared in `DESCRIPTION`:

```r
desc <- read.dcf(system.file("DESCRIPTION", package = "GExPipe"))
pkgs <- sub("\\s*\\(.*", "", strsplit(desc[1, "Imports"], ",\\s*")[[1]])
pkgs <- pkgs[pkgs != "parallel"]
sapply(pkgs, function(p) as.character(packageVersion(p)))
```

Use `sessionInfo()` when reporting issues.

---

## Building for Bioconductor submission

The vignette is R Markdown and requires **Pandoc**. If Pandoc is not installed, build without vignettes:

```bash
R CMD build . --no-build-vignettes
```

The resulting `GExPipe_0.99.14.tar.gz` is valid for submission; vignette source is in `vignettes/`. To build the vignette locally, install [Pandoc](https://pandoc.org/installing.html) and run `R CMD build .` without `--no-build-vignettes`.

### Package check

From the package root:

```bash
R CMD build . --no-build-vignettes
R CMD check GExPipe_0.99.14.tar.gz --no-build-vignettes --no-manual
```

Before submission, fix any **ERROR**s and **WARNING**s from `R CMD check`, then run `BiocCheck("GExPipe_0.99.14.tar.gz")` and address any reported issues.

---

## Contributors

- **Safa Rafique** — author and maintainer
