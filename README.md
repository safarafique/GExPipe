# GExPipe

**Gene Expression Pipeline** — a Shiny application for high-throughput genomic analysis of bulk RNA-seq and microarray data (e.g. from GEO).

| Resource      | Link |
|---------------|------|
| **Bioconductor** | [bioconductor.org/packages/GExPipe](https://bioconductor.org/packages/GExPipe) |
| **GitHub**       | [github.com/safarafique/GExPipe-](https://github.com/safarafique/GExPipe-) |
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

## Installation

GExPipe depends on Bioconductor packages (DESeq2, GEOquery, limma, etc.). Install them first, then install GExPipe from source or from Bioconductor once accepted.

### From local source (clone or download this repo)

```r
# 1. Install BiocManager and Bioconductor dependencies
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c(
  "DESeq2", "GEOquery", "limma", "edgeR", "Biobase", "AnnotationDbi",
  "org.Hs.eg.db", "clusterProfiler", "enrichplot", "STRINGdb", "sva"
), update = FALSE, ask = FALSE)

# 2. Install GExPipe from the package root (directory containing DESCRIPTION)
install.packages(".", repos = NULL, type = "source")
```

If you see *dependency 'X' is not available*, run step 1 again. Do **not** use `install.packages("DESeq2")` — DESeq2 is on Bioconductor.

### From Bioconductor (when accepted)

```r
BiocManager::install("GExPipe")
```

---

## Run the app

```r
library(GExPipe)
runGExPipe()
```

Install GExPipe (e.g. `BiocManager::install("GExPipe")` or from source as above) so all dependencies are available; the app does not install packages at runtime.

---

## Running in Google Colab (or other cloud notebooks)

In RStudio the app can open in your browser. In **Google Colab** (or any headless R session) there is no local browser, so you must expose the app and open the URL Colab provides.

1. **Install dependencies and GExPipe** (see [Installation](#installation) above).

2. **Start the app** (no browser launch, listen on all interfaces):

   ```r
   library(GExPipe)
   runGExPipe(launch.browser = FALSE, host = "0.0.0.0", port = 3838)
   ```

3. **Get the URL:** Use Colab’s port forwarding / Preview for port **3838**, or run a tunnel (e.g. `ngrok http 3838`) and open the HTTPS URL in your browser.

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

The resulting `GExPipe_0.99.0.tar.gz` is valid for submission; vignette source is in `vignettes/`. To build the vignette locally, install [Pandoc](https://pandoc.org/installing.html) and run `R CMD build .` without `--no-build-vignettes`.

### Package check

From the package root:

```bash
R CMD build . --no-build-vignettes
R CMD check GExPipe_0.99.0.tar.gz --no-build-vignettes --no-manual
```

Before submission, fix any **ERROR**s and **WARNING**s from `R CMD check`, then run `BiocCheck("GExPipe_0.99.0.tar.gz")` and address any reported issues.
