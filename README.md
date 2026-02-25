# OmniVerse

Shiny application for bulk RNA-seq and microarray analysis (GEO). Workflow: download, QC, normalization, differential expression, WGCNA, pathway enrichment, PPI, machine learning, and optional immune deconvolution. Dependencies are from CRAN and Bioconductor only.

## Installation

**From source (e.g. after cloning or unpacking):**
```r
install.packages(".", repos = NULL, type = "source")
```

**From Bioconductor (when accepted):**
```r
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("OmniVerse")
```

## Run

```r
library(OmniVerse)
runOmniVerse()
```

Install OmniVerse (e.g. `BiocManager::install("OmniVerse")`) so all dependencies are installed; the app does not install packages at runtime.

## Bioconductor submission checklist

- **High-throughput genomic analysis**: RNA-seq, microarrays (GEO), differential expression, pathways, WGCNA, PPI.
- **Interoperability**: Uses Bioconductor data structures (`ExpressionSet`, `DGEList`, GEOquery, limma, DESeq2, edgeR, clusterProfiler).
- **No runtime installs**: All dependencies are in DESCRIPTION; no `install.packages()` or `BiocManager::install()` in package code.
- **CRAN/Bioconductor only**: Every dependency is on CRAN or Bioconductor (no Remotes, no GitHub-only dependencies in Imports).
- **Documentation**: Vignette (`vignettes/OmniVerse.Rmd`), man pages, NEWS.md.
- **Not on CRAN**: Submit only to Bioconductor (not to CRAN).
- Before submitting: run `R CMD check` then `BiocCheck("<tarball_or_dir>")` and address ERRORs and WARNINGs.

## Building for Bioconductor submission

The vignette is R Markdown and requires **Pandoc** to build. If Pandoc is not installed, build with:

```bash
R CMD build . --no-build-vignettes
```

The resulting `OmniVerse_0.1.0.tar.gz` is valid for submission: the vignette source (`vignettes/OmniVerse.Rmd`) is included and Bioconductor will build it on their servers (they have Pandoc). To build the vignette locally as well, install [Pandoc](https://pandoc.org/installing.html) and run `R CMD build .` without the flag.

From the package root you can also run the script:
```bash
.\build_for_bioc.ps1
```
This creates the submission tarball and prints its path.

## Package check

From the package root (directory containing `DESCRIPTION`):

```bash
R CMD build . --no-build-vignettes
R CMD check OmniVerse_0.1.0.tar.gz --no-build-vignettes --no-manual
```

Resolve any ERRORs or WARNINGs before submission. NOTEs may be acceptable.

## Optional

Immune deconvolution can use the package `immunedeconv` if installed by the user (it is not on CRAN/Bioconductor). The app runs fully without it.
