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

First run may install Bioconductor and CRAN dependencies; later runs start immediately.

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

Immune deconvolution uses the optional package `immunedeconv` (GitHub). The app runs without it. To enable: `remotes::install_github("omnideconv/immunedeconv")`.
