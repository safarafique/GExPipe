# OmniVerse

Shiny application for bulk RNA-seq and microarray analysis (GEO). Workflow: download, QC, normalization, differential expression, WGCNA, pathway enrichment, PPI, machine learning, and optional immune deconvolution. Dependencies are from CRAN and Bioconductor only.

## Installation

**From GitHub (recommended until on Bioconductor):**  
OmniVerse depends on **Bioconductor** packages (DESeq2, GEOquery, limma, etc.). Install them first, then install OmniVerse:

```r
# 1. Install BiocManager and Bioconductor dependencies
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c(
  "DESeq2", "GEOquery", "limma", "edgeR", "Biobase", "AnnotationDbi",
  "org.Hs.eg.db", "clusterProfiler", "enrichplot", "STRINGdb", "sva"
), update = FALSE, ask = FALSE)

# 2. Install OmniVerse from GitHub
remotes::install_github("safarafique/Shiny_app-OmniVerse-", dependencies = TRUE)
```

If you see "dependency 'X' is not available", run step 1 again (BiocManager will install from Bioconductor). Do **not** use `install.packages("DESeq2")` — DESeq2 is on Bioconductor.

**From Bioconductor (when accepted):**
```r
BiocManager::install("OmniVerse")
```

**From local source (e.g. after cloning):**
```r
# Install Bioconductor dependencies first (see above), then:
install.packages(".", repos = NULL, type = "source")
```

## Run

```r
library(OmniVerse)
runOmniVerse()
```

Install OmniVerse (e.g. `BiocManager::install("OmniVerse")`) so all dependencies are installed; the app does not install packages at runtime.

## Running in Google Colab (or other cloud notebooks)

In **RStudio**, the app can open in your browser. In **Google Colab** (or any headless/cloud R session) there is no local browser, and the app runs on a remote machine, so you must **expose the app and open the URL Colab gives you**.

1. **Install dependencies and OmniVerse** (see Installation above). In Colab you may need to install R and Bioconductor first (e.g. use an R Colab notebook or `rpy2` with R kernel).

2. **Start the app with Colab-friendly options** (no browser launch, listen on all interfaces):
   ```r
   library(OmniVerse)
   runOmniVerse(launch.browser = FALSE, host = "0.0.0.0", port = 3838)
   ```

3. **Get the URL to open the app:**
   - **Colab with port forwarding:** In the Colab UI, use “Port forwarding” or “Preview” for port **3838** (or the port you used). Colab will show a URL like `https://xxxx-3838.proxy.runtime.googleusercontent.com` — open that in your browser.
   - **Manual tunnel (e.g. ngrok):** If Colab doesn’t show a link, in a separate terminal run a tunnel to port 3838 (e.g. `ngrok http 3838`) and open the HTTPS URL ngrok prints.

4. **Why it “doesn’t work” in Colab:** The app process often runs fine, but (a) there is no local browser to open, and (b) the app is bound to `127.0.0.1` by default, so it’s not reachable from outside. Using `launch.browser = FALSE` and `host = "0.0.0.0"` fixes (b); then you must use Colab’s port forwarding or a tunnel to get a URL (step 3).

## Package versions (tested with)

A full list of package names and versions the app was tested with is in the installed package:

- **`inst/pkg_versions.txt`** — tab-separated list (package, version) for scripting or pasting.
- **`inst/PACKAGE_VERSIONS.md`** — same info plus how to get it from R: `system.file("pkg_versions.txt", package = "OmniVerse")`.

Use these when reporting issues or when you need complete info about which packages and versions are used.

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
