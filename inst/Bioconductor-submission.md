# Bioconductor submission notes (OmniVerse)

This document summarizes how OmniVerse aligns with Bioconductor package requirements.

## Requirements addressed

1. **High-throughput genomic analysis**  
   RNA-seq and microarray (GEO) analysis: download, QC, normalization, differential expression, WGCNA, pathway enrichment, PPI, ML. See `biocViews` in DESCRIPTION.

2. **Interoperability**  
   Uses Bioconductor data structures and packages:
   - `Biobase::ExpressionSet`, `exprs()`, `fData()`, `GEOquery::getGEO()`, platform annotations
   - `edgeR::DGEList`, `DESeq2::DESeqDataSetFromMatrix()`, limma, clusterProfiler

3. **Reproducibility and support**  
   - Vignette: `vignettes/OmniVerse.Rmd` (workflow and run instructions)
   - Man page: `runOmniVerse()`
   - NEWS.md for version history
   - BugReports/URL point to Bioconductor support and package page

4. **Not on CRAN**  
   Package is intended for Bioconductor only (do not submit to CRAN).

5. **Dependencies**  
   All Imports and Suggests are on CRAN or Bioconductor. No `Remotes:`. No runtime installation: the package does not call `install.packages()` or `BiocManager::install()` in its code; users install via `BiocManager::install("OmniVerse")`.

## Before submitting

- Run `R CMD build .` (optionally with `--no-build-vignettes` if Pandoc is missing).
- Run `R CMD check <tarball> --no-manual` and fix ERRORs/WARNINGs.
- Remove any existing `OmniVerse.BiocCheck` folder from the package root (or it is excluded via `.Rbuildignore`), then run `BiocCheck::BiocCheck("<path_to_tarball>")` and address ERRORs and WARNINGs.
- **Support site:** Register at https://support.bioconductor.org using the **same email** as in `Authors@R` (DESCRIPTION). BiocCheck will ERROR until this is done.
- Ensure package size is within Bioconductor limits (e.g. Software package source &lt; 5 MB if applicable).
- Update BugReports URL if you use a different tracker before acceptance.
