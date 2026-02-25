# Package versions used by OmniVerse

## Tested-with versions (core Imports)

The app was tested with the following package versions. Newer versions from CRAN/Bioconductor are expected to work; if you see errors, you can compare your versions to this list.

| Package        | Version   |
|----------------|-----------|
| AnnotationDbi  | 1.72.0    |
| Biobase        | 2.70.0    |
| caret          | 7.0.1     |
| circlize       | 0.4.17    |
| clusterProfiler| 4.18.4    |
| corrplot       | 0.95      |
| data.table     | 1.18.2.1  |
| DESeq2         | 1.50.2    |
| dplyr          | 1.2.0     |
| DT             | 0.34.0    |
| dynamicTreeCut | 1.63.1    |
| e1071          | 1.7.17    |
| edgeR          | 4.8.2     |
| enrichplot     | 1.30.4    |
| GEOquery       | 2.78.0    |
| ggplot2        | 4.0.2     |
| ggpubr         | 0.6.2     |
| ggraph         | 2.2.2     |
| ggrepel        | 0.9.6     |
| glmnet         | 4.1.10    |
| gridExtra      | 2.3       |
| igraph         | 2.2.2     |
| kernlab        | 0.9.33    |
| limma          | 3.66.0    |
| msigdbr        | 25.1.1    |
| org.Hs.eg.db   | 3.22.0    |
| parallel       | 4.5.2     |
| pheatmap       | 1.0.13    |
| pROC           | 1.19.0.1  |
| R.utils        | 2.13.0    |
| randomForest   | 4.7.1.2   |
| RColorBrewer   | 1.1.3     |
| reshape2       | 1.4.5     |
| scales         | 1.4.0     |
| shiny          | 1.13.0    |
| shinydashboard | 0.7.3     |
| shinyjs        | 2.1.1     |
| STRINGdb       | 2.22.0    |
| sva            | 3.58.0    |
| tibble         | 3.3.1     |
| tidygraph      | 1.3.1     |
| tidyr          | 1.3.2     |
| UpSetR         | 1.4.0     |
| VennDiagram    | 1.8.2     |
| WGCNA          | 1.74      |

- **Plain list:** `pkg_versions.txt` (in this folder) has the same info in tab-separated form for scripting or pasting.
- **Optional packages (Suggests):** Boruta, mixOmics, xgboost, SHAPforxgboost, rms, rmda, cicerone, biomaRt, oligo, affy — not required; install if you use those features.

---

## How users get this info

After installing OmniVerse, the files are in the package directory:

```r
# Path to installed package (example)
pkg_dir <- system.file(package = "OmniVerse")
# List versions file
list.files(pkg_dir, pattern = "pkg_versions|PACKAGE_VERSIONS", recursive = TRUE)
# Read tested-with versions
read.delim(system.file("pkg_versions.txt", package = "OmniVerse"), comment.char = "#")
```

Or open the file directly: `system.file("pkg_versions.txt", package = "OmniVerse")`.

---

## What the install update list means

When you run `BiocManager::install("OmniVerse")` or `remotes::install_github("...")`, R may print a long list of package updates (e.g. `vctrs (0.7.0 -> 0.7.1) [CRAN]`). That is R proposing to install or upgrade dependencies. Answer the prompt (`y`/`a`/`n`); you do not need to paste that list anywhere. The app works with the versions that get installed.

To record your own session’s versions, run `source(system.file("script_save_pkg_versions.R", package = "OmniVerse"))` after loading OmniVerse (from package source); that overwrites `inst/pkg_versions.txt` with current versions.
