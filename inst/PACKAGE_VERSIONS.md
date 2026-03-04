# Package versions for GExPipe (R 4.5.2)

## Tested and specified for R >= 4.5.0 (best with R 4.5.2)

The `DESCRIPTION` file uses **minimum version** constraints (e.g. `shiny (>= 1.13.0)`) so that installs via `BiocManager::install("GExPipe")` pull versions known to work with **R 4.5.2** and Bioconductor 3.22. The table below lists the versions used when setting those minimums; newer compatible versions from CRAN/Bioconductor should work.

| Package         | Version (≥ in DESCRIPTION) |
|-----------------|-----------------------------|
| AnnotationDbi   | 1.72.0                      |
| Biobase         | 2.70.0                      |
| caret           | 7.0.0                       |
| circlize        | 0.4.17                      |
| clusterProfiler | 4.18.0                      |
| corrplot        | 0.95                        |
| data.table      | 1.18.0                      |
| DESeq2          | 1.50.0                      |
| dplyr           | 1.2.0                       |
| DT              | 0.34                        |
| dynamicTreeCut  | 1.63.1                      |
| e1071           | 1.7.17                      |
| edgeR           | 4.8.0                       |
| enrichplot      | 1.30.0                      |
| GEOquery        | 2.78.0                      |
| ggplot2         | 4.0.0                       |
| ggpubr          | 0.6.0                       |
| ggraph          | 2.2.0                       |
| ggrepel         | 0.9.6                       |
| glmnet          | 4.1.0                       |
| gridExtra       | 2.3                         |
| igraph          | 2.2.0                       |
| kernlab         | 0.9.33                      |
| limma           | 3.66.0                      |
| msigdbr         | 25.0.0                      |
| org.Hs.eg.db    | 3.22.0                      |
| pheatmap        | 1.0.13                      |
| pROC            | 1.19.0                      |
| R.utils         | 2.13.0                      |
| randomForest    | 4.7.0                       |
| RColorBrewer    | 1.1.3                       |
| reshape2        | 1.4.5                       |
| scales          | 1.4.0                       |
| shiny           | 1.13.0                      |
| shinydashboard  | 0.7.3                       |
| shinyjs         | 2.1.0                       |
| STRINGdb        | 2.22.0                      |
| sva             | 3.58.0                      |
| tibble          | 3.3.0                       |
| tidygraph       | 1.3.0                       |
| tidyr           | 1.3.0                       |
| UpSetR          | 1.4.0                       |
| VennDiagram     | 1.8.0                       |
| WGCNA           | 1.74                        |

### Suggests (optional)

| Package        | Version (≥ in DESCRIPTION) |
|----------------|-----------------------------|
| affy           | 1.74.0                      |
| biomaRt        | 2.56.0                      |
| BiocCheck      | 1.43.0                      |
| Boruta         | 8.0.0                       |
| cicerone       | 0.1.0                       |
| knitr          | 1.45.0                      |
| mixOmics       | 6.0.0                       |
| oligo          | 1.62.0                      |
| rmarkdown      | 2.25.0                      |
| rms            | 6.7.0                       |
| rmda           | 1.6.0                       |
| SHAPforxgboost | 0.1.0                       |
| xgboost        | 2.0.0                       |

Install Suggests only if you use those features (e.g. knitr/rmarkdown for vignettes, BiocCheck for submission).

- **Plain list:** `inst/pkg_versions.txt` has the same info in tab-separated form.
- **Record your session:** Use `scripts/generate_session_info.R` in the source tree to generate session/version info; the installed package ships a static `inst/pkg_versions.txt` and `inst/PACKAGE_VERSIONS.md`.
