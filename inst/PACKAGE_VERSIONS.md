# GExPipe Package Versions (v0.99.7)

## Supported R versions: 4.4, 4.5, 4.6

The `DESCRIPTION` file uses **minimum version** constraints so that
`BiocManager::install("GExPipe")` pulls versions compatible with your R version:

- **R 4.4** → Bioconductor 3.19 / 3.20
- **R 4.5** → Bioconductor 3.21
- **R 4.6** → Bioconductor 3.22

`BiocManager` selects the correct package versions for your R version automatically.

---

## Imports (required)

| Package         | Minimum version in DESCRIPTION |
|-----------------|-------------------------------|
| AnnotationDbi   | 1.64.0                        |
| Biobase         | 2.62.0                        |
| caret           | 6.0-94                        |
| circlize        | 0.4.16                        |
| clusterProfiler | 4.10.0                        |
| corrplot        | 0.92                          |
| data.table      | 1.15.0                        |
| DESeq2          | 1.42.0                        |
| dplyr           | 1.1.0                         |
| DT              | 0.30                          |
| dynamicTreeCut  | 1.63-1                        |
| e1071           | 1.7.14                        |
| edgeR           | 4.0.0                         |
| enrichplot      | 1.22.0                        |
| GEOquery        | 2.70.0                        |
| ggplot2         | 3.4.0                         |
| ggpubr          | 0.6.0                         |
| ggraph          | 2.2.0                         |
| ggrepel         | 0.9.5                         |
| glmnet          | 4.1.0                         |
| gridExtra       | 2.3                           |
| igraph          | 2.0.0                         |
| kernlab         | 0.9.32                        |
| limma           | 3.58.0                        |
| msigdbr         | 7.5.1                         |
| org.Hs.eg.db    | 3.17.0                        |
| parallel        | (base R — no minimum)         |
| pheatmap        | 1.0.12                        |
| pROC            | 1.18.0                        |
| R.utils         | 2.12.0                        |
| randomForest    | 4.7-1                         |
| RColorBrewer    | 1.1.3                         |
| reshape2        | 1.4.4                         |
| scales          | 1.3.0                         |
| shiny           | 1.8.0                         |
| shinydashboard  | 0.7.2                         |
| shinyjs         | 2.1.0                         |
| STRINGdb        | 2.14.0                        |
| sva             | 3.50.0                        |
| tibble          | 3.2.0                         |
| tidygraph       | 1.3.0                         |
| tidyr           | 1.3.0                         |
| UpSetR          | 1.4.0                         |
| VennDiagram     | 1.7.0                         |
| WGCNA           | 1.72                          |

---

## Suggests (optional)

Install only if you use those features (e.g. `knitr`/`rmarkdown` for vignettes,
`BiocCheck` for submission, `xgboost`/`Boruta` for additional ML methods).

| Package        | Minimum version in DESCRIPTION |
|----------------|-------------------------------|
| affy           | 1.68.0                        |
| biomaRt        | 2.50.0                        |
| BiocCheck      | 1.36.0                        |
| BiocManager    | (any)                         |
| BiocStyle      | (any)                         |
| Boruta         | 8.0.0                         |
| chromote       | 0.1.0                         |
| cicerone       | 0.1.0                         |
| knitr          | 1.43.0                        |
| mixOmics       | 6.0.0                         |
| oligo          | 1.56.0                        |
| pkgload        | 1.3.0                         |
| rmarkdown      | 2.25.0                        |
| rms            | 6.7.0                         |
| rmda           | 1.6.0                         |
| SHAPforxgboost | 0.1.0                         |
| shinytest2     | 0.3.0                         |
| testthat       | 3.0.0                         |
| xgboost        | 1.7.0                         |

---

A machine-readable copy of this table is in `inst/pkg_versions.txt`
(tab-separated: package, version).
