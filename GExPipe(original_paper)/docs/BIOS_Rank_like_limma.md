# BIOS-Rank like limma — use on ANY dataset

BIOS-Rank is a **general R function**, the same way `lmFit` / `topTable` work on any matrix.  
Paper GSEs (GSE89076, GSE9348, …) were only **examples for validation**, not a limit of the method.

## Install once

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("limma")
```

## Load the function

```r
source("R/biosRank.R")   # from this repo
# or: source("path/to/biosRank.R")
```

## Pattern A — single dataset (any disease, any platform)

```r
library(limma)

## expr: genes x samples (numeric matrix, log-scale)
## Condition: factor with levels Normal, Disease  (your labels)

design <- model.matrix(~ Condition)
fit <- eBayes(lmFit(expr, design))

bios <- biosRank(fit)              # Ep = 1 (single platform)
top  <- topBIOS(bios, n = 20)      # like topTable(fit, n = 20)

print(top)
write.csv(top, "BIOS_top20.csv", row.names = FALSE)
```

## Pattern B — merged microarray + RNA-seq

```r
## Platform: factor Microarray / RNAseq
design <- model.matrix(~ Condition + Platform)
fit <- eBayes(lmFit(expr, design))

bios <- biosRank(fit, coef_condition = 2, coef_platform = 3)
topBIOS(bios, n = 20)
```

## Pattern C — with your own WGCNA / ML gene lists (full BIOS)

```r
bios <- biosRank(
  fit,
  coef_condition = 2,
  coef_platform  = 3,          # or NULL
  module_genes   = wgcna_genes,
  stable_genes   = ml_consensus_genes
)
topBIOS(bios, n = 30)
```

## Change panel size

```r
topBIOS(bios, n = 10)
topBIOS(bios, n = 50)
```

## What stays the same on every dataset

| Step | Always |
|------|--------|
| 1 | Your expression + Disease/Normal labels |
| 2 | `lmFit` + `eBayes` (limma DE) |
| 3 | `biosRank(fit)` |
| 4 | `topBIOS(bios, n = k)` |

Only **your data and labels** change — not the algorithm.

## Minimal toy check

```r
source("R/biosRank.R")
library(limma)
set.seed(1)
expr <- matrix(rnorm(1000 * 20), 1000, 20)
rownames(expr) <- paste0("G", 1:1000)
Condition <- factor(rep(c("Normal", "Disease"), each = 10))
# plant a disease gene
expr["G5", Condition == "Disease"] <- expr["G5", Condition == "Disease"] + 3

fit <- eBayes(lmFit(expr, model.matrix(~ Condition)))
head(topBIOS(biosRank(fit), n = 5))
```
)
