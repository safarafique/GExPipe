# Heterogeneous multi-GSE: paper ∩ vs joint limma vs BIOS-Rank

**Reference paper:** Mosharaf et al., *BMC Medical Genomics* (2023).
DOI: [10.1186/s12920-023-01488-w](https://doi.org/10.1186/s12920-023-01488-w)

Published 11 key genes:
`CXCL8`, `CEMIP`, `MMP7`, `CA4`, `ADH1C`, `GUCA2A`, `GUCA2B`, `ZG16`, `CLCA4`, `MS4A12`, `CLDN1`

**Heterogeneous GSEs:** Microarray GSE9348, GSE110224, GSE23878 × RNA-seq GSE50760.
Joint model: `limma ~ Condition + Platform` (platform scale retained; no wipe-out z-score).

Paper-style ∩ significant DEGs: **1035** genes.
Common genes after merge: **18201**.

## Panel metrics (top-20)

| Method | Hits/11 KG | MinMedian cross-assay AUC | Med AUC Micro | Med AUC RNA |
|--------|------------:|--------------------------:|--------------:|------------:|
| Paper_style_intersect_top20 | 7 | 0.891 | 0.929 | 0.918 |
| Joint_limma_Condition_Platform_top20 | 3 | 0.930 | 0.963 | 0.937 |
| BIOS_API_default_top20 | 8 | 0.924 | 0.941 | 0.934 |
| BIOS_v2_het_ExTrain_top20 | 8 | 0.924 | 0.946 | 0.929 |

## Pairwise top-20 overlaps

| Comparison | n shared |
|------------|---------:|
| BIOS_v2 ∩ Joint_limma | 8 |
| BIOS_v2 ∩ Paper_style_∩ | 11 |
| BIOS_API ∩ Joint_limma | 7 |
| Joint_limma ∩ Paper_style_∩ | 4 |

## Verdict

- **Hits vs published 11 KG:** literature concordance (paper used ∩ + PPI).
- **Med AUC RNA / MinMedian:** heterogeneous transfer (Micro → RNA);
  BIOS_v2 ranks with Ec+Ep+Em+Es+Ex_train (microarray AUC only; RNA held out).

Generated: 2026-07-12 17:32:15 UTC
