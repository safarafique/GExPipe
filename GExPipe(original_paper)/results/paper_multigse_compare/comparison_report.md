# Multi-GSE paper reproduction vs BIOS-Rank

**Reference paper:** Mosharaf et al., *BMC Medical Genomics* (2023).
DOI: [10.1186/s12920-023-01488-w](https://doi.org/10.1186/s12920-023-01488-w)

Published 11 key genes:
`CXCL8`, `CEMIP`, `MMP7`, `CA4`, `ADH1C`, `GUCA2A`, `GUCA2B`, `ZG16`, `CLCA4`, `MS4A12`, `CLDN1`

**GSEs used here (Affy GPL570 subset of that paper):** GSE9348, GSE110224, GSE23878.
(GSE35279 Agilent + GSE50760 RNA-seq omitted for same-platform joint limma.)

Paper-style ∩ significant DEGs (all ranks): **1291** genes.

## Overlap with published 11 KGs

| Method | Hits in 11 KG | Genes shared | Frac of paper KG |
|--------|---------------:|--------------|-----------------:|
| Paper_style_intersect_top20 | 8 | ADH1C;CA4;CLCA4;GUCA2A;GUCA2B;MMP7;MS4A12;ZG16 | 0.727 |
| Joint_limma_top20 | 3 | CEMIP;CLDN1;GUCA2B | 0.273 |
| BIOS_Rank_top20 | 3 | CEMIP;CLDN1;GUCA2B | 0.273 |

## Pairwise top-20 overlaps

| Comparison | n shared |
|------------|---------:|
| BIOS ∩ Joint_limma | 20 |
| BIOS ∩ Paper_style_∩ | 3 |
| Joint_limma ∩ Paper_style_∩ | 3 |

## Verdict

Compare which method recovers more of the **independent published** 11-gene CRC signature
from the same multi-GSE literature context.

Generated: 2026-07-12 17:16:35 UTC
