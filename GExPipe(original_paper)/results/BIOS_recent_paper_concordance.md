# Concordance with recent CRC signature papers (2022–2024)

BIOS-Rank panel from real GSE89076×GSE50760 data vs gene lists published in recent papers
(including studies that used **GSE50760** and/or **GSE9348**).

Universe N=11300; recent-paper union size K=24.
BIOS hits in union=8/20 (0.400); limma=3/20; random mean≈0.032; BIOS emp.P vs random=0.

## Per-paper overlap (BIOS v2 vs limma)

| Paper | Year | BIOS hits | BIOS frac | Limma hits | Limma frac | BIOS genes shared |
|-------|------|----------:|----------:|-----------:|-----------:|-------------------|
| P1_JGO_2022 | 2022 | 2 | 0.100 | 3 | 0.158 | BEST4;MMP7 |
| P1b_JGO_SVM_extra | 2022 | 4 | 0.200 | 3 | 0.158 | BEST4;CLDN1;GUCA2B;MMP7 |
| P2_Genes_2023 | 2023 | 1 | 0.050 | 0 | 0.000 | CA7 |
| P3_BMCMedGenomics_2023 | 2023 | 4 | 0.200 | 0 | 0.000 | CLCA4;CLDN1;GUCA2B;MMP7 |
| P4_SPIB_AQP8_GUCA2B_2023 | 2023 | 3 | 0.150 | 0 | 0.000 | AQP8;GUCA2B;SPIB |
| P5_OTOP2_2022 | 2022 | 3 | 0.150 | 2 | 0.105 | BEST4;GUCA2B;OTOP2 |

## Key agreements (what this means)

- **P4 (SPIB, AQP8, GUCA2B):** recent in-vitro/in-silico co-network — BIOS recovers **all 3**.
- **P2 (CA7 + GSE50760 ML paper):** BIOS recovers **CA7** from the published early-detection trio.
- **P3 (BMC 2023; used GSE9348+GSE50760):** BIOS shares **GUCA2B, CLCA4, CLDN1, MMP7** with their 11 key genes.
- **P1 (LASSO/SVM diagnostic):** BIOS shares **BEST4, MMP7** (and related SVM pool GUCA2B/CLDN1).
- **P5 (OTOP2 + GSE50760):** BIOS includes **OTOP2** (functionally validated on your RNA-seq cohort).

## Verdict

BIOS-Rank is **concordant with independent recent CRC papers**, often more than limma top-20,
especially for mucosa-suppressor signatures (GUCA2B/AQP8/SPIB/CLCA4) and shared GEO cohorts.
This is literature concordance (not claiming those papers used BIOS).

Source table: `data/recent_CRC_paper_signatures.csv`. Generated: 2026-07-12 13:46:26 UTC
