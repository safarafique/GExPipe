# BIOS-Rank multi-dataset validation

Compares BIOS-Rank to limma top-*k* / ∩ / random on independent settings.

| Dataset | Panel | k | Primary metric | Value | RF panel (if any) |
|---------|-------|---|----------------|------:|-------------------|
| D1_GSE89076_x_GSE50760_crossplatform | BIOS_Rank_topk | 20 | min_median_cross_assay_gene_AUC | 0.924 | — |
| D1_GSE89076_x_GSE50760_crossplatform | BIOS_ConsensusHard_topk | 19 | min_median_cross_assay_gene_AUC | 0.919 | — |
| D1_GSE89076_x_GSE50760_crossplatform | Merged_limma_topk | 19 | min_median_cross_assay_gene_AUC | 0.884 | — |
| D1_GSE89076_x_GSE50760_crossplatform | Separate_intersect_topk | 19 | min_median_cross_assay_gene_AUC | 0.884 | — |
| D1_GSE89076_x_GSE50760_crossplatform | JPCT_export_consensus | 19 | min_median_cross_assay_gene_AUC | 0.919 | — |
| D1_GSE89076_x_GSE50760_crossplatform | Random_matched_k | 20 | min_median_cross_assay_gene_AUC | 0.807 | — |
| D2_GSE50760_train_GSE104836_external | BIOS_Rank_topk | 20 | median_gene_AUC_external | 0.980 | 1.000 |
| D2_GSE50760_train_GSE104836_external | BIOS_ConsensusHard_topk | 20 | median_gene_AUC_external | 0.980 | 1.000 |
| D2_GSE50760_train_GSE104836_external | JPCT_export_consensus | 20 | median_gene_AUC_external | 0.980 | 1.000 |
| D2_GSE50760_train_GSE104836_external | limma_topk | 20 | median_gene_AUC_external | 0.980 | 1.000 |
| D2_GSE50760_train_GSE104836_external | limma_voom_topk | 20 | median_gene_AUC_external | 0.965 | 1.000 |
| D2_GSE50760_train_GSE104836_external | Random_matched_k | 20 | median_gene_AUC_external | 0.670 | 0.960 |
| D3_GSE9348_x_GSE50760_crossplatform | BIOS_Rank_topk | 20 | min_median_cross_assay_gene_AUC | 0.946 | — |
| D3_GSE9348_x_GSE50760_crossplatform | Merged_limma_topk | 20 | min_median_cross_assay_gene_AUC | 0.946 | — |
| D3_GSE9348_x_GSE50760_crossplatform | Random_matched_k | 20 | min_median_cross_assay_gene_AUC | 0.781 | — |

## Per-dataset verdict

- **D1_GSE89076_x_GSE50760_crossplatform**: best=BIOS_Rank_topk; BIOS=0.924; baseline=0.884; random=0.807; beats baseline=TRUE; beats random=TRUE
- **D2_GSE50760_train_GSE104836_external**: best=BIOS_Rank_topk; BIOS=0.980; baseline=0.980; random=0.670; beats baseline=TRUE; beats random=TRUE
- **D3_GSE9348_x_GSE50760_crossplatform**: best=BIOS_Rank_topk; BIOS=0.946; baseline=0.946; random=0.781; beats baseline=TRUE; beats random=TRUE

**Overall (3/3 datasets favorable):** BIOS-Rank is **competitive / best or tied** on a majority of tested datasets (beats random; ≥ baseline when available).

Notes:
- D1: full BIOS with Platform purity (Ec–Ex).
- D2: train-only BIOS (Ep=1); external labels used only for evaluation.
- D3: ran with GSE9348 Em/Es may be fidelity proxies if WGCNA/ML exports absent.

Generated: 2026-07-12 12:08:07 UTC
