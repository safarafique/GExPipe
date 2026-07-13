# BIOS-Rank — novel biomarker filter results

**BIOS-Rank** = mean of scaled (Ec, Ep, Em, Es, Ex). k=20.
Primary metric: **min-median cross-assay gene AUC** (not saturated panel RF).

| Panel | k | Med AUC micro | Med AUC RNA | **Min-median cross-assay** | Mean BIOS |
|-------|---|---------------|-------------|---------------------------|-----------|
| BIOS_Rank_topk | 20 | 0.942 | 0.937 | 0.924 | 0.901 |
| BIOS_ConsensusHard_topk | 19 | 0.924 | 0.941 | 0.919 | 0.891 |
| Merged_limma_topk | 19 | 0.966 | 0.884 | 0.884 | 0.570 |
| Separate_intersect_topk | 19 | 0.966 | 0.884 | 0.884 | 0.575 |
| JPCT_export_consensus | 19 | 0.924 | 0.941 | 0.919 | 0.891 |
| Random_matched_k | 20 | 0.877 | 0.827 | 0.807 | 0.462 |

## Components
- **Ec**: Condition evidence (−log10 adj.P × |logFC|)
- **Ep**: Platform purity |β_cond| / (|β_cond|+|β_plat|)
- **Em**: trait-WGCNA membership
- **Es**: ML/consensus stability
- **Ex**: min(AUC_micro, AUC_rna)

Generated: 2026-07-12 10:52:20 UTC
