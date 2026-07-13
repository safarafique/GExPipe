# Table 5. Cross-platform panel transfer (JPCT M3)

Train on one assay, test on the other after joint harmonization + limma batch correction.
Cohorts: GSE89076 (Microarray, n=80) ↔ GSE50760 (RNA-seq, n=36).
Matched panel size target k=20. Seed=42.

**Primary:** mean of RF transfer AUCs (Micro→RNA and RNA→Micro).

| Panel | k | Med. gene AUC (RNA) | RF Micro→RNA | RF RNA→Micro | **RF mean** | glmnet mean* |
|-------|---|---------------------|--------------|--------------|-------------|--------------|
| Merged_limma_topk | 19 | 0.884 | 0.952 | 0.967 | 0.960 | 0.899 |
| Separate_intersect_topk | 19 | 0.884 | 0.952 | 0.967 | 0.960 | 0.899 |
| Same_in_both_topk | 19 | 0.884 | 0.952 | 0.967 | 0.960 | 0.899 |
| Only_in_merged_topk | 13 | 0.846 | 0.952 | 0.967 | 0.960 | 0.899 |
| Only_in_separate_topk | 14 | 0.790 | 0.952 | 0.967 | 0.960 | 0.899 |
| JPCT_consensus_overlap | 19 | 0.941 | 0.952 | 0.967 | 0.960 | 0.899 |
| Random_matched_k | 20 | 0.659 | 0.952 | 0.967 | 0.960 | 0.899 |

## Headline (biological transfer)

- **Merged limma top-20** RF mean transfer = 0.960.
- **Separate∩ top-20** RF mean transfer = 0.960.
- **Only-in-merged top-20** RF mean transfer = 0.960 (genes ∩ would discard).
- **Random** RF mean transfer = 0.960.

JPCT prediction: merged / same-in-both / only-in-merged should transfer better than random;
merged should match or beat separate∩ at matched k if joint estimand captures shared biology.

\* glmnet mean = average of both transfer directions.
Generated: 2026-07-12 10:48:16 UTC
