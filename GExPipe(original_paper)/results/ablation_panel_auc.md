# Table 3 (draft). Ablation + matched-k — external panel AUC

Train: GSE50760 (n=36). External: GSE104836 (n=20).
**Primary endpoint:** RF panel AUC (better behaved at small external n). Matched k = |B5| = 20.
Secondary: glmnet panel AUC (often saturates at n≈20). Nested CV = 5-fold on train only.
Bootstrap 95% CI: 500 resamples on external RF scores. Seed = 42.

| Signature | Definition | k | Med. gene AUC | **RF panel AUC (95% CI)** | NestedCV RF | glmnet panel |
|-----------|------------|---|---------------|---------------------------|-------------|--------------|
| B0_limma_top50 | limma top-50 | 50 | 0.980 | 1.000 (1.000–1.000) | 0.988 | 1.000 |
| B1_limma_voom_top50 | limma-voom top-50 | 50 | 0.950 | 1.000 (1.000–1.000) | 0.978 | 0.980 |
| B0k_limma_matched_k | limma top-20 (matched k) | 20 | 0.970 | 1.000 (1.000–1.000) | 0.994 | 1.000 |
| B1k_limma_voom_matched_k | limma-voom top-20 (matched k) | 20 | 0.965 | 1.000 (1.000–1.000) | 0.994 | 0.980 |
| B3_DE_intersect_WGCNA | DE ∩ trait-WGCNA (capped) | 200 | 0.930 | 1.000 (1.000–1.000) | 0.975 | 1.000 |
| B4_DE_ML_true | DE ∩ (≥2 ML), no WGCNA | 20 | 0.980 | 1.000 (1.000–1.000) | 0.988 | 0.980 |
| B4leg_ML_after_WGCNA | legacy ML after WGCNA | 20 | 0.980 | 1.000 (1.000–1.000) | 0.991 | 0.980 |
| B5_full_consensus | DE ∩ WGCNA ∩ (≥2 ML) | 20 | 0.980 | 1.000 (1.000–1.000) | 0.991 | 0.980 |
| B6_random_matched_size | random genes (|B5|) | 20 | 0.695 | 0.870 (0.519–1.000) | 0.892 | 1.000 |

True B4 size = 20; Jaccard(B4,B5) = 0.481. Legacy B4≡B5: TRUE.
Full B2 size = 7051; full B3 size = 2479.
See `signatures/B4_note.txt`.

Generated: 2026-07-12 09:59:31 UTC
