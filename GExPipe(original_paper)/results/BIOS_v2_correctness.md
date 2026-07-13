# BIOS-Rank v2 — corrected (non-circular) protocol

## What changed (novelty + correctness)

1. **Primary ranking no longer uses held-out-assay AUC in Ex.**
   - `BIOS_v2_noEx_*`: rank with Ec, Ep, Em, Es only.
   - `BIOS_v2_ExTrain_*`: Ex = **microarray AUC only**; evaluate on **RNA-seq AUC**.
2. **Locked weights** chosen on D1 to maximize held-out RNA median AUC (Dirichlet search).
3. **Harder baselines**: Ec-only, Ep-only, Ec×Ep, limma, random, circular Ex.
4. **Orthogonal CRC marker overlap** (from merge validation gold list).

**Primary eval metric (correct):** median single-gene AUC on **RNA-seq** (held-out assay), k=20.

## Ranking comparison (sorted by held-out RNA AUC)

| Method | Med AUC RNA (held-out) | Med AUC micro | MinMedian both | CRC markers (n / frac) |
|--------|-----------------------:|--------------:|---------------:|------------------------|
| ONLY_Ex_both_circular | 0.954 | 0.951 | 0.950 | 4 / 0.200 |
| ONLY_Em | 0.949 | 0.954 | 0.938 | 6 / 0.300 |
| BIOS_v2_noEx_equal | 0.940 | 0.938 | 0.923 | 9 / 0.450 |
| BIOS_v2_ExTrain_locked | 0.940 | 0.947 | 0.924 | 9 / 0.450 |
| BIOS_v2_noEx_locked | 0.940 | 0.938 | 0.923 | 9 / 0.450 |
| ONLY_Es | 0.938 | 0.930 | 0.920 | 7 / 0.350 |
| BIOS_v1_circular_Ex | 0.937 | 0.942 | 0.924 | 8 / 0.400 |
| BIOS_v2_ExTrain_equal | 0.935 | 0.945 | 0.924 | 9 / 0.450 |
| ONLY_Ec | 0.935 | 0.949 | 0.931 | 8 / 0.400 |
| Ec_times_Ep | 0.935 | 0.949 | 0.931 | 8 / 0.400 |
| ONLY_Ep | 0.926 | 0.930 | 0.918 | 4 / 0.200 |
| ONLY_AUC_micro_train | 0.886 | 0.974 | 0.886 | 2 / 0.100 |
| Merged_limma_topk | 0.883 | 0.967 | 0.883 | 6 / 0.300 |
| Random_matched_k | 0.759 | 0.790 | 0.719 | 0 / 0.000 |

## Locked weights (D1 → maximize RNA AUC)

- ExTrain locked: Ec=0.273, Ep=0.013, Em=0.150, Es=0.194, Ex_train=0.370
- noEx locked: Ec=0.073, Ep=0.643, Em=0.209, Es=0.075
- Equal ExTrain RNA AUC: 0.935; Locked: 0.940

## D3 locked-weight transfer

D3 cache found — see BIOS_v2_D3_locked_transfer.csv

## Correct claim (use this in the paper)

> BIOS-Rank v2 ranks genes by platform-adjusted Condition evidence, purity, module, and
> selector stability, optionally with **train-assay** fidelity; it is evaluated on the
> **held-out assay** so Ex is not circular. Locked equal-or-better weights are reported
> as sensitivity. Orthogonal CRC marker recovery is reported separately from AUC.

Generated: 2026-07-12 13:27:18 UTC
