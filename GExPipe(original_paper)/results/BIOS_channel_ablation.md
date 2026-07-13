# BIOS-Rank channel ablation & weight sensitivity (D1)

Cohort: GSE89076 × GSE50760. Default k=20. Primary metric: **min-median cross-assay gene AUC**.
Equal-weight BIOS MinMedian = **0.924**.

## Claim (for the paper)

1. **Leave-one-out:** dropping **Ec, Ex** lowers MinMedian at k=20; **Ec, Es, Ex** change top-*k* composition (Jaccard < 0.95).
2. **Saturation:** at small *k*, Em/Ep may show Δ≈0 and Jaccard=1 because top genes already have Em=Es=Ep≈1 — check multi-*k* table.
3. **Weights:** equal-weight MinMedian=0.924; best Dirichlet=0.945 (gap -0.021). **Equal weights are near-optimal** on this cohort. 65.8% of random simplex weights ≥ equal.
4. **ONLY_Ex can look best on MinMedian because Ex is both a ranking feature and the evaluation metric; prefer LOO + biology secondaries for necessity claims.**

## 1. Leave-one-out (k=20)

| Channel | Drop → MinMedian | Δ vs full | Panel Jaccard vs full | Verdict |
|---------|------------------:|----------:|----------------------:|---------|
| Ec | 0.921 | -0.002 | 0.667 | necessary (drop ↓ metric) |
| Ep | 0.924 | 0.000 | 1.000 | saturated at this k (panel unchanged) |
| Em | 0.924 | 0.000 | 1.000 | saturated at this k (panel unchanged) |
| Es | 0.931 | 0.007 | 0.538 | changes panel composition |
| Ex | 0.923 | -0.001 | 0.905 | necessary (drop ↓ metric) |

## 1b. Multi-*k* leave-one-out

| k | Channel | Full | LOO | Δ | Jaccard |
|---|---------|-----:|----:|--:|--------:|
| 20 | Ec | 0.924 | 0.921 | -0.002 | 0.667 |
| 20 | Ep | 0.924 | 0.924 | 0.000 | 1.000 |
| 20 | Em | 0.924 | 0.924 | 0.000 | 1.000 |
| 20 | Es | 0.924 | 0.931 | 0.007 | 0.538 |
| 20 | Ex | 0.924 | 0.923 | -0.001 | 0.905 |
| 50 | Ec | 0.923 | 0.932 | 0.009 | 0.449 |
| 50 | Ep | 0.923 | 0.923 | 0.000 | 1.000 |
| 50 | Em | 0.923 | 0.923 | 0.000 | 1.000 |
| 50 | Es | 0.923 | 0.925 | 0.002 | 0.786 |
| 50 | Ex | 0.923 | 0.916 | -0.007 | 0.639 |
| 100 | Ec | 0.920 | 0.924 | 0.004 | 0.538 |
| 100 | Ep | 0.920 | 0.920 | 0.000 | 1.000 |
| 100 | Em | 0.920 | 0.920 | 0.000 | 0.961 |
| 100 | Es | 0.920 | 0.920 | 0.000 | 0.942 |
| 100 | Ex | 0.920 | 0.913 | -0.006 | 0.754 |

## 2. Single-channel only

| Channel only | MinMedian | Δ vs full |
|--------------|----------:|----------:|
| ONLY_Ec | 0.931 | 0.007 |
| ONLY_Ep | 0.918 | -0.005 |
| ONLY_Em | 0.938 | 0.015 |
| ONLY_Es | 0.920 | -0.004 |
| ONLY_Ex | 0.950 | 0.026 |

## 2b. Biology secondaries (non-circular with Ex)

| Setting | MinMedian | Mean Ep (purity) | Frac Em | Frac Es | Mean Ec |
|---------|----------:|-----------------:|--------:|--------:|--------:|
| BIOS_equal | 0.924 | 1.000 | 1.000 | 0.800 | 0.717 |
| ONLY_Ec | 0.931 | 1.000 | 0.950 | 0.500 | 0.800 |
| ONLY_Ep | 0.918 | 1.000 | 0.950 | 0.400 | 0.717 |
| ONLY_Em | 0.938 | 1.000 | 1.000 | 0.250 | 0.627 |
| ONLY_Es | 0.920 | 1.000 | 1.000 | 0.950 | 0.613 |
| ONLY_Ex | 0.950 | 1.000 | 0.650 | 0.200 | 0.470 |
| BIOS_best_Dirichlet | 0.945 | 1.000 | 1.000 | 0.450 | 0.680 |
| Merged_limma_topk | 0.883 | 1.000 | 0.400 | 0.100 | 0.361 |

## 3. Weight sensitivity (Dirichlet, n=500)

- Equal-weight MinMedian: **0.924**
- Best random weights MinMedian: **0.945** (gap vs equal: -0.021)
- Median / IQR of random-weight MinMedian: 0.927 [0.921, 0.927]
- Fraction of draws ≥ equal: 65.8%
- Best weights (D1-tuned): Ec=0.018, Ep=0.332, Em=0.087, Es=0.025, Ex=0.538

**Default recommendation:** keep **equal weights** in Methods; report best Dirichlet as sensitivity. 
Do not replace the formula with D1-tuned weights unless they are locked and validated on D3.

Generated: 2026-07-12 12:18:35 UTC
