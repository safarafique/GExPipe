# Table 4 (draft). Simulation — joint limma vs separate DEG ∩

Replicates: 50. Genes: 2000 (true DEGs: 100). Samples/platform: 30. Effect mix: 40% strong beta=1.5, rest weak beta=0.9, sigma=0.85. Call: adj.P < 0.05 and |logFC| > 0.25. Seed base = 42.

**Methods:** Separate_intersect = limma per platform → ∩; 
Joint_*_harmonized = after within-platform z-score; 
Joint_*_raw = no z-score (batch remains). 
M1 = `~ Condition + Platform`.

| Scenario | Method | Sensitivity | Empirical FDR | Precision | n called | Jaccard(∩, joint) |
|----------|--------|-------------|-----------------|-----------|----------|-------------------|
| balanced | Separate_intersect | 0.264 ± 0.041 | 0.000 ± 0.000 | 1.000 ± 0.000 | 26.4 ± 4.1 | 1.000 ± 0.000 |
| balanced | Separate_union | 0.571 ± 0.047 | 0.025 ± 0.019 | 0.975 ± 0.019 | 58.5 ± 4.9 | — |
| balanced | Joint_Condition_only_harmonized | 0.877 ± 0.035 | 0.041 ± 0.023 | 0.959 ± 0.023 | 91.5 ± 4.2 | — |
| balanced | Joint_Condition_Platform_harmonized | 0.870 ± 0.038 | 0.038 ± 0.021 | 0.962 ± 0.021 | 90.4 ± 4.4 | 0.292 ± 0.049 |
| balanced | Joint_Condition_only_raw | 0.038 ± 0.039 | 0.000 ± 0.000 | 1.000 ± 0.000 | 3.8 ± 3.9 | — |
| balanced | Joint_Condition_Platform_raw | 0.908 ± 0.026 | 0.050 ± 0.022 | 0.950 ± 0.022 | 95.6 ± 3.6 | — |
| confounded | Separate_intersect | 0.025 ± 0.026 | 0.000 ± 0.000 | 1.000 ± 0.000 | 2.5 ± 2.6 | 1.000 ± 0.000 |
| confounded | Separate_union | 0.200 ± 0.096 | 0.018 ± 0.026 | 0.982 ± 0.026 | 20.5 ± 10.0 | — |
| confounded | Joint_Condition_only_harmonized | 0.328 ± 0.056 | 0.001 ± 0.004 | 0.999 ± 0.004 | 32.8 ± 5.6 | — |
| confounded | Joint_Condition_Platform_harmonized | 0.672 ± 0.055 | 0.039 ± 0.022 | 0.961 ± 0.022 | 69.9 ± 6.0 | 0.036 ± 0.037 |
| confounded | Joint_Condition_only_raw | 0.760 ± 0.047 | 0.962 ± 0.002 | 0.038 ± 0.002 | 1976.0 ± 4.8 | — |
| confounded | Joint_Condition_Platform_raw | 0.736 ± 0.041 | 0.051 ± 0.024 | 0.949 ± 0.024 | 77.6 ± 5.0 | — |

## Key contrasts (mean)

- **Balanced (harmonized):** joint(+Platform) sensitivity 0.870 vs ∩ 0.264; FDR 0.038 vs ∩ 0.000; Jaccard 0.292.
- **Confounded (harmonized):** joint recovers TP missed by ∩: mean TP_only_joint=64.6.
- **Confounded (raw, no z-score):** Condition-only FDR 0.962 vs joint(+Platform) FDR 0.051 (Platform covariate required when batch remains).
- Genes only in joint vs ∩ (balanced, harmonized): mean n=64.1 of which TP=60.6.

Accept C4-simulation if: (i) joint(+Platform) sensitivity ≥ ∩ with controlled FDR under harmonized data;
(ii) lists are not identical (Jaccard < 1); (iii) on raw confounded data, Condition-only inflates FDR vs Platform-adjusted joint.

Generated: 2026-07-12 10:27:56 UTC
