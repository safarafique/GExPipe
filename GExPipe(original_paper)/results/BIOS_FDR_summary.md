# BIOS-FDR — permutation empirical FDR for BIOS-Rank

Cohort: GSE89076 (Microarray) + GSE50760 (RNA-seq), merged cache. k=20, B=100 Condition permutations. Seed=42.

**Null model:** shuffle Condition; keep Platform; refit `~ Condition + Platform`;
recompute Ec, Ep, Ex; gene-wise shuffle Em, Es.

**Empirical FDR (top-k list):** mean over permutations of
`(# genes with null BIOS ≥ observed k-th BIOS) / k`.

| Method | Empirical FDR | Mean null discoveries | Observed mean top-k BIOS | Emp. P (mean top-k) |
|--------|--------------:|----------------------:|-------------------------:|--------------------:|
| BIOS_Rank_topk | 0.0015 | 0.03 | 0.9065 | 0.0099 |
| limma_adjP_topk | 0.0000 | 0.00 | 0.7452 | — |

## Observed BIOS top-k (with per-gene empirical P)

| Gene | BIOS | adj.P | Emp.P | Em | Es |
|------|-----:|------:|------:|---:|---:|
| CA7 | 0.998 | 1.12e-28 | 0.010 | 1 | 1 |
| BEST4 | 0.986 | 1.12e-28 | 0.010 | 1 | 1 |
| GUCA2B | 0.974 | 3.61e-22 | 0.010 | 1 | 1 |
| KRT80 | 0.973 | 1.12e-28 | 0.010 | 1 | 1 |
| CA1 | 0.964 | 1.42e-19 | 0.010 | 1 | 1 |
| SPIB | 0.959 | 6.01e-29 | 0.010 | 1 | 1 |
| TMIGD1 | 0.956 | 2.12e-20 | 0.010 | 1 | 1 |
| OTOP2 | 0.953 | 7.12e-27 | 0.010 | 1 | 1 |
| AQP8 | 0.950 | 3.96e-21 | 0.010 | 1 | 1 |
| CLCA4 | 0.939 | 5.94e-19 | 0.010 | 1 | 1 |
| MYOC | 0.911 | 9.54e-22 | 0.010 | 1 | 1 |
| KRT24 | 0.893 | 1.76e-22 | 0.010 | 1 | 1 |
| INHBA | 0.862 | 4.19e-18 | 0.010 | 1 | 1 |
| WNT2 | 0.861 | 1.72e-18 | 0.010 | 1 | 1 |
| ESM1 | 0.858 | 7.79e-18 | 0.010 | 1 | 1 |
| ETV4 | 0.839 | 6.01e-29 | 0.010 | 1 | 0.33 |
| MMP7 | 0.832 | 1.63e-22 | 0.010 | 1 | 0.33 |
| CLDN1 | 0.816 | 3.29e-27 | 0.010 | 1 | 0.33 |
| GUCA2A | 0.807 | 1.92e-20 | 0.010 | 1 | 0.33 |
| EPHX4 | 0.800 | 3.06e-26 | 0.010 | 1 | 0.33 |

## Verdict

**BIOS-FDR passes.** Observed mean top-20 BIOS (**0.907**) ≫ null mean (**0.631**); empirical *P* = **0.0099**; list FDR ≈ **0.15%**.

Do **not** claim “BIOS beats limma on FDR”: limma’s top-20 adj.P threshold (~1e−23) is so extreme that null discoveries are also ~0. Both filters are highly specific under Condition permutation.

**What BIOS-FDR uniquely supports:** the **composite BIOS score** (not adj.P alone) is significantly enriched vs a phenotype-null, so the multi-channel filter is not an artifact of label noise. Pair this with §4.5 (cross-assay fidelity win over limma top-*k*).

Generated: 2026-07-12 11:34:31 UTC
