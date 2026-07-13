# Multi-dataset + ablation — final interpretation

**Paper wording (paste):**  
“BIOS-Rank improved platform-invariant gene selection on a microarray–RNA-seq CRC merge (D1: 0.924 vs limma/∩ 0.884), matched limma on an independent RNA-seq external and a second cross-platform pair (D2–D3) while far exceeding random panels, and remained tightly controlled under phenotype-permutation BIOS-FDR. Leave-one-out and Dirichlet weight search supported equal channel weights as a robust default; Ex-only ranking was circular on the primary metric and lost ML/module membership.”

| Claim | Status |
|-------|--------|
| Better cross-assay fidelity vs limma (D1) | **Yes** |
| Match limma on easy/ceiling sets (D2–D3) | **Yes** |
| Beats random always | **Yes** |
| Equal weights justified | **Yes** (~0.02 gap to best; 66% draws ≥ equal) |
| Each channel necessary | **Partial** — Ec/Ex/Es matter; Em/Ep saturated at small *k* |
| Better FDR than limma | **No** — do not claim |

Generated from completed runs 2026-07-12.
