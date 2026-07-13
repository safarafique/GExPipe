# GExPipe Original Paper — draft skeleton

**Status:** Simulation C4 **green**. Application Note = software track. Original = method track (C1 + C4 solid; C2–C3 limited by small external *n*).

**Do not merge these sections into the Application Note manuscript before Note submission.**

Working title (method-first):

> Joint cross-platform limma modelling and multi-evidence consensus improve GEO biomarker panels: evaluation of the GExPipe framework

---

## Abstract (placeholder)

Cross-platform GEO biomarker studies often treat the intersection of platform-specific DEG lists as a joint analysis and treat large DE lists as biomarkers without external panel validation. We evaluate (M1) joint limma with Dataset/Platform covariates after HGNC harmonization, and (M2) a multi-evidence consensus rule DE ∩ trait-WGCNA ∩ (≥2 ML selectors) using **panel-level external AUC** as the primary endpoint. On known-truth simulations, joint limma recovers far more true DEGs than separate∩ (sensitivity 0.87 vs 0.26) and controls FDR when Platform is confounded with Condition; on CRC cohorts, a consensus panel of size 20 matches limma top-20/50 discrimination while shrinking the list. GExPipe provides a reproducible Bioconductor implementation (Application Note).

---

## Claims

| ID | Claim | Status |
|----|--------|--------|
| C1 | Separate DEG ∩ ≠ joint limma under platform structure | **Green** (real CRC merge + simulation) |
| C2 | Full consensus: smaller *k*, external panel AUC ≥ limma top-*k* | **Partial** (matches matched-*k*; ceiling at *n*=20) |
| C3 | Ablation: WGCNA and ML layers change size and/or panel AUC | **Partial** (gene sets differ; AUC saturated) |
| C4 | Known-truth / generalization support for M1 | **Green** (simulation v4) |

### Acceptance gate (Original)

- [x] C1 quantified (real merge + sim)
- [x] Panel AUC + ablation table exists
- [x] Simulation: joint sens > ∩; Jaccard < 1; raw confound FDR controlled by Platform
- [ ] Optional: 2nd GEO external
- [ ] Full Original rewrite still to write

---

## Results outline

### 1. Merge ≠ ∩ (C1)

Cite Application Note / `validation_manual/cross_platform/` (Jaccard, concordance, only-in-merged *n* = 587).

### 2. Ablation + panel AUC (C2–C3)

Source: `results/ablation_panel_auc.md`. Train GSE50760 *n*=36; external GSE104836 *n*=20. Primary: RF panel AUC; matched *k*=20.

Headline: B5 (*k*=20) RF AUC = 1.00 matches limma top-20/50; median gene AUC 0.98 vs random B6 0.695; true B4 ≠ B5 (Jaccard 0.481). Panel AUC ceiling limits ranking among biology arms.

### 3. Generalization (C4) — simulation **green**

Source: `results/simulation_joint_vs_intersect.md` (v4, 50 reps).

| Contrast | Result |
|----------|--------|
| Balanced harmonized: joint(+Platform) vs ∩ | Sens **0.870** vs **0.264**; FDR 0.038 vs 0.000; Jaccard **0.292** |
| True DEGs only in joint (not in ∩) | Mean **60.6** TP |
| Confounded raw: Condition-only vs +Platform | FDR **0.962** vs **0.051** |

Full table in `results/simulation_joint_vs_intersect.md`.

---

## Methods (evaluation protocol)

1. Train GSE50760; hold out GSE104836 (never tune on external).
2. Panel primary: RF external AUC + nested CV; glmnet secondary.
3. Simulation: mixed strong/weak true DEGs; balanced + confounded; harmonized vs raw.
4. Code under `GExPipe(original_paper)/scripts/` only.

---

## Software pointer

Analyses use GExPipe helpers and GEO exports described in the Application Note; this Original Paper focuses on claims C1–C4.
