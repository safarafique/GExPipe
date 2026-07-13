# BIOS-Rank — expert introduction of the novel biomarker filter

## Positioning (expert statement)

Most GEO “biomarker” pipelines either:

1. intersect platform DEG lists, or  
2. dump thousands of DEGs, or  
3. hand the top-50 limma genes to a classifier.

Those habits optimize **list agreement** or **in-cohort separation**, not **biology that is invariant to how we measured it**.

**BIOS-Rank** (*Biological Invariance Orthogonal Selection Ranking*) is a **gene-level filter and ranking method** for cross-platform biomarker discovery. It does not replace limma; it **re-weights and filters** limma (and companion evidence) so that selected genes are:

- driven by **Condition**, not Platform,  
- embedded in a **phenotype-linked co-expression programme**,  
- **stable** under multiple selectors, and  
- **faithful on both assays** (cross-assay single-gene discrimination).

Together with JPCT’s joint model (M1) and panel/transfer evaluation (M3), BIOS-Rank is the **novel filtering engine** (M2+).

---

## Why this is biologically novel (not just another ∩)

| Old habit | Biological failure | BIOS-Rank fix |
|-----------|-------------------|---------------|
| DEG ∩ across platforms | Boolean agreement ≠ shared Condition effect; discards true genes weak on one assay | Joint \(E_c\) + purity \(E_p\) |
| Top-*k* by *P*-value only | Favours assay-easiest genes; ignores batch entanglement | \(E_p\) penalizes platform-dominated genes |
| Huge DEG lists | Not actionable; many are non-programmatic spikes | \(E_m\) requires trait-WGCNA context |
| Single ML method | Method artefacts | \(E_s\) stability |
| Train/test on same assay only | Misses technology shift | \(E_x\) = min(AUC on each assay) |

**Novelty type:** new *selection objective* and *score*, not a new differential-expression likelihood. That is the correct claim for *Bioinformatics* Original Papers in this space.

---

## Formal definition

After joint limma on the harmonized matrix:

\[
y_{gi} = \alpha_g + \beta_g^{C}\mathrm{Condition}_i + \beta_g^{P}\mathrm{Platform}_i + \varepsilon_{gi}
\]

define five evidence channels for gene \(g\):

| Symbol | Name | Formula (operational) |
|--------|------|------------------------|
| \(E_c\) | Condition evidence | \(-\log_{10}(p_g^{\mathrm{adj}})\cdot\|\widehat\beta_g^{C}\|\) |
| \(E_p\) | Platform purity | \(\|\widehat\beta_g^{C}\| / (\|\widehat\beta_g^{C}\|+\|\widehat\beta_g^{P}\|+\epsilon)\) |
| \(E_m\) | Module support | 1 if in trait-associated WGCNA module, else 0 |
| \(E_s\) | Selector stability | 1 if in ≥2-ML / consensus export; partial credit if WGCNA-only |
| \(E_x\) | Cross-assay fidelity | \(\min(\mathrm{AUC}_{g,\mathrm{micro}},\mathrm{AUC}_{g,\mathrm{rna}})\) |

Scale \(E_c,E_p,E_x\) to \([0,1]\) across genes. Then:

\[
\mathrm{BIOS}(g) = \frac{1}{5}\big(\tilde E_c + \tilde E_p + E_m + E_s + \tilde E_x\big)
\]

**Soft mode (discovery):** rank by BIOS; take top-*k*.  
**Hard consensus mode:** require \(E_m=1\) and high \(E_s\), then rank by BIOS (JPCT panel).

---

## How to evaluate (important)

On GSE89076↔GSE50760, **multivariate RF transfer AUC saturated** (even random panels ≈ 0.96) because Condition is highly separable after joint batch modelling. That does **not** invalidate filtering; it means the right metric is:

**Primary filter metric:** median (or min-median) **single-gene** AUC across assays  

**Secondary:** simulation joint vs ∩ (already green); external cohort panel AUC with matched-*k* and random controls  

BIOS-Rank is successful if top-*k* BIOS genes beat random and beat naïve ∩ / limma-only on **cross-assay gene fidelity**, while staying compact.

### Empirical result (GSE89076 ↔ GSE50760, k=20)

| Panel | Min-median cross-assay AUC |
|-------|---------------------------:|
| **BIOS-Rank top-20** | **0.924** |
| Limma / separate∩ top-20 | 0.884 |
| Random | 0.807 |

Limma top-20 is strong on microarray (0.966) but weaker on RNA-seq (0.884). BIOS-Rank stays balanced (≈0.94 / 0.94) — the biological point of the filter.

---

## Relation to JPCT

```text
JPCT
├── M1  Joint limma ~ Condition + Platform     → supplies βC, βP, p-values
├── M2+ BIOS-Rank filter (THIS METHOD)         → ranks / filters biomarkers
└── M3  Panel + transfer / external tests         → proves utility
```

Application Note = software packaging.  
Original Paper = **BIOS-Rank + JPCT evaluation**.

---

## One-sentence pitch (cover letter)

> We introduce BIOS-Rank, a platform-invariant biomarker filter that scores each gene by joint condition evidence, platform purity, trait-module membership, multi-selector stability, and cross-assay fidelity—replacing DEG intersection and top-*P* lists with a biologically motivated ranking rule, implemented in the GExPipe/JPCT workflow.

---

## BIOS-FDR (error control) — flagship add-on

Phenotype-permutation null for **Condition** (Platform fixed):

1. Observed BIOS top-*k* threshold \(t_{(k)}\).  
2. For each permutation: shuffle Condition; refit joint limma; recompute Ec, Ep, Ex; shuffle Em, Es across genes.  
3. Empirical FDR = average(\# null genes with BIOS ≥ \(t_{(k)}\)) / *k*.  
4. Compare to limma top-*k* under the **same** null.

```bash
"/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
  scripts/bios-fdr.R --gexpipe-repo "E:/GExPipe" --n-perm 100
```

This upgrades BIOS-Rank from a score to a **filter with calibrated specificity**.

## BIOS-FDR (error control)

See `results/BIOS_FDR_summary.md` after running `scripts/bios-fdr.R`.

 Phenotype permutation null for Condition; empirical FDR of top-*k* BIOS vs limma top-*k*.

## BIOS-FDR (error control)

See `results/BIOS_FDR_summary.md` after running `scripts/bios-fdr.R`.

 Phenotype permutation null for Condition; empirical FDR of top-*k* BIOS vs limma top-*k*.
