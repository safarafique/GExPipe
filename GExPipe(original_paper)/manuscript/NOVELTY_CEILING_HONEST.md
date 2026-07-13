# Honest novelty ceiling — what “100% novel” can and cannot mean

**Fact:** No ranking that reuses limma, WGCNA, and RF can be *100% algorithmically novel*.  
**Goal of v2:** Make the **idea correct** (non-circular) and the **novelty claim maximally defendable**.

## Correct novelty claim (use this)

> **BIOS-Rank** is a **platform-invariant biomarker ranking objective**: after joint `~ Condition + Platform`, genes are scored by Condition strength, platform purity, trait-module membership, and multi-selector stability, optionally with **train-assay** fidelity (\(E_x^{\mathrm{train}}\)). Selection is evaluated on the **held-out assay**, with **BIOS-FDR** calibrating the composite top-*k*, and orthogonal CRC marker recovery reported separately. Equal weights are the default; locked weights are sensitivity.

## What is novel vs not

| Element | Novel? | Notes |
|---------|--------|-------|
| Joint limma `~ Condition + Platform` | No | Setup |
| \(E_p\) platform purity in the rank | **Yes (component)** | Rarely used as explicit DE ranking term |
| Putting train-assay fidelity in rank + held-out eval | **Yes (protocol)** | Fixes circular Ex |
| Composite + BIOS-FDR for multi-channel top-*k* | **Yes (package)** | Error control for the score |
| WGCNA / ML / limma themselves | No | Cite only |

## Improvements applied in code

| # | Improvement | Script / output |
|---|-------------|-----------------|
| 1 | Non-circular Ex | `scripts/bios-rank-v2-correctness.R` → `BIOS_v2_correctness.*` |
| 2 | Locked weights on D1 | `BIOS_v2_locked_weights.csv` |
| 3 | Harder baselines | same comparison table |
| 4 | CRC marker overlap | columns in comparison CSV |
| 5 | Formula §8 | `manuscript/BIOS_FORMULA.md` |

## Still open (do next for higher score)

| # | Item | Why |
|---|------|-----|
| A | Build D3 gene scores + apply locked weights | External weight test |
| B | Non-CRC disease pair | Generalizability |
| C | Sample bootstrap Jaccard of top-20 | Stability |
| D | Reverse direction (RNA train → micro test) | Symmetry |

**Do not claim “100% novel algorithm.”** Claim **correct platform-invariant ranking protocol + calibrated composite filter** — that is the publishable, honest peak.
