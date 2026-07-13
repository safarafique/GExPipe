# Is JPCT biologically novel? — honest assessment

## Short answer

**JPCT is a novel *research procedure / evaluation framework* for GEO practice, not a novel core algorithm.**

- **New as a method story:** yes — it names a clear estimand, rejects DEG∩-as-joint-analysis, and demands panel + cross-assay transfer success.  
- **New as a mathematical invention:** no — limma, WGCNA, RF/LASSO, and batch correction already exist.  
- **Biologically accurate idea:** yes — the *target* (platform-adjusted Condition effect + systems/stability filters + transfer) matches how biology should behave across assays.

Journals (*Bioinformatics* Original) routinely accept this class of contribution **if** the evaluation is rigorous. They reject it if the paper pretends limma/WGCNA are new.

---

## What is genuinely new

| Element | Novelty type | Why it matters biologically |
|--------|--------------|------------------------------|
| **BIOS-Rank** (Ec–Ex composite) + equal-weight justification | Mathematical / procedural | Platform-invariant biomarker ranking beyond adj.P |
| **BIOS-FDR** (Condition-permutation FDR of composite) | Statistical practice | Error control for multi-channel top-*k* |
| Explicit estimand: \(\beta^{\mathrm{Cond}}\) after Platform | Conceptual / statistical practice | Disease biology ≠ technology main effect |
| Formal proof that DEG∩ ≠ joint call set (real + simulation) | Empirical method claim | Stops a widespread false equivalence in GEO meta-analysis |
| Consensus as **evidence combination with ablation** | Procedural | Requires DE + co-expression programme + multi-learner stability |
| **Cross-platform gene fidelity** as primary hard success | Evaluation novelty | A biomarker should work when the assay changes |
| Named package **JPCT** tying M1→M2→M3 + BIOS | Framing | Makes the contribution citeable and falsifiable (C1–C5) |

## What is *not* novel (do not overclaim)

| Component | Reality |
|-----------|---------|
| limma / voom | Standard |
| WGCNA trait modules | Standard |
| LASSO / RF / SVM-RFE | Standard |
| HGNC remapping, ComBat/limma batch | Standard |
| Shiny GUI | Application Note territory, not Original novelty |

**Safe sentence for the paper:**  
> JPCT does not introduce new base learners; it introduces a joint platform-aware estimand, a multi-evidence selection rule, and a transfer-centred evaluation that together correct biologically inaccurate GEO habits.

---

## Biological accuracy vs algorithmic novelty

**Biological accuracy** (JPCT scores well here):

1. Shared disease effect should survive platform adjustment.  
2. True programmes are co-expressed with phenotype, not only univariate DE.  
3. Stable multi-learner selection reduces method artefacts.  
4. A real biomarker panel should transfer across measurement technologies.

**Algorithmic novelty** (JPCT scores modestly):

- No new likelihood, no new network model, no new ML architecture.  
- Novelty is in **what is estimated, how candidates are filtered, and how success is judged**.

That is enough for an Original Paper **when paired with** your simulation (joint ≫ ∩; confound FDR control) and transfer results — not enough if the Abstract leads with “user-friendly Shiny.”

---

## How reviewers will attack — and the rebuttal

| Attack | Rebuttal with your evidence |
|--------|-----------------------------|
| “Just limma + WGCNA + RF” | Estimand + ∩ falsification + transfer endpoint are the contribution; components are acknowledged |
| “∩ is meant to be conservative” | Simulation: ∩ sensitivity 0.26 vs joint 0.87 at FDR≈0.05 — conservatism discards true biology |
| “Panel AUC saturated” | Matched-*k*, random control, nested CV, and **transfer** are the harder tests |
| “Only CRC” | Simulation is disease-agnostic known truth; 2nd disease optional |

---

## Verdict for submission strategy

| Track | Recommendation |
|-------|----------------|
| **Application Note** | Submit for software (GExPipe UI/pipeline); cite from Original |
| **Original (JPCT + BIOS-Rank)** | Ready to draft journal format: lead with BIOS-Rank/BIOS-FDR + joint≠∩ + D1 win / D2–D3 tie + channel ablation |
| **Overclaim risk** | Do **not** say “beats limma everywhere” or “new DE algorithm”; say platform-invariant filter with calibrated composite FDR |

**Bottom line:** BIOS-Rank + BIOS-FDR give a **defendable Original Paper core**; JPCT frames the workflow. Application Note remains the software track.
