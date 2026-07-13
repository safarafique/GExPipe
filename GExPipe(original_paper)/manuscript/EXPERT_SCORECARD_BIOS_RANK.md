# Expert review scorecard — BIOS-Rank / BIOS-FDR

**Reviewer stance:** Senior computational biologist / bioinformatics methods editor  
**Object reviewed:** BIOS-Rank + BIOS-FDR (within JPCT), CRC GEO evidence package  
**Date:** 2026-07-12  

---

## Overall score

| | Score | Band |
|--|------:|------|
| **BIOS-Rank as a publishable method** | **7.2 / 10** | **Solid Original / strong Application-methods hybrid** |
| **As a fundamental statistical invention** | **4.5 / 10** | Incremental composite, not a new likelihood |
| **As a solution to a real GEO practice problem** | **8.0 / 10** | Clear estimand; joint ≠ ∩ is important |
| **Readiness for *Bioinformatics* Original** | **7.0 / 10** | Submitable with honest claims; not *Nature Methods* |

**One-line verdict:**  
BIOS-Rank is a **credible, well-motivated multi-evidence ranking rule** with **good empirical discipline** (ablation, FDR, multi-cohort). It is **not** a paradigm-shifting algorithm. Worth publishing if you lead with platform-invariant *utility* and joint≠∩ — and refuse to claim “beats limma everywhere.”

---

## Dimension scores (each /10)

| Dimension | Score | Expert comment |
|-----------|------:|----------------|
| **Problem importance** | **8.5** | DEG∩-as-replication and “7000 DEGs = biomarkers” are real, damaging GEO habits. Worth fixing. |
| **Conceptual clarity** | **8.0** | Five channels map cleanly to invariance / biology / compactness. Estimand after Platform is the right target. |
| **Mathematical novelty** | **5.0** | Weighted/equal mean of known evidences. \(E_p\) and putting \(E_x\) *inside the rank* are the freshest bits; rest is assemblage. |
| **Statistical rigor** | **7.0** | BIOS-FDR + random panels + matched-*k* are good. \(E_x\) partly circular with the primary metric (you acknowledged this — keep that honesty). Equal weights justified empirically. |
| **Empirical strength** | **7.5** | D1 win (0.924 vs 0.884) is the key positive result. D2–D3 ties at ceiling are honest, not failures. Simulation joint≫∩ is strong supporting evidence for JPCT/M1. |
| **Biological plausibility** | **7.5** | WGCNA + multi-ML filters are standard but appropriate. High Em/Es vs limma is a real utility gain. No wet-lab / pathway orthogonal validation yet. |
| **Generalizability** | **5.5** | CRC-heavy; one hard win + two easy ties. Need another disease or harder negative-control disease before claiming a general biomarker engine. |
| **Clinical/translational readiness** | **5.0** | Compact *k*=20 is actionable *in principle*. No prospective cohort, no qPCR lock-down, no regulatory path. “Useful” ≠ “clinic-ready.” |
| **Reproducibility / engineering** | **8.0** | Scripts, caches, formula doc, ablation — above average for a methods draft. Package separation from Application Note is clean. |
| **Overclaim risk** | **6.5** | Currently well-controlled in docs. Highest rejection risk if Abstract says “superior to limma” without D1-only qualification. |
| **Reviewer attack surface** | **6.0** | Expect: (i) Ex circularity, (ii) Em/Ep LOO saturation, (iii) “just limma+WGCNA+RF,” (iv) CRC-only, (v) ceiling AUCs. You already have rebuttals — use them. |

**Mean of dimensions (unweighted):** ≈ **6.8 / 10**  
**Overall (importance-weighted toward problem + empirics):** **7.2 / 10**

---

## What would raise the score to 8+

| Upgrade | Expected lift |
|---------|---------------|
| Lock weights on D1, validate on D3 (or reverse) | +0.3 |
| Non-CRC disease pair with same protocol | +0.5 |
| Primary metric that does **not** use \(E_x\) for ranking evaluation (e.g. held-out assay-only AUC, or literature CRC gene enrichment) | +0.4 |
| Orthogonal biology (pathway / known CRC marker overlap beyond WGCNA) | +0.3 |
| Prospective or RT-qPCR subset of top-20 | +0.5 (translational band) |

---

## Journal fit (expert guess)

| Venue | Fit | Notes |
|-------|-----|-------|
| *Bioinformatics* Original | **Good** | Method + evaluation; lead BIOS + joint≠∩ |
| *BMC Bioinformatics* | **Very good** | Safer if Original wants more theory |
| *Briefings in Bioinformatics* | **Possible** | Needs more survey/context framing |
| *Nature Methods* / *Genome Biology* Methods | **Unlikely** | Novelty bar too high without new model + broad benchmarks |
| Application Note only | **Undersells** BIOS | Software Note ≠ bury the score |

---

## Final examiner table

| Question | Grade | Score |
|----------|-------|------:|
| Is the scientific *question* important? | A− | 8.5 |
| Is the *method* novel enough for Original? | B | 6.5 |
| Is the *evaluation* convincing? | B+ | 7.5 |
| Is the *writing stance* defendable? | A− | 8.0 |
| Would I recommend **accept with revision**? | **Yes** | — |
| Would I recommend **accept as-is at top venue**? | **No** | — |

### Bottom line score

# **7.2 / 10 — Publishable methods contribution**

Strong enough for a serious Original Paper if claims stay precise.  
Not a foundational algorithm; **do not** market it as one.
