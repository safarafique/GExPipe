# Submission checklist — Original Paper vs Application Note

**Date:** 2026-07-12

## Original Paper (`manuscript/GExPipe_Original_Paper.md`) — method track

**Status: analysis-complete; ready for journal formatting / co-author polish.**

### Must-have evidence (done)

| Item | Result file | Status |
|------|-------------|--------|
| Joint ≠ ∩ (real) | `validation_manual/cross_platform/` | Done |
| Simulation joint ≫ ∩ | `results/simulation_joint_vs_intersect.*` | Done |
| BIOS-Rank D1 win | `results/BIOS_Rank_panel_comparison.*` | Done (0.924 > 0.884) |
| Channel ablation / equal weights | `results/BIOS_channel_ablation.*` | Done |
| BIOS-FDR | `results/BIOS_FDR_summary.*` | Done (FDR 0.0015, P 0.0099) |
| Multi-dataset D1–D3 | `results/BIOS_Rank_multi_dataset.*` | Done (WIN / TIE / TIE) |

### Safe claims

- BIOS improves cross-assay fidelity vs limma/∩ on **D1**.
- BIOS matches limma on easy/ceiling **D2–D3**; beats random.
- Equal weights near-optimal; ONLY_Ex is circular on MinMedian.
- BIOS-FDR calibrates the composite score (not “better FDR than limma”).

### Forbidden claims

- “New limma / WGCNA / RF”
- “Beats limma on all datasets”
- “Shiny UI is the novelty”

### Next editorial steps (human)

1. Convert markdown → journal Word/LaTeX template (*Bioinformatics* Original or *BMC Bioinformatics*).
2. Add figures: workflow (M1–M3), D1 bar chart BIOS vs limma vs random, LOO Δ plot, simulation FDR panel.
3. Cite Application Note / GExPipe software separately.
4. Cover letter: lead with BIOS-Rank + joint≠∩, not the GUI.

---

## Application Note — software track

**Status: submit independently when package vignette + Bioconductor checklist are ready.**

- Novelty = reproducible GEO → DE → consensus → Shiny workflow.
- Do **not** wait for Original; do **not** put BIOS-Rank as the only Note claim unless already in the package.
- Cite Original later (or “method described elsewhere”) once accepted.

---

## Recommended order

1. **Application Note** → Oxford *Bioinformatics* (software) — when packaging ready.  
2. **Original Paper** → method journal — manuscript now analysis-ready.  
3. If Original rejected → *BMC Bioinformatics* backup with same honest claims.
