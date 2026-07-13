# BIOS-Rank Original Paper (archived draft)

> **Submission-ready manuscript:** [`BIOS_Rank_Manuscript_for_Submission.md`](BIOS_Rank_Manuscript_for_Submission.md)  
> That file is the complete paper for publication (includes multi-GSE homogeneous/heterogeneous results vs Mosharaf et al. 2023).

**Article type:** Original Research / Methods  
**Suggested venues:** *Bioinformatics* (Original Paper), *BMC Bioinformatics*, or *Briefings in Bioinformatics*  
**Software:** https://github.com/safarafique/BIOS-Rank  
**Reproducible code:** `GExPipe(original_paper)/scripts/`  

---

## Title

**BIOS-Rank: Biological Invariance Orthogonal Selection Ranking improves disease-linked gene panels from joint microarray–RNA-seq analysis**

---

## Abstract

**Background.** Public GEO studies are often combined by intersecting separate differentially expressed gene (DEG) lists across microarray and RNA-seq, or by treating thousands of FDR-significant genes as biomarkers. Intersection is not a joint model of a shared disease effect under platform structure, and long DEG lists are rarely useful as clinical or experimental panels.

**Methods.** We introduce **JPCT** (*Joint Platform-aware Consensus with Transfer*) and its novel ranking layer **BIOS-Rank**. After HGNC harmonization, we fit limma `~ Condition + Platform` on the merged matrix. BIOS-Rank scores each gene by equal-weighted evidences: Condition strength (\(E_c\)), platform purity (\(E_p\)), trait-WGCNA membership (\(E_m\)), multi-selector stability (\(E_s\)), and optional **train-assay** fidelity (\(E_x^{\mathrm{train}}\)). Primary evaluation uses the **held-out assay** (non-circular). **BIOS-FDR** calibrates the composite top-*k* list by Condition permutation. Panel size *k* is user-chosen (default *k* = 20 for matched comparisons).

**Results.** Separate∩ and joint limma are not equivalent on GSE89076×GSE50760 (Jaccard = 0.37; 587 genes only in the joint call). Simulations show joint(+Platform) sensitivity 0.870 versus ∩ 0.264. On the hard cross-platform merge, non-circular BIOS top-20 achieves held-out RNA-seq median gene AUC **0.940** versus limma **0.883** and random **0.759**, with **9/20** CRC markers versus **6/20**. BIOS recovers **8/20** genes with PMID-backed wet-lab support versus **5/19** for limma, and **8/20** genes from recent 2022–2024 CRC signature papers versus **3/20**. BIOS-FDR for the top-20 composite list is **0.0015** (empirical *P* = 0.0099). On easier cohorts (GSE50760→GSE104836; GSE9348×GSE50760) BIOS matches limma at high AUC while beating random. Equal weights are near-optimal; locked weights transfer to a second merge.

**Conclusions.** BIOS-Rank turns joint DE into a compact, disease-linked, platform-aware biomarker panel. It is most advantageous in **merged multi-platform** settings; on single-assay data it remains a useful biology-aware filter even when AUC ties limma.

**Keywords:** biomarker ranking; cross-platform; GEO; limma; colorectal cancer; differential expression; WGCNA

---

## 1. Introduction

Biologically meaningful disease signatures should reflect **condition-driven expression change**, not measurement technology. When the same pathology is profiled by microarray and RNA-seq, a true disease gene should retain a coherent Condition effect after platform structure is modelled. Genes that appear only because one assay is noisier, or because platform is entangled with phenotype, are poor biomarkers.

GEO practice often operationalizes “replication” as: (i) run DEG analysis separately on each platform, then (ii) take the **intersection** of significant lists—or stop at huge FDR tables and call them biomarker candidates. Both habits confuse boolean agreement with joint estimation of shared biology.

Separate-then-intersect fails in three ways: power asymmetry (true genes may clear FDR on only one assay); absence of a shared Condition estimand; and blindness to Platform–Condition confounding. Thus ∩ can be ultra-conservative yet still wrong, while naive merges can be anti-conservative.

We do not claim novelty for limma, WGCNA, or random forests as algorithms. We introduce:

1. **JPCT** — a joint platform-aware workflow (harmonize → `~ Condition + Platform` → multi-evidence consensus → transfer-centred evaluation).  
2. **BIOS-Rank** — a multi-evidence ranking objective that selects **top disease-linked genes after DE**.  
3. **BIOS-FDR** — permutation calibration of the composite top-*k* score.

**Falsifiable claims.** (C1) Separate∩ ≠ joint limma sets. (C2–C3) Consensus changes membership vs DE top-*k* while remaining compact. (C4) Under known truth, joint(+Platform) recovers more true DEGs than ∩ and controls FDR when confounded. (C5) BIOS improves (or matches) held-out / cross-assay fidelity vs limma top-*k*, beats random, and enriches wet-lab / recent-paper disease genes.

---

## 2. Methods

### 2.1 Datasets

| Role | Accessions | Assay | Contrast |
|------|------------|-------|----------|
| D1 hard merge | GSE89076 + GSE50760 | microarray + RNA-seq | Disease vs Normal |
| D2 external | train GSE50760 → test GSE104836 | RNA-seq | Disease vs Normal |
| D3 second merge | GSE9348 + GSE50760 | microarray + RNA-seq | Disease vs Normal (70 tumor / 12 healthy on GSE9348) |

Phenotypes were assigned from title / source_name / characteristics fields only (avoiding free-text `data_processing` false matches to “normal”).

### 2.2 Joint differential expression (M1)

Expression matrices were mapped to HGNC symbols, normalized within platform, and stacked after within-platform standardization. We fit:

\[
y_{gi} = \alpha_g + \beta_g^{C}\,C_i + \beta_g^{P}\,P_i + \varepsilon_{gi}
\]

using limma–eBayes. \(\beta_g^{C}\) is the disease effect after removing platform main effects—the quantity ∩ never estimates.

### 2.3 BIOS-Rank (novel filter)

For each gene \(g\):

| Channel | Definition | Meaning |
|---------|------------|---------|
| \(E_c\) | \((-\log_{10}p_g^{\mathrm{adj}})\,\|\hat\beta_g^{C}\|\) | Disease strength |
| \(E_p\) | \(\|\hat\beta_g^{C}\|/(\|\hat\beta_g^{C}\|+\|\hat\beta_g^{P}\|+\epsilon)\) | Platform purity |
| \(E_m\) | 1 if trait-associated WGCNA module | Co-expression programme |
| \(E_s\) | 1 if ≥2 ML/consensus; \(1/3\) if WGCNA-only | Selector stability |
| \(E_x^{\mathrm{train}}\) | AUC on **train assay only** (optional) | Within-assay fidelity |

Continuous channels are min–max scaled to \([0,1]\). **Default (recommended for claims):**

\[
\mathrm{BIOS}_4(g)=\tfrac14\bigl(\tilde E_c+\tilde E_p+E_m+E_s\bigr).
\]

Optional five-channel form adds \(\tilde E_x^{\mathrm{train}}\). Soft panel = top-*k* by BIOS; hard panel requires \(E_m=E_s=1\).

**Panel size.** *k* is not fixed by the method (typical range 8–50). We report *k* = 20 for matched-*k* comparisons with limma and random.

**Non-circular evaluation.** Ranking must not use held-out-assay AUC inside \(E_x\) when the same assay defines the primary success metric. Primary metric on D1/D3: **median single-gene AUC on RNA-seq** after ranking without RNA AUC in the score.

### 2.4 BIOS-FDR

Condition labels are permuted (Platform fixed), BIOS recomputed, and empirical FDR of the observed top-*k* threshold estimated; enrichment of mean top-*k* BIOS vs the null is reported.

### 2.5 Baselines and orthogonality

Baselines: merged limma top-*k*, separate∩ top-*k*, single-channel ranks, Ec×Ep, circular min(AUC) ranking, matched random genes. Orthogonal checks: curated CRC markers; PMID-backed wet-lab literature; concordance with independent 2022–2024 CRC signature papers (including studies using GSE50760/GSE9348).

### 2.6 Simulation

Known-truth simulations (2000 genes, 100 true DEGs; balanced and Platform–Condition confounded designs) compared separate∩ vs joint Condition-only vs joint Condition+Platform (50 replicates).

### 2.7 Implementation

Analyses live under `GExPipe(original_paper)/` and read GExPipe `validation_manual/` exports without modifying package code. Key scripts: `bios-rank-filter.R`, `bios-rank-v2-correctness.R`, `bios-fdr.R`, `bios-channel-ablation.R`, `bios-wetlab-validation.R`, `bios-recent-paper-concordance.R`, `simulate-joint-vs-intersect.R`.

---

## 3. Results

### 3.1 Separate∩ is not a joint analysis (C1)

On GSE89076 + GSE50760:

| Metric | Value |
|--------|------:|
| Separate∩ DEGs | 1811 |
| Joint limma DEGs | 1464 |
| Only in joint | **587** |
| Jaccard(∩, joint) | **0.37** |
| Direction concordance | **98.9%** |
| logFC Pearson *r* | **0.989** |

Hundreds of genes with a coherent joint Condition effect are invisible to ∩; shared calls remain directionally concordant.

### 3.2 Simulation confirms the estimand (C4)

| Setting | Method | Sensitivity | Empirical FDR |
|---------|--------|------------:|--------------:|
| Balanced, harmonized | Separate∩ | 0.264 | ≈0 |
| Balanced, harmonized | Joint(+Platform) | **0.870** | 0.038 |
| Confounded, raw | Joint Condition-only | 0.760 | **0.962** |
| Confounded, raw | Joint(+Platform) | 0.736 | **0.051** |

Without a Platform term under confounding, “disease” calls are largely technology artefacts.

### 3.3 BIOS-Rank on the hard merge (C5; non-circular)

Primary metric: median gene AUC on **held-out RNA-seq**, *k* = 20.

| Method | Med AUC RNA | CRC markers |
|--------|------------:|------------:|
| **BIOS_v2 (no Ex, equal weights)** | **0.940** | **9 (45%)** |
| BIOS_v2 ExTrain locked | 0.940 | 9 (45%) |
| ONLY_Ec | 0.935 | 8 |
| Merged limma top-20 | 0.883 | 6 (30%) |
| ONLY_Ex (both assays; circular) | 0.954 | 4 (20%) |
| Random | 0.759 | 0 |

BIOS improves held-out fidelity and marker recovery versus limma. Circular Ex-only can inflate AUC while losing biology—hence the non-circular protocol.

Using an older min-median cross-assay metric (historical), BIOS also scored 0.924 vs limma/∩ 0.884 vs random 0.807, with balanced micro/RNA AUCs (0.942/0.937) versus limma skew (0.966→0.884).

### 3.4 Channel ablation and weights

Leave-one-out on D1 shows Ec/Ex affect the metric and/or panel membership; Em/Ep are often saturated at small *k* (top genes already pass). Among 500 Dirichlet weight draws, equal-weight performance is near the best (gap ≈0.02); **Methods default remains equal weights**. Locked weights tuned on D1 transfer to D3 (locked RNA AUC 0.941 > equal 0.937).

### 3.5 BIOS-FDR

For *k* = 20 and *B* = 100 permutations: empirical BIOS-FDR = **0.0015**; mean top-20 BIOS = 0.907 vs null 0.631; empirical *P* = **0.0099**. We claim calibrated multi-channel enrichment, not superiority of FDR over extreme limma adj.*P* cutoffs.

### 3.6 Multi-dataset behaviour

| Cohort | Setting | BIOS | Limma/∩ | Random | Verdict |
|--------|---------|-----:|--------:|-------:|---------|
| D1 GSE89076×GSE50760 | merged, hard | **0.940** (RNA) | 0.883 | 0.759 | **WIN** |
| D2 GSE50760→GSE104836 | single RNA-seq external | 0.980 | 0.980 | 0.670 | **TIE** (ceiling) |
| D3 GSE9348×GSE50760 | merged, easier | 0.941–0.946 | 0.946 | 0.753 | **TIE**; ≫ random |

**Interpretation.** BIOS is most advantageous when platforms disagree; on easy single-assay or strongly separable merges it matches limma while remaining compact and biology-constrained.

### 3.7 Wet-lab literature validation

Against curated CRC genes with published qPCR/IHC/functional evidence:

| Panel | PMID-backed hits | Strong experimental hits |
|-------|-----------------:|-------------------------:|
| BIOS_v2 | **8/20 (40%)** | 4–5 |
| limma top-20 | 5/19 (26%) | 5 |
| random | ~0 | 0 |

Notable BIOS genes with wet-lab papers include BEST4, GUCA2B, OTOP2 (study used GSE50760), and CLDN1.

### 3.8 Concordance with recent CRC papers (2022–2024)

| Panel | Hits in recent-paper gene union |
|-------|--------------------------------:|
| BIOS_v2 | **8/20 (40%)** |
| limma | 3/20 (15%) |
| random | ≈0 |

BIOS recovers the full SPIB–AQP8–GUCA2B co-network; shares GUCA2B/CLCA4/CLDN1/MMP7 with a 2023 multi-GEO key-gene study using GSE9348+GSE50760; recovers CA7 from a 2023 ML signature built on GSE50760; and includes OTOP2 from a functional GSE50760 paper.

### 3.9 Compactness (clinical utility)

Unfiltered limma DEGs on training legs reach thousands of genes (e.g. ~7051). BIOS returns a user-chosen top-*k* (here 20) suitable for multiplex qPCR or targeted assays, while matching or beating matched-*k* limma on disease-linked criteria.

---

## 4. Discussion

### 4.1 What BIOS changes in GEO practice

| Common practice | BIOS / JPCT replacement |
|-----------------|-------------------------|
| DEG∩ across platforms | Joint `~ Condition + Platform` |
| Top-*P* or huge DEG lists as “biomarkers” | BIOS top-*k* multi-evidence ranking |
| Evaluating with the same AUCs used in ranking | Held-out assay evaluation |
| Uncalibrated composites | BIOS-FDR |

### 4.2 Merged versus single-type data

- **Merged microarray + RNA-seq:** strongest setting for BIOS; platform purity and held-out assay tests are defined.  
- **Single assay:** omit \(E_p\) / cross-assay \(E_x\); retain \(E_c,E_m,E_s\). Expect compact biology-aware panels; AUC may tie limma when signal is easy.

### 4.3 Novelty and limits (honest)

BIOS-Rank is a **novel ranking protocol / objective**, not a new DE likelihood. Components (limma, WGCNA, ML) are cited, not reinvented. Limits: CRC-focused real cohorts; D2/D3 AUC ceilings; literature wet-lab validation is not a new wet experiment in our laboratory; Em/Ep can look redundant at very small *k* due to saturation. Future work: non-CRC diseases, bootstrap panel stability, and RT-qPCR of top 8–12 genes on an independent tissue cohort.

### 4.4 Conclusions

BIOS-Rank converts joint differential expression into a **compact panel of disease-linked genes** that better survive platform transfer than limma top-*k* on hard merges, enrich published wet-lab and recent CRC signature genes, and remain calibrated under permutation. Panel size *k* is flexible. The method is ready for use whenever GEO users ask not only “which genes are significant?” but “which genes are useful disease biomarkers?”

---

## 5. Data and code availability

- GEO: GSE89076, GSE50760, GSE104836, GSE9348  
- Results: `GExPipe(original_paper)/results/` (`BIOS_v2_correctness.*`, `BIOS_wetlab_validation.*`, `BIOS_recent_paper_concordance.*`, `BIOS_FDR_*`, `BIOS_channel_ablation.*`, simulation outputs)  
- Formula: `manuscript/BIOS_FORMULA.md`  
- Software: https://github.com/safarafique/GExPipe  

---

## 6. Author contributions / acknowledgements

*[To be completed by authors.]*

---

## 7. Competing interests

*[To be completed.]*

---

## 8. Method box (journal highlight)

**Name:** BIOS-Rank (within JPCT)  

**Input:** ≥1 GEO expression matrices with Condition labels; ideally ≥2 platforms.  

**Steps:**  
1. Harmonize to HGNC; normalize within platform.  
2. Fit limma `~ Condition + Platform` (or Condition alone if single assay).  
3. Score genes with equal-weight BIOS (\(E_c,E_p,E_m,E_s\) ± \(E_x^{\mathrm{train}}\)).  
4. Take top-*k* (user-chosen); optionally control with BIOS-FDR.  
5. Evaluate on held-out assay / external cohort with matched-*k* and random controls.  

**Output:** Compact disease-linked gene panel + tables proving joint ≠ ∩ and BIOS ≥ limma on hard cross-platform merges.

---

## Supplementary pointers

| Supplement | Path |
|------------|------|
| Full formula | `manuscript/BIOS_FORMULA.md` |
| Channel ablation | `results/BIOS_channel_ablation.md` |
| Wet-lab gene–PMID map | `results/BIOS_wetlab_validation.md` |
| Recent-paper concordance | `results/BIOS_recent_paper_concordance.md` |
| Expert scorecard | `manuscript/EXPERT_SCORECARD_BIOS_RANK.md` |
| Usefulness checklist | `manuscript/USEFULNESS_CRITERIA_CHECKLIST.md` |

---

*Manuscript compiled from completed Original Paper analyses (2026-07-12). Convert to journal Word/LaTeX template for submission; add figures (workflow, D1 bar chart, LOO/weights, simulation FDR).*
