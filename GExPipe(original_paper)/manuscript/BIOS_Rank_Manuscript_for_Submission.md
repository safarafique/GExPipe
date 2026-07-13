# BIOS-Rank: Biological Invariance Orthogonal Selection Ranking for compact disease-linked gene panels from multi-platform GEO expression data

**Article type:** Original Research (Methods)  
**Suggested venues:** *Bioinformatics*, *BMC Bioinformatics*, *Briefings in Bioinformatics*, *Scientific Reports*  
**Software:** https://github.com/safarafique/BIOS-Rank  

---

## Title

**BIOS-Rank: a multi-evidence ranking method for compact, platform-aware biomarker panels from joint microarray and RNA-seq analysis of GEO data**

## Authors

*[Author list, affiliations, and corresponding author to be completed.]*

## Abstract

**Background.** Gene Expression Omnibus (GEO) studies are routinely combined by intersecting separate differentially expressed gene (DEG) lists across platforms, or by treating thousands of FDR-significant genes as biomarker candidates. Intersection is not a joint estimate of a shared disease effect under platform structure, and long DEG lists are rarely useful as experimental or clinical panels.

**Methods.** We introduce **BIOS-Rank** (*Biological Invariance Orthogonal Selection Ranking*), applied after limma joint modelling of Condition and Platform on harmonized multi-study expression matrices. Each gene is scored by equal-weighted evidence channels: condition strength (\(E_c\)), platform purity (\(E_p\)), trait-module membership (\(E_m\)), multi-selector stability (\(E_s\)), and optionally train-assay fidelity (\(E_x^{\mathrm{train}}\)). Soft panels are the top-*k* genes by BIOS score (*k* flexible; default *k* = 20 for matched comparisons). Evaluation is **non-circular**: ranking does not use held-out-assay AUC when that AUC is the success metric. **BIOS-FDR** calibrates the composite top-*k* list by Condition permutation. We compare BIOS to joint limma top-*k*, separate-study DEG intersection, and matched random genes on colorectal cancer (CRC) cohorts, and to an independent published 11-gene multi-GEO signature (Mosharaf et al., 2023).

**Results.** Separate DEG intersection and joint limma are not equivalent on GSE89076×GSE50760 (Jaccard = 0.37; 587 genes called only jointly). In simulation, joint(+Platform) sensitivity was 0.870 versus 0.264 for intersection. On the hard cross-platform merge, non-circular BIOS top-20 achieved held-out RNA-seq median gene AUC **0.940** versus limma **0.883** and random **0.759**, recovering **9/20** curated CRC markers versus **6/20**. Against PMID-backed wet-lab evidence, BIOS recovered **8/20** genes versus **5/19** for limma; against recent 2022–2024 CRC signature papers, **8/20** versus **3/20**. BIOS-FDR for the top-20 list was **0.0015** (empirical *P* = 0.0099). On a heterogeneous Affymetrix trio (GSE9348, GSE110224, GSE23878) merged with RNA-seq GSE50760, BIOS recovered **8/11** published key genes versus **3/11** for joint limma, with competitive cross-assay AUC (0.924 vs 0.930). On same-platform Affymetrix-only merges, BIOS ≈ limma. On easier cohorts, BIOS tied limma at high AUC while beating random.

**Conclusions.** BIOS-Rank converts joint differential expression into compact, disease-linked panels that improve literature concordance and cross-platform discrimination on heterogeneous merges, while remaining equivalent to limma when platforms are homogeneous. It addresses the practical question: not only which genes are significant, but which are useful disease biomarkers.

**Keywords:** biomarker ranking; cross-platform integration; GEO; limma; colorectal cancer; differential expression; multi-evidence ranking

---

## 1. Introduction

Public transcriptomic repositories such as GEO enable reuse of microarray and RNA-seq studies for disease biomarker discovery. A biologically meaningful disease gene should reflect **condition-driven** expression change that is coherent after technology (platform) structure is accounted for. In practice, multi-study “replication” is often implemented as: (i) differential expression on each study or platform separately, then (ii) intersection (∩) of significant DEG lists—or by nominating the entire FDR-significant set as a biomarker panel. Both habits conflate boolean agreement with joint estimation of shared biology, and confuse statistical significance with panel utility.

Separate-then-intersect fails in several ways. Power asymmetry means true disease genes may clear FDR on only one assay. There is no shared Condition estimand across platforms. Platform–Condition confounding is invisible to ∩. Consequently, intersection can be ultra-conservative yet still miss jointly coherent genes, while naïve merges without a Platform term can produce technology artefacts that look like disease signals.

Conversely, joint limma on a merged matrix (`~ Condition + Platform`) estimates a disease effect after adjusting for platform main effects, but the resulting DEG table is typically still thousands of genes. Experimental follow-up (qPCR, IHC, multiplex assays) requires a **compact** panel. Ranking solely by *P*-value or |logFC| does not explicitly reward platform purity, co-expression programme membership, or stability across selectors.

We do **not** claim novelty for limma, WGCNA, or random forests as algorithms. We introduce:

1. **BIOS-Rank** — a multi-evidence ranking objective that selects top disease-linked genes **after** joint differential expression.  
2. **BIOS-FDR** — phenotype-permutation calibration of the composite top-*k* score.  
3. A **non-circular evaluation protocol** for cross-platform panels (train-assay evidence for ranking; held-out assay for scoring).

**Falsifiable claims.** (C1) Separate ∩ ≠ joint limma gene sets. (C2) Under known truth, joint(+Platform) recovers more true DEGs than ∩ and controls FDR when confounded. (C3) On hard multi-platform merges, BIOS improves held-out fidelity and/or disease-gene enrichment versus matched-*k* limma and random. (C4) On homogeneous same-platform merges, BIOS is not expected to dominate limma. (C5) BIOS panels enrich independent wet-lab and recent-paper CRC genes beyond limma top-*k*.

---

## 2. Methods

### 2.1 Overview of the workflow

1. Map probes/genes to HGNC symbols; normalize **within** study/platform.  
2. Merge studies on common genes; fit the joint limma model (Eq. 1).  
3. Compute BIOS evidence channels (Eqs. 2–6) and the composite score (Eqs. 8–10).  
4. Form the soft panel as top-*k* by BIOS (*k* user-chosen; Eq. 11).  
5. Optionally estimate BIOS-FDR by Condition permutation (Eqs. 13–14).  
6. Evaluate with matched-*k* limma, separate ∩, and random baselines; use held-out assay AUC when platforms differ.

An R API mirrors limma usage: `biosRank(fit)` then `topBIOS(bios, n = k)`.

### 2.2 Datasets

| Role | Accessions | Assays | Contrast |
|------|------------|--------|----------|
| D1 hard merge | GSE89076 + GSE50760 | microarray + RNA-seq | Disease vs Normal |
| D2 external | train GSE50760 → test GSE104836 | RNA-seq | Disease vs Normal |
| D3 second merge | GSE9348 + GSE50760 | microarray + RNA-seq | Disease vs Normal |
| Homogeneous multi-GSE | GSE9348 + GSE110224 + GSE23878 | Affymetrix GPL570 | Disease vs Normal |
| Heterogeneous multi-GSE | Affy trio above × GSE50760 | microarray + RNA-seq | Disease vs Normal |

Phenotypes were assigned from title / source_name / characteristics fields only, avoiding free-text `data_processing` fields that falsely match the token “normal”.

**Independent literature panel.** Mosharaf et al. (*BMC Medical Genomics*, 2023; DOI 10.1186/s12920-023-01488-w) reported 11 CRC key genes after multi-GEO limma and PPI analysis: CXCL8, CEMIP, MMP7, CA4, ADH1C, GUCA2A, GUCA2B, ZG16, CLCA4, MS4A12, CLDN1. We use this list as an external concordance set (not as training labels).

### 2.3 Joint linear model (setup)

For gene \(g\) and sample \(i\) on the harmonized multi-platform matrix,

\begin{equation}
y_{gi}
=
\alpha_g
+
\beta_g^{C}\,C_i
+
\beta_g^{P}\,P_i
+
\varepsilon_{gi},
\end{equation}

where \(C_i\) is Condition (Disease vs Normal), \(P_i\) is Platform (e.g. Microarray vs RNA-seq), and \(\varepsilon_{gi}\) is residual error. Coefficients \(\widehat\beta_g^{C}\) and \(\widehat\beta_g^{P}\) and the BH-adjusted \(P\)-value \(p_g^{\mathrm{adj}}\) for \(\beta_g^{C}\) are obtained by limma–eBayes. For single-platform analyses the Platform term is omitted and \(E_p\equiv 1\) for all genes. Equation (1) is standard limma modelling and is **not** claimed as novel; it supplies the inputs to BIOS-Rank.

### 2.4 BIOS-Rank formulae (novel score)

#### 2.4.1 Evidence channels

For each gene \(g\) we define five evidence channels:

\begin{align}
E_c(g)
&=
\bigl(-\log_{10} p_g^{\mathrm{adj}}\bigr)
\cdot
\bigl|\widehat\beta_g^{C}\bigr|,
\\[0.5em]
E_p(g)
&=
\frac{\bigl|\widehat\beta_g^{C}\bigr|}
{\bigl|\widehat\beta_g^{C}\bigr|+\bigl|\widehat\beta_g^{P}\bigr|+\epsilon},
\\[0.5em]
E_m(g)
&=
\mathbf{1}\!\left\{g \in \mathcal{M}_{\mathrm{trait}}\right\},
\\[0.5em]
E_s(g)
&=
\begin{cases}
1 & \text{if } g \in \mathcal{S}_{\ge 2}\ \text{(selected by ≥2 ML / consensus methods)},\\
1/3 & \text{if } g \in \mathcal{M}_{\mathrm{trait}}\ \text{only},\\
0 & \text{otherwise},
\end{cases}
\\[0.5em]
E_x^{\mathrm{train}}(g)
&=
\mathrm{AUC}_{g}^{(\mathrm{train})},
\end{align}

with \(\epsilon=10^{-6}\). Here \(\mathcal{M}_{\mathrm{trait}}\) denotes genes in trait-associated WGCNA modules; \(\mathcal{S}_{\ge 2}\) denotes genes selected by at least two of LASSO / random forest / SVM-RFE (or an exported consensus list). \(\mathrm{AUC}_{g}^{(\mathrm{train})}\) is the single-gene ROC AUC on the **training assay only** (e.g. microarray when RNA-seq is held out). When WGCNA/ML lists are unavailable, \(E_m\) and \(E_s\) use DE-based proxies (strong BH-significant genes with large \(|\widehat\beta_g^{C}|\)), as implemented in `biosRank()`.

| Symbol | Biological meaning |
|--------|--------------------|
| \(E_c\) | Strength of the **disease** (Condition) effect |
| \(E_p\) | **Purity** of that effect versus platform/batch |
| \(E_m\) | Membership in a **phenotype-linked co-expression module** |
| \(E_s\) | **Stability** across machine-learning / consensus selectors |
| \(E_x^{\mathrm{train}}\) | Discrimination on the **train assay** (optional channel) |

#### 2.4.2 Scaling

Let \(\mathrm{scale}_{[0,1]}(\cdot)\) denote min–max scaling across genes:

\begin{equation}
\tilde E_c=\mathrm{scale}_{[0,1]}(E_c),\quad
\tilde E_p=\mathrm{scale}_{[0,1]}(E_p),\quad
\tilde E_x^{\mathrm{train}}=\mathrm{scale}_{[0,1]}(E_x^{\mathrm{train}}).
\end{equation}

Channels \(E_m\) and \(E_s\) already lie in \(\{0,\,1/3,\,1\}\) and are not rescaled.

#### 2.4.3 Master BIOS score (equal weights — default)

**Four-channel default** (recommended when no train-assay AUC is used in ranking):

\begin{equation}
\boxed{
\mathrm{BIOS}_4(g)
=
\frac{1}{4}
\Bigl(
\tilde E_c(g)
+
\tilde E_p(g)
+
E_m(g)
+
E_s(g)
\Bigr)
}
\end{equation}

**Five-channel form** (optional; train-assay fidelity only):

\begin{equation}
\boxed{
\mathrm{BIOS}_5(g)
=
\frac{1}{5}
\Bigl(
\tilde E_c(g)
+
\tilde E_p(g)
+
E_m(g)
+
E_s(g)
+
\tilde E_x^{\mathrm{train}}(g)
\Bigr)
}
\end{equation}

**Weighted generalization** (sensitivity analyses; equal weights remain the Methods default):

\begin{equation}
\mathrm{BIOS}_w(g)
=
\sum_{u\in\{c,p,m,s,x\}}
w_u\,\tilde E_u(g),
\qquad
w_u\ge 0,\;
\sum_u w_u=1.
\end{equation}

#### 2.4.4 Panel construction

**Soft discovery panel** (primary output):

\begin{equation}
\mathcal{P}_{\mathrm{soft}}
=
\operatorname{Top}\text{-}k
\bigl\{\,g : \mathrm{BIOS}(g)\,\bigr\}.
\end{equation}

**Hard consensus panel** (stricter JPCT mode):

\begin{equation}
\mathcal{P}_{\mathrm{hard}}
=
\operatorname{Top}\text{-}k
\Bigl\{
g :
E_m(g)=1,\;
E_s(g)=1,\;
\text{ranked by }\mathrm{BIOS}(g)
\Bigr\}.
\end{equation}

Panel size \(k\) is user-chosen (typical range 8–50). We report \(k=20\) for matched comparisons with limma and random baselines.

#### 2.4.5 Non-circular evaluation protocol

To avoid circularity between ranking and the primary success metric:

1. **Ranking:** use \(\mathrm{BIOS}_4\), or \(\mathrm{BIOS}_5\) with \(E_x^{\mathrm{train}}\) only (never held-out-assay AUC in the score).  
2. **Evaluation:** median single-gene AUC on the **held-out assay** (e.g. rank without RNA AUC → score RNA-seq AUC).  
3. **Weights:** equal by default; locked weights may be tuned on D1 held-out AUC and tested on D3.  
4. **Orthogonal biology** (CRC markers, wet-lab PMIDs, published signatures) is reported separately and is **not** part of the ranking objective.

A circular baseline that ranks by \(\min(\mathrm{AUC}^{(A)},\mathrm{AUC}^{(B)})\) using both assays is shown only to illustrate inflation risk and is **not** used for primary claims.

### 2.5 BIOS-FDR (permutation calibration)

Let \(t_{(k)}\) be the \(k\)-th largest **observed** BIOS score. For permutation \(b=1,\ldots,B\) (\(B=100\) in this study):

1. Shuffle Condition labels \(C^{(b)}\) (keep Platform \(P\) fixed).  
2. Refit Eq. (1); recompute \(E_c^{(b)}\), \(E_p^{(b)}\), and (if used) \(E_x^{(b)}\).  
3. Randomly reassign \(E_m\) and \(E_s\) across genes.  
4. Form \(\mathrm{BIOS}^{(b)}(g)\).  
5. Count null discoveries \(N^{(b)}=\#\{\,g:\mathrm{BIOS}^{(b)}(g)\ge t_{(k)}\,\}\).

Empirical false-discovery rate of the top-\(k\) list:

\begin{equation}
\boxed{
\widehat{\mathrm{FDR}}_{\mathrm{BIOS}}(k)
=
\min\!\left(
1,\;
\frac{1}{kB}\sum_{b=1}^{B} N^{(b)}
\right).
}
\end{equation}

Enrichment test for the mean top-\(k\) score:

\begin{equation}
p_{\mathrm{emp}}
=
\frac{
1+\sum_{b=1}^{B}
\mathbf{1}\!\left\{
\overline{\mathrm{BIOS}}^{(b)}_{\mathrm{top}\,k}
\ge
\overline{\mathrm{BIOS}}_{\mathrm{top}\,k}
\right\}
}{1+B}.
\end{equation}

### 2.6 Baselines and orthogonal checks

Baselines: merged limma top-*k*, separate-study ∩ ranked by mean |logFC|, single-channel ranks, and matched random genes. Orthogonal biology: curated CRC markers; PMID-backed wet-lab literature (qPCR/IHC/functional); concordance with independent 2022–2024 CRC signature papers; recovery of the Mosharaf et al. 11-gene set.

### 2.7 Simulation

Known-truth simulations (2000 genes, 100 true DEGs; balanced and Platform–Condition confounded designs; 50 replicates) compared separate ∩, joint Condition-only, and joint Condition+Platform.

### 2.8 Implementation and reproducibility

Analyses and scripts are provided under the BIOS-Rank repository (`scripts/`, `R/biosRank.R`). Key scripts include `bios-rank-v2-correctness.R`, `bios-fdr.R`, `bios-channel-ablation.R`, `bios-wetlab-validation.R`, `bios-recent-paper-concordance.R`, `compare-paper-multigse-bios.R`, `compare-paper-heterogeneous-bios.R`, and `simulate-joint-vs-intersect.R`. The formal formula reference is also archived in `manuscript/BIOS_FORMULA.md`.

---

## 3. Results

### 3.1 Separate intersection is not a joint analysis (C1)

On GSE89076 + GSE50760:

| Metric | Value |
|--------|------:|
| Separate ∩ DEGs | 1811 |
| Joint limma DEGs | 1464 |
| Genes only in joint call | **587** |
| Jaccard(∩, joint) | **0.37** |
| Direction concordance among shared calls | **98.9%** |
| logFC Pearson *r* | **0.989** |

Hundreds of genes with a coherent joint Condition effect are invisible to ∩, while shared calls remain directionally concordant. Intersection therefore cannot substitute for joint modelling.

### 3.2 Simulation supports the joint estimand (C2)

| Setting | Method | Sensitivity | Empirical FDR |
|---------|--------|------------:|--------------:|
| Balanced, harmonized | Separate ∩ | 0.264 | ≈0 |
| Balanced, harmonized | Joint(+Platform) | **0.870** | 0.038 |
| Confounded, raw | Joint Condition-only | 0.760 | **0.962** |
| Confounded, raw | Joint(+Platform) | 0.736 | **0.051** |

Without a Platform term under confounding, disease calls are dominated by technology artefacts.

### 3.3 BIOS-Rank on the hard cross-platform merge (C3)

Primary metric: median gene AUC on **held-out RNA-seq**, *k* = 20 (non-circular).

| Method | Med AUC RNA | Curated CRC markers |
|--------|------------:|--------------------:|
| **BIOS_v2 (equal weights; no held-out Ex)** | **0.940** | **9/20 (45%)** |
| BIOS_v2 with \(E_x^{\mathrm{train}}\) (locked) | 0.940 | 9/20 (45%) |
| \(E_c\) only | 0.935 | 8/20 |
| Merged limma top-20 | 0.883 | 6/20 (30%) |
| Circular Ex using both assays | 0.954 | 4/20 (20%) |
| Random matched-*k* | 0.759 | 0 |

BIOS improves held-out fidelity and marker recovery versus limma. Circular Ex-only can inflate AUC while losing biology—motivating the non-circular protocol.

Using a historical min-median cross-assay metric, BIOS also scored **0.924** versus limma/∩ **0.884** and random **0.807**, with balanced microarray/RNA AUCs (≈0.942/0.937) versus limma skew (0.966 → 0.884).

### 3.4 Channel ablation and weights

Leave-one-out ablation on D1 shows that \(E_c\) and \(E_x^{\mathrm{train}}\) affect the metric and/or panel membership; \(E_m\) and \(E_p\) are often saturated among top genes at small *k*. Among 500 Dirichlet weight draws, equal-weight performance is near the best (gap ≈ 0.02). **Methods default remains equal weights.** Locked weights tuned on D1 transferred to D3 (locked RNA AUC 0.941 > equal 0.937).

### 3.5 BIOS-FDR calibration

For *k* = 20 and *B* = 100 permutations: empirical BIOS-FDR = **0.0015**; mean top-20 BIOS = 0.907 versus null mean 0.631; empirical *P* = **0.0099**. We claim calibrated multi-channel enrichment of the composite list, not that BIOS-FDR dominates extreme limma adj.*P* cutoffs for every gene.

### 3.6 Multi-dataset behaviour (C3–C4)

| Cohort | Setting | BIOS | Limma / ∩ | Random | Verdict |
|--------|---------|-----:|----------:|-------:|---------|
| D1 GSE89076×GSE50760 | merged, hard | **0.940** (RNA) | 0.883 | 0.759 | **BIOS wins** |
| D2 GSE50760→GSE104836 | single-platform external | 0.980 | 0.980 | 0.670 | **Tie** (ceiling) |
| D3 GSE9348×GSE50760 | merged, easier | 0.941–0.946 | 0.946 | 0.753 | **Tie**; ≫ random |

BIOS is most advantageous when platforms disagree; on easy single-assay or strongly separable merges it matches limma while remaining compact and biology-constrained.

### 3.7 Concordance with a published multi-GSE CRC signature

**Homogeneous Affymetrix-only** (GSE9348, GSE110224, GSE23878; *k* = 20):

| Method | Hits in published 11 key genes |
|--------|-------------------------------:|
| Paper-style ∩ top-20 | **8/11** |
| Joint limma top-20 | 3/11 |
| BIOS-Rank top-20 | 3/11 (identical to limma) |

On same-platform Affymetrix merges, BIOS collapses toward joint limma, as expected when platform purity is uninformative.

**Heterogeneous Affymetrix × RNA-seq** (same Affy trio × GSE50760; *k* = 20):

| Method | Hits / 11 KG | MinMedian Micro↔RNA AUC | Med AUC RNA |
|--------|-------------:|------------------------:|------------:|
| Paper-style ∩ | 7 | 0.891 | 0.918 |
| Joint limma `~ Condition + Platform` | 3 | **0.930** | **0.937** |
| **BIOS-Rank** | **8** | 0.924 | 0.929–0.934 |

BIOS and limma panels diverge (overlap 7–8/20). BIOS recovers eight published key genes (CA4, CEMIP, CLCA4, CLDN1, GUCA2A, GUCA2B, MS4A12, ZG16). Joint limma slightly leads raw transfer AUC; BIOS leads literature concordance while remaining close on transfer. Paper-style ∩ recovers many of the paper’s own genes (as expected from an ∩-based discovery pipeline) but transfers worse across platforms.

### 3.8 Wet-lab literature and recent-paper concordance (C5)

Against curated CRC genes with published qPCR/IHC/functional evidence:

| Panel | PMID-backed hits |
|-------|-----------------:|
| BIOS_v2 | **8/20 (40%)** |
| limma top-20 | 5/19 (26%) |
| random | ≈0 |

Notable BIOS genes with wet-lab support include BEST4, GUCA2B, OTOP2 (functional study on GSE50760), and CLDN1.

Against a union of genes from independent recent CRC signature papers (2022–2024):

| Panel | Hits in recent-paper union |
|-------|---------------------------:|
| BIOS_v2 | **8/20 (40%)** |
| limma | 3/20 (15%) |
| random (mean) | ≈0.03 |

BIOS recovers the SPIB–AQP8–GUCA2B co-network; shares multiple genes with the 2023 multi-GEO key-gene study; recovers CA7 from a 2023 ML signature on GSE50760; and includes OTOP2 from a functional GSE50760 paper.

### 3.9 Compactness

Unfiltered limma DEG tables on training legs often exceed thousands of genes. BIOS returns a user-chosen top-*k* (here 20) suitable for multiplex qPCR or targeted assays, while matching or beating matched-*k* limma on disease-linked criteria in the settings above.

---

## 4. Discussion

### 4.1 Practical guidance

| Analysis setting | Recommended use |
|------------------|-----------------|
| Heterogeneous multi-platform merge (microarray × RNA-seq) | **BIOS-Rank** after joint limma `~ Condition + Platform` |
| Homogeneous same-platform multi-GSE | Joint limma; BIOS ≈ limma (use either) |
| Reproduce an ∩-based published gene list | Paper-style intersection (+ network steps if used in the paper) |
| Single assay, easy separation | limma or BIOS; expect AUC ties; BIOS still yields a compact biology-aware shortlist |

### 4.2 What changes in GEO practice

| Common practice | BIOS replacement |
|-----------------|------------------|
| DEG ∩ across platforms as “replication” | Joint `~ Condition + Platform` |
| Huge DEG lists as “biomarkers” | BIOS top-*k* multi-evidence ranking |
| Evaluating with the same AUCs used in ranking | Held-out assay evaluation |
| Uncalibrated composite scores | BIOS-FDR |

### 4.3 Novelty and limitations (honest)

BIOS-Rank is a **novel ranking protocol / objective**, not a new differential-expression likelihood. Components (limma, WGCNA, ML selectors) are cited, not reinvented. The defendable contribution is the **platform-aware multi-evidence ranking package** with non-circular evaluation and permutation calibration of the composite top-*k*.

Limitations: real-data cohorts are CRC-focused; some cohorts hit AUC ceilings (D2/D3); wet-lab concordance uses published PMIDs rather than new experiments from this study; \(E_m\)/\(E_p\) can appear saturated at very small *k*. Future work should include non-CRC diseases, bootstrap panel stability, reverse train/test platform direction, and RT-qPCR of top 8–12 genes on an independent tissue cohort.

### 4.4 Relation to GExPipe

GExPipe is a separate Application Note describing interactive GEO workflow software. BIOS-Rank is an Original Methods contribution with a limma-like R API and is intended to be cited independently.

---

## 5. Conclusions

Across real multi-GSE colorectal cohorts, **BIOS-Rank yields compact panels that recover more literature-supported disease genes than joint limma top-*k* on cross-platform merges and improve or match cross-assay discrimination versus DEG intersection, while remaining equivalent to limma when platforms are homogeneous.** Panel size *k* is flexible. The method is ready for use whenever analysts ask not only “which genes are significant?” but “which genes are useful disease biomarkers?”

---

## 6. Data and code availability

- **GEO accessions:** GSE89076, GSE50760, GSE104836, GSE9348, GSE110224, GSE23878  
- **Independent signature:** Mosharaf et al., *BMC Med Genomics* (2023), DOI [10.1186/s12920-023-01488-w](https://doi.org/10.1186/s12920-023-01488-w)  
- **Code and results:** https://github.com/safarafique/BIOS-Rank  
- **Key result tables:** `results/BIOS_v2_correctness.*`, `BIOS_FDR_*`, `BIOS_wetlab_validation.*`, `BIOS_recent_paper_concordance.*`, `paper_multigse_compare/`, `paper_heterogeneous_compare/`  
- **Formula reference:** `manuscript/BIOS_FORMULA.md`

---

## 7. Author contributions

*[CRediT statement to be completed by authors.]*

## 8. Acknowledgements

*[To be completed.]*

## 9. Competing interests

The authors declare that they have no competing interests. *[Confirm before submission.]*

## 10. Funding

*[To be completed.]*

---

## 11. References

1. Ritchie ME, et al. limma powers differential expression analyses for RNA-sequencing and microarray studies. *Nucleic Acids Res.* 2015;43:e47.  
2. Langfelder P, Horvath S. WGCNA: an R package for weighted correlation network analysis. *BMC Bioinformatics.* 2008;9:559.  
3. Barrett T, et al. NCBI GEO: archive for functional genomics data sets—update. *Nucleic Acids Res.* 2013;41:D991–D995.  
4. Mosharaf MP, et al. Computational identification of hub genes and potential key pathways for the cellular transition from colorectal adenoma to carcinoma. *BMC Med Genomics.* 2023;16:88. DOI: 10.1186/s12920-023-01488-w.  
5. Love MI, Huber W, Anders S. Moderated estimation of fold change and dispersion for RNA-seq data with DESeq2. *Genome Biol.* 2014;15:550.  
6. Robinson MD, McCarthy DJ, Smyth GK. edgeR: a Bioconductor package for differential expression analysis of digital gene expression data. *Bioinformatics.* 2010;26:139–140.  
7. Johnson WE, Li C, Rabinovic A. Adjusting batch effects in microarray expression data using empirical Bayes methods. *Biostatistics.* 2007;8:118–127.  
8. Szklarczyk D, et al. The STRING database in 2023. *Nucleic Acids Res.* 2023;51:D638–D646.  
9. GEO Series: GSE89076, GSE50760, GSE104836, GSE9348, GSE110224, GSE23878 (NCBI Gene Expression Omnibus).  

*[Expand reference list with wet-lab and 2022–2024 CRC signature PMIDs cited in `results/BIOS_wetlab_validation.md` and `results/BIOS_recent_paper_concordance.md` before journal submission.]*

---

## Figure legends (to prepare for submission)

**Figure 1.** BIOS-Rank workflow and core formulae: within-study normalization → merge → limma Eq. (1) → evidence channels Eqs. (2)–(6) → \(\mathrm{BIOS}_4/\mathrm{BIOS}_5\) Eqs. (8)–(9) → top-*k* panel Eq. (11) → held-out assay evaluation / BIOS-FDR Eqs. (13)–(14).

**Figure 2.** Separate ∩ versus joint limma on GSE89076×GSE50760 (set sizes, Jaccard, genes unique to joint).

**Figure 3.** D1 panel comparison: held-out RNA-seq median AUC and CRC marker recovery for BIOS, limma, and random (*k* = 20).

**Figure 4.** Heterogeneous multi-GSE comparison versus the Mosharaf et al. 11-gene signature: hits and cross-assay AUC for ∩, joint limma, and BIOS.

**Figure 5.** Simulation: sensitivity and empirical FDR for ∩ versus joint(+Platform) under balanced and confounded designs.

**Supplementary Figure S1.** Channel ablation / weight sensitivity on D1.  
**Supplementary Figure S2.** BIOS-FDR null distribution of mean top-*k* scores.  
**Supplementary Tables.** Full gene panels, wet-lab PMID maps, recent-paper concordance, homogeneous Affy-only comparison.

---

## Method box (journal highlight)

**Name:** BIOS-Rank  

**Core formulae:**  
Eq. (1) joint limma; Eqs. (2)–(6) evidence channels \(E_c,E_p,E_m,E_s,E_x^{\mathrm{train}}\);  
Eqs. (8)–(9) \(\mathrm{BIOS}_4\) / \(\mathrm{BIOS}_5\); Eq. (11) top-*k* panel; Eqs. (13)–(14) BIOS-FDR.  

**Input:** One or more expression matrices with Disease/Normal labels; ideally ≥2 platforms.  

**Steps:**  
1. Harmonize to HGNC; normalize within study.  
2. Fit limma `~ Condition + Platform` (or Condition alone).  
3. Score genes with equal-weight BIOS (\(E_c,E_p,E_m,E_s\) ± \(E_x^{\mathrm{train}}\)).  
4. Take top-*k*; optionally control with BIOS-FDR.  
5. Evaluate on held-out assay / external cohort with matched-*k* and random controls.  

**Output:** Compact disease-linked gene panel with evidence that joint ≠ ∩ and that BIOS improves literature concordance and/or cross-platform discrimination on heterogeneous merges.

---

*Manuscript compiled for publication from completed BIOS-Rank analyses (2026-07-12). Convert to the target journal’s Word/LaTeX template; insert figures; complete author/funding metadata; expand references from result PMIDs before submission.*
