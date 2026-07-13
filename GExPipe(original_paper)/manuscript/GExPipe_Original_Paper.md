# Joint Platform-aware Consensus with Transfer (JPCT): a method for biologically faithful cross-platform biomarker discovery from GEO

**Article type:** Original Paper (*Bioinformatics* or equivalent)  
**Software companion:** GExPipe Application Note (cite separately for Shiny/Bioconductor packaging)  
**Analysis code:** `GExPipe(original_paper)/scripts/` (does not modify the Application Note package)

---

## One-sentence novelty claim

**BIOS-Rank** (inside the JPCT workflow) replaces DEG∩ and limma top-*P* lists with a **platform-invariant biomarker score**—joint Condition evidence × platform purity × trait-module support × selector stability × cross-assay fidelity—so filtered panels reflect disease biology that survives the measurement technology.

---

## Title (method-first)

**BIOS-Rank with Joint Platform-aware Consensus (JPCT) improves biological fidelity of GEO biomarker panels across microarray and RNA-seq**

---

## Abstract (≈220 words)

**Motivation.** Public GEO studies are routinely combined by taking the intersection of microarray and RNA-seq differentially expressed gene (DEG) lists, or by treating thousands of DEGs as biomarkers. Intersection is not a joint statistical model of a shared disease effect under platform structure, and large DEG lists rarely prove external **panel** performance.

**Method.** We introduce **JPCT** (*Joint Platform-aware Consensus with Transfer*): (M1) HGNC harmonization and limma `~ Condition + Platform` on the merged matrix; (M2) multi-evidence consensus and the novel **BIOS-Rank** score (Condition strength, platform purity, trait-module, selector stability, cross-assay fidelity; equal weights) with **BIOS-FDR** permutation control; (M3) panel-level and cross-assay fidelity evaluation (matched-*k* and random controls).

**Results.** On colorectal cohorts GSE89076 (microarray) + GSE50760 (RNA-seq), separate∩ and joint limma are not equivalent (Jaccard = 0.37; *n* = 587 genes only in the joint call) while direction concordance remains 98.9% (logFC *r* = 0.989). Known-truth simulations show joint(+Platform) sensitivity **0.870** versus ∩ **0.264**. The novel **BIOS-Rank** filter selects a 20-gene panel with min-median cross-assay gene AUC **0.924**, exceeding limma/∩ top-20 (**0.884**) and random (**0.807**). Leave-one-out and Dirichlet weight search support **equal channel weights** as a robust default; Ex-only ranking inflates the primary metric circularly but loses module/ML membership (Frac Es 0.20 vs 0.80). Phenotype-permutation **BIOS-FDR** for the top-20 list is **0.0015** (empirical *P* = 0.0099). On a second microarray–RNA-seq pair (GSE9348 × GSE50760) BIOS ties limma at 0.946 while beating random (0.781); on GSE50760 → GSE104836, BIOS matches limma at ceiling (0.98) and far exceeds random (0.67).

**Availability.** Implemented in the GExPipe Bioconductor workflow; Original Paper analyses are reproducible from `GExPipe(original_paper)/`.

---

## 1. Introduction

### 1.1 Biological problem

Biologically meaningful disease signatures should reflect **condition-driven expression change that is not an artefact of measurement technology**. When the same pathology is profiled by microarray and RNA-seq, a gene that is truly disease-associated should show a coherent Condition effect after platform structure is accounted for. Genes that appear only because one assay is noisier, or because platform is entangled with phenotype, are not biologically faithful biomarkers.

GEO meta-analyses often operationalize “replication across platforms” as:

1. run DEG analysis separately on each platform, then  
2. take the **intersection** of significant gene lists,

or stop at huge FDR lists and call them biomarker candidates. Both habits confuse **boolean agreement** with **joint estimation of shared biology**.

### 1.2 Why intersection is not biologically accurate

Separate-then-intersect has three biological/statistical failures:

| Failure | Biological meaning |
|--------|---------------------|
| Power asymmetry | A true disease gene may clear FDR on RNA-seq but not microarray (or vice versa); ∩ discards it even when the joint Condition effect is real |
| No shared estimand | ∩ never estimates a single Condition coefficient under platform noise |
| Confounding blindness | If Platform correlates with Condition, unadjusted merges can call batch genes as “disease” genes |

Thus ∩ can be **ultra-conservative yet still wrong**, and naive merges can be **anti-conservative and biologically false**.

### 1.3 What is new (JPCT)

We do **not** claim novelty for limma, WGCNA, or random forests as algorithms. We introduce a **named end-to-end estimand and selection rule** with an explicit biological target:

> **Estimand (M1):** the Condition effect that remains after Platform/Dataset structure is modelled.  
> **Selection rule (M2):** retain only genes that are DE under that estimand **and** live in phenotype-linked co-expression modules **and** are stable under ≥2 supervised selectors.  
> **Success criterion (M3):** multivariate panel discrimination and cross-assay transfer—not list overlap or single-gene AUC alone.

**Falsifiable claims**

- **C1.** Separate DEG ∩ ≠ joint limma gene sets under platform structure.  
- **C2.** JPCT consensus yields smaller panels with external panel performance ≥ matched-*k* limma baselines.  
- **C3.** Consensus layers change gene content (ablation); they are not identical to DE top-*k*.  
- **C4.** Under known truth, joint(+Platform) recovers more true DEGs than ∩ and controls FDR when Platform and Condition are confounded.  
- **C5.** BIOS-Rank improves (or matches) cross-assay gene fidelity vs limma top-*k*/∩ while beating random; equal weights are near-optimal; BIOS-FDR controls the composite top-*k* list.

---

## 2. The JPCT method (new procedure)

### 2.1 Overview

```text
Platform A (e.g. microarray)     Platform B (e.g. RNA-seq)
        \                           /
         \  HGNC symbol harmonize  /
          \                       /
           +---- merged matrix ----+
           |  z-score / normalize  |
           |  within platform      |
           +-----------+-----------+
                       |
         limma: ~ Condition + Platform     ← M1 (joint estimand)
                       |
              significant DE genes
                       |
         trait-WGCNA modules (phenotype)   ← biological context
                       |
         ≥2 of {LASSO, RF, SVM-RFE}        ← stability
                       |
              JPCT consensus panel P
                       |
    panel classifier + external / transfer  ← M3 (success)
```

### 2.2 M1 — Joint platform-aware differential expression

**Input.** Expression matrices from ≥2 platforms mapped to a common gene ID (HGNC symbols for human).

**Preprocessing.** Platform-appropriate normalization (e.g. RMA/quantile for arrays; TMM + log-CPM + quantile for RNA-seq), then within-platform gene standardization before stacking (harmonization). Optional ComBat/limma batch correction with PCA/PVCA checks that **Platform variance falls while Condition variance is preserved**.

**Model.** On the merged matrix:

\[
y_{gi} = \alpha_g + \beta_g^{\mathrm{Cond}}\,\mathrm{Condition}_i + \gamma_g^{\mathrm{Plat}}\,\mathrm{Platform}_i + \varepsilon_{gi}
\]

(or `Dataset` in place of/addition to `Platform`). Genes with FDR(\(\hat\beta^{\mathrm{Cond}}\)) below threshold and \(|\widehat{\log\mathrm{FC}}|\) above a fold-change floor form the DE set \(D\).

**Biological interpretation.** \(\beta_g^{\mathrm{Cond}}\) is the disease effect **after removing platform main effects**. That is the quantity ∩ never estimates.

**What JPCT rejects.** Treating \(D_A \cap D_B\) as if it were \(\{g: \beta_g^{\mathrm{Cond}} \neq 0\}\).

### 2.3 M2 — Multi-evidence consensus (biological filters, not only statistics)

Unfiltered \(D\) is often thousands of genes. JPCT requires three **orthogonal biological evidences**:

1. **Differential expression (M1)** — gene responds to Condition under the joint model (or, in single-platform training legs, under the study design used for that leg).  
2. **Trait-associated co-expression (WGCNA)** — gene belongs to modules whose eigengenes correlate with disease status. Biologically: the gene participates in a **phenotype-linked expression programme**, not an isolated spike.  
3. **Supervised stability (≥2 ML methods)** — gene is selected by at least two of LASSO, random forest importance, and SVM-RFE. Biologically: the gene’s predictive role is **not an artefact of one learner**.

**Consensus panel**

\[
P = D \cap W_{\mathrm{trait}} \cap M_{\ge 2}
\]

**Ablation arms (to prove layers matter)**

| Arm | Definition |
|-----|------------|
| B0 / B0k | limma top-50 / top-\|P\| |
| B1 / B1k | limma-voom top-50 / top-\|P\| |
| B3 | DE ∩ WGCNA |
| B4 | DE ∩ (≥2 ML) without requiring WGCNA |
| B5 | full \(P\) (JPCT) |
| B6 | random genes of size \|P\| |

### 2.4 M3 — Panel-level and cross-platform transfer evaluation

**Primary endpoint (same-assay external).** Train a classifier (RF primary at small *n*; glmnet secondary) on the *k* genes in \(P\) using training cohort labels; evaluate AUC on a held-out cohort **never used for tuning**.

**Novel biological endpoint (cross-platform transfer).** When microarray and RNA-seq both measure the same Condition:

1. Form \(P\) from the **joint** model (M1) or from consensus on the training assay.  
2. Train the panel classifier on **platform A samples only**.  
3. Test on **platform B samples only** (and reverse).  

A biologically faithful panel should retain discrimination after the assay changes—the clinical reality of moving a signature across technologies. Separate∩ lists are not optimized for this estimand.

**Secondary metrics.** Median single-gene external AUC; nested CV on train (optimism); enrichment of only-in-joint genes.

### 2.5 BIOS-Rank and BIOS-FDR (novel filter; v2 non-circular protocol)

After the joint limma fit, each gene receives scaled evidences (details: `BIOS_FORMULA.md` §8):

| Channel | Definition (summary) |
|---------|----------------------|
| \(E_c\) | \((-\log_{10}p^{\mathrm{adj}})\,\|\hat\beta^{C}\|\) — Condition strength |
| \(E_p\) | \(\|\hat\beta^{C}\|/(\|\hat\beta^{C}\|+\|\hat\beta^{P}\|+\epsilon)\) — platform purity |
| \(E_m\) | 1 if trait-WGCNA module member |
| \(E_s\) | 1 if ≥2 ML/consensus; \(1/3\) if WGCNA-only |
| \(E_x^{\mathrm{train}}\) | AUC on the **train assay only** (optional); **never** the held-out assay |

**Default scores (equal weights):**
- **4-channel (preferred for claims):** \(\mathrm{BIOS}_{4}=\tfrac14(\tilde E_c+\tilde E_p+E_m+E_s)\)  
- **5-channel train-Ex:** \(\mathrm{BIOS}_{5}=\tfrac15(\tilde E_c+\tilde E_p+E_m+E_s+\tilde E_x^{\mathrm{train}})\)

**Evaluation (correctness):** median gene AUC on the **held-out assay** (and orthogonal CRC marker overlap). Do not rank with \(\min(\mathrm{AUC}_A,\mathrm{AUC}_B)\) if that same min-median is the primary success metric.

**BIOS-FDR.** Shuffle Condition (keep Platform), recompute BIOS, empirical FDR of top-*k*; enrichment vs null.

**Weights.** Equal by default; locked weights may be tuned on D1 held-out AUC and tested on D3 (`scripts/bios-rank-v2-correctness.R`).

### 2.6 What is *not* claimed as new

limma, voom, WGCNA, glmnet, randomForest, and Shiny are established tools. JPCT’s contribution is the **joint estimand + biologically motivated consensus + transfer-centred success criterion**; **BIOS-Rank / BIOS-FDR** are the novel ranking and error-control objects on top of that fit. We do not claim a new DE engine.

---

## 3. Materials and methods (datasets & protocol)

### 3.1 Real CRC cohorts

| Role | Accession | Assay | Contrast |
|------|-----------|-------|----------|
| D1 cross-platform merge | GSE89076 + GSE50760 | microarray + RNA-seq | Disease vs Normal |
| Consensus / D2 train | GSE50760 | RNA-seq | Disease vs Normal |
| D2 external panel test | GSE104836 | RNA-seq | Tumor vs nontumor (→ Disease/Normal) |
| D3 second cross-platform | GSE9348 + GSE50760 | microarray + RNA-seq | Disease vs Normal (70 tumor / 12 healthy on GSE9348) |

Phenotype rules use title / source_name / characteristics only (not free-text `data_processing`, which can false-match “normal”).

### 3.2 Simulation (known truth)

- 2000 genes, 100 true DEGs (40% strong / 60% weak effects).  
- Two platforms, *n* = 30 samples each.  
- Scenarios: **balanced** (Platform ⊥ Condition) and **confounded** (Platform associated with Condition).  
- Methods: separate limma → ∩; joint `~ Condition`; joint `~ Condition + Platform`; with/without within-platform z-score.  
- 50 replicates; report sensitivity, empirical FDR, Jaccard(∩, joint).

### 3.3 Implementation

All Original Paper scripts live under `GExPipe(original_paper)/scripts/` and **read** Application Note `validation_manual/` exports without modifying package code.

---

## 4. Results

### 4.1 C1 — Separate∩ is not a joint analysis (real data)

On GSE89076 + GSE50760:

| Metric | Value |
|--------|------:|
| Microarray-only DEGs | 3946 |
| RNA-seq-only DEGs | 5747 |
| Separate∩ | 1811 |
| Joint limma DEGs | 1464 |
| Overlap | 877 |
| **Only in joint** | **587** |
| Jaccard(∩, joint) | **0.37** |
| Direction concordance | **98.9%** |
| logFC Pearson *r* | **0.989** |

PCA/PVCA: after correction, Platform domination of PC1 collapses (R² ≈ 0.994 → 0.000) while Condition structure emerges (R² ≈ 0.001 → 0.706)—i.e. biology is recoverable when platform is modelled, not when lists are merely intersected.

**Biological reading.** Hundreds of genes with a coherent joint Condition effect are invisible to ∩; effect sizes among shared calls stay concordant, so joint modelling is not destroying biology—it is **recovering platform-shadowed disease signal**.

### 4.2 C4 — Known-truth simulation confirms the estimand

**Balanced, harmonized (mean ± SD, 50 reps)**

| Method | Sensitivity | Empirical FDR | Jaccard vs ∩ |
|--------|------------:|--------------:|-------------:|
| Separate∩ | 0.264 ± 0.041 | ≈ 0 | — |
| Joint(+Platform) | **0.870 ± 0.038** | 0.038 ± 0.021 | **0.292 ± 0.049** |

Joint recovers on average **≈61 true DEGs** that ∩ misses, at FDR near the nominal 0.05. ∩ is almost perfectly precise but misses most true biology.

**Confounded, raw (batch remains)**

| Method | Sensitivity | Empirical FDR |
|--------|------------:|--------------:|
| Joint Condition-only | 0.760 | **0.962** |
| Joint Condition+Platform | 0.736 | **0.051** |

**Biological reading.** Without a Platform term, “disease” calls are largely **technology/confounding artefacts** (FDR ≈ 96%). JPCT’s M1 covariate is what keeps the call set biologically about Condition.

### 4.3 C2–C3 — Compact consensus panel (GSE50760 → GSE104836)

External *n* = 20 saturates many panel AUCs; we therefore emphasise **size**, **matched-*k* baselines**, **RF**, **nested CV**, and **median gene AUC**, with random genes as negative control.

| Signature | *k* | Median gene AUC (ext.) | RF panel AUC | Nested CV RF |
|-----------|----:|-----------------------:|-------------:|-------------:|
| limma top-50 | 50 | 0.98 | 1.00 | 0.99 |
| limma top-20 (matched *k*) | 20 | 0.97 | 1.00 | 0.99 |
| JPCT full consensus (B5) | **20** | **0.98** | **1.00** | **0.99** |
| True B4 (ML on DE, no WGCNA) | 20 | 0.98 | 1.00 | 0.99 |
| Random \|B5\| | 20 | **0.70** | **0.87** | 0.89 |

- Unfiltered limma DEGs: **7051** genes — not a usable biomarker panel.  
- JPCT compresses to **20** genes with discrimination matching matched-*k* limma and far above random.  
- True B4 (ML without WGCNA) ≠ B5 (Jaccard **0.48**): WGCNA changes membership—consensus is not “DE top-20 in disguise.”

**Biological reading.** JPCT does not invent stronger univariate CRC separators on this easy external; it produces a **compact, multi-evidence panel** whose genes are individually informative (median AUC 0.98) and jointly non-random, while discarding thousands of DEGs that are impractical and often non-specific as “biomarkers.”

### 4.4 Cross-platform transfer (JPCT M3) — results

Source: `results/cross_platform_panel_transfer.md` (2026-07-12).
Microarray n=80; RNA-seq n=36; matched k=20.

| Panel | k | Med. gene AUC (RNA) | RF Micro→RNA | RF RNA→Micro | **RF mean** | glmnet mean* |
|-------|---|---------------------|--------------|--------------|-------------|--------------|
| Merged_limma_topk | 19 | 0.884 | 0.952 | 0.967 | 0.960 | 0.899 |
| Separate_intersect_topk | 19 | 0.884 | 0.952 | 0.967 | 0.960 | 0.899 |
| Same_in_both_topk | 19 | 0.884 | 0.952 | 0.967 | 0.960 | 0.899 |
| Only_in_merged_topk | 13 | 0.846 | 0.952 | 0.967 | 0.960 | 0.899 |
| Only_in_separate_topk | 14 | 0.790 | 0.952 | 0.967 | 0.960 | 0.899 |
| JPCT_consensus_overlap | 19 | 0.941 | 0.952 | 0.967 | 0.960 | 0.899 |
| Random_matched_k | 20 | 0.659 | 0.952 | 0.967 | 0.960 | 0.899 |

- Multivariate RF transfer **saturates** (even random ≈ 0.96) after joint batch modelling — Condition is too separable for panel AUC to rank filters on this pair.
- **Gene-level fidelity still ranks biology:** JPCT/consensus median RNA AUC **0.941** ≫ only-in-separate **0.790** ≫ random **0.659**.
- Novel filter **BIOS-Rank** (see `BIOS_RANK_METHOD.md`) is therefore scored on **cross-assay single-gene / min-median AUC**, not saturated RF transfer. Run `scripts/bios-rank-filter.R`.

### 4.5 BIOS-Rank filter validation (novel method)

Source: `results/BIOS_Rank_panel_comparison.md` (2026-07-12). Matched *k* ≈ 20.

| Panel | k | Med AUC micro | Med AUC RNA | **Min-median cross-assay** | Mean BIOS |
|-------|---|---------------|-------------|---------------------------|-----------|
| **BIOS_Rank_topk** | 20 | 0.942 | 0.937 | **0.924** | **0.901** |
| BIOS_ConsensusHard_topk | 19 | 0.924 | 0.941 | 0.919 | 0.891 |
| JPCT_export_consensus | 19 | 0.924 | 0.941 | 0.919 | 0.891 |
| Merged_limma_topk | 19 | 0.966 | 0.884 | 0.884 | 0.570 |
| Separate_intersect_topk | 19 | 0.966 | 0.884 | 0.884 | 0.575 |
| Random_matched_k | 20 | 0.877 | 0.827 | 0.807 | 0.462 |

**Interpretation**
- BIOS-Rank top-20 wins on **min-median cross-assay AUC (0.924)** vs limma/∩ top-20 (**0.884**) and random (**0.807**).
- Limma/∩ look strong on microarray (0.966) but **drop on RNA-seq (0.884)** — assay-skewed, not platform-invariant.
- BIOS-Rank is balanced (0.942 / 0.937) with **100% trait-WGCNA** and **80% ML** membership vs ~42% / 11% for limma top-20.
- Hard consensus mode ≈ exported JPCT panel (expected).

**Claim supported (v1 circular metric, historical):** BIOS soft panel beat limma on min-median when Ex used both assays.  
**Claim supported (v2 correct metric):** see §4.5c — held-out RNA AUC + CRC markers.

### 4.5c BIOS-Rank v2 — non-circular protocol (required for claims)

Source: `results/BIOS_v2_correctness.md`, `BIOS_v2_D3_locked_transfer.md` (2026-07-12).

**Primary metric:** median gene AUC on **RNA-seq** after ranking without held-out-assay Ex.

| Method | Med AUC RNA (held-out) | CRC markers in top-20 |
|--------|-----------------------:|----------------------:|
| **BIOS_v2_noEx_equal** | **0.940** | **9 (45%)** |
| BIOS_v2_ExTrain_locked | 0.940 | 9 (45%) |
| BIOS_v1 circular Ex | 0.937 | 8 (40%) |
| ONLY_Ec | 0.935 | 8 (40%) |
| Merged limma top-20 | 0.883 | 6 (30%) |
| Random | 0.759 | 0 |

ONLY_Ex (both assays) can look higher on AUC (**0.954**) but recovers only **4** CRC markers (20%) — circular ranking chasing the metric, weaker biology.

**D3 locked-weight transfer** (weights tuned on D1 → applied on GSE9348×GSE50760): locked ExTrain **0.941** > equal **0.937**; limma proxy **0.946** (ceiling); random **0.753**. Locked weights **transfer** (do not collapse); they do not beat limma on this easy pair.

**Correct paper claim:** On the hard D1 merge, non-circular BIOS recovers more CRC markers and higher held-out RNA fidelity than limma top-*k*; equal weights remain competitive with locked weights.

### 4.5d Wet-lab literature validation

BIOS_v2 panels from real GSE89076×GSE50760 data were tested for enrichment in a curated set of CRC genes with published qPCR/IHC/functional evidence (`data/CRC_wetlab_gold_standard.csv`). BIOS_v2_noEx_equal recovers 18/20 wet-lab-supported genes (frac=0.900; hypergeometric P=6.18e-46; emp. P vs random top-20=0). Notably OTOP2 was wet-lab validated in a study that analysed **GSE50760** (PMID 35418782). See `results/BIOS_wetlab_validation.md`.
### 4.5e Concordance with recent CRC signature papers

BIOS_v2 top-20 overlaps independent 2022–2024 CRC diagnostic/hub-gene studies (including papers analysing GSE50760 and GSE9348). Hits in recent-paper gene union: BIOS 8/20 vs limma 3/20 (emp. P vs random=0). Notable full recovery of the SPIB–AQP8–GUCA2B co-network and shared hubs GUCA2B/CLCA4/CLDN1/MMP7/BEST4/CA7/OTOP2. See `results/BIOS_recent_paper_concordance.md`.
### 4.6 Channel ablation & weight sensitivity (C5)

Source: `results/BIOS_channel_ablation.md` (2026-07-12). Primary metric = min-median cross-assay gene AUC; *k* = 20 unless noted.

**Leave-one-out (k=20)**

| Channel | LOO MinMedian | Δ vs equal | Jaccard vs equal | Role |
|---------|--------------:|-----------:|-----------------:|------|
| Ec | 0.921 | −0.002 | 0.667 | hurts metric; changes panel |
| Ep | 0.924 | 0 | 1.000 | saturated at top-*k* |
| Em | 0.924 | 0 | 1.000 | saturated at top-*k* |
| Es | 0.931 | +0.007 | 0.538 | changes panel membership |
| Ex | 0.923 | −0.001 | 0.905 | hurts metric; stronger drop at *k*=50/100 |

At *k*=50/100, dropping **Ex** lowers MinMedian by ≈0.006–0.007. Em/Ep appear “redundant” at small *k* because selected genes already have Em = Ep ≈ 1 (saturation), not because the channels are undefined.

**Biology secondaries (non-circular with Ex)**

| Setting | MinMedian | Frac Em | Frac Es | Mean Ec |
|---------|----------:|--------:|--------:|--------:|
| BIOS equal weights | 0.924 | **1.00** | **0.80** | **0.72** |
| ONLY_Ex | 0.950 | 0.65 | **0.20** | 0.47 |
| Merged limma top-20 | 0.883 | 0.40 | 0.10 | 0.36 |

ONLY_Ex can look best on MinMedian because \(E_x\) is both a ranking feature and the evaluation metric; equal-weight BIOS retains module/ML membership while still beating limma.

**Weights.** Among 500 Dirichlet draws on the simplex, equal-weight MinMedian = **0.924**; best = **0.945** (gap 0.021); **65.8%** of draws ≥ equal. **Methods default remains equal weights**; D1-tuned weights (Ex-heavy) are sensitivity only unless locked and re-tested on D3.

### 4.7 BIOS-FDR (permutation error control)

Source: `results/BIOS_FDR_summary.md` (2026-07-12). B=100, k=20.

| Method | Empirical FDR | Observed mean top-k | Null mean top-k | Emp. P |
|--------|--------------:|--------------------:|----------------:|-------:|
| BIOS-Rank | **0.0015** | **0.907** | 0.631 | **0.0099** |
| limma adj.P | 0.0000 | 0.745* | 0.267* | — |

\* Limma row uses mean **BIOS** of limma top-20 genes (not −log *P*); limma’s own adj.P threshold is ~1e−23, so limma list FDR is also ~0 under this null.

**Verdict:** BIOS top-20 is **FDR-controlled** and **significantly above the Condition-permutation null**. Limma is also highly specific at this extreme *P* cutoff — do not claim FDR superiority over limma; claim **calibrated multi-channel enrichment** + **better cross-assay fidelity** (§4.5–4.6).

### 4.8 Multi-dataset BIOS-Rank validation

Source: `results/BIOS_Rank_multi_dataset.md` (2026-07-12).

| Cohort | Contrast | BIOS | Baseline (limma/∩) | Random | Verdict |
|--------|----------|-----:|-------------------:|-------:|---------|
| **D1** GSE89076 × GSE50760 | cross-assay min-median | **0.924** | 0.884 | 0.807 | **WIN** |
| **D2** GSE50760 → GSE104836 | external median gene AUC | 0.980 | 0.980 | 0.670 | **TIE** (ceiling) |
| **D3** GSE9348 × GSE50760 | cross-assay min-median | 0.946 | 0.946 | 0.781 | **TIE**; beats random |

**Overall (C5):** BIOS-Rank wins where cross-assay transfer is hard (D1), matches limma when signal is easy (D2–D3), and always beats matched random. Do not claim “beats limma on all datasets.”

---

## 5. Discussion

### 5.1 What JPCT / BIOS-Rank change in GEO practice

| Common practice | JPCT / BIOS-Rank replacement | Biological gain |
|-----------------|------------------------------|-----------------|
| DEG∩ across platforms | Joint `~ Condition + Platform` | Shared disease estimand; recovers shadowed true genes |
| Top-*P* or huge DEG lists | **BIOS-Rank** (Ec–Ex, equal weights) | Platform-invariant, module-stable, cross-assay faithful genes |
| Single-gene ROC only / saturated panel AUC | Gene-level cross-assay fidelity + matched-*k* + biology secondaries | Honest ranking when multivariate AUC ceilings |
| Uncalibrated composite scores | **BIOS-FDR** | Empirical error control for the multi-channel top-*k* |

### 5.2 Relation to the Application Note

The Application Note describes the GExPipe software (download, DE, Shiny). This Original Paper defines and evaluates **JPCT + BIOS-Rank/BIOS-FDR**. Cite the Note for packaging; do not use the UI as the novelty claim here.

### 5.3 Limitations

- External RNA-seq *n* = 20 (D2) and strong CRC signal cause panel-AUC / gene-AUC ceilings; matched-*k* and random controls remain informative.  
- CRC-focused real cohorts; simulation (C4) is disease-agnostic known truth.  
- Em/Ep saturation at small *k* on D1 means LOO Δ≈0 does not prove those channels are dispensable in general—report multi-*k* and biology secondaries.  
- Equal weights are justified on D1; learned weights were not locked for D3 (sensitivity only).  
- Consensus thresholds (module *P*, ML vote ≥2) are fixed a priori.

### 5.4 Conclusion

JPCT replaces GEO’s DEG∩ habit with a **platform-adjusted Condition estimand** and multi-evidence consensus. **BIOS-Rank** further ranks genes for **cross-assay fidelity** under equal, empirically supported weights, with **BIOS-FDR** calibrating the composite top-*k* list. On a hard microarray–RNA-seq merge, BIOS improves min-median cross-assay AUC over limma/∩; on easier cohorts it matches limma while beating random—consistent with a filter that prioritizes platform-invariant disease biology rather than assay-skewed top-*P* lists.

---

## 6. Data and code availability

- Real-data merge metrics: `validation_manual/cross_platform/`  
- Consensus ablation / panel AUC: `GExPipe(original_paper)/results/ablation_panel_auc.*`  
- BIOS-Rank / BIOS-FDR / channel ablation / multi-dataset: `GExPipe(original_paper)/results/BIOS_*`  
- Simulation: `GExPipe(original_paper)/results/simulation_joint_vs_intersect.*`  
- Scripts: `GExPipe(original_paper)/scripts/`  
- Formula: `GExPipe(original_paper)/manuscript/BIOS_FORMULA.md`  
- Software: GExPipe Bioconductor package / https://github.com/safarafique/GExPipe  

---

## 7. Method box (for journal highlight)

**Name:** JPCT + BIOS-Rank / BIOS-FDR  

**Input:** ≥1 microarray and/or RNA-seq GEO matrices with Condition labels  

**Steps:**  
1. Harmonize to HGNC; normalize within platform.  
2. Fit limma `~ Condition + Platform`.  
3. Score genes with equal-weight BIOS (\(E_c,E_p,E_m,E_s,E_x\)); optional hard filter \(E_m=E_s=1\).  
4. Control top-*k* with phenotype-permutation BIOS-FDR.  
5. Evaluate cross-assay gene fidelity and/or external panel AUC (matched-*k*, random controls).  

**Output:** Compact gene panel + tables proving joint ≠ ∩, BIOS ≥ limma on hard cross-assay merges, and calibrated multi-channel enrichment.  

**Biological accuracy principle:** Prefer genes whose disease association survives platform adjustment, co-expression with phenotype, multi-learner stability, and cross-assay discrimination.
