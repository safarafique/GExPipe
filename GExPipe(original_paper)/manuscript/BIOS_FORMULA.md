# Novel formula used in this work: BIOS-Rank and BIOS-FDR

**Name:** BIOS-Rank (*Biological Invariance Orthogonal Selection Ranking*)  
**Add-on:** BIOS-FDR (phenotype-permutation empirical FDR)  
**Implemented in:** `scripts/bios-rank-filter.R`, `scripts/bios-fdr.R`

This is the **novel mathematical object** of the Original Paper (not limma itself).

---

## 1. Joint model (setup; not claimed as new)

For gene \(g\) and sample \(i\) on the harmonized multi-platform matrix:

\[
y_{gi}
=
\alpha_g
+
\beta_g^{C}\,C_i
+
\beta_g^{P}\,P_i
+
\varepsilon_{gi}
\]

- \(C_i\): Condition (e.g. Disease vs Normal)  
- \(P_i\): Platform (e.g. Microarray vs RNA-seq)  
- \(\widehat\beta_g^{C}\), \(\widehat\beta_g^{P}\): limma estimates  
- \(p_g^{\mathrm{adj}}\): BH-adjusted \(P\) for \(\beta_g^{C}\)

---

## 2. Novel score — BIOS-Rank (the invention)

### 2.1 Five evidence channels

\[
\begin{aligned}
E_c(g)
&=
\bigl(-\log_{10} p_g^{\mathrm{adj}}\bigr)
\cdot
\bigl|\widehat\beta_g^{C}\bigr|
\\[0.6em]
E_p(g)
&=
\frac{\bigl|\widehat\beta_g^{C}\bigr|}
{\bigl|\widehat\beta_g^{C}\bigr|+\bigl|\widehat\beta_g^{P}\bigr|+\epsilon}
\\[0.6em]
E_m(g)
&=
\mathbf{1}\!\left\{g \in \mathcal{M}_{\mathrm{trait}}\right\}
\\[0.6em]
E_s(g)
&=
\begin{cases}
1 & g \in \mathcal{S}_{\ge 2}\ \text{(≥2 ML / consensus)}\\
1/3 & g \in \mathcal{M}_{\mathrm{trait}}\ \text{only}\\
0 & \text{otherwise}
\end{cases}
\\[0.6em]
E_x(g)
&=
\min\bigl(
\mathrm{AUC}_{g}^{(A)},\,
\mathrm{AUC}_{g}^{(B)}
\bigr)
\end{aligned}
\]

| Symbol | Biological meaning |
|--------|--------------------|
| \(E_c\) | How strong the **disease** effect is |
| \(E_p\) | How **pure** that effect is vs platform/batch |
| \(E_m\) | Gene sits in a **phenotype-linked co-expression module** |
| \(E_s\) | Gene is **stable** across selectors |
| \(E_x\) | Gene discriminates on **both assays** (cross-assay fidelity) |

\(\mathcal{M}_{\mathrm{trait}}\): trait-associated WGCNA modules.  
\(\mathcal{S}_{\ge 2}\): genes selected by ≥2 of LASSO / RF / SVM-RFE (consensus export).  
\(\mathrm{AUC}_{g}^{(A)},\mathrm{AUC}_{g}^{(B)}\): single-gene ROC AUC on platform A and B.  
\(\epsilon>0\) small constant (code uses \(10^{-6}\)).

### 2.2 Scaling

Let \(\mathrm{scale}_{[0,1]}(\cdot)\) be min–max scaling across genes:

\[
\tilde E_c=\mathrm{scale}_{[0,1]}(E_c),\quad
\tilde E_p=\mathrm{scale}_{[0,1]}(E_p),\quad
\tilde E_x=\mathrm{scale}_{[0,1]}(E_x)
\]

(\(E_m,E_s\) already lie in \(\{0,1/3,1\}\).)

### 2.3 Master formula (equal weights — default used here)

\[
\boxed{
\mathrm{BIOS}(g)
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
\tilde E_x(g)
\Bigr)
}
\]

**Soft panel (discovery):**

\[
\mathcal{P}_{\mathrm{soft}}
=
\operatorname{Top}\text{-}k
\bigl\{\,g : \mathrm{BIOS}(g)\,\bigr\}
\]

**Hard consensus panel (JPCT mode):**

\[
\mathcal{P}_{\mathrm{hard}}
=
\operatorname{Top}\text{-}k
\Bigl\{
g :
E_m(g)=1,\;
E_s(g)=1,\;
\text{ranked by }\mathrm{BIOS}(g)
\Bigr\}
\]

---

## 3. Novel error control — BIOS-FDR

Let \(t_{(k)}\) be the \(k\)-th largest **observed** \(\mathrm{BIOS}(g)\).

For permutation \(b=1,\ldots,B\):

1. Shuffle Condition labels \(C^{(b)}\) (keep Platform \(P\)).  
2. Refit the joint limma model; recompute \(E_c^{(b)}, E_p^{(b)}, E_x^{(b)}\).  
3. Randomly reassign \(E_m,E_s\) across genes.  
4. Form \(\mathrm{BIOS}^{(b)}(g)\).  
5. Count null “discoveries”  
   \(N^{(b)}=\#\{\,g:\mathrm{BIOS}^{(b)}(g)\ge t_{(k)}\,\}\).

\[
\boxed{
\widehat{\mathrm{FDR}}_{\mathrm{BIOS}}(k)
=
\min\!\left(
1,\;
\frac{1}{kB}\sum_{b=1}^{B} N^{(b)}
\right)
}
\]

Enrichment test for the mean top-\(k\) score:

\[
p_{\mathrm{emp}}
=
\frac{
1+\sum_{b=1}^{B}
\mathbf{1}\!\left\{
\overline{\mathrm{BIOS}}^{(b)}_{\mathrm{top}\,k}
\ge
\overline{\mathrm{BIOS}}_{\mathrm{top}\,k}
\right\}
}{1+B}
\]

---

## 4. What is new vs what is reused

| New (publish this) | Reused (cite only) |
|--------------------|--------------------|
| \(\mathrm{BIOS}(g)\) multi-channel objective | limma / \(\beta^C,\beta^P\) |
| \(E_p\) platform-purity term | WGCNA modules |
| \(E_x\) cross-assay fidelity in the **rank** | LASSO / RF / SVM |
| \(\widehat{\mathrm{FDR}}_{\mathrm{BIOS}}\) | BH FDR inside limma |

---

## 5. One-line Methods sentence (paste into paper)

> We define the BIOS-Rank score of gene \(g\) as the mean of five scaled evidences—joint condition strength, platform purity, trait-module membership, multi-selector stability, and cross-assay AUC fidelity—and control the top-\(k\) list by phenotype-permutation BIOS-FDR.

---

## 6. Optional extension (not required for current results)

Weighted BIOS (future / adaptive novelty):

\[
\mathrm{BIOS}_w(g)
=
\sum_{u\in\{c,p,m,s,x\}}
w_u\,\tilde E_u(g),
\qquad
w_u\ge 0,\;
\sum_u w_u=1
\]

with \(w\) chosen by nested CV on the training cohort only (never on the external test set).

## 7. Empirical channel necessity (D1 ablation)

See `results/BIOS_channel_ablation.md` (generated 2026-07-12 12:13:32 UTC).
Equal weights are the **default**; LOO and Dirichlet search justify or refine them.


## 8. Corrected evaluation protocol (BIOS-Rank v2) — REQUIRED for claims

To avoid circularity of (E_x) with the primary metric:

1. **Ranking Ex (optional):** use only the **train assay** AUC, or omit Ex (4-channel BIOS).
2. **Evaluation:** median gene AUC on the **held-out assay** (e.g. rank with microarray evidence → score RNA-seq AUC).
3. **Weights:** equal by default; locked weights may be tuned on D1 held-out AUC and tested on D3.
4. **Orthogonal biology:** CRC marker overlap is reported separately (not the ranking objective).

See `results/BIOS_v2_correctness.md` and `scripts/bios-rank-v2-correctness.R`.

