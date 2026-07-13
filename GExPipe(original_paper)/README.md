# GExPipe (Original Paper)

Standalone workspace for the **Original Paper** analyses (panel AUC + consensus ablation).

This folder does **not** modify Application Note package code (`R/`, `inst/scripts/`, etc.). Scripts only **read** existing exports under `validation_manual/` and write outputs here.

Plan (read-only): [`../inst/manuscript/ORIGINAL_PAPER_PLAN.md`](../inst/manuscript/ORIGINAL_PAPER_PLAN.md)

## Claims covered here

| ID | Claim |
|----|--------|
| C2 | Full consensus panel AUC ≥ limma / limma-voom top-50 (with smaller *k*) |
| C3 | Ablation: each layer (WGCNA, ML) matters |

C1 (merge ≠ ∩) already lives under `validation_manual/cross_platform/` — cite it; do not re-implement here.

## Layout

```
GExPipe(original_paper)/
  README.md
  scripts/
    panel-external-auc.R          # helpers + primary endpoint
    ablation-consensus-panels.R   # build B0–B6 + run evaluation
  signatures/                     # written gene lists per arm
  results/                        # tables / bootstrap CIs
  manuscript/
    GExPipe_original_paper_draft.md
```

## Prerequisites (Windows R 4.6)

Packages: `edgeR`, `limma`, `pROC`, `glmnet`, `randomForest`, `org.Hs.eg.db`, `AnnotationDbi`

Data expected under the Application Note repo (`--gexpipe-repo`, default parent):

- `validation_manual/competitor_benchmark/upload_pack/GSE50760_counts.csv`
- `validation_manual/competitor_benchmark/upload_pack/GSE50760_phenotype.csv`
- `validation_manual/work/rna_data/GSE104836_parsed.rds`
- `validation_manual/GSE50760_limma_DE_all.csv`
- `validation_manual/GSE50760_limma_voom_DE_all.csv`
- `validation_manual/common_genes_DEG_WGCNA.csv`
- `validation_manual/final_list_common_genes_ML.csv` / `consensus_signature_genes.csv`

## Run (PowerShell)

From this folder:

```powershell
cd "E:\GExPipe\GExPipe(original_paper)"
& "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" scripts/ablation-consensus-panels.R --gexpipe-repo E:/GExPipe
```

Outputs:

| File | Role |
|------|------|
| `signatures/B*.csv` | Ablation gene lists |
| `results/ablation_panel_auc.csv` | Primary Results table (panel AUC) |
| `results/ablation_gene_auc_median.csv` | Secondary single-gene medians |
| `results/ablation_panel_auc.md` | Markdown draft for Table 3 |

## Evaluation protocol (fixed)

1. Train GSE50760; never tune on GSE104836.
2. Phenotype: Disease vs Normal (same rules as Application Note headless scripts).
3. **Primary (small *n*):** RF external panel AUC + 95% bootstrap CI; nested 5-fold CV AUC on train.
4. **Secondary:** glmnet panel AUC (can saturate at external *n*≈20); median single-gene AUC.
5. **Matched *k*:** limma / limma-voom top-\|B5\| (B0k / B1k) for fair size comparison.
6. **True B4:** ≥2 of LASSO/RF/SVM on DE genes **without** WGCNA (legacy export kept as B4leg).
7. **B6:** random genes of size \|B5\| (seed = 42).

## Manuscript

| File | Role |
|------|------|
| `manuscript/GExPipe_Original_Paper.md` | Full Original Paper draft (**JPCT**) |
| `manuscript/NOVELTY_ASSESSMENT.md` | Honest novelty verdict |
| `manuscript/GExPipe_original_paper_draft.md` | Short skeleton / gate checklist |

## Cross-platform transfer (JPCT M3)

```bash
cd "/mnt/e/GExPipe/GExPipe(original_paper)"
"/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
  scripts/cross-platform-panel-transfer.R \
  --gexpipe-repo "E:/GExPipe"
```

**Note:** On this CRC pair, multivariate RF transfer AUC saturates (including random). Use **BIOS-Rank** gene-level cross-assay fidelity as the filter metric:

```bash
"/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
  scripts/bios-rank-filter.R --gexpipe-repo "E:/GExPipe"
```

### Multi-dataset check (does it generalize?)

```bash
"/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
  scripts/bios-rank-multi-dataset.R --gexpipe-repo "E:/GExPipe"
```

- **D1:** GSE89076 × GSE50760 (existing BIOS table)  
- **D2:** GSE50760 → GSE104836 external (train-only ranking)  
- **D3:** second microarray `GSE9348` × GSE50760 (override with `--micro2`; skip with `--skip-d3 true`)

Outputs: `results/BIOS_Rank_multi_dataset.csv` / `.md`

### BIOS-Rank v2 (non-circular + locked weights + CRC markers)

```bash
cd /mnt/e/GExPipe/GExPipe\(original_paper\)
"/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
  scripts/bios-rank-v2-correctness.R --gexpipe-repo "E:/GExPipe"
"/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
  scripts/bios-rank-d3-scores.R --gexpipe-repo "E:/GExPipe"
```

Outputs: `BIOS_v2_correctness.*`, `BIOS_v2_locked_weights.csv`, `BIOS_v2_D3_locked_transfer.*`, `BIOS_Rank_D3_gene_scores.csv`

### Wet-lab literature validation (published qPCR/IHC)

```bash
"/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
  scripts/bios-wetlab-validation.R --gexpipe-repo "E:/GExPipe"
```

Gold list: `data/CRC_wetlab_gold_standard.csv`  
Outputs: `results/BIOS_wetlab_validation.md`, `BIOS_wetlab_validation_summary.csv`

### Concordance with recent CRC papers (2022–2024)

```bash
"/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
  scripts/bios-recent-paper-concordance.R --gexpipe-repo "E:/GExPipe"
```

Paper gene lists: `data/recent_CRC_paper_signatures.csv`  
Outputs: `results/BIOS_recent_paper_concordance.md`, `BIOS_recent_paper_union_summary.csv`

### Channel ablation & weight sensitivity (necessity of Ec–Ex)

Requires `results/BIOS_Rank_gene_scores.csv` from `bios-rank-filter.R`.

```bash
"/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
  scripts/bios-channel-ablation.R --gexpipe-repo "E:/GExPipe"
```

Outputs: `BIOS_channel_ablation.*`, `BIOS_channel_necessity.csv`, `BIOS_channel_loo_multik.csv`, `BIOS_channel_biology_metrics.csv`, `BIOS_weight_sensitivity_summary.csv`

### BIOS-FDR (permutation empirical FDR) — flagship novelty add-on

```bash
"/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
  scripts/bios-fdr.R --gexpipe-repo "E:/GExPipe" --n-perm 100
```

Faster smoke test: `--n-perm 20`. Full paper run: `--n-perm 200` (slower).

Outputs: `results/BIOS_FDR_summary.csv` / `.md`, `BIOS_FDR_topk_genes.csv`, `BIOS_FDR_null_distribution.csv`

## Gate

Submit Original only after panel AUC + ablation are done **and** simulation (or 2nd cohort) is green.
Otherwise stay Application Note.

## C4 — Simulation (chosen over 2nd GEO external)

```bash
cd "/mnt/e/GExPipe/GExPipe(original_paper)"
"/mnt/c/Program Files/R/R-4.6.0/bin/Rscript.exe" \
  scripts/simulate-joint-vs-intersect.R \
  --outdir "E:/GExPipe/GExPipe(original_paper)"
```

Writes `results/simulation_joint_vs_intersect_{raw,summary}.csv` and `.md`.

