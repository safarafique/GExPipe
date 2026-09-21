#!/usr/bin/env python3
"""Generate manuscript tables from GExPipe source (no Shiny run required).

Output directory: inst/manuscript/tool_generated/
Regenerate: python inst/scripts/generate_tool_information_tables.py
"""
from __future__ import annotations

import csv
from datetime import date
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "inst" / "manuscript" / "tool_generated"
PKG_VERSION = "0.99.105"


def write_csv(name: str, fieldnames: list[str], rows: list[dict]) -> Path:
    path = OUT / name
    with path.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
        w.writeheader()
        w.writerows(rows)
    return path


def md_table(headers: list[str], rows: list[list[str]]) -> str:
    lines = [
        "| " + " | ".join(headers) + " |",
        "| " + " | ".join(["---"] * len(headers)) + " |",
    ]
    for row in rows:
        lines.append("| " + " | ".join(row) + " |")
    return "\n".join(lines)


def main() -> None:
    OUT.mkdir(parents=True, exist_ok=True)
    generated: list[str] = []

    # --- Table: 16 pipeline steps ---
    steps = [
        ("1", "Download Data", "Data preparation", "GEOquery; RNA-seq + microarray GSE boxes; empty Series Matrix count ingest; probe→HGNC (Step 2b); four analysis types"),
        ("2", "Normalize Data", "Data preparation", "Per-GSE platform normalization; Auto/Manual; Merged: common genes + optional global quantile; Parallel: separate, no intersection"),
        ("3", "QC & Visualization", "Data preparation", "Sample/gene QC; density/boxplot; outlier exclusion; Venn/UpSet gene overlap (Parallel: per-platform plots)"),
        ("4", "Select Groups", "Data preparation", "Normal vs Disease (and related); Parallel: separate success banners per platform"),
        ("5", "Batch Correction", "Data preparation", "Variance filter; 6 joint methods (Merged/single); Parallel: per-platform batch (no joint ComBat)"),
        ("6", "Differential Expression", "Gene discovery", "limma, limma-voom, DESeq2, edgeR; Parallel: dual engines one click; independent cutoffs per platform"),
        ("7", "RNA-seq ∩ microarray", "Gene discovery", "Parallel DE only: same-direction dual-platform significant genes (consensus for downstream)"),
        ("8", "WGCNA Analysis", "Gene discovery", "Top variable genes (default 5000); one network; Parallel: RNA VST or array matrix on chosen platform"),
        ("9", "Common Genes (DEG & WGCNA)", "Gene discovery", "DEG ∩ trait-associated modules; GO/KEGG; DEG source depends on analysis type"),
        ("10", "PPI Interaction", "Candidate refinement", "STRINGdb network; hub ranking (internet on first use)"),
        ("11", "Machine Learning", "Candidate refinement", "8 algorithms; ≥2-method consensus final list; feature-importance plots"),
        ("12", "Validation Setup", "Candidate refinement", "External GEO cohort or internal split; feeds ROC"),
        ("13", "ROC Curve Analysis", "Candidate refinement", "Training/validation AUC; optional immune deconvolution correlation"),
        ("14", "Diagnostic Nomogram", "Clinical translation", "Exploratory nomogram (rms); reporting aid"),
        ("15", "GSEA Analysis", "Clinical translation", "GSEA / MSigDB-related pathways (msigdbr)"),
        ("16", "Results Summary", "Clinical translation", "Automated PDF-oriented summary; workspace save/load in sidebar"),
    ]
    write_csv(
        "Table_pipeline_16_steps.csv",
        ["Step", "Sidebar_label", "Phase", "Tool_capabilities"],
        [{"Step": s[0], "Sidebar_label": s[1], "Phase": s[2], "Tool_capabilities": s[3]} for s in steps],
    )
    generated.append("Table_pipeline_16_steps.csv")

    # --- S12: Four analysis types ---
    analysis_rows = [
        {
            "Analysis_type": "RNA-seq only",
            "Step1_GSE_boxes": "RNA-seq only (1+ GSE; single mode uses first ID only)",
            "Gene_intersection_timing": "Within RNA-seq GSEs only (multi-GSE intersect genes across RNA studies)",
            "Normalization": "Auto skips TMM for DESeq2/edgeR/voom; TMM/log2 for limma RNA DE",
            "Batch_correction": "When 2+ RNA GSEs; joint methods on RNA matrix",
            "DE_engine_Step6": "User choice: DESeq2, edgeR, limma-voom, or limma",
            "Step7_consensus": "Hidden (not used)",
            "WGCNA_input": "RNA VST or normalized RNA matrix; top variable genes",
            "Primary_DEG_source_steps_9_16": "Step 6 RNA-seq DEGs",
        },
        {
            "Analysis_type": "Microarray only",
            "Step1_GSE_boxes": "Microarray only (1+ GSE)",
            "Gene_intersection_timing": "Within microarray GSEs only",
            "Normalization": "Quantile, log2+quantile, RMA (CEL), Agilent normexp",
            "Batch_correction": "When 2+ array GSEs",
            "DE_engine_Step6": "limma (microarray)",
            "Step7_consensus": "Hidden",
            "WGCNA_input": "Batch-corrected or normalized array matrix",
            "Primary_DEG_source_steps_9_16": "Step 6 microarray DEGs (limma)",
        },
        {
            "Analysis_type": "Merged (Both)",
            "Step1_GSE_boxes": "Both boxes; 1+ GSE each; always multi-GSE when several IDs typed",
            "Gene_intersection_timing": "After per-study normalize: common HGNC genes; optional global quantile",
            "Normalization": "Per-study then intersect; global quantile on by default (Merged)",
            "Batch_correction": "One joint correction on merged matrix (Condition + Platform/Dataset in model)",
            "DE_engine_Step6": "Single joint limma on merged matrix",
            "Step7_consensus": "Hidden",
            "WGCNA_input": "Merged batch-corrected matrix; top variable genes",
            "Primary_DEG_source_steps_9_16": "Step 6 joint limma DEGs (common genes)",
        },
        {
            "Analysis_type": "Parallel DE, then merge",
            "Step1_GSE_boxes": "Both boxes; 1+ GSE each; platforms kept separate through Step 6",
            "Gene_intersection_timing": "No early intersection; Step 7 same-direction RNA-seq ∩ microarray DEGs",
            "Normalization": "Separate per platform; no global quantile; no shared gene subset before DE",
            "Batch_correction": "Separate per platform (Auto: array ComBat-ref; RNA limma if count DE else ComBat-ref)",
            "DE_engine_Step6": "RNA: DESeq2/edgeR/voom/limma (user); Micro: limma; one Run DE for both",
            "Step7_consensus": "Active: same-direction genes significant on both platforms",
            "WGCNA_input": "One platform (Auto = more samples): RNA VST or array matrix; not consensus DEG list",
            "Primary_DEG_source_steps_9_16": "Step 7 consensus DEGs (after Apply Step 7)",
        },
    ]
    write_csv("Supplementary_Table_S12_four_analysis_types.csv", list(analysis_rows[0].keys()), analysis_rows)
    generated.append("Supplementary_Table_S12_four_analysis_types.csv")

    # --- Batch methods (implemented; not ComBat-seq) ---
    batch = [
        ("combat_ref", "ComBat-ref", "sva::ComBat with ref.batch = largest Dataset", "Condition (+ Platform in mod when identifiable)", "Merged / single-track default"),
        ("sva", "SVA + ComBat", "Estimate SVs (be method), then ComBat with mod + SVs", "Condition protected in mod", "Merged / single-track"),
        ("limma", "limma removeBatchEffect", "limma::removeBatchEffect(batch = Dataset, design = mod)", "Condition (+ Platform when in mod)", "Merged; Parallel RNA when count DE"),
        ("combat", "ComBat", "sva::ComBat with phenotype mod", "Condition in mod", "Merged; Parallel manual option"),
        ("quantile_limma", "Quantile + limma", "normalizeBetweenArrays quantile then removeBatchEffect", "Condition in mod", "Merged; Parallel micro manual"),
        ("hybrid", "Hybrid", "Quantile then ComBat", "Condition in mod", "Merged; Parallel micro manual"),
    ]
    write_csv(
        "Supplementary_Table_batch_correction_methods.csv",
        ["Method_id", "UI_label", "Implementation", "Condition_protection", "Availability"],
        [
            {
                "Method_id": b[0],
                "UI_label": b[1],
                "Implementation": b[2],
                "Condition_protection": b[3],
                "Availability": b[4],
            }
            for b in batch
        ],
    )
    generated.append("Supplementary_Table_batch_correction_methods.csv")

    # --- DE engines ---
    de = [
        ("limma", "limma", "Normalized log-scale expression or TMM log-CPM", "Yes", "Yes", "Yes (micro fixed)", "Yes (RNA if selected)"),
        ("limma_voom", "limma-voom", "Raw RNA-seq counts → voom", "Yes", "No", "No", "Yes (RNA if selected)"),
        ("deseq2", "DESeq2", "Raw RNA-seq counts", "Yes", "No", "No", "Yes (RNA if selected)"),
        ("edger", "edgeR", "Raw RNA-seq counts", "Yes", "No", "No", "Yes (RNA if selected)"),
    ]
    write_csv(
        "Table_differential_expression_engines.csv",
        ["Method_id", "Display_name", "Input_data", "RNA_seq_only", "Microarray_only", "Parallel_micro", "Parallel_RNA"],
        [
            {
                "Method_id": d[0],
                "Display_name": d[1],
                "Input_data": d[2],
                "RNA_seq_only": d[3],
                "Microarray_only": d[4],
                "Parallel_micro": d[5],
                "Parallel_RNA": d[6],
            }
            for d in de
        ],
    )
    generated.append("Table_differential_expression_engines.csv")

    # --- Normalization ---
    norm = [
        ("quantile", "Microarray", "Quantile (processed / log2)", "Merged, microarray, Parallel micro"),
        ("log2_quantile", "Microarray", "log2 then quantile", "Merged, microarray, Parallel micro"),
        ("rma", "Microarray", "RMA from Affymetrix CEL (affy/oligo)", "When CEL supplementary files available"),
        ("normexp", "Microarray", "Agilent normexp + quantile", "Merged, microarray, Parallel micro"),
        ("TMM", "RNA-seq", "TMM + log2-CPM", "Merged limma path; RNA limma DE"),
        ("log2fpkm", "RNA-seq", "log2(x+1) FPKM/TPM", "Manual RNA options"),
        ("log2cpm_only", "RNA-seq", "log2(CPM+1) exploration", "Manual RNA options"),
        ("skip_counts", "RNA-seq", "Raw counts retained", "DESeq2, edgeR, limma-voom (Auto skips TMM)"),
        ("global_quantile", "Merged", "Global quantile after common-gene intersection", "Merged (Both) only; not Parallel"),
        ("per_gse_auto", "All", "Auto mode follows Step 1 DE method and data type", "Steps 2 Auto default"),
    ]
    write_csv(
        "Table_normalization_options.csv",
        ["Option_id", "Platform", "Description", "Typical_use"],
        [
            {
                "Option_id": n[0],
                "Platform": n[1],
                "Description": n[2],
                "Typical_use": n[3],
            }
            for n in norm
        ],
    )
    generated.append("Table_normalization_options.csv")

    # --- ML ---
    ml = [
        ("lasso", "LASSO", "glmnet", "L1; coefficient path plots"),
        ("elastic", "Elastic Net", "glmnet", "alpha=0.5"),
        ("ridge", "Ridge", "glmnet", "L2 shrinkage"),
        ("rf", "Random Forest", "randomForest", "Variable importance"),
        ("svm", "SVM-RFE", "kernlab (Suggests)", "Recursive feature elimination"),
        ("boruta", "Boruta", "Boruta (Suggests)", "All-relevant features vs shadows"),
        ("splsda", "sPLS-DA", "mixOmics (Suggests)", "Sparse multivariate selection"),
        ("xgboost", "XGBoost+SHAP", "xgboost + SHAPforxgboost (Suggests)", "SHAP-ranked importance"),
    ]
    write_csv(
        "Table_ML_ensemble_methods.csv",
        ["Method_id", "UI_name", "R_package", "Notes"],
        [{"Method_id": m[0], "UI_name": m[1], "R_package": m[2], "Notes": m[3]} for m in ml],
    )
    generated.append("Table_ML_ensemble_methods.csv")

    # --- Downstream modules ---
    downstream = [
        ("QC diagnostics", "3", "PCA-style views, sample filtering, PVCA helper in batch/QC pipeline", "gexp_qc_pipeline.R, gexp_platform_helpers.R"),
        ("Consensus Step 7", "7", "require_same_direction default TRUE; mean logFC; conservative adj.P", "gexp_consensus_pipeline.R"),
        ("WGCNA", "8", "dynamicTreeCut; top 5000 variable genes default; module-trait correlation", "gexp_wgcna_pipeline.R"),
        ("GO/KEGG", "9", "clusterProfiler enrichment on common genes", "Step 9 UI"),
        ("STRING PPI", "10", "STRINGdb download on first use; human taxonomy 9606", "ui_ppi.R, STRINGdb"),
        ("Validation", "12", "External GSE or hold-out split", "server_validation.R"),
        ("ROC / immune", "13", "pROC; optional immune deconvolution matrix", "ui_roc.R"),
        ("Nomogram", "14", "rms nomogram", "ui_nomogram.R"),
        ("GSEA", "15", "msigdbr gene sets", "ui_gsea.R"),
        ("Workspace", "sidebar", "Save/load RData workspace", "observers_workspace.R"),
    ]
    write_csv(
        "Table_downstream_modules.csv",
        ["Module", "Step", "Description", "Source_reference"],
        [
            {
                "Module": d[0],
                "Step": d[1],
                "Description": d[2],
                "Source_reference": d[3],
            }
            for d in downstream
        ],
    )
    generated.append("Table_downstream_modules.csv")

    # --- Software scope ---
    scope = [
        ("Package", "GExPipe", PKG_VERSION),
        ("Minimum_R", "4.6.0", "DESCRIPTION Depends"),
        ("Bioconductor", "3.22 (target)", "DESCRIPTION / README"),
        ("Organism", "Homo sapiens only", "HGNC, org.Hs.eg.db, human KEGG/STRING"),
        ("Data_source", "NCBI GEO", "GEOquery download"),
        ("Assay_types", "Bulk microarray and bulk RNA-seq", "Not single-cell or spatial"),
        ("Multi_GSE", "Comma-separated IDs per platform box", "Merged and Parallel keep all GSEs; single-platform can truncate to first GSE"),
        ("License", "MIT", "DESCRIPTION"),
    ]
    write_csv(
        "Table_software_scope.csv",
        ["Item", "Value", "Notes"],
        [{"Item": s[0], "Value": s[1], "Notes": s[2]} for s in scope],
    )
    generated.append("Table_software_scope.csv")

    # --- Main Table 1 style: GExPipe 16-step capability matrix (tool only) ---
    cap_rows = []
    for step_num, label, phase, cap in steps:
        cap_rows.append(
            {
                "Step": step_num,
                "Feature": label,
                "GExPipe": "Yes",
                "Notes": cap,
            }
        )
    cap_rows.append(
        {
            "Step": "—",
            "Feature": "Four analysis types at Step 1",
            "GExPipe": "Yes",
            "Notes": "RNA-seq only; Microarray only; Merged (Both); Parallel DE then merge",
        }
    )
    cap_rows.append(
        {
            "Step": "—",
            "Feature": "Multi-GSE per platform (Merged / Parallel)",
            "GExPipe": "Yes",
            "Notes": "Several comma-separated GSE IDs in each box",
        }
    )
    cap_rows.append(
        {
            "Step": "—",
            "Feature": "Empty GEO Series Matrix RNA-seq ingest",
            "GExPipe": "Yes",
            "Notes": "Supplementary count files / NCBI counts (download pipeline)",
        }
    )
    write_csv("Table1_GExPipe_sixteen_step_capability.csv", ["Step", "Feature", "GExPipe", "Notes"], cap_rows)
    generated.append("Table1_GExPipe_sixteen_step_capability.csv")

    # --- README index ---
    readme = f"""# Tool-generated manuscript tables (no Shiny run)

Generated from GExPipe source metadata on **{date.isoformat()}** (package **{PKG_VERSION}**).

**Directory:** `inst/manuscript/tool_generated/`

**Regenerate:**

```bash
python inst/scripts/generate_tool_information_tables.py
```

## Files in this folder (specification / methods tables)

| File | Suggested manuscript ID | Content |
|------|-------------------------|---------|
| `Table1_GExPipe_sixteen_step_capability.csv` | **Table 1** (GExPipe column) or supplement | 16 gated steps + distinctive flags |
| `Table_pipeline_16_steps.csv` | Methods / Fig. 2 legend | Step order aligned with `interface_app.R` sidebar |
| `Supplementary_Table_S12_four_analysis_types.csv` | **Supplementary Table S12** | Merged vs Parallel vs single-platform rules |
| `Supplementary_Table_batch_correction_methods.csv` | **Supplementary Table** (batch; fix S6 numbering in paper) | Six implemented batch methods (**ComBat-seq not implemented**) |
| `Table_differential_expression_engines.csv` | Methods | limma, voom, DESeq2, edgeR by analysis type |
| `Table_normalization_options.csv` | Methods / Supplementary | Platform normalization and Merged global quantile |
| `Table_ML_ensemble_methods.csv` | Methods | Eight ML algorithms in Step 11 |
| `Table_downstream_modules.csv` | Methods | PPI, validation, ROC, GSEA, workspace |
| `Table_software_scope.csv` | Availability | R, organism, GEO, assay scope |
| `INDEX.md` | — | Markdown copies of key tables for Word paste |

## NOT generated here (require data / app exports)

These need `validation_manual/` exports or a Shiny run — see `validation_manual/README_BENCHMARK_EXPORTS.md`:

- **S1** DE concordance (`make-supplementary-table-s1.py`)
- **S2** Per-gene AUC (`make-supplementary-table-s2.py`)
- **S3, S13** Merge / PVCA / Jaccard (`cross_platform/report/`)
- **S4–S11** Competitor GUI, reproducibility, published concordance, timing

## Source files used

- `R/interface_app.R` (16 sidebar steps)
- `R/interface_download.R` (analysis types, multi-GSE)
- `R/gexp_batch_pipeline.R`, `R/ui_batch.R` (batch methods)
- `R/gexp_de_pipeline.R`, `R/gexp_consensus_pipeline.R`, `R/gexp_wgcna_pipeline.R`
- `R/ui_ml.R`, `DESCRIPTION`, `README.md`
"""
    (OUT / "README.md").write_text(readme, encoding="utf-8")

    # --- INDEX.md with markdown tables for Word ---
    index_parts = [
        f"# GExPipe tool-generated tables ({PKG_VERSION})\n",
        "## Supplementary Table S12 — Four analysis types\n",
        md_table(
            list(analysis_rows[0].keys()),
            [[str(r[k]) for k in analysis_rows[0].keys()] for r in analysis_rows],
        ),
        "\n## Batch correction methods (implemented in code)\n",
        md_table(
            ["Method_id", "UI_label", "Implementation"],
            [[b[0], b[1], b[2]] for b in batch],
        ),
        "\n## Sixteen pipeline steps\n",
        md_table(
            ["Step", "Label", "Phase"],
            [[s[0], s[1], s[2]] for s in steps],
        ),
        "\n## ML ensemble (Step 11)\n",
        md_table(
            ["Method", "Package"],
            [[m[1], m[2]] for m in ml],
        ),
    ]
    (OUT / "INDEX.md").write_text("\n".join(index_parts), encoding="utf-8")

    print(f"Wrote {len(generated)} CSV files + README.md + INDEX.md to:\n  {OUT}")
    for g in generated:
        print(f"  - {g}")


if __name__ == "__main__":
    main()
