#!/usr/bin/env python3
"""Generate Supplementary Table S2 per-gene metrics from validation_manual exports.

Consensus gene list (first match wins):
  1. --consensus-genes PATH
  2. validation_manual/consensus_signature_genes.csv
  3. Intersection of common_genes_DEG_WGCNA.csv and final_list_common_genes_ML.csv
  4. common_genes_DEG_WGCNA.csv alone
  5. Placeholder shared-DEG list (clearly labelled)

AUC columns merged from (first match wins):
  1. --auc-file PATH
  2. validation_manual/ROC_AUC_Training_vs_Validation.csv  (AUC_Internal, AUC_External)
  3. validation_manual/ROC_AUC_scores.csv  (Gene, AUC -> Train_AUC only)

Usage:
  python inst/scripts/make-supplementary-table-s2.py
  python inst/scripts/make-supplementary-table-s2.py --repo e:/GExPipe
  python inst/scripts/make-supplementary-table-s2.py --consensus-genes validation_manual/consensus_signature_genes.csv
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

import pandas as pd

WORKFLOW_LIMMA = "Standard: limma-only (top-50 by adj.P)"
WORKFLOW_VOOM = "Standard: limma-voom-only (top-50 by adj.P)"
WORKFLOW_CONSENSUS = "GExPipe consensus (DE & WGCNA & ML ensemble)"


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--repo", default=None, help="Package root (default: two levels above this script)")
    p.add_argument(
        "--consensus-genes",
        default=None,
        help="CSV with Gene column (overrides auto-detect)",
    )
    p.add_argument(
        "--auc-file",
        default=None,
        help="ROC export CSV (overrides auto-detect)",
    )
    p.add_argument(
        "--out",
        default=None,
        help="Output CSV path (default: inst/manuscript/Supplementary_Table_S2_per_gene_metrics.csv)",
    )
    return p.parse_args()


def read_gene_column(path: Path) -> list[str]:
    df = pd.read_csv(path)
    col = "Gene" if "Gene" in df.columns else df.columns[0]
    genes = df[col].dropna().astype(str).str.strip()
    return [g for g in genes.unique().tolist() if g]


def load_consensus_genes(base: Path, override: Path | None) -> tuple[list[str], str]:
    """Return (genes, source_description)."""
    if override is not None:
        if not override.is_file():
            sys.exit(f"Consensus file not found: {override}")
        return read_gene_column(override), str(override.name)

    canonical = base / "consensus_signature_genes.csv"
    if canonical.is_file():
        genes = read_gene_column(canonical)
        # Ignore template/empty file (header only or comment rows)
        genes = [g for g in genes if g and not g.startswith("#")]
        if genes:
            return genes, "consensus_signature_genes.csv"

    deg_wgcna = base / "common_genes_DEG_WGCNA.csv"
    ml_final = base / "final_list_common_genes_ML.csv"
    ml_alt = base / "common_genes_ML_methods.csv"

    if deg_wgcna.is_file() and ml_final.is_file():
        a, b = set(read_gene_column(deg_wgcna)), set(read_gene_column(ml_final))
        inter = sorted(a & b, key=lambda g: g)
        if inter:
            return inter, "common_genes_DEG_WGCNA.csv & final_list_common_genes_ML.csv"

    if deg_wgcna.is_file() and ml_alt.is_file():
        a, b = set(read_gene_column(deg_wgcna)), set(read_gene_column(ml_alt))
        inter = sorted(a & b, key=lambda g: g)
        if inter:
            return inter, "common_genes_DEG_WGCNA.csv & common_genes_ML_methods.csv"

    if deg_wgcna.is_file():
        genes = read_gene_column(deg_wgcna)
        if genes:
            return genes, "common_genes_DEG_WGCNA.csv (ML export not found; add final_list_common_genes_ML.csv)"

    if ml_final.is_file():
        genes = read_gene_column(ml_final)
        if genes:
            return genes, "final_list_common_genes_ML.csv (Step 8 export not found)"

    # Placeholder
    tr_sig = pd.read_csv(base / "GSE50760_deseq2_DE_sig.csv")["Gene"]
    va_sig = set(pd.read_csv(base / "GSE104836_deseq2_DE_sig.csv")["Gene"])
    tr_all = pd.read_csv(base / "GSE50760_deseq2_DE_all.csv").set_index("Gene")
    shared = [g for g in tr_sig if g in va_sig]
    proxy = tr_all.loc[shared].sort_values("adj.P.Val").head(28).index.tolist()
    return proxy, "PLACEHOLDER shared DEGs - copy Shiny exports to validation_manual/"


def load_auc_maps(base: Path, override: Path | None) -> tuple[dict[str, float], dict[str, float], str]:
    """Return (train_auc_by_gene, external_auc_by_gene, source_label)."""
    train_map: dict[str, float] = {}
    ext_map: dict[str, float] = {}

    candidates: list[Path] = []
    if override is not None:
        candidates.append(override)
    else:
        candidates.extend([
            base / "ROC_AUC_Training_vs_Validation.csv",
            base / "ROC_AUC_scores.csv",
        ])

    for path in candidates:
        if not path.is_file():
            continue
        df = pd.read_csv(path)
        if "Gene" not in df.columns:
            continue

        if "AUC_Internal" in df.columns:
            for _, row in df.iterrows():
                g = str(row["Gene"]).strip()
                if pd.notna(row.get("AUC_Internal")):
                    train_map[g] = float(row["AUC_Internal"])
                if pd.notna(row.get("AUC_External")):
                    ext_map[g] = float(row["AUC_External"])
            return train_map, ext_map, path.name

        if "AUC" in df.columns:
            for _, row in df.iterrows():
                g = str(row["Gene"]).strip()
                if pd.notna(row["AUC"]):
                    train_map[g] = float(row["AUC"])
            return train_map, ext_map, path.name

    return train_map, ext_map, "(none - export Step 12 ROC_AUC_Training_vs_Validation.csv)"


def top_n(method: str, gse: str, base: Path, n: int = 50) -> pd.DataFrame:
    df = pd.read_csv(base / f"{gse}_{method}_DE_all.csv")
    return df.sort_values("adj.P.Val").head(n)


def val_de_path(method: str, base: Path) -> Path:
    if method == "limma_voom":
        return base / "GSE104836_limma_voom_DE_all.csv"
    if method == "limma":
        return base / "GSE104836_limma_DE_all.csv"
    return base / f"GSE104836_{method}_DE_all.csv"


def add_workflow_rows(
    rows: list[dict],
    workflow: str,
    method: str,
    base: Path,
    train_auc: dict[str, float],
    ext_auc: dict[str, float],
) -> None:
    tr = top_n(method, "GSE50760", base, 50)
    val = pd.read_csv(val_de_path(method, base)).set_index("Gene")
    for _, r in tr.iterrows():
        g = str(r["Gene"])
        vr = val.loc[g] if g in val.index else None
        rows.append({
            "Workflow": workflow,
            "Gene": g,
            "Train_log2FC": round(float(r["logFC"]), 3),
            "Train_adjP": f"{float(r['adj.P.Val']):.2e}",
            "Val_log2FC": round(float(vr["logFC"]), 3) if vr is not None else "",
            "Val_adjP": f"{float(vr['adj.P.Val']):.2e}" if vr is not None else "",
            "Direction_concordant": bool(vr is not None and r["logFC"] * vr["logFC"] > 0),
            "Train_AUC": train_auc.get(g, ""),
            "External_AUC_GSE104836": ext_auc.get(g, ""),
        })


def add_consensus_rows(
    rows: list[dict],
    genes: list[str],
    base: Path,
    workflow_label: str,
    train_auc: dict[str, float],
    ext_auc: dict[str, float],
    de_method: str = "deseq2",
) -> None:
    tr_all = pd.read_csv(base / f"GSE50760_{de_method}_DE_all.csv").set_index("Gene")
    va_all = pd.read_csv(base / f"GSE104836_{de_method}_DE_all.csv").set_index("Gene")
    for g in genes:
        if g not in tr_all.index:
            rows.append({
                "Workflow": workflow_label,
                "Gene": g,
                "Train_log2FC": "",
                "Train_adjP": "",
                "Val_log2FC": "",
                "Val_adjP": "",
                "Direction_concordant": False,
                "Train_AUC": train_auc.get(g, ""),
                "External_AUC_GSE104836": ext_auc.get(g, ""),
            })
            continue
        r = tr_all.loc[g]
        vr = va_all.loc[g] if g in va_all.index else None
        rows.append({
            "Workflow": workflow_label,
            "Gene": g,
            "Train_log2FC": round(float(r["logFC"]), 3),
            "Train_adjP": f"{float(r['adj.P.Val']):.2e}",
            "Val_log2FC": round(float(vr["logFC"]), 3) if vr is not None else "",
            "Val_adjP": f"{float(vr['adj.P.Val']):.2e}" if vr is not None else "",
            "Direction_concordant": bool(vr is not None and r["logFC"] * vr["logFC"] > 0),
            "Train_AUC": train_auc.get(g, ""),
            "External_AUC_GSE104836": ext_auc.get(g, ""),
        })


def median_auc(values: list) -> float | None:
    nums = [float(v) for v in values if v != "" and pd.notna(v)]
    return round(float(pd.Series(nums).median()), 3) if nums else None


def main() -> None:
    args = parse_args()
    repo = Path(args.repo).resolve() if args.repo else Path(__file__).resolve().parents[2]
    base = repo / "validation_manual"
    out_dir = repo / "inst" / "manuscript"
    out_dir.mkdir(parents=True, exist_ok=True)

    consensus_override = Path(args.consensus_genes).resolve() if args.consensus_genes else None
    auc_override = Path(args.auc_file).resolve() if args.auc_file else None

    consensus_genes, consensus_src = load_consensus_genes(base, consensus_override)
    train_auc, ext_auc, auc_src = load_auc_maps(base, auc_override)

    is_placeholder = consensus_src.startswith("PLACEHOLDER")
    consensus_label = (
        f"{WORKFLOW_CONSENSUS} [placeholder]"
        if is_placeholder
        else WORKFLOW_CONSENSUS
    )

    rows: list[dict] = []
    add_workflow_rows(rows, WORKFLOW_LIMMA, "limma", base, train_auc, ext_auc)
    add_workflow_rows(rows, WORKFLOW_VOOM, "limma_voom", base, train_auc, ext_auc)
    add_consensus_rows(rows, consensus_genes, base, consensus_label, train_auc, ext_auc)

    df = pd.DataFrame(rows)
    out_csv = Path(args.out).resolve() if args.out else out_dir / "Supplementary_Table_S2_per_gene_metrics.csv"
    df.to_csv(out_csv, index=False)

    # Summary markdown for Table 1 footer
    summary_path = out_dir / "Table_S2_summary_for_Table1.md"
    lines = [
        "# Table S2 summary (paste into Table 1 / Supplementary)",
        "",
        f"- **Consensus source:** `{consensus_src}` ({len(consensus_genes)} genes)",
        f"- **AUC source:** `{auc_src}`",
        "",
        "## Per-workflow summary",
        "",
        "| Workflow | n | Direction concordance | Median Train AUC | Median External AUC |",
        "|----------|--:|----------------------:|-----------------:|--------------------:|",
    ]

    for wf in df["Workflow"].unique():
        sub = df[df["Workflow"] == wf]
        conc = sub["Direction_concordant"].mean() * 100 if len(sub) else 0
        med_tr = median_auc(sub["Train_AUC"].tolist())
        med_ext = median_auc(sub["External_AUC_GSE104836"].tolist())
        lines.append(
            f"| {wf} | {len(sub)} | {conc:.1f}% | "
            f"{med_tr if med_tr is not None else '—'} | "
            f"{med_ext if med_ext is not None else '—'} |"
        )

    if is_placeholder:
        lines.extend([
            "",
            "> **Action:** Copy Shiny exports into `validation_manual/` (see README_BENCHMARK_EXPORTS.md), then re-run this script.",
        ])

    summary_path.write_text("\n".join(lines) + "\n", encoding="utf-8")

    print(f"Consensus: {len(consensus_genes)} genes from {consensus_src}")
    print(f"AUC maps: {len(train_auc)} train, {len(ext_auc)} external ({auc_src})")
    for wf in df["Workflow"].unique():
        sub = df[df["Workflow"] == wf]
        conc = sub["Direction_concordant"].mean() * 100
        print(f"  {wf}: n={len(sub)}, direction={conc:.1f}%")
    print(f"Wrote {out_csv} ({len(df)} rows)")
    print(f"Wrote {summary_path}")


if __name__ == "__main__":
    main()
