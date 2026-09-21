#!/usr/bin/env python3
"""
Prove the publication RNA-seq claim:

  GExPipe 20-gene consensus reaches high median external single-gene AUC,
  using 2.5-fold fewer genes than DE-only top-50 panels from limma-voom
  and DESeq2 (trained on GSE50760, scored on GSE104836).

Primary baselines (RNA-seq): limma_voom, deseq2
Optional (merge continuity only): limma

Inputs (default under validation_manual/):
  - consensus_signature_genes.csv
  - ROC_AUC_Training_vs_Validation.csv
  - ROC_AUC_baselines_top50.csv

Usage:
  python inst/scripts/verify-paper-auc-claim.py --repo E:/GExPipe
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

import pandas as pd


CLAIMS = {
    "n_consensus": 20,
    "median_consensus_ext": 0.98,
    "n_top50": 50,
    "median_limma_voom_ext": 0.95,  # previously measured; still checked if present
    "fold_fewer": 2.5,
}


def round3(x: float) -> float:
    return round(float(x), 3)


def load_genes(path: Path) -> list[str]:
    df = pd.read_csv(path)
    col = "Gene" if "Gene" in df.columns else df.columns[0]
    genes = df[col].astype(str).str.strip().replace({"nan": ""}).tolist()
    return [g for g in genes if g and not g.startswith("#")]


def check(name: str, ok: bool, detail: str, failures: list[str]) -> None:
    status = "PASS" if ok else "FAIL"
    print(f"  [{status}] {name}: {detail}")
    if not ok:
        failures.append(name)


def median_ext(df: pd.DataFrame) -> float:
    if df.empty:
        return float("nan")
    return round3(df["AUC_External"].median())


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--repo", default=".", help="Repo root (default: cwd)")
    ap.add_argument("--outdir", default=None, help="Folder with ROC CSVs")
    ap.add_argument("--tol", type=float, default=0.005, help="AUC tolerance")
    args = ap.parse_args()

    repo = Path(args.repo).resolve()
    outdir = Path(args.outdir).resolve() if args.outdir else repo / "validation_manual"
    tol = args.tol

    cons_path = outdir / "consensus_signature_genes.csv"
    roc_path = outdir / "ROC_AUC_Training_vs_Validation.csv"
    base_path = outdir / "ROC_AUC_baselines_top50.csv"

    print("=" * 72)
    print("VERIFY: consensus 20 vs limma-voom top-50 vs DESeq2 top-50")
    print("=" * 72)
    print(f"Repo:   {repo}")
    print(f"Outdir: {outdir}")
    print()

    missing = [p for p in (cons_path, roc_path, base_path) if not p.exists()]
    if missing:
        print("ERROR: required files missing:")
        for p in missing:
            print(f"  - {p}")
        print("\nAfter app DE downloads, run:")
        print(f"  Rscript inst/scripts/fill-limma-baseline-aucs.R --repo {repo}")
        return 2

    cons = load_genes(cons_path)
    roc = pd.read_csv(roc_path)
    base = pd.read_csv(base_path)

    if "Gene" not in roc.columns or "AUC_External" not in roc.columns:
        print("ERROR: ROC file needs Gene, AUC_External")
        return 2
    if not {"Workflow", "Gene", "AUC_External"}.issubset(base.columns):
        print("ERROR: baselines file needs Workflow, Gene, AUC_External")
        return 2

    cons_auc = roc[roc["Gene"].isin(cons)].copy()
    voom = base[base["Workflow"] == "limma_voom"].copy()
    deseq2 = base[base["Workflow"] == "deseq2"].copy()
    limma = base[base["Workflow"] == "limma"].copy()

    med_cons = median_ext(cons_auc)
    med_voom = median_ext(voom)
    med_deseq2 = median_ext(deseq2)
    med_limma = median_ext(limma)
    n_cons = len(cons)
    fold = round(CLAIMS["n_top50"] / n_cons, 2) if n_cons else float("nan")

    print("Observed medians (external single-gene AUC on GSE104836)")
    print(f"  GExPipe consensus (n={n_cons}, matched={len(cons_auc)}): {med_cons}")
    print(f"  limma-voom top-50 (n={len(voom)}): {med_voom}")
    print(f"  DESeq2 top-50     (n={len(deseq2)}): {med_deseq2}")
    if len(limma):
        print(f"  limma top-50      (n={len(limma)}): {med_limma}  [optional]")
    print(f"  Fold fewer genes vs top-50: {fold}x")
    print()

    print("Publication claim checks (RNA-seq baselines)")
    failures: list[str] = []
    check("panel size = 20", n_cons == CLAIMS["n_consensus"], f"n={n_cons}", failures)
    check(
        "all consensus genes have external AUC",
        len(cons_auc) == n_cons and cons_auc["AUC_External"].notna().all(),
        f"matched={len(cons_auc)} / {n_cons}",
        failures,
    )
    check(
        "consensus median external AUC = 0.98",
        abs(med_cons - CLAIMS["median_consensus_ext"]) <= tol,
        f"{med_cons} (claim {CLAIMS['median_consensus_ext']})",
        failures,
    )
    check("limma-voom top-50 present (n=50)", len(voom) == CLAIMS["n_top50"], f"n={len(voom)}", failures)
    check("DESeq2 top-50 present (n=50)", len(deseq2) == CLAIMS["n_top50"], f"n={len(deseq2)}", failures)
    if len(voom) == CLAIMS["n_top50"]:
        check(
            "limma-voom median matches prior estimate 0.95 (info)",
            abs(med_voom - CLAIMS["median_limma_voom_ext"]) <= tol,
            f"{med_voom}",
            failures,
        )
        check(
            "consensus superior or equal to limma-voom",
            med_cons >= med_voom,
            f"consensus={med_cons} vs limma-voom={med_voom}",
            failures,
        )
    if len(deseq2) == CLAIMS["n_top50"]:
        check(
            "consensus superior or equal to DESeq2",
            med_cons >= med_deseq2,
            f"consensus={med_cons} vs DESeq2={med_deseq2}",
            failures,
        )
    check(
        "2.5-fold fewer genes",
        abs(fold - CLAIMS["fold_fewer"]) < 1e-9,
        f"{fold}-fold",
        failures,
    )

    print()
    print("No-leakage / cohort separation (file provenance)")
    for p in (
        outdir / "GSE50760_limma_voom_DE_all.csv",
        outdir / "GSE50760_deseq2_DE_all.csv",
    ):
        check(
            f"train DE file present ({p.name})",
            p.exists(),
            "top-50 ranked on GSE50760 only" if p.exists() else "MISSING — download from app Step 6",
            failures,
        )
    check(
        "external AUCs stored separately from training",
        "AUC_External" in roc.columns and "AUC_Internal" in roc.columns,
        "Internal=GSE50760, External=GSE104836",
        failures,
    )

    print()
    print("Consensus 20-gene external AUCs (sorted)")
    show = (
        cons_auc[["Gene", "AUC_Internal", "AUC_External"]]
        .sort_values("AUC_External", ascending=False)
        .reset_index(drop=True)
    )
    show["AUC_Internal"] = show["AUC_Internal"].map(lambda x: round(float(x), 4))
    show["AUC_External"] = show["AUC_External"].map(lambda x: round(float(x), 4))
    print(show.to_string(index=False))

    report_dir = outdir / "scoring_report"
    report_dir.mkdir(parents=True, exist_ok=True)
    rows = [
        {
            "Workflow": "GExPipe consensus",
            "Panel_size": n_cons,
            "Median_external_AUC": med_cons,
            "Role": "primary claim",
        },
        {
            "Workflow": "limma-voom top-50",
            "Panel_size": len(voom),
            "Median_external_AUC": med_voom,
            "Role": "primary RNA-seq baseline",
        },
        {
            "Workflow": "DESeq2 top-50",
            "Panel_size": len(deseq2),
            "Median_external_AUC": med_deseq2,
            "Role": "primary RNA-seq baseline",
        },
    ]
    if len(limma):
        rows.append(
            {
                "Workflow": "limma top-50",
                "Panel_size": len(limma),
                "Median_external_AUC": med_limma,
                "Role": "optional / merge continuity only",
            }
        )
    summary = pd.DataFrame(rows)
    out_csv = report_dir / "verify_paper_auc_claim.csv"
    summary.to_csv(out_csv, index=False)
    show.to_csv(report_dir / "verify_consensus20_per_gene_auc.csv", index=False)

    print()
    print(f"Wrote: {out_csv}")
    print(f"Wrote: {report_dir / 'verify_consensus20_per_gene_auc.csv'}")
    print()
    if failures:
        print(f"RESULT: FAIL ({len(failures)} check(s) failed)")
        for f in failures:
            print(f"  - {f}")
        if not len(deseq2):
            print("\nDESeq2 baseline missing. After downloading app DE CSV as")
            print("  validation_manual/GSE50760_deseq2_DE_all.csv")
            print(f"run: Rscript inst/scripts/fill-limma-baseline-aucs.R --repo {repo}")
        return 1

    print("RESULT: PASS — consensus vs limma-voom vs DESeq2 checks OK.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
