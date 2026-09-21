#!/usr/bin/env python3
"""Print Supplementary Table S1 concordance metrics."""

from pathlib import Path

import pandas as pd

base = Path(__file__).resolve().parents[2] / "validation_manual"


def jaccard(a: set, b: set) -> float:
    u = a | b
    return len(a & b) / len(u) if u else float("nan")


methods = ["limma", "limma_voom", "deseq2", "edger"]
for gse in ["GSE50760", "GSE104836"]:
    sig = {m: set(pd.read_csv(base / f"{gse}_{m}_DE_sig.csv")["Gene"]) for m in methods}
    print(f"\n=== {gse} ===")
    for m in methods:
        print(f"  {m}: {len(sig[m])} DEGs")
    d = sig["deseq2"]
    for m in methods:
        if m != "deseq2":
            print(f"  DESeq2 vs {m} Jaccard: {jaccard(d, sig[m]):.3f}")

tr = {m: set(pd.read_csv(base / f"GSE50760_{m}_DE_sig.csv")["Gene"]) for m in methods}
va = {m: set(pd.read_csv(base / f"GSE104836_{m}_DE_sig.csv")["Gene"]) for m in methods}
print("\n=== Cross-cohort ===")
for m in methods:
    j = jaccard(tr[m], va[m])
    shared = len(tr[m] & va[m])
    print(f"  {m}: shared={shared}, Jaccard={j:.3f}")
