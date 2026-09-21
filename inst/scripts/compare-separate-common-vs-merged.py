#!/usr/bin/env python3
"""Compare separate-then-common DEGs vs merged DEGs."""

from pathlib import Path

import numpy as np
import pandas as pd

b = Path(r"E:/GExPipe/validation_manual/cross_platform")
out = b / "report"
out.mkdir(exist_ok=True)

merged_sig = set(pd.read_csv(b / "merged_limma_DE_sig.csv")["Gene"])
micro_sig = set(pd.read_csv(b / "microarray_limma_DE_sig.csv")["Gene"])
rna_sig = set(pd.read_csv(b / "rnaseq_DE_sig.csv")["Gene"])
sep_common = micro_sig & rna_sig

both = sep_common & merged_sig
only_sep = sep_common - merged_sig
only_merged = merged_sig - sep_common
union = sep_common | merged_sig
jaccard = len(both) / len(union) if union else 0.0

print("=== SIGNIFICANT DEG COUNTS ===")
print(f"Microarray-only DEGs: {len(micro_sig)}")
print(f"RNA-seq-only DEGs:    {len(rna_sig)}")
print(f"Separate-then-COMMON: {len(sep_common)}  (micro INTERSECT rna)")
print(f"Merged limma DEGs:    {len(merged_sig)}")
print()
print("=== SEPARATE-COMMON vs MERGED ===")
print(f"Same (in both):                       {len(both)}")
print(f"Only in separate-common (not merged): {len(only_sep)}")
print(f"Only in merged (not separate-common): {len(only_merged)}")
print(f"Jaccard index:                        {jaccard:.3f}")
print(f"% separate-common recovered by merged: {100 * len(both) / len(sep_common):.1f}%")
print(f"% merged that were also separate-common: {100 * len(both) / len(merged_sig):.1f}%")

cmp = pd.read_csv(b / "common_genes_DE_comparison.csv")
sub = cmp[cmp["sig_both_separate"] == True]
print()
print(f"Genes with DE stats in all 3 analyses: {len(cmp)}")
print(f"Separate-common genes in comparison table: {len(sub)}")

dir_ok = r = med_m = med_r = None
if len(sub):
    dir_ok = float((sub["direction_merged_vs_micro"] & sub["direction_merged_vs_rna"]).mean() * 100)
    r = float(np.corrcoef(sub["logFC_merged"], sub["logFC_mean_separate"])[0, 1])
    med_m = float(sub["abs_diff_merged_micro"].median())
    med_r = float(sub["abs_diff_merged_rna"].median())
    print(f"Direction concordance merged vs BOTH separate: {dir_ok:.1f}%")
    print(f"logFC Pearson r (merged vs mean separate): {r:.3f}")
    print(f"Median |merged-micro| logFC: {med_m:.3f}")
    print(f"Median |merged-rna| logFC:   {med_r:.3f}")
    print(f"Mean |merged-micro| logFC:   {sub['abs_diff_merged_micro'].mean():.3f}")
    print(f"Mean |merged-rna| logFC:     {sub['abs_diff_merged_rna'].mean():.3f}")

r_all = float(np.corrcoef(cmp["logFC_merged"], cmp["logFC_mean_separate"])[0, 1])
print()
print("=== ALL COMMON GENES ===")
print(f"logFC Pearson r merged vs mean(separate): {r_all:.3f}")
print(f"Direction concordance merged vs micro: {cmp['direction_merged_vs_micro'].mean()*100:.1f}%")
print(f"Direction concordance merged vs rna:   {cmp['direction_merged_vs_rna'].mean()*100:.1f}%")

summary = pd.DataFrame(
    [
        {"Metric": "Microarray-only significant DEGs", "Value": len(micro_sig)},
        {"Metric": "RNA-seq-only significant DEGs", "Value": len(rna_sig)},
        {"Metric": "Separate-then-COMMON (micro INTERSECT rna)", "Value": len(sep_common)},
        {"Metric": "Merged limma significant DEGs", "Value": len(merged_sig)},
        {"Metric": "Overlap (same in both approaches)", "Value": len(both)},
        {"Metric": "Only separate-common (missed by merged)", "Value": len(only_sep)},
        {"Metric": "Only merged (gained by integration)", "Value": len(only_merged)},
        {"Metric": "Jaccard (separate-common vs merged)", "Value": round(jaccard, 3)},
        {"Metric": "% separate-common recovered by merged", "Value": round(100 * len(both) / len(sep_common), 1)},
        {"Metric": "% merged that were also separate-common", "Value": round(100 * len(both) / len(merged_sig), 1)},
        {"Metric": "Direction concordance % (among sep-common)", "Value": round(dir_ok, 1) if dir_ok is not None else ""},
        {"Metric": "logFC Pearson r merged vs mean separate (sep-common)", "Value": round(r, 3) if r is not None else ""},
        {"Metric": "Median |logFC merged-micro| (sep-common)", "Value": round(med_m, 3) if med_m is not None else ""},
        {"Metric": "Median |logFC merged-rna| (sep-common)", "Value": round(med_r, 3) if med_r is not None else ""},
        {"Metric": "logFC Pearson r merged vs mean separate (all common genes)", "Value": round(r_all, 3)},
    ]
)
summary.to_csv(out / "separate_common_vs_merged_summary.csv", index=False)
pd.DataFrame({"Gene": sorted(only_sep)}).to_csv(out / "only_in_separate_common.csv", index=False)
pd.DataFrame({"Gene": sorted(only_merged)}).to_csv(out / "only_in_merged.csv", index=False)
pd.DataFrame({"Gene": sorted(both)}).to_csv(out / "same_in_both.csv", index=False)

md = [
    "# Separate-then-common DEGs vs Merged DE",
    "",
    "## Verdict",
    "",
    f"**Not the same.** Jaccard = **{jaccard:.3f}**. "
    f"Merged recovers **{100 * len(both) / len(sep_common):.1f}%** of separate-common DEGs, "
    f"but **{len(only_merged)}** merged DEGs are not in the separate-common list "
    f"({100 * len(only_merged) / len(merged_sig):.1f}% of merged).",
    "",
    "## Counts",
    "",
    "| Approach | Significant DEGs |",
    "|----------|-----------------:|",
    f"| Microarray-only | {len(micro_sig)} |",
    f"| RNA-seq-only | {len(rna_sig)} |",
    f"| Separate-then-COMMON (intersection) | {len(sep_common)} |",
    f"| Merged limma | {len(merged_sig)} |",
    f"| Same in both approaches | {len(both)} |",
    f"| Only separate-common | {len(only_sep)} |",
    f"| Only merged | {len(only_merged)} |",
    "",
    "## Effect-size agreement (among separate-common genes)",
    "",
]
if dir_ok is not None:
    md.extend(
        [
            f"- Direction concordance (merged vs both separate): **{dir_ok:.1f}%**",
            f"- logFC Pearson r (merged vs mean of separate): **{r:.3f}**",
            f"- Median |ΔlogFC| merged–microarray: **{med_m:.3f}**",
            f"- Median |ΔlogFC| merged–RNA-seq: **{med_r:.3f}**",
            "",
        ]
    )
md.extend(
    [
        "## Interpretation for the paper",
        "",
        "1. Separate DE then take common genes ≠ merged DE gene list.",
        "2. High direction concordance shows the biology is consistent.",
        "3. Moderate Jaccard is expected: joint limma uses all samples + Dataset covariate;",
        "   list intersection has no shared statistical model.",
        "4. Genes only found by merge are the added value of integration.",
        "",
        f"Summary CSV: `{out / 'separate_common_vs_merged_summary.csv'}`",
    ]
)
(out / "separate_common_vs_merged_report.md").write_text("\n".join(md), encoding="utf-8")
print()
print("Wrote", out / "separate_common_vs_merged_summary.csv")
print("Wrote", out / "separate_common_vs_merged_report.md")
