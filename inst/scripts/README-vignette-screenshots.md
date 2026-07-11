# Vignette screenshots (maintainers)

End-user figures live in **`vignettes/images/`**. The vignette references
knitr-safe copies (`step01_*.png` … `step15_*.png`).

## Steps 1–5 (sidebar numbering)

| App step | Source file(s) | Vignette copy |
|----------|----------------|---------------|
| 1 Download | `image_1 download.png` | `step01_download.png` |
| 2 QC | `image_2a/b/c quality control.png` | `step02a/b/c_qc.png` |
| 3 Normalize | `image_3 Normalization.png`, `image_3b Normalization.png` | `step03a/b_normalize.png` |
| 4 Groups | `image_4a/b group selection.png` | `step04a/b_groups.png` |
| 5 Batch | `image_5a batch effect.png`, `Image_5b batch effect.png` | `step05a/b_batch.png` |
| 6 DE | `Image_6a/b DE.png` | `step06a/b_de.png` |
| 7 WGCNA | `Image_7a WCGNA.png`, `Image_7b–d WGCNA.png` | `step07a–d_wgcna.png` |
| 8 Common genes | `Image_8a Common DE and Wgcna.png`, `Image_8b/c` GO/KEGG | `step08a_common_genes.png`, `step08b_go.png`, `step08c_kegg.png` |
| 9 PPI | `Image_9a/b PPI.png` | `step09a/b_ppi.png` |
| 10 ML | `Image_10a/b ML.png` | `step10a/b_ml.png` |

**Note:** Step 6+ use `Image_6` … `Image_15` (DE starts at Image_6, not Image_5).

After updating source PNGs, re-sync copies:

```powershell
powershell -ExecutionPolicy Bypass -File inst/scripts/sync-vignette-images.ps1
```

Or copy manually from the table above before `R CMD build`.
