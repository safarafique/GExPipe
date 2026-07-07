# Vignette screenshots (`vignettes/images/`)

PNG files referenced from `vignettes/GExPipe.Rmd` live next to the vignette
source (not under `inst/extdata/`).

## Regeneration

From the package root on Windows:

```powershell
powershell -ExecutionPolicy Bypass -File inst/scripts/make-vignette-screenshots.ps1
```

Replace the generated PNGs with real app captures when available:

| File | Panel |
|------|-------|
| `step1_download.png` | Step 1 — Download Data |
| `step2_qc.png` | Step 2 — Quality Control |
| `step6_de.png` | Step 6 — Differential Expression |
| `step9_ppi.png` | Step 9 — PPI Network |
| `step15_summary.png` | Step 15 — Summary Report |

Do not include real patient identifiers in screenshots.
