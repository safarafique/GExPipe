Place application screenshots for `vignettes/GExPipe.Rmd` in this folder.

The vignette includes them automatically when these PNG files exist (via
`system.file("extdata", "vignette-screenshots", ..., package = "GExPipe")`).

## Capture workflow

1. Launch the app: `app <- GExPipe::runGExPipe(); shiny::runApp(app)`.
2. Walk through Steps 1–15; capture the main panel (sidebar visible).
3. Save PNGs using the filenames below (1400–2000 px wide recommended).
4. Rebuild the vignette: `rmarkdown::render("vignettes/GExPipe.Rmd")`.

## Required filenames

| File | Panel |
|------|-------|
| `step1_download.png` | Step 1 — Download Data |
| `step2_qc.png` | Step 2 — Quality Control |
| `step6_de.png` | Step 6 — Differential Expression (volcano visible) |
| `step9_ppi.png` | Step 9 — PPI Network |
| `step15_summary.png` | Step 15 — Summary Report |

Do not include real patient identifiers in screenshots.
