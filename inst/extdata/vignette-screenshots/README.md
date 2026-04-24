Place real application screenshots for `vignettes/GExPipe.Rmd` in this folder.

To add screenshots to the vignette:

1. Run the app (`shiny::runGitHub("GExPipe", "safarafique", ref="main", subdir="inst/shinyapp")`).
2. Capture each step using your OS screenshot tool or browser "Save as image".
3. Save as PNG using the filenames below.
4. Re-add the Screenshots section to the vignette (see git history for the chunk template).

Recommended files:

- `step1_download.png`  — Step 1: Download Data panel
- `step2_qc.png`        — Step 2: Quality Control panel
- `step6_de.png`        — Step 6: Differential Expression (volcano plot visible)
- `step9_ppi.png`       — Step 9: PPI Network (interactive graph visible)
- `step15_summary.png`  — Step 15: Summary Report

Tips:

- PNG format, 1400–2000 px wide for legible text in HTML vignette output.
- Capture the main panel with the sidebar visible where possible.
- Do not include screenshots containing real patient data or sensitive identifiers.
