# Sync vignette/images/step*.png from source image_N / Image_N files.
$dir = Join-Path (Split-Path (Split-Path $PSScriptRoot -Parent) -Parent) "vignettes\images"
$map = [ordered]@{
  "image_1 download.png" = "step01_download.png"
  "image_2a quality control.png" = "step02a_qc.png"
  "image_2b quality control.png" = "step02b_qc.png"
  "image_2c quality control.png" = "step02c_qc.png"
  "image_3 Normalization.png" = "step03a_normalize.png"
  "image_3b Normalization.png" = "step03b_normalize.png"
  "image_4a group selection.png" = "step04a_groups.png"
  "image_4b group selection.png" = "step04b_groups.png"
  "image_5a batch effect.png" = "step05a_batch.png"
  "Image_5b batch effect.png" = "step05b_batch.png"
  "Image_6a DE.png" = "step06a_de.png"
  "Image_6b DE.png" = "step06b_de.png"
  "Image_7a WCGNA.png" = "step07a_wgcna.png"
  "Image_7b WGCNA.png" = "step07b_wgcna.png"
  "Image_7c WGCNA.png" = "step07c_wgcna.png"
  "Image_7d WGCNA.png" = "step07d_wgcna.png"
  "Image_8a Common DE and Wgcna.png" = "step08a_common_genes.png"
  "Image_8b Go analysis.png" = "step08b_go.png"
  "Image_8c Kegg analysis.png" = "step08c_kegg.png"
  "Image_9a PPI.png" = "step09a_ppi.png"
  "Image_9b PPI.png" = "step09b_ppi.png"
  "Image_10a ML.png" = "step10a_ml.png"
  "Image_10b ML.png" = "step10b_ml.png"
  "Image_11a validation step.png" = "step11a_validation.png"
  "Image_11b validation(external).png" = "step11b_validation_external.png"
  "Image_12 AUC.png" = "step12a_roc.png"
  "Image_12b AUC.png" = "step12b_roc.png"
  "Image_13a Nomogram analysis.png" = "step13_nomogram.png"
  "Image_14a GSEA.png" = "step14_gsea.png"
  "Image_15 Summary.png" = "step15_summary.png"
}
foreach ($src in $map.Keys) {
  $from = Join-Path $dir $src
  $to = Join-Path $dir $map[$src]
  if (-not (Test-Path $from)) {
    Write-Warning "Missing source: $src"
    continue
  }
  Copy-Item $from $to -Force
  Write-Output "Synced $($map[$src])"
}
