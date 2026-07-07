# Generate illustrative vignette screenshots (maintainer script).
$dir = Join-Path (Split-Path (Split-Path $PSScriptRoot -Parent) -Parent) "vignettes\images"
New-Item -ItemType Directory -Force -Path $dir | Out-Null
Add-Type -AssemblyName System.Drawing

function New-VigScreenshot {
  param(
    [string]$Path,
    [string]$Title,
    [string]$Subtitle
  )
  $w = 1400
  $h = 820
  $bmp = New-Object System.Drawing.Bitmap $w, $h
  $g = [System.Drawing.Graphics]::FromImage($bmp)
  $g.SmoothingMode = [System.Drawing.Drawing2D.SmoothingMode]::AntiAlias
  $g.TextRenderingHint = [System.Drawing.Text.TextRenderingHint]::AntiAliasGridFit
  $g.Clear([System.Drawing.Color]::FromArgb(248, 249, 252))
  $headerBrush = New-Object System.Drawing.Drawing2D.LinearGradientBrush (
    [System.Drawing.Rectangle]::new(0, 0, $w, 120),
    [System.Drawing.Color]::FromArgb(102, 126, 234),
    [System.Drawing.Color]::FromArgb(118, 75, 162),
    45
  )
  $g.FillRectangle($headerBrush, 0, 0, $w, 120)
  $titleFont = New-Object System.Drawing.Font("Segoe UI", 28, [System.Drawing.FontStyle]::Bold, [System.Drawing.GraphicsUnit]::Pixel)
  $g.DrawString("GExPipe", $titleFont, [System.Drawing.Brushes]::White, 24, 36)
  $sideBrush = New-Object System.Drawing.SolidBrush ([System.Drawing.Color]::FromArgb(44, 62, 80))
  $g.FillRectangle($sideBrush, 0, 120, 260, $h - 120)
  $g.FillRectangle([System.Drawing.Brushes]::White, 280, 160, $w - 320, $h - 200)
  $borderPen = New-Object System.Drawing.Pen ([System.Drawing.Color]::FromArgb(220, 224, 232)), 2
  $g.DrawRectangle($borderPen, 280, 160, $w - 320, $h - 200)
  $stepFont = New-Object System.Drawing.Font("Segoe UI", 22, [System.Drawing.FontStyle]::Bold, [System.Drawing.GraphicsUnit]::Pixel)
  $subFont = New-Object System.Drawing.Font("Segoe UI", 14, [System.Drawing.FontStyle]::Regular, [System.Drawing.GraphicsUnit]::Pixel)
  $accent = New-Object System.Drawing.SolidBrush ([System.Drawing.Color]::FromArgb(52, 73, 94))
  $muted = New-Object System.Drawing.SolidBrush ([System.Drawing.Color]::FromArgb(120, 130, 145))
  $g.DrawString($Title, $stepFont, $accent, 320, 200)
  $g.DrawString($Subtitle, $subFont, $muted, 320, 250)
  $bmp.Save($Path, [System.Drawing.Imaging.ImageFormat]::Png)
  $g.Dispose()
  $bmp.Dispose()
}

$shots = @(
  @("step1_download.png", "Step 1 - Download Data", "Enter GEO accessions and download merged expression data."),
  @("step2_qc.png", "Step 2 - Quality Control", "Review boxplots, density curves, and outlier flags."),
  @("step6_de.png", "Step 6 - Differential Expression", "Volcano plot and top differentially expressed genes."),
  @("step9_ppi.png", "Step 9 - PPI Network", "STRING protein-protein interaction network and hub genes."),
  @("step15_summary.png", "Step 15 - Summary Report", "Download the integrated PDF results summary.")
)
foreach ($s in $shots) {
  New-VigScreenshot -Path (Join-Path $dir $s[0]) -Title $s[1] -Subtitle $s[2]
}
Get-ChildItem $dir | Format-Table Name, Length
