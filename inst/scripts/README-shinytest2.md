# Shiny integration tests (`shinytest2`)

GExPipe uses [`shinytest2`](https://rstudio.github.io/shinytest2/) for browser-level
tests of the live Shiny app (welcome screen, analysis dashboard, sidebar navigation).

## Run locally

From the package root in R:

```r
pkgload::load_all(".")
if (!requireNamespace("shinytest2", quietly = TRUE)) {
  install.packages("shinytest2")
}
if (!requireNamespace("chromote", quietly = TRUE)) {
  install.packages("chromote")
}
testthat::test_file("tests/testthat/test-shiny-integration.R")
```

Or during check (uses installed package):

```bash
R CMD check .
```

## Environment variables

| Variable | Effect |
|----------|--------|
| `GEXPIPE_SKIP_SHINYTEST2=1` | Skip all shinytest2 tests |
| `GEXPIPE_SHINYTEST2_MS=600000` | App load timeout (default 300000 ms) |
| `GEXPIPE_SHINYTEST2_USE_PKGLOAD=1` | Child app process uses `pkgload::load_all()` instead of installed GExPipe |

## Record new interactions (optional)

Interactive recording (opens browser; you click through the app):

```r
pkgload::load_all(".")
source("inst/scripts/record-shinytest2-geo.R")
```

Or manually:

```r
shinytest2::record_test(
  app = GExPipe::runGExPipe(launch.browser = FALSE),
  name = "geo-download"
)
```

## GEO download test (automated)

`test-shiny-integration.R` includes:

| Test | Network | Description |
|------|---------|-------------|
| `start processing warns when no GSE IDs` | No | Clicks Start with empty GSE box |
| `start processing with GSE ID updates download log` | Yes | Enters `GSE62646` (or `GEXPIPE_SHINYTEST2_GSE`) and clicks Start |

Environment variables:

| Variable | Default | Effect |
|----------|---------|--------|
| `GEXPIPE_SKIP_SHINYTEST2_GEO=1` | off | Skip GEO network test |
| `GEXPIPE_SHINYTEST2_GSE` | `GSE62646` | GSE accession to download |
| `GEXPIPE_SHINYTEST2_GEO_MS` | `180000` | Max wait for download log (ms) |

Tests live in `tests/testthat/test-shiny-integration.R`; helpers in
`tests/testthat/helper-shinytest2.R`.
