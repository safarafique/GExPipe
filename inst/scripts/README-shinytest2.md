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

```r
pkgload::load_all(".")
shinytest2::record_test(
  app = GExPipe::runGExPipe(launch.browser = FALSE),
  name = "my-new-scenario"
)
```

Save generated files under `tests/testthat/_shinytest/` if you add recorded scenarios.

Tests live in `tests/testthat/test-shiny-integration.R`; helpers in
`tests/testthat/helper-shinytest2.R`.
