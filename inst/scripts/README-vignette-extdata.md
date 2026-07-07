# Bundled example data (`inst/extdata/`)

## Files

| File | Description |
|------|-------------|
| `vignette_expression.csv` | Synthetic log-scale expression matrix (120 genes × 12 samples) |
| `vignette_sample_metadata.csv` | Sample metadata (Dataset, Condition) aligned to expression columns |

## Provenance

These files are **synthetic demonstration data** for the user vignette and
`R CMD check` examples. They are **not** real patient or GEO-derived data.

## Regeneration

From the package root:

```bash
Rscript inst/scripts/make-vignette-extdata.R
```

The script sets `set.seed(42)` and writes both CSV files to `inst/extdata/`.

## License

Same as GExPipe (MIT). Safe to redistribute with the package.
