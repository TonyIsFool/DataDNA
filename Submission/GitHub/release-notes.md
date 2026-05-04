# DataDNA 0.1.0

Initial public release candidate for DataDNA.

## Highlights

- Profile R data frames as compact data fingerprints with `data_dna()`.
- Render portable HTML identity cards with `dna_card()`.
- Compare table versions with `dna_compare()` and `dna_diff()`.
- Find related historical data sets with `dna_match()`.
- Generate report-ready PNG/PDF lineage figures with `dna_match_plot()`.

## Data

This release includes only small synthetic customer examples for tests and
documentation:

- `customers_old`
- `customers_new`

No commercial or production data should be committed with this release.

## Check Status

Local full CRAN-style check:

```text
R CMD check DataDNA_0.1.0.tar.gz --as-cran
0 ERROR, 0 WARNING, 1 NOTE
```

The remaining NOTE is `New submission`.
