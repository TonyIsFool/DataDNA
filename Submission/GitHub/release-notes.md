# DataDNA 0.2.0

Major quality, algorithm, and reporting upgrade for DataDNA.

## Highlights

- **Zero External Dependencies**: Replaced `htmltools` with a lightweight built-in HTML5/SVG generator (`R >= 4.1.0` base packages only).
- **Edge-Case & CRAN Hardening**: Fixed crashes on 0-row, 0-column, and all-`NA` data frames; preserved caller `.Random.seed` in `dna_example_customers()`; sharpened ID vs. low-cardinality category role inference.
- **Smarter Drift & Rename Detection**: Added automatic column rename inference (`cust_id -> customer_id`), temporal window/regularity drift scoring, role transition tracking, and spread-aware numeric drift scaling.
- **Batch Fingerprint Libraries**: Added `dna_library()` to stream-profile lists or directories of `.csv`, `.tsv`, `.rds`, and `.rda` files and persist compact `.rds` libraries.
- **Full Static Figure Suite + SVG + S3 Generics**: Added `dna_plot()` and `dna_diff_plot()`, `.svg` vector export across all static plots, precision DNA electrophoresis alignment figures, interactive light/dark HTML instruments (`dna_card()`, `dna_match_card()`), and `summary()`, `plot()`, and `as.data.frame()` S3 methods.
