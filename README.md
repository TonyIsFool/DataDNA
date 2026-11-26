# DataDNA

[![R-CMD-check](https://github.com/TonyIsFool/DataDNA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/TonyIsFool/DataDNA/actions/workflows/R-CMD-check.yaml)

**DataDNA** is a **zero-dependency** R package (pure base R >= 4.1.0) that gives
every data frame a compact fingerprint, drift diagnosis, lineage match, and
report-ready figure.

Instead of only asking *"what is in this table?"*, DataDNA asks:

- **What kind of data set is this?** (`customer_table`, `event_stream`, `transaction_log`, `wide_feature_matrix`, ...)
- **How stable is its identity?** (schema, shape, and statistical profile hashes + drift risk)
- **Did this version drift from the previous one?** (missingness, category shifts, numeric shifts, temporal window shifts)
- **Were columns renamed or repurposed?** (automatic column rename & role-change inference)

The package is built for analysts and data scientists who receive recurring CSVs,
warehouse extracts, or modeling tables and want fast, memory-light recognition
and comparison.

---

## Quick Start

```r
library(DataDNA)

demo <- dna_example_customers()

# 1. Profile a single data frame
dna <- data_dna(demo$customers_new, name = "customers_new")
dna
summary(dna)

# Save a static paper-style figure (.png / .pdf / .svg) or self-contained HTML card
dna_plot(dna, file = "customers_profile.png")
dna_card(dna, file = "customers_dna.html")
```

![DataDNA Specimen Profile](man/figures/dna-plot.png)

---

## Compare Versions & Diagnose Drift

`dna_compare()` and `dna_diff()` combine exact schema overlap with automatic
column-rename matching, table shape, species, role structure, adaptive numeric
distribution shifts, missingness, category top-values, uniqueness, and temporal
window coverage:

```r
dna_compare(demo$customers_old, demo$customers_new)
dna_diff(demo$customers_old, demo$customers_new)

# Render a report-ready drift & mutation figure
dna_diff_plot(demo$customers_old, demo$customers_new, file = "drift_report.png")
```

![DataDNA Drift Report](man/figures/dna-diff-plot.png)

---

## Find the Closest Ancestor in a DNA Library

Build a lightweight fingerprint library from a list of tables or directly from a
directory of `.csv` / `.tsv` / `.rds` / `.rda` files (raw tables are profiled
one by one and not retained in memory):

```r
lib <- dna_library(list(
  customers_2024 = customers_old,
  customers_2025 = customers_new
))

match <- dna_match(customers_new, lib)
match

# Static PNG / PDF / SVG lineage figure for papers, slides, and reports
dna_match_plot(match, file = "lineage.png")
```

![DataDNA Lineage Match](man/figures/dna-match-plot.png)

---

## Core API & S3 Methods

```r
# Profiling & specimen visuals
data_dna(df)
dna_species(df)
dna_plot(df, file = "specimen.png")
dna_card(df, file = "specimen.html")

# Version comparison & mutation reports
dna_compare(old_df, new_df)
dna_diff(old_df, new_df)
dna_diff_plot(old_df, new_df, file = "drift.png")

# Fingerprint libraries & lineage matching
dna_library(list_or_path, file = "library.rds")
dna_match(query_df, lib)
dna_match_plot(match, file = "lineage.png")
dna_match_card(match, file = "lineage.html")

# Standard R generics
summary(dna)
plot(dna)
plot(dna_old, dna_new)
plot(match)
as.data.frame(dna)
as.data.frame(dna_diff(old_df, new_df))
as.data.frame(match)
as.data.frame(lib)
```

---

## Installation

From GitHub:

```r
install.packages("remotes")
remotes::install_github("TonyIsFool/DataDNA")
```

Or from a local source tarball (requires only base R >= 4.1.0):

```r
install.packages("DataDNA_0.2.0.tar.gz", repos = NULL, type = "source")
```
