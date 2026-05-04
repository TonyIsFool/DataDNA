# DataDNA

**DataDNA** is an R package that gives every data frame a compact fingerprint,
lineage match, and report-ready identity figure.

Instead of only asking "what is in this table?", DataDNA asks:

- What kind of data set is this?
- How stable is its identity?
- Did this version drift from the previous one?
- Which columns changed their role, missingness, categories, or distribution?

The package is designed for analysts who receive CSVs, extracts, dashboards, or
modeling data sets and need a fast way to recognize and compare them.

## Example

```r
library(DataDNA)

demo <- dna_example_customers()

dna <- data_dna(demo$customers_new, name = "customers_new")
dna

card <- dna_card(dna, file = "customers_dna.html")

dna_compare(demo$customers_old, demo$customers_new)
dna_diff(demo$customers_old, demo$customers_new)
```

`dna_compare()` combines exact schema overlap with shape, species, role
structure, distribution, missingness, category, and identity signals. This makes
the score feel more like a data fingerprint than a strict column-name check.

The package also includes lazy-loaded `customers_old` and `customers_new`
example data sets.

## Find the closest ancestor

```r
library <- list(
  customers_2024 = data_dna(customers_old),
  customers_2025 = data_dna(customers_new)
)

match <- dna_match(customers_new, library)
match

dna_match_plot(match, file = "lineage.png")
```

`dna_match_plot()` is now the recommended reporting output. It renders a static
PNG/PDF lineage figure with base R graphics: white background, compact ranking
table, and restrained similarity lines that fit technical reports, papers, and
slide decks better than a web page.

## Core API

```r
data_dna(df)
dna_card(df)
dna_compare(old_df, new_df)
dna_diff(old_df, new_df)
dna_match(new_df, dna_library)
dna_match_card(match)
dna_match_plot(match)
dna_species(df)
```

## Installation

From GitHub:

```r
install.packages("devtools")
devtools::install_github("TonyIsFool/DataDNA")
```

Or with the lighter `remotes` package:

```r
install.packages("remotes")
remotes::install_github("TonyIsFool/DataDNA")
```

From a local source tarball:

```r
install.packages("DataDNA_0.1.0.tar.gz", repos = NULL, type = "source")
```
