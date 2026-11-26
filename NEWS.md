# DataDNA 0.2.0

- Removed external runtime package dependencies (`htmltools`); HTML cards and SVG reports now render using a built-in zero-dependency HTML5 builder with IDE viewer support.
- Fixed crashes when profiling 0-row data frames, 0-column data frames, and all-`NA` columns.
- Preserved and restored caller `.Random.seed` state in `dna_example_customers()`.
- Refined role inference so low-cardinality `*_code`/`*_key` columns are treated as categories rather than IDs, while high-uniqueness keys and sequential integer surrogate keys are detected as IDs.
- Added automatic column rename inference (`detect_renamed_columns()`) in `dna_compare()` and `dna_diff()`.
- Added temporal window and sampling-regularity drift signals (`temporal` similarity and `dna_diff()` temporal mutations), role transition reporting, and adaptive spread scaling for small-magnitude numeric columns (such as `[0, 1]` rates).
- Added `dna_library()` to build, save, and reload compact fingerprint libraries from lists or directories of `.csv`, `.tsv`, `.rds`, and `.rda` files without keeping raw data frames in memory.
- Added static paper-style figures `dna_plot()` and `dna_diff_plot()`, added `.svg` export across all static plot functions, and improved `dna_match_plot()` layout scaling when displaying more than 5 candidates.
- Added standard S3 methods: `summary.data_dna()`, `plot.*`, and `as.data.frame.*` for all core DataDNA classes.

# DataDNA 0.1.0

- Initial release.
- Added data DNA profiling for R data frames.
- Added laboratory-style HTML DNA cards.
- Added data set comparison and mutation diffing.
- Added example customer data generator.
- Added data lineage matching with `dna_match()` and HTML/SVG match cards.
- Added paper-style non-HTML lineage figures with `dna_match_plot()`.
