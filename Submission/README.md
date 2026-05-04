# DataDNA Submission Folder

This folder is a local release handoff area for GitHub and CRAN preparation.
It is not included in the CRAN source package because `Submission` is excluded
in `.Rbuildignore`.

## Contents

- `CRAN/DataDNA_0.1.0.tar.gz`: privacy-safe source tarball for CRAN checks.
- `CRAN/cran-comments.md`: CRAN-facing check notes.
- `CRAN/check-summary.md`: sanitized local check summary without machine paths.
- `GitHub/release-notes.md`: short release notes for a GitHub release.
- `sample-data/`: only two small synthetic data files for validation.

## Data Policy

Do not place commercial, client, production, or internal data in this folder.
Only one or two synthetic samples should be included for testing. For this
release, the selected samples are:

- `customers_old.rda`
- `customers_new.rda`

These are sufficient to test package examples, comparisons, lineage matching,
and plotting without exposing proprietary data.

## Privacy Policy

Do not include raw check logs, local tool folders, local paths, workspace names,
or generated reports from private work. The final CRAN tarball was scanned for
local path and user-name patterns before being copied here.
