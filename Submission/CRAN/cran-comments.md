## R CMD check results

This is a local pre-release check for DataDNA 0.2.0. The package has not yet
been submitted to CRAN.

The package was checked locally with R 4.6.0 on Windows.

Runtime dependencies:

- Pure base R (`>= 4.1.0`), importing only base/recommended packages
  (`grDevices`, `grid`, `stats`, `tools`, `utils`) with zero external CRAN
  package dependencies.

Privacy check:

- The source tarball was built with `R CMD build --user=DataDNA` so the
  generated `Packaged:` field does not include the local Windows user name.
- `cran-comments.md`, `Submission/`, `.github/`, local check directories, and
  source tarballs are excluded from package builds via `.Rbuildignore`.

Maintainer:

- `DESCRIPTION` uses the reachable maintainer email `xulunt123@gmail.com`.

Latest local commands:

```sh
R CMD build --user=DataDNA DataDNA
R CMD check DataDNA_0.2.0.tar.gz --as-cran --no-manual
```

Latest local result:

- `0 errors | 0 warnings` (new submission).
