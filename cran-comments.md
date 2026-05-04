## R CMD check results

This is a local pre-release check only. The package has not been submitted to
CRAN.

The package was checked locally with R 4.6.0 on Windows.

Package name checks were performed against the current CRAN package index, CRAN
Archive listing, and current Bioconductor package indexes. `DataDNA` was not
found in those indexes at the time of this local check.

Local toolchain used for the final check:

- Project-local TinyTeX was used for PDF manual generation.
- Project-local Pandoc 3.9.0.2 was used for top-level Markdown checks.
- LaTeX packages added to the local TinyTeX installation: `courier`,
  `makeindex`.
- The TinyTeX and Pandoc executable directories were added to `PATH` only for
  local check commands, not as global system PATH changes.

Privacy check:

- The source tarball was built with `R CMD build --user=DataDNA Package1` so
  the generated `Packaged:` field does not include the local Windows user name.
- The source tarball was scanned for local user directories, local user names,
  workspace folder names, local tool directories, and placeholder emails; no
  such local privacy patterns were found.
- `cran-comments.md`, local check directories, local toolchains, and source
  tarballs are excluded from package builds via `.Rbuildignore`.

Maintainer:

- `DESCRIPTION` uses the reachable maintainer email `xulunt123@gmail.com`.

Latest local commands:

```sh
R CMD build --user=DataDNA Package1
R CMD check DataDNA_0.1.0.tar.gz --as-cran --no-manual
R CMD check DataDNA_0.1.0.tar.gz --as-cran
```

Latest local result:

- `R CMD build --user=DataDNA Package1` completed successfully and produced
  `DataDNA_0.1.0.tar.gz`.
- `R CMD check DataDNA_0.1.0.tar.gz --as-cran --no-manual` completed with
  0 errors, 0 warnings, and 1 note.
- `R CMD check DataDNA_0.1.0.tar.gz --as-cran` completed with
  0 errors, 0 warnings, and 1 note.

Expected note:

- New submission.

Recommended external checks before submission:

- win-builder with current R-devel.
- R-hub or equivalent cross-platform checks.
