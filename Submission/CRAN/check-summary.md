# DataDNA CRAN Check Summary

Package: DataDNA
Version: 0.1.0
Maintainer: Tony Lu <xulunt123@gmail.com>

## Commands

```sh
R CMD build --user=DataDNA Package1
R CMD check DataDNA_0.1.0.tar.gz --as-cran
```

## Result

```text
0 ERROR
0 WARNING
1 NOTE
```

The remaining NOTE is expected for a first CRAN submission:

```text
New submission
```

## Manual Checks

- PDF manual: OK
- HTML manual: OK
- Examples: OK
- Tests: OK
- Package load/unload: OK
- Namespace: OK
- Data files: OK

## Privacy Check

The source tarball was scanned for local user directories, local user names,
workspace folder names, local tool directories, and old placeholder emails.

No private path, local user-name, local toolchain, workspace-name, or
placeholder-email hits were found in the tarball.
