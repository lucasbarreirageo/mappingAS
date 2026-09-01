## Submission

This is a new submission of mappingAS (version 1.13.1).

## Response to the previous CRAN review

* Examples no longer use `\dontrun{}`. Fast examples run directly, using the
  bundled dataset and offline computation. Examples that read public data over
  the network (MapBiomas, WDPA) use `\donttest{}`. Entry points intended for
  interactive use (`run_app()`, `mas_plotly()`) use `if (interactive()){}`.
* No function writes to the user's home or working directory by default:
  `export_ranges()` now defaults to `dir = tempdir()`, and every example that
  writes a file writes it under `tempdir()`.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE

  New submission.

  Possibly misspelled words in DESCRIPTION: AOO, EOO, IUCN, WDPA, Amazonia,
  RAISG, anthropic. These are standard acronyms and technical terms in
  conservation biology / remote sensing, spelled intentionally.

(A local `R CMD check` additionally reports the "unable to verify current
time" NOTE, which is specific to the build sandbox and does not appear on
win-builder.)

## Test environments

* local: Ubuntu 24.04, R 4.3.3 (`R CMD check --as-cran`)
* win-builder: R-devel (x86_64-w64-mingw32)
* R-hub: linux, macos, macos-arm64, windows

## Internet access

Some functions read public MapBiomas Cloud-Optimized GeoTIFFs and the WDPA
service over the network. Examples that require this are wrapped in
`\donttest{}`. The test suite uses only bundled local fixtures, and the
vignette is not evaluated (`eval = FALSE`), so neither the tests nor the
vignette access the internet.
