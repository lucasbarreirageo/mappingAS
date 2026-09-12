## Submission

This is a resubmission of mappingAS (version 1.13.2).

## Response to the previous CRAN review

* Reset of the user's graphical parameters. The Shiny app
  (`inst/shiny/app.R`) changed `par()` inside a download handler without
  restoring it. It now saves and restores the state with
  `oldpar <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(oldpar))`.
  We re-checked all examples, vignettes and app code: every `par()` / `setwd()`
  change is now paired with an `on.exit()` restore, and no example, vignette or
  demo changes `options()` without restoring it.

## Other changes in this version

* Fixed a rendering bug where a stacked bar summing to exactly 100% could lose
  its top segment in downloaded PNGs (clipping now uses `coord_cartesian()`).
* The report and factsheet now honour the applied IUCN Criterion B category and
  render support figures with a high-quality graphics device.

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
