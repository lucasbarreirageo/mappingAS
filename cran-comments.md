## Submission

This is an update of mappingAS, from 1.14.0 to 1.15.0.

## Changes in this version

* Criterion B helpers now raise the EOO to the AOO when it is smaller (IUCN
  Guidelines sect. 4.10) and write the code in the official notation
  (e.g. `EN B1ab(iii)+2ab(iii)`).
* A continuing decline in habitat (b(iii)) is inferred only from a significant
  recent decline of natural habitat (new `decline_years` argument and
  `habitat_trend_pct_yr` / `habitat_trend_p` columns), instead of from any
  converted area in the range.
* Nested or overlapping protected areas count as one location.
* AOO and number of locations are reproducible (deterministic grid offsets).
* `calc_aoh()` reads land cover within its pixel budget with unbiased sampling,
  and resolves `year = NULL` to the latest available year.
* Ranges crossing the antimeridian are handled correctly.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE

  Possibly misspelled words in DESCRIPTION: AOO, EOO, IUCN, WDPA, Amazonia,
  RAISG, anthropic. These are standard acronyms and technical terms in
  conservation biology / remote sensing, spelled intentionally.

(A local `R CMD check` additionally reports the "unable to verify current
time" NOTE, which is specific to the build sandbox and does not appear on
win-builder.)

## URLs

* checking URLs, R CMD check / urlchecker may report a 403 for the DOI
  <https://doi.org/10.1002/ece3.3704> (Dauby et al. 2017, cited in the README).
  The DOI is valid and resolves correctly in a web browser; the Wiley host
  simply returns HTTP 403 to non-interactive/automated requests. The link is
  intentional and correct.

## Test environments

* local: Windows 11, R 4.5.0 (`R CMD check --as-cran`)
* win-builder: R-devel (x86_64-w64-mingw32)
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1), windows-latest,
  macos-latest

## Internet access

Some functions read public MapBiomas Cloud-Optimized GeoTIFFs and the WDPA
service over the network. Examples that require this are wrapped in
`\donttest{}`. The test suite uses only bundled local fixtures, and the
vignette is not evaluated (`eval = FALSE`), so neither the tests nor the
vignette access the internet.
