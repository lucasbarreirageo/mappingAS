## Submission

This is an update of mappingAS, from 1.13.2 to 1.14.0.

## Changes in this version

* Assimilated three full-criteria supporting analyses (kept focused on
  Criterion B, and computed entirely from the data the package already
  produces), each as an exported function and a Shiny tab: countries and
  biogeographical realm of occurrence (`countries_of_occurrence()`), the
  lower-upper AOO bounds and their B2 category range (`aoo_bounds()`,
  `iucn_category_B_range()`), and severe-fragmentation screening
  (`assess_fragmentation()`, `fragment_habitat()`).
* Added an elevation-refined Area of Habitat: `elevation_preferences()` and
  `calc_aoh()` read a digital elevation model only over the range extent
  (windowed; the read is now bounded by area so it cannot exhaust memory), via
  the optional `elevatr` suggestion or a user-supplied DEM. `map_aoh()` maps the
  result.
* An inferred population size (Area of Habitat times a density) is shown on the
  Assessment tab and, via a new optional `population` argument to
  `assessment_report()`, in the report.
* Shiny app fixes: the interactive-map HTML download is now genuinely
  self-contained even when pandoc is absent; the "Add points" map places and
  drags points correctly; and the land-cover time-series step has a five-year
  minimum.

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
