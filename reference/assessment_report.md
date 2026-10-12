# Narrative conservation-assessment report for one species

Builds a written, interpretive summary of a species' preliminary
Criterion B screening from an
[`assess_species`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md)
result. Beyond restating the headline numbers, the text *interprets*
them: the EOO/AOO composition divergence, the effectiveness (not just
the extent) of protected-area overlap, and an honest framing of
continuing decline (subcriterion b). When the land-cover and/or fire
time series are supplied it also analyses their temporal trend. Habitat
conversion, fire and protected-area passages appear only when those
modules were run. It can be returned as HTML (on-screen preview), plain
text, or a Word `.docx` file (via officer) - the format used by the
Shiny app's Report tab.

## Usage

``` r
assessment_report(
  assessment,
  species = NULL,
  lang = c("en", "pt"),
  output = c("html", "text", "docx"),
  file = NULL,
  cover_series = NULL,
  fire_series = NULL,
  applied_category = NULL,
  applied_code = NULL,
  population = NULL,
  figures = FALSE
)
```

## Arguments

- assessment:

  A `geoconv_assessment` from
  [`assess_species`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md).

- species:

  Species name (default: the first assessed).

- lang:

  Report language: `"en"` (default) or `"pt"`.

- output:

  One of `"html"` (default), `"text"` or `"docx"`.

- file:

  Target path for `output = "docx"` (required in that case).

- cover_series:

  Optional land-cover time series for this species (as from
  [`timeseries_for_species`](https://lucasbarreirageo.github.io/mappingAS/reference/timeseries_for_species.md)
  /
  [`cover_timeseries`](https://lucasbarreirageo.github.io/mappingAS/reference/cover_timeseries.md),
  with `year`, `pct` and `group` columns). When supplied, a temporal
  trend of habitat conversion is added.

- fire_series:

  Optional burned-area time series (as from
  [`fire_timeseries_for_species`](https://lucasbarreirageo.github.io/mappingAS/reference/fire_timeseries_for_species.md)
  /
  [`fire_timeseries`](https://lucasbarreirageo.github.io/mappingAS/reference/fire_timeseries.md),
  with `year` and `burned_pct`). When supplied, the fire regime is
  characterised over time.

- applied_category, applied_code:

  Optional applied Criterion B category (e.g. `"VU"`) and full code
  (e.g. `"VU B1ab(iii)"`) reflecting the user's sub-criteria choices.
  When supplied, an "Applied Criterion B category" section is added;
  otherwise the assessment's `category_B` / `criterion_B_code` columns
  are used if present.

- population:

  Optional named list carrying an inferred population size to report:
  `pop_low` / `pop_high` (mature individuals, the estimate and its upper
  end), `dens_low` / `dens_high` (the density in mature individuals per
  km\\^2\\ used) and `occupancy` (percent of habitat occupied). When
  supplied and `pop_low` is finite, a "Population size (inferred)"
  section is added, framed as an inferred, screening-level figure.

- figures:

  Logical; for `output = "docx"` only, embed the supporting figures
  (composition, protection and the land-cover/fire time-series charts)
  in the Word document (default `FALSE`). Ignored for other outputs.

## Value

For `"html"`/`"text"`, a length-one character string. For `"docx"`, the
`file` path (written as a side effect), invisibly.

## Examples

``` r
occ <- read_occurrences(system.file("extdata", "example_occurrences.csv",
                                    package = "mappingAS"))
res <- assess_species(occ, mapbiomas = FALSE, verbose = FALSE)
cat(assessment_report(res, output = "text"))
#> Preliminary Conservation Assessment
#> IUCN Red List Criterion B screening (provisional) - generated on 2026-10-12 with the mappingAS R package.
#> Species: Prepusa connata Gardner
#> 
#> OVERVIEW AND EXECUTIVE STATUS
#> This document reports a preliminary, size-based screening of Prepusa connata Gardner against Criterion B of the IUCN Red List Categories and Criteria. It is intended for screening only and does not constitute a formal Red List assessment.
#> It is based on 5 occurrence records (5 with unique coordinates). The combined provisional category, from range size alone, is EN (B1 size).
#> The category is driven not by range size alone but by its combination with a spatially concentrated pattern of occupation and active threat vectors (below). The most defensible full-criterion formulation would pair this size-based category with subconditions (a) few locations and (b) continuing decline in habitat quality, contingent on the validation set out in the recommendations.
#> 
#> GEOGRAPHIC RANGE METRICS
#> The Extent of Occurrence (EOO), estimated as the area of the minimum convex polygon enclosing all occurrence points, is 2,461.73 km2. The Area of Occupancy (AOO), the number of occupied 2x2 km cells scaled to the IUCN 2x2 km reference, is 16.00 km2 (4 occupied cells).
#> Both areas were measured on a data-centred Lambert Azimuthal Equal-Area projection, in line with the IUCN guidelines.
#> As additional Criterion B evidence, the number of subpopulations is estimated at 4 (circular-buffer method) and the number of locations at 4 (occupied 10x10 km grid cells). These estimates use the same spatial rationale as the ConR package and are preliminary screening aids, not a formal count of locations under a specific threat.
#> 
#> PROVISIONAL CRITERION B CATEGORY
#> Under the size thresholds of Criterion B, the EOO corresponds to EN (B1 size) (sub-criterion B1) and the AOO to EN (B2 size) (sub-criterion B2). The combined provisional category is EN (B1 size).
#> Thresholds are CR / EN / VU for EOO < 100 / 5,000 / 20,000 km2 (B1) and AOO < 10 / 500 / 2,000 km2 (B2). These reflect range size only and, by themselves, do not qualify a taxon for listing: a formal Criterion B assessment additionally requires at least two of the subconditions (severe fragmentation or few locations, continuing decline, and extreme fluctuation).
#> Both subcriteria converge on the same category, which strengthens the provisional attribution and makes it robust to moderate changes in either metric.
#> 
#> APPLIED CRITERION B CATEGORY
#> Combining the size thresholds with the sub-criteria set for this assessment, the applied Criterion B category is EN (EN B1ab+2ab). The sub-criteria considered met are: (a) severe fragmentation / few locations, (b) continuing decline (assumed). The number of locations caps the category under sub-criterion (a).
#> A continuing decline (sub-criterion b) was assumed rather than documented, as recommended for range-restricted taxa in converting landscapes; it should be confirmed against the trend of the specific habitat. As with the provisional category, this is a screening aid rather than a formal Red List assessment.
#> 
#> RECOMMENDATIONS FOR FORMAL ASSESSMENT
#> Delimit the number of locations and assess severe fragmentation (subcondition a), defining 'location' by the most plausible threat (e.g. a fire season or climatic extreme per massif).
#> Validate and refine the occurrences: confirm peripheral records, which drive the EOO, and expand sampling in undersampled terrain, which may bias the AOO downward.
#> Obtain population size, structure and trend (supports subcriterion b(v) or Criteria C/D), currently absent from this screening.
#> Submit the taxon to a formal assessment (CNCFlora / IUCN) and integrate this screening into protected-area management (fire and visitor management) where relevant.
#> 
#> CAVEATS AND LIMITATIONS
#> The provisional category reflects range-size thresholds only and must not be reported as a final IUCN category.
#> The AOO is sensitive to grid origin and to sampling effort; the EOO can be inflated by outlying or erroneous records.
#> 
#> REFERENCES
#> - IUCN Standards and Petitions Committee (2024). Guidelines for Using the IUCN Red List Categories and Criteria, Version 16. Prepared by the Standards and Petitions Committee. https://www.iucnredlist.org/documents/RedListGuidelines.pdf
#> - IUCN (2012). IUCN Red List Categories and Criteria: Version 3.1, Second edition. IUCN, Gland, Switzerland and Cambridge, UK.
# \donttest{
# Word (.docx) report written to a temporary file:
f <- file.path(tempdir(), "report.docx")
assessment_report(res, output = "docx", file = f)
unlink(f)
# }
```
