# Land-cover time series for one assessed species (EOO or AOO)

Convenience wrapper that pulls the stored EOO hull or AOO geometry from
an assessment and runs
[`cover_timeseries`](https://lucasbarreirageo.github.io/mappingAS/reference/cover_timeseries.md)
on it, reusing the assessment's collection and backend by default.

## Usage

``` r
timeseries_for_species(
  assessment,
  species = NULL,
  range = c("eoo", "aoo"),
  years = NULL,
  by = c("class", "group"),
  src = NULL,
  verbose = TRUE,
  max_pixels = 8e+06
)
```

## Arguments

- assessment:

  A `geoconv_assessment` from
  [`assess_species`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md).

- species:

  Species name (default: first assessed species).

- range:

  `"eoo"` (default) or `"aoo"`.

- years, by, src, verbose:

  Passed to
  [`cover_timeseries`](https://lucasbarreirageo.github.io/mappingAS/reference/cover_timeseries.md).

- max_pixels:

  Pixel budget per year, passed to
  [`cover_timeseries`](https://lucasbarreirageo.github.io/mappingAS/reference/cover_timeseries.md).
  Defaults to `8e6` (~8 million) - much lower than the single-snapshot
  assessment, because a composition *trend* is a ratio and needs far
  less resolution, and every year is a separate read: the coarser budget
  makes a multi-year series over a large range several times faster with
  no meaningful change to the percentages.

## Value

The long data frame from
[`cover_timeseries`](https://lucasbarreirageo.github.io/mappingAS/reference/cover_timeseries.md),
with attributes `species` and `range` set.
