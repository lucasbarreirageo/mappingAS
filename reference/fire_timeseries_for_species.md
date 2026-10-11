# Burned-area time series for one assessed species (EOO or AOO)

Convenience wrapper around
[`fire_timeseries`](https://lucasbarreirageo.github.io/mappingAS/reference/fire_timeseries.md)
that pulls the stored EOO hull or AOO geometry (and its area) from an
assessment.

## Usage

``` r
fire_timeseries_for_species(
  assessment,
  species = NULL,
  range = c("eoo", "aoo"),
  years = NULL,
  verbose = TRUE
)
```

## Arguments

- assessment:

  A `geoconv_assessment` from
  [`assess_species`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md).

- species:

  Species name (default: first assessed).

- range:

  `"eoo"` (default) or `"aoo"`.

- years, verbose:

  Passed to
  [`fire_timeseries`](https://lucasbarreirageo.github.io/mappingAS/reference/fire_timeseries.md).

## Value

The data frame from
[`fire_timeseries`](https://lucasbarreirageo.github.io/mappingAS/reference/fire_timeseries.md)
with attributes `species` and `range`.
