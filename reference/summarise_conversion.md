# Summarise habitat conversion from MapBiomas class areas

Aggregates a table of per-class areas into conservation groups and
computes the percentage of converted (anthropic) versus remaining
natural habitat.

## Usage

``` r
summarise_conversion(
  class_areas,
  collection = 10,
  initiative = "brazil",
  water_in_denominator = FALSE
)
```

## Arguments

- class_areas:

  Either a `data.frame` with columns `code` and `area_km2` (e.g. from
  [`mb_class_areas_gee`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_class_areas_gee.md))
  or a terra `SpatRaster` of MapBiomas codes (which is tabulated via
  [`mb_class_areas_raster`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_class_areas_raster.md)).

- collection:

  Integer collection number (default `10`).

- initiative:

  One of `"brazil"` (default), `"amazonia"` or `"colombia"`; selects the
  standardised legend context (see
  [`mb_legend`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_legend.md)).

- water_in_denominator:

  Logical; if `TRUE`, water is treated as natural and included in the
  terrestrial denominator (default `FALSE`).

## Value

A list with: `converted_pct`, `natural_pct` (terrestrial, the headline
numbers), `converted_pct_total`, `natural_pct_total`, the group areas
(`natural_km2`, `anthropic_km2`, `water_km2`, `other_km2`,
`not_observed_km2`, `total_km2`) and `by_class` (the per-class table
joined to the legend).

## Details

The headline metrics use a *terrestrial* denominator (natural +
anthropic), excluding water, not-observed and ambiguous "other" classes,
which is the most defensible measure of habitat conversion. A total-area
version (denominator = everything except not-observed) is also returned
for transparency.

## Examples

``` r
ca <- data.frame(code = c(3, 15, 24, 33), area_km2 = c(60, 30, 10, 5))
summarise_conversion(ca)$converted_pct
#> [1] 40
```
