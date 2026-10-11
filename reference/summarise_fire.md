# Summarise fire pressure inside a range

Fire analogue of
[`summarise_conversion`](https://lucasbarreirageo.github.io/mappingAS/reference/summarise_conversion.md).
Pass the known range area (`range_km2`, e.g. `eoo$area_km2`) as
denominator so the percentage is exact and robust to the raster's NoData
handling.

## Usage

``` r
summarise_fire(fire_areas, range_km2 = NULL)
```

## Arguments

- fire_areas:

  A `data.frame` from
  [`fire_areas`](https://lucasbarreirageo.github.io/mappingAS/reference/fire_areas.md).

- range_km2:

  Total range area (km^2). If `NULL`, the sum of all tabulated cells is
  used (valid only when unburned pixels are coded 0).

## Value

A list with `burned_km2`, `total_km2`, `burned_pct`, `freq_mean`,
`freq_max` and `by_freq`.
