# Burned-area time series for a polygon (% of range x year)

Fire analogue of
[`cover_timeseries`](https://lucasbarreirageo.github.io/mappingAS/reference/cover_timeseries.md):
reads the annual burned layer for each year and returns burned area
(km^2) and percentage of the range. Each year is one windowed read, so a
full annual series can be slow.

## Usage

``` r
fire_timeseries(
  geom,
  years = NULL,
  fire_collection = 4,
  host_collection = 9,
  range_km2 = NULL,
  verbose = TRUE
)
```

## Arguments

- geom:

  An `sf`/`sfc` polygon.

- years:

  Integer vector (default
  [`mb_years()`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_years.md)).

- fire_collection:

  Fire collection number (default `4`).

- host_collection:

  Initiative folder hosting the fire data (default `9`; Fire Col.4 lives
  under `collection_9/`).

- range_km2:

  Range area for the percentage denominator (optional).

- verbose:

  Logical; print progress.

## Value

A `data.frame` with `year`, `burned_km2`, `burned_pct`.
