# MapBiomas land-cover composition over time for a polygon

For a given area (e.g. an EOO hull or the union of AOO cells), computes
the area of each MapBiomas class (or conservation group) for several
years, returning a tidy long table suitable for a stacked-area chart of
percentage of area versus year (as in the MapBiomas coverage figures).

## Usage

``` r
cover_timeseries(
  geom,
  years = NULL,
  collection = NULL,
  initiative = "brazil",
  backend = c("local", "gee"),
  src = NULL,
  by = c("class", "group"),
  include_not_observed = FALSE,
  verbose = TRUE,
  max_pixels = 5e+07
)
```

## Arguments

- geom:

  An `sf`/`sfc` polygon (any CRS).

- years:

  Integer vector of years. If `NULL` (default) a 5-year step across the
  initiative's span (plus the last year) is used.

- collection:

  Integer MapBiomas collection. `NULL` uses the initiative default.

- initiative:

  One of `"brazil"` (default), `"amazonia"` or `"colombia"` (see
  [`mb_initiatives`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_initiatives.md)).

- backend:

  `"local"` (default, windowed GeoTIFF read) or `"gee"` (Earth Engine;
  Brazil only).

- src:

  Optional GeoTIFF path/URL for the local backend (overrides the public
  per-year URL).

- by:

  `"class"` (default; one series per MapBiomas class, with the official
  colours) or `"group"` (natural / anthropic / water / other).

- include_not_observed:

  Logical; keep the "Not Observed" class (default `FALSE`).

- verbose:

  Logical; print progress (default `TRUE`).

- max_pixels:

  Pixel budget for each year's local area read (default `5e7`). Large or
  widely-spread ranges are read per polygon part and/or decimated with
  majority resampling so the read stays bounded; class proportions are
  preserved. Ignored by the GEE backend.

## Value

A `data.frame` (long) with columns `year`, `label`, `hex`, `group`,
`area_km2` and `pct` (percentage of the mapped area in that year). For
`by = "class"` it also has `code` and `class_en`.

## Details

Each year is read independently (local windowed read or Earth Engine),
so a long span of annual data can take a while. By default a 5-year step
is used; pass `years = mb_years()` for the full annual series.

## See also

[`plot_timeseries`](https://lucasbarreirageo.github.io/mappingAS/reference/plot_timeseries.md),
[`timeseries_for_species`](https://lucasbarreirageo.github.io/mappingAS/reference/timeseries_for_species.md)
