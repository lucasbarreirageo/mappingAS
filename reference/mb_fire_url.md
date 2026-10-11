# Build a MapBiomas Fire (Fogo) GeoTIFF URL

`"frequency"` is the fire-frequency layer (pixel = number of years
burned over `period`, 1-40); `"accumulated"` is binary (burned at least
once); `"annual"` is the per-year burned area.

## Usage

``` r
mb_fire_url(
  year = 2024,
  product = c("accumulated", "annual", "frequency"),
  fire_collection = 4,
  host_collection = 9,
  period = c(1985, 2024)
)
```

## Arguments

- year:

  Integer year (ignored when `product = "accumulated"`).

- product:

  `"accumulated"` (default) or `"annual"`.

- fire_collection:

  Fire collection number (default `4`).

- host_collection:

  Initiative folder hosting the fire data (default `9`; Fire Col.4 lives
  under `collection_9/`).

- period:

  Two-integer accumulated span (default `c(1985, 2024)`).

## Value

A length-1 character URL.

## Examples

``` r
mb_fire_url(product = "accumulated")
#> [1] "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/fire-col4/accumulated_burned_v1/mapbiomas_fire_col4_br_accumulated_burned_1985_2024.tif"
mb_fire_url(2024, "annual")
#> [1] "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/fire-col4/annual_burned/mapbiomas_fire_col4_br_annual_burned_2024.tif"
```
