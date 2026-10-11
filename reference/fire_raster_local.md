# Crop a MapBiomas Fire raster to an area of interest (local backend)

Thin wrapper over
[`mb_raster_local`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_raster_local.md);
reuses its windowed `/vsicurl/` read and on-disk cache.

## Usage

``` r
fire_raster_local(
  aoi,
  year = 2024,
  product = c("accumulated", "annual", "frequency"),
  fire_collection = 4,
  host_collection = 9,
  src = NULL,
  mask = TRUE,
  cache = TRUE,
  cache_dir = NULL
)
```

## Arguments

- aoi:

  An `sf`/`sfc` polygon (e.g. an EOO hull or AOO union).

- year:

  Integer year (ignored when `product = "accumulated"`).

- product:

  `"accumulated"` (default) or `"annual"`.

- fire_collection:

  Fire collection number (default `4`).

- host_collection:

  Initiative folder hosting the fire data (default `9`; Fire Col.4 lives
  under `collection_9/`).

- src:

  Optional GeoTIFF path/URL overriding the built fire URL.

- mask, cache, cache_dir:

  Passed to
  [`mb_raster_local`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_raster_local.md).

## Value

A terra `SpatRaster` of fire values clipped to the AOI.
