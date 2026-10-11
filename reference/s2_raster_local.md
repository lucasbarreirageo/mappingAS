# Crop a Sentinel-2 / Esri LULC mosaic to an area of interest

Global analogue of
[`mb_raster_local`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_raster_local.md)
for the Esri / Impact Observatory 10 m Annual Land Use Land Cover
product. Determines the MGRS grid-zone tile(s) intersecting `aoi`,
streams a windowed read of each via GDAL's `/vsicurl/` driver (no Google
Earth Engine, no account), masks to the polygon and, when the AOI spans
more than one tile, merges the pieces (reprojected to geographic
coordinates for display). For accurate area statistics the assessment
path tabulates each tile in its native projection and sums, rather than
reprojecting; use
[`summarise_conversion`](https://lucasbarreirageo.github.io/mappingAS/reference/summarise_conversion.md)
on the class areas for that.

## Usage

``` r
s2_raster_local(
  aoi,
  year = 2023,
  src = NULL,
  base_url = NULL,
  mask = TRUE,
  cache = TRUE,
  cache_dir = NULL
)
```

## Arguments

- aoi:

  An `sf`/`sfc` polygon (any CRS), e.g. an EOO hull or the union of AOO
  cells.

- year:

  Integer year (2017-2023, default `2023`).

- src:

  Optional path/URL to a single Esri LULC GeoTIFF (ends in `.tif`, used
  as-is), or a base-URL prefix that replaces the default bucket. If
  `NULL` (default) the public bucket is used through `/vsicurl/`.

- base_url:

  Base URL of the public bucket (see
  [`s2_source_url`](https://lucasbarreirageo.github.io/mappingAS/reference/s2_source_url.md)).

- mask:

  Logical; if `TRUE` (default) pixels outside the polygon are set to
  `NA`.

- cache:

  Logical; cache the windowed crops on disk (default `TRUE`).

- cache_dir:

  Directory for the crop cache (default under
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html)).

## Value

A terra `SpatRaster` of Esri land-cover codes for the AOI.

## See also

[`esri_legend`](https://lucasbarreirageo.github.io/mappingAS/reference/esri_legend.md),
[`s2_source_url`](https://lucasbarreirageo.github.io/mappingAS/reference/s2_source_url.md),
[`mb_raster_local`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_raster_local.md)
