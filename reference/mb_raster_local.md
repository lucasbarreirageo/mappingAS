# Crop a MapBiomas LULC raster to an area of interest (local backend)

Reads the MapBiomas national mosaic and crops/masks it to `aoi` using
terra. By default it streams a *windowed* read of only the AOI's
bounding box from the public GeoTIFF via GDAL's `/vsicurl/` driver, so
there is no Google Earth Engine account and no full-country download.
For offline or repeated use, point `src` to a local GeoTIFF instead.

## Usage

``` r
mb_raster_local(
  aoi,
  year = 2024,
  collection = NULL,
  initiative = "brazil",
  src = NULL,
  mask = TRUE,
  cache = TRUE,
  cache_dir = NULL
)
```

## Arguments

- aoi:

  An `sf`/`sfc` polygon (any CRS) defining the area to extract, e.g. an
  EOO hull or the union of AOO cells.

- year:

  Integer year (default `2024`).

- collection:

  Integer collection number. `NULL` uses the `initiative` default
  (10/6/3 for Brazil/Amazonia/Colombia).

- initiative:

  One of `"brazil"` (default), `"amazonia"` or `"colombia"` (see
  [`mb_initiatives`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_initiatives.md)).

- src:

  Optional path or URL to a MapBiomas GeoTIFF. If `NULL` (default) the
  public URL for `year`/`collection`/`initiative` is used through
  `/vsicurl/`.

- mask:

  Logical; if `TRUE` (default) pixels outside the polygon are set to
  `NA`. If `FALSE` only a rectangular crop is returned.

- cache:

  Logical; if `TRUE` (default) the rectangular windowed crop is cached
  on disk (keyed by source + bounding box), so reading the same area
  again (e.g. the EOO during assessment and later when mapping, or a
  re-run) does not re-download. Masking is always applied in memory
  after the cached crop is loaded.

- cache_dir:

  Directory for the windowed-crop cache (default a `mappingAS_mb_cache`
  folder under [`tempdir()`](https://rdrr.io/r/base/tempfile.html)).

## Value

A terra `SpatRaster` of MapBiomas pixel codes restricted to the AOI, in
the raster's native CRS.

## Details

For very large ranges (e.g. continental EOOs) the windowed read can
still transfer a lot of data; in that case prefer
[`mb_class_areas_gee`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_class_areas_gee.md).
