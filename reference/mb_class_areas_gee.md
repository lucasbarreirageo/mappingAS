# MapBiomas class areas via Google Earth Engine (optional backend)

Computes the area (km^2) of each MapBiomas pixel class inside `aoi`
entirely on Google Earth Engine, without downloading any raster. This is
the recommended backend for very large ranges (e.g. continental EOOs)
where a local windowed read would transfer too much data.

## Usage

``` r
mb_class_areas_gee(
  aoi,
  year = 2024,
  collection = 10,
  version = "v1",
  asset = "projects/mapbiomas-public/assets/brazil/lulc/v1",
  scale = 30,
  max_pixels = 1e+13
)
```

## Arguments

- aoi:

  An `sf`/`sfc` polygon (any CRS).

- year:

  Integer year (default `2024`).

- collection:

  Integer MapBiomas collection id (default `10`).

- version:

  Asset version string for the public LULC collection (default `"v1"`;
  some mirrors expose `"v2"` for Collection 10.1).

- asset:

  Earth Engine `ImageCollection` id (default
  `"projects/mapbiomas-public/assets/brazil/lulc/v1"`).

- scale:

  Nominal pixel scale in metres for the reduction (default `30`).

- max_pixels:

  Maximum pixels for the server-side reduction (default `1e13`).

## Value

A `data.frame` with columns `code` and `area_km2`.

## Details

Requires the rgee package and an initialised Earth Engine session
([`rgee::ee_Initialize()`](https://r-spatial.github.io/rgee/reference/ee_Initialize.html));
see the package README for one-time setup.
