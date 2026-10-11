# Tabulate area per MapBiomas class from a cropped raster

Computes the true (geodesic, latitude-corrected) area of each MapBiomas
pixel class inside a cropped/masked raster. Works for rasters in
geographic CRS, where pixel area varies with latitude, by using
[`terra::cellSize()`](https://rspatial.github.io/terra/reference/cellSize.html).

## Usage

``` r
mb_class_areas_raster(r)
```

## Arguments

- r:

  A terra `SpatRaster` of MapBiomas codes (e.g. from
  [`mb_raster_local`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_raster_local.md)).

## Value

A `data.frame` with columns `code` and `area_km2`.
