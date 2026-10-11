# Area (km^2) per fire value from a cropped fire raster

Fire analogue of
[`mb_class_areas_raster`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_class_areas_raster.md).
For the accumulated product, `freq` is the fire frequency (years
burned).

## Usage

``` r
fire_areas(r)
```

## Arguments

- r:

  A terra `SpatRaster` from
  [`fire_raster_local`](https://lucasbarreirageo.github.io/mappingAS/reference/fire_raster_local.md).

## Value

A `data.frame` with columns `freq` and `area_km2`.
