# Build the Sentinel-2 / Esri land-cover GeoTIFF URL for a tile and year

Returns the public Cloud-Optimized GeoTIFF URL of one MGRS grid-zone
tile of the Esri / Impact Observatory 10 m Annual Land Use Land Cover
product. The global mosaic is tiled by MGRS grid-zone designator (e.g.
`"47P"`); an area of interest usually intersects one or a few tiles,
which the local backend resolves automatically (see
[`s2_raster_local`](https://lucasbarreirageo.github.io/mappingAS/reference/s2_raster_local.md)).

## Usage

``` r
s2_source_url(tile, year = 2023, base_url = NULL)
```

## Arguments

- tile:

  MGRS grid-zone designator (UTM zone number + latitude band letter),
  e.g. `"47P"`. The public bucket zero-pads the zone to two digits (e.g.
  `"01C"`).

- year:

  Integer year (2017-2023).

- base_url:

  Base URL of the public bucket holding the per-tile COGs. Defaults to
  the public AWS Open Data bucket `io-10m-annual-lulc`; kept overridable
  so a mirror or an updated layout can be used without changing the
  package.

## Value

A length-1 character URL of the form `<base>/<tile>_<year>.tif`.

## See also

[`s2_raster_local`](https://lucasbarreirageo.github.io/mappingAS/reference/s2_raster_local.md),
[`esri_legend`](https://lucasbarreirageo.github.io/mappingAS/reference/esri_legend.md)

## Examples

``` r
s2_source_url("47P", 2023)
#> [1] "https://io-10m-annual-lulc.s3.us-west-2.amazonaws.com/47P_2023.tif"
```
