# Build a data-centred equal-area CRS (Lambert Azimuthal Equal Area)

EOO and AOO must be measured on an equal-area projection. This helper
returns a LAEA proj string centred on the centroid of `geom`, which
minimises areal distortion for the spatial extent of a single species.

## Usage

``` r
laea_crs(geom)
```

## Arguments

- geom:

  An `sf` or `sfc` object (any CRS; will be treated as WGS84
  longitude/latitude for centroid computation).

## Value

A character proj4 string suitable for
[`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html).
