# Extent of Occurrence (EOO) via minimum convex polygon

Computes the EOO as the area of the convex hull (minimum convex polygon)
enclosing all occurrence points, following the IUCN Red List guidelines
and the approach used by the ConR package. The hull is built in
longitude/latitude, its edges are densified with `st_segmentize()` so
that they follow great circles, and the area is measured directly on the
WGS84 ellipsoid (a "spheroid" measurement).

## Usage

``` r
calc_eoo(points, segmentize_km = 20)
```

## Arguments

- points:

  An `sf` of POINT geometries (one species). Use
  [`read_occurrences`](https://lucasbarreirageo.github.io/mappingAS/reference/read_occurrences.md)
  to produce it.

- segmentize_km:

  Numeric edge length, in kilometres, used to densify the hull edges
  along great circles (default `20`, matching ConR).

## Value

A list with `area_km2` (numeric), `n_records` (integer), `n_unique`
(distinct coordinates), `hull` (the hull as an `sfc` in WGS84, or `NULL`
when the EOO is undefined) and `crs_laea` (a data-centred equal-area
proj string, kept for mapping).

## Details

Degenerate cases are handled so the function never errors:

- With fewer than three unique points a polygon cannot be built and the
  EOO is `NA`.

- When the unique points are perfectly collinear (they would form a line
  of area zero) a small amount of noise is added to the coordinates
  (`jitter`), exactly as in ConR, so that a valid polygon can still be
  produced.

- If, for any other reason, a valid polygon still cannot be built, the
  EOO is returned as `NA` rather than throwing an error.

## Examples

``` r
f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
occ <- read_occurrences(f)
sp1 <- occ[occ$species == occ$species[1], ]
calc_eoo(sp1)$area_km2
#> [1] 2468.019
```
