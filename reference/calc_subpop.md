# Estimate of number of subpopulations (circular-buffer method)

Estimates the number of subpopulations by the "circular buffer" method:
a circle of radius `resol_km` is drawn around every occurrence, the
circles are dissolved (unioned), and the number of resulting disjoint
polygons is the estimated number of subpopulations. Occurrences whose
buffers overlap are treated as belonging to the same subpopulation.

## Usage

``` r
calc_subpop(points, resol_km = NULL)
```

## Arguments

- points:

  An `sf` of POINT geometries (one species). Use
  [`read_occurrences`](https://lucasbarreirageo.github.io/mappingAS/reference/read_occurrences.md)
  to produce it.

- resol_km:

  Circle radius in kilometres. `NULL` (default) uses one tenth of the
  maximum distance between two occurrences.

## Value

A list with `n_subpop` (integer estimate), `resol_km` (the radius
actually used), `n_unique` (distinct coordinates), `subpop` (the
dissolved subpopulation polygons as an `sfc` in WGS84, or `NULL`) and
`crs_laea` (the equal-area proj string).

## Details

This is the approach popularised by the ConR package (Dauby *et al.*
2017) and originally proposed by Rivers *et al.* (2010). The buffering
is done on a data-centred Lambert Azimuthal Equal-Area projection (see
[`laea_crs`](https://lucasbarreirageo.github.io/mappingAS/reference/laea_crs.md)),
consistent with the EOO/AOO computations elsewhere in mappingAS.

The buffer radius follows the widely used default of one tenth of the
greatest distance separating two occurrences (Rivers *et al.* 2010):
when `resol_km` is `NULL` it is computed from the data. Because the most
distant pair of points always lies on the convex hull, the maximum
distance is measured over the hull vertices only (great-circle distance
on the WGS84 ellipsoid), which keeps the computation fast for large
datasets.

## References

Dauby G. *et al.* (2017) ConR: An R package to assist large-scale
multispecies preliminary conservation assessments using distribution
data. *Ecology and Evolution* 7:11292-11303.
[doi:10.1002/ece3.3704](https://doi.org/10.1002/ece3.3704)

Rivers M.C. *et al.* (2010) How many herbarium specimens are needed to
detect threatened species? *Biological Conservation* 143:2545-2560.

## See also

[`calc_locations`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_locations.md),
[`calc_aoo`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_aoo.md)

## Examples

``` r
f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
occ <- read_occurrences(f)
sp1 <- occ[occ$species == occ$species[1], ]
calc_subpop(sp1)$n_subpop
#> [1] 4
```
