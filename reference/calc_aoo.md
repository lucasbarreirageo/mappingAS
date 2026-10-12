# Area of Occupancy (AOO) via occupied grid cells

Computes the AOO as the number of occupied grid cells multiplied by the
cell area, following the IUCN Red List guidelines (reference scale: 2 x
2 km cells = 4 km^2 each) and the approach used by the ConR package.
Points are snapped to a regular grid defined on a data-centred
equal-area projection.

## Usage

``` r
calc_aoo(points, cell_km = 2, n_rep = 100)
```

## Arguments

- points:

  An `sf` of POINT geometries (one species).

- cell_km:

  Grid cell side length in kilometres (default `2`).

- n_rep:

  Integer; number of translated grids to test (default `100`; the
  lattice is cheap, and more offsets bring the count closer to the true
  minimum). The minimum occupied-cell count over the replicates is
  returned.

## Value

A list with `area_km2`, `n_cells`, `cell_km`, `cells` (occupied cells as
an `sfc` polygon set in WGS84) and `crs_laea`.

## Details

Because the occupied-cell count depends on where the grid is placed,
this function tests several translated grids and returns the *smallest*
count, as ConR does. The translations follow a deterministic, evenly
spread lattice, so the result is reproducible (no random seed needed).
The returned `cells` correspond to the grid placement that produced the
reported value.

## Examples

``` r
f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
occ <- read_occurrences(f)
sp1 <- occ[occ$species == occ$species[1], ]
calc_aoo(sp1)$area_km2
#> [1] 16
```
