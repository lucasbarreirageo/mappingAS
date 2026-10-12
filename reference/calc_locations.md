# Estimate of number of locations (occupied-grid-cell method, optionally protected-area aware)

Estimates the number of "locations" (in the IUCN sense: geographically
or ecologically distinct areas in which a single threatening event can
rapidly affect all individuals) by overlaying a regular grid and
counting the occupied cells, optionally decoupling occurrences that fall
inside protected areas from those outside. This mirrors the two
complementary approaches used by the ConR package (Dauby *et al.* 2017):
the grid method, and the protected-area integration
(`method_protected`).

## Usage

``` r
calc_locations(
  points,
  grid_km = 10,
  n_rep = 100,
  protected = NULL,
  method_protected = c("no_more_than_one", "other"),
  cell_scale = NULL
)
```

## Arguments

- points:

  An `sf` of POINT geometries (one species).

- grid_km:

  Grid cell side length in kilometres (default `10`). Ignored when
  `cell_scale` is supplied.

- n_rep:

  Integer; number of translated grids to test (default `100`). The
  minimum occupied-cell count over the replicates is returned.

- protected:

  Optional protected-area layer (an `sf`/`sfc` of polygons, e.g. from
  [`protected_areas`](https://lucasbarreirageo.github.io/mappingAS/reference/protected_areas.md)).
  `NULL` (default) uses the plain grid method.

- method_protected:

  How protected areas enter the count when `protected` is given:
  `"no_more_than_one"` (default) or `"other"`. See Details.

- cell_scale:

  Optional numeric fraction for the sliding-scale cell size: the cell
  side becomes `cell_scale` times the maximum distance between two
  occurrences. When supplied it overrides `grid_km`.

## Value

A list with `n_locations` (integer estimate), `grid_km` (the cell size
actually used), `method` (`"grid"`, `"no_more_than_one"` or `"other"`),
`n_out` / `n_in` (locations outside / inside protected areas; `n_in` is
`NA` for the plain grid), `n_cells`, `cells` (gridded location cells as
an `sfc` in WGS84), `pa_locations` (the protected areas counted as
locations under `"no_more_than_one"`, else `NULL`) and `crs_laea`.

## Details

As for the AOO, the occupied-cell count depends on where the grid is
placed, so several translated grids (a deterministic lattice) are tested
and the *smallest* count is returned (the most conservative, i.e. most
threatened, estimate). The grid is built on a data-centred Lambert
Azimuthal Equal-Area projection and this function reuses mappingAS's own
AOO grid machinery, so it differs from
[`calc_aoo`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_aoo.md)
only in the (larger) default cell size.

**Cell size.** By default a fixed cell of `grid_km` (10 km) is used,
representing the scale at which a single threat could affect the whole
occupied cell. Alternatively a species-specific "sliding scale" (Rivers
*et al.* 2010) is available: set `cell_scale` to a fraction (e.g.
`0.05`) and the cell side becomes that fraction of the maximum distance
between two occurrences (`cell_scale` then overrides `grid_km`).

**Protected areas.** When `protected` is a protected-area layer (e.g.
from
[`protected_areas`](https://lucasbarreirageo.github.io/mappingAS/reference/protected_areas.md)),
occurrences are split into those inside and outside protected areas,
because a subpopulation inside a protected area is not subject to the
same threats as one outside:

- `method_protected = "no_more_than_one"` (default): every protected
  area holding at least one occurrence counts as exactly one location
  (all occurrences within a protected area share a location);
  occurrences outside protected areas are gridded as usual. Overlapping
  or nested designations (common in WDPA, e.g. a national park inside an
  environmental protection area) are counted once: the smallest set of
  protected areas that holds every inside occurrence is used. The total
  is (occupied cells outside) + (size of that set).

- `method_protected = "other"`: occurrences inside protected areas are
  gridded *separately* from those outside, and the two counts are added.
  Two occurrences closer than the cell size are then still counted as
  separate locations when one is inside and the other outside a
  protected area.

With `protected = NULL` (the default) no protected-area information is
used and the plain grid count is returned.

## References

Dauby G. *et al.* (2017) ConR: An R package to assist large-scale
multispecies preliminary conservation assessments using distribution
data. *Ecology and Evolution* 7:11292-11303.
[doi:10.1002/ece3.3704](https://doi.org/10.1002/ece3.3704)

Rivers M.C. *et al.* (2010) How many herbarium specimens are needed to
detect threatened species? *Biological Conservation* 143:2545-2560.

IUCN Standards and Petitions Committee. Guidelines for Using the IUCN
Red List Categories and Criteria.

## See also

[`calc_subpop`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_subpop.md),
[`calc_aoo`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_aoo.md),
[`protected_areas`](https://lucasbarreirageo.github.io/mappingAS/reference/protected_areas.md)

## Examples

``` r
f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
occ <- read_occurrences(f)
sp1 <- occ[occ$species == occ$species[1], ]
calc_locations(sp1)$n_locations
#> [1] 4
```
