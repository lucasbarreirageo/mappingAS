# Severe fragmentation from suitable-habitat patches and population density

Implements the habitat-and-density fragmentation method of the IUCN Red
List guidelines (Santini *et al.* 2019), as used by sRedList (Cazalis
*et al.* 2024): the suitable habitat (Area of Habitat) within the range
is split into **patches**, patches within a **dispersal / isolation
distance** of one another are grouped into isolated **subpopulations**,
and each subpopulation's size in **individuals** is its habitat area
times the population **density**. This is the guideline-compliant
companion to
[`assess_fragmentation`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_fragmentation.md)
(which uses occurrence counts as a rough proxy when no habitat model or
density is available).

## Usage

``` r
fragment_habitat(
  range,
  isolation_km,
  density,
  year = NULL,
  collection = NULL,
  initiative = "brazil",
  src = NULL,
  suitable_groups = "natural",
  suitable_codes = NULL,
  elev_min = NA,
  elev_max = NA,
  z = 9,
  elev_src = NULL,
  max_pixels = 4000,
  small_size = NULL
)
```

## Arguments

- range:

  An `sf`/`sfc` range **polygon** (e.g. the EOO hull).

- isolation_km:

  Isolation / dispersal distance in km: patches further apart than this
  host isolated subpopulations. Required.

- density:

  Population density in mature individuals per km^2 of suitable habitat.
  A length-2 vector gives a low/high range (a second population column
  `pop_high` is added). Required.

- year, collection, initiative, src:

  Land-cover selection, as in
  [`calc_aoh`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_aoh.md)
  /
  [`mb_raster_local`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_raster_local.md).

- suitable_groups, suitable_codes:

  Which land-cover classes count as suitable habitat (default: the
  `"natural"` group). See
  [`calc_aoh`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_aoh.md).

- elev_min, elev_max, z, elev_src:

  Optional elevation band (metres) and DEM source, as in
  [`calc_aoh`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_aoh.md).

- max_pixels:

  Display pixel budget for the habitat mask read (default `4000`);
  larger ranges are read coarser, as sRedList aggregates them.

- small_size:

  Optional size (individuals) the assessor considers "small"; when
  given, `prop_in_small` and `severe_suggested` are returned.

## Value

A list with `method = "habitat"`, `n_subpop` (occupied subpopulations),
`isolation_km`, `density`, `sizes` (a `data.frame` of `cluster`,
`aoh_km2`, `pop` and, for a density range, `pop_high`), `clusters`
(subpopulation polygons as an `sfc` in WGS84), `curve`,
`median_subpop_size`, `max_subpop` (C2a(i)), `pct_largest` (C2a(ii)),
`total_pop`, and `prop_in_small` / `severe_suggested`. Values degrade to
`NA` when the land cover cannot be read.

## Details

Following Santini *et al.* (2019): contiguous suitable cells form
patches; each patch is buffered by half the isolation distance and
buffers that touch are merged, so patches closer than the isolation
distance share a subpopulation; the number of individuals in a
subpopulation is its total suitable area (km^2) times `density`
(individuals km^-2), and a subpopulation of fewer than two individuals
is treated as unoccupied. The `curve` then gives the proportion of the
population in subpopulations no larger than a given size, whose
`median_subpop_size` is the size below which half the population lives;
if the assessor considers a subpopulation of that size "small", the
population can be treated as severely fragmented (Criterion B
sub-criterion a). The largest subpopulation's size (`max_subpop`,
Criterion C2a(i)) and its share of the population (`pct_largest`,
Criterion C2a(ii)) are also returned.

**Use with care**: use the result only when the mapped clusters make
ecological sense as subpopulations, exactly as sRedList advises.
Barriers (water, mountains, infrastructure) are not accounted for.

## References

Santini L. *et al.* (2019) Applying habitat and population-density
models to land-cover time series to inform IUCN Red List assessments.
*Conservation Biology* 33:1084-1093.
[doi:10.1111/cobi.13279](https://doi.org/10.1111/cobi.13279)

Cazalis V. *et al.* (2024) Accelerating and standardising IUCN Red List
assessments with sRedList. *Biological Conservation* 298:110761.
[doi:10.1016/j.biocon.2024.110761](https://doi.org/10.1016/j.biocon.2024.110761)

## See also

[`assess_fragmentation`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_fragmentation.md),
[`calc_aoh`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_aoh.md)

## Examples

``` r
# \donttest{
f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
occ <- read_occurrences(f)
sp1 <- occ[occ$species == occ$species[1], ]
hull <- calc_eoo(sp1)$hull
# Reads land cover; density in mature individuals per km^2:
fr <- fragment_habitat(hull, isolation_km = 20, density = 5)
#> fragment_habitat(): suitable-habitat raster unavailable; NA.
fr$n_subpop
#> [1] NA
fr$max_subpop      # C2a(i)
#> [1] NA
# }
```
