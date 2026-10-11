# Assess severe fragmentation from occurrences and an isolation distance

Clusters occurrences into subpopulations - groups of occurrences no
further apart than an isolation distance - and quantifies how much of
the population lies in small subpopulations, to guide the "severely
fragmented" judgement of IUCN Criterion B sub-criterion (a). This
mirrors the fragmentation step of sRedList (Cazalis *et al.* 2024, after
Santini *et al.* 2019) but from occurrence data alone, so no habitat
model or density estimate is required.

## Usage

``` r
assess_fragmentation(
  points,
  isolation_km,
  weights = NULL,
  small_size = NULL,
  n_thresholds = 50
)
```

## Arguments

- points:

  An `sf` of POINT geometries (one species). Use
  [`read_occurrences`](https://lucasbarreirageo.github.io/mappingAS/reference/read_occurrences.md)
  to produce it.

- isolation_km:

  Isolation distance in km: occurrences further apart than this are
  treated as belonging to isolated subpopulations. Required.

- weights:

  Optional numeric vector, one weight per occurrence in `points` (a
  relative subpopulation-size contribution, e.g. occupied area). `NULL`
  (default) weights every occurrence equally (1).

- small_size:

  Optional single size the assessor considers "small". When supplied,
  `prop_in_small` (share of the population in subpopulations \\\le\\
  this size) and `severe_suggested` (`TRUE` when that share \\\ge\\ 0.5)
  are returned. `NULL` (default) leaves the judgement to the curve.

- n_thresholds:

  Number of points on the fragmentation `curve` (default `50`).

## Value

A list with `n_subpop`, `isolation_km`, `sizes` (a `data.frame` of
`cluster`, `size`, `n_points`, ordered largest first), `clusters` (the
subpopulation hulls/buffers as an `sfc` in WGS84), `curve` (a
`data.frame` of `size` and `prop_pop_le`), `median_subpop_size`,
`largest_subpop_pct` (percent of the population in the single largest
subpopulation), `prop_in_small` / `severe_suggested` (when `small_size`
is given, else `NA`) and `crs_laea`.

## Details

Two occurrences are placed in the same subpopulation when they are
within `isolation_km` of each other (a circular-buffer clustering with
radius `isolation_km / 2`, unioned on a data-centred equal-area
projection, as in
[`calc_subpop`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_subpop.md)).
The relative *size* of each subpopulation is the sum of its occurrence
`weights` (the number of occurrences by default; supply, for example,
per-occurrence occupied area for an area-weighted size). The
fragmentation `curve` then gives, for a range of candidate "small
subpopulation" sizes, the proportion of the total population contained
in subpopulations no larger than that size; the `median_subpop_size` is
the size at which that proportion reaches one half (half the population
lives in subpopulations smaller than this). If the assessor considers a
subpopulation of `median_subpop_size` to be "small", the population can
be considered severely fragmented.

**This is a screening proxy**: occurrence counts are sampling-biased and
are not individuals. Use the result only when the mapped clusters make
ecological sense as subpopulations, exactly as sRedList advises.

## References

Santini L. *et al.* (2019) Applying habitat and population-density
models to land-cover time series to inform IUCN Red List assessments.
*Conservation Biology* 33:1084-1093.
[doi:10.1111/cobi.13279](https://doi.org/10.1111/cobi.13279)

Cazalis V. *et al.* (2024) Accelerating and standardising IUCN Red List
assessments with sRedList. *Biological Conservation* 298:110761.
[doi:10.1016/j.biocon.2024.110761](https://doi.org/10.1016/j.biocon.2024.110761)

## See also

[`calc_subpop`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_subpop.md),
[`iucn_criterion_B`](https://lucasbarreirageo.github.io/mappingAS/reference/iucn_criterion_B.md)

## Examples

``` r
f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
occ <- read_occurrences(f)
sp1 <- occ[occ$species == occ$species[1], ]
fr <- assess_fragmentation(sp1, isolation_km = 20)
fr$n_subpop
#> [1] 4
fr$median_subpop_size
#> [1] 1
```
