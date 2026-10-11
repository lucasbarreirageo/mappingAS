# Overlap between a species' range and federal Protected areas

Given a species' occurrence points and (optionally) its EOO and AOO,
plus a UC layer from
[`protected_areas()`](https://lucasbarreirageo.github.io/mappingAS/reference/protected_areas.md),
computes how much of the range is legally protected: the share of
occurrences inside UCs, the percentage/area of the EOO and AOO that fall
within UCs, and the list of UCs touched.

## Usage

``` r
summarise_protected(points, pa, eoo = NULL, aoo = NULL)
```

## Arguments

- points:

  An `sf` of POINT geometries (one species).

- pa:

  A UC `sf` from
  [`protected_areas()`](https://lucasbarreirageo.github.io/mappingAS/reference/protected_areas.md)
  (standardised columns).

- eoo, aoo:

  The `eoo`/`aoo` sublists produced by
  [`assess_species()`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md)
  (or
  [`calc_eoo()`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_eoo.md)/[`calc_aoo()`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_aoo.md)).
  Either may be `NULL`.

## Value

A list: `n_occ`, `n_occ_in`, `occ_pct`; `eoo_pct`, `eoo_km2`; `aoo_pct`,
`aoo_km2`, `aoo_cells_in`; `n_uc`; `list` (per-UC data.frame) and
`layer` (the UC `sf`).

## Details

Percentages are computed on the species' equal-area projection so ratios
are exact; the reported `eoo_km2`/`aoo_km2` inside UCs are derived from
the headline EOO/AOO so they stay consistent with
[`assess_species()`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md).
The AOO overlap follows the grid nature of the metric: an occupied 2-km
cell counts as protected when it intersects any UC.
