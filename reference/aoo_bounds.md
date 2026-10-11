# Lower/upper Area of Occupancy bounds and the Criterion B2 category range

Turns the occurrence-based AOO (the lower bound) and the Area of Habitat
(AOH; the suitable/natural habitat within the range, an upper bound)
into a bracketed AOO and the corresponding range of Criterion B2 *size*
categories, following the IUCN guidance that the true AOO lies between
an occurrence-based minimum and a habitat-based maximum (Brooks *et al.*
2019; IUCN Standards and Petitions Committee 2024), as implemented in
sRedList (Cazalis *et al.* 2024).

## Usage

``` r
aoo_bounds(aoo_lower_km2, aoh_km2 = NA_real_, eoo_km2 = NA_real_)
```

## Arguments

- aoo_lower_km2:

  The occurrence-based AOO in km^2 (from
  [`calc_aoo`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_aoo.md));
  the lower bound.

- aoh_km2:

  The Area of Habitat (suitable/natural habitat area) within the range,
  in km^2 - e.g. the `natural_km2` of the EOO conversion from
  [`summarise_conversion`](https://lucasbarreirageo.github.io/mappingAS/reference/summarise_conversion.md).
  `NA` (default) if unknown.

- eoo_km2:

  The EOO in km^2 (from
  [`calc_eoo`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_eoo.md)),
  used to cap the upper bound. `NA` (default) leaves the upper bound
  uncapped.

## Value

A list with `aoo_lower`, `aoo_upper`, `aoh` (the clamped habitat area
used), `cat_lower` / `cat_upper` (the B2 size category at each bound,
e.g. `"EN (B2 size)"`) and `cat_range` (a compact string such as
`"EN-VU"`, or a single category when both bounds agree).

## Details

The upper bound is the AOH clamped so it is never below the lower bound
and never above the EOO (occupancy cannot exceed the extent of
occurrence). When the AOH is not available (`aoh_km2 = NA`, e.g. land
cover was not computed) the upper bound falls back to the lower bound
and the range collapses to a single category.

## References

Brooks T.M. *et al.* (2019) Measuring terrestrial Area of Habitat (AOH)
and its utility for the IUCN Red List. *Trends in Ecology & Evolution*
34:977-986.
[doi:10.1016/j.tree.2019.06.009](https://doi.org/10.1016/j.tree.2019.06.009)

Cazalis V. *et al.* (2024) Accelerating and standardising IUCN Red List
assessments with sRedList. *Biological Conservation* 298:110761.
[doi:10.1016/j.biocon.2024.110761](https://doi.org/10.1016/j.biocon.2024.110761)

## See also

[`calc_aoo`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_aoo.md),
[`iucn_category_B`](https://lucasbarreirageo.github.io/mappingAS/reference/iucn_category_B.md),
[`iucn_category_B_range`](https://lucasbarreirageo.github.io/mappingAS/reference/iucn_category_B_range.md)

## Examples

``` r
# Occurrence AOO of 48 km^2, but 900 km^2 of natural habitat within a
# 6000 km^2 EOO: AOO is bracketed EN (lower) to VU (upper).
aoo_bounds(aoo_lower_km2 = 48, aoh_km2 = 900, eoo_km2 = 6000)
#> $aoo_lower
#> [1] 48
#> 
#> $aoo_upper
#> [1] 900
#> 
#> $aoh
#> [1] 900
#> 
#> $cat_lower
#> [1] "EN (B2 size)"
#> 
#> $cat_upper
#> [1] "VU (B2 size)"
#> 
#> $cat_range
#> [1] "EN-VU"
#> 
```
