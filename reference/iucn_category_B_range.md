# Criterion B *size* category range under AOO uncertainty

Combines the fixed EOO (B1) size flag with the *bracketed* AOO (B2) size
flags - from a lower (occurrence-based) and an upper (habitat-based) AOO
bound - into the plausible *range* of Criterion B screening categories.
This is the size-only companion to
[`iucn_category_B`](https://lucasbarreirageo.github.io/mappingAS/reference/iucn_category_B.md)
that lets an assessment report the uncertainty explicitly, as the
sRedList summary does, rather than a single point estimate. Like
[`iucn_category_B`](https://lucasbarreirageo.github.io/mappingAS/reference/iucn_category_B.md)
it reflects size thresholds only (the sub-criteria still apply on top).

## Usage

``` r
iucn_category_B_range(
  eoo_km2 = NA_real_,
  aoo_lower_km2 = NA_real_,
  aoo_upper_km2 = NA_real_
)
```

## Arguments

- eoo_km2:

  EOO in km^2 (`NA` if undefined).

- aoo_lower_km2:

  Lower (occurrence-based) AOO bound in km^2.

- aoo_upper_km2:

  Upper (habitat-based) AOO bound in km^2. `NA` (default) reuses the
  lower bound (no uncertainty).

## Value

A list with `b1_category` (from EOO), `b2_lower` / `b2_upper` (B2 size
flags at the two AOO bounds), `worst` and `best` (the most- and
least-threatened plausible screening categories, as short codes
`"CR"/"EN"/"VU"/"NT-/LC-level"`) and `range` (a compact label such as
`"EN-VU"`, or a single code when they agree).

## Details

Because the EOO is a single value, the most-threatened plausible flag
pairs B1 with B2 at the *lower* AOO, and the least-threatened plausible
flag pairs B1 with B2 at the *upper* AOO; in each case the more
threatened of the two axes wins (as in
[`iucn_category_B`](https://lucasbarreirageo.github.io/mappingAS/reference/iucn_category_B.md)).

## See also

[`iucn_category_B`](https://lucasbarreirageo.github.io/mappingAS/reference/iucn_category_B.md),
[`aoo_bounds`](https://lucasbarreirageo.github.io/mappingAS/reference/aoo_bounds.md)

## Examples

``` r
iucn_category_B_range(eoo_km2 = 6000, aoo_lower_km2 = 48, aoo_upper_km2 = 900)
#> $b1_category
#> [1] "VU (B1 size)"
#> 
#> $b2_lower
#> [1] "EN (B2 size)"
#> 
#> $b2_upper
#> [1] "VU (B2 size)"
#> 
#> $worst
#> [1] "EN"
#> 
#> $best
#> [1] "VU"
#> 
#> $range
#> [1] "EN-VU"
#> 
```
