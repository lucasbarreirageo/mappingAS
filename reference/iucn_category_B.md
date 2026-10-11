# Provisional IUCN Red List Criterion B category from range size

Returns the category implied by the EOO (B1) and AOO (B2) *size*
thresholds only. This is a screening aid, exactly like GeoCat: a full
Criterion B listing additionally requires at least two of the
sub-criteria (a) severe fragmentation / few locations, (b) continuing
decline, and (c) extreme fluctuation. Never report these as final
categories.

## Usage

``` r
iucn_category_B(eoo_km2 = NA_real_, aoo_km2 = NA_real_)
```

## Arguments

- eoo_km2, aoo_km2:

  Numeric range sizes in square kilometres.

## Value

A list with `eoo_category`, `aoo_category` and `combined` (the more
threatened of the two size-based flags).
