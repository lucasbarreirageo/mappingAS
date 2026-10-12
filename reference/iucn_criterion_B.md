# Apply IUCN Red List Criterion B (size thresholds plus sub-criteria)

Combines the EOO (B1) and AOO (B2) *size* thresholds of Criterion B with
the sub-criteria required for a threatened listing, following the IUCN
Red List Categories and Criteria (v3.1) and the Guidelines for Using
them (Section 6, Criterion B; Section 10, DD/NT/NE), and aligning the
category selection with the ConR package (Dauby *et al.* 2017,
`cat_criterion_b()`). Unlike
[`iucn_category_B`](https://lucasbarreirageo.github.io/mappingAS/reference/iucn_category_B.md)
(size flags only), this returns a category that can actually be applied.

## Usage

``` r
iucn_criterion_B(
  eoo_km2 = NA_real_,
  aoo_km2 = NA_real_,
  n_locations = NA_real_,
  severe_fragmentation = NA,
  decline = NA,
  extreme_fluctuation = NA,
  assume_decline = TRUE,
  decline_detail = NULL
)
```

## Arguments

- eoo_km2, aoo_km2:

  EOO and AOO in km^2 (`NA` if undefined).

- n_locations:

  Estimated number of locations (`NA` if unknown). This is the value
  that caps the category through sub-criterion (a); see Details.

- severe_fragmentation:

  Logical; `TRUE` if the taxon is severely fragmented (meets
  condition (a) at any level). `NA` (default) = not assessed.

- decline:

  Logical; `TRUE` if there is a continuing decline (sub-criterion b),
  `FALSE` if a decline is known to be absent. `NA` (default) = not
  documented, resolved by `assume_decline`.

- extreme_fluctuation:

  Logical; `TRUE` if there are extreme fluctuations (sub-criterion c).
  `NA` (default) = not documented (never assumed).

- assume_decline:

  Logical; how an undocumented decline (`decline = NA`) is treated.
  `TRUE` (default) assumes a continuing decline is present, as ConR does
  and as the conversion data support; `FALSE` treats it as not met (a
  stricter, more conservative screening). Ignored when `decline` is
  `TRUE`/`FALSE`.

- decline_detail:

  Optional character giving the element(s) of the continuing decline for
  the IUCN code notation (v16, Section 6), appended in parentheses after
  the `b` in `code`: `"i"` EOO, `"ii"` AOO, `"iii"` area/extent/quality
  of habitat, `"iv"` number of locations/subpopulations, `"v"` number of
  mature individuals (combine with commas, e.g. `"ii,iii"`). `NULL`
  (default) omits it, so the code reads e.g. `"EN B1ab"`; with `"iii"`
  it reads `"EN B1ab(iii)"`.

## Value

A list with `category` (one of `"CR"`, `"EN"`, `"VU"`, `"NT"`, `"LC"`,
or `NA` when neither EOO nor AOO is available), `code` (e.g.
`"VU B1ab"`), `qualifies_size` (the highest size level met, or `NA`),
the evaluated sub-criteria `a`, `b`, `c`, and `decline_assumed` (`TRUE`
when sub-criterion (b) was assumed rather than documented).

## Details

A taxon qualifies for a threatened category (CR, EN, VU) only if it
meets the size threshold *and* at least two of these three sub-criteria:

- **(a)** severely fragmented *or* number of locations \\\le\\ 1 (CR), 5
  (EN) or 10 (VU);

- **(b)** continuing decline (in EOO, AOO, area/extent/quality of
  habitat, number of locations/subpopulations, or mature individuals);

- **(c)** extreme fluctuations.

**Relation to the number of locations.** Sub-criterion (a) is derived
from `n_locations` (and `severe_fragmentation` when supplied), and it
*caps* the category exactly as ConR does: the returned category is the
*less threatened* of the level implied by range size and the level
implied by the number of locations. A range whose EOO/AOO alone would be
CR or EN but that is spread over, say, eight locations is therefore
returned as **VU** (\\\le 10\\ locations), not EN - the number of
locations, not size alone, sets the category. Equivalently, per level:
CR needs a single location, EN \\\le 5\\, VU \\\le 10\\ (unless severely
fragmented, which meets condition (a) at any level).

**Continuing decline (b), assumed by default.** Sub-criteria (b) and (c)
cannot be read from occurrence points. Following ConR - and because
mappingAS is built around habitat-conversion data, which is direct
evidence of a continuing decline in the area, extent and quality of
habitat (sub-criterion b(iii)) - a continuing decline is **assumed to be
present** when it is not explicitly documented (`decline = NA` with
`assume_decline = TRUE`, the default). This is what makes the screening
track the IUCN thresholds instead of collapsing every undocumented taxon
to NT: with the location condition (a) met and decline (b) assumed, a
small-range, few-location taxon is returned at its CR/EN/VU
size-and-location level. Set `assume_decline = FALSE` (or
`decline = FALSE`) to require a documented decline instead, in which
case a size-and-location match with no second sub-criterion falls to NT.
Extreme fluctuation (c) is never assumed.

Following Section 10: a taxon that meets a size threshold but not two
sub-criteria (for example a small range spread over too many locations,
or with decline explicitly absent) is returned as **NT** (Near
Threatened, it "nearly meets" the requirements); one clearly far from
every threshold as **LC**. **DD** and **NE** are *never* assigned
automatically - they require the assessor's judgement about data
adequacy and are left to the user.

## References

Dauby G. *et al.* (2017) ConR: An R package to assist large-scale
multispecies preliminary conservation assessments using distribution
data. *Ecology and Evolution* 7:11292-11303.
[doi:10.1002/ece3.3704](https://doi.org/10.1002/ece3.3704)

## See also

[`iucn_category_B`](https://lucasbarreirageo.github.io/mappingAS/reference/iucn_category_B.md)

## Examples

``` r
# EN-sized range with few locations: decline assumed by default -> EN B1ab
iucn_criterion_B(eoo_km2 = 3000, aoo_km2 = 400, n_locations = 4)$code
#> [1] "EN B1ab+2ab"
# Same size but spread over 8 locations -> capped to VU by the location count
iucn_criterion_B(eoo_km2 = 3000, aoo_km2 = 400, n_locations = 8)$category
#> [1] "VU"
# Require a documented decline: with none, a size/location match falls to NT
iucn_criterion_B(eoo_km2 = 3000, aoo_km2 = 400, n_locations = 4,
                 assume_decline = FALSE)$category
#> [1] "NT"
```
