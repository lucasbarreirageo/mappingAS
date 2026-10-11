# Countries of occurrence and biogeographical realm(s) from a range

Derives the list of countries a taxon occurs in - and an approximate
biogeographical realm - by intersecting its range with the public
Natural Earth country base map, mirroring the "countries of occurrence"
and "realms" supporting information that the IUCN Red List
requires/recommends and that sRedList extracts automatically (Cazalis
*et al.* 2024).

## Usage

``` r
countries_of_occurrence(
  range,
  points = NULL,
  scale = c("medium", "small", "large")
)
```

## Arguments

- range:

  An `sf`/`sfc` polygon delimiting the range (e.g. the EOO hull from
  [`calc_eoo`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_eoo.md)),
  or an `sf` of occurrence points. Any CRS is accepted (it is
  transformed to WGS84 internally).

- points:

  Optional `sf` of occurrence POINT geometries used to set the
  **Extant** / **Possibly Extant** presence flag. `NULL` (default) means
  the presence is taken from `range` itself when it is a set of points,
  otherwise every overlapping country is **Possibly Extant**.

- scale:

  Natural Earth scale passed to
  [`rnaturalearth::ne_countries`](https://docs.ropensci.org/rnaturalearth/reference/ne_countries.html)
  (`"small"`, `"medium"` (default) or `"large"`).

## Value

A `data.frame` with one row per country: `country`, `presence`
(`"Extant"`/`"Possibly Extant"`), `n_records` (occurrences in that
country), `continent` and `realm`. Rows are ordered by presence then
country. An empty `data.frame` (with a message) is returned when
rnaturalearth is not installed or no country overlaps.

## Details

Following the Red List convention, every country the range *polygon*
overlaps is reported, and each is flagged by presence:

- **Extant** - the country holds at least one occurrence record;

- **Possibly Extant** - the country overlaps the range but holds no
  record in the supplied points.

When no polygon is supplied (fewer than three points, so no EOO hull)
the countries are taken from the points alone and are all reported as
**Extant**.

**Realm.** The biogeographical realm is a *coarse approximation* derived
from the continent of each country (Neotropical, Nearctic, Afrotropical,
Palearctic, Indomalayan, Australasian, Oceanian or Antarctic); it is
exact for the South-American ranges mappingAS is built around but should
be checked for taxa spanning continental transitions (e.g. North/Central
America, North Africa, South-East Asia), where realm boundaries do not
follow country borders.

## References

Cazalis V. *et al.* (2024) Accelerating and standardising IUCN Red List
assessments with sRedList. *Biological Conservation* 298:110761.
[doi:10.1016/j.biocon.2024.110761](https://doi.org/10.1016/j.biocon.2024.110761)

## See also

[`calc_eoo`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_eoo.md),
[`assess_species`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md)

## Examples

``` r
# \donttest{
f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
occ <- read_occurrences(f)
sp1 <- occ[occ$species == occ$species[1], ]
# Needs the 'rnaturalearth' package (Suggests):
countries_of_occurrence(calc_eoo(sp1)$hull, points = sp1)
#>   country presence n_records     continent       realm
#> 1  Brazil   Extant         5 South America Neotropical
# }
```
