# Area of Habitat (AOH): suitable land cover within a range and elevation band

Computes the terrestrial Area of Habitat as the *suitable* land cover
within a range polygon, optionally restricted to an elevation band,
following the Area-of-Habitat concept (Brooks *et al.* 2019) and the
sRedList workflow (Cazalis *et al.* 2024). "Suitable" defaults to the
**natural** conservation group of the land-cover legend, so the result
is consistent with the natural-versus-converted metrics used elsewhere
in mappingAS; supply `suitable_groups` to change it.

## Usage

``` r
calc_aoh(
  range,
  year = NULL,
  collection = NULL,
  initiative = "brazil",
  backend = "local",
  src = NULL,
  elev_src = NULL,
  elev_min = NA,
  elev_max = NA,
  z = 9,
  suitable_groups = "natural",
  suitable_codes = NULL,
  marginal_codes = NULL,
  points = NULL,
  max_pixels = 5e+07
)
```

## Arguments

- range:

  An `sf`/`sfc` range **polygon** (e.g. the EOO hull from
  [`calc_eoo`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_eoo.md)
  or the AOO cell union). Points yield `NA`.

- year, collection, initiative, backend, src:

  Land-cover selection, as in
  [`assess_species`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md)
  /
  [`mb_raster_local`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_raster_local.md).

- elev_src:

  Optional DEM path/URL used instead of elevatr.

- elev_min, elev_max:

  Elevation band in metres. `NA` (default) on either side leaves that
  side open; both `NA` skips the elevation mask and the AOH is the
  suitable land cover alone.

- z:

  Terrain-tile zoom for elevatr (default `9`). Ignored when `elev_src`
  is given.

- suitable_groups:

  Conservation group(s) counted as suitable habitat (default
  `"natural"`). See
  [`mb_legend`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_legend.md).

- suitable_codes:

  Optional integer land-cover class codes counted as suitable,
  overriding `suitable_groups` (e.g. the specific classes that make
  ecological sense for the species).

- marginal_codes:

  Optional integer class codes of *marginal* or unknown-suitability
  habitat (sRedList's "Marginal_Unknown"). When supplied, a second,
  broader AOH (`aoh_max_km2`) is returned that adds these to the
  suitable classes, giving a strict-to-broad AOH range.

- points:

  Optional `sf` of occurrence points; when given, the `point_prevalence`
  (share of occurrences inside the suitable habitat) validation metric
  is returned.

- max_pixels:

  Pixel budget for the land-cover read (default `5e7`).

## Value

A list with `aoh_km2` (suitable area within the elevation band, the
AOH), `aoh_max_km2` (the broader AOH including marginal classes; equal
to `aoh_km2` when none), `aoo_upper_km2` / `aoo_upper_cells` (the AOH
rescaled to the 2 km reference scale - the IUCN-compliant AOO upper
bound - and its cell count), `model_prevalence` (share of the range that
is suitable habitat) and `point_prevalence` (share of `points` inside
the habitat; `NA` without `points`), `suitable_km2` (suitable area
*without* the elevation mask, for reference), `elev_min`, `elev_max`,
`elev_applied`, `suitable_groups`, `suitable_codes` and
`marginal_codes`.

## Details

The land cover is read over the range with the same backend as
[`assess_species`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md)
(MapBiomas or Sentinel-2/Esri) and, when an elevation band is given, an
elevation model read over the same extent (windowed; via `elev_src` or
the optional elevatr package) is resampled to the land-cover grid and
used to mask out pixels outside `[elev_min, elev_max]`. The remaining
suitable area is the AOH. The read is bounded by `max_pixels` (the land
cover is aggregated with a majority rule beyond that) so a large range
does not exhaust memory. This AOH can be passed as the habitat-based
upper AOO bound to
[`aoo_bounds`](https://lucasbarreirageo.github.io/mappingAS/reference/aoo_bounds.md).

Every failure (no DEM, no land cover, an unsupported backend) degrades
to `NA` with a message rather than an error.

## References

Brooks T.M. *et al.* (2019) Measuring terrestrial Area of Habitat (AOH)
and its utility for the IUCN Red List. *Trends in Ecology & Evolution*
34:977-986.
[doi:10.1016/j.tree.2019.06.009](https://doi.org/10.1016/j.tree.2019.06.009)

Cazalis V. *et al.* (2024) Accelerating and standardising IUCN Red List
assessments with sRedList. *Biological Conservation* 298:110761.
[doi:10.1016/j.biocon.2024.110761](https://doi.org/10.1016/j.biocon.2024.110761)

## See also

[`aoo_bounds`](https://lucasbarreirageo.github.io/mappingAS/reference/aoo_bounds.md),
[`elevation_preferences`](https://lucasbarreirageo.github.io/mappingAS/reference/elevation_preferences.md),
[`summarise_conversion`](https://lucasbarreirageo.github.io/mappingAS/reference/summarise_conversion.md)

## Examples

``` r
# \donttest{
f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
occ <- read_occurrences(f)
sp1 <- occ[occ$species == occ$species[1], ]
hull <- calc_eoo(sp1)$hull
# Reads MapBiomas land cover (and a DEM if elevatr is installed):
calc_aoh(hull, year = 2024, elev_min = 0, elev_max = 1500)
#> $aoh_km2
#> [1] 1370.189
#> 
#> $aoh_max_km2
#> [1] 1370.189
#> 
#> $suitable_km2
#> [1] 1370.189
#> 
#> $aoo_upper_km2
#> [1] 2708
#> 
#> $aoo_upper_cells
#> [1] 677
#> 
#> $model_prevalence
#> [1] 0.5551775
#> 
#> $point_prevalence
#> [1] NA
#> 
#> $elev_min
#> [1] 0
#> 
#> $elev_max
#> [1] 1500
#> 
#> $elev_applied
#> [1] FALSE
#> 
#> $suitable_groups
#> [1] "natural"
#> 
#> $suitable_codes
#> NULL
#> 
#> $marginal_codes
#> NULL
#> 
# }
```
