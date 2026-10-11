# Interactive Area of Habitat (AOH) map for one species

Builds a leaflet map of the terrestrial Area of Habitat: the *suitable*
land-cover classes within the EOO, optionally restricted to an elevation
band, drawn as a single green overlay together with the EOO hull and the
occurrence points. It is the map companion to
[`calc_aoh`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_aoh.md)
and uses the same suitable-class and elevation definitions, so the map
shows exactly the area
[`calc_aoh()`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_aoh.md)
measures.

## Usage

``` r
map_aoh(
  assessment,
  species = NULL,
  suitable_groups = "natural",
  suitable_codes = NULL,
  elev_min = NA,
  elev_max = NA,
  z = 9,
  elev_src = NULL,
  src = NULL,
  max_pixels = 800,
  lang = c("pt", "en")
)
```

## Arguments

- assessment:

  A `geoconv_assessment` from
  [`assess_species`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md).

- species:

  Species name. `NULL` (default) uses the first.

- suitable_groups:

  Conservation group(s) counted as suitable habitat (default
  `"natural"`). Ignored when `suitable_codes` is given.

- suitable_codes:

  Optional integer land-cover class codes counted as suitable,
  overriding `suitable_groups`.

- elev_min, elev_max:

  Elevation band in metres (`NA` leaves that side open; both `NA` skips
  the elevation mask).

- z:

  Terrain-tile zoom for elevatr (default `9`).

- elev_src, src:

  Optional DEM and land-cover overrides (path/URL).

- max_pixels:

  Display pixel budget for the land-cover read (default `800`).

- lang:

  `"pt"` (default) or `"en"` for the legend labels.

## Value

A `leaflet` widget, or a base map with just the points/EOO when the land
cover cannot be read.

## Details

Suitable habitat is, by default, the **natural** conservation group of
the land-cover legend; pass `suitable_groups` (one or more groups) or
`suitable_codes` (specific land-cover class codes, e.g. the classes that
make ecological sense for the species) to change it. When an elevation
band is given, an elevation model read over the EOO (via `elev_src` or
the optional elevatr package) masks out the habitat outside
`[elev_min, elev_max]`.

## See also

[`calc_aoh`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_aoh.md),
[`map_species`](https://lucasbarreirageo.github.io/mappingAS/reference/map_species.md)
