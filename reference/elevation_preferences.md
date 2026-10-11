# Suggest elevation preferences from the occurrences' elevation

Extracts the elevation of each occurrence from a digital elevation model
and suggests the species' elevation limits, mirroring the sRedList step
that pre-fills elevation preferences from occurrence records (Cazalis
*et al.* 2024). The suggested minimum is rounded down and the maximum
rounded up to the nearest `round_to` metres, as sRedList does.

## Usage

``` r
elevation_preferences(points, z = 9, src = NULL, round_to = 100)
```

## Arguments

- points:

  An `sf` of POINT geometries (one species). Use
  [`read_occurrences`](https://lucasbarreirageo.github.io/mappingAS/reference/read_occurrences.md)
  to produce it.

- z:

  Terrain-tile zoom level passed to
  [`elevatr::get_elev_raster`](https://rdrr.io/pkg/elevatr/man/get_elev_raster.html)
  (default `9`, ~300 m; higher is finer but heavier). Ignored when `src`
  is given.

- src:

  Optional path/URL to a DEM GeoTIFF used instead of elevatr (read
  windowed through GDAL `/vsicurl/` for a URL).

- round_to:

  Rounding step in metres for the suggested limits (default `100`);
  `0`/`NA` disables rounding.

## Value

A list with `n` (occurrences with a value), `min`, `max`, `median`
(metres), `suggested_min` / `suggested_max` (rounded limits) and
`values` (the per-occurrence elevations). All-`NA` when no DEM is
available.

## Details

The DEM is read only over the occurrences' extent (windowed), from `src`
or the public AWS Terrain Tiles via the optional elevatr package. When
no DEM is available the function returns `NA` values with a message
rather than raising an error.

## References

Cazalis V. *et al.* (2024) Accelerating and standardising IUCN Red List
assessments with sRedList. *Biological Conservation* 298:110761.
[doi:10.1016/j.biocon.2024.110761](https://doi.org/10.1016/j.biocon.2024.110761)

## See also

[`calc_aoh`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_aoh.md)

## Examples

``` r
# \donttest{
f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
occ <- read_occurrences(f)
sp1 <- occ[occ$species == occ$species[1], ]
# Needs the 'elevatr' package (Suggests) and internet:
elevation_preferences(sp1)
#> $n
#> [1] 0
#> 
#> $min
#> [1] NA
#> 
#> $max
#> [1] NA
#> 
#> $median
#> [1] NA
#> 
#> $suggested_min
#> [1] NA
#> 
#> $suggested_max
#> [1] NA
#> 
#> $values
#> numeric(0)
#> 
#> $z
#> [1] 9
#> 
# }
```
