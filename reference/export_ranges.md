# Export EOO and AOO as spatial files (shapefile or GeoPackage)

Writes the Extent of Occurrence (EOO) polygons and the Area of Occupancy
(AOO) polygons produced by
[`assess_species()`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md)
to disk, with the area, habitat-conversion and provisional-category
attributes attached to each feature (one feature per species by
default). Handy for opening the results in QGIS/ArcGIS or sharing them.

## Usage

``` r
export_ranges(
  assessment,
  dir = tempdir(),
  layer_prefix = "mappingAS",
  what = c("both", "eoo", "aoo"),
  format = c("shapefile", "gpkg"),
  crs = 4326,
  aoo_as = c("cells", "union"),
  class_csv = TRUE,
  zip = FALSE,
  quiet = TRUE
)
```

## Arguments

- assessment:

  A `geoconv_assessment` object from
  [`assess_species()`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md).

- dir:

  Output directory (created if it does not exist). Default
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html); pass an explicit
  path to keep the files.

- layer_prefix:

  File/layer name prefix. Default `"mappingAS"`.

- what:

  Which ranges to export: `"both"` (default), `"eoo"` or `"aoo"`.

- format:

  `"shapefile"` (default; one file set per range, i.e. `*_EOO.shp` and
  `*_AOO.shp`) or `"gpkg"` (a single GeoPackage holding both layers).

- crs:

  Output CRS (default `4326`, WGS84). Anything accepted by
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
  also works (an EPSG code or a proj string).

- aoo_as:

  `"cells"` (default; one feature per occupied 2-km grid cell, each with
  a `cell_id`) or `"union"` (one dissolved feature per species).

- class_csv:

  If `TRUE` (default), also writes `<prefix>_classes.csv` with the full
  per-class MapBiomas breakdown (every class) for the EOO and AOO of
  each species; included in the zip when `zip = TRUE`. Skipped if
  MapBiomas was not computed.

- zip:

  If `TRUE`, the written files are bundled into a single `.zip` inside
  `dir` and the path to that zip is returned (most useful for the
  shapefile format, which has several sidecar files). Default `FALSE`.

- quiet:

  Passed to
  [`sf::st_write()`](https://r-spatial.github.io/sf/reference/st_write.html).
  Default `TRUE`.

## Value

(Invisibly) a character vector with the file paths written, or the path
to the `.zip` when `zip = TRUE`.

## Details

Because the ESRI Shapefile format limits field names to 10 characters,
the attribute fields use compact names (see the Attribute fields
section). A GeoPackage (`format = "gpkg"`) stores both ranges as two
layers (`eoo`, `aoo`) inside a single file and has no such limitation.

## Attribute fields

`species`; `eoo_km2` or `aoo_km2` (and `n_cells` for AOO); `conv_pct`
(percent converted/anthropic within that range); `nat_pct` (percent
natural); `cat_B1` or `cat_B2` (provisional size-only category);
`prov_cat`; `mb_year`; `mb_coll` (MapBiomas year and collection).
Conversion fields are `NA` when MapBiomas was not computed.

## Examples

``` r
occ <- read_occurrences(system.file("extdata", "example_occurrences.csv",
                                    package = "mappingAS"))
res <- assess_species(occ, mapbiomas = FALSE, verbose = FALSE)

d <- file.path(tempdir(), "mappingAS_ranges")
dir.create(d, showWarnings = FALSE)

# Two shapefiles (EOO + AOO) written to a temporary directory:
export_ranges(res, dir = d)

# A single GeoPackage with both layers:
export_ranges(res, dir = d, format = "gpkg")

# \donttest{
# Everything bundled into one .zip (good for downloads):
export_ranges(res, dir = d, zip = TRUE)
# }
```
