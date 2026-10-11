# Read species occurrence points from XLSX, CSV or vector files

Imports occurrence records from a spreadsheet (`.xlsx`/`.xls`),
delimited text (`.csv`/`.tsv`/`.txt`) or a vector file (`.shp`, `.gpkg`,
`.geojson`; for shapefiles you may also pass a `.zip` containing the
.shp/.shx/.dbf/.prj set) and returns a clean point `sf` in WGS84
(EPSG:4326) with a standardised `species` column.

## Usage

``` r
read_occurrences(
  path,
  species_col = NULL,
  lon_col = NULL,
  lat_col = NULL,
  crs = 4326,
  sheet = NULL,
  sep = NULL,
  dec = NULL,
  encoding = "UTF-8",
  default_species = "sp1"
)
```

## Arguments

- path:

  Path to the input file.

- species_col, lon_col, lat_col:

  Optional column names to override auto-detection. `lon_col`/`lat_col`
  are ignored for vector files that already carry geometry.

- crs:

  Input CRS for tabular coordinates (default `4326`). Output is always
  reprojected to WGS84.

- sheet:

  For Excel inputs, the sheet name or index (default first sheet).

- sep:

  For delimited text, the field separator. If `NULL` (default) it is
  auto-detected from the header line (`;`, `,` or tab).

- dec:

  Decimal mark for delimited text. If `NULL` (default) it is inferred
  from `sep` (`,` when `sep` is `;`, else `.`), matching Brazilian Excel
  exports.

- encoding:

  File encoding for delimited text (default `"UTF-8"`; use e.g.
  `"latin1"` for Windows-1252 files).

- default_species:

  Species label used when no species column is found (default `"sp1"`).

## Value

An `sf` of POINT geometries in EPSG:4326 with a `species` column plus
all original attributes.

## Details

Longitude, latitude and species columns are auto-detected from common
header names (Darwin Core `decimalLongitude`/`decimalLatitude`,
`lon`/`lat`, `x`/`y`, `scientificName`, etc.). You can override the
detection with the corresponding arguments. Rows with missing or
out-of-range coordinates are dropped with a message.

For delimited text the field separator and decimal mark are
auto-detected, so Brazilian-style CSVs exported from Excel (`;`
separator, `,` decimals, optional UTF-8 BOM) are read correctly without
extra arguments.

## Examples

``` r
f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
occ <- read_occurrences(f)
table(occ$species)
#> 
#>       Prepusa connata Gardner    Prepusa hookeriana Gardner 
#>                             5                            21 
#> Worsleya procera (Lem.) Traub 
#>                             3 
```
