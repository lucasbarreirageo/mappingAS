# Build the MapBiomas land-use GeoTIFF URL for a given year and initiative

Returns the public Google Cloud Storage URL of the MapBiomas
land-use/land-cover annual mosaic for the chosen initiative. All
supported initiatives publish annual single-year Cloud-Optimized
GeoTIFFs on the same public bucket, so they can be streamed with GDAL's
`/vsicurl/` driver (no Google Earth Engine, no Google Drive). See
[`mb_initiatives`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_initiatives.md)
for the full list of initiatives, their default collections and year
spans.

## Usage

``` r
mb_source_url(year = 2024, collection = NULL, initiative = "brazil")
```

## Arguments

- year:

  Integer year.

- collection:

  Integer collection number. `NULL` (default) uses the initiative's
  default collection.

- initiative:

  One of the keys of
  [`mb_initiatives`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_initiatives.md)
  (`"brazil"` by default).

## Value

A length-1 character URL.

## Examples

``` r
mb_source_url(2024)
#> [1] "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_10/lulc/coverage/brazil_coverage_2024.tif"
mb_source_url(2023, initiative = "amazonia")
#> [1] "https://storage.googleapis.com/mapbiomas-public/initiatives/amazon/lulc/collection_6/integration/mapbiomas_collection60_integration_v1-classification_2023.tif"
mb_source_url(2024, initiative = "peru")
#> [1] "https://storage.googleapis.com/mapbiomas-public/initiatives/peru/collection_3/LULC/peru_collection3_integration_v1-classification_2024.tif"
```
