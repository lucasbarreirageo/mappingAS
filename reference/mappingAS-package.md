# mappingAS: Range Metrics and MapBiomas Habitat Conversion

A GeoCat-style toolkit for IUCN Red List Criterion B screening. It
imports species occurrence points from XLSX, CSV, or shapefile inputs,
computes the Extent of Occurrence (EOO, minimum convex polygon) and the
Area of Occupancy (AOO, 2 km grid), and quantifies the percentage of
converted (anthropic) versus remaining natural habitat inside both the
EOO and the AOO using the most recent MapBiomas land-use/land-cover
collection.

## Main functions

- [`read_occurrences`](https://lucasbarreirageo.github.io/mappingAS/reference/read_occurrences.md):

  Import points from csv/xlsx/shapefile.

- [`assess_species`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md):

  Full pipeline (EOO, AOO, conversion).

- [`calc_eoo`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_eoo.md),
  [`calc_aoo`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_aoo.md):

  Range metrics only.

- [`summarise_conversion`](https://lucasbarreirageo.github.io/mappingAS/reference/summarise_conversion.md):

  Convert class areas to % converted.

- [`map_species`](https://lucasbarreirageo.github.io/mappingAS/reference/map_species.md),
  [`plot_conversion`](https://lucasbarreirageo.github.io/mappingAS/reference/plot_conversion.md):

  Visual output.

- [`run_app`](https://lucasbarreirageo.github.io/mappingAS/reference/run_app.md):

  Launch the Shiny GUI.

## MapBiomas backends

`backend = "local"` (default) reads a windowed crop of the national
GeoTIFF via GDAL /vsicurl/ (no Google Earth Engine account needed).
`backend = "gee"` uses rgee server-side reductions for very large
ranges.

## See also

Useful links:

- <https://github.com/lucasbarreirageo/mappingAS>

- Report bugs at <https://github.com/lucasbarreirageo/mappingAS/issues>

## Author

**Maintainer**: Antônio Lucas Barreira <barreira_rodrigues@hotmail.com>
\[copyright holder\]
