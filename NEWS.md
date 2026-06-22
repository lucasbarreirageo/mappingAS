# mappingAS 1.4.0

* Multi-country MapBiomas support for South America. `assess_species()` now
  detects which MapBiomas initiative each species spans (Brazil, Argentina,
  Bolivia, Ecuador, Peru) and reads the matching national raster(s); the new
  `multicountry` and `countries` arguments control this (defaults: auto-detect
  on the local backend).
* Transboundary species are handled by reading each country's windowed crop and
  mosaicking them into a single seamless raster covering the whole range
  (`mb_raster_multicountry()`), with per-country failures warned and skipped.
* Legend harmonisation: foreign pixel codes are reclassified to the Brazil
  legend vocabulary on read (`mb_harmonise_raster()`), so `summarise_conversion()`,
  `class_table()`, the time series and every plot keep working unchanged. The
  reclassification preserves each class's conservation group
  (natural / anthropic / water / other).
* New country registry as the single source of truth for URLs, collection
  numbers and reclassification tables (`mb_countries()`, `mb_country_info()`).
* `mb_source_url()` gained a `country` argument; new `mb_raster_country()` reads
  and harmonises one country's window.
* Point-in-country detection (`country_of_points()`, `species_countries()`)
  using an embedded borders layer (`inst/extdata/sa_borders.gpkg`), with a
  bounding-box fallback when the layer is absent.
* `mb_legend()`: added code 34 (Glacier) to the `water` group, so glaciers in
  the Andean initiatives are not dropped from the conversion denominator.


# mappingAS 1.3.2

* Publishable/exported maps now honour the AOO clip selected in the app
  (`map_static()` gained a `clip` argument), fixing the missing AOO layer.
* Removed the redundant MapBiomas "Collection" selector from the Shiny app
  (only the Collection 10 legend ships with the package).
* Moved the "Download results (CSV)" button into the Results tab.


# mappingAS 1.3.1

* Shiny app: language selector (English/Português) for the MapBiomas land-cover
  and fire-frequency legends, on both the interactive (`map_species()`) and
  publishable (`map_static()`) maps.
* Shiny app: IUCN-style badges (EOO/B1 and AOO/B2) with the official Red List
  colours shown beside the map, reflecting each species' provisional category.
* `map_species()`: new `clip` argument to view the MapBiomas land-cover and fire
  rasters clipped to the AOO as well as the EOO; selectable in the Map tab.
* Publishable map: the geographic-reference label now shows the actual CRS
  (projection + datum) instead of fixed wording.
* Fixed the "Publishable map (PNG)" download in the app (the handler now reports
  errors and validates the plot before saving).
* Silenced the `st_union` planar-assumption warnings when dissolving AOO cells.
* Reworked the app's Methods tab with key references and the MapBiomas Land
  Cover and MapBiomas Fire data sources.



# mappingAS 1.2.0

* Added the fire risk assessment module using MapBiomas Fire (Collection 4) data.
* Incorporated a new `fire = TRUE` argument into the `assess_species()` function to calculate the percentage of cumulative burned area within the EOO and AOO.
* Added new functions for extracting and visualizing fire timeseries: `fire_timeseries()`, `fire_timeseries_for_species()`, and `plot_fire_timeseries()`.
* Updated the static mapping function `map_static()` to include the `fire = TRUE` argument, enabling the visualization of the fire recurrence layer.
* Updated `export_ranges()` to include the burned area (`brnd_pct`) and MapBiomas Fire collection (`fire_col`) columns in the exported polygons (Shapefile/GeoPackage).

# mappingAS 1.0.3

* Maintenance release: documentation and packaging only — computed results are
  unchanged.
* README rewritten with a clearer description of the package and a corrected
  quick-start example.
* Leaner dependencies: removed the unused `dplyr` import. Removed the `LazyData`
  field (example data ships under `inst/extdata/`, not `data/`). Author and
  copyright metadata tidied for consistency.

# mappingAS 1.0.2

* `map_static()`: new publication-ready static map (MapBiomas raster clipped to
  the EOO, EOO outline, AOO cells, points, north arrow, scale bar and legends)
  on an equal-area projection; returns a `ggplot` for `ggplot2::ggsave()`.
* `map_species()`: MapBiomas layer on the leaflet map (clipped to the EOO), with
  a class legend and a toggle in the layers control.
* `mb_raster_local()`: on-disk cache of the windowed crop (reused across the
  EOO/AOO/map and re-runs) and restoration of the user's GDAL settings.
* Import: separator/decimal and BOM detection for Brazilian-style CSVs.
* EOO is undefined (`NA`) for collinear points.

# mappingAS 1.0.0

* Initial release.
* Import of occurrence points from `.csv`/`.tsv`/`.txt`, `.xlsx`/`.xls` and
  vector files (`.shp`, `.gpkg`, `.geojson`, `.zip`), with automatic detection
  of the species, longitude and latitude columns.
* EOO (minimum convex polygon) and AOO (2 km grid) computed on a data-centred
  equal-area (LAEA) projection.
* Habitat conversion (anthropic) versus natural within the EOO and AOO using
  MapBiomas (Collection 10), with two backends: a local windowed read
  (`/vsicurl/`, no Google Earth Engine account) and `rgee` (optional).
* Provisional Criterion B category (EOO/AOO size thresholds only), flagged as
  screening.
* Visualisations: interactive map (`leaflet`) and conversion chart.
* Per-class breakdown (`class_table()`): area and % of every MapBiomas class
  within the EOO and AOO; also written to CSV by `export_ranges()`
  (`class_csv = TRUE`) and shown in the app's "Classes" tab.
* Land-cover time series (`cover_timeseries()`, `timeseries_for_species()`) and a
  stacked-area chart (% × year) with the official MapBiomas colours
  (`plot_timeseries()`); new "Time series" tab in the app. The series is
  "rectangularised" (every class in every year, 0 where absent), fixing a
  `ggplot2::geom_area()` failure when a class was missing in some years.
* Conversion chart (`plot_conversion()`) reworked: horizontal stacked bars
  (natural/altered/water/other) with inner labels and the legend outside the
  plot area, so nothing overlaps.
* Official legend colours and the `mb_palette()` and `mb_years()` helpers.
* Export of the EOO and AOO as a shapefile or GeoPackage (`export_ranges()`),
  with area, % converted/natural and provisional category in the attribute
  table; matching download button in the Shiny app.
* Shiny application (`run_app()`) for interactive use, with buttons to save the
  map (HTML; PNG via `webshot2`) and the conversion and time-series charts as
  images (PNG).
* `man/*.Rd` documentation, a vignette (`vignette("mappingAS")`) and a GitHub
  Actions workflow (`R CMD check` on Linux/macOS/Windows).
* Offline tests with `testthat`.
