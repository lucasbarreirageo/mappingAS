# mappingAS 1.5.0

* Chart legends in the Shiny app (conversion, classes, time-series and fire
  tabs) now follow the app's language selector. English is the default and is
  taken directly from the MapBiomas `class_en` column in `mb_legend()`;
  Portuguese remains available via the selector.
* Added a `lang` argument to `plot_conversion()`, `plot_timeseries()` and
  `plot_fire_timeseries()` (default `"en"`), mirroring the existing map
  functions.
* Introduced an internal single source of truth for the conservation-group
  labels and colours, so a group is no longer labelled inconsistently across
  functions (e.g. "Altered" vs "Altered (anthropic)").
* `read_occurrences()` now fails with a clear message when every row is dropped
  as having missing or invalid coordinates, instead of surfacing an opaque
  downstream error.
* Translated the remaining Portuguese runtime messages in `cover_timeseries()`
  to English for consistency with the English package interface.
* Added a test file covering legend language across all four chart tabs.

# mappingAS 1.3.2

* Publishable/exported maps now honour the AOO clip selected in the app
  (`map_static()` gained a `clip` argument), fixing the missing AOO layer.
* Removed the redundant MapBiomas "Collection" selector from the Shiny app
  (only the Collection 10 legend ships with the package).
* Moved the "Download results (CSV)" button into the Results tab.
* Fixed a syntax error in the Shiny app (a stray bracket in the fire
  time-series reactive) that stopped the app from launching; added a test
  that parses the bundled app sources to prevent regressions.
* Removed the "Download map (PNG)" button (interactive-map snapshot via
  webshot2/Chrome); the HTML download and the publishable PNG remain.
  Dropped the now-unused 'webshot2' from Suggests.
* Refactored the time-series reactives to share a `.year_grid()` helper,
  removing duplicated year-range logic.
* App interface is now fully in English. (Map legends can still be exported
  in English or Portuguese.)
* All download buttons now report failures as a notification instead of a
  raw Shiny error.

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

* Added the fire risk assessment module using MapBiomas Fire (Collection 4)
  data.
* Incorporated a new `fire = TRUE` argument into the `assess_species()` function
  to calculate the percentage of cumulative burned area within the EOO and AOO.
* Added new functions for extracting and visualizing fire timeseries:
  `fire_timeseries()`, `fire_timeseries_for_species()`, and
  `plot_fire_timeseries()`.
* Updated the static mapping function `map_static()` to include the
  `fire = TRUE` argument, enabling the visualization of the fire recurrence
  layer.
* Updated `export_ranges()` to include the burned area (`brnd_pct`) and
  MapBiomas Fire collection (`fire_col`) columns in the exported polygons
  (Shapefile/GeoPackage).

# mappingAS 1.0.3

* Maintenance release: documentation and packaging only — computed results are
  unchanged.
* README rewritten with a clearer description of the package and a corrected
  quick-start example.
* Leaner dependencies: removed the unused dplyr import. Removed the LazyData
  field (example data ships under `inst/extdata/`, not `data/`). Author and
  copyright metadata tidied for consistency.

# mappingAS 1.0.2

* `map_static()`: new publication-ready static map (MapBiomas raster clipped to
  the EOO, EOO outline, AOO cells, points, north arrow, scale bar and legends)
  on an equal-area projection; returns a ggplot for `ggplot2::ggsave()`.
* `map_species()`: MapBiomas layer on the leaflet map (clipped to the EOO), with
  a class legend and a toggle in the layers control.
* `mb_raster_local()`: on-disk cache of the windowed crop (reused across the
  EOO/AOO/map and re-runs) and restoration of the user's GDAL settings.
* Import: separator/decimal and BOM detection for Brazilian-style CSVs.
* EOO is undefined (NA) for collinear points.

# mappingAS 1.0.0

* Initial release.
* Import of occurrence points from .csv/.tsv/.txt, .xlsx/.xls and
  vector files (.shp, .gpkg, .geojson, .zip), with automatic detection
  of the species, longitude and latitude columns.
* EOO (minimum convex polygon) and AOO (2 km grid) computed on a data-centred
  equal-area (LAEA) projection.
* Habitat conversion (anthropic) versus natural within the EOO and AOO using
  MapBiomas (Collection 10), with two backends: a local windowed read
  (`/vsicurl/`, no Google Earth Engine account) and rgee (optional).