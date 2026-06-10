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
