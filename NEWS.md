# mappingAS 1.9.0

* **Six more MapBiomas countries.** `assess_species()` (and the whole pipeline)
  now accepts `initiative = "argentina"`, `"bolivia"`, `"chile"`, `"ecuador"`,
  `"peru"` and `"venezuela"`, in addition to `"brazil"`, `"amazonia"` and
  `"colombia"`. All are streamed as annual Cloud-Optimized GeoTIFFs from the
  public MapBiomas bucket via GDAL `/vsicurl/` - **no Google Earth Engine and no
  Google Drive**. The registry handles each product's file layout
  (`coverage/<country>_coverage_YYYY.tif`, Peru/Venezuela's
  `..._integration_v1-classification_YYYY.tif`, Argentina's hyphenated
  `collection-2`, and Chile's collection-less `chile/coverage/` path). Year
  spans follow each product: Chile is 2000-2022 and Venezuela (Collection 2) is
  1985-2023; the others run 1985-2024. `mb_initiatives()` lists them all.
  Paraguay and Uruguay are intentionally **not** included: their annual maps are
  not published as per-year GeoTIFFs on the public bucket, so they cannot be
  streamed the same way (they would require Earth Engine).
* **Standardised legend extended to all of South America.** `mb_legend()` gains
  the country-specific classes from the pan-continental harmonisation table -
  primary/secondary/dwarf forest (59, 60, 67), scrubland/open shrublands/steppe/
  fog oasis/peatlands (66, 77, 63, 70, 73), other crops (72), Pinus/Eucalyptus/
  other forest plantations (79, 80, 83) and salt flat (61) - so a range that
  spans several countries is still assessed on one coherent legend. The existing
  Brazil/Amazonia/Colombia codes, names, colours and groups are unchanged; a
  code absent from a given raster contributes zero area.
* **Protected areas: WDPA only.** The Brazilian ICMBio/SNUC Conservation-Unit
  source has been removed - the exported `icmbio_wfs_base()` and
  `protected_layers()`, the WFS reader, and the bundled `ucs_federais.rds` data
  are gone. Protected-area overlap (`assess_species(protected = TRUE)`) now
  always uses the global **World Database on Protected Areas (WDPA)**, so it
  works the same way everywhere; a local `pa_src` file still overrides it for
  offline use. `assess_species()` drops the `pa_source`/`pa_typename` arguments,
  and `protected_areas()` now reads WDPA (or a local file) instead of the ICMBio
  WFS. The Shiny app drops the protected-area source selector and lists all nine
  initiatives, and the written report cites WDPA instead of ICMBio/INDE.

# mappingAS 1.8.1

* **MapBiomas beyond Brazil: Pan-Amazon and Colombia.** `assess_species()` (and
  the whole pipeline) gains an `initiative` argument - `"brazil"` (default),
  `"amazonia"` (Pan-Amazon / RAISG, Collection 6, 1986-2023) or `"colombia"`
  (Collection 3, 1985-2024). All three products are streamed as annual
  Cloud-Optimized GeoTIFFs from the public MapBiomas bucket via GDAL
  `/vsicurl/` - **no Google Earth Engine and no Google Drive download**. Colombia
  is served per-year on the same public bucket
  (`initiatives/colombia/collection_3/coverage/`), so it reads exactly like
  Brazil (windowed, cached) rather than through the multi-band Drive archive. A
  new exported `mb_initiatives()` lists the products, their default collections
  and year spans; `mb_source_url()`, `mb_raster_local()`, `mb_years()`,
  `mb_legend()`, `summarise_conversion()` and `cover_timeseries()` all take the
  `initiative` argument. `year`/`collection` now default to the initiative's
  latest year and native collection when left `NULL`.
* **Standardised pan-MapBiomas legend.** `mb_legend()` now returns a single
  standardised legend that labels the Brazil, Amazonia and Colombia rasters
  consistently (same class names, colours and conservation groups), built from
  the cross-product harmonisation table. The Colombia/Amazonia-specific classes
  are added - Andinean herbaceous/shrubby formations (81, 82), Glacier (34),
  Other natural non-vegetated area (68) and Banana (74) - so a species whose
  range spans more than one country is assessed on one coherent legend. Existing
  Brazil codes, names, colours and groups are unchanged.
* **Global protected areas via WDPA.** New `wdpa_areas()` reads the World
  Database on Protected Areas from its public ArcGIS FeatureServer (bounding-box
  query, GeoJSON, on-disk cached), standardised to the same
  `pa_name`/`pa_category`/`pa_group` columns as the ICMBio layer, with IUCN
  categories mapped to strict-protection (Ia-III) vs sustainable-use (IV-VI).
  `assess_species()` gains `pa_source` (`"icmbio"` or `"wdpa"`); it defaults to
  ICMBio for Brazil and WDPA for the Amazonia/Colombia initiatives, so
  protected-area overlap now works outside Brazil.
* The assessment `summary` gains a `mapbiomas_initiative` column and the stored
  `settings` record the `initiative` and `pa_source`; the Shiny app adds an
  **initiative** selector (with the Year list auto-updating to the product's
  span) and a **protected-area source** selector, and threads both through the
  map overlay, static map, time series and raster export. MapBiomas Fire remains
  Brazil-only and is skipped with a warning for the other initiatives.

# mappingAS 1.8.0

* **MapBiomas composition donut charts.** The Conversion tab now shows, below
  the natural/altered bar, twin donut (ring) charts of the MapBiomas
  composition inside the EOO and AOO - switchable between the full per-class
  breakdown (`By class`) and the conservation-group summary (`By group`) - in
  the official MapBiomas colours. New exported `plot_conversion_donut()`
  function; both the interactive (plotly) view and the `Save donut` button
  export a transparent-background PNG.
* **Results tab redesign.** Beyond the table, the Results tab now shows a
  visual overview for the selected species — stat cards with in-cell bars for
  EOO/AOO, provisional category badges, converted/natural, burned and
  protected-area percentages — plus a collapsible **column glossary** explaining
  every field (bilingual EN/PT).
* **Export MapBiomas rasters.** The Map tab can now download the MapBiomas
  **land-use** and/or **fire (accumulated)** GeoTIFF rasters, clipped to the
  selected species' EOO or AOO, bundled as a `.zip`.
* **Faster remote reads.** Tuned the GDAL `/vsicurl/` configuration (bigger
  block cache, HTTP/2 multiplexing, larger chunked range requests, threaded
  decompression) to speed up the MapBiomas streaming reads during assessment.
  This only affects I/O speed — pixel values and area statistics are unchanged.
* **Written assessment report.** New `assessment_report()` builds a narrative,
  referenced summary of a species' Criterion B screening (range metrics,
  provisional category, and — when computed — habitat conversion, fire and
  protected-area overlap), with the IUCN/GeoCAT/MapBiomas references. It
  returns HTML, plain text, or a Word `.docx` (via \pkg{officer}). A new
  **Report** tab in the Shiny app (after *Fire*) previews the text and downloads
  the `.docx`.
* The report is now **interpretive**, not just a restatement of numbers: it
  discusses the EOO/AOO composition divergence, the *effectiveness* of the
  protected-area overlap (strict-protection vs sustainable-use units and the
  concentration of occurrences in few units), and frames continuing decline
  (subcriterion b) honestly at the level of habitat quality. New
  `cover_series`/`fire_series` arguments add a temporal-trend analysis of
  conversion and of the fire regime; the app passes these automatically when
  the matching Time series / Fire series have been calculated.
* The temporal analysis covers **both the EOO and the AOO** and is now
  **accumulated passively**: `cover_series`/`fire_series` accept a list of
  per-range series (each reported separately, not summed), and the app's Report
  tab collects every Time series / Fire series you calculate for the species
  (EOO or AOO) — no dedicated button and no extra computation on the Report tab.
  With `figures = TRUE` (used by the app's `.docx` download), the report also
  **embeds the support figures** (composition, protection, and the
  land-cover/fire time series) via \pkg{officer}.
* **Faster map overlay.** The MapBiomas layer drawn on the interactive map is
  now read *decimated* via GDAL (`-outsize`, using the raster overviews) instead
  of streaming the full 30 m window, which markedly speeds up the map for
  large-range species. It falls back to the previous native read on any error,
  and area/conversion statistics are unaffected (they still use the native read).
* **Interactive charts.** The conversion, protection, land-cover and fire
  time-series charts are now interactive in the Shiny app (hover tooltips,
  zoom, pan) via a new exported helper `mas_plotly()`, which wraps any
  mappingAS `plot_*` ggplot into a \pkg{plotly} widget and preserves the chart
  subtitle in the title. `plot_conversion()` and `plot_protection()` now return
  a `ggplot` object (with the percentage matrix kept in `attr(p, "pct")`); a
  base-graphics fallback remains when \pkg{ggplot2} is unavailable.
* All four charts share a single minimal theme (`.mas_theme()`) so the static
  PNG exports and their interactive versions look consistent.
* `ggplot2` and `plotly` moved to `Imports` (previously `ggplot2` was a
  suggestion); PNG downloads of the conversion/protection charts now use
  `ggplot2::ggsave()`.
* **Prettier tables.** Every table in the Shiny app now uses a shared style
  (compact striped rows, numerics rounded to two decimals, and an in-cell
  colour bar on percentage columns) for quicker reading.

# mappingAS 1.7.0

* Federal Conservation Units are now read from a bundled copy
  (`inst/extdata/ucs_federais.rds`, full resolution) by default, so overlap works
  offline and does not depend on the (intermittent) ICMBio WFS. The WFS remains
  available and is used automatically as a fallback when the bundled data is not
  installed. Pass `pa_src=` to use your own UC file. Source: ICMBio/INDE (PDDL).
* `assess_species(protected = TRUE, mapbiomas = TRUE)` reports the natural
  habitat that is *also* inside UCs (effectively protected): `eoo_nat_uc_pct` /
  `aoo_nat_uc_pct` (of the whole range) and `eoo_nat_uc_pct_in` /
  `aoo_nat_uc_pct_in` (of the UC area). New `plot_protection()` charts this.
* Fixed `plot_protection()` failing to render in the Shiny "Conservation Units"
  tab on small plot areas ("invalid graphics state"): leaner margins, resilient
  margin annotations, and a taller plot panel.

# mappingAS 1.6.0

* New module `R/protected_areas.R` integrating Brazilian federal Conservation
  Units (Unidades de Conservacao, UCs) from the ICMBio geoservice on the INDE.
  Added `protected_areas()` (reads UCs intersecting an area of interest from the
  WFS, with a local-file fallback and on-disk cache), `protected_layers()`
  (lists the WFS `typeName`s), `summarise_protected()` (overlap metrics),
  `pa_table()` (per-species UC list) and `icmbio_wfs_base()`.
* `assess_species()` gains `protected`, `pa_src` and `pa_typename`. With
  `protected = TRUE` the summary now reports `occ_in_uc_pct` (share of
  occurrences inside UCs), `eoo_uc_pct`, `aoo_uc_pct` and `n_uc`, and each
  species' `detail` stores the full UC overlap and layer.
* `map_species()` and `map_static()` gain `protected`/`pa_src` to draw the UC
  polygons as a labelled layer.
* New `plot_protection()`: horizontal stacked bars (EOO and AOO) of the range
  inside vs outside UCs, mirroring `plot_conversion()`.
* With `protected = TRUE` and `mapbiomas = TRUE`, `assess_species()` also
  reports the natural habitat that is *also* inside UCs (effectively protected):
  `eoo_nat_uc_pct`/`aoo_nat_uc_pct` (of the whole range) and
  `eoo_nat_uc_pct_in`/`aoo_nat_uc_pct_in` (of the UC area). `plot_protection()`
  splits the inside-UC bar into natural vs altered when these are present.
* `export_ranges()` writes the UC fields (`uc_pct`, `uc_occ_pct`, `n_uc`) into
  the EOO/AOO attribute tables when available.
* Shiny app: a "Overlap with Conservation Units (ICMBio)" option and a new
  "Conservation Units" tab (headline cards, protection chart, per-UC table and
  CSV/PNG downloads); the UC layer is added to the interactive map.

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