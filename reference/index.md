# Package index

## Shiny application

The interactive app that ties the whole workflow together — upload
points, compute EOO/AOO and habitat conversion, explore and export. The
recommended entry point for most users.

- [`run_app()`](https://lucasbarreirageo.github.io/mappingAS/reference/run_app.md)
  : Launch the mappingAS Shiny application

## Occurrence data

Read and prepare occurrence points.

- [`read_occurrences()`](https://lucasbarreirageo.github.io/mappingAS/reference/read_occurrences.md)
  : Read species occurrence points from XLSX, CSV or vector files
- [`vouchers_from_occ()`](https://lucasbarreirageo.github.io/mappingAS/reference/vouchers_from_occ.md)
  : Build examined-voucher strings from occurrence attributes

## One-call assessment

Run the whole pipeline for one or many species.

- [`assess_species()`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md)
  : Assess one or many species: EOO, AOO, MapBiomas conversion and fire
- [`class_table()`](https://lucasbarreirageo.github.io/mappingAS/reference/class_table.md)
  : Per-class MapBiomas composition table (every class) for an
  assessment

## Range metrics & Criterion B

Extent of Occurrence, Area of Occupancy and the provisional Criterion B
categories.

- [`calc_eoo()`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_eoo.md)
  : Extent of Occurrence (EOO) via minimum convex polygon

- [`calc_aoo()`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_aoo.md)
  : Area of Occupancy (AOO) via occupied grid cells

- [`aoo_bounds()`](https://lucasbarreirageo.github.io/mappingAS/reference/aoo_bounds.md)
  : Lower/upper Area of Occupancy bounds and the Criterion B2 category
  range

- [`iucn_category_B()`](https://lucasbarreirageo.github.io/mappingAS/reference/iucn_category_B.md)
  : Provisional IUCN Red List Criterion B category from range size

- [`iucn_category_B_range()`](https://lucasbarreirageo.github.io/mappingAS/reference/iucn_category_B_range.md)
  :

  Criterion B *size* category range under AOO uncertainty

- [`iucn_criterion_B()`](https://lucasbarreirageo.github.io/mappingAS/reference/iucn_criterion_B.md)
  : Apply IUCN Red List Criterion B (size thresholds plus sub-criteria)

- [`calc_locations()`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_locations.md)
  : Estimate of number of locations (occupied-grid-cell method,
  optionally protected-area aware)

- [`calc_subpop()`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_subpop.md)
  : Estimate of number of subpopulations (circular-buffer method)

## Habitat conversion (MapBiomas)

Converted vs natural land cover within the range, per-class and over
time.

- [`summarise_conversion()`](https://lucasbarreirageo.github.io/mappingAS/reference/summarise_conversion.md)
  : Summarise habitat conversion from MapBiomas class areas
- [`cover_timeseries()`](https://lucasbarreirageo.github.io/mappingAS/reference/cover_timeseries.md)
  : MapBiomas land-cover composition over time for a polygon
- [`timeseries_for_species()`](https://lucasbarreirageo.github.io/mappingAS/reference/timeseries_for_species.md)
  : Land-cover time series for one assessed species (EOO or AOO)
- [`mb_class_areas_raster()`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_class_areas_raster.md)
  : Tabulate area per MapBiomas class from a cropped raster
- [`mb_class_areas_gee()`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_class_areas_gee.md)
  : MapBiomas class areas via Google Earth Engine (optional backend)
- [`mb_raster_local()`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_raster_local.md)
  : Crop a MapBiomas LULC raster to an area of interest (local backend)
- [`mb_source_url()`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_source_url.md)
  : Build the MapBiomas land-use GeoTIFF URL for a given year and
  initiative
- [`mb_initiatives()`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_initiatives.md)
  : MapBiomas initiatives supported by mappingAS
- [`mb_groups()`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_groups.md)
  : Group lookup vectors for MapBiomas conservation groups
- [`mb_legend()`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_legend.md)
  : Standardised MapBiomas legend with conservation groupings
- [`mb_palette()`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_palette.md)
  : Official MapBiomas colours for a set of class codes
- [`mb_years()`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_years.md)
  : Years available for a MapBiomas initiative/collection

## Area of Habitat & elevation

Elevation-refined suitable habitat within the range.

- [`calc_aoh()`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_aoh.md)
  : Area of Habitat (AOH): suitable land cover within a range and
  elevation band
- [`map_aoh()`](https://lucasbarreirageo.github.io/mappingAS/reference/map_aoh.md)
  : Interactive Area of Habitat (AOH) map for one species
- [`elevation_preferences()`](https://lucasbarreirageo.github.io/mappingAS/reference/elevation_preferences.md)
  : Suggest elevation preferences from the occurrences' elevation

## Fragmentation

Subpopulation clustering and severe-fragmentation screening.

- [`assess_fragmentation()`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_fragmentation.md)
  : Assess severe fragmentation from occurrences and an isolation
  distance
- [`fragment_habitat()`](https://lucasbarreirageo.github.io/mappingAS/reference/fragment_habitat.md)
  : Severe fragmentation from suitable-habitat patches and population
  density

## Fire (MapBiomas Fire)

Burned-area metrics and fire time series.

- [`summarise_fire()`](https://lucasbarreirageo.github.io/mappingAS/reference/summarise_fire.md)
  : Summarise fire pressure inside a range
- [`fire_areas()`](https://lucasbarreirageo.github.io/mappingAS/reference/fire_areas.md)
  : Area (km^2) per fire value from a cropped fire raster
- [`fire_timeseries()`](https://lucasbarreirageo.github.io/mappingAS/reference/fire_timeseries.md)
  : Burned-area time series for a polygon (% of range x year)
- [`fire_timeseries_for_species()`](https://lucasbarreirageo.github.io/mappingAS/reference/fire_timeseries_for_species.md)
  : Burned-area time series for one assessed species (EOO or AOO)
- [`fire_raster_local()`](https://lucasbarreirageo.github.io/mappingAS/reference/fire_raster_local.md)
  : Crop a MapBiomas Fire raster to an area of interest (local backend)
- [`fire_palette()`](https://lucasbarreirageo.github.io/mappingAS/reference/fire_palette.md)
  : Sequential palette for fire frequency (YlOrRd)
- [`mb_fire_url()`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_fire_url.md)
  : Build a MapBiomas Fire (Fogo) GeoTIFF URL

## Protected areas (WDPA)

Overlap of the range with the global World Database on Protected Areas.

- [`protected_areas()`](https://lucasbarreirageo.github.io/mappingAS/reference/protected_areas.md)
  : Read Protected areas intersecting an area of interest
- [`summarise_protected()`](https://lucasbarreirageo.github.io/mappingAS/reference/summarise_protected.md)
  : Overlap between a species' range and federal Protected areas
- [`pa_table()`](https://lucasbarreirageo.github.io/mappingAS/reference/pa_table.md)
  : Per-species table of Protected areas overlapping the range
- [`wdpa_areas()`](https://lucasbarreirageo.github.io/mappingAS/reference/wdpa_areas.md)
  : Read World Database on Protected Areas polygons intersecting an area
- [`wdpa_query_url()`](https://lucasbarreirageo.github.io/mappingAS/reference/wdpa_query_url.md)
  : Default WDPA ArcGIS FeatureServer query endpoint

## Global land cover (Sentinel-2 / Esri)

Fallback 10 m land cover for ranges outside MapBiomas coverage.

- [`s2_raster_local()`](https://lucasbarreirageo.github.io/mappingAS/reference/s2_raster_local.md)
  : Crop a Sentinel-2 / Esri LULC mosaic to an area of interest
- [`s2_source_url()`](https://lucasbarreirageo.github.io/mappingAS/reference/s2_source_url.md)
  : Build the Sentinel-2 / Esri land-cover GeoTIFF URL for a tile and
  year
- [`s2_years()`](https://lucasbarreirageo.github.io/mappingAS/reference/s2_years.md)
  : Years available for the global Sentinel-2 / Esri land-cover series
- [`esri_legend()`](https://lucasbarreirageo.github.io/mappingAS/reference/esri_legend.md)
  : Sentinel-2 / Esri global 10 m land-cover legend

## Countries & realm

Countries of occurrence and biogeographical realm.

- [`countries_of_occurrence()`](https://lucasbarreirageo.github.io/mappingAS/reference/countries_of_occurrence.md)
  : Countries of occurrence and biogeographical realm(s) from a range

## Maps & charts

Interactive and publication-ready outputs.

- [`map_species()`](https://lucasbarreirageo.github.io/mappingAS/reference/map_species.md)
  : Interactive map of a species' points, EOO and AOO (MapBiomas + fire)
- [`map_static()`](https://lucasbarreirageo.github.io/mappingAS/reference/map_static.md)
  : Publication-ready static map of a species (MapBiomas + EOO + AOO +
  points)
- [`mas_plotly()`](https://lucasbarreirageo.github.io/mappingAS/reference/mas_plotly.md)
  : Turn a mappingAS chart into an interactive plotly widget
- [`plot_conversion()`](https://lucasbarreirageo.github.io/mappingAS/reference/plot_conversion.md)
  : Habitat composition chart (natural / altered / water / other) for
  EOO and AOO
- [`plot_conversion_donut()`](https://lucasbarreirageo.github.io/mappingAS/reference/plot_conversion_donut.md)
  : MapBiomas composition donut charts (EOO and AOO)
- [`plot_timeseries()`](https://lucasbarreirageo.github.io/mappingAS/reference/plot_timeseries.md)
  : Stacked-area chart of land-cover percentage over time
- [`plot_class_trendline()`](https://lucasbarreirageo.github.io/mappingAS/reference/plot_class_trendline.md)
  : Regression trendline of one land-cover class through time
- [`plot_fire_timeseries()`](https://lucasbarreirageo.github.io/mappingAS/reference/plot_fire_timeseries.md)
  : Bar/line chart of burned area (%) over time
- [`plot_protection()`](https://lucasbarreirageo.github.io/mappingAS/reference/plot_protection.md)
  : Protection chart (inside vs outside Protected areas) for EOO and AOO

## Report & export

Written assessment report, factsheet and spatial/raster exports.

- [`assessment_report()`](https://lucasbarreirageo.github.io/mappingAS/reference/assessment_report.md)
  : Narrative conservation-assessment report for one species
- [`factsheet_html()`](https://lucasbarreirageo.github.io/mappingAS/reference/factsheet_html.md)
  : Standalone HTML species factsheet with user metadata and photos
- [`export_ranges()`](https://lucasbarreirageo.github.io/mappingAS/reference/export_ranges.md)
  : Export EOO and AOO as spatial files (shapefile or GeoPackage)

## Utilities

Projection helpers and the package overview.

- [`laea_crs()`](https://lucasbarreirageo.github.io/mappingAS/reference/laea_crs.md)
  : Build a data-centred equal-area CRS (Lambert Azimuthal Equal Area)
- [`mappingAS`](https://lucasbarreirageo.github.io/mappingAS/reference/mappingAS-package.md)
  [`mappingAS-package`](https://lucasbarreirageo.github.io/mappingAS/reference/mappingAS-package.md)
  : mappingAS: Range Metrics and MapBiomas Habitat Conversion
