# mappingAS <img src="man/figures/featured_Resultado.png" align="right" height="139" alt="mappingAS hex logo" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/lucasbarreirageo/mappingAS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lucasbarreirageo/mappingAS/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20570406.svg)](https://doi.org/10.5281/zenodo.20570406)
<!-- badges: end -->

> **mappingAS** — *Mapping Area of Species.* Geographic range metrics (EOO / AOO) and MapBiomas habitat conversion for extinction-risk screening.

`mappingAS` is an R package, with an accompanying **Shiny** interface, for screening species against **Criterion B** of the IUCN Red List. Starting from a set of occurrence points, it estimates the geographic range of each species, measures how much of that range has been converted to anthropic land cover using **MapBiomas**, and packages the results for mapping, inspection and export. It is designed to feel familiar to anyone who has used [GeoCAT](https://geocat.iucnredlist.org/), while adding a habitat-conversion layer driven by Brazil's national land-cover maps.

---

## Overview

For every species in the input, `mappingAS` runs an end-to-end pipeline:

1. **Read occurrence data.** Imports records from spreadsheets (`.xlsx`, `.csv`/`.tsv`/`.txt`) and vector files (`.shp`, `.gpkg`, `.geojson`, or a zipped shapefile), auto-detecting the species, longitude and latitude columns. Brazilian-style CSVs (`;` separator, `,` decimal, optional UTF-8 BOM) are read correctly with no extra arguments.

2. **Compute the range metrics.** Estimates the **Extent of Occurrence** (EOO; minimum convex polygon) and the **Area of Occupancy** (AOO; 2 km grid), both measured on a data-centred **equal-area projection** in line with IUCN guidelines.

3. **Quantify habitat conversion.** For each species — within both the EOO and the AOO — computes the percentage of **converted (anthropic)** land cover versus **remaining natural** habitat, from the most recent MapBiomas collection (Collection 10). The headline index uses a *terrestrial* denominator — `converted (%) = anthropic / (anthropic + natural) × 100`, with water and not-observed areas excluded by default — and the full class-by-class breakdown is also provided.

4. **Build the land-cover time series.** Generates the evolution of land-cover composition (percentage of area per year) inside the EOO/AOO, mirroring the MapBiomas coverage figures.

5. **Explore interactively (Shiny).** Provides a graphical interface for mapping, browsing tables and charts, and exporting the results (CSV, shapefile / GeoPackage, and images).

> **Note.** The risk categories produced are **provisional** and intended for screening only, because they rest solely on the EOO/AOO *size* thresholds. They **do not replace** a formal IUCN assessment, which also requires the conditions of fragmentation, continuing decline and extreme fluctuation (sub-criteria a, b and c).

---

## Installation

```r
# install.packages("remotes")
remotes::install_github("lucasbarreirageo/mappingAS")
```

The core dependencies (`sf`, `terra`, `readxl`, `dplyr`, `leaflet`, `DT`, `shiny`, `bslib`) are installed automatically.

The **local** MapBiomas backend uses **GDAL with `/vsicurl/`** (shipped with `terra`/`sf`), so **no Google Earth Engine account and no full national mosaic download are needed** — only the window covering each species' range is read.

The optional Earth Engine backend requires a configured `rgee`:

```r
install.packages("rgee")
rgee::ee_install()      # creates the Python environment
rgee::ee_Initialize()   # authenticates with your Earth Engine account
```

---

## Quick start

```r
library(mappingAS)

# 1. Read points (csv / xlsx / shp) — columns are auto-detected
occ <- read_occurrences("my_occurrences.xlsx")

# If detection fails, name the columns explicitly:
# occ <- read_occurrences("data.csv",
#                         species_col = "species",
#                         lon_col = "longitude",
#                         lat_col = "latitude")

# 2. Run the full assessment (EOO, AOO and MapBiomas conversion)
res <- assess_species(occ, year = 2024, collection = 10, backend = "local")

# 3. Inspect the summary table (one row per species)
res$summary

# 4. Interactive map and conversion chart
map_species(res)        # leaflet: points + EOO + AOO (+ MapBiomas layer)
plot_conversion(res)    # bars: natural vs. converted

# 5. Export the EOO and AOO as spatial data
export_ranges(res)                    # mappingAS_EOO.shp + mappingAS_AOO.shp
export_ranges(res, format = "gpkg")   # one GeoPackage with eoo and aoo layers
export_ranges(res, zip = TRUE)        # everything bundled into a single .zip
```

### Bundled example

```r
ex  <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
occ <- read_occurrences(ex)
res <- assess_species(occ, backend = "local")
res
```

---

## The Shiny application

```r
library(mappingAS)
run_app()
```

In the app you can upload a file (`.xlsx`/`.csv`/`.shp` in a `.zip`), map the columns if needed, choose the MapBiomas **year** (1985–2024) and **collection**, the AOO **cell size**, and the **backend** (local / GEE), then **download the results** as CSV and the **EOO and AOO** as a shapefile (`.zip`) or GeoPackage (`.gpkg`). It has **Results**, **Map**, **Conversion**, **Classes**, **Time series** and **Methods** tabs, with PNG/HTML downloads for the map and charts.

---

## Exporting ranges (shapefile / GeoPackage)

`export_ranges()` writes the **EOO** and **AOO** polygons (one feature per species by default) with the area, conversion and provisional-category attributes already attached:

```r
export_ranges(res,
              dir    = "output",      # output directory
              format = "shapefile",   # or "gpkg"
              what   = "both",        # "eoo", "aoo" or "both"
              crs    = 4326,          # output CRS (WGS84 by default)
              aoo_as = "union",       # "union" (1 feature/species) or "cells"
              zip    = FALSE)         # TRUE = bundle everything into a .zip
```

Because the **ESRI Shapefile** format limits field names to 10 characters, the attribute table uses compact names:

| Field | Meaning |
|---|---|
| `species` | Species |
| `eoo_km2` / `aoo_km2` | EOO / AOO area (km²) |
| `n_cells` | Number of occupied AOO cells (AOO layer) |
| `conv_pct` | % converted (anthropic) **within that polygon** |
| `nat_pct` | % natural **within that polygon** |
| `cat_B1` / `cat_B2` | Provisional category (size only) |
| `prov_cat` | Combined provisional category |
| `mb_year`, `mb_coll` | MapBiomas year and collection |

The **GeoPackage** (`format = "gpkg"`) stores both layers (`eoo` and `aoo`) in a **single file** with no field-name limit — the most convenient option for opening in QGIS/ArcGIS.

> By default, `export_ranges()` also writes `mappingAS_classes.csv` with the **area and percentage of every MapBiomas class** inside the EOO and AOO. Turn it off with `class_csv = FALSE`.

---

## Per-class composition (every MapBiomas class)

Beyond the natural-versus-converted summary, you can obtain the **area and percentage of each MapBiomas class** within the EOO and AOO:

```r
ct <- class_table(res)   # one row per species × range (EOO/AOO) × class
head(ct)
```

Columns: `species`, `range` (EOO/AOO), `code` (pixel code), `class_pt`/`class_en`, `group` (natural / anthropic / water / other), `area_km2` and `pct` (percentage of the mapped area within that polygon). In the app, the **Classes** tab displays this table and allows a CSV download.

---

## Land-cover time series (% of area × year)

To see **change through time**, the package computes land-cover composition across several years and draws a **stacked-area chart** (% × year), in the style of the MapBiomas coverage figures:

```r
# cover series inside a species' EOO (5-year step by default, 1985–2024)
ts <- timeseries_for_species(res, species = "sp1", range = "eoo", by = "class")
plot_timeseries(ts)      # stacked-area chart with the official MapBiomas colours

# annual (slower) and by conservation group:
ts_annual <- timeseries_for_species(res, range = "eoo",
                                    years = mb_years(), by = "group")
```

Or directly on any geometry:

```r
ts <- cover_timeseries(my_geometry, years = c(1990, 2000, 2010, 2020),
                       by = "class", backend = "local")
```

Each year is read separately from MapBiomas (one GeoTIFF window per year), so an annual series across the whole collection can take a while; the default uses a 5-year step. In the app, the **Time series** tab provides the chart, the table and a CSV download, with selectors for **EOO/AOO**, **class/group** and **step (years)**.

---

## The results table

| Column | Meaning |
|---|---|
| `species` | Species name |
| `n_records`, `n_unique` | Number of records and of unique coordinates |
| `eoo_km2` | EOO in km² (`NA` if fewer than 3 unique points) |
| `aoo_km2`, `aoo_cells` | AOO in km² and number of occupied cells |
| `eoo_converted_pct` / `eoo_natural_pct` | % converted / % natural **within the EOO** |
| `aoo_converted_pct` / `aoo_natural_pct` | % converted / % natural **within the AOO** |
| `eoo_cat_B1`, `aoo_cat_B2`, `provisional_cat` | Provisional categories (size only) |
| `mapbiomas_year`, `mapbiomas_collection` | Land-cover source used |

---

## Methods in brief

- **EOO** = area of the minimum convex polygon over the points (at least 3 unique coordinates; otherwise `NA`).
- **AOO** = number of occupied 2 × 2 km cells × 4 km² (cell size configurable via `cell_km`).
- Both areas are measured on a data-centred **LAEA equal-area** projection, in line with IUCN guidelines.
- **Conversion**: MapBiomas classes are grouped into *natural*, *anthropic*, *water*, *not observed* and *other*. The headline index is `converted_pct = anthropic / (anthropic + natural) × 100` (terrestrial denominator; water and not-observed are excluded by default). Set `water_in_denominator = TRUE` to include water. Total-area versions (`*_total`) are also available in the per-species detail (`res$detail`).
- **Criterion B categories**: size thresholds — EOO < 100 / 5,000 / 20,000 km² and AOO < 10 / 500 / 2,000 km² for CR / EN / VU respectively. **Screening only.**

---

## MapBiomas backends

- **`"local"` (default)** — reads, over the network, only the *window* of the national GeoTIFF via GDAL `/vsicurl/`. No Earth Engine account and no full mosaic download. For offline or repeated use, point `src=` to a local GeoTIFF. The windowed crop is cached on disk, so the EOO read during assessment is reused for mapping and for re-runs.
- **`"gee"`** — computes the per-class areas server-side on Google Earth Engine (recommended for very large, e.g. continental, ranges). Requires `rgee::ee_Initialize()`.

---

## Caveats and limitations

- The **categories are provisional** — they exclude fragmentation, decline and fluctuation.
- The **AOO is sensitive** to grid origin/placement and to sampling effort.
- The **local backend** streams the MapBiomas GeoTIFF window over the network; for **very large (continental) ranges** this can be slow — prefer `backend = "gee"` in that case.
- **MapBiomas accuracy** varies by class, biome and year; consult the official documentation.

---

## Citation

When using this package, please also cite the underlying data and methods:

- **MapBiomas** — Projeto MapBiomas, Collection 10 of the Annual Series of Land Use and Land Cover Maps of Brazil (<https://brasil.mapbiomas.org>).
- **IUCN** — IUCN Standards and Petitions Committee. *Guidelines for Using the IUCN Red List Categories and Criteria.*
- **GeoCAT** — Bachman, S. *et al.* (2011). *Supporting Red List threat assessments with GeoCAT.* ZooKeys.

---

## Licence
MIT © Antônio Lucas Barreira Rodrigues. See the [`LICENSE`](LICENSE) file.
