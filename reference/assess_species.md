# Assess one or many species: EOO, AOO, MapBiomas conversion and fire

End-to-end pipeline. For each species it computes the EOO (minimum
convex polygon) and AOO (2 km grid), then quantifies the percentage of
converted (anthropic) and remaining natural habitat from the most recent
MapBiomas collection inside both the EOO and the AOO. When `fire = TRUE`
it also adds the percentage of the EOO/AOO that has burned and the mean
fire recurrence, from the MapBiomas Fire accumulated layer.

## Usage

``` r
assess_species(
  occ,
  initiative = "brazil",
  year = NULL,
  collection = NULL,
  backend = c("local", "gee"),
  fallback = c("sentinel2", "none"),
  cell_km = 2,
  subpop = TRUE,
  subpop_resol_km = NULL,
  loc_km = 10,
  loc_scale = NULL,
  loc_method = c("no_more_than_one", "other"),
  mapbiomas = TRUE,
  fire = FALSE,
  fire_collection = 4,
  fire_host_collection = 9,
  protected = FALSE,
  pa_src = NULL,
  src = NULL,
  fire_src = NULL,
  water_in_denominator = FALSE,
  area_max_pixels = 5e+07,
  decline = NA,
  extreme_fluctuation = NA,
  severe_fragmentation = NA,
  assume_decline = TRUE,
  infer_decline_from_habitat = TRUE,
  decline_years = 10,
  min_records = 1,
  verbose = TRUE
)
```

## Arguments

- occ:

  An `sf` of POINT geometries from
  [`read_occurrences`](https://lucasbarreirageo.github.io/mappingAS/reference/read_occurrences.md).

- initiative:

  Land-cover product. A MapBiomas initiative - `"brazil"` (default),
  `"amazonia"` (Pan-Amazon / RAISG), `"colombia"` or any of the other
  South-American products (Argentina, Bolivia, Chile, Ecuador, Peru,
  Venezuela, Paraguay, Uruguay) - selects that product, its default
  collection and the standardised legend (see
  [`mb_initiatives`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_initiatives.md)).
  Two extra keys unlock coverage anywhere on Earth: `"sentinel2"` forces
  the global Esri / Impact Observatory 10 m land cover (Sentinel-2, the
  data behind the ArcGIS Living Atlas Land Cover Explorer; see
  [`esri_legend`](https://lucasbarreirageo.github.io/mappingAS/reference/esri_legend.md)),
  and `"auto"` tries MapBiomas first and automatically falls back to
  Sentinel-2 for ranges outside MapBiomas coverage. All backends stream
  public Cloud-Optimized GeoTIFFs - no Google Earth Engine and no
  account.

- year:

  Integer MapBiomas year. `NULL` (default) uses the most recent year
  available for the initiative (e.g. 2024 for Brazil/Colombia, 2023 for
  Amazonia/Paraguay, 2022 for Uruguay).

- collection:

  Integer MapBiomas collection number. `NULL` (default) uses the
  initiative default (10/6/3 for Brazil/Amazonia/Colombia).

- backend:

  Habitat backend: `"local"` (default) or `"gee"`. The `"gee"` backend
  is only wired for `initiative = "brazil"`.

- fallback:

  What to do when the chosen MapBiomas product has no data over a
  species' range (i.e. the occurrences fall outside every supported
  country): `"sentinel2"` (default) recomputes that species with the
  global Sentinel-2 / Esri layer; `"none"` leaves its conversion as
  `NA`. Ignored when `initiative = "sentinel2"`.

- cell_km:

  AOO grid cell size in km (default `2`).

- subpop:

  Logical; if `TRUE` (default) also estimate the number of
  subpopulations (circular-buffer method) and the number of locations
  (occupied-grid-cell method). See
  [`calc_subpop`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_subpop.md)
  and
  [`calc_locations`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_locations.md).

- subpop_resol_km:

  Circular-buffer radius in km for the subpopulation estimate. `NULL`
  (default) uses one tenth of the maximum distance between occurrences
  (Rivers *et al.* 2010).

- loc_km:

  Grid cell size in km for the number-of-locations estimate (default
  `10`).

- loc_scale:

  Optional sliding-scale cell size for the locations estimate: a
  fraction of the maximum distance between occurrences (Rivers *et al.*
  2010). When supplied it overrides `loc_km`. See
  [`calc_locations`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_locations.md).

- loc_method:

  How protected areas enter the number-of-locations estimate when
  `protected = TRUE`: `"no_more_than_one"` (default; each protected area
  with occurrences is one location) or `"other"` (inside and outside
  protected areas gridded separately). Ignored when `protected = FALSE`.
  See
  [`calc_locations`](https://lucasbarreirageo.github.io/mappingAS/reference/calc_locations.md).

- mapbiomas:

  Logical; if `FALSE`, only EOO/AOO are computed.

- fire:

  Logical; if `TRUE`, also compute burned-area metrics from MapBiomas
  Fire (accumulated layer, read locally). Default `FALSE`.

- fire_collection, fire_host_collection:

  Fire collection number (default `4`) and the initiative folder hosting
  it (default `9`).

- protected:

  Logical; if `TRUE`, also compute the overlap of the range with
  protected areas from the global World Database on Protected Areas
  (WDPA): the share of occurrences inside protected areas and the \\ the
  EOO/AOO within them. Default `FALSE`. Requires internet unless
  `pa_src` is given. See
  [`wdpa_areas`](https://lucasbarreirageo.github.io/mappingAS/reference/wdpa_areas.md).

- pa_src:

  Optional local protected-area vector file (`.shp`/ `.gpkg`/`.geojson`)
  used instead of WDPA (offline).

- src:

  Optional MapBiomas LULC GeoTIFF for the local backend.

- fire_src:

  Optional MapBiomas Fire GeoTIFF (overrides the public URL).

- water_in_denominator:

  Passed to
  [`summarise_conversion`](https://lucasbarreirageo.github.io/mappingAS/reference/summarise_conversion.md).

- area_max_pixels:

  Pixel budget for the land-cover *area* read on the local backend
  (default `5e7`, ~50 million). Ranges whose native window fits the
  budget are read at full resolution (30 m MapBiomas, 10 m Sentinel-2);
  larger or widely-spread ranges are read per polygon part and/or
  decimated with majority resampling so a continental EOO no longer
  streams billions of pixels. Class proportions - and thus the %
  converted / natural - are preserved. Raise it for finer reads of huge
  ranges, lower it for speed. Ignored by the GEE backend (reduction is
  server-side).

- decline, extreme_fluctuation, severe_fragmentation:

  Expert inputs for the Criterion B sub-criteria, applied by
  [`iucn_criterion_B`](https://lucasbarreirageo.github.io/mappingAS/reference/iucn_criterion_B.md)
  on top of the size thresholds to yield an applicable category
  (`category_B` and `criterion_B_code` in the summary). `decline` is
  sub-criterion (b) (a continuing decline), `extreme_fluctuation` is
  (c), and `severe_fragmentation` feeds (a) alongside the estimated
  number of locations, which *caps* the category (a small range spread
  over many locations is held to the less threatened level its location
  count supports; see
  [`iucn_criterion_B`](https://lucasbarreirageo.github.io/mappingAS/reference/iucn_criterion_B.md)).
  Each may be a single value applied to every species, or a named
  logical vector keyed by species name. `NA` (default) means *not
  documented*: (a)/(c) are then treated as not met, while an
  undocumented decline (b) is resolved by `assume_decline`. Sub-criteria
  (b)/(c) cannot be inferred from occurrence points, hence they are
  inputs, not computed. DD/NE are never assigned automatically.

- assume_decline:

  Logical; how an undocumented continuing decline (`decline = NA`) is
  treated by
  [`iucn_criterion_B`](https://lucasbarreirageo.github.io/mappingAS/reference/iucn_criterion_B.md).
  `TRUE` (default) assumes a decline is present - as ConR does, and as
  the habitat-conversion data support (sub-criterion b(iii)) - so a
  small-range, few-location taxon is reported at its CR/EN/VU
  size-and-location level instead of collapsing to **NT**. `FALSE`
  restores the strict screening where an undocumented decline is not met
  (NT until a decline is supplied). The `decline_assumed` column flags
  each species for which (b) was assumed rather than documented.

- infer_decline_from_habitat:

  Logical; when `TRUE` (default) and `decline` is not documented for a
  species, a continuing decline (b) is *inferred from the recent trend
  of natural habitat*: the natural area of the range (EOO, or AOO when
  the EOO is undefined) is measured at six years spanning the last
  `decline_years` and a decline is inferred only when the least-squares
  slope is negative and significant (\\p \< 0.05\\). The IUCN guidelines
  (v16, sect. 4.6) allow a continuing decline in the area/quality of
  habitat (b(iii)) to be inferred from the rate of habitat loss, at any
  rate, but the loss must be ongoing: a static share of converted land
  is past loss and is no longer used. The slope (\\
  `habitat_trend_pct_yr` and `habitat_trend_p`. The trend needs the
  multi-year public products, so it is skipped when a single-year `src`
  file is given. This is recorded as
  `decline_basis = "inferred (habitat loss)"` - distinct from
  `"assumed"` (the ConR-style global fallback used when no conversion is
  available), `"documented"` / `"documented (absent)"` (an explicit
  `decline`), or `"not documented"`. Set `FALSE` to rely only on
  `decline` / `assume_decline`.

- decline_years:

  Length in years of the recent window used to test the habitat trend
  (default `10`, the IUCN reference of ten years or three generations
  for the shortest-lived taxa). Clamped to the years the product
  publishes.

- min_records:

  Minimum records required to attempt an assessment.

- verbose:

  Logical; print progress (default `TRUE`).

## Value

An object of class `geoconv_assessment`: a list with `summary` (one row
per species, including `n_subpop` and `n_locations`) and `detail`
(per-species `points`, `eoo`, `aoo`, `subpop`, `locations`,
`eoo_conversion`, `aoo_conversion`, and - when `fire = TRUE`

- `eoo_fire`, `aoo_fire`).

## Examples

``` r
f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
occ <- read_occurrences(f)
# EOO/AOO, subpopulations and locations only (offline, no network):
res <- assess_species(occ, mapbiomas = FALSE, verbose = FALSE)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is FALSE
res$summary
#>                         species n_records n_unique eoo_km2 aoo_km2 aoo_cells
#> 1       Prepusa connata Gardner         5        5 2461.73      16         4
#> 2    Prepusa hookeriana Gardner        21       21 2077.46      52        13
#> 3 Worsleya procera (Lem.) Traub         3        3    0.00       4         1
#>   n_subpop n_locations eoo_converted_pct eoo_natural_pct aoo_converted_pct
#> 1        4           4                NA              NA                NA
#> 2        4           7                NA              NA                NA
#> 3        3           1                NA              NA                NA
#>   aoo_natural_pct   eoo_cat_B1   aoo_cat_B2 provisional_cat category_B
#> 1              NA EN (B1 size) EN (B2 size)    EN (B1 size)         EN
#> 2              NA EN (B1 size) EN (B2 size)    EN (B1 size)         VU
#> 3              NA CR (B1 size) CR (B2 size)    CR (B1 size)         CR
#>   criterion_B_code subcrit_a subcrit_b subcrit_c decline_assumed decline_basis
#> 1      EN B1ab+2ab      TRUE      TRUE     FALSE            TRUE       assumed
#> 2      VU B1ab+2ab      TRUE      TRUE     FALSE            TRUE       assumed
#> 3      CR B1ab+2ab      TRUE      TRUE     FALSE            TRUE       assumed
#>   habitat_trend_pct_yr habitat_trend_p mapbiomas_initiative mapbiomas_year
#> 1                   NA              NA               brazil           2024
#> 2                   NA              NA               brazil           2024
#> 3                   NA              NA               brazil           2024
#>   mapbiomas_collection
#> 1                   10
#> 2                   10
#> 3                   10
# \donttest{
# Full screening with MapBiomas land cover and fire (reads data over the web):
# (one species: the habitat-trend test reads six more years of land cover)
sp1 <- occ[occ$species == occ$species[1], ]
res2 <- assess_species(sp1, year = 2024, fire = TRUE, verbose = FALSE)
res2$summary
#>                   species n_records n_unique eoo_km2 aoo_km2 aoo_cells n_subpop
#> 1 Prepusa connata Gardner         5        5 2461.73      16         4        4
#>   n_locations eoo_converted_pct eoo_natural_pct aoo_converted_pct
#> 1           4              44.3            55.7              45.1
#>   aoo_natural_pct   eoo_cat_B1   aoo_cat_B2 provisional_cat category_B
#> 1            54.9 EN (B1 size) EN (B2 size)    EN (B1 size)         EN
#>   criterion_B_code subcrit_a subcrit_b subcrit_c decline_assumed decline_basis
#> 1      EN B1ab+2ab      TRUE      TRUE     FALSE            TRUE       assumed
#>   habitat_trend_pct_yr habitat_trend_p mapbiomas_initiative mapbiomas_year
#> 1                  0.1          0.0388               brazil           2024
#>   mapbiomas_collection eoo_burned_pct aoo_burned_pct fire_collection
#> 1                   10           15.5           20.3               4
# }
```
