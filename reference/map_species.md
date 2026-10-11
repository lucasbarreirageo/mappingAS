# Interactive map of a species' points, EOO and AOO (MapBiomas + fire)

Builds a leaflet map with the occurrence points, EOO hull and occupied
AOO cells. Optionally overlays the MapBiomas land-cover raster and the
MapBiomas Fire accumulated layer (fire recurrence: number of years
burned), both clipped to the EOO, as toggleable layers with legends.

## Usage

``` r
map_species(
  assessment,
  species = NULL,
  mapbiomas = TRUE,
  fire = FALSE,
  src = NULL,
  fire_src = NULL,
  max_pixels = 800,
  lang = c("pt", "en"),
  clip = c("eoo", "aoo", "all"),
  protected = FALSE,
  pa_src = NULL,
  pa_occ_only = TRUE
)
```

## Arguments

- assessment:

  A `geoconv_assessment` from
  [`assess_species`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md).

- species:

  Species name to plot (default: first assessed).

- mapbiomas:

  Logical; overlay the MapBiomas raster (default `TRUE`).

- fire:

  Logical; overlay the MapBiomas Fire accumulated layer (default
  `FALSE`).

- src:

  Optional MapBiomas LULC GeoTIFF path/URL.

- fire_src:

  Optional MapBiomas Fire GeoTIFF path/URL.

- max_pixels:

  Target maximum raster size (longest side, pixels) for the overlays
  (default `800`).

- lang:

  Legend language: `"pt"` (default) or `"en"`. Controls the language of
  the MapBiomas class labels and the fire-frequency legend.

- clip:

  Geometry the rasters are clipped to: `"eoo"` (default, the EOO hull),
  `"aoo"` (the union of occupied AOO cells) or `"all"` (the union of
  both, so the raster spans the whole range).

- protected:

  Logical; overlay protected areas. Uses the protected-area layer
  already stored by `assess_species(..., protected = TRUE)`, or fetches
  it on the fly from WDPA otherwise. Default `FALSE`.

- pa_src:

  Optional local protected-area vector file for the overlay (offline).

- pa_occ_only:

  Logical; when `TRUE` (default) the map draws only the UCs that
  actually contain occurrences of the species. Set `FALSE` to draw every
  UC the range overlaps. Does not affect the EOO/AOO overlap
  percentages, which are computed over the whole range.

## Value

A `leaflet` widget.
