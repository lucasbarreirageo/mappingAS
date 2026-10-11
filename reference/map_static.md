# Publication-ready static map of a species (MapBiomas + EOO + AOO + points)

Builds an exportable ggplot2 map for one species: the MapBiomas
land-cover layer clipped to the EOO (with the official colour legend of
the classes actually present, drawn beside the map so it never overlaps
the EOO/AOO), the EOO minimum convex polygon, the occupied AOO cells,
the occurrence points, plus a north arrow and a scale bar with the
geographic reference (datum/projection) printed beneath it. The map is
drawn in the species' equal-area projection so the scale bar is
meaningful, and is returned as a `ggplot` object you can save with
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
to make a usable, georeferenced-looking figure.

## Usage

``` r
map_static(
  assessment,
  species = NULL,
  mapbiomas = TRUE,
  fire = FALSE,
  src = NULL,
  max_pixels = 600,
  crs = NULL,
  scalebar = TRUE,
  north = TRUE,
  title = NULL,
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

  Species name (default: first assessed).

- mapbiomas:

  Logical; draw the MapBiomas raster layer (default `TRUE`). Set `FALSE`
  for a quick points/EOO/AOO map with no raster read.

- fire:

  Logical; draw the MapBiomas Fire *frequency* layer (number of years
  burned, 1985-2024). With `mapbiomas = TRUE` it is overlaid on the
  land-cover raster when ggnewscale is installed; otherwise only the
  land-cover layer is drawn (default `FALSE`).

- src:

  Optional local path / URL to a MapBiomas GeoTIFF (same as in
  [`assess_species`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md));
  `NULL` (default) uses the public URL.

- max_pixels:

  Target maximum raster size (longest side, in pixels) for the display
  layer (default `600`).

- crs:

  Optional CRS for the map (proj string or EPSG code). Defaults to the
  species' equal-area projection from the EOO/AOO.

- scalebar, north:

  Logical; add a scale bar / north arrow (default `TRUE`).

- title:

  Optional plot title (default: the species name).

- lang:

  Legend language: `"pt"` (default) or `"en"`. Controls the language of
  the MapBiomas class labels, the fire-frequency legend and the feature
  labels (EOO/AOO/occurrences).

- clip:

  Geometry the rasters are clipped to: `"eoo"` (default, the EOO hull),
  `"aoo"` (the union of occupied AOO cells) or `"all"` (the union of
  both). Matches the `clip` argument of
  [`map_species`](https://lucasbarreirageo.github.io/mappingAS/reference/map_species.md).

- protected:

  Logical; overlay protected areas on the publishable map (default
  `FALSE`). Uses the protected-area layer stored by
  `assess_species(..., protected = TRUE)`, or fetches it on the fly.

- pa_src:

  Optional local protected-area vector file for the overlay (offline).

- pa_occ_only:

  Logical; when `TRUE` (default) draw only the protected areas that
  contain occurrences of the species. `FALSE` draws every protected area
  the range overlaps.

## Value

A `ggplot` object.

## Details

The MapBiomas layer is read with the cached windowed reader and
aggregated to `max_pixels` for display, so it is fast and reuses the
raster already fetched during
[`assess_species`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md)
(same EOO window).

## Examples

``` r
occ <- read_occurrences(system.file("extdata", "example_occurrences.csv",
                                    package = "mappingAS"))
res <- assess_species(occ, mapbiomas = FALSE, verbose = FALSE)
m <- map_static(res, mapbiomas = FALSE)
# \donttest{
# Save it wherever you like (a temporary file here):
f <- file.path(tempdir(), "eoo_map.png")
ggplot2::ggsave(f, m, width = 8, height = 7)
unlink(f)
# }
```
