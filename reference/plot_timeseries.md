# Stacked-area chart of land-cover percentage over time

Draws percentage of area versus year as stacked areas, one band per
class (or group), using the official MapBiomas colours, in the style of
the MapBiomas coverage figures. Uses ggplot2 when available, otherwise
base graphics.

## Usage

``` r
plot_timeseries(ts, title = NULL, legend = TRUE, lang = c("en", "pt"))
```

## Arguments

- ts:

  A long data frame from
  [`cover_timeseries`](https://lucasbarreirageo.github.io/mappingAS/reference/cover_timeseries.md)
  or
  [`timeseries_for_species`](https://lucasbarreirageo.github.io/mappingAS/reference/timeseries_for_species.md).

- title:

  Optional plot title. If `NULL`, a title is built from the
  `species`/`range` attributes when present.

- legend:

  Logical; draw the class legend (default `TRUE`).

- lang:

  Legend language: `"en"` (default) or `"pt"`. When `"en"`, the band
  labels use the MapBiomas `class_en` column (when present in `ts`) and
  the axis/legend/title text is in English.

## Value

A ggplot object (invisibly) when ggplot2 is available; otherwise `NULL`
after drawing a base plot.
