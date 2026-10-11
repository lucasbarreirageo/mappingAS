# Regression trendline of one land-cover class through time

Fits a regression of a single land-cover class (or conservation group)
percentage of area against year and plots the observed points with the
fitted curve, its equation, R-squared and p-value. When the ggtrendline
package is installed (<https://CRAN.R-project.org/package=ggtrendline>)
it is used to draw the trendline and annotate the statistics; otherwise
the function falls back to a plain ggplot2 linear fit carrying the same
statistics, and finally to base graphics.

## Usage

``` r
plot_class_trendline(
  ts,
  class_label = NULL,
  model = "line2P",
  title = NULL,
  lang = c("en", "pt")
)
```

## Arguments

- ts:

  A long data frame from
  [`cover_timeseries`](https://lucasbarreirageo.github.io/mappingAS/reference/cover_timeseries.md)
  or
  [`timeseries_for_species`](https://lucasbarreirageo.github.io/mappingAS/reference/timeseries_for_species.md).

- class_label:

  The `label` (class or group name) to analyse. When `NULL` (default),
  the class with the largest mean percentage across the series is used.

- model:

  ggtrendline model, one of `"line2P"` (linear, default), `"line3P"`
  (quadratic), `"log2P"`, `"exp2P"`, `"exp3P"`, `"power2P"` or
  `"power3P"`. Ignored (a linear fit is used) when ggtrendline is not
  installed.

- title:

  Optional plot title. When `NULL`, a title is built from the class name
  and the `species`/`range` attributes when present.

- lang:

  Axis/title language: `"en"` (default) or `"pt"`. When `"en"` the
  English MapBiomas class names (`class_en`) are used when available.

## Value

A ggplot object when ggplot2 is available; otherwise `NULL` (invisibly)
after drawing a base plot.

## See also

[`plot_timeseries`](https://lucasbarreirageo.github.io/mappingAS/reference/plot_timeseries.md),
[`timeseries_for_species`](https://lucasbarreirageo.github.io/mappingAS/reference/timeseries_for_species.md)
