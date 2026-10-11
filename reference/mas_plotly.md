# Turn a mappingAS chart into an interactive plotly widget

Converts a ggplot2 object produced by the mappingAS `plot_*` functions
([`plot_conversion`](https://lucasbarreirageo.github.io/mappingAS/reference/plot_conversion.md),
[`plot_protection`](https://lucasbarreirageo.github.io/mappingAS/reference/plot_protection.md),
[`plot_timeseries`](https://lucasbarreirageo.github.io/mappingAS/reference/plot_timeseries.md),
[`plot_fire_timeseries`](https://lucasbarreirageo.github.io/mappingAS/reference/plot_fire_timeseries.md))
into an interactive plotly widget. Those functions attach the tidy data
and palette needed, so the widget is built as *native* plotly (correct
per-segment hover tooltips and in-bar labels) rather than through
`ggplotly()`. Any other ggplot falls back to `ggplotly()`. This is the
single place that controls the interactive look across the package and
the Shiny app.

## Usage

``` r
mas_plotly(p, tooltip = c("fill", "x", "y"), ...)
```

## Arguments

- p:

  A ggplot object (as returned by the `plot_*` functions). Any
  non-ggplot is returned unchanged.

- tooltip:

  Aesthetics passed to
  [`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html)
  in the generic fallback (default `c("fill", "x", "y")`).

- ...:

  Reserved for future use.

## Value

A `plotly` htmlwidget (or `p` unchanged when it is not a ggplot or
plotly is not installed).

## Examples

``` r
if (interactive()) {
  occ <- read_occurrences(system.file("extdata", "example_occurrences.csv",
                                      package = "mappingAS"))
  res <- assess_species(occ, year = 2024, verbose = FALSE)  # reads MapBiomas
  mas_plotly(plot_conversion(res))
}
```
