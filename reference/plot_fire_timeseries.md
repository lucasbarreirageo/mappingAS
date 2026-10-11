# Bar/line chart of burned area (%) over time

Bar/line chart of burned area (%) over time

## Usage

``` r
plot_fire_timeseries(ts, title = NULL, lang = c("en", "pt"))
```

## Arguments

- ts:

  A data frame from
  [`fire_timeseries`](https://lucasbarreirageo.github.io/mappingAS/reference/fire_timeseries.md)
  /
  [`fire_timeseries_for_species`](https://lucasbarreirageo.github.io/mappingAS/reference/fire_timeseries_for_species.md).

- title:

  Optional title (built from attributes when `NULL`).

- lang:

  Label language: `"en"` (default) or `"pt"`.

## Value

A ggplot object when ggplot2 is available; otherwise `NULL` after a base
plot.
