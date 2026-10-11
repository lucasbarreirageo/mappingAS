# Habitat composition chart (natural / altered / water / other) for EOO and AOO

Draws, for one species, two horizontal stacked bars (EOO and AOO)
showing the MapBiomas composition as a percentage of the mapped
(observed) area: natural, altered (anthropic), water and other. Segment
labels are placed inside each bar and the legend sits below the plot, so
nothing overlaps. The headline *terrestrial* converted percentage is
shown as a subtitle.

## Usage

``` r
plot_conversion(assessment, species = NULL, lang = c("en", "pt"))
```

## Arguments

- assessment:

  A `geoconv_assessment`.

- species:

  Species name (default: first).

- lang:

  Label language: `"en"` (default) or `"pt"`.

## Value

A `ggplot` object (ggplot2 available) or, in the base-graphics fallback,
invisibly the plotted percentage matrix.

## Details

When ggplot2 is available (a hard dependency) the function returns a
`ggplot` object, which
[`mas_plotly`](https://lucasbarreirageo.github.io/mappingAS/reference/mas_plotly.md)
can turn into an interactive chart; otherwise it draws a base-graphics
fallback and returns the percentage matrix invisibly.

## See also

[`mas_plotly`](https://lucasbarreirageo.github.io/mappingAS/reference/mas_plotly.md)
for the interactive version.
