# Protection chart (inside vs outside Protected areas) for EOO and AOO

Two horizontal stacked bars (EOO and AOO) showing the share of the range
inside federal Protected areas (Protected areas, UCs) versus outside,
from `assess_species(..., protected = TRUE)`. When MapBiomas was also
computed (`mapbiomas = TRUE`), the inside-UC part is split into
*natural* and *altered* habitat, so the dark-green segment is the range
that is natural *and* protected. Layout mirrors
[`plot_conversion`](https://lucasbarreirageo.github.io/mappingAS/reference/plot_conversion.md):
title, one subtitle with the occurrence count, and a legend on the
right.

## Usage

``` r
plot_protection(assessment, species = NULL, lang = c("en", "pt"))
```

## Arguments

- assessment:

  A `geoconv_assessment` run with `protected = TRUE`.

- species:

  Species name (default: first).

- lang:

  Label language: `"en"` (default) or `"pt"`.

## Value

A `ggplot` object (ggplot2 available) or, in the base-graphics fallback,
invisibly the plotted percentage matrix (rows `EOO`/`AOO`).

## See also

[`mas_plotly`](https://lucasbarreirageo.github.io/mappingAS/reference/mas_plotly.md)
for the interactive version.

## Examples

``` r
# \donttest{
occ <- read_occurrences(system.file("extdata", "example_occurrences.csv",
                                    package = "mappingAS"))
res <- assess_species(occ, protected = TRUE, verbose = FALSE)  # reads WDPA
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
plot_protection(res)

# }
```
