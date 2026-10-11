# Sentinel-2 / Esri global 10 m land-cover legend

Returns the 9-class legend of the Esri / Impact Observatory / Microsoft
"10 m Annual Land Use Land Cover" product (derived from ESA Sentinel-2,
the data behind the ArcGIS Living Atlas Land Cover Explorer), with each
pixel class mapped to the same conservation groups used by mappingAS for
MapBiomas (`"natural"`, `"anthropic"`, `"water"`, `"other"`,
`"not_observed"`). This lets a range assessed with the global fallback
flow through the exact same conversion, plotting and report pipeline as
a MapBiomas one (see
[`summarise_conversion`](https://lucasbarreirageo.github.io/mappingAS/reference/summarise_conversion.md)).

## Usage

``` r
esri_legend()
```

## Value

A `data.frame` with columns `code`, `class_en`, `class_pt`, `hex`
(official Esri colour), `level1` and `group`.

## Details

Group mapping: Trees, Rangeland and Flooded Vegetation are `natural`;
Crops and Built Area are `anthropic` (converted habitat); Water is
`water`; Bare Ground and Snow/Ice are `other` (ambiguous, excluded from
the conversion denominator by default); Clouds are `not_observed`.

## See also

[`mb_legend`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_legend.md),
[`summarise_conversion`](https://lucasbarreirageo.github.io/mappingAS/reference/summarise_conversion.md)

## Examples

``` r
esri_legend()
#>   code           class_en            class_pt     hex             level1
#> 1    1              Water                Agua #1A5BAB              Water
#> 2    2              Trees             Arvores #358221             Forest
#> 3    4 Flooded Vegetation  Vegetacao Alagavel #87D19E Herbaceous/Shrubby
#> 4    5              Crops         Agricultura #FFDB5C            Farming
#> 5    7         Built Area     Area Construida #ED022A       NonVegetated
#> 6    8        Bare Ground        Solo Exposto #EDE9E4       NonVegetated
#> 7    9           Snow/Ice           Neve/Gelo #F2FAFF              Water
#> 8   10             Clouds              Nuvens #C8C8C8        NotObserved
#> 9   11          Rangeland Vegetacao Campestre #C6AD8D Herbaceous/Shrubby
#>          group
#> 1        water
#> 2      natural
#> 3      natural
#> 4    anthropic
#> 5    anthropic
#> 6        other
#> 7        other
#> 8 not_observed
#> 9      natural
subset(esri_legend(), group == "anthropic")$class_en
#> [1] "Crops"      "Built Area"
```
