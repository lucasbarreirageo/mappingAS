# Years available for the global Sentinel-2 / Esri land-cover series

Years available for the global Sentinel-2 / Esri land-cover series

## Usage

``` r
s2_years()
```

## Value

An integer vector of the years covered by the Esri / Impact Observatory
10 m Annual Land Use Land Cover product (currently 2017-2023).

## See also

[`esri_legend`](https://lucasbarreirageo.github.io/mappingAS/reference/esri_legend.md),
[`s2_source_url`](https://lucasbarreirageo.github.io/mappingAS/reference/s2_source_url.md),
[`assess_species`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md)

## Examples

``` r
range(s2_years())
#> [1] 2017 2023
```
