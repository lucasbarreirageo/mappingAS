# Standardised MapBiomas legend with conservation groupings

Returns the *standardised* MapBiomas land-use/land-cover legend used
across every initiative supported by mappingAS (Brazil, Pan-Amazon /
Amazonia, Colombia, Argentina, Bolivia, Chile, Ecuador, Peru, Venezuela,
Paraguay and Uruguay - see
[`mb_initiatives`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_initiatives.md)),
with each pixel class assigned to a conservation-relevant group:
`"natural"`, `"anthropic"`, `"water"`, `"not_observed"` or `"other"`.
The `"anthropic"` classes are what counts as *converted* habitat;
`"natural"` is the remaining (current) natural habitat. By default
`"water"`, `"not_observed"` and `"other"` are excluded from the
conversion denominator (see
[`summarise_conversion`](https://lucasbarreirageo.github.io/mappingAS/reference/summarise_conversion.md)).

## Usage

``` r
mb_legend(collection = 10, initiative = "brazil")
```

## Arguments

- collection:

  Integer collection number. Interpreted together with `initiative`; a
  value that does not match the initiative's default collection emits a
  warning and the standardised legend is still returned.

- initiative:

  One of the keys of
  [`mb_initiatives`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_initiatives.md)
  (`"brazil"` by default). Kept for API symmetry and warning logic; the
  returned legend is the same standardised table for every initiative.

## Value

A `data.frame` with columns `code`, `class_en`, `class_pt`, `hex`
(standardised MapBiomas colour), `level1` and `group`.

## Details

MapBiomas harmonises pixel codes across its initiatives, so a single
legend can label every country's raster consistently - this is what
makes cross-border assessment possible for a species whose range spans
more than one product. The table is the union of the classes that occur
in any of the products; a code that does not occur in a given raster
simply contributes zero area. Besides the Brazil Collection 10 classes
it includes the classes specific to the other countries - e.g. Andinean
formations (81, 82), Glacier (34), primary/secondary/dwarf forest (59,
60, 67), scrubland/steppe/fog oasis/peatlands (66, 63, 70, 73),
Pinus/Eucalyptus plantations (79, 80) and salt flat (61).

## Examples

``` r
head(mb_legend())
#>   code                   class_en           class_pt     hex level1   group
#> 1    1                     Forest           Floresta #1f8d49 Forest natural
#> 2    3           Forest Formation Formacao Florestal #1f8d49 Forest natural
#> 3    4          Savanna Formation  Formacao Savanica #7dc975 Forest natural
#> 4    5                   Mangrove             Mangue #04381d Forest natural
#> 5    6           Floodable Forest  Floresta Alagavel #007785 Forest natural
#> 6   49 Wooded Sandbank Vegetation   Restinga Arborea #02d659 Forest natural
subset(mb_legend(), group == "anthropic")$class_en
#>  [1] "Farming"                          "Forest Plantation"               
#>  [3] "Pasture"                          "Agriculture"                     
#>  [5] "Temporary Crop"                   "Soybean"                         
#>  [7] "Sugar cane"                       "Rice"                            
#>  [9] "Cotton (beta)"                    "Other Temporary Crops"           
#> [11] "Perennial Crop"                   "Coffee"                          
#> [13] "Citrus"                           "Palm Oil"                        
#> [15] "Other Perennial Crops"            "Mosaic of Uses"                  
#> [17] "Banana (beta)"                    "Other Crops"                     
#> [19] "Pinus Plantation"                 "Eucalyptus Plantation"           
#> [21] "Other Types of Forest Plantation" "Urban Area"                      
#> [23] "Mining"                           "Photovoltaic Power Plant (beta)" 
#> [25] "Aquaculture"                     
```
