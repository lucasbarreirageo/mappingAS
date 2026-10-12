# Per-class MapBiomas composition table (every class) for an assessment

Returns, for each species, the area and percentage of *every* MapBiomas
class found inside the EOO and/or the AOO. This complements the headline
natural-versus-converted summary in
[`assess_species`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md)
with the full class-by-class breakdown.

## Usage

``` r
class_table(assessment, species = NULL, range = c("both", "eoo", "aoo"))
```

## Arguments

- assessment:

  A `geoconv_assessment` from
  [`assess_species`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md)
  (run with `mapbiomas = TRUE`).

- species:

  Optional species name (or vector). `NULL` (default) uses all assessed
  species.

- range:

  `"both"` (default), `"eoo"` or `"aoo"`.

## Value

A long `data.frame` with columns `species`, `range` (`"EOO"`/`"AOO"`),
`code`, `class_pt`, `class_en`, `group`, `area_km2` and `pct`
(percentage of the mapped area within that range). Empty (with a
warning) if MapBiomas was not computed.

## Examples

``` r
# \donttest{
occ <- read_occurrences(system.file("extdata", "example_occurrences.csv",
                                    package = "mappingAS"))
# reads MapBiomas (one year; the multi-year habitat trend is skipped)
res <- assess_species(occ, year = 2024, infer_decline_from_habitat = FALSE,
                      verbose = FALSE)
class_table(res)
#>                          species range code                       class_pt
#> 1        Prepusa connata Gardner   AOO    3             Formacao Florestal
#> 2        Prepusa connata Gardner   AOO   15                       Pastagem
#> 3        Prepusa connata Gardner   AOO   21                Mosaico de Usos
#> 4        Prepusa connata Gardner   AOO   12             Formacao Campestre
#> 5        Prepusa connata Gardner   AOO    9                   Silvicultura
#> 6        Prepusa connata Gardner   AOO   33             Rio, Lago e Oceano
#> 7        Prepusa connata Gardner   AOO   29            Afloramento Rochoso
#> 8        Prepusa connata Gardner   EOO    3             Formacao Florestal
#> 9        Prepusa connata Gardner   EOO   21                Mosaico de Usos
#> 10       Prepusa connata Gardner   EOO   15                       Pastagem
#> 11       Prepusa connata Gardner   EOO   12             Formacao Campestre
#> 12       Prepusa connata Gardner   EOO   24                Area Urbanizada
#> 13       Prepusa connata Gardner   EOO    9                   Silvicultura
#> 14       Prepusa connata Gardner   EOO   33             Rio, Lago e Oceano
#> 15       Prepusa connata Gardner   EOO   25     Outras Areas nao Vegetadas
#> 16       Prepusa connata Gardner   EOO   11 Campo Alagado e Area Pantanosa
#> 17       Prepusa connata Gardner   EOO   29            Afloramento Rochoso
#> 18       Prepusa connata Gardner   EOO   41    Outras Lavouras Temporarias
#> 19       Prepusa connata Gardner   EOO   39                           Soja
#> 20       Prepusa connata Gardner   EOO   30                      Mineracao
#> 21       Prepusa connata Gardner   EOO   32                         Apicum
#> 22       Prepusa connata Gardner   EOO    4              Formacao Savanica
#> 23    Prepusa hookeriana Gardner   AOO    3             Formacao Florestal
#> 24    Prepusa hookeriana Gardner   AOO   21                Mosaico de Usos
#> 25    Prepusa hookeriana Gardner   AOO   24                Area Urbanizada
#> 26    Prepusa hookeriana Gardner   AOO   12             Formacao Campestre
#> 27    Prepusa hookeriana Gardner   AOO   15                       Pastagem
#> 28    Prepusa hookeriana Gardner   AOO   29            Afloramento Rochoso
#> 29    Prepusa hookeriana Gardner   AOO   33             Rio, Lago e Oceano
#> 30    Prepusa hookeriana Gardner   AOO    9                   Silvicultura
#> 31    Prepusa hookeriana Gardner   AOO   25     Outras Areas nao Vegetadas
#> 32    Prepusa hookeriana Gardner   AOO   32                         Apicum
#> 33    Prepusa hookeriana Gardner   EOO    3             Formacao Florestal
#> 34    Prepusa hookeriana Gardner   EOO   21                Mosaico de Usos
#> 35    Prepusa hookeriana Gardner   EOO   15                       Pastagem
#> 36    Prepusa hookeriana Gardner   EOO   12             Formacao Campestre
#> 37    Prepusa hookeriana Gardner   EOO   24                Area Urbanizada
#> 38    Prepusa hookeriana Gardner   EOO    9                   Silvicultura
#> 39    Prepusa hookeriana Gardner   EOO   25     Outras Areas nao Vegetadas
#> 40    Prepusa hookeriana Gardner   EOO   33             Rio, Lago e Oceano
#> 41    Prepusa hookeriana Gardner   EOO   11 Campo Alagado e Area Pantanosa
#> 42    Prepusa hookeriana Gardner   EOO   29            Afloramento Rochoso
#> 43    Prepusa hookeriana Gardner   EOO   41    Outras Lavouras Temporarias
#> 44    Prepusa hookeriana Gardner   EOO   39                           Soja
#> 45    Prepusa hookeriana Gardner   EOO   30                      Mineracao
#> 46    Prepusa hookeriana Gardner   EOO   32                         Apicum
#> 47    Prepusa hookeriana Gardner   EOO    4              Formacao Savanica
#> 48 Worsleya procera (Lem.) Traub   AOO    3             Formacao Florestal
#> 49 Worsleya procera (Lem.) Traub   AOO   21                Mosaico de Usos
#> 50 Worsleya procera (Lem.) Traub   AOO   15                       Pastagem
#> 51 Worsleya procera (Lem.) Traub   AOO   12             Formacao Campestre
#> 52 Worsleya procera (Lem.) Traub   EOO   12             Formacao Campestre
#> 53 Worsleya procera (Lem.) Traub   EOO   21                Mosaico de Usos
#>                     class_en     group  area_km2   pct
#> 1           Forest Formation   natural    8.0841 49.22
#> 2                    Pasture anthropic    3.9554 24.08
#> 3             Mosaic of Uses anthropic    3.4068 20.74
#> 4                  Grassland   natural    0.9241  5.63
#> 5          Forest Plantation anthropic    0.0324  0.20
#> 6      River, Lake and Ocean     water    0.0157  0.10
#> 7              Rocky Outcrop   natural    0.0075  0.05
#> 8           Forest Formation   natural 1310.1092 53.13
#> 9             Mosaic of Uses anthropic  587.9621 23.84
#> 10                   Pasture anthropic  443.1769 17.97
#> 11                 Grassland   natural   58.6202  2.38
#> 12                Urban Area anthropic   45.2100  1.83
#> 13         Forest Plantation anthropic   13.7537  0.56
#> 14     River, Lake and Ocean     water    2.6326  0.11
#> 15 Other non Vegetated Areas     other    2.3218  0.09
#> 16                   Wetland   natural    0.7386  0.03
#> 17             Rocky Outcrop   natural    0.6735  0.03
#> 18     Other Temporary Crops anthropic    0.2317  0.01
#> 19                   Soybean anthropic    0.2292  0.01
#> 20                    Mining anthropic    0.0928  0.00
#> 21    Hypersaline Tidal Flat   natural    0.0464  0.00
#> 22         Savanna Formation   natural    0.0008  0.00
#> 23          Forest Formation   natural   31.7461 59.37
#> 24            Mosaic of Uses anthropic    9.1545 17.12
#> 25                Urban Area anthropic    6.3550 11.88
#> 26                 Grassland   natural    3.7249  6.97
#> 27                   Pasture anthropic    1.9575  3.66
#> 28             Rocky Outcrop   natural    0.2526  0.47
#> 29     River, Lake and Ocean     water    0.1210  0.23
#> 30         Forest Plantation anthropic    0.0714  0.13
#> 31 Other non Vegetated Areas     other    0.0695  0.13
#> 32    Hypersaline Tidal Flat   natural    0.0215  0.04
#> 33          Forest Formation   natural 1191.5279 57.25
#> 34            Mosaic of Uses anthropic  468.3845 22.51
#> 35                   Pasture anthropic  317.5282 15.26
#> 36                 Grassland   natural   46.5826  2.24
#> 37                Urban Area anthropic   39.2056  1.88
#> 38         Forest Plantation anthropic   12.4421  0.60
#> 39 Other non Vegetated Areas     other    1.7030  0.08
#> 40     River, Lake and Ocean     water    1.5466  0.07
#> 41                   Wetland   natural    0.7871  0.04
#> 42             Rocky Outcrop   natural    0.7115  0.03
#> 43     Other Temporary Crops anthropic    0.3658  0.02
#> 44                   Soybean anthropic    0.2706  0.01
#> 45                    Mining anthropic    0.0928  0.00
#> 46    Hypersaline Tidal Flat   natural    0.0588  0.00
#> 47         Savanna Formation   natural    0.0008  0.00
#> 48          Forest Formation   natural    1.9004 46.23
#> 49            Mosaic of Uses anthropic    1.0947 26.63
#> 50                   Pasture anthropic    0.5714 13.90
#> 51                 Grassland   natural    0.5440 13.23
#> 52                 Grassland   natural    0.0041 62.50
#> 53            Mosaic of Uses anthropic    0.0025 37.50
# }
```
