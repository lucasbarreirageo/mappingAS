# Read World Database on Protected Areas polygons intersecting an area

Queries the public WDPA ArcGIS `FeatureServer` for the protected-area
polygons whose envelope intersects the bounding box of `aoi` and returns
them as an `sf` in WGS84 with the same three standardised columns used
by
[`protected_areas()`](https://lucasbarreirageo.github.io/mappingAS/reference/protected_areas.md):
`pa_name` (WDPA `NAME`), `pa_category` (designation / IUCN category) and
`pa_group` (*Strict protection* for IUCN Ia-III, *Sustainable use* for
IUCN IV-VI, else the reported status), plus all original WDPA
attributes.

## Usage

``` r
wdpa_areas(
  aoi,
  url = wdpa_query_url(),
  marine = c("all", "terrestrial"),
  cache = TRUE,
  cache_dir = NULL,
  quiet = TRUE
)
```

## Arguments

- aoi:

  An `sf`/`sfc` polygon or point set (any CRS).

- url:

  WDPA query endpoint (default
  [`wdpa_query_url()`](https://lucasbarreirageo.github.io/mappingAS/reference/wdpa_query_url.md)).

- marine:

  One of `"all"` (default), `"terrestrial"` (drop purely marine PAs,
  `MARINE = 2`) - useful for terrestrial species screening.

- cache, cache_dir:

  Cache the result on disk (default `TRUE`; folder `mappingAS_pa_cache`
  under [`tempdir()`](https://rdrr.io/r/base/tempfile.html)).

- quiet:

  Passed to
  [`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html)
  (default `TRUE`).

## Value

An `sf` of WDPA polygons in EPSG:4326 (possibly zero rows), or `NULL` if
the service could not be read.

## Details

Only the AOI's bounding box is requested (server-side envelope filter)
and the server's transfer limit is followed with pagination, so this
stays light even though WDPA is a global dataset. Results are cached on
disk by bounding box. This is the recommended protected-area source for
the `"amazonia"` and `"colombia"` initiatives (and anywhere outside
Brazil), where the ICMBio service does not apply.

## Examples

``` r
# \donttest{
occ <- read_occurrences(system.file("extdata", "example_occurrences.csv",
                                    package = "mappingAS"))
sp1 <- occ[occ$species == occ$species[1], ]
pas <- wdpa_areas(calc_eoo(sp1)$hull)  # reads WDPA over the web
unique(pas$pa_name)
#>  [1] "Parque Nacional Da Serra Dos Órgãos"                                      
#>  [2] "Reserva Biológica De Araras"                                              
#>  [3] "Área De Proteção Ambiental Da Região Serrana De Petrópolis"               
#>  [4] "Reserva Biológica Do Tinguá"                                              
#>  [5] "Área De Proteção Ambiental De Macaé De Cima"                              
#>  [6] "Área De Proteção Ambiental Da Bacia Do Rio São João - Mico-Leão-Dourado"  
#>  [7] "Área De Proteção Ambiental Da Bacia Do Rio Dos Frades"                    
#>  [8] "Área De Proteção Ambiental Da Bacia Do Rio Macacu"                        
#>  [9] "Parque Natural Municipal De Petrópolis"                                   
#> [10] "Reserva Particular Do Patrimônio Natural Fazenda Miosótis"                
#> [11] "Monumento Natural Municipal Da Pedra Do Colégio"                          
#> [12] "Reserva Particular Do Patrimônio Natural Caldeirão"                       
#> [13] "Reserva Particular Do Patrimônio Natural Lençóis"                         
#> [14] "Reserva Particular Do Patrimônio Natural Olho Dágua"                      
#> [15] "Reserva Particular Do Patrimônio Natural Duas Pedras"                     
#> [16] "Reserva Particular Do Patrimônio Natural Soledade"                        
#> [17] "Reserva Particular Do Patrimônio Natural Nêga Fulô"                       
#> [18] "Parque Natural Municipal Montanhas De Teresópolis"                        
#> [19] "Reserva Particular Do Patrimônio Natural Pedra Branca"                    
#> [20] "Reserva Particular Do Patrimônio Natural Córrego Frio"                    
#> [21] "Reserva Particular Do Patrimônio Natural Reserva Ecológica De Guapiaçu"   
#> [22] "Reserva Particular Do Patrimônio Natural Woodstock"                       
#> [23] "Reserva Particular Do Patrimônio Natural Sítio Serra Negra"               
#> [24] "Reserva Particular Do Patrimônio Natural Canto Dos Pássaros Ii"           
#> [25] "Reserva Particular Do Patrimônio Natural Fazenda Suspiro"                 
#> [26] "Reserva Particular Do Patrimônio Natural Canto Dos Pássaros"              
#> [27] "Reserva Particular Do Patrimônio Natural Reserva Serra Do Caramandu"      
#> [28] "Reserva Particular Do Patrimônio Natural Pilões"                          
#> [29] "Reserva Particular Do Patrimônio Natural Rildo De Oliveira Gomes Ii"      
#> [30] "Reserva Particular Do Patrimônio Natural Alto Da Boa Vista - Resgate Viii"
#> [31] "Reserva Particular Do Patrimônio Natural Reserva Ecológica De Guapiaçu 2" 
#> [32] "Reserva Particular Do Patrimônio Natural Mico Leão Dourado"               
#> [33] "Reserva Particular Do Patrimônio Natural Rogério Marinho"                 
#> [34] "Reserva Particular Do Patrimônio Natural Graziela Maciel Barroso"         
#> [35] "Reserva Particular Do Patrimônio Natural Regina Clara"                    
#> [36] "Reserva Particular Do Patrimônio Natural Regina"                          
#> [37] "Reserva Particular Do Patrimônio Natural Nossa Senhora Aparecida"         
#> [38] "Reserva Particular Do Patrimônio Natural Jacutinga"                       
#> [39] "Refúgio De Vida Silvestre Estadual Do Médio Paraíba"                      
#> [40] "Área De Proteção Ambiental Bemposta"                                      
#> [41] "Estação Ecológica Monte Das Flores"                                       
#> [42] "Reserva Particular Do Patrimônio Natural Pastos Dos Bois E Pedregulhos"   
#> [43] "Reserva Particular Do Patrimônio Natural Reserva Ecológica De Guapiaçu 3" 
#> [44] "Reserva Particular Do Patrimônio Natural Rildo De Oliveira Gomes I"       
#> [45] "Área De Proteção Ambiental Santa Fé"                                      
#> [46] "Área De Proteção Ambiental Vale Do Morro Da Torre"                        
#> [47] "Reserva Particular Do Patrimônio Natural Floresta Alta"                   
#> [48] "Parque Estadual Dos Três Picos"                                           
#> [49] "Monumento Natural Municipal Da Concórdia"                                 
#> [50] "Refúgio Da Vida Silvestre Do Sucavão"                                     
#> [51] "Área De Proteção Ambiental Vale Do Piabanha"                              
#> [52] "Área De Proteção Ambiental Vale Fagundes"                                 
#> [53] "Área De Proteção Ambiental Vale Lagoa Do Morro Grande."                   
#> [54] "Monumento Natural Municipal Pico Do Cambote"                              
#> [55] "Parque Natural Municipal José Franklin Dos Santos Vaz"                    
#> [56] "Parque Natural Municipal Odette De Lima Soares"                           
#> [57] "Reserva Particular Do Patrimônio Natural  Regua Iv"                       
#> [58] "Reserva Particular Do Patrimônio Natural Regua V"                         
#> [59] "Reserva Particular Do Patrimônio Natural Caiuá"                           
#> [60] "Reserva Particular Do Patrimônio Natural Toca Da Onça"                    
#> [61] "Área De Proteção Ambiental Maravilha"                                     
#> [62] "Parque Natural Municipal Da Araponga"                                     
#> [63] "Área De Proteção Ambiental De Nossa Senhora De Santana"                   
#> [64] "Monumento Natural Estadual Da Serra Da Maria Comprida"                    
#> [65] "Área De Proteção Ambiental De Três Picos"                                 
#> [66] "Parque Natural Municipal Da Colina Do Conde"                              
#> [67] "Refúgio De Vida Silvestre De Macacu"                                      
#> [68] "Reserva Biologica Do Dindi"                                               
#> [69] "Monuimento Natural Pedra Das Flores"                                      
#> [70] "Parque Natural Municipal Fazenda Das Laranjeiras"                         
#> [71] "Refúgio De Vida Silvestre De Santa Fé"                                    
#> [72] "Reserva Particular Do Patrimônio Natural El Shammah"                      
#> [73] "Área De Proteção Ambiental De Nossa Senhora Aparecida"                    
#> [74] "Área De Proteção Ambiental Grotão"                                        
#> [75] "Área De Proteção Ambiental Fábrica"                                       
#> [76] "Reserva Particular Do Patrimônio Natural Fazenda Colomi"                  
# }
```
