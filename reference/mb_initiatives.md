# MapBiomas initiatives supported by mappingAS

mappingAS can read land-use/land-cover from several public MapBiomas
initiatives, all streamed as annual Cloud-Optimized GeoTIFFs from the
public `storage.googleapis.com/mapbiomas-public` bucket via GDAL's
`/vsicurl/` driver - no Google Earth Engine account and no Google Drive
download required. Each initiative is one coherent product with its own
default collection, year span and file layout, but all share the same
*standardised* legend (see
[`mb_legend`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_legend.md)),
so a species whose range spans more than one product can be assessed
with consistent class names, colours and conservation groups.

## Usage

``` r
mb_initiatives()
```

## Value

A named list, one element per initiative, each a list with `label`
(human-readable name), `collection` (default collection number), `years`
(integer vector of available years) and `key`.

## Details

- `"brazil"`:

  MapBiomas Brazil, Collection 10 (1985-2024).

- `"amazonia"`:

  MapBiomas Amazonia / Pan-Amazon (RAISG), Collection 6 (1986-2023). The
  whole Amazon basin across countries.

- `"colombia"`:

  MapBiomas Colombia, Collection 3 (1985-2024).

- `"argentina"`:

  MapBiomas Argentina, Collection 2 (1985-2024).

- `"bolivia"`:

  MapBiomas Bolivia, Collection 3 (1985-2024).

- `"chile"`:

  MapBiomas Chile, Collection 1 (2000-2022).

- `"ecuador"`:

  MapBiomas Ecuador, Collection 3 (1985-2024).

- `"peru"`:

  MapBiomas Peru, Collection 3 (1985-2024).

- `"venezuela"`:

  MapBiomas Venezuela, Collection 2 (1985-2023).

- `"paraguay"`:

  MapBiomas Paraguay, Collection 2 (1985-2023).

- `"uruguay"`:

  MapBiomas Uruguay, Collection 1 (1985-2022).

## See also

[`mb_source_url`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_source_url.md),
[`mb_legend`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_legend.md),
[`assess_species`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md)

## Examples

``` r
names(mb_initiatives())
#>  [1] "brazil"    "amazonia"  "colombia"  "argentina" "bolivia"   "chile"    
#>  [7] "ecuador"   "peru"      "venezuela" "paraguay"  "uruguay"  
mb_initiatives()$peru$years
#>  [1] 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999
#> [16] 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014
#> [31] 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024
```
