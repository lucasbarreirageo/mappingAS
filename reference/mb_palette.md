# Official MapBiomas colours for a set of class codes

Official MapBiomas colours for a set of class codes

## Usage

``` r
mb_palette(
  codes = NULL,
  collection = 10,
  initiative = "brazil",
  by = c("code", "class_pt", "class_en")
)
```

## Arguments

- codes:

  Optional integer vector of MapBiomas pixel codes. If `NULL` (default)
  colours for the full legend are returned.

- collection:

  Integer collection number. Interpreted together with `initiative`; a
  value that does not match the initiative's default collection emits a
  warning and the standardised legend is still returned.

- initiative:

  One of the keys of
  [`mb_initiatives`](https://lucasbarreirageo.github.io/mappingAS/reference/mb_initiatives.md)
  (`"brazil"` by default). Kept for API symmetry and warning logic; the
  returned legend is the same standardised table for every initiative.

- by:

  One of `"code"` (names are codes) or `"class_pt"` / `"class_en"`
  (names are class labels). Default `"code"`.

## Value

A named character vector of hex colours.

## Examples

``` r
mb_palette(c(3, 15, 39, 33))
#>         3        15        39        33 
#> "#1f8d49" "#edde8e" "#f5b3c8" "#2532e4" 
```
