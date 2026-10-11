# Years available for a MapBiomas initiative/collection

Years available for a MapBiomas initiative/collection

## Usage

``` r
mb_years(collection = 10, initiative = "brazil")
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

An integer vector of years available for the initiative (e.g. Brazil
1985-2024, Amazonia 1986-2023, Paraguay 1985-2023, Uruguay 1985-2022).

## Examples

``` r
range(mb_years())
#> [1] 1985 2024
range(mb_years(initiative = "amazonia"))
#> [1] 1986 2023
```
