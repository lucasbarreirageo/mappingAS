# Group lookup vectors for MapBiomas conservation groups

Convenience wrapper returning the pixel codes that belong to each group.

## Usage

``` r
mb_groups(collection = 10, initiative = "brazil")
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

A named list with elements `natural`, `anthropic`, `water`, `other` and
`not_observed`, each an integer vector of MapBiomas pixel codes.
