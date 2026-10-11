# Per-species table of Protected areas overlapping the range

Long `data.frame` (one row per species x UC) listing each federal UC
that contains occurrences of, or overlaps the EOO of, each assessed
species. Requires `assess_species(..., protected = TRUE)`.

## Usage

``` r
pa_table(assessment, species = NULL)
```

## Arguments

- assessment:

  A `geoconv_assessment` from
  [`assess_species()`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md).

- species:

  Optional species name(s); `NULL` (default) uses all.

## Value

A `data.frame` with `species`, `pa_name`, `pa_category`, `pa_group`,
`n_occ` (occurrences of that species inside the UC) and
`overlap_eoo_km2`.
