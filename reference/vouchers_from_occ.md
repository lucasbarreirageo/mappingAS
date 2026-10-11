# Build examined-voucher strings from occurrence attributes

Derives a character vector of herbarium-style voucher citations from the
attribute columns of an occurrence table (or the `sf` returned by
[`read_occurrences`](https://lucasbarreirageo.github.io/mappingAS/reference/read_occurrences.md)),
so the "Examined vouchers" box of the factsheet can be filled
automatically instead of by hand.

## Usage

``` r
vouchers_from_occ(
  occ,
  species = NULL,
  voucher_col = NULL,
  collector_col = NULL,
  number_col = NULL,
  herbarium_col = NULL,
  unique = TRUE,
  sort = TRUE
)
```

## Arguments

- occ:

  A data frame or an `sf` of occurrence records. Geometry, if present,
  is ignored.

- species:

  Optional species name (or vector) to filter the rows before building
  the vouchers, matched against the `species` column when it exists.
  `NULL` (default) uses every row.

- voucher_col, collector_col, number_col, herbarium_col:

  Optional column names overriding the automatic detection. Set a column
  to `NA` to disable that part of the detection.

- unique:

  Collapse duplicate vouchers to a single entry (default `TRUE`).

- sort:

  Sort the returned vouchers alphabetically (default `TRUE`).

## Value

A character vector of voucher strings (possibly empty).

## Details

Two strategies are tried, in order:

1.  If a ready-made voucher column exists (e.g. `voucher`, `exsiccata`,
    `material`) its non-empty values are used verbatim.

2.  Otherwise a collector column (e.g. `collector`, `recordedBy`) is
    combined with a collection-number column (e.g. `collectorNumber`,
    `recordNumber`) into `"Collector Number"`. When a
    herbarium/institution column is present (e.g. `herbarium`,
    `institutionCode`) its code is appended in parentheses, giving
    `"Collector Number (RB)"`.

Column names are matched case-insensitively from a set of common Darwin
Core and herbarium aliases; pass the `*_col` arguments to override the
detection. When no relevant column is found, an empty character vector
is returned (so the caller simply leaves the box empty).

## Examples

``` r
df <- data.frame(
  species = c("Aus bus", "Aus bus"),
  collector = c("Barreira", "Silva"),
  collectorNumber = c("123", "456"),
  herbarium = c("RB", "R"),
  stringsAsFactors = FALSE)
vouchers_from_occ(df)
#> [1] "Barreira 123 (RB)" "Silva 456 (R)"    
```
