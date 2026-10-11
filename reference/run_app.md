# Launch the mappingAS Shiny application

Starts the interactive app for uploading occurrences, computing EOO/AOO
and MapBiomas habitat conversion, and exploring/exporting the results.

## Usage

``` r
run_app(launch.browser = TRUE, max_upload_mb = 500, ...)
```

## Arguments

- launch.browser:

  Logical; open in the default browser (default `TRUE`).

- max_upload_mb:

  Maximum size, in megabytes, of a file that can be uploaded in the app
  (default `500`). Shiny's own default is only 5 MB, which rejects large
  occurrence tables before they can be read; raise this for very large
  datasets. Set to `NULL` to leave any pre-existing
  `shiny.maxRequestSize` option untouched.

- ...:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Invisibly `NULL`; called for its side effect.

## Examples

``` r
if (interactive()) {
  run_app()
  # allow uploads up to 2 GB
  run_app(max_upload_mb = 2048)
}
```
