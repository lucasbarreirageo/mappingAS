# Standalone HTML species factsheet with user metadata and photos

Builds a single, self-contained HTML file that presents one species as a
printable/shareable factsheet - the kind hosted on a supplementary
website. It combines two sources of information:

1.  **Everything the assessment already computes** - range metrics
    (EOO/AOO and provisional Criterion B category), habitat conversion,
    fire and protected-area overlap - rendered as a key-metrics grid,
    the standard charts (composition, protection, and any land-cover /
    fire time series supplied), a "top anthropic activities" chart
    derived from the per-class land-cover breakdown, and the
    interpretive narrative of
    [`assessment_report`](https://lucasbarreirageo.github.io/mappingAS/reference/assessment_report.md).

2.  **Information the package cannot know**, supplied by the user: the
    taxonomy (Family, Genus, Authority), the supporting-information
    block (Countries, Life Form, Substrate, Habitat, Biome, Vegetation),
    free-text land use and conservation units, a list of examined
    vouchers, a taxonomic reference (a Reflora / POWO link, or - for a
    newly described species - the article citation) and up to four
    photographs.

The whole page is self-contained (fonts fall back to the system stack,
images and charts are embedded as base64 data URIs), so the returned
file can be opened offline or published as-is (e.g. on GitHub Pages).
Each photograph carries a watermark in its lower-right corner with the
owner name given in `photo_credit`.

## Usage

``` r
factsheet_html(
  assessment,
  species = NULL,
  lang = c("en", "pt"),
  file = NULL,
  family = NULL,
  genus = NULL,
  authority = NULL,
  countries = NULL,
  life_form = NULL,
  substrate = NULL,
  habitat = NULL,
  biome = NULL,
  vegetation = NULL,
  land_use = NULL,
  conservation_units = NULL,
  vouchers = NULL,
  reference = NULL,
  photos = NULL,
  photo_credit = NULL,
  cover_series = NULL,
  fire_series = NULL,
  applied_category = NULL,
  applied_code = NULL,
  map = TRUE,
  map_interactive = TRUE,
  top_n_threats = 5L
)
```

## Arguments

- assessment:

  A `geoconv_assessment` from
  [`assess_species`](https://lucasbarreirageo.github.io/mappingAS/reference/assess_species.md).

- species:

  Species name (default: the first assessed).

- lang:

  Factsheet language: `"en"` (default) or `"pt"`.

- file:

  Optional output path. When supplied the HTML is written there and the
  path is returned invisibly; otherwise the HTML is returned as a
  length-one character string.

- family, genus, authority:

  Taxonomy fields. `genus` defaults to the first word of `species`; the
  others are user-supplied (optional).

- countries, life_form, substrate, habitat, biome, vegetation:

  The "Supporting information" block, all optional free text.

- land_use, conservation_units:

  Optional overrides. When left `NULL` they are filled in automatically
  from the assessment: `land_use` from the anthropic land-cover classes
  within the EOO (the Conversion module) and `conservation_units` from
  the overlapping protected areas (the Protected-areas module). Pass a
  string to override the derived value; line breaks are preserved.

- vouchers:

  Optional examined material: a character vector (one voucher per
  element) or a single string with one voucher per line.

- reference:

  Optional taxonomic reference: a Reflora / POWO URL (rendered as a
  link) or, for a newly described taxon, the article citation.

- photos:

  Optional character vector of image file paths (PNG/JPEG/...); at most
  the first four are used.

- photo_credit:

  Watermark text (photo owner). A single value is applied to every
  photo; a vector is matched per photo.

- cover_series, fire_series:

  Optional land-cover / burned-area time series for this species (as for
  [`assessment_report`](https://lucasbarreirageo.github.io/mappingAS/reference/assessment_report.md));
  when supplied they add the temporal-trend narrative and their charts
  (a land-cover composition-over-time chart, and a burned-area-per-year
  chart).

- applied_category, applied_code:

  Optional applied Criterion B category (e.g. `"VU"`) and full code
  (e.g. `"VU B1ab(iii)"`), reflecting the sub-criteria chosen by the
  user. When supplied they are shown as an "Applied Criterion B
  category" metric; otherwise the assessment's `category_B` /
  `criterion_B_code` columns are used if present.

- map:

  Logical; embed the distribution map (occurrence points, EOO, AOO and
  land cover). Default `TRUE`; set `FALSE` to skip it (e.g. offline or
  for speed).

- map_interactive:

  Logical; when `TRUE` (default) embed the *interactive* Leaflet map
  ([`map_species`](https://lucasbarreirageo.github.io/mappingAS/reference/map_species.md),
  the same widget the app downloads via "Download map (HTML)") as a
  self-contained document inside an iframe, so the whole factsheet
  remains one portable file. When `FALSE`, or when the widget cannot be
  built (no leaflet / htmlwidgets / pandoc), the publication-ready
  static map
  ([`map_static`](https://lucasbarreirageo.github.io/mappingAS/reference/map_static.md))
  is embedded instead.

- top_n_threats:

  Number of anthropic land-cover classes to show in the "top anthropic
  activities" chart (default `5`).

## Value

The HTML string, or (when `file` is given) the file path, invisibly.

## See also

[`assessment_report`](https://lucasbarreirageo.github.io/mappingAS/reference/assessment_report.md)
for the narrative-only report.

## Examples

``` r
# \donttest{
occ <- read_occurrences(system.file("extdata", "example_occurrences.csv",
                                    package = "mappingAS"))
res <- assess_species(occ, year = 2024, verbose = FALSE)  # reads MapBiomas
out <- file.path(tempdir(), "factsheet.html")
factsheet_html(res, file = out, map = FALSE,
               family = "Gentianaceae", countries = "Brazil",
               life_form = "Herb", biome = "Atlantic Forest")
unlink(out)
# }
```
