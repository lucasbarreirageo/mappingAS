# Multi-initiative support (Brazil, Amazonia, Colombia and the South-American
# countries) and the standardised legend. Pure-URL / pure-table checks (no
# network).

test_that("mb_initiatives lists every product with sane metadata", {
  ini <- mb_initiatives()
  expect_setequal(
    names(ini),
    c("brazil", "amazonia", "colombia", "argentina", "bolivia", "chile",
      "ecuador", "peru", "venezuela"))
  expect_identical(ini$brazil$collection, 10L)
  expect_identical(ini$amazonia$collection, 6L)
  expect_identical(ini$colombia$collection, 3L)
  expect_identical(ini$argentina$collection, 2L)
  expect_identical(ini$chile$collection, 1L)
  expect_identical(ini$venezuela$collection, 2L)
  expect_equal(range(ini$brazil$years), c(1985L, 2024L))
  expect_equal(range(ini$amazonia$years), c(1986L, 2023L))
  expect_equal(range(ini$chile$years), c(2000L, 2022L))
  expect_equal(range(ini$venezuela$years), c(1985L, 2023L))
})

test_that("mb_source_url builds per-initiative public bucket URLs", {
  # Brazil (default) - unchanged behaviour
  b <- mb_source_url(2024, 10)
  expect_match(b, "initiatives/brasil/collection_10")
  expect_match(b, "brazil_coverage_2024\\.tif$")

  # Amazonia (Pan-Amazon) Collection 6 integration classification
  a <- mb_source_url(2023, initiative = "amazonia")
  expect_match(a, "initiatives/amazon/lulc/collection_6/integration")
  expect_match(a, "mapbiomas_collection60_integration_v1-classification_2023\\.tif$")

  # Colombia Collection 3 per-year coverage
  co <- mb_source_url(2024, initiative = "colombia")
  expect_match(co, "initiatives/colombia/collection_3/coverage")
  expect_match(co, "colombia_coverage_2024\\.tif$")

  # Argentina uses a hyphen in the collection folder
  ar <- mb_source_url(2024, initiative = "argentina")
  expect_match(ar, "initiatives/argentina/collection-2/coverage")
  expect_match(ar, "argentina_coverage_2024\\.tif$")

  # Bolivia / Ecuador share the coverage pattern
  expect_match(mb_source_url(2024, initiative = "bolivia"),
               "initiatives/bolivia/collection_3/coverage/bolivia_coverage_2024\\.tif$")
  expect_match(mb_source_url(2024, initiative = "ecuador"),
               "initiatives/ecuador/collection_3/coverage/ecuador_coverage_2024\\.tif$")

  # Chile has no collection segment in the path
  expect_match(mb_source_url(2022, initiative = "chile"),
               "initiatives/chile/coverage/chile_coverage_2022\\.tif$")

  # Peru / Venezuela use the integration-classification naming
  expect_match(mb_source_url(2024, initiative = "peru"),
               "initiatives/peru/collection_3/LULC/peru_collection3_integration_v1-classification_2024\\.tif$")
  expect_match(mb_source_url(2023, initiative = "venezuela"),
               "venezuela/collection_2/lulc/integration/mapbiomas_venezuela_collection2_integration_v1-classification_2023\\.tif$")

  # collection defaults follow the initiative when NULL
  expect_match(mb_source_url(2023, collection = NULL, initiative = "amazonia"),
               "collection_6")
})

test_that("friendly initiative aliases resolve", {
  expect_match(mb_source_url(2023, initiative = "pan-amazonia"), "amazon")
  expect_match(mb_source_url(2024, initiative = "brasil"), "brazil_coverage")
  expect_match(mb_source_url(2024, initiative = "equador"), "ecuador_coverage")
})

test_that("unknown initiative errors", {
  expect_error(mb_source_url(2024, initiative = "narnia"), "Unknown MapBiomas")
})

test_that("mb_years follows the initiative span", {
  expect_equal(range(mb_years(initiative = "amazonia")), c(1986L, 2023L))
  expect_equal(range(mb_years(initiative = "chile")), c(2000L, 2022L))
  # positional collection still returns Brazil default
  expect_equal(range(mb_years(10)), c(1985L, 2024L))
})

test_that("standardised legend adds the country classes without breaking Brazil", {
  leg <- mb_legend()
  expect_false(any(duplicated(leg$code)))
  expect_setequal(unique(leg$group),
                  c("natural", "anthropic", "water", "other", "not_observed"))
  g <- function(code) leg$group[leg$code == code]
  # Brazil mappings preserved
  expect_identical(g(3), "natural")
  expect_identical(g(15), "anthropic")
  expect_identical(g(33), "water")
  expect_identical(g(25), "other")
  # Colombia/Amazonia codes
  expect_identical(g(81), "natural")   # Andinean herbaceous/shrubby
  expect_identical(g(34), "water")     # Glacier
  expect_identical(g(68), "other")     # Other natural non-vegetated
  expect_identical(g(74), "anthropic") # Banana (beta)
  # New South-American codes
  expect_identical(g(59), "natural")   # Primary Forest (Chile)
  expect_identical(g(63), "natural")   # Steppe (Chile)
  expect_identical(g(66), "natural")   # Scrubland (Peru)
  expect_identical(g(79), "anthropic") # Pinus plantation (Uruguay code)
  expect_identical(g(72), "anthropic") # Other crops (Peru)
  expect_identical(g(61), "other")     # Salt flat (Peru)
})

test_that("mb_legend does not warn for a valid initiative/collection pair", {
  expect_silent(mb_legend(6, "amazonia"))
  expect_silent(mb_legend(2, "argentina"))
  expect_silent(mb_legend(3, "peru"))
  expect_silent(mb_legend(10, "brazil"))
  # mismatched collection still warns
  expect_warning(mb_legend(9))
})

test_that("wdpa_query_url points at the WDPA FeatureServer query endpoint", {
  u <- wdpa_query_url()
  expect_match(u, "services5\\.arcgis\\.com")
  expect_match(u, "WDPA_v0/FeatureServer/1/query$")
})
