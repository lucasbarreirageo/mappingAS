# Multi-initiative support (Brazil / Amazonia / Colombia) and the standardised
# legend. These are pure-URL / pure-table checks (no network).

test_that("mb_initiatives lists the three products with sane metadata", {
  ini <- mb_initiatives()
  expect_setequal(names(ini), c("brazil", "amazonia", "colombia"))
  expect_identical(ini$brazil$collection, 10L)
  expect_identical(ini$amazonia$collection, 6L)
  expect_identical(ini$colombia$collection, 3L)
  expect_equal(range(ini$brazil$years), c(1985L, 2024L))
  expect_equal(range(ini$amazonia$years), c(1986L, 2023L))
  expect_equal(range(ini$colombia$years), c(1985L, 2024L))
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

  # collection defaults follow the initiative when NULL
  expect_match(mb_source_url(2023, collection = NULL, initiative = "amazonia"),
               "collection_6")
})

test_that("friendly initiative aliases resolve", {
  expect_match(mb_source_url(2023, initiative = "pan-amazonia"), "amazon")
  expect_match(mb_source_url(2024, initiative = "brasil"), "brazil_coverage")
})

test_that("mb_years follows the initiative span", {
  expect_equal(range(mb_years(initiative = "amazonia")), c(1986L, 2023L))
  expect_equal(range(mb_years(initiative = "colombia")), c(1985L, 2024L))
  # positional collection still returns Brazil default
  expect_equal(range(mb_years(10)), c(1985L, 2024L))
})

test_that("standardised legend adds Colombia/Amazonia classes without breaking Brazil", {
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
  # new standardised (Colombia/Amazonia) codes present and grouped
  expect_identical(g(81), "natural")   # Andinean herbaceous/shrubby
  expect_identical(g(82), "natural")   # Flooded Andinean
  expect_identical(g(34), "water")     # Glacier
  expect_identical(g(68), "other")     # Other natural non-vegetated
  expect_identical(g(74), "anthropic") # Banana (beta)
})

test_that("mb_legend does not warn for a valid initiative/collection pair", {
  expect_silent(mb_legend(6, "amazonia"))
  expect_silent(mb_legend(3, "colombia"))
  expect_silent(mb_legend(10, "brazil"))
  # mismatched collection still warns
  expect_warning(mb_legend(9))
})

test_that("wdpa_query_url points at the WDPA FeatureServer query endpoint", {
  u <- wdpa_query_url()
  expect_match(u, "services5\\.arcgis\\.com")
  expect_match(u, "WDPA_v0/FeatureServer/1/query$")
})
