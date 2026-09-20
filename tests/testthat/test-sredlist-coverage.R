# Extra offline unit tests for the sRedList-assimilated analyses, exercising the
# pure branches (no network, no rasters) to keep coverage meaningful.

mkpts <- function(coords, species = "sp") {
  sf::st_as_sf(
    data.frame(species = species, lon = coords[, 1], lat = coords[, 2]),
    coords = c("lon", "lat"), crs = 4326)
}

# ---- aoo_bounds branches ---------------------------------------------------

test_that("aoo_bounds collapses to a single category without a habitat area", {
  b <- aoo_bounds(aoo_lower_km2 = 300)          # aoh NA -> upper = lower
  expect_equal(b$aoo_upper, 300)
  expect_true(is.na(b$aoh))
  expect_match(b$cat_lower, "^EN")              # 300 km^2 -> EN (B2 size)
  expect_equal(b$cat_range, "EN")
})

test_that("aoo_bounds caps the upper bound at the EOO", {
  b <- aoo_bounds(aoo_lower_km2 = 5, aoh_km2 = 3000, eoo_km2 = 2000)
  expect_equal(b$aoo_upper, 2000)               # habitat 3000 capped at EOO 2000
  expect_match(b$cat_lower, "^CR")              # 5 km^2 -> CR (B2 size)
})

# ---- iucn_category_B_range branches ----------------------------------------

test_that("iucn_category_B_range works from the EOO alone", {
  r <- iucn_category_B_range(eoo_km2 = 50)      # CR by B1; AOO unknown
  expect_equal(r$worst, "CR")
  expect_equal(r$best, "CR")
  expect_equal(r$range, "CR")
})

test_that("iucn_category_B_range returns no size flag when nothing qualifies", {
  r <- iucn_category_B_range()                  # all NA
  expect_match(r$worst, "not VU/EN/CR")
  expect_match(r$range, "not VU/EN/CR")
})

test_that("iucn_category_B_range keeps CR when the EOO drives it", {
  r <- iucn_category_B_range(eoo_km2 = 50, aoo_lower_km2 = 5, aoo_upper_km2 = 900)
  expect_equal(r$worst, "CR")
  expect_equal(r$best, "CR")                    # B1 (CR) dominates the upper AOO
})

# ---- assess_fragmentation branches -----------------------------------------

test_that("a single occurrence is one subpopulation", {
  fr <- assess_fragmentation(mkpts(matrix(c(-45, -10), ncol = 2)),
                             isolation_km = 20)
  expect_equal(fr$n_subpop, 1L)
  expect_equal(nrow(fr$sizes), 1L)
  expect_equal(fr$sizes$n_points, 1L)
})

test_that("assess_fragmentation honours per-occurrence weights", {
  occ <- mkpts(rbind(c(-45, -10), c(-45.01, -10.01), c(-40, -10)))
  fr <- assess_fragmentation(occ, isolation_km = 50, weights = c(3, 3, 1))
  expect_equal(fr$n_subpop, 2L)
  expect_equal(sum(fr$sizes$size), 7)           # 3+3 in one cluster, 1 in the other
})

test_that("assess_fragmentation flags severe when all population is small", {
  occ <- mkpts(rbind(c(-45, -10), c(-45.01, -10.01), c(-45.02, -10.02)))
  fr <- assess_fragmentation(occ, isolation_km = 50, small_size = 1000)
  expect_true(fr$severe_suggested)              # single cluster, all <= small
  expect_equal(fr$prop_in_small, 1)
})

# ---- internal helpers ------------------------------------------------------

test_that(".continent_to_realm maps continents to realms", {
  r <- mappingAS:::.continent_to_realm(
    c("South America", "Europe", "Antarctica", NA))
  expect_equal(r[1], "Neotropical")
  expect_equal(r[2], "Palearctic")
  expect_equal(r[3], "Antarctic")
  expect_true(is.na(r[4]))
})

test_that(".first_present returns the first matching name", {
  expect_equal(mappingAS:::.first_present(c("a", "name", "b"), c("x", "name")),
               "name")
  expect_null(mappingAS:::.first_present(c("a", "b"), c("x", "y")))
})

test_that(".resolve_suitable_codes takes explicit codes then natural groups", {
  expect_equal(
    mappingAS:::.resolve_suitable_codes(c(1L, 2L, 2L, NA), NULL, 10, "brazil"),
    c(1L, 2L))
  nat <- mappingAS:::.resolve_suitable_codes(NULL, "natural", 10, "brazil")
  expect_true(length(nat) > 0 && is.integer(nat))
})

test_that("aoo_bounds and iucn_category_B_range agree on the B2 flags", {
  # 48 km^2 lower (EN), 900 km^2 upper (VU): both routes bracket EN..VU.
  b <- aoo_bounds(48, 900, 6000)
  r <- iucn_category_B_range(6000, b$aoo_lower, b$aoo_upper)
  expect_equal(b$cat_range, "EN-VU")
  expect_equal(r$range, "EN-VU")
})
