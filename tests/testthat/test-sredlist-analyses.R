# Analyses assimilated from sRedList: AOO lower/upper bounds and the Criterion
# B2 category range (aoo_bounds / iucn_category_B_range), severe fragmentation
# (assess_fragmentation), and countries of occurrence (countries_of_occurrence).
# The bound/range functions are pure and fully offline; fragmentation needs only
# sf; countries needs the optional 'rnaturalearth' package.

make_pts <- function(coords, species = "sp") {
  sf::st_as_sf(
    data.frame(species = species, lon = coords[, 1], lat = coords[, 2]),
    coords = c("lon", "lat"), crs = 4326
  )
}

test_that("aoo_bounds brackets the AOO and yields a B2 category range", {
  # Lower AOO 48 (EN band, < 500) and 900 km^2 of habitat (VU band, < 2000),
  # inside a 6000 km^2 EOO -> AOO bracketed EN..VU.
  b <- aoo_bounds(aoo_lower_km2 = 48, aoh_km2 = 900, eoo_km2 = 6000)
  expect_equal(b$aoo_lower, 48)
  expect_equal(b$aoo_upper, 900)
  expect_equal(b$aoh, 900)
  expect_match(b$cat_lower, "^EN")
  expect_match(b$cat_upper, "^VU")
  expect_equal(b$cat_range, "EN-VU")
})

test_that("aoo_bounds caps the upper bound at the EOO and never below lower", {
  # Habitat (5000) exceeds the EOO (1200): upper is capped at the EOO.
  b <- aoo_bounds(aoo_lower_km2 = 40, aoh_km2 = 5000, eoo_km2 = 1200)
  expect_equal(b$aoo_upper, 1200)
  # Habitat below the lower bound: upper cannot drop below the lower AOO.
  b2 <- aoo_bounds(aoo_lower_km2 = 300, aoh_km2 = 100, eoo_km2 = 5000)
  expect_equal(b2$aoo_upper, 300)
})

test_that("aoo_bounds with no habitat collapses to the lower bound", {
  b <- aoo_bounds(aoo_lower_km2 = 48)
  expect_equal(b$aoo_upper, 48)
  expect_true(is.na(b$aoh))
  expect_equal(b$cat_range, "EN")  # single category, both bounds agree
})

test_that("iucn_category_B_range spans B1 with the AOO bounds", {
  # EOO 6000 -> B1 is VU; AOO 48 -> B2 EN (worst), AOO 900 -> B2 VU (best).
  r <- iucn_category_B_range(eoo_km2 = 6000, aoo_lower_km2 = 48,
                             aoo_upper_km2 = 900)
  expect_equal(r$worst, "EN")   # B1 (VU) vs B2-lower (EN) -> EN
  expect_equal(r$best, "VU")    # B1 (VU) vs B2-upper (VU) -> VU
  expect_equal(r$range, "EN-VU")
})

test_that("iucn_category_B_range collapses when the bounds agree", {
  r <- iucn_category_B_range(eoo_km2 = 3000, aoo_lower_km2 = 400)
  expect_equal(r$range, r$worst)
  expect_equal(r$worst, r$best)
})

test_that("assess_fragmentation clusters occurrences by the isolation distance", {
  # Two tight clusters ~500 km apart: isolated at 50 km, merged at 2000 km.
  cl <- rbind(
    c(-45.00, -10.00), c(-45.01, -10.01), c(-45.02, -10.02),
    c(-40.00, -10.00), c(-40.01, -10.01), c(-40.02, -10.02))
  occ <- make_pts(cl)
  far <- assess_fragmentation(occ, isolation_km = 50)
  expect_equal(far$n_subpop, 2L)
  expect_equal(sum(far$sizes$n_points), 6L)
  near <- assess_fragmentation(occ, isolation_km = 2000)
  expect_equal(near$n_subpop, 1L)
})

test_that("assess_fragmentation returns a curve and a severe-fragmentation flag", {
  cl <- rbind(  # 4 near + 1 isolated
    c(-45, -10), c(-45.01, -10.01), c(-45.02, -10.02), c(-45.03, -10.03),
    c(-40, -10))
  occ <- make_pts(cl)
  fr <- assess_fragmentation(occ, isolation_km = 50, small_size = 1)
  expect_equal(fr$n_subpop, 2L)
  expect_s3_class(fr$curve, "data.frame")
  expect_true(all(c("size", "prop_pop_le") %in% names(fr$curve)))
  # With "small" = 1 occurrence, only the singleton (1/5 of the population) is
  # small, so < 50% -> not suggested severely fragmented.
  expect_false(fr$severe_suggested)
  expect_equal(round(fr$prop_in_small, 2), 0.2)
})

test_that("assess_fragmentation rejects a non-positive isolation distance", {
  occ <- make_pts(rbind(c(-45, -10), c(-44, -10)))
  expect_error(assess_fragmentation(occ, isolation_km = 0))
  expect_error(assess_fragmentation(occ, isolation_km = -5))
})

test_that("calc_aoh returns NA for non-polygon input (points)", {
  occ <- make_pts(rbind(c(-45, -10), c(-44, -10), c(-44.5, -9.5)))
  r <- suppressMessages(calc_aoh(occ))          # points, not a polygon
  expect_true(is.na(r$aoh_km2))
  expect_false(isTRUE(r$elev_applied))
})

test_that("calc_aoh degrades to NA when the land cover is unreadable", {
  cl <- rbind(c(-45, -10), c(-44, -10), c(-44.5, -9.5), c(-44.7, -10.3))
  hull <- calc_eoo(make_pts(cl))$hull
  skip_if(is.null(hull), "EOO hull undefined.")
  r <- suppressWarnings(suppressMessages(
    calc_aoh(hull, src = tempfile(fileext = ".tif"))))  # nonexistent raster
  expect_true(is.na(r$aoh_km2))
})

test_that("elevation_preferences degrades to NA when no DEM is available", {
  occ <- make_pts(rbind(c(-45, -10), c(-44, -10), c(-44.5, -9.5)))
  p <- suppressWarnings(suppressMessages(
    elevation_preferences(occ, src = tempfile(fileext = ".tif"))))
  expect_equal(p$n, 0L)
  expect_true(is.na(p$min) && is.na(p$suggested_min))
})

test_that("fragment_habitat requires a positive density and isolation distance", {
  hull <- calc_eoo(make_pts(rbind(c(-45, -10), c(-44, -10), c(-44.5, -9.5),
                                  c(-44.7, -10.3))))$hull
  skip_if(is.null(hull), "EOO hull undefined.")
  expect_error(fragment_habitat(hull, isolation_km = 20, density = numeric(0)))
  expect_error(fragment_habitat(hull, isolation_km = 0, density = 5))
})

test_that("fragment_habitat degrades to NA when the habitat is unreadable", {
  hull <- calc_eoo(make_pts(rbind(c(-45, -10), c(-44, -10), c(-44.5, -9.5),
                                  c(-44.7, -10.3))))$hull
  skip_if(is.null(hull), "EOO hull undefined.")
  fr <- suppressWarnings(suppressMessages(
    fragment_habitat(hull, isolation_km = 20, density = 5,
                     src = tempfile(fileext = ".tif"))))
  expect_equal(fr$method, "habitat")
  expect_true(is.na(fr$n_subpop) || fr$n_subpop == 0L)
})

test_that("countries_of_occurrence flags Extant vs Possibly Extant", {
  skip_if_not_installed("rnaturalearth")
  skip_if_offline()
  f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
  occ <- read_occurrences(f)
  sp1 <- occ[occ$species == occ$species[1], ]
  hull <- calc_eoo(sp1)$hull
  skip_if(is.null(hull), "EOO hull undefined for the example species.")
  df <- countries_of_occurrence(hull, points = sp1)
  skip_if(nrow(df) == 0, "Natural Earth base map unavailable.")
  expect_true(all(c("country", "presence", "n_records", "realm") %in% names(df)))
  expect_true(all(df$presence %in% c("Extant", "Possibly Extant")))
  # A country holding records must be Extant.
  expect_true(all(df$presence[df$n_records > 0] == "Extant"))
})
