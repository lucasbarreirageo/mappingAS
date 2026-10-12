mk <- function(x, y) sf::st_as_sf(data.frame(x = x, y = y), coords = c("x", "y"), crs = 4326)
sq <- function(x0, y0, s) sf::st_polygon(list(cbind(x0 + c(0, s, s, 0, 0), y0 + c(0, 0, s, s, 0))))

test_that("AOO and locations are reproducible without a seed", {
  p <- mk(-43 + (1:60 %% 7) * 0.04 + (1:60) * 1e-3, -22 + (1:60 %% 5) * 0.05)
  expect_length(unique(replicate(5, calc_aoo(p)$n_cells)), 1L)
  expect_length(unique(replicate(5, calc_locations(p)$n_locations)), 1L)
})

test_that("grid search never does worse than the origin grid", {
  p <- mk(-43 + (1:40) * 0.013, -22 + (1:40 %% 4) * 0.02)
  expect_lte(calc_aoo(p, n_rep = 30)$n_cells, calc_aoo(p, n_rep = 1)$n_cells)
})

test_that("EOO below AOO is raised to AOO (IUCN 4.10)", {
  expect_equal(iucn_category_B(50, 200)$eoo_category, "EN (B1 size)")
  expect_equal(iucn_criterion_B(50, 200, n_locations = 1)$category, "EN")
  expect_equal(iucn_category_B_range(50, 200, 300)$b1_category, "EN (B1 size)")
})

test_that("iucn_category_B is vectorised and matches the scalar results", {
  e <- c(50, 6000, NA, 30000); a <- c(8, 600, 1500, NA)
  v <- iucn_category_B(e, a)
  s <- lapply(seq_along(e), function(i) iucn_category_B(e[i], a[i]))
  expect_identical(v$combined, vapply(s, `[[`, "", "combined"))
  expect_identical(v$combined, c("CR (B1 size)", "VU (B1 size)", "VU (B2 size)",
                                 "not VU/EN/CR by B1 size"))
  expect_equal(iucn_category_B(100, 10)$combined, "EN (B1 size)")  # strict "<"
})

test_that("criterion B code repeats sub-criteria per axis (IUCN notation)", {
  expect_equal(iucn_criterion_B(3000, 400, n_locations = 4,
                                decline_detail = "iii")$code, "EN B1ab(iii)+2ab(iii)")
  expect_equal(iucn_criterion_B(3000, 4000, n_locations = 4)$code, "EN B1ab")
})

test_that("nested protected areas count as one location", {
  pa <- sf::st_sfc(sq(-43.5, -22.5, 1), sq(-43.2, -22.2, .2), crs = 4326)
  pts <- mk(c(-43.1, -43.05, -43.3), c(-22.1, -22.05, -22.4))
  expect_equal(calc_locations(pts, protected = pa)$n_locations, 1L)
  disj <- sf::st_sfc(sq(-43.5, -22.5, .2), sq(-43.2, -22.2, .2), crs = 4326)
  expect_equal(calc_locations(mk(c(-43.4, -43.1), c(-22.4, -22.1)),
                              protected = disj)$n_locations, 2L)
})

test_that(".min_cover picks the smallest covering set", {
  expect_equal(sort(.min_cover(list(c(1L, 2L), 2L, c(2L, 3L)))), 2L)
  expect_equal(sort(.min_cover(list(1L, 3L))), c(1L, 3L))
  expect_length(.min_cover(list()), 0L)
})

test_that("land-cover year defaults to the latest published one", {
  expect_equal(.lc_year(NULL, "brazil"), 2024L)
  expect_equal(.lc_year(NULL, "sentinel2"), 2023L)
  expect_equal(.lc_year(2010, "brazil"), 2010L)
  expect_null(.elev_mask(NULL, NULL, NA, NA))
})

test_that("habitat decline is inferred only from a significant downward trend", {
  fake <- function(nat) function(geom, year, ...) list(natural_km2 = nat(year))
  g <- sf::st_sfc(sq(-43, -22, 1), crs = 4326)
  local_mocked_bindings(.conversion_for = fake(function(y) 1000 - 5 * (y - 2014)))
  tr <- .habitat_trend(g, 2024, 10, "brazil", "local", NULL)
  expect_true(tr$declining)
  expect_lt(tr$slope_pct_yr, 0)
  expect_length(tr$years, 6L)
  local_mocked_bindings(.conversion_for = fake(function(y) 1000))
  expect_false(.habitat_trend(g, 2024, 10, "brazil", "local", NULL)$declining)
  local_mocked_bindings(.conversion_for = fake(function(y) 1000 + 3 * (y - 2014)))
  expect_false(.habitat_trend(g, 2024, 10, "brazil", "local", NULL)$declining)
  # a single-year local file cannot give a trend
  expect_false(.habitat_trend(g, 2024, 10, "brazil", "local", "x.tif")$declining)
})

test_that("static converted area no longer implies a continuing decline", {
  f <- system.file("extdata", "mini_mapbiomas.tif", package = "mappingAS")
  e <- as.vector(terra::ext(terra::rast(f)))
  occ <- mk(e[1] + (e[2] - e[1]) * c(.2, .4, .6, .8),
            e[3] + (e[4] - e[3]) * c(.3, .7, .6, .4))
  s <- suppressWarnings(assess_species(occ, src = f, subpop = FALSE,
                                       assume_decline = FALSE, verbose = FALSE))$summary
  expect_equal(s$decline_basis, "not documented")
  expect_true(all(c("habitat_trend_pct_yr", "habitat_trend_p") %in% names(s)))
})

test_that("calc_aoh stays within the pixel budget without modal bias", {
  f <- system.file("extdata", "mini_mapbiomas.tif", package = "mappingAS")
  g <- sf::st_as_sfc(sf::st_bbox(terra::ext(terra::rast(f)) * 0.9, crs = 4326))
  full <- calc_aoh(g, src = f, year = 2024)$aoh_km2
  small <- calc_aoh(g, src = f, year = 2024, max_pixels = 100)$aoh_km2
  expect_true(is.finite(small))
  expect_lt(abs(small - full) / full, 0.25)
})

test_that("ranges crossing the antimeridian are handled on the short side", {
  p <- mk(c(179, -179, 179.5, -179.5, 179.9), c(-16, -16, -18, -18, -17))
  expect_match(laea_crs(p), "lon_0=(-)?1[78][0-9]\\.")
  ref <- mk(c(-1, 1, -0.5, 0.5, -0.1), c(-16, -16, -18, -18, -17))
  old <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old)))
  expect_equal(suppressMessages(calc_eoo(p)$area_km2),
               suppressMessages(calc_eoo(ref)$area_km2), tolerance = 0.01)
  expect_equal(.unwrap_lon(cbind(c(-43, -42), 0))[, 1], c(-43, -42))
})

test_that("fire is skipped per species outside Brazil under initiative = 'auto'", {
  p <- mk(c(10, 10.2, 10.1), c(45, 45.1, 45.2))
  expect_message(
    s <- assess_species(cbind(p, species = "eu"), initiative = "auto",
                        mapbiomas = FALSE, fire = TRUE, subpop = FALSE)$summary,
    "fire skipped")
  expect_true(is.na(s$eoo_burned_pct))
})
