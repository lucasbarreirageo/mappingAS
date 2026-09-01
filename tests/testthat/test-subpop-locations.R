make_pts <- function(coords, species = "sp") {
  sf::st_as_sf(
    data.frame(species = species, lon = coords[, 1], lat = coords[, 2]),
    coords = c("lon", "lat"), crs = 4326
  )
}

test_that("a single occupied point is one subpopulation and one location", {
  p1 <- make_pts(matrix(c(-43, -22), ncol = 2))
  s <- calc_subpop(p1)
  l <- calc_locations(p1)
  expect_equal(s$n_subpop, 1L)
  expect_equal(l$n_locations, 1L)
  expect_equal(l$n_cells, 1L)
})

test_that("well-separated clusters are counted as distinct subpopulations", {
  # two tight clusters ~200 km apart; a small buffer keeps them separate
  cl <- rbind(
    c(-43.00, -22.00), c(-43.02, -22.01), c(-42.99, -22.02),
    c(-41.00, -22.00), c(-41.02, -22.01), c(-40.99, -22.02)
  )
  s <- calc_subpop(make_pts(cl), resol_km = 5)
  expect_equal(s$n_subpop, 2L)
  expect_s3_class(s$subpop, "sfc")

  # a buffer wide enough to bridge the gap merges them into one
  s1 <- calc_subpop(make_pts(cl), resol_km = 200)
  expect_equal(s1$n_subpop, 1L)
})

test_that("default subpopulation radius is 1/10 of the max inter-point distance", {
  cl <- rbind(c(-43.0, -22.0), c(-42.0, -22.0), c(-42.5, -21.5))
  s <- calc_subpop(make_pts(cl))
  expect_true(is.finite(s$resol_km) && s$resol_km > 0)
  expect_gte(s$n_subpop, 1L)
  expect_lte(s$n_subpop, nrow(cl))
})

test_that("locations use a 10 km grid and the minimum over translations", {
  set.seed(1)
  sq <- matrix(c(-43.5, -22.5,
                 -43.0, -22.5,
                 -43.0, -22.0,
                 -43.5, -22.0,
                 -43.25, -22.25), ncol = 2, byrow = TRUE)
  l_origin <- calc_locations(make_pts(sq), grid_km = 10, n_rep = 1)
  l_min    <- calc_locations(make_pts(sq), grid_km = 10, n_rep = 50)
  expect_lte(l_min$n_locations, l_origin$n_locations)
  expect_equal(l_min$n_locations, l_min$n_cells)
  expect_s3_class(l_min$cells, "sfc")
  expect_gte(l_min$grid_km, 10)
})

test_that("sliding-scale cell size overrides the fixed grid", {
  set.seed(1)
  sq <- matrix(c(-43.5, -22.5,
                 -43.0, -22.5,
                 -43.0, -22.0,
                 -43.5, -22.0,
                 -43.25, -22.25), ncol = 2, byrow = TRUE)
  p <- make_pts(sq)
  l_fixed <- calc_locations(p, grid_km = 10)
  l_scale <- calc_locations(p, cell_scale = 0.05)
  # 5% of a ~70 km max distance ~ 3.5 km cells, i.e. smaller than 10 km
  expect_lt(l_scale$grid_km, 10)
  expect_gte(l_scale$n_locations, 1L)
  expect_equal(l_fixed$grid_km, 10)
})

test_that("protected areas decouple inside/outside locations", {
  # two occurrences within one grid cell (~1 km apart)
  pp <- rbind(c(-43.000, -22.000), c(-43.005, -22.000))
  pts <- make_pts(pp)

  # a protected polygon covering ONLY the first point
  pa <- sf::st_sf(
    pa_name = "PA1",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(-43.002, -22.002), c(-42.998, -22.002),
      c(-42.998, -21.998), c(-43.002, -21.998),
      c(-43.002, -22.002)))), crs = 4326))

  # no PA: both points share one 10 km cell -> 1 location
  expect_equal(calc_locations(pts, grid_km = 10)$n_locations, 1L)

  # no_more_than_one: 1 PA (the inside point) + 1 outside cell = 2
  l1 <- calc_locations(pts, grid_km = 10, protected = pa,
                       method_protected = "no_more_than_one")
  expect_equal(l1$n_locations, 2L)
  expect_equal(l1$n_in, 1L)
  expect_equal(l1$n_out, 1L)
  expect_s3_class(l1$pa_locations, "sfc")

  # other: inside gridded separately from outside -> also 2 here
  l2 <- calc_locations(pts, grid_km = 10, protected = pa,
                       method_protected = "other")
  expect_equal(l2$n_locations, 2L)
  expect_null(l2$pa_locations)
})

test_that("estimates run on the packaged example data", {
  f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
  skip_if(!nzchar(f))
  occ <- read_occurrences(f)
  sp1 <- occ[occ$species == occ$species[1], ]
  s <- calc_subpop(sp1)
  l <- calc_locations(sp1)
  expect_true(is.integer(s$n_subpop) && s$n_subpop >= 1L)
  expect_true(is.integer(l$n_locations) && l$n_locations >= 1L)
})
