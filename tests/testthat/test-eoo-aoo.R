make_pts <- function(coords) {
  sf::st_as_sf(
    data.frame(species = "sp", lon = coords[, 1], lat = coords[, 2]),
    coords = c("lon", "lat"), crs = 4326
  )
}

test_that("EOO is NA for fewer than 3 unique points", {
  p1 <- make_pts(matrix(c(-43, -22), ncol = 2))
  expect_true(is.na(suppressMessages(calc_eoo(p1))$area_km2))
  expect_null(suppressMessages(calc_eoo(p1))$hull)

  p2 <- make_pts(matrix(c(-43, -22, -43.5, -22.5), ncol = 2, byrow = TRUE))
  expect_true(is.na(suppressMessages(calc_eoo(p2))$area_km2))
})

test_that("EOO is a positive area for >= 3 non-collinear points (spheroid)", {
  skip_if_not_installed("units")
  skip_if_not_installed("lwgeom")
  sq <- matrix(c(-43.5, -22.5,
                 -43.0, -22.5,
                 -43.0, -22.0,
                 -43.5, -22.0), ncol = 2, byrow = TRUE)
  e <- calc_eoo(make_pts(sq))
  expect_true(is.finite(e$area_km2) && e$area_km2 > 0)
  expect_s3_class(e$hull, "sfc")
  # ~0.5 deg x ~0.5 deg near 22S is on the order of a few thousand km^2
  expect_gt(e$area_km2, 1000)
  expect_lt(e$area_km2, 5000)
})

test_that("AOO equals number of occupied cells times cell area", {
  set.seed(1)
  sq <- matrix(c(-43.5, -22.5,
                 -43.0, -22.5,
                 -43.0, -22.0,
                 -43.5, -22.0), ncol = 2, byrow = TRUE)
  a <- calc_aoo(make_pts(sq), cell_km = 2)
  expect_equal(a$area_km2, a$n_cells * 4)
  expect_gte(a$n_cells, 1)
  expect_s3_class(a$cells, "sfc")

  # single point -> exactly one cell, regardless of grid translations
  one <- calc_aoo(make_pts(matrix(c(-43, -22), ncol = 2)), cell_km = 2)
  expect_equal(one$n_cells, 1)
  expect_equal(one$area_km2, 4)
})

test_that("AOO minimum-over-grids never exceeds the origin grid", {
  set.seed(42)
  sq <- matrix(c(-43.5, -22.5,
                 -43.0, -22.5,
                 -43.0, -22.0,
                 -43.5, -22.0,
                 -43.25, -22.25), ncol = 2, byrow = TRUE)
  a_origin <- calc_aoo(make_pts(sq), cell_km = 2, n_rep = 1)   # origin grid only
  a_min    <- calc_aoo(make_pts(sq), cell_km = 2, n_rep = 50)  # ConR-style minimum
  expect_lte(a_min$n_cells, a_origin$n_cells)
  expect_equal(a_min$area_km2, a_min$n_cells * 4)
})

test_that("iucn_category_B applies size thresholds", {
  cats <- iucn_category_B(eoo_km2 = 50, aoo_km2 = 8)
  expect_match(cats$eoo_category, "^CR")
  expect_match(cats$aoo_category, "^CR")
  expect_match(iucn_category_B(eoo_km2 = 3000)$eoo_category, "^EN")
  expect_match(iucn_category_B(aoo_km2 = 1500)$aoo_category, "^VU")
})
