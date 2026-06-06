test_that("EOO is NA when >= 3 unique points are collinear", {
  # Points sharing one longitude fall on the LAEA central meridian, so their
  # projected coordinates are exactly collinear and the convex hull is a line.
  occ <- sf::st_as_sf(
    data.frame(species = "sp",
               lon = c(-43.0, -43.0, -43.0),
               lat = c(-22.0, -22.3, -22.6)),
    coords = c("lon", "lat"), crs = 4326
  )
  e <- suppressMessages(calc_eoo(occ))
  expect_equal(e$n_unique, 3L)
  expect_true(is.na(e$area_km2))
  expect_null(e$hull)
})

test_that("read_occurrences parses Brazilian-style CSV (; separator, , decimals)", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(
    c("especie;longitude;latitude",
      "Sp A;-43,21;-22,90",
      "Sp A;-43,18;-22,95"),
    tmp
  )
  occ <- suppressMessages(read_occurrences(tmp))

  expect_s3_class(occ, "sf")
  expect_equal(nrow(occ), 2L)
  expect_setequal(unique(occ$species), "Sp A")

  xy <- sf::st_coordinates(occ)
  expect_true(all(xy[, "X"] > -45 & xy[, "X"] < -40))  # decimals parsed, not NA
  expect_true(all(xy[, "Y"] > -25 & xy[, "Y"] < -20))
})

test_that("read_occurrences still reads a plain comma CSV with . decimals", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(
    c("species,lon,lat",
      "Sp B,-43.21,-22.90",
      "Sp B,-43.18,-22.95"),
    tmp
  )
  occ <- suppressMessages(read_occurrences(tmp))
  expect_equal(nrow(occ), 2L)
  expect_setequal(unique(occ$species), "Sp B")
})
