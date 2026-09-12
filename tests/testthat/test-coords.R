test_that(".parse_dms reads sexagesimal coordinates into decimal degrees", {
  # Southern / western hemispheres are negative.
  expect_equal(mappingAS:::.parse_dms("15 46 48 S"),
               -(15 + 46 / 60 + 48 / 3600), tolerance = 1e-9)
  expect_equal(mappingAS:::.parse_dms("47 55 45 W"),
               -(47 + 55 / 60 + 45 / 3600), tolerance = 1e-9)
  # Northern / eastern hemispheres stay positive.
  expect_equal(mappingAS:::.parse_dms("23 27 30 N"),
               23 + 27 / 60 + 30 / 3600, tolerance = 1e-9)
  expect_equal(mappingAS:::.parse_dms("10 30 0 E"), 10.5, tolerance = 1e-9)
})

test_that(".parse_dms honours the symbol spelling and a leading minus", {
  expect_equal(mappingAS:::.parse_dms("23°27'30\"S"),
               -(23 + 27 / 60 + 30 / 3600), tolerance = 1e-9)
  expect_equal(mappingAS:::.parse_dms("-23 27 30"),
               -(23 + 27 / 60 + 30 / 3600), tolerance = 1e-9)
})

test_that(".parse_dms tolerates partial and bare-decimal input", {
  # Degrees only, minutes/seconds default to zero.
  expect_equal(mappingAS:::.parse_dms("10 N"), 10, tolerance = 1e-9)
  # Degrees + minutes, no seconds.
  expect_equal(mappingAS:::.parse_dms("10 30 S"), -10.5, tolerance = 1e-9)
  # A plain decimal degree passes straight through.
  expect_equal(mappingAS:::.parse_dms("23.4583"), 23.4583, tolerance = 1e-9)
})

test_that(".parse_dms returns NA for empty or non-numeric input", {
  expect_true(is.na(mappingAS:::.parse_dms("")))
  expect_true(is.na(mappingAS:::.parse_dms("   ")))
  expect_true(is.na(mappingAS:::.parse_dms(NULL)))
  expect_true(is.na(mappingAS:::.parse_dms("abc")))
})

test_that(".utm_to_lonlat round-trips a known WGS84 point", {
  skip_if_not_installed("sf")
  # Brasília (~ -47.9292, -15.7801) -> project to UTM 23S -> back to lon/lat.
  lon <- -47.9292; lat <- -15.7801
  p <- sf::st_transform(
    sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326), 32723)
  xy <- sf::st_coordinates(p)
  out <- mappingAS:::.utm_to_lonlat(xy[1, 1], xy[1, 2], zone = 23, hemi = "S")
  expect_length(out, 2L)
  expect_equal(out[1], lon, tolerance = 1e-4)
  expect_equal(out[2], lat, tolerance = 1e-4)
})

test_that(".utm_to_lonlat handles the northern hemisphere", {
  skip_if_not_installed("sf")
  lon <- 11.0; lat <- 47.0
  p <- sf::st_transform(
    sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326), 32632)
  xy <- sf::st_coordinates(p)
  out <- mappingAS:::.utm_to_lonlat(xy[1, 1], xy[1, 2], zone = 32, hemi = "N")
  expect_equal(out[1], lon, tolerance = 1e-4)
  expect_equal(out[2], lat, tolerance = 1e-4)
})

test_that(".utm_to_lonlat returns c(NA, NA) for invalid input", {
  expect_equal(mappingAS:::.utm_to_lonlat(NA, 1e6, 23, "S"),
               c(NA_real_, NA_real_))
  expect_equal(mappingAS:::.utm_to_lonlat("x", 1e6, 23, "S"),
               c(NA_real_, NA_real_))
  expect_equal(mappingAS:::.utm_to_lonlat(2e5, 8e6, 0, "S"),
               c(NA_real_, NA_real_))
  expect_equal(mappingAS:::.utm_to_lonlat(2e5, 8e6, 61, "S"),
               c(NA_real_, NA_real_))
})
