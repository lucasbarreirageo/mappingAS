test_that("laea_crs centres a LAEA proj string on the data", {
  pts <- sf::st_as_sf(
    data.frame(lon = c(-43.5, -43.0, -42.5), lat = c(-22.5, -22.0, -23.0)),
    coords = c("lon", "lat"), crs = 4326
  )
  s <- laea_crs(pts)
  expect_type(s, "character")
  expect_match(s, "\\+proj=laea")
  expect_match(s, "\\+lat_0=-22")     # mean latitude ~ -22.5
  expect_match(s, "\\+lon_0=-43")     # mean longitude ~ -43.0
  expect_match(s, "\\+units=m")
})

test_that("laea_crs assigns 4326 when the CRS is missing", {
  g <- sf::st_sfc(sf::st_point(c(-43, -22)), sf::st_point(c(-42, -23)))
  # no crs set
  expect_silent(s <- laea_crs(g))
  expect_match(s, "\\+proj=laea")
})

test_that(".match_col is case-insensitive and returns the original name", {
  match_col <- mappingAS:::.match_col
  nms <- c("ScientificName", "DecimalLongitude", "DecimalLatitude")
  expect_identical(match_col(nms, c("scientificname")), "ScientificName")
  expect_identical(match_col(nms, c("decimallongitude")), "DecimalLongitude")
  # first candidate that matches wins
  expect_identical(match_col(nms, c("missing", "decimallatitude")),
                   "DecimalLatitude")
  # no match -> NA
  expect_true(is.na(match_col(nms, c("nope"))))
})

test_that(".coord_candidates lists the documented header synonyms", {
  cand <- mappingAS:::.coord_candidates()
  expect_named(cand, c("lon", "lat", "species"))
  expect_true("decimalLongitude" %in% cand$lon)
  expect_true("decimalLatitude" %in% cand$lat)
  expect_true("scientificName" %in% cand$species)
  expect_true("especie" %in% cand$species)   # Portuguese synonym
})

test_that(".assert_points accepts point sf and rejects everything else", {
  assert_points <- mappingAS:::.assert_points
  pts <- sf::st_as_sf(data.frame(lon = -43, lat = -22),
                      coords = c("lon", "lat"), crs = 4326)
  expect_true(assert_points(pts))

  # not an sf
  expect_error(assert_points(data.frame(x = 1)), "must be an sf")

  # polygon geometry, not points
  poly <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0)))), crs = 4326))
  expect_error(assert_points(poly), "POINT")
})

test_that(".is_collinear flags straight lines and clears triangles", {
  is_collinear <- mappingAS:::.is_collinear
  # fewer than 3 points -> treated as collinear
  expect_true(is_collinear(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)))
  # three points on x = const -> perfectly collinear
  line <- cbind(c(0, 0, 0), c(0, 1, 2))
  expect_true(is_collinear(line))
  # a proper triangle -> not collinear
  tri <- cbind(c(0, 1, 0), c(0, 0, 1))
  expect_false(is_collinear(tri))
})
