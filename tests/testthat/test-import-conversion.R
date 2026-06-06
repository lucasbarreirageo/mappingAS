test_that("read_occurrences imports the bundled example", {
  f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
  skip_if(f == "", "example file not installed")
  occ <- read_occurrences(f)
  expect_s3_class(occ, "sf")
  expect_true("species" %in% names(occ))
  expect_equal(length(unique(occ$species)), 2)
  expect_equal(nrow(occ), 10)
  expect_equal(sf::st_crs(occ)$epsg, 4326L)
})

test_that("invalid coordinates are dropped", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    scientificName = c("a", "a", "a"),
    decimalLongitude = c(-43, 999, -42.5),
    decimalLatitude = c(-22, -22, -22.5)
  ), tmp, row.names = FALSE)
  occ <- suppressMessages(read_occurrences(tmp))
  expect_equal(nrow(occ), 2)
})

test_that("summarise_conversion computes percentages correctly", {
  ca <- data.frame(code = c(3, 15, 24, 33), area_km2 = c(60, 30, 10, 5))
  s <- summarise_conversion(ca, water_in_denominator = FALSE)
  # natural = 60; anthropic = 30 + 10 = 40; terrestrial denom = 100
  expect_equal(s$natural_km2, 60)
  expect_equal(s$anthropic_km2, 40)
  expect_equal(s$converted_pct, 40)
  expect_equal(s$natural_pct, 60)
  # total denom excludes not-observed only -> 105
  expect_equal(round(s$converted_pct_total, 2), round(40 / 105 * 100, 2))
})

test_that("water can be moved into the denominator", {
  ca <- data.frame(code = c(3, 15, 33), area_km2 = c(50, 50, 100))
  s <- summarise_conversion(ca, water_in_denominator = TRUE)
  # denom = natural(50) + anthropic(50) + water(100) = 200
  expect_equal(s$converted_pct, 25)
})
