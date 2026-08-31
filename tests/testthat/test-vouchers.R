# vouchers_from_occ(): derive examined-material strings from occurrence columns.

test_that("builds vouchers from collector + number (+ herbarium)", {
  df <- data.frame(
    species = c("Aus bus", "Aus bus", "Cus dus"),
    collector = c("Barreira", "Silva", "Souza"),
    collectorNumber = c("123", "456", "9"),
    herbarium = c("RB", "R", "SPF"),
    stringsAsFactors = FALSE)
  v <- vouchers_from_occ(df)
  expect_setequal(v, c("Barreira 123 (RB)", "Silva 456 (R)", "Souza 9 (SPF)"))

  # species filter narrows the result
  expect_setequal(vouchers_from_occ(df, species = "Aus bus"),
                  c("Barreira 123 (RB)", "Silva 456 (R)"))
})

test_that("a ready-made voucher column takes precedence", {
  df <- data.frame(
    voucher = c("Barreira 123 (RB)", "Barreira 123 (RB)", ""),
    collector = c("X", "Y", "Z"),
    collectorNumber = c("1", "2", "3"),
    stringsAsFactors = FALSE)
  # duplicates collapsed, blank dropped, voucher column wins over collector cols
  expect_equal(vouchers_from_occ(df), "Barreira 123 (RB)")
})

test_that("collector alone (no number/herbarium) still yields a voucher", {
  df <- data.frame(recordedBy = "Barreira", stringsAsFactors = FALSE)
  expect_equal(vouchers_from_occ(df), "Barreira")
})

test_that("returns empty when no relevant columns are present", {
  df <- data.frame(species = "Aus bus", locality = "Rio",
                   stringsAsFactors = FALSE)
  expect_length(vouchers_from_occ(df), 0L)
  expect_length(vouchers_from_occ(data.frame()), 0L)
})

test_that("case-insensitive detection and overrides work", {
  df <- data.frame(COLETOR = "Barreira", NUMERO = "77",
                   stringsAsFactors = FALSE)
  expect_equal(vouchers_from_occ(df), "Barreira 77")

  # explicit override to a specific column
  df2 <- data.frame(a = "Barreira", b = "77", stringsAsFactors = FALSE)
  expect_equal(
    vouchers_from_occ(df2, collector_col = "a", number_col = "b"),
    "Barreira 77")
})

test_that("accepts an sf and ignores its geometry", {
  skip_if_not_installed("sf")
  df <- data.frame(species = "Aus bus", collector = "Barreira",
                   collectorNumber = "123", lon = -43, lat = -22,
                   stringsAsFactors = FALSE)
  occ <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  expect_equal(vouchers_from_occ(occ), "Barreira 123")
})
