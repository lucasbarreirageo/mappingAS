test_that("mb_source_url builds the expected MapBiomas LULC URL", {
  u <- mb_source_url(2024, 10)
  expect_type(u, "character")
  expect_length(u, 1L)
  expect_match(u, "collection_10")
  expect_match(u, "brazil_coverage_2024\\.tif$")
  # collection number is interpolated
  expect_match(mb_source_url(2020, 9), "collection_9")
  expect_match(mb_source_url(2020, 9), "brazil_coverage_2020\\.tif$")
})

test_that("mb_fire_url switches product paths and interpolates collections", {
  acc <- mb_fire_url(product = "accumulated")
  ann <- mb_fire_url(2024, "annual")
  frq <- mb_fire_url(product = "frequency", period = c(1985, 2024))

  expect_match(acc, "accumulated_burned")
  expect_match(acc, "1985_2024\\.tif$")
  expect_match(ann, "annual_burned")
  expect_match(ann, "annual_burned_2024\\.tif$")
  expect_match(frq, "fire_frequency")
  expect_match(frq, "1985_2024\\.tif$")

  # host_collection and fire_collection are both reflected in the path
  u <- mb_fire_url(2024, "annual", fire_collection = 4, host_collection = 9)
  expect_match(u, "collection_9/fire-col4")
  expect_match(u, "fire_col4_br_annual_burned_2024")
})

test_that("mb_fire_url rejects an unknown product", {
  expect_error(mb_fire_url(product = "nope"))
})

test_that("mb_palette returns named hex colours keyed as requested", {
  # default: all classes, names are codes
  p_all <- mb_palette()
  expect_type(p_all, "character")
  expect_true(all(grepl("^#", p_all)))
  expect_true("3" %in% names(p_all))

  # subset of codes, names are codes
  p <- mb_palette(c(3, 15, 39, 33))
  expect_length(p, 4L)
  expect_identical(unname(p[as.character(3)]), "#1f8d49")

  # names can be the English/Portuguese class labels instead
  p_en <- mb_palette(c(3, 15), by = "class_en")
  expect_true(all(c("Forest Formation", "Pasture") %in% names(p_en)))
  p_pt <- mb_palette(c(3, 15), by = "class_pt")
  expect_true(all(c("Formacao Florestal", "Pastagem") %in% names(p_pt)))

  # unknown codes are dropped, not turned into NA rows
  p_mixed <- mb_palette(c(3, 999999))
  expect_length(p_mixed, 1L)
})

test_that("mb_years spans the documented collection range", {
  y <- mb_years(10)
  expect_type(y, "integer")
  expect_equal(min(y), 1985L)
  expect_equal(max(y), 2024L)
  expect_true(all(diff(y) == 1L))
})

test_that("mb_legend warns but still works for an unshipped collection", {
  expect_warning(leg <- mb_legend(9))
  expect_s3_class(leg, "data.frame")
  expect_true(nrow(leg) > 0)
})

test_that("fire_palette returns n graded hex colours", {
  p <- fire_palette(6)
  expect_length(p, 6L)
  expect_true(all(grepl("^#", p)))
  expect_length(fire_palette(3), 3L)
})
