test_that(".nice_len snaps to a 1/2/5 x 10^k step", {
  # logic: p = 10^floor(log10(x)); f = x/p; multiplier = 5 if f>=5, 2 if f>=2, else 1
  nice_len <- mappingAS:::.nice_len
  expect_equal(nice_len(1),    1)    # p=1,   f=1   -> 1
  expect_equal(nice_len(1.5),  1)    # p=1,   f=1.5 -> 1
  expect_equal(nice_len(3),    2)    # p=1,   f=3   -> 2
  expect_equal(nice_len(7),    5)    # p=1,   f=7   -> 5
  expect_equal(nice_len(45),   20)   # p=10,  f=4.5 -> 2*10
  expect_equal(nice_len(120), 100)   # p=100, f=1.2 -> 1*100
  expect_equal(nice_len(600), 500)   # p=100, f=6   -> 5*100
})

test_that(".plot_bbox unions geometries and pads the extent", {
  plot_bbox <- mappingAS:::.plot_bbox
  g1 <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  g2 <- sf::st_sfc(sf::st_point(c(10, 5)), crs = 4326)
  bb <- plot_bbox(list(g1, g2, NULL))   # NULL is skipped
  expect_named(bb, c("xmin", "xmax", "ymin", "ymax"))
  expect_lt(bb[["xmin"]], 0)            # padded outwards
  expect_gt(bb[["xmax"]], 10)
  expect_lt(bb[["ymin"]], 0)
  expect_gt(bb[["ymax"]], 5)
})

test_that(".crs_label describes EPSG and LAEA projections", {
  crs_label <- mappingAS:::.crs_label
  lab_epsg <- crs_label(4326)
  expect_true(is.character(lab_epsg))
  expect_match(lab_epsg, "4326")

  laea <- "+proj=laea +lat_0=-22 +lon_0=-43 +datum=WGS84 +units=m +no_defs"
  lab_laea <- crs_label(laea)
  expect_match(lab_laea, "LAEA")   # compact label: projection name + datum

  # an invalid CRS yields NULL rather than an error
  expect_null(crs_label(NA))
})

test_that(".scalebar_layer and .north_layer build ggplot annotation lists", {
  skip_if_not_installed("ggplot2")
  scalebar <- mappingAS:::.scalebar_layer
  north    <- mappingAS:::.north_layer
  bb <- c(xmin = 0, xmax = 100000, ymin = 0, ymax = 80000)  # metres

  sb <- scalebar(bb)
  expect_type(sb, "list")
  expect_gt(length(sb), 0)

  # with a CRS reference label, extra annotation(s) are appended
  sb_ref <- scalebar(bb, ref = "EPSG:4326")
  expect_gte(length(sb_ref), length(sb))

  nl <- north(bb)
  expect_type(nl, "list")
  expect_gt(length(nl), 0)
})
