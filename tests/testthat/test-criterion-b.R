# iucn_criterion_B(): provisional Criterion B category from range size plus the
# sub-criteria (a) fragmentation/few locations, (b) decline, (c) fluctuation.
# Pure function, fully offline.

test_that("size threshold plus >=2 sub-criteria yields a threatened category", {
  # CR EOO (< 100) with fragmentation (a) and decline (b) -> CR B1ab
  r <- iucn_criterion_B(eoo_km2 = 50, aoo_km2 = 5000,
                        severe_fragmentation = TRUE, decline = TRUE)
  expect_equal(r$category, "CR")
  expect_match(r$code, "^CR B1")
  expect_true(grepl("a", r$code) && grepl("b", r$code))
  expect_true(r$a && r$b)
})

test_that("the B-axes reflect which of EOO (B1) and AOO (B2) meet the size", {
  # Both EOO and AOO in the EN band, two sub-criteria -> EN B1+2ab
  r <- iucn_criterion_B(eoo_km2 = 4000, aoo_km2 = 400,
                        decline = TRUE, extreme_fluctuation = TRUE)
  expect_equal(r$category, "EN")
  expect_match(r$code, "B1\\+2")
  expect_true(r$b && r$c)
})

test_that("few locations drives sub-criterion (a)", {
  # VU by AOO size, few locations (a) + decline (b) -> VU B2ab
  r <- iucn_criterion_B(aoo_km2 = 1500, n_locations = 8, decline = TRUE)
  expect_equal(r$category, "VU")
  expect_match(r$code, "^VU B2")
  expect_true(r$a)
})

test_that("size met but fewer than two sub-criteria yields NT", {
  r <- iucn_criterion_B(eoo_km2 = 50, decline = TRUE)  # only (b)
  expect_equal(r$category, "NT")
  expect_match(r$code, "size only")
  expect_equal(r$qualifies_size, "VU")
})

test_that("ranges above every threshold yield LC", {
  r <- iucn_criterion_B(eoo_km2 = 1e6, aoo_km2 = 1e5,
                        severe_fragmentation = TRUE, decline = TRUE)
  expect_equal(r$category, "LC")
  expect_true(is.na(r$qualifies_size))
})

test_that("no EOO and no AOO yields NA (not assessable by size)", {
  r <- iucn_criterion_B()
  expect_true(is.na(r$category))
  expect_true(is.na(r$code))
})

test_that("s2_years returns the Esri/Sentinel-2 coverage years as integers", {
  y <- s2_years()
  expect_type(y, "integer")
  expect_true(all(c(2017L, 2023L) %in% y))
})
