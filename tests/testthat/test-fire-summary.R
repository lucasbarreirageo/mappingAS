test_that("summarise_fire computes burned area, pct and frequency stats", {
  fa <- data.frame(freq = c(0, 1, 2, 4), area_km2 = c(50, 30, 15, 5))
  # explicit denominator
  s <- summarise_fire(fa, range_km2 = 100)
  expect_equal(s$burned_km2, 50)          # freq > 0 -> 30 + 15 + 5
  expect_equal(s$total_km2, 100)
  expect_equal(s$burned_pct, 50)
  expect_equal(s$freq_max, 4L)
  # weighted mean frequency = (1*30 + 2*15 + 4*5) / 50 = 1.6
  expect_equal(round(s$freq_mean, 4), 1.6)
  # by_freq drops the unburned (freq == 0) row and is sorted ascending
  expect_false(0 %in% s$by_freq$freq)
  expect_equal(s$by_freq$freq, sort(s$by_freq$freq))
})

test_that("summarise_fire falls back to the tabulated total when range is NULL", {
  fa <- data.frame(freq = c(0, 1), area_km2 = c(80, 20))
  s <- summarise_fire(fa)               # no range_km2
  expect_equal(s$total_km2, 100)        # 80 + 20
  expect_equal(s$burned_pct, 20)
})

test_that("summarise_fire handles a completely unburned range", {
  fa <- data.frame(freq = 0, area_km2 = 100)
  s <- summarise_fire(fa, range_km2 = 100)
  expect_equal(s$burned_km2, 0)
  expect_equal(s$burned_pct, 0)
  expect_true(is.na(s$freq_mean))
  expect_true(is.na(s$freq_max))
  expect_equal(nrow(s$by_freq), 0L)
})

test_that("summarise_fire validates its input columns", {
  expect_error(summarise_fire(data.frame(x = 1, y = 2)))
})

test_that(".fire_freq_bins returns six labelled, coloured bins", {
  b <- mappingAS:::.fire_freq_bins()
  expect_equal(nrow(b), 6L)
  expect_named(b, c("lower", "upper", "label", "hex"))
  expect_identical(b$label, c("1", "2-3", "4-6", "7-10", "11-20", "21-40"))
  expect_true(all(grepl("^#", b$hex)))
  # bins are contiguous and increasing
  expect_true(all(b$lower <= b$upper))
  expect_true(all(diff(b$lower) > 0))
})

test_that("%||% returns the fallback only for NULL or scalar NA", {
  `%or%` <- mappingAS:::`%||%`
  expect_equal(1 %or% 2, 1)
  expect_equal(NULL %or% 2, 2)
  expect_equal(NA %or% 2, 2)            # scalar NA triggers the fallback
  expect_equal("x" %or% "y", "x")
})
