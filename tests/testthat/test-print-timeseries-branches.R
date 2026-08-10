# A minimal fake assessment object reused across tests.
fake_assessment <- function(with_fire = FALSE) {
  summ <- data.frame(
    species = "sp1", n_records = 5L, n_unique = 5L,
    eoo_km2 = 1234.5, aoo_km2 = 16, aoo_cells = 4L,
    eoo_converted_pct = 30, eoo_natural_pct = 70,
    aoo_converted_pct = 25, aoo_natural_pct = 75,
    eoo_cat_B1 = "EN (B1 size)", aoo_cat_B2 = "EN (B2 size)",
    provisional_cat = "EN (B1 size)",
    mapbiomas_year = 2024, mapbiomas_collection = 10,
    stringsAsFactors = FALSE
  )
  if (with_fire) {
    summ$eoo_burned_pct <- 12
    summ$aoo_burned_pct <- 8
    summ$fire_collection <- 4
  }
  structure(
    list(summary = summ, detail = list(sp1 = list()),
         settings = list(year = 2024, collection = 10, backend = "local",
                         cell_km = 2, mapbiomas = TRUE, fire = with_fire,
                         fire_collection = 4)),
    class = "geoconv_assessment"
  )
}

test_that("print.geoconv_assessment prints a header and the summary", {
  a <- fake_assessment()
  out <- capture.output(print(a))
  expect_true(any(grepl("mappingAS assessment", out)))
  expect_true(any(grepl("Collection 10", out)))
  expect_true(any(grepl("1 species assessed", out)))
  expect_true(any(grepl("sp1", out)))
  # print returns its argument invisibly
  expect_invisible(print(a))
})

test_that("print.geoconv_assessment handles an empty assessment", {
  a <- structure(
    list(summary = data.frame(), detail = list(),
         settings = list(year = 2024, collection = 10, backend = "local",
                         cell_km = 2)),
    class = "geoconv_assessment")
  out <- capture.output(print(a))
  expect_true(any(grepl("0 species assessed", out)))
})

test_that("plot_fire_timeseries renders for a normal series", {
  ts <- data.frame(year = c(2018, 2019, 2020),
                   burned_km2 = c(1, 2, 0.5),
                   burned_pct = c(10, 20, 5))
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- plot_fire_timeseries(ts)
    expect_s3_class(p, "ggplot")
  } else {
    tmp <- tempfile(fileext = ".pdf"); grDevices::pdf(tmp)
    on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
    expect_silent(plot_fire_timeseries(ts))
  }
})

test_that("plot_fire_timeseries builds a title from attributes", {
  skip_if_not_installed("ggplot2")
  ts <- data.frame(year = 2020, burned_km2 = 1, burned_pct = 10)
  attr(ts, "species") <- "sp1"; attr(ts, "range") <- "EOO"
  p <- plot_fire_timeseries(ts)
  # The title is a plotmath expression (genus/epithet italic, rest normal), so
  # deparse it before matching.
  ttl <- paste(deparse(p$labels$title), collapse = " ")
  expect_true(grepl("sp1", ttl))
  expect_true(grepl("EOO", ttl))
})

test_that("plot_fire_timeseries validates required columns", {
  expect_error(plot_fire_timeseries(data.frame(year = 1)))
})

test_that(".complete_years makes a series rectangular with zero fill", {
  complete_years <- mappingAS:::.complete_years
  res <- data.frame(
    year = c(2000, 2000, 2010),
    label = c("Forest", "Water", "Pasture"),
    code = c(3, 33, 15), class_en = c("Forest", "Water", "Pasture"),
    group = c("natural", "water", "anthropic"),
    hex = c("#1f8d49", "#2532e4", "#edde8e"),
    area_km2 = c(90, 10, 50), pct = c(90, 10, 100),
    stringsAsFactors = FALSE
  )
  out <- complete_years(res)
  # every (year, label) combination is now present: 2 years x 3 labels
  expect_equal(nrow(out), 6L)
  # the originally-missing cells are zero-filled
  pasture_2000 <- out[out$year == 2000 & out$label == "Pasture", ]
  expect_equal(pasture_2000$area_km2, 0)
  expect_equal(pasture_2000$pct, 0)
})

test_that("timeseries_for_species rejects non-assessment input", {
  expect_error(timeseries_for_species(list()), "geoconv_assessment")
})

test_that("timeseries_for_species errors for an unknown species", {
  a <- fake_assessment()
  expect_error(timeseries_for_species(a, species = "ghost"),
               "not found")
})

test_that("fire_timeseries_for_species rejects non-assessment input", {
  expect_error(fire_timeseries_for_species(list()), "geoconv_assessment")
})

test_that("fire_timeseries_for_species errors for an unknown species", {
  a <- fake_assessment(with_fire = TRUE)
  expect_error(fire_timeseries_for_species(a, species = "ghost"), "not found")
})
