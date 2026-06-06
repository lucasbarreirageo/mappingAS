test_that("class_table returns per-class rows for EOO and AOO", {
  fake <- structure(
    list(
      summary = data.frame(species = "sp1", stringsAsFactors = FALSE),
      detail = list(sp1 = list(
        eoo_conversion = summarise_conversion(
          data.frame(code = c(3, 15, 33), area_km2 = c(60, 30, 10))),
        aoo_conversion = summarise_conversion(
          data.frame(code = c(3, 24), area_km2 = c(8, 2)))
      )),
      settings = list(year = 2024, collection = 10, backend = "local")
    ),
    class = "geoconv_assessment"
  )

  ct <- class_table(fake)
  expect_true(all(c("species", "range", "code", "class_pt", "class_en",
                    "group", "area_km2", "pct") %in% names(ct)))
  expect_setequal(unique(ct$range), c("EOO", "AOO"))
  # EOO had 100 km2 total -> Forest Formation (code 3) = 60%
  eoo3 <- ct[ct$range == "EOO" & ct$code == 3, ]
  expect_equal(eoo3$pct, 60)
})

test_that("class_table warns and returns empty when no MapBiomas data", {
  fake <- structure(
    list(summary = data.frame(species = "sp1"),
         detail = list(sp1 = list(eoo_conversion = NULL, aoo_conversion = NULL)),
         settings = list(year = 2024, collection = 10, backend = "local")),
    class = "geoconv_assessment")
  expect_warning(out <- class_table(fake))
  expect_equal(nrow(out), 0)
})

test_that("plot_timeseries renders without error", {
  ts <- data.frame(
    year     = rep(c(2000, 2010), each = 2),
    code     = c(3, 15, 3, 15),
    label    = c("Formacao Florestal", "Pastagem",
                 "Formacao Florestal", "Pastagem"),
    class_en = c("Forest Formation", "Pasture",
                 "Forest Formation", "Pasture"),
    group    = c("natural", "anthropic", "natural", "anthropic"),
    hex      = c("#1f8d49", "#edde8e", "#1f8d49", "#edde8e"),
    pct      = c(70, 30, 60, 40),
    stringsAsFactors = FALSE
  )
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- plot_timeseries(ts)
    expect_s3_class(p, "ggplot")
  } else {
    tmp <- tempfile(fileext = ".pdf")
    grDevices::pdf(tmp)
    on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
    expect_silent(plot_timeseries(ts))
  }
})

test_that("plot_timeseries handles a non-rectangular series (missing class-years)", {
  # 'Pastagem' is absent in 2000 -> would break geom_area without completion
  ts <- data.frame(
    year     = c(2000, 2000, 2010),
    code     = c(3, 33, 15),
    label    = c("Formacao Florestal", "Rio, Lago e Oceano", "Pastagem"),
    class_en = c("Forest Formation", "River, Lake and Ocean", "Pasture"),
    group    = c("natural", "water", "anthropic"),
    hex      = c("#1f8d49", "#2532e4", "#edde8e"),
    pct      = c(90, 10, 50),
    stringsAsFactors = FALSE
  )
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    expect_s3_class(plot_timeseries(ts), "ggplot")
  } else {
    tmp <- tempfile(fileext = ".pdf")
    grDevices::pdf(tmp)
    on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
    expect_silent(plot_timeseries(ts))
  }
})
