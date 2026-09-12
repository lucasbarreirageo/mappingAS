# plot_conversion_donut() / .donut_data(): the twin-donut composition chart.
# Runs offline from summarise_conversion() mocks (no sf/terra).

.mk_donut_assessment <- function(tiny = FALSE) {
  # A <1% class (code 30, mining) triggers the "Other (<1%)" lumping branch.
  ca_e <- if (tiny)
    data.frame(code = c(3, 15, 30), area_km2 = c(600, 390, 1))
  else
    data.frame(code = c(3, 15, 24), area_km2 = c(60, 30, 10))
  structure(list(
    summary = data.frame(species = "Testus specus", stringsAsFactors = FALSE),
    detail = list("Testus specus" = list(
      eoo_conversion = summarise_conversion(ca_e),
      aoo_conversion = summarise_conversion(
        data.frame(code = c(3, 15), area_km2 = c(55, 45))))),
    settings = list(year = 2024, collection = 10, initiative = "brazil")),
    class = "geoconv_assessment")
}

test_that("plot_conversion_donut returns a ggplot by class and by group", {
  skip_if_not_installed("ggplot2")
  a <- .mk_donut_assessment()
  expect_s3_class(plot_conversion_donut(a, by = "class"), "ggplot")
  expect_s3_class(plot_conversion_donut(a, by = "group"), "ggplot")
})

test_that("plot_conversion_donut supports Portuguese labels", {
  skip_if_not_installed("ggplot2")
  a <- .mk_donut_assessment()
  p <- plot_conversion_donut(a, by = "group", lang = "pt")
  expect_s3_class(p, "ggplot")
  # the donut metadata carries the tidy data used by the plotly version
  expect_true(all(c("range", "label", "pct", "hex") %in% names(attr(p, "mas")$df)))
})

test_that("plot_conversion_donut lumps sub-1% classes into an 'Other' slice", {
  skip_if_not_installed("ggplot2")
  a <- .mk_donut_assessment(tiny = TRUE)
  df <- attr(plot_conversion_donut(a, by = "class"), "mas")$df
  expect_true(any(grepl("Other", as.character(df$label))))
})

test_that("plot_conversion_donut errors when no land-cover data is present", {
  a <- structure(list(
    summary = data.frame(species = "Testus specus", stringsAsFactors = FALSE),
    detail = list("Testus specus" = list(eoo_conversion = NULL,
                                          aoo_conversion = NULL)),
    settings = list(year = 2024, collection = 10, initiative = "brazil")),
    class = "geoconv_assessment")
  expect_error(suppressWarnings(plot_conversion_donut(a)))
})
