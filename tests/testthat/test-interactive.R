# mas_plotly() and the native plotly builders in R/interactive.R. Uses plain
# summary / detail mocks (no sf/terra) so it runs offline; each block is guarded
# with skip_if_not_installed() so it never fails when a Suggests-only chart
# backend is missing.

# A conversion + protected-area assessment built from summarise_conversion(),
# enough to drive plot_conversion(), plot_protection() and plot_conversion_donut().
.mk_chart_assessment <- function() {
  conv_e <- summarise_conversion(
    data.frame(code = c(3, 15, 24, 33), area_km2 = c(60, 30, 8, 2)))
  conv_a <- summarise_conversion(
    data.frame(code = c(3, 15), area_km2 = c(55, 45)))
  pa <- list(
    eoo_nat_uc_pct = 10, eoo_alt_uc_pct = 64,
    aoo_nat_uc_pct = 27, aoo_alt_uc_pct = 48,
    eoo_pct = 74, aoo_pct = 75, occ_pct = 60,
    n_occ_in = 3L, n_occ = 5L, n_uc = 49L)
  structure(list(
    summary = data.frame(species = "Testus specus", stringsAsFactors = FALSE),
    detail = list("Testus specus" = list(
      year = 2024, eoo_conversion = conv_e, aoo_conversion = conv_a, pa = pa)),
    settings = list(year = 2024, collection = 10, initiative = "brazil")),
    class = "geoconv_assessment")
}

.mk_ts <- function(years = c(2000, 2010, 2020)) {
  n <- length(years)
  ts <- data.frame(
    year     = rep(years, each = 2),
    code     = rep(c(3, 15), n),
    label    = rep(c("Formacao Florestal", "Pastagem"), n),
    class_en = rep(c("Forest Formation", "Pasture"), n),
    group    = rep(c("natural", "anthropic"), n),
    hex      = rep(c("#1f8d49", "#edde8e"), n),
    pct      = as.numeric(rbind(seq(70, 55, length.out = n),
                                seq(30, 45, length.out = n))),
    stringsAsFactors = FALSE)
  attr(ts, "species") <- "Testus specus"; attr(ts, "range") <- "EOO"
  ts
}

.is_plotly <- function(x) inherits(x, "plotly") || inherits(x, "htmlwidget")

test_that("the plot_* builders attach the plotly metadata mas_plotly dispatches on", {
  skip_if_not_installed("ggplot2")
  a <- .mk_chart_assessment()
  expect_identical(attr(plot_conversion(a), "mas")$kind, "bar")
  expect_identical(attr(plot_protection(a), "mas")$kind, "bar")
  expect_identical(attr(plot_timeseries(.mk_ts()), "mas")$kind, "area")
  expect_identical(
    attr(plot_conversion_donut(a, by = "class"), "mas")$kind, "donut")
  fts <- data.frame(year = 2000:2004, burned_pct = c(1, 5, 2, 8, 3),
                    burned_km2 = c(1, 5, 2, 8, 3), total_km2 = 100)
  attr(fts, "species") <- "Testus specus"; attr(fts, "range") <- "EOO"
  expect_identical(attr(plot_fire_timeseries(fts), "mas")$kind, "fire")
})

test_that("mas_plotly builds native plotly widgets for every chart kind", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  a <- .mk_chart_assessment()

  expect_true(.is_plotly(mas_plotly(plot_conversion(a))))          # bar
  expect_true(.is_plotly(mas_plotly(plot_protection(a))))          # bar
  expect_true(.is_plotly(mas_plotly(plot_timeseries(.mk_ts()))))   # area (multi-year)
  expect_true(.is_plotly(                                          # area (single year)
    mas_plotly(plot_timeseries(.mk_ts(years = 2020)))))
  expect_true(.is_plotly(mas_plotly(plot_conversion_donut(a))))    # donut

  fts <- data.frame(year = 2000:2004, burned_pct = c(1, 5, 2, 8, 3),
                    burned_km2 = c(1, 5, 2, 8, 3), total_km2 = 100)
  attr(fts, "species") <- "Testus specus"; attr(fts, "range") <- "EOO"
  expect_true(.is_plotly(mas_plotly(plot_fire_timeseries(fts))))   # fire
})

test_that("mas_plotly falls back to ggplotly for a plain ggplot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
    ggplot2::geom_point() + ggplot2::labs(title = "plain", subtitle = "sub")
  expect_true(.is_plotly(mas_plotly(p)))
})

test_that("mas_plotly returns non-ggplot input unchanged", {
  expect_identical(mas_plotly("not a ggplot"), "not a ggplot")
  expect_null(mas_plotly(NULL))
})
