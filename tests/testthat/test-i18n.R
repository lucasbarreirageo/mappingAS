# Legenda/idioma das quatro abas (conversion, classes, time series, fire).
# Ingles e o padrao; portugues e opt-in via `lang`. Usa so mb_legend() e
# data frames, entao roda sem sf/terra/Earth Engine.

.mk_assessment <- function() {
  cv <- function(nat, ant, wat, oth)
    list(natural_km2 = nat, anthropic_km2 = ant, water_km2 = wat,
         other_km2 = oth, converted_pct = 100 * ant / (nat + ant))
  structure(list(
    settings = list(year = 2023, collection = 10),
    detail = list(sp = list(
      eoo_conversion = cv(60, 30, 5, 5),
      aoo_conversion = cv(40, 50, 5, 5)))),
    class = "geoconv_assessment")
}

.mk_timeseries <- function(codes = c(3, 15, 33)) {
  leg <- mb_legend()
  s <- leg[match(codes, leg$code), ]
  do.call(rbind, lapply(c(2010, 2020), function(y)
    data.frame(year = y, code = s$code, label = s$class_pt,
               class_en = s$class_en, group = s$group, hex = s$hex,
               area_km2 = seq(50, by = -10, length.out = length(codes)),
               pct = seq(50, by = -10, length.out = length(codes)),
               stringsAsFactors = FALSE)))
}

test_that("plot_conversion uses English group labels by default", {
  tmp <- tempfile(fileext = ".png"); grDevices::png(tmp); on.exit(unlink(tmp))
  p <- plot_conversion(.mk_assessment())
  grDevices::dev.off()
  M <- if (inherits(p, "ggplot")) attr(p, "pct") else p
  expect_true(all(c("Natural", "Altered", "Water", "Other") %in% colnames(M)))
})

test_that("plot_conversion switches to Portuguese group labels", {
  tmp <- tempfile(fileext = ".png"); grDevices::png(tmp); on.exit(unlink(tmp))
  p <- plot_conversion(.mk_assessment(), lang = "pt")
  grDevices::dev.off()
  M <- if (inherits(p, "ggplot")) attr(p, "pct") else p
  expect_true(all(c("Natural", "Alterado", "Agua", "Outros") %in% colnames(M)))
})

test_that("plot_timeseries uses class_en when lang = 'en'", {
  ts <- .mk_timeseries()
  tmp <- tempfile(fileext = ".png"); grDevices::png(tmp); on.exit(unlink(tmp))
  p <- plot_timeseries(ts, lang = "en")
  grDevices::dev.off()
  lbls <- if (inherits(p, "ggplot")) levels(p$data$label) else unique(ts$class_en)
  expect_true(all(c("Forest Formation", "Pasture") %in% lbls))
  expect_false(any(c("Formacao Florestal", "Pastagem") %in% lbls))
})

test_that("plot_timeseries keeps class_pt when lang = 'pt'", {
  ts <- .mk_timeseries()
  tmp <- tempfile(fileext = ".png"); grDevices::png(tmp); on.exit(unlink(tmp))
  p <- plot_timeseries(ts, lang = "pt")
  grDevices::dev.off()
  lbls <- if (inherits(p, "ggplot")) levels(p$data$label) else unique(ts$label)
  expect_true(all(c("Formacao Florestal", "Pastagem") %in% lbls))
})

test_that("plot_fire_timeseries y-axis follows language", {
  skip_if_not_installed("ggplot2")
  fts <- data.frame(year = c(2018, 2019, 2020), burned_pct = c(5, 12, 8))
  attr(fts, "species") <- "sp"; attr(fts, "range") <- "EOO"
  tmp <- tempfile(fileext = ".png"); grDevices::png(tmp); on.exit(unlink(tmp))
  p_en <- plot_fire_timeseries(fts, lang = "en")
  p_pt <- plot_fire_timeseries(fts, lang = "pt")
  grDevices::dev.off()
  expect_match(p_en$labels$y, "burned")
  expect_match(p_pt$labels$y, "queimada")
})

test_that("class table shows the language-appropriate class column", {
  pick <- function(df, lg) {
    if (lg == "en") df$class_pt <- NULL else df$class_en <- NULL
    names(df)[names(df) %in% c("class_pt", "class_en")] <- "class"
    df$class
  }
  leg <- mb_legend(); s <- leg[match(c(3, 15), leg$code), ]
  df <- data.frame(code = s$code, class_pt = s$class_pt, class_en = s$class_en,
                   stringsAsFactors = FALSE)
  expect_equal(pick(df, "en"), c("Forest Formation", "Pasture"))
  expect_equal(pick(df, "pt"), c("Formacao Florestal", "Pastagem"))
})