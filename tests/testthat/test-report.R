# assessment_report(): narrative report in html / text / docx. Uses a plain
# summary data frame, so it runs without sf/terra/officer.

.mk_report_assessment <- function(fire = FALSE, protected = FALSE,
                                  mapbiomas = TRUE) {
  sm <- data.frame(
    species = "Testus specus", n_records = 10L, n_unique = 8L,
    eoo_km2 = 1234.56, aoo_km2 = 48, aoo_cells = 12L,
    eoo_converted_pct = 42.3, eoo_natural_pct = 57.7,
    aoo_converted_pct = 25.1, aoo_natural_pct = 74.9,
    eoo_cat_B1 = "EN (B1 size)", aoo_cat_B2 = "EN (B2 size)",
    provisional_cat = "EN (B1 size)",
    mapbiomas_year = 2024, mapbiomas_collection = 10,
    stringsAsFactors = FALSE)
  if (fire) {
    sm$eoo_burned_pct <- 30.5; sm$aoo_burned_pct <- 12.0; sm$fire_collection <- 4
  }
  if (protected) {
    sm$occ_in_uc_pct <- 90; sm$eoo_uc_pct <- 34.4
    sm$aoo_uc_pct <- 91.7; sm$n_uc <- 53L
  }
  structure(list(
    summary = sm, detail = list(),
    settings = list(year = 2024, collection = 10, cell_km = 2,
                    mapbiomas = mapbiomas, fire = fire,
                    fire_collection = 4, protected = protected)),
    class = "geoconv_assessment")
}

test_that("assessment_report renders HTML and text with key facts", {
  a <- .mk_report_assessment(fire = TRUE, protected = TRUE)
  html <- assessment_report(a, output = "html")
  expect_type(html, "character")
  expect_match(html, "Testus specus")
  expect_match(html, "EN")

  txt <- assessment_report(a, output = "text")
  expect_match(txt, "Extent of Occurrence")
  expect_match(txt, "MapBiomas Brazil Collection")
  expect_match(txt, "burned")
  expect_match(txt, "protected areas")
  expect_match(txt, "IUCN")
})

test_that("assessment_report omits modules that were not computed", {
  a <- .mk_report_assessment(fire = FALSE, protected = FALSE, mapbiomas = FALSE)
  txt <- assessment_report(a, output = "text")
  expect_false(grepl("MapBiomas Collection", txt))
  expect_false(grepl("burned", txt))
  expect_false(grepl("protected areas", txt))
})

test_that("assessment_report supports Portuguese", {
  a <- .mk_report_assessment()
  pt <- assessment_report(a, lang = "pt", output = "text")
  expect_match(pt, "Avaliacao")
  expect_match(pt, "Extensao de Ocorrencia")
})

test_that("assessment_report writes a .docx when officer is available", {
  skip_if_not_installed("officer")
  a <- .mk_report_assessment(fire = TRUE, protected = TRUE)
  f <- tempfile(fileext = ".docx"); on.exit(unlink(f))
  out <- assessment_report(a, output = "docx", file = f)
  expect_identical(out, f)
  expect_true(file.exists(f))
  expect_gt(file.info(f)$size, 0)
})

test_that("assessment_report weaves in cover and fire trends when supplied", {
  a <- .mk_report_assessment(fire = TRUE, protected = TRUE)
  cover <- data.frame(
    year  = rep(c(1985, 2024), each = 2),
    group = rep(c("natural", "anthropic"), 2),
    pct   = c(45, 55, 55, 45))          # converted (terrestrial) falls 55% -> 45%
  attr(cover, "range") <- "eoo"
  fire <- data.frame(year = 2010:2014, burned_pct = c(0.1, 2.6, 0.2, 0.3, 1.0))
  attr(fire, "range") <- "eoo"
  txt <- assessment_report(a, output = "text",
                           cover_series = cover, fire_series = fire)
  expect_match(txt, "1985-2024")                       # trend window
  expect_match(txt, "forest transition")               # net decline interpretation
  expect_match(txt, "2012")                             # fire peak year listed
})

test_that("assessment_report reports per-class regression trends when a class series is supplied", {
  a <- .mk_report_assessment(fire = FALSE, protected = FALSE)
  yrs <- seq(1985, 2020, by = 5)
  n <- length(yrs)
  cover <- data.frame(
    year     = rep(yrs, 2),
    label    = rep(c("Formacao Florestal", "Pastagem"), each = n),
    class_en = rep(c("Forest Formation", "Pasture"), each = n),
    group    = rep(c("natural", "anthropic"), each = n),
    pct      = c(seq(70, 55, length.out = n), seq(30, 45, length.out = n)),
    stringsAsFactors = FALSE)
  attr(cover, "range") <- "eoo"
  txt <- assessment_report(a, output = "text", cover_series = cover)
  expect_match(txt, "Per-class linear trends")
  expect_match(txt, "pp/yr")
  expect_match(txt, "Pasture")          # a fast-changing class is named (EN labels)
})

test_that("assessment_report reports protection effectiveness from the UC list", {
  a <- .mk_report_assessment(protected = TRUE)
  a$detail <- list(`Testus specus` = list(pa = list(
    list = data.frame(
      pa_name     = c("Parque X", "APA Y", "RPPN Z"),
      pa_category = c("Parque", "Area de Protecao Ambiental", "RPPN"),
      pa_group    = c("Protecao Integral", "Uso Sustentavel", "Uso Sustentavel"),
      n_occ       = c(8L, 1L, 0L),
      overlap_eoo_km2 = c(50, 30, 5),
      stringsAsFactors = FALSE))))
  txt <- assessment_report(a, output = "text")
  expect_match(txt, "Parque X")                        # dominant UC named
  expect_match(txt, "number of locations")             # concentration interpretation
})

test_that("assessment_report reports EOO and AOO trends from a list of series", {
  a <- .mk_report_assessment(fire = FALSE, protected = FALSE)
  mk <- function(rng, p1985, p2024) {
    d <- data.frame(year = rep(c(1985, 2024), each = 2),
                    group = rep(c("natural", "anthropic"), 2),
                    pct = c(100 - p1985, p1985, 100 - p2024, p2024))
    attr(d, "range") <- rng; d
  }
  covers <- list(mk("eoo", 57, 52), mk("aoo", 30, 25))
  txt <- assessment_report(a, output = "text", cover_series = covers)
  expect_match(txt, "EOO")
  expect_match(txt, "AOO")
  # both per-range trend sentences present (two distinct windows described)
  expect_gt(lengths(regmatches(txt, gregexpr("terrestrial converted fraction", txt)))[1], 1)
})

test_that("assessment_report includes recommendations and threat synthesis", {
  a <- .mk_report_assessment(fire = TRUE, protected = TRUE)
  txt <- assessment_report(a, output = "text")
  expect_match(txt, "RECOMMENDATIONS FOR FORMAL ASSESSMENT")
  expect_match(txt, "continuing decline", ignore.case = TRUE)
})

test_that("assessment_report validates its inputs", {
  a <- .mk_report_assessment()
  expect_error(assessment_report(a, output = "docx"), "file")
  empty <- structure(list(summary = data.frame(), detail = list(),
                          settings = list()), class = "geoconv_assessment")
  expect_error(assessment_report(empty), "no results")
})
