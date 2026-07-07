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
  expect_match(txt, "MapBiomas Collection")
  expect_match(txt, "burned")
  expect_match(txt, "Conservation Units")
  expect_match(txt, "IUCN")
})

test_that("assessment_report omits modules that were not computed", {
  a <- .mk_report_assessment(fire = FALSE, protected = FALSE, mapbiomas = FALSE)
  txt <- assessment_report(a, output = "text")
  expect_false(grepl("MapBiomas Collection", txt))
  expect_false(grepl("burned", txt))
  expect_false(grepl("Conservation Units", txt))
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

test_that("assessment_report validates its inputs", {
  a <- .mk_report_assessment()
  expect_error(assessment_report(a, output = "docx"), "file")
  empty <- structure(list(summary = data.frame(), detail = list(),
                          settings = list()), class = "geoconv_assessment")
  expect_error(assessment_report(empty), "no results")
})
