# factsheet_html(): standalone HTML factsheet with user metadata + photos.
# Uses a plain summary data frame (plus an optional per-class detail), so it
# runs without sf/terra. Chart rendering is best-effort and skipped silently
# when a graphics device is unavailable, so the string assertions below do not
# depend on it.

.mk_factsheet_assessment <- function(mapbiomas = TRUE, with_detail = FALSE) {
  sm <- data.frame(
    species = "Testus specus", n_records = 10L, n_unique = 8L,
    eoo_km2 = 1234.56, aoo_km2 = 48, aoo_cells = 12L,
    eoo_converted_pct = 42.3, eoo_natural_pct = 57.7,
    aoo_converted_pct = 25.1, aoo_natural_pct = 74.9,
    eoo_cat_B1 = "EN (B1 size)", aoo_cat_B2 = "EN (B2 size)",
    provisional_cat = "EN (B1 size)",
    mapbiomas_year = 2024, mapbiomas_collection = 10,
    stringsAsFactors = FALSE)
  detail <- list()
  if (with_detail) {
    byc <- data.frame(
      code = c(3L, 15L, 24L, 33L),
      area_km2 = c(60, 120, 25, 5),
      group = c("natural", "anthropic", "anthropic", "water"),
      class_en = c("Forest", "Pasture", "Urban area", "River"),
      class_pt = c("Floresta", "Pastagem", "Area urbana", "Rio"),
      hex = c("#1f8d49", "#edde8e", "#af2a2a", "#0000ff"),
      stringsAsFactors = FALSE)
    detail[["Testus specus"]] <- list(
      eoo_conversion = list(by_class = byc),
      aoo_conversion = list(by_class = byc))
  }
  structure(list(
    summary = sm, detail = detail,
    settings = list(year = 2024, collection = 10, cell_km = 2,
                    mapbiomas = mapbiomas, fire = FALSE, protected = FALSE)),
    class = "geoconv_assessment")
}

test_that("factsheet_html embeds user metadata, vouchers and a reference link", {
  a <- .mk_factsheet_assessment()
  html <- factsheet_html(
    a, family = "Araceae", authority = "(Engl.) Croat",
    countries = "Brazil", system = "Terrestrial",
    habitat = "Rocky outcrops", biome = "Atlantic Forest",
    vegetation = "Rupicolous herb",
    land_use = "Pasture and urban expansion",
    conservation_units = "PARNA da Tijuca",
    vouchers = c("Barreira 123 (RB)", "Silva 456 (R)"),
    reference = "https://reflora.jbrj.gov.br/x")

  expect_type(html, "character")
  expect_match(html, "<!DOCTYPE html>", fixed = TRUE)
  expect_match(html, "Testus specus")
  expect_match(html, "Araceae")
  expect_match(html, "Atlantic Forest")
  expect_match(html, "Rocky outcrops")
  expect_match(html, "Barreira 123 \\(RB\\)")
  # A URL reference becomes a link.
  expect_match(html, "href='https://reflora.jbrj.gov.br/x'")
  # Genus defaults to the first word of the species name.
  expect_match(html, "<i>Testus</i>")
})

test_that("factsheet_html watermarks photos with the owner name", {
  a <- .mk_factsheet_assessment()
  img <- tempfile(fileext = ".png")
  writeBin(as.raw(c(1, 2, 3, 4, 5, 6)), img)
  html <- factsheet_html(a, photos = img, photo_credit = "A. L. Barreira")
  expect_match(html, "fs-wm")
  expect_match(html, "A. L. Barreira")
  expect_match(html, "data:image/png;base64,")
})

test_that("factsheet_html writes a file when `file` is given", {
  a <- .mk_factsheet_assessment()
  out <- tempfile(fileext = ".html")
  res <- factsheet_html(a, file = out)
  expect_identical(res, out)
  expect_true(file.exists(out))
  expect_match(paste(readLines(out), collapse = "\n"), "Testus specus")
})

test_that("factsheet_html supports Portuguese labels", {
  a <- .mk_factsheet_assessment()
  html <- factsheet_html(a, lang = "pt", family = "Araceae",
                         countries = "Brasil", biome = "Mata Atlantica")
  expect_match(html, "Taxonomia")
  expect_match(html, "Informacoes de apoio")
  expect_match(html, "Familia")
  expect_match(html, "Paises")
})

test_that(".factsheet_threats ranks the top anthropic classes", {
  a <- .mk_factsheet_assessment(with_detail = TRUE)
  det <- a$detail[["Testus specus"]]
  thr <- mappingAS:::.factsheet_threats(det, lang = "en", range = "eoo",
                                        top_n = 5L)
  expect_s3_class(thr, "data.frame")
  # Anthropic classes only, ordered by area (Pasture > Urban area).
  expect_identical(thr$label, c("Pasture", "Urban area"))
  # Percentage is of the terrestrial (natural + anthropic) area.
  expect_equal(thr$pct[1], 100 * 120 / (60 + 120 + 25), tolerance = 1e-6)
})

test_that(".base64_encode matches known RFC 4648 vectors", {
  b64 <- function(s) mappingAS:::.base64_encode(charToRaw(s))
  expect_identical(b64("Man"), "TWFu")
  expect_identical(b64("Ma"), "TWE=")
  expect_identical(b64("M"), "TQ==")
  expect_identical(b64("foobar"), "Zm9vYmFy")
  expect_identical(mappingAS:::.base64_encode(raw(0)), "")
})
