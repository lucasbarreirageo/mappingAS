# End-to-end coverage of the assess_species() pipeline with mapbiomas = TRUE
# and fire = TRUE, driven entirely by a local synthetic GeoTIFF (no network).
# This is the path the existing suite never exercises: .conversion_for(),
# .fire_for(), the MapBiomas/fire columns in the summary, class_table() on real
# per-class data, export_ranges() with the fire attribute branch, and
# map_static() rendering.

skip_if_not_installed("terra")
skip_if_not_installed("sf")

# Reuse the same synthetic-raster builders as test-raster-local.R.
local_lulc <- function() {
  r <- terra::rast(nrows = 30, ncols = 30,
                   xmin = -43.6, xmax = -43.0, ymin = -23.0, ymax = -22.4,
                   crs = "EPSG:4326")
  set.seed(3)
  terra::values(r) <- sample(c(3, 15, 24, 33, 27), terra::ncell(r),
                             replace = TRUE, prob = c(0.45, 0.3, 0.1, 0.1, 0.05))
  f <- tempfile(fileext = ".tif")
  terra::writeRaster(r, f, datatype = "INT1U", overwrite = TRUE); f
}
local_fire <- function() {
  r <- terra::rast(nrows = 30, ncols = 30,
                   xmin = -43.6, xmax = -43.0, ymin = -23.0, ymax = -22.4,
                   crs = "EPSG:4326")
  set.seed(5)
  terra::values(r) <- sample(c(0, 1, 2, 3), terra::ncell(r), replace = TRUE,
                             prob = c(0.5, 0.25, 0.15, 0.1))
  f <- tempfile(fileext = ".tif")
  terra::writeRaster(r, f, datatype = "INT1U", overwrite = TRUE); f
}

# A 6-point occurrence set for one species, inside the raster footprint.
occ_sf <- function() {
  sf::st_as_sf(
    data.frame(species = "Example atlantica",
               lon = c(-43.5, -43.45, -43.2, -43.15, -43.35, -43.25),
               lat = c(-22.9, -22.75, -22.55, -22.85, -22.6, -22.7)),
    coords = c("lon", "lat"), crs = 4326)
}

test_that("assess_species computes EOO/AOO + MapBiomas conversion locally", {
  lulc <- local_lulc()
  res <- assess_species(occ_sf(), year = 2024, src = lulc,
                        mapbiomas = TRUE, fire = FALSE, verbose = FALSE)
  expect_s3_class(res, "geoconv_assessment")
  expect_equal(nrow(res$summary), 1L)

  s <- res$summary
  expect_true(s$eoo_km2 > 0)
  expect_true(s$aoo_km2 > 0)
  # conversion columns are populated (not NA) because MapBiomas ran
  expect_false(is.na(s$eoo_converted_pct))
  expect_false(is.na(s$eoo_natural_pct))
  expect_true(s$eoo_converted_pct >= 0 && s$eoo_converted_pct <= 100)

  # detail carries the conversion sub-objects
  det <- res$detail[["Example atlantica"]]
  expect_false(is.null(det$eoo_conversion))
  expect_s3_class(det$eoo_conversion$by_class, "data.frame")
})

test_that("assess_species adds fire metrics when fire = TRUE", {
  res <- assess_species(occ_sf(), year = 2024,
                        src = local_lulc(), fire_src = local_fire(),
                        mapbiomas = TRUE, fire = TRUE, verbose = FALSE)
  s <- res$summary
  expect_true("eoo_burned_pct" %in% names(s))
  expect_true("fire_collection" %in% names(s))
  expect_false(is.null(res$detail[[1]]$eoo_fire))
})

test_that("class_table returns real per-class rows from a local assessment", {
  res <- assess_species(occ_sf(), year = 2024, src = local_lulc(),
                        mapbiomas = TRUE, verbose = FALSE)
  ct <- class_table(res, range = "eoo")
  expect_s3_class(ct, "data.frame")
  expect_true(nrow(ct) > 0)
  expect_true(all(c("species", "range", "code", "pct") %in% names(ct)))
  expect_setequal(unique(ct$range), "EOO")
  # percentages within the range sum to ~100
  expect_equal(round(sum(ct$pct)), 100)
})

test_that("export_ranges writes the fire attribute branch (gpkg)", {
  res <- assess_species(occ_sf(), year = 2024,
                        src = local_lulc(), fire_src = local_fire(),
                        mapbiomas = TRUE, fire = TRUE, verbose = FALSE)
  td <- file.path(tempdir(), paste0("expfire_", as.integer(Sys.time())))
  # class_csv = FALSE keeps the class breakdown out entirely; with the default
  # class_csv = TRUE the gpkg format now stores it as an aspatial layer inside
  # the same .gpkg, so a single path is still returned either way.
  gpkg <- export_ranges(res, dir = td, format = "gpkg", aoo_as = "union",
                        class_csv = FALSE)
  expect_length(gpkg, 1L)
  expect_true(file.exists(gpkg))
  eoo <- sf::st_read(gpkg, layer = "eoo", quiet = TRUE)
  # the fire branch in .attr_row adds brnd_pct / fire_col
  expect_true("brnd_pct" %in% names(eoo))
  expect_true("fire_col" %in% names(eoo))
})

test_that("export_ranges can bundle outputs into a zip", {
  skip_if_not(requireNamespace("zip", quietly = TRUE) ||
              nzchar(Sys.which("zip")), "no zip backend available")
  res <- assess_species(occ_sf(), src = local_lulc(),
                        mapbiomas = FALSE, verbose = FALSE)
  td <- file.path(tempdir(), paste0("expzip_", as.integer(Sys.time())))
  z <- export_ranges(res, dir = td, format = "shapefile", zip = TRUE)
  expect_true(file.exists(z))
  expect_match(z, "\\.zip$")
})

test_that("map_static renders a ggplot from a local assessment", {
  skip_if_not_installed("ggplot2")
  res <- assess_species(occ_sf(), year = 2024, src = local_lulc(),
                        mapbiomas = TRUE, verbose = FALSE)
  m <- map_static(res, mapbiomas = TRUE, src = local_lulc())
  expect_s3_class(m, "ggplot")
})

test_that("map_static without the raster layer still draws points/EOO/AOO", {
  skip_if_not_installed("ggplot2")
  res <- assess_species(occ_sf(), src = local_lulc(),
                        mapbiomas = FALSE, verbose = FALSE)
  m <- map_static(res, mapbiomas = FALSE)
  expect_s3_class(m, "ggplot")
})

test_that("timeseries_for_species runs through a local assessment EOO", {
  res <- assess_species(occ_sf(), src = local_lulc(),
                        mapbiomas = FALSE, verbose = FALSE)
  ts <- timeseries_for_species(res, range = "eoo", years = c(2023, 2024),
                               src = local_lulc(), verbose = FALSE)
  expect_s3_class(ts, "data.frame")
  expect_identical(attr(ts, "range"), "EOO")
})
