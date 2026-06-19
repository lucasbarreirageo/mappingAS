# The raster-tabulation paths (mb_class_areas_raster, fire_areas,
# summarise_conversion <SpatRaster>, mb_raster_local <local src>) need a raster
# but NOT the network. A tiny synthetic MapBiomas-style GeoTIFF ships in
# inst/extdata/ (mini_mapbiomas.tif, ~0.5 KB). It is NOT real MapBiomas data:
# just a 20x20 grid of integer class codes (3/15/33/27) with the same structure
# the local backend expects, so these tests stay offline and deterministic.
#
# A fire raster has no shipped fixture (fire codes are just integers too), so it
# is built on the fly with terra. Both paths fall back to terra generation if
# the shipped file is missing, so the suite never hard-depends on the fixture.

skip_if_not_installed("terra")

# --- fixtures ---------------------------------------------------------------

# Shipped LULC fixture; regenerate with terra only if it is not installed.
lulc_tif <- function() {
  f <- system.file("extdata", "mini_mapbiomas.tif", package = "mappingAS")
  if (nzchar(f) && file.exists(f)) return(f)
  r <- terra::rast(nrows = 20, ncols = 20,
                   xmin = -43.5, xmax = -43.0, ymin = -22.5, ymax = -22.0,
                   crs = "EPSG:4326")
  set.seed(7)
  terra::values(r) <- sample(c(3, 15, 33, 27), terra::ncell(r), replace = TRUE,
                             prob = c(0.5, 0.3, 0.15, 0.05))
  g <- tempfile(fileext = ".tif")
  terra::writeRaster(r, g, datatype = "INT1U", overwrite = TRUE); g
}

# Fire fixture is always generated (small integer raster, no shipped file).
make_fire_tif <- function() {
  r <- terra::rast(nrows = 20, ncols = 20,
                   xmin = -43.5, xmax = -43.0, ymin = -22.5, ymax = -22.0,
                   crs = "EPSG:4326")
  set.seed(11)
  terra::values(r) <- sample(c(0, 1, 2, 4), terra::ncell(r), replace = TRUE,
                             prob = c(0.4, 0.3, 0.2, 0.1))
  f <- tempfile(fileext = ".tif")
  terra::writeRaster(r, f, datatype = "INT1U", overwrite = TRUE)
  f
}

aoi_poly <- function() {
  sf::st_sfc(sf::st_polygon(list(rbind(
    c(-43.4, -22.4), c(-43.1, -22.4),
    c(-43.1, -22.1), c(-43.4, -22.1), c(-43.4, -22.4)))), crs = 4326)
}

# --- mb_class_areas_raster --------------------------------------------------

test_that("mb_class_areas_raster tabulates positive area per code", {
  r <- terra::rast(lulc_tif())
  ca <- mb_class_areas_raster(r)
  expect_s3_class(ca, "data.frame")
  expect_named(ca, c("code", "area_km2"))
  expect_type(ca$code, "integer")
  expect_true(all(ca$area_km2 > 0))
  expect_true(all(ca$code %in% c(3, 15, 33, 27)))
})

# --- summarise_conversion on a SpatRaster -----------------------------------

test_that("summarise_conversion accepts a SpatRaster directly", {
  r <- terra::rast(lulc_tif())
  s <- summarise_conversion(r)
  expect_true(is.list(s))
  expect_true(s$natural_km2 > 0)
  expect_true(s$anthropic_km2 > 0)
  expect_true(s$converted_pct >= 0 && s$converted_pct <= 100)
  expect_s3_class(s$by_class, "data.frame")
})

# --- mb_raster_local with a local src (no network) --------------------------

test_that("mb_raster_local crops a local GeoTIFF to the AOI", {
  rc <- mb_raster_local(aoi_poly(), src = lulc_tif(), cache = FALSE)
  expect_s4_class(rc, "SpatRaster")
  expect_identical(names(rc), "mapbiomas_class")
  # cropped extent must sit inside the source extent
  e <- terra::ext(rc)
  expect_gte(e$xmin, -43.5 - 0.05)
  expect_lte(e$xmax, -43.0 + 0.05)
})

test_that("mb_raster_local caches the windowed crop and re-reads it", {
  src <- lulc_tif()
  cache_dir <- file.path(tempdir(), paste0("mbcache_", as.integer(Sys.time())))
  r1 <- mb_raster_local(aoi_poly(), src = src, cache = TRUE, cache_dir = cache_dir)
  expect_true(length(list.files(cache_dir, pattern = "\\.tif$")) >= 1)
  # second call must hit the cache and return an equivalent raster
  r2 <- mb_raster_local(aoi_poly(), src = src, cache = TRUE, cache_dir = cache_dir)
  expect_equal(dim(r1), dim(r2))
})

test_that("mb_raster_local errors on an unreadable source", {
  expect_error(
    mb_raster_local(aoi_poly(), src = "/no/such/raster.tif", cache = FALSE),
    "Could not open"
  )
})

# --- fire_areas + fire_raster_local -----------------------------------------

test_that("fire_areas tabulates burned area per frequency", {
  f <- make_fire_tif()
  r <- terra::rast(f); names(r) <- "fire"
  fa <- fire_areas(r)
  expect_named(fa, c("freq", "area_km2"))
  expect_type(fa$freq, "integer")
  expect_true(all(fa$area_km2 > 0))
})

test_that("fire_raster_local reads a local fire GeoTIFF", {
  f <- make_fire_tif()
  r <- fire_raster_local(aoi_poly(), src = f, cache = FALSE)
  expect_s4_class(r, "SpatRaster")
  expect_identical(names(r), "fire")
})

# --- cover_timeseries end-to-end on the local backend -----------------------

test_that("cover_timeseries runs over a local src for two years", {
  src <- lulc_tif()
  ts <- cover_timeseries(aoi_poly(), years = c(2023, 2024),
                         backend = "local", src = src, verbose = FALSE)
  expect_s3_class(ts, "data.frame")
  expect_true(all(c("year", "label", "hex", "pct") %in% names(ts)))
  expect_setequal(unique(ts$year), c(2023, 2024))
  # percentages within a year sum to ~100
  agg <- tapply(ts$pct, ts$year, sum)
  expect_true(all(abs(agg - 100) < 1e-6))
})

test_that("cover_timeseries by = 'group' aggregates into conservation groups", {
  src <- lulc_tif()
  ts <- cover_timeseries(aoi_poly(), years = 2024, backend = "local",
                         src = src, by = "group", verbose = FALSE)
  expect_true("group" %in% names(ts))
  expect_true(all(ts$group %in%
                  c("natural", "anthropic", "water", "other", "not_observed")))
})
