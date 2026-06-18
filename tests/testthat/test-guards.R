# These exercise the *guard* and *validation* branches of the remote/GEE
# backends, which run without any network access or raster I/O.

test_that("mb_class_areas_gee errors clearly when rgee is absent", {
  skip_if(requireNamespace("rgee", quietly = TRUE),
          "rgee is installed; the missing-package branch can't be tested")
  aoi <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(-43, -22), c(-42, -22), c(-42, -21), c(-43, -22)))), crs = 4326)
  expect_error(mb_class_areas_gee(aoi), "rgee")
})

test_that("cover_timeseries rejects an empty geometry", {
  empty <- sf::st_sfc(crs = 4326)
  expect_error(cover_timeseries(empty), "empty")
})

test_that("cover_timeseries validates backend and 'by' arguments", {
  poly <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(-43, -22), c(-42, -22), c(-42, -21), c(-43, -22)))), crs = 4326)
  expect_error(cover_timeseries(poly, backend = "cloud"))
  expect_error(cover_timeseries(poly, by = "pixel"))
})

test_that("export_ranges rejects a non-assessment object", {
  expect_error(export_ranges(list()), "geoconv_assessment")
})

test_that("export_ranges validates what/format/aoo_as arguments", {
  a <- structure(list(summary = data.frame(), detail = list(),
                      settings = list()), class = "geoconv_assessment")
  expect_error(export_ranges(a, what = "everything"))
  expect_error(export_ranges(a, format = "kml"))
  expect_error(export_ranges(a, aoo_as = "blob"))
})

test_that("read_occurrences errors on a missing file", {
  expect_error(read_occurrences("/no/such/file.csv"), "not found")
})

test_that("read_occurrences errors when coordinate columns can't be found", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("a,b,c", "1,2,3"), tmp)
  expect_error(suppressMessages(read_occurrences(tmp)),
               "auto-detect")
})

test_that("read_occurrences assigns a default species when none is present", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("lon,lat", "-43.2,-22.9", "-43.1,-22.8"), tmp)
  occ <- suppressMessages(read_occurrences(tmp, default_species = "unknown_sp"))
  expect_setequal(unique(occ$species), "unknown_sp")
})

test_that("read_occurrences errors when every row is dropped as invalid", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("species,lon,lat", "sp,999,999", "sp,0,0"), tmp)
  expect_error(suppressMessages(read_occurrences(tmp)), "No valid")
})

test_that(".shp_siblings enumerates the shapefile sidecar set", {
  sibs <- mappingAS:::.shp_siblings("/tmp/ranges.shp")
  expect_true(all(grepl("ranges\\.", sibs)))
  expect_setequal(tools::file_ext(sibs), c("shp", "shx", "dbf", "prj", "cpg"))
})

test_that(".same_crs compares CRS values robustly", {
  same_crs <- mappingAS:::.same_crs
  expect_true(same_crs(4326, 4326))
  expect_false(same_crs(4326, 3857))
})
