test_that("export_ranges writes EOO and AOO (gpkg and shapefile)", {
  skip_if_not_installed("sf")
  f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
  skip_if(identical(f, ""))

  occ <- read_occurrences(f)
  res <- assess_species(occ, mapbiomas = FALSE, verbose = FALSE)
  n_sp <- nrow(res$summary)
  expect_gt(n_sp, 0)

  td <- file.path(tempdir(), paste0("exp_", as.integer(Sys.time())))
  dir.create(td, showWarnings = FALSE, recursive = TRUE)

  # --- GeoPackage with both layers ---
  gpkg <- export_ranges(res, dir = td, format = "gpkg")
  expect_true(file.exists(gpkg))
  lyrs <- sf::st_layers(gpkg)$name
  expect_true(all(c("eoo", "aoo") %in% lyrs))

  eoo <- sf::st_read(gpkg, layer = "eoo", quiet = TRUE)
  expect_true(all(c("species", "eoo_km2", "conv_pct", "nat_pct", "prov_cat")
                  %in% names(eoo)))
  expect_equal(nrow(eoo), n_sp)              # one dissolved feature per species

  aoo <- sf::st_read(gpkg, layer = "aoo", quiet = TRUE)
  expect_true(all(c("species", "aoo_km2", "n_cells") %in% names(aoo)))
  expect_equal(nrow(aoo), n_sp)

  # --- Shapefiles ---
  shp <- export_ranges(res, dir = td, format = "shapefile")
  expect_true(any(grepl("_EOO\\.shp$", shp)))
  expect_true(any(grepl("_AOO\\.shp$", shp)))

  # --- AOO as individual cells: more features than species ---
  aoo_cells <- export_ranges(res, dir = td, what = "aoo", format = "gpkg",
                             aoo_as = "cells", layer_prefix = "cells")
  ac <- sf::st_read(aoo_cells, layer = "aoo", quiet = TRUE)
  expect_gte(nrow(ac), sum(res$summary$aoo_cells))
})
