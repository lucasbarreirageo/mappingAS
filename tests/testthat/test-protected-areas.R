## Offline tests for the Conservation-Unit (UC) overlap module.
## A synthetic UC polygon is used throughout, so no network access is required.

make_pts <- function(coords) {
  sf::st_as_sf(
    data.frame(species = "sp", lon = coords[, 1], lat = coords[, 2]),
    coords = c("lon", "lat"), crs = 4326
  )
}

# 5 points forming a ~0.5 deg square near 22 S (two points sit at lon -43.5)
sq <- matrix(c(-43.5, -22.5,
               -43.0, -22.5,
               -43.0, -22.0,
               -43.5, -22.0,
               -43.25, -22.25), ncol = 2, byrow = TRUE)

make_uc <- function(xmin, ymin, xmax, ymax, name = "UC Teste",
                    cat = "Parque Nacional", grp = "Protecao Integral") {
  poly <- sf::st_polygon(list(rbind(
    c(xmin, ymin), c(xmax, ymin), c(xmax, ymax),
    c(xmin, ymax), c(xmin, ymin))))
  sf::st_sf(nome_uc = name, categoria = cat, grupo = grp,
            geometry = sf::st_sfc(poly, crs = 4326))
}

test_that("summarise_protected: a UC covering the whole range gives 100%", {
  skip_if_not_installed("units"); skip_if_not_installed("lwgeom")
  pts <- make_pts(sq)
  e <- suppressMessages(calc_eoo(pts))
  a <- suppressMessages(calc_aoo(pts, cell_km = 2))
  uc <- make_uc(-44, -23, -42.5, -21.5)            # envelops everything

  s <- summarise_protected(pts, uc, eoo = e, aoo = a)
  expect_equal(s$n_occ_in, nrow(pts))
  expect_equal(s$occ_pct, 100)
  expect_gt(s$eoo_pct, 99)
  expect_equal(s$aoo_pct, 100)
  expect_gte(s$n_uc, 1)
  expect_true(all(c("pa_name", "pa_category", "pa_group", "n_occ",
                    "overlap_eoo_km2") %in% names(s$list)))
})

test_that("summarise_protected: a partial UC gives partial overlap", {
  skip_if_not_installed("units"); skip_if_not_installed("lwgeom")
  pts <- make_pts(sq)
  e <- suppressMessages(calc_eoo(pts))
  a <- suppressMessages(calc_aoo(pts, cell_km = 2))
  uc <- make_uc(-44, -23, -43.3, -21.5)            # only the western half

  s <- summarise_protected(pts, uc, eoo = e, aoo = a)
  expect_equal(s$n_occ_in, 2L)                      # the two lon -43.5 points
  expect_equal(round(s$occ_pct), 40)
  expect_gt(s$eoo_pct, 0)
  expect_lt(s$eoo_pct, 100)
  expect_gte(s$n_uc, 1)
})

test_that("summarise_protected: a disjoint UC gives zero overlap", {
  skip_if_not_installed("units"); skip_if_not_installed("lwgeom")
  pts <- make_pts(sq)
  e <- suppressMessages(calc_eoo(pts))
  a <- suppressMessages(calc_aoo(pts, cell_km = 2))
  uc <- make_uc(0, 0, 1, 1)                         # far away

  s <- summarise_protected(pts, uc, eoo = e, aoo = a)
  expect_equal(s$n_occ_in, 0L)
  expect_equal(s$occ_pct, 0)
  expect_equal(s$eoo_pct, 0)
  expect_equal(s$aoo_pct, 0)
  expect_equal(nrow(s$list), 0L)
})

test_that("protected_areas reads a local UC file and standardises columns", {
  skip_if_not_installed("sf")
  uc <- make_uc(-44, -23, -42.5, -21.5)
  f <- file.path(tempdir(), paste0("uc_", as.integer(Sys.time()), ".gpkg"))
  sf::st_write(uc, f, quiet = TRUE, append = FALSE)

  aoi <- suppressMessages(calc_eoo(make_pts(sq)))$hull
  got <- protected_areas(aoi, src = f)
  expect_s3_class(got, "sf")
  expect_gte(nrow(got), 1)
  expect_true(all(c("pa_name", "pa_category", "pa_group") %in% names(got)))
  expect_identical(got$pa_name[1], "UC Teste")
})

test_that("assess_species(protected=TRUE, pa_src=) adds UC columns; pa_table works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units"); skip_if_not_installed("lwgeom")
  uc <- make_uc(-44, -23, -42.5, -21.5)
  f <- file.path(tempdir(), paste0("uc2_", as.integer(Sys.time()), ".gpkg"))
  sf::st_write(uc, f, quiet = TRUE, append = FALSE)

  occ <- make_pts(sq)
  res <- suppressMessages(assess_species(
    occ, mapbiomas = FALSE, protected = TRUE, pa_src = f, verbose = FALSE))

  expect_true(all(c("occ_in_uc_pct", "eoo_uc_pct", "aoo_uc_pct", "n_uc")
                  %in% names(res$summary)))
  expect_equal(res$summary$occ_in_uc_pct[1], 100)
  expect_gte(res$summary$n_uc[1], 1)

  tb <- pa_table(res)
  expect_gt(nrow(tb), 0)
  expect_true(all(c("species", "pa_name", "n_occ", "overlap_eoo_km2")
                  %in% names(tb)))
})

test_that("plot_protection returns the EOO/AOO percentage matrix", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units"); skip_if_not_installed("lwgeom")
  uc <- make_uc(-44, -23, -42.5, -21.5)
  f <- file.path(tempdir(), paste0("uc3_", as.integer(Sys.time()), ".gpkg"))
  sf::st_write(uc, f, quiet = TRUE, append = FALSE)

  res <- suppressMessages(assess_species(
    make_pts(sq), mapbiomas = FALSE, protected = TRUE, pa_src = f, verbose = FALSE))

  grDevices::pdf(NULL)                     # draw to a null device
  on.exit(grDevices::dev.off(), add = TRUE)
  p <- plot_protection(res)
  M <- if (inherits(p, "ggplot")) attr(p, "pct") else p
  expect_true(is.matrix(M))
  expect_equal(rownames(M), c("AOO", "EOO"))
  expect_equal(unname(rowSums(M)), c(100, 100))

  expect_error(plot_protection(
    suppressMessages(assess_species(make_pts(sq), mapbiomas = FALSE, verbose = FALSE))),
    "protected")
})