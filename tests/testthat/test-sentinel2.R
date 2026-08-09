# Global Sentinel-2 / Esri (Impact Observatory) fallback layer used when a
# range falls outside every MapBiomas product. Pure-logic / pure-table checks
# (no network).

test_that("esri_legend has the expected structure and groupings", {
  leg <- esri_legend()
  expect_true(all(c("code", "class_en", "class_pt", "hex", "level1", "group")
                  %in% names(leg)))
  expect_false(any(duplicated(leg$code)))
  expect_setequal(unique(leg$group),
                  c("natural", "anthropic", "water", "other", "not_observed"))
  g <- function(code) leg$group[leg$code == code]
  expect_identical(g(2),  "natural")       # Trees
  expect_identical(g(11), "natural")       # Rangeland
  expect_identical(g(4),  "natural")       # Flooded vegetation
  expect_identical(g(5),  "anthropic")     # Crops
  expect_identical(g(7),  "anthropic")     # Built area
  expect_identical(g(1),  "water")         # Water
  expect_identical(g(8),  "other")         # Bare ground
  expect_identical(g(9),  "other")         # Snow/Ice
  expect_identical(g(10), "not_observed")  # Clouds
  expect_true(all(grepl("^#", leg$hex)))
})

test_that("sentinel2 aliases resolve to the Esri provider", {
  res <- function(x) mappingAS:::.mb_resolve_initiative(x)
  for (a in c("sentinel2", "sentinel-2", "s2", "esri", "io-lulc", "global",
              "world")) {
    ini <- res(a)
    expect_identical(ini$provider, "esri")
    expect_identical(ini$key, "sentinel2")
  }
  expect_equal(range(res("sentinel2")$years), c(2017L, 2023L))
  # MapBiomas products keep provider = "mapbiomas"
  expect_identical(res("brazil")$provider, "mapbiomas")
})

test_that("mb_legend / mb_groups / mb_palette route to the Esri legend", {
  leg <- mb_legend(initiative = "sentinel2")
  expect_identical(leg, esri_legend())
  grp <- mb_groups(initiative = "sentinel2")
  expect_true(2 %in% grp$natural)
  expect_true(5 %in% grp$anthropic)
  pal <- mb_palette(c(2, 5), initiative = "sentinel2")
  expect_identical(unname(pal[as.character(2)]), esri_legend()$hex[
    esri_legend()$code == 2])
})

test_that("mb_legend does not warn for the Esri layer regardless of collection", {
  expect_silent(mb_legend(initiative = "sentinel2"))
  expect_silent(mb_legend(99, "esri"))
})

test_that("s2_source_url builds the tiled public COG URL", {
  u <- s2_source_url("47P", 2023)
  expect_type(u, "character")
  expect_length(u, 1L)
  expect_match(u, "io-10m-annual-lulc.*/47P_2023\\.tif$")
  expect_match(s2_source_url("10T", 2017), "/10T_2017\\.tif$")
  # base_url is overridable
  expect_match(s2_source_url("47P", 2023, base_url = "https://mirror/x"),
               "^https://mirror/x/47P_2023\\.tif$")
})

test_that("mb_source_url refuses the Esri layer and points to s2_source_url", {
  expect_error(mb_source_url(2024, initiative = "sentinel2"), "s2_source_url")
})

test_that("MGRS helpers map lon/lat to the right grid zone", {
  zone <- mappingAS:::.s2_utm_zone
  band <- mappingAS:::.s2_lat_band
  expect_equal(zone(-51),   as.integer((floor((-51 + 180) / 6) + 1)))  # Brazil
  expect_equal(zone(0),     31L)
  expect_equal(zone(-180),  1L)
  expect_equal(band(0),     "N")   # equator -> band N
  expect_identical(band(-10), "L")
  expect_identical(band(80),  "X") # high north -> extended X band
  expect_identical(band(-80), "C")
})

test_that(".s2_cell_bounds returns a 6x8 degree cell", {
  cb <- mappingAS:::.s2_cell_bounds(31L, "N")
  expect_equal(unname(cb["xmin"]), 0)
  expect_equal(unname(cb["xmax"]), 6)
  expect_equal(unname(cb["ymin"]), 0)
  expect_equal(unname(cb["ymax"]), 8)
  # X band is extended to 84 N
  expect_equal(unname(mappingAS:::.s2_cell_bounds(31L, "X")["ymax"]), 84)
})

test_that(".s2_tiles_for_aoi enumerates intersecting tiles", {
  skip_if_not_installed("sf")
  # A small AOI in France (~ lon 2, lat 48) -> UTM zone 31, band U
  bb <- sf::st_bbox(c(xmin = 2, ymin = 48, xmax = 2.4, ymax = 48.3),
                    crs = 4326)
  aoi <- sf::st_as_sfc(bb)
  tiles <- mappingAS:::.s2_tiles_for_aoi(aoi)
  expect_true(length(tiles) >= 1L)
  gzd <- vapply(tiles, function(t) sprintf("%02d%s", t$zone, t$band),
                character(1))
  expect_true("31U" %in% gzd)
})

test_that(".s2_clamp_year keeps years within the available range", {
  cy <- mappingAS:::.s2_clamp_year
  expect_equal(cy(2023), 2023L)
  expect_equal(cy(2024), 2023L)   # beyond series end -> last year
  expect_equal(cy(1990), 2017L)   # before series start -> first year
  expect_equal(cy(3000), 2023L)   # after series end -> last year
  expect_equal(cy(NA),   2023L)
})

test_that("summarise_conversion works with Esri codes via initiative", {
  # Trees(2)=natural 60, Crops(5)=anthropic 30, Built(7)=anthropic 10, Water(1)=5
  ca <- data.frame(code = c(2, 5, 7, 1), area_km2 = c(60, 30, 10, 5))
  cv <- summarise_conversion(ca, initiative = "sentinel2")
  expect_equal(cv$natural_km2, 60)
  expect_equal(cv$anthropic_km2, 40)
  expect_equal(cv$water_km2, 5)
  # terrestrial denominator excludes water: 40 / 100 = 40%
  expect_equal(cv$converted_pct, 40)
  expect_equal(cv$natural_pct, 60)
  expect_true(all(c("class_en", "group") %in% names(cv$by_class)))
})

test_that(".terr_area detects empty (out-of-coverage) conversion results", {
  ta <- mappingAS:::.terr_area
  expect_equal(ta(NULL), 0)
  expect_equal(ta(list(natural_km2 = 0, anthropic_km2 = 0)), 0)
  expect_equal(ta(list(natural_km2 = 12, anthropic_km2 = 3)), 15)
})

test_that(".auto_initiative picks MapBiomas in South America, Sentinel-2 outside", {
  skip_if_not_installed("sf")
  mk <- function(lon, lat) sf::st_as_sf(
    data.frame(lon = lon, lat = lat),
    coords = c("lon", "lat"), crs = 4326)
  ai <- mappingAS:::.auto_initiative
  # Brazil (Rio de Janeiro-ish)
  expect_identical(ai(mk(c(-43.2, -43.3), c(-22.9, -22.8))), "brazil")
  # Peru (Lima-ish) -> peru product, not brazil
  expect_identical(ai(mk(c(-77.0, -76.9), c(-12.0, -12.1))), "peru")
  # Outside South America (Kenya / USA) -> global Sentinel-2
  expect_identical(ai(mk(c(36.8, 36.9), c(-1.3, -1.2))), "sentinel2")
  expect_identical(ai(mk(c(-92.4, -92.5), c(39.7, 39.8))), "sentinel2")
})

test_that("auto resolves as a Brazil-default alias for direct helper calls", {
  # assess_species detects "auto" itself; other helpers should not error on it.
  expect_identical(mappingAS:::.mb_resolve_initiative("auto")$key, "brazil")
  expect_equal(range(mb_years(initiative = "auto")), c(1985L, 2024L))
})

test_that("assess_species accepts the fallback argument and sentinel2/auto keys", {
  # signature exposes `fallback`
  expect_true("fallback" %in% names(formals(assess_species)))
  # unknown initiative still errors and mentions the global fallback
  expect_error(mb_source_url(2024, initiative = "narnia"), "sentinel2")
})
