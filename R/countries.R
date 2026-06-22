## R/countries.R — Multi-country MapBiomas support for mappingAS
## ---------------------------------------------------------------------------
## Single source of truth for every supported MapBiomas initiative:
##   * the public GeoTIFF URL template (year injected at read time),
##   * the native collection number, and
##   * a reclassification table mapping each country's native pixel codes to the
##     Brazil vocabulary used everywhere downstream (R/legend.R).
##
## Design contract: foreign rasters are translated to Brazil codes the moment
## they are read (mb_raster_country() / mb_raster_multicountry()). Therefore
## summarise_conversion(), class_table(), the time series and every plot keep
## working unchanged, because they only ever see Brazil codes/groups.
##
## Harmonisation rule: a foreign code need not map to an *identical* Brazil class,
## only to a Brazil code whose conservation GROUP (natural/anthropic/water/other)
## is correct — that is the only thing the conversion metric depends on. Where a
## faithful thematic equivalent exists (forest->forest, pasture->pasture) it is
## preferred; otherwise the nearest Brazil code within the right group is used.

#' Normalise a country name or ISO-2 alias to a canonical key
#' @keywords internal
#' @noRd
.mb_country_aliases <- function(x) {
  x <- tolower(trimws(as.character(x)))
  switch(x,
    "br" = , "bra" = , "brasil" = , "brazil" = "brazil",
    "ar" = , "arg" = , "argentina" = "argentina",
    "bo" = , "bol" = , "bolivia" = , "bol\u00edvia" = "bolivia",
    "ec" = , "ecu" = , "ecuador" = , "equador" = "ecuador",
    "pe" = , "per" = , "peru" = , "per\u00fa" = "peru",
    x)
}

#' Supported MapBiomas countries
#'
#' @return Character vector of canonical country keys.
#' @examples
#' mb_countries()
#' @export
mb_countries <- function() c("brazil", "argentina", "bolivia", "ecuador", "peru")

#' MapBiomas initiative registry (URL template, collection, reclass table)
#'
#' Returns the metadata for one supported country. The \code{url_template} has a
#' single \code{\%d} where the year is injected. \code{reclass} is a two-column
#' \code{data.frame} (\code{from} = native code, \code{to} = Brazil code) used to
#' harmonise foreign rasters to the Brazil legend; it is \code{NULL} for Brazil
#' (already native).
#'
#' @param country Country name or ISO-2 alias (e.g. \code{"peru"}, \code{"PE"}).
#' @return A list with \code{country}, \code{collection}, \code{url_template},
#'   \code{year_min}, \code{year_max} and \code{reclass}.
#' @examples
#' mb_country_info("peru")$collection
#' mb_country_info("AR")$url_template
#' @export
mb_country_info <- function(country = "brazil") {
  key <- .mb_country_aliases(country)
  reg <- .mb_registry()
  if (is.null(reg[[key]]))
    stop("Unsupported MapBiomas country: ", country,
         ". Supported: ", paste(mb_countries(), collapse = ", "), call. = FALSE)
  reg[[key]]
}

#' @keywords internal
#' @noRd
.mb_registry <- function() {
  list(
    brazil = list(
      country = "brazil", collection = 10L,
      url_template = paste0(
        "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/",
        "collection_10/lulc/coverage/brazil_coverage_%d.tif"),
      year_min = 1985L, year_max = 2024L, reclass = NULL
    ),
    argentina = list(
      country = "argentina", collection = 2L,
      url_template = paste0(
        "https://storage.googleapis.com/mapbiomas-public/initiatives/argentina/",
        "collection-2/coverage/argentina_coverage_%d.tif"),
      year_min = 1985L, year_max = 2024L,
      reclass = .reclass_argentina()
    ),
    bolivia = list(
      country = "bolivia", collection = 3L,
      url_template = paste0(
        "https://storage.googleapis.com/mapbiomas-public/initiatives/bolivia/",
        "collection_2/lclu/coverage/bolivia_coverage_%d.tif"),
      year_min = 1985L, year_max = 2024L,
      reclass = .reclass_bolivia()
    ),
    ecuador = list(
      country = "ecuador", collection = 3L,
      url_template = paste0(
        "https://storage.googleapis.com/mapbiomas-public/initiatives/ecuador/",
        "collection_3/coverage/ecuador_coverage_%d.tif"),
      year_min = 1985L, year_max = 2024L,
      reclass = .reclass_ecuador()
    ),
    peru = list(
      country = "peru", collection = 3L,
      url_template = paste0(
        "https://storage.googleapis.com/mapbiomas-public/initiatives/peru/",
        "collection_3/LULC/peru_collection3_integration_v1-classification_%d.tif"),
      year_min = 1985L, year_max = 2024L,
      reclass = .reclass_peru()
    )
  )
}

## ---- reclassification tables (native code -> Brazil code) ------------------
## Brazil target codes and their groups (from R/legend.R), for reference:
##   3 Forest(nat) · 4 Savanna/OpenForest(nat) · 5 Mangrove(nat) · 6 Flooded forest(nat)
##   11 Wetland(nat) · 12 Grassland(nat) · 29 Rocky outcrop(nat) · 13 Other non-forest(nat)
##   23 Beach/dune(nat) · 15 Pasture(anth) · 18 Agriculture(anth) · 39 Soybean(anth)
##   40 Rice(anth) · 35 Palm oil(anth) · 36 Perennial crop(anth) · 9 Forest plantation(anth)
##   21 Mosaic of uses(anth) · 24 Urban(anth) · 30 Mining(anth) · 31 Aquaculture(anth)
##   25 Other non-vegetated(other) · 33 River/lake/ocean(water) · 34 Glacier(water) · 27 Not observed

#' @keywords internal
#' @noRd
.rc <- function(from, to) data.frame(from = as.integer(from),
                                     to = as.integer(to))

#' Peru (Collection 3) native -> Brazil
#' @keywords internal
#' @noRd
.reclass_peru <- function() .rc(
  from = c(1, 3, 4, 5, 6, 10, 11, 12, 29, 66, 70, 13, 14, 15, 18, 35, 40, 72,
           9, 21, 22, 23, 24, 30, 32, 61, 68, 25, 26, 33, 31, 34, 27),
  to   = c(3, 3, 4, 5, 6, 12, 11, 12, 29, 13, 13, 13, 18, 15, 18, 35, 40, 18,
           9, 21, 25, 23, 24, 30, 25, 25, 25, 25, 33, 33, 31, 34, 27)
)
## Peru notes: 4 Bosque seco -> Savanna/open(4); 66 Matorral & 70 Loma costera ->
## other non-forest natural(13); 32 Salina costera & 61 Salar -> other non-veg(25);
## 72 Otros cultivos -> Agriculture(18); 34 Glaciar kept (water group).

#' Bolivia (Collection 3) native -> Brazil
#' @keywords internal
#' @noRd
.reclass_bolivia <- function() .rc(
  from = c(1, 3, 4, 6, 10, 11, 12, 29, 66, 81, 82, 13, 14, 15, 18, 39, 72,
           21, 22, 23, 24, 30, 61, 68, 25, 26, 33, 31, 34, 27),
  to   = c(3, 3, 4, 6, 12, 11, 12, 29, 13, 13, 11, 13, 18, 15, 18, 39, 18,
           21, 25, 23, 24, 30, 25, 25, 25, 33, 33, 31, 34, 27)
)
## Bolivia notes: 81 Pajonal andino -> other non-forest natural(13);
## 82 Pajonal andino inundable -> Wetland(11); 66 Matorral -> 13;
## 39 Soya -> Soybean(39); 61 Salar & 68 -> other non-vegetated(25).

#' Ecuador (Collection 3) native -> Brazil
#' @keywords internal
#' @noRd
.reclass_ecuador <- function() .rc(
  from = c(1, 3, 4, 5, 6, 10, 11, 12, 81, 82, 29, 13, 14, 9, 21, 74,
           22, 24, 30, 23, 25, 68, 26, 33, 34, 31, 27),
  to   = c(3, 3, 4, 5, 6, 12, 11, 12, 13, 11, 29, 13, 21, 9, 21, 36,
           25, 24, 30, 23, 25, 25, 33, 33, 34, 31, 27)
)
## Ecuador notes: 81 Herbazales andinos -> other non-forest natural(13);
## 82 inundables -> Wetland(11); 74 Banano -> Perennial crop(36);
## 14 Agropecuaria parent -> Mosaic(21); 9 Silvicultura -> Forest plantation(9).

#' Argentina (Collection 2) native -> Brazil
#' @keywords internal
#' @noRd
.reclass_argentina <- function() .rc(
  from = c(1, 3, 4, 6, 2, 66, 77, 63, 12, 11, 73, 18, 19, 36, 15, 9, 21,
           24, 25, 34, 33, 27),
  to   = c(3, 3, 4, 6, 13, 13, 13, 13, 12, 11, 11, 18, 18, 36, 15, 9, 21,
           24, 25, 34, 33, 27)
)
## Argentina (Coll.2) notes: 2 parent shrub, 66 Arbustales cerrados,
## 77 Arbustales abiertos, 63 Mosaico arbustos -> other non-forest natural(13);
## 73 Turberas (peatland) -> Wetland(11); 19 Cultivos temporarios -> Agriculture(18);
## 36 Cultivos perennes -> Perennial crop(36); 34 Hielo/nieve -> Glacier(34, water).

#' Harmonise a foreign MapBiomas SpatRaster to the Brazil code vocabulary
#'
#' Reclassifies pixel codes in-place using the country's table. Codes absent from
#' the table are left untouched (they then fall into the "other" group downstream,
#' contributing zero distortion to the natural/anthropic split). A no-op for
#' Brazil.
#'
#' @param r A \pkg{terra} \code{SpatRaster} of native MapBiomas codes.
#' @param country Country name/alias.
#' @return The \code{SpatRaster} with codes remapped to the Brazil legend.
#' @export
mb_harmonise_raster <- function(r, country) {
  info <- mb_country_info(country)
  if (is.null(info$reclass)) return(r)
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' is required.", call. = FALSE)
  rcl <- as.matrix(info$reclass[, c("from", "to")])
  out <- terra::classify(r, rcl, others = NA, othersFROM = FALSE)
  names(out) <- "mapbiomas_class"
  out
}
