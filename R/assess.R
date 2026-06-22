#' Assess one or many species: EOO, AOO, MapBiomas conversion and fire
#'
#' End-to-end pipeline. For each species it computes the EOO (minimum convex
#' polygon) and AOO (2 km grid), then quantifies the percentage of converted
#' (anthropic) and remaining natural habitat from MapBiomas inside both the EOO
#' and the AOO. With \code{multicountry = TRUE} (and the local backend) it detects
#' which MapBiomas country each species spans and reads the matching national
#' raster(s); transboundary species are mosaicked seamlessly. When
#' \code{fire = TRUE} it also adds the percentage of the EOO/AOO that has burned
#' and the mean fire recurrence, from the MapBiomas Fire accumulated layer.
#'
#' @param occ An \code{sf} of POINT geometries from \code{\link{read_occurrences}}.
#' @param year Integer MapBiomas year (default \code{2024}).
#' @param collection Integer MapBiomas collection number (default \code{10}).
#'   Used by the GEE backend and as the legend vocabulary; per-country
#'   collections are taken from the registry automatically.
#' @param backend Habitat backend: \code{"local"} (default) or \code{"gee"}.
#' @param cell_km AOO grid cell size in km (default \code{2}).
#' @param mapbiomas Logical; if \code{FALSE}, only EOO/AOO are computed.
#' @param fire Logical; if \code{TRUE}, also compute burned-area metrics from
#'   MapBiomas Fire (accumulated layer, read locally). Default \code{FALSE}.
#' @param fire_collection,fire_host_collection Fire collection number (default
#'   \code{4}) and the initiative folder hosting it (default \code{9}).
#' @param src Optional MapBiomas LULC GeoTIFF for the local backend. For
#'   multi-country runs this may be a named list of per-country overrides
#'   (e.g. \code{list(brazil = "...", peru = "...")}); a single path is treated
#'   as a Brazil/global override.
#' @param fire_src Optional MapBiomas Fire GeoTIFF (overrides the public URL).
#' @param water_in_denominator Passed to \code{\link{summarise_conversion}}.
#' @param multicountry Logical; if \code{TRUE} (default) and \code{backend =
#'   "local"}, detect the MapBiomas country set per species and read/mosaic the
#'   matching national rasters. If \code{FALSE}, Brazil is assumed.
#' @param countries Optional character vector of country keys to force for ALL
#'   species (skips per-species detection). \code{NULL} (default) auto-detects.
#' @param min_records Minimum records required to attempt an assessment.
#' @param verbose Logical; print progress (default \code{TRUE}).
#' @return An object of class \code{geoconv_assessment}: a list with
#'   \code{summary} (one row per species) and \code{detail} (per-species
#'   \code{points}, \code{eoo}, \code{aoo}, \code{eoo_conversion},
#'   \code{aoo_conversion}, \code{countries}, and — when \code{fire = TRUE} —
#'   \code{eoo_fire}, \code{aoo_fire}).
#' @examples
#' \dontrun{
#' f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
#' occ <- read_occurrences(f)
#' res <- assess_species(occ, year = 2024, fire = TRUE)
#' res$summary
#' }
#' @export
assess_species <- function(occ, year = 2024, collection = 10,
                           backend = c("local", "gee"),
                           cell_km = 2, mapbiomas = TRUE,
                           fire = FALSE, fire_collection = 4,
                           fire_host_collection = 9,
                           src = NULL, fire_src = NULL,
                           water_in_denominator = FALSE,
                           multicountry = TRUE, countries = NULL,
                           min_records = 1, verbose = TRUE) {
  .assert_points(occ, "occ")
  backend <- match.arg(backend)
  if (is.null(occ$species)) occ$species <- "sp1"
  sp_list <- unique(occ$species)

  rows <- vector("list", length(sp_list))
  detail <- vector("list", length(sp_list))
  names(detail) <- sp_list

  for (i in seq_along(sp_list)) {
    sp <- sp_list[i]
    pts <- occ[occ$species == sp, , drop = FALSE]
    if (verbose) message(sprintf("[%d/%d] %s (%d records)",
                                 i, length(sp_list), sp, nrow(pts)))

    if (nrow(pts) < min_records) {
      if (verbose) message("  skipped: fewer than min_records.")
      next
    }

    # --- which MapBiomas country(ies) does this species span? ---
    sp_countries <- .resolve_countries(pts, multicountry, backend, countries)
    if (verbose && length(sp_countries) > 1L)
      message("  transboundary: ", paste(sp_countries, collapse = " + "))

    eoo <- calc_eoo(pts)
    aoo <- calc_aoo(pts, cell_km = cell_km)
    cats <- iucn_category_B(eoo$area_km2, aoo$area_km2)

    eoo_conv <- aoo_conv <- NULL
    if (mapbiomas) {
      eoo_conv <- .conversion_for(eoo$hull, year, collection, backend, src,
                                  water_in_denominator, verbose, "EOO",
                                  countries = sp_countries)
      aoo_conv <- .conversion_for(.st_union_quiet(aoo$cells), year, collection,
                            backend, src, water_in_denominator, verbose, "AOO",
                            countries = sp_countries)
    }

    eoo_fire <- aoo_fire <- NULL
    if (fire) {
      eoo_fire <- .fire_for(eoo$hull, eoo$area_km2, fire_collection,
                            fire_host_collection, fire_src, verbose, "EOO")
      aoo_fire <- .fire_for(.st_union_quiet(aoo$cells), aoo$area_km2,
                      fire_collection, fire_host_collection, fire_src,
                      verbose, "AOO")
    }

    row <- data.frame(
      species = sp,
      n_records = eoo$n_records,
      n_unique = eoo$n_unique,
      eoo_km2 = round(eoo$area_km2, 2),
      aoo_km2 = round(aoo$area_km2, 2),
      aoo_cells = aoo$n_cells,
      eoo_converted_pct = .pp(eoo_conv$converted_pct),
      eoo_natural_pct   = .pp(eoo_conv$natural_pct),
      aoo_converted_pct = .pp(aoo_conv$converted_pct),
      aoo_natural_pct   = .pp(aoo_conv$natural_pct),
      eoo_cat_B1 = cats$eoo_category,
      aoo_cat_B2 = cats$aoo_category,
      provisional_cat = cats$combined,
      countries = paste(sp_countries, collapse = "+"),
      mapbiomas_year = year,
      mapbiomas_collection = collection,
      stringsAsFactors = FALSE
    )
    if (fire) {
      row$eoo_burned_pct  <- .pp(eoo_fire$burned_pct)
      row$aoo_burned_pct  <- .pp(aoo_fire$burned_pct)
      row$fire_collection <- fire_collection
    }
    rows[[i]] <- row
    detail[[sp]] <- list(points = pts, eoo = eoo, aoo = aoo,
                         eoo_conversion = eoo_conv, aoo_conversion = aoo_conv,
                         countries = sp_countries,
                         eoo_fire = eoo_fire, aoo_fire = aoo_fire)
  }

  summary_df <- do.call(rbind, rows)
  if (is.null(summary_df)) summary_df <- data.frame()
  structure(list(summary = summary_df, detail = detail,
                 settings = list(year = year, collection = collection,
                                 backend = backend, cell_km = cell_km,
                                 mapbiomas = mapbiomas, fire = fire,
                                 fire_collection = fire_collection,
                                 fire_host_collection = fire_host_collection,
                                 multicountry = multicountry,
                                 water_in_denominator = water_in_denominator)),
            class = "geoconv_assessment")
}

#' @keywords internal
#' @noRd
## Decide the country set for one species. Auto-detection only runs on the local
## backend (the GEE asset is Brazil-only here); otherwise fall back to Brazil.
.resolve_countries <- function(pts, multicountry, backend, countries) {
  if (!is.null(countries) && length(countries))
    return(unique(vapply(countries, .mb_country_aliases, character(1))))
  if (!isTRUE(multicountry) || backend != "local") return("brazil")
  cs <- tryCatch(species_countries(pts), error = function(e) character(0))
  if (!length(cs)) "brazil" else cs
}

#' @keywords internal
#' @noRd
.pp <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else round(x, 1)

#' @keywords internal
#' @noRd
## Country-aware conversion. GEE path unchanged (Brazil asset); local path uses
## the windowed multi-country mosaic so transboundary ranges are seamless.
.conversion_for <- function(geom, year, collection, backend, src,
                            water_in_denominator, verbose, label,
                            countries = "brazil") {
  if (is.null(geom) || length(geom) == 0 || all(sf::st_is_empty(geom))) {
    if (verbose) message(sprintf("  %s: no polygon (need >= 3 points); ",
                                 label), "conversion = NA.")
    return(NULL)
  }
  out <- tryCatch({
    if (backend == "gee") {
      ca <- mb_class_areas_gee(geom, year = year, collection = collection)
    } else {
      r <- mb_raster_multicountry(geom, countries = countries, year = year,
                                  src = if (is.list(src)) src else
                                        if (is.null(src)) NULL else
                                        stats::setNames(list(src),
                                          countries[1]),
                                  verbose = verbose)
      ca <- mb_class_areas_raster(r)
    }
    summarise_conversion(ca, collection = collection,
                         water_in_denominator = water_in_denominator)
  }, error = function(e) {
    warning(sprintf("%s conversion failed: %s", label, conditionMessage(e)),
            call. = FALSE)
    NULL
  })
  out
}

#' @export
print.geoconv_assessment <- function(x, ...) {
  cat("<mappingAS assessment>\n")
  s <- x$settings
  cat(sprintf("  MapBiomas Collection %s, year %s | backend: %s | AOO cell: %g km\n",
              s$collection, s$year, s$backend, s$cell_km))
  if (isTRUE(s$multicountry)) cat("  multi-country: on\n")
  cat(sprintf("  %d species assessed\n", nrow(x$summary)))
  if (nrow(x$summary)) print(utils::head(x$summary, 10), row.names = FALSE)
  invisible(x)
}