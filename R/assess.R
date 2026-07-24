#' Assess one or many species: EOO, AOO, MapBiomas conversion and fire
#'
#' End-to-end pipeline. For each species it computes the EOO (minimum convex
#' polygon) and AOO (2 km grid), then quantifies the percentage of converted
#' (anthropic) and remaining natural habitat from the most recent MapBiomas
#' collection inside both the EOO and the AOO. When \code{fire = TRUE} it also
#' adds the percentage of the EOO/AOO that has burned and the mean fire
#' recurrence, from the MapBiomas Fire accumulated layer.
#'
#' @param occ An \code{sf} of POINT geometries from \code{\link{read_occurrences}}.
#' @param initiative MapBiomas initiative: \code{"brazil"} (default),
#'   \code{"amazonia"} (Pan-Amazon / RAISG) or \code{"colombia"}. Selects the
#'   land-cover product, its default collection and the standardised legend, so a
#'   species in the Amazon basin or Colombia can be assessed without Google Earth
#'   Engine (see \code{\link{mb_initiatives}}).
#' @param year Integer MapBiomas year. \code{NULL} (default) uses the most recent
#'   year available for the initiative (2024 for Brazil/Colombia, 2023 for
#'   Amazonia).
#' @param collection Integer MapBiomas collection number. \code{NULL} (default)
#'   uses the initiative default (10/6/3 for Brazil/Amazonia/Colombia).
#' @param backend Habitat backend: \code{"local"} (default) or \code{"gee"}.
#'   The \code{"gee"} backend is only wired for \code{initiative = "brazil"}.
#' @param cell_km AOO grid cell size in km (default \code{2}).
#' @param mapbiomas Logical; if \code{FALSE}, only EOO/AOO are computed.
#' @param fire Logical; if \code{TRUE}, also compute burned-area metrics from
#'   MapBiomas Fire (accumulated layer, read locally). Default \code{FALSE}.
#' @param fire_collection,fire_host_collection Fire collection number (default
#'   \code{4}) and the initiative folder hosting it (default \code{9}).
#' @param protected Logical; if \code{TRUE}, also compute the overlap of the
#'   range with federal Conservation Units (UCs) from the ICMBio WFS: the share
#'   of occurrences inside UCs and the \% of the EOO/AOO within UCs. Default
#'   \code{FALSE}. Requires internet unless \code{pa_src} is given.
#' @param pa_src Optional local UC vector file (\code{.shp}/\code{.gpkg}/
#'   \code{.geojson}) used instead of the WFS (offline).
#' @param pa_source Protected-area source: \code{"icmbio"} (Brazilian federal
#'   Conservation Units, INDE WFS) or \code{"wdpa"} (World Database on Protected
#'   Areas, global). \code{NULL} (default) picks \code{"icmbio"} for
#'   \code{initiative = "brazil"} and \code{"wdpa"} otherwise (see
#'   \code{\link{wdpa_areas}}).
#' @param pa_typename Optional ICMBio WFS layer name; \code{NULL} (default)
#'   auto-detects it (see \code{\link{protected_layers}}).
#' @param src Optional MapBiomas LULC GeoTIFF for the local backend.
#' @param fire_src Optional MapBiomas Fire GeoTIFF (overrides the public URL).
#' @param water_in_denominator Passed to \code{\link{summarise_conversion}}.
#' @param min_records Minimum records required to attempt an assessment.
#' @param verbose Logical; print progress (default \code{TRUE}).
#' @return An object of class \code{geoconv_assessment}: a list with
#'   \code{summary} (one row per species) and \code{detail} (per-species
#'   \code{points}, \code{eoo}, \code{aoo}, \code{eoo_conversion},
#'   \code{aoo_conversion}, and — when \code{fire = TRUE} — \code{eoo_fire},
#'   \code{aoo_fire}).
#' @examples
#' \dontrun{
#' f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
#' occ <- read_occurrences(f)
#' res <- assess_species(occ, year = 2024, fire = TRUE)
#' res$summary
#' }
#' @export
assess_species <- function(occ, initiative = "brazil",
                           year = NULL, collection = NULL,
                           backend = c("local", "gee"),
                           cell_km = 2, mapbiomas = TRUE,
                           fire = FALSE, fire_collection = 4,
                           fire_host_collection = 9,
                           protected = FALSE, pa_src = NULL, pa_source = NULL,
                           pa_typename = NULL,
                           src = NULL, fire_src = NULL,
                           water_in_denominator = FALSE,
                           min_records = 1, verbose = TRUE) {
  .assert_points(occ, "occ")
  backend <- match.arg(backend)
  ini <- .mb_resolve_initiative(initiative)
  initiative <- ini$key
  if (is.null(collection)) collection <- ini$collection
  if (is.null(year)) year <- max(ini$years)
  year <- as.integer(year)
  collection <- as.integer(collection)
  if (is.null(pa_source))
    pa_source <- if (initiative == "brazil") "icmbio" else "wdpa"
  pa_source <- match.arg(tolower(pa_source), c("icmbio", "wdpa"))
  if (backend == "gee" && initiative != "brazil") {
    warning("The GEE backend is only wired for initiative = 'brazil'; ",
            "using the local (/vsicurl/) backend for ", initiative, ".",
            call. = FALSE)
    backend <- "local"
  }
  if (isTRUE(fire) && initiative != "brazil") {
    warning("MapBiomas Fire is only published for Brazil; skipping fire ",
            "metrics for initiative = '", initiative, "'.", call. = FALSE)
    fire <- FALSE
  }
  if (!year %in% ini$years) {
    warning(sprintf("Year %d is outside the %s range (%d-%d); reading it anyway.",
                    year, ini$label, min(ini$years), max(ini$years)),
            call. = FALSE)
  }
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
    
    eoo <- calc_eoo(pts)
    aoo <- calc_aoo(pts, cell_km = cell_km)
    cats <- iucn_category_B(eoo$area_km2, aoo$area_km2)
    
    eoo_conv <- aoo_conv <- NULL
    if (mapbiomas) {
      eoo_conv <- .conversion_for(eoo$hull, year, collection, initiative, backend,
                                  src, water_in_denominator, verbose, "EOO")
      aoo_conv <- .conversion_for(.st_union_quiet(aoo$cells), year, collection,
                            initiative, backend, src, water_in_denominator,
                            verbose, "AOO")
    }
    
    eoo_fire <- aoo_fire <- NULL
    if (fire) {
      eoo_fire <- .fire_for(eoo$hull, eoo$area_km2, fire_collection,
                            fire_host_collection, fire_src, verbose, "EOO")
      aoo_fire <- .fire_for(.st_union_quiet(aoo$cells), aoo$area_km2,
                      fire_collection, fire_host_collection, fire_src,
                      verbose, "AOO")
    }

    pa <- NULL
    if (protected) {
      pa <- .protected_for(pts, eoo, aoo, src = pa_src, source = pa_source,
                           typename = pa_typename, verbose = verbose)
    }
    if (protected && mapbiomas && !is.null(pa) && !is.null(pa$layer)) {
      uc_u <- .st_union_quiet(sf::st_geometry(pa$layer))
      e_nat <- .nat_in_uc(eoo$hull, uc_u, eoo_conv, year, collection, initiative,
                          backend, src, water_in_denominator, verbose, "EOO")
      a_nat <- .nat_in_uc(.st_union_quiet(aoo$cells), uc_u, aoo_conv, year,
                          collection, initiative, backend, src,
                          water_in_denominator, verbose, "AOO")
      pa$eoo_nat_uc_pct <- e_nat$nat_pct_total
      pa$eoo_alt_uc_pct <- e_nat$alt_pct_total
      pa$eoo_nat_uc_pct_in <- e_nat$nat_pct_uc
      pa$aoo_nat_uc_pct <- a_nat$nat_pct_total
      pa$aoo_alt_uc_pct <- a_nat$alt_pct_total
      pa$aoo_nat_uc_pct_in <- a_nat$nat_pct_uc
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
      mapbiomas_initiative = initiative,
      mapbiomas_year = year,
      mapbiomas_collection = collection,
      stringsAsFactors = FALSE
    )
    if (fire) {
      row$eoo_burned_pct  <- .pp(eoo_fire$burned_pct)
      row$aoo_burned_pct  <- .pp(aoo_fire$burned_pct)
      row$fire_collection <- fire_collection
    }
    if (protected) {
      row$occ_in_uc_pct <- .pp(pa$occ_pct)
      row$eoo_uc_pct    <- .pp(pa$eoo_pct)
      row$aoo_uc_pct    <- .pp(pa$aoo_pct)
      row$n_uc          <- if (is.null(pa)) NA_integer_ else pa$n_uc
      if (mapbiomas) {
        row$eoo_nat_uc_pct    <- .pp(pa$eoo_nat_uc_pct)
        row$eoo_nat_uc_pct_in <- .pp(pa$eoo_nat_uc_pct_in)
        row$aoo_nat_uc_pct    <- .pp(pa$aoo_nat_uc_pct)
        row$aoo_nat_uc_pct_in <- .pp(pa$aoo_nat_uc_pct_in)
      }
    }
    rows[[i]] <- row
    detail[[sp]] <- list(points = pts, eoo = eoo, aoo = aoo,
                         eoo_conversion = eoo_conv, aoo_conversion = aoo_conv,
                         eoo_fire = eoo_fire, aoo_fire = aoo_fire, pa = pa)
  }
  
  summary_df <- do.call(rbind, rows)
  if (is.null(summary_df)) summary_df <- data.frame()
  structure(list(summary = summary_df, detail = detail,
                 settings = list(initiative = initiative, year = year,
                                 collection = collection,
                                 backend = backend, cell_km = cell_km,
                                 mapbiomas = mapbiomas, fire = fire,
                                 fire_collection = fire_collection,
                                 fire_host_collection = fire_host_collection,
                                 protected = protected, pa_source = pa_source,
                                 water_in_denominator = water_in_denominator)),
            class = "geoconv_assessment")
}

#' @keywords internal
#' @noRd
.pp <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else round(x, 1)

#' @keywords internal
#' @noRd
.conversion_for <- function(geom, year, collection, initiative, backend, src,
                            water_in_denominator, verbose, label) {
  if (is.null(geom) || length(geom) == 0 || all(sf::st_is_empty(geom))) {
    if (verbose) message(sprintf("  %s: no polygon (need >= 3 points); ",
                                 label), "conversion = NA.")
    return(NULL)
  }
  out <- tryCatch({
    if (backend == "gee") {
      ca <- mb_class_areas_gee(geom, year = year, collection = collection)
    } else {
      r <- mb_raster_local(geom, year = year, collection = collection,
                           initiative = initiative, src = src)
      ca <- mb_class_areas_raster(r)
    }
    summarise_conversion(ca, collection = collection, initiative = initiative,
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
  ini <- tryCatch(.mb_resolve_initiative(s$initiative %||% "brazil")$label,
                  error = function(e) s$initiative %||% "brazil")
  cat(sprintf("  %s Collection %s, year %s | backend: %s | AOO cell: %g km\n",
              ini, s$collection, s$year, s$backend, s$cell_km))
  cat(sprintf("  %d species assessed\n", nrow(x$summary)))
  if (nrow(x$summary)) print(utils::head(x$summary, 10), row.names = FALSE)
  invisible(x)
}