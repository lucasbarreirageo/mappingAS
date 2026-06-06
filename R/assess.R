#' Assess one or many species: EOO, AOO and MapBiomas habitat conversion
#'
#' End-to-end pipeline. For each species it computes the EOO (minimum convex
#' polygon) and AOO (2 km grid), then quantifies the percentage of converted
#' (anthropic) and remaining natural habitat from the most recent MapBiomas
#' collection inside both the EOO and the AOO. Returns a tidy per-species
#' summary plus the spatial objects needed for mapping.
#'
#' @param occ An \code{sf} of POINT geometries from
#'   \code{\link{read_occurrences}}. If it has a \code{species} column with more
#'   than one value, every species is assessed in turn.
#' @param year Integer MapBiomas year (default \code{2024}, the latest in
#'   Collection 10).
#' @param collection Integer MapBiomas collection number (default \code{10}).
#' @param backend Habitat backend: \code{"local"} (default; \pkg{terra} +
#'   \code{/vsicurl/} or a local GeoTIFF) or \code{"gee"} (Google Earth Engine
#'   via \pkg{rgee}).
#' @param cell_km AOO grid cell size in km (default \code{2}).
#' @param mapbiomas Logical; if \code{FALSE}, only EOO/AOO are computed and the
#'   conversion columns are \code{NA} (fast, fully offline).
#' @param src Optional local path / URL to a MapBiomas GeoTIFF for the local
#'   backend.
#' @param water_in_denominator Passed to \code{\link{summarise_conversion}}.
#' @param min_records Minimum records required to attempt an assessment
#'   (default \code{1}). Species below this are skipped with a note.
#' @param verbose Logical; print progress (default \code{TRUE}).
#' @return An object of class \code{geoconv_assessment}: a list with
#'   \code{summary} (a \code{data.frame}, one row per species) and \code{detail}
#'   (a named list per species with \code{points}, \code{eoo}, \code{aoo},
#'   \code{eoo_conversion}, \code{aoo_conversion}).
#' @examples
#' \dontrun{
#' f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
#' occ <- read_occurrences(f)
#' res <- assess_species(occ, year = 2024)          # local windowed read
#' res$summary
#' map_species(res, species = res$summary$species[1])
#' }
#' @export
assess_species <- function(occ, year = 2024, collection = 10,
                           backend = c("local", "gee"),
                           cell_km = 2, mapbiomas = TRUE, src = NULL,
                           water_in_denominator = FALSE,
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

    eoo <- calc_eoo(pts)
    aoo <- calc_aoo(pts, cell_km = cell_km)
    cats <- iucn_category_B(eoo$area_km2, aoo$area_km2)

    eoo_conv <- NULL
    aoo_conv <- NULL
    if (mapbiomas) {
      eoo_conv <- .conversion_for(eoo$hull, year, collection, backend, src,
                                  water_in_denominator, verbose, "EOO")
      aoo_conv <- .conversion_for(sf::st_union(aoo$cells), year, collection,
                                  backend, src, water_in_denominator, verbose,
                                  "AOO")
    }

    rows[[i]] <- data.frame(
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
      mapbiomas_year = year,
      mapbiomas_collection = collection,
      stringsAsFactors = FALSE
    )
    detail[[sp]] <- list(points = pts, eoo = eoo, aoo = aoo,
                         eoo_conversion = eoo_conv, aoo_conversion = aoo_conv)
  }

  summary_df <- do.call(rbind, rows)
  if (is.null(summary_df)) summary_df <- data.frame()
  structure(list(summary = summary_df, detail = detail,
                 settings = list(year = year, collection = collection,
                                 backend = backend, cell_km = cell_km,
                                 mapbiomas = mapbiomas,
                                 water_in_denominator = water_in_denominator)),
            class = "geoconv_assessment")
}

#' @keywords internal
#' @noRd
.pp <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else round(x, 1)

#' @keywords internal
#' @noRd
.conversion_for <- function(geom, year, collection, backend, src,
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
      r <- mb_raster_local(geom, year = year, collection = collection, src = src)
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
  cat(sprintf("  %d species assessed\n", nrow(x$summary)))
  if (nrow(x$summary)) print(utils::head(x$summary, 10), row.names = FALSE)
  invisible(x)
}
