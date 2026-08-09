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
#' @param initiative Land-cover product. A MapBiomas initiative -
#'   \code{"brazil"} (default), \code{"amazonia"} (Pan-Amazon / RAISG),
#'   \code{"colombia"} or any of the other South-American products (Argentina,
#'   Bolivia, Chile, Ecuador, Peru, Venezuela, Paraguay, Uruguay) - selects that
#'   product, its default collection and the standardised legend (see
#'   \code{\link{mb_initiatives}}). Two extra keys unlock coverage anywhere on
#'   Earth: \code{"sentinel2"} forces the global Esri / Impact Observatory 10 m
#'   land cover (Sentinel-2, the data behind the ArcGIS Living Atlas Land Cover
#'   Explorer; see \code{\link{esri_legend}}), and \code{"auto"} tries MapBiomas
#'   first and automatically falls back to Sentinel-2 for ranges outside
#'   MapBiomas coverage. All backends stream public Cloud-Optimized GeoTIFFs -
#'   no Google Earth Engine and no account.
#' @param fallback What to do when the chosen MapBiomas product has no data over
#'   a species' range (i.e. the occurrences fall outside every supported
#'   country): \code{"sentinel2"} (default) recomputes that species with the
#'   global Sentinel-2 / Esri layer; \code{"none"} leaves its conversion as
#'   \code{NA}. Ignored when \code{initiative = "sentinel2"}.
#' @param year Integer MapBiomas year. \code{NULL} (default) uses the most recent
#'   year available for the initiative (e.g. 2024 for Brazil/Colombia, 2023 for
#'   Amazonia/Paraguay, 2022 for Uruguay).
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
#'   range with protected areas from the global World Database on Protected
#'   Areas (WDPA): the share of occurrences inside protected areas and the \% of
#'   the EOO/AOO within them. Default \code{FALSE}. Requires internet unless
#'   \code{pa_src} is given. See \code{\link{wdpa_areas}}.
#' @param pa_src Optional local protected-area vector file (\code{.shp}/
#'   \code{.gpkg}/\code{.geojson}) used instead of WDPA (offline).
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
                           fallback = c("sentinel2", "none"),
                           cell_km = 2, mapbiomas = TRUE,
                           fire = FALSE, fire_collection = 4,
                           fire_host_collection = 9,
                           protected = FALSE, pa_src = NULL,
                           src = NULL, fire_src = NULL,
                           water_in_denominator = FALSE,
                           min_records = 1, verbose = TRUE) {
  .assert_points(occ, "occ")
  backend <- match.arg(backend)
  fallback <- match.arg(fallback)

  # "auto" = try MapBiomas (Brazil-flagship spatial coverage) then fall back to
  # the global Sentinel-2 / Esri layer wherever MapBiomas has no data.
  requested <- if (length(initiative) == 1L) as.character(initiative) else "brazil"
  is_auto <- length(initiative) == 1L &&
    tolower(trimws(as.character(initiative))) %in%
      c("auto", "automatic", "auto-detect", "autodetect")
  if (is_auto) {
    initiative <- "brazil"
    fallback <- "sentinel2"
  }

  ini <- .mb_resolve_initiative(initiative)
  initiative <- ini$key
  if (is.null(collection)) collection <- ini$collection
  if (is.null(year)) year <- max(ini$years)
  year <- as.integer(year)
  collection <- as.integer(collection)
  # The global Sentinel-2/Esri layer only covers 2017-2023: clamp the requested
  # year into range so an explicit initiative = "sentinel2" never asks for a
  # nonexistent tile (e.g. 2024).
  if (identical(ini$provider, "esri")) year <- .s2_clamp_year(year)
  # Fallback only applies when starting from a MapBiomas product.
  do_fallback <- identical(fallback, "sentinel2") &&
    !identical(ini$provider, "esri")
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
    aoo_geom <- .st_union_quiet(aoo$cells)
    cats <- iucn_category_B(eoo$area_km2, aoo$area_km2)

    # Per-species land-cover product actually used (may switch on fallback).
    eff_ini <- initiative; eff_year <- year; eff_coll <- collection
    eff_prov <- ini$provider

    # "auto": pick the product from this species' coordinates - a MapBiomas
    # country when the points fall in South America, else global Sentinel-2.
    if (is_auto) {
      eff_ini  <- .auto_initiative(pts)
      ri       <- .mb_resolve_initiative(eff_ini)
      eff_prov <- ri$provider
      eff_coll <- ri$collection
      eff_year <- .clamp_year(year, ri$years)
      if (verbose) message("  auto: ", ri$label, ".")
    }

    eoo_conv <- aoo_conv <- NULL
    if (mapbiomas) {
      eoo_conv <- .conversion_for(eoo$hull, eff_year, eff_coll, eff_ini, backend,
                                  src, water_in_denominator, verbose, "EOO")
      aoo_conv <- .conversion_for(aoo_geom, eff_year, eff_coll, eff_ini, backend,
                                  src, water_in_denominator, verbose, "AOO")

      # No MapBiomas data over this range -> fall back to global Sentinel-2/Esri.
      if (do_fallback && !identical(eff_prov, "esri") &&
          .terr_area(eoo_conv) == 0 && .terr_area(aoo_conv) == 0) {
        if (verbose) message("  outside ", ini$label,
                             " coverage; using global Sentinel-2/Esri land cover.")
        eff_ini <- "sentinel2"; eff_prov <- "esri"
        eff_coll <- NA_integer_; eff_year <- .s2_clamp_year(year)
        eoo_conv <- .conversion_for(eoo$hull, eff_year, eff_coll, eff_ini,
                                    "local", NULL, water_in_denominator,
                                    verbose, "EOO")
        aoo_conv <- .conversion_for(aoo_geom, eff_year, eff_coll, eff_ini,
                                    "local", NULL, water_in_denominator,
                                    verbose, "AOO")
      }
    }

    eoo_fire <- aoo_fire <- NULL
    if (fire) {
      eoo_fire <- .fire_for(eoo$hull, eoo$area_km2, fire_collection,
                            fire_host_collection, fire_src, verbose, "EOO")
      aoo_fire <- .fire_for(aoo_geom, aoo$area_km2,
                      fire_collection, fire_host_collection, fire_src,
                      verbose, "AOO")
    }

    # Land cover inside protected areas must use the same product as the range.
    pa_src_lc <- if (identical(eff_prov, "esri")) NULL else src
    pa <- NULL
    if (protected) {
      pa <- .protected_for(pts, eoo, aoo, src = pa_src, verbose = verbose)
    }
    if (protected && mapbiomas && !is.null(pa) && !is.null(pa$layer)) {
      uc_u <- .st_union_quiet(sf::st_geometry(pa$layer))
      e_nat <- .nat_in_uc(eoo$hull, uc_u, eoo_conv, eff_year, eff_coll, eff_ini,
                          backend, pa_src_lc, water_in_denominator, verbose, "EOO")
      a_nat <- .nat_in_uc(aoo_geom, uc_u, aoo_conv, eff_year,
                          eff_coll, eff_ini, backend, pa_src_lc,
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
      mapbiomas_initiative = eff_ini,
      mapbiomas_year = eff_year,
      mapbiomas_collection = eff_coll,
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
                         eoo_fire = eoo_fire, aoo_fire = aoo_fire, pa = pa,
                         initiative = eff_ini, year = eff_year,
                         collection = eff_coll)
  }
  
  summary_df <- do.call(rbind, rows)
  if (is.null(summary_df)) summary_df <- data.frame()
  structure(list(summary = summary_df, detail = detail,
                 settings = list(initiative = initiative, year = year,
                                 collection = collection,
                                 requested_initiative = requested,
                                 fallback = fallback, auto = is_auto,
                                 backend = backend, cell_km = cell_km,
                                 mapbiomas = mapbiomas, fire = fire,
                                 fire_collection = fire_collection,
                                 fire_host_collection = fire_host_collection,
                                 protected = protected,
                                 water_in_denominator = water_in_denominator)),
            class = "geoconv_assessment")
}

#' @keywords internal
#' @noRd
.pp <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else round(x, 1)

#' Terrestrial area (natural + anthropic) of a conversion result; 0 if empty.
#' Used to detect a range that falls outside the MapBiomas product's coverage.
#' @keywords internal
#' @noRd
.terr_area <- function(conv) {
  if (is.null(conv)) return(0)
  a <- suppressWarnings(as.numeric(conv$natural_km2) +
                          as.numeric(conv$anthropic_km2))
  if (length(a) == 0 || is.na(a)) 0 else a
}

#' Clamp a year into an initiative's available range (defaults to its latest).
#' @keywords internal
#' @noRd
.clamp_year <- function(year, yrs) {
  y <- suppressWarnings(as.integer(year))
  if (length(y) != 1L || is.na(y)) return(max(yrs))
  min(max(y, min(yrs)), max(yrs))
}

#' @keywords internal
#' @noRd
.conversion_for <- function(geom, year, collection, initiative, backend, src,
                            water_in_denominator, verbose, label) {
  if (is.null(geom) || length(geom) == 0 || all(sf::st_is_empty(geom))) {
    if (verbose) message(sprintf("  %s: no polygon (need >= 3 points); ",
                                 label), "conversion = NA.")
    return(NULL)
  }
  prov <- tryCatch(.mb_resolve_initiative(initiative)$provider,
                   error = function(e) "mapbiomas")
  out <- tryCatch({
    if (identical(prov, "esri")) {
      ca <- .s2_class_areas(geom, year = year, src = src)
    } else if (backend == "gee") {
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