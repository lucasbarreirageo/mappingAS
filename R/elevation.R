# -----------------------------------------------------------------------------
# Elevation preferences and elevation-refined Area of Habitat (AOH).
#
# The IUCN Red List Area of Habitat (Brooks et al. 2019) is the suitable habitat
# within a species' range that also falls within its elevation limits. sRedList
# (Cazalis et al. 2024) suggests the elevation limits from the elevation of the
# occurrence records, and refines the AOH with a digital elevation model; the
# AOHr package (Kessous) builds the AOH from a range polygon intersected with a
# habitat layer. mappingAS assimilates both ideas while staying consistent with
# its own land-cover backends (MapBiomas / Sentinel-2 via summarise_conversion):
#
#   * elevation_preferences() suggests min/max elevation from the occurrences;
#   * calc_aoh() computes the AOH as the suitable (natural) land cover within the
#     range, optionally masked to an elevation band.
#
# Elevation is read only over the EOO/AOO extent (windowed), via the optional
# 'elevatr' package (AWS Terrain Tiles, no account) or a user-supplied DEM
# (elev_src). Everything degrades gracefully to NA with a message when the DEM
# or land cover is unavailable, so an assessment is never blocked.
# -----------------------------------------------------------------------------

#' Digital elevation model over an area of interest (windowed)
#'
#' Reads a DEM restricted to \code{aoi}, from a user file/URL (\code{src}) or,
#' failing that, the public AWS Terrain Tiles through the optional
#' \pkg{elevatr} package (no account). Returns \code{NULL} (with a message) when
#' no source is available, so callers can degrade gracefully.
#' @keywords internal
#' @noRd
.get_dem <- function(aoi, z = 9, src = NULL) {
  if (!requireNamespace("terra", quietly = TRUE) ||
      !requireNamespace("sf", quietly = TRUE)) return(NULL)
  g <- tryCatch(sf::st_geometry(aoi), error = function(e) NULL)
  if (is.null(g) || !length(g)) return(NULL)
  if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
  g <- sf::st_transform(g, 4326)
  z <- .dem_zoom_cap(g, z)

  if (!is.null(src)) {
    path <- if (grepl("^https?://", src)) paste0("/vsicurl/", src) else src
    r <- tryCatch(suppressWarnings(terra::rast(path)), error = function(e) NULL)
    if (is.null(r)) return(NULL)
    v <- tryCatch(terra::vect(sf::st_as_sf(g)), error = function(e) NULL)
    if (is.null(v)) return(NULL)
    return(tryCatch(terra::crop(r, terra::ext(v), snap = "out"),
                    error = function(e) NULL))
  }

  if (!requireNamespace("elevatr", quietly = TRUE)) {
    message("Elevation needs the 'elevatr' package (install.packages(\"elevatr\")) ",
            "or a DEM passed via `src=`/`elev_src=`.")
    return(NULL)
  }
  loc <- sf::st_as_sf(g)
  ty <- as.character(sf::st_geometry_type(g))
  clip <- if (all(ty %in% c("POINT", "MULTIPOINT"))) "bbox" else "locations"
  r <- tryCatch(
    suppressWarnings(elevatr::get_elev_raster(locations = loc, z = z,
                                              clip = clip, verbose = FALSE)),
    error = function(e) NULL)
  if (is.null(r)) return(NULL)
  tryCatch(terra::rast(r), error = function(e) NULL)
}

#' Cap the terrain-tile zoom so the DEM mosaic stays within a pixel budget.
#'
#' \code{elevatr::get_elev_raster()} downloads a slippy-map terrain-tile mosaic
#' whose size grows as \code{4^z} with the zoom, and is otherwise unbounded by
#' the area requested. Over a large range (e.g. a wide-ranging montane species)
#' a high zoom would fetch a multi-gigabyte mosaic and exhaust memory, which the
#' operating system answers by killing R ("Terminated"). This lowers the zoom
#' until the estimated mosaic is at most \code{max_px} pixels, so the DEM read
#' stays windowed and bounded exactly like the land-cover read. The DEM is
#' always resampled to a coarse grid afterwards, so a lower zoom over a large
#' area costs no meaningful accuracy for the elevation mask.
#' @keywords internal
#' @noRd
.dem_zoom_cap <- function(g, z, max_px = 4e6) {
  z <- suppressWarnings(as.integer(z)[1])
  if (!is.finite(z)) z <- 9L
  bb <- tryCatch(sf::st_bbox(g), error = function(e) NULL)
  if (is.null(bb)) return(z)
  wdeg <- as.numeric(bb[["xmax"]] - bb[["xmin"]])
  hdeg <- as.numeric(bb[["ymax"]] - bb[["ymin"]])
  if (!is.finite(wdeg) || !is.finite(hdeg) || wdeg <= 0 || hdeg <= 0) return(z)
  # Approximate pixels of the terrain-tile mosaic at zoom zz (256-px tiles on a
  # 360 x 180 degree global grid); conservative enough to cap the download.
  est_px <- function(zz)
    (2^zz * wdeg / 360) * 256 * (2^zz * hdeg / 180) * 256
  while (z > 1L && est_px(z) > max_px) z <- z - 1L
  z
}

#' Suggest elevation preferences from the occurrences' elevation
#'
#' Extracts the elevation of each occurrence from a digital elevation model and
#' suggests the species' elevation limits, mirroring the sRedList step that
#' pre-fills elevation preferences from occurrence records (Cazalis \emph{et al.}
#' 2024). The suggested minimum is rounded down and the maximum rounded up to the
#' nearest \code{round_to} metres, as sRedList does.
#'
#' The DEM is read only over the occurrences' extent (windowed), from
#' \code{src} or the public AWS Terrain Tiles via the optional \pkg{elevatr}
#' package. When no DEM is available the function returns \code{NA} values with a
#' message rather than raising an error.
#'
#' @param points An \code{sf} of POINT geometries (one species). Use
#'   \code{\link{read_occurrences}} to produce it.
#' @param z Terrain-tile zoom level passed to \code{elevatr::get_elev_raster}
#'   (default \code{9}, ~300 m; higher is finer but heavier). Ignored when
#'   \code{src} is given.
#' @param src Optional path/URL to a DEM GeoTIFF used instead of \pkg{elevatr}
#'   (read windowed through GDAL \code{/vsicurl/} for a URL).
#' @param round_to Rounding step in metres for the suggested limits (default
#'   \code{100}); \code{0}/\code{NA} disables rounding.
#' @return A list with \code{n} (occurrences with a value), \code{min},
#'   \code{max}, \code{median} (metres), \code{suggested_min} /
#'   \code{suggested_max} (rounded limits) and \code{values} (the per-occurrence
#'   elevations). All-\code{NA} when no DEM is available.
#' @references
#' Cazalis V. \emph{et al.} (2024) Accelerating and standardising IUCN Red List
#'   assessments with sRedList. \emph{Biological Conservation} 298:110761.
#'   \doi{10.1016/j.biocon.2024.110761}
#' @seealso \code{\link{calc_aoh}}
#' @examples
#' \donttest{
#' f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
#' occ <- read_occurrences(f)
#' sp1 <- occ[occ$species == occ$species[1], ]
#' # Needs the 'elevatr' package (Suggests) and internet:
#' elevation_preferences(sp1)
#' }
#' @export
elevation_preferences <- function(points, z = 9, src = NULL, round_to = 100) {
  .assert_points(points, "points")
  pts <- sf::st_transform(points, 4326)
  out <- list(n = 0L, min = NA_real_, max = NA_real_, median = NA_real_,
              suggested_min = NA_real_, suggested_max = NA_real_,
              values = numeric(0), z = z)
  dem <- .get_dem(pts, z = z, src = src)
  if (is.null(dem)) return(out)

  vals <- tryCatch(
    terra::extract(dem, terra::vect(sf::st_geometry(pts)))[, 2],
    error = function(e) NULL)
  vals <- suppressWarnings(as.numeric(vals))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) return(out)

  out$n <- length(vals)
  out$min <- min(vals); out$max <- max(vals)
  out$median <- stats::median(vals); out$values <- vals
  rt <- suppressWarnings(as.numeric(round_to))[1]
  if (is.finite(rt) && rt > 0) {
    out$suggested_min <- floor(out$min / rt) * rt
    out$suggested_max <- ceiling(out$max / rt) * rt
  } else {
    out$suggested_min <- out$min; out$suggested_max <- out$max
  }
  out
}

#' Area of Habitat (AOH): suitable land cover within a range and elevation band
#'
#' Computes the terrestrial Area of Habitat as the \emph{suitable} land cover
#' within a range polygon, optionally restricted to an elevation band, following
#' the Area-of-Habitat concept (Brooks \emph{et al.} 2019) and the sRedList
#' workflow (Cazalis \emph{et al.} 2024). "Suitable" defaults to the
#' \strong{natural} conservation group of the land-cover legend, so the result is
#' consistent with the natural-versus-converted metrics used elsewhere in
#' \pkg{mappingAS}; supply \code{suitable_groups} to change it.
#'
#' The land cover is read over the range with the same backend as
#' \code{\link{assess_species}} (MapBiomas or Sentinel-2/Esri) and, when an
#' elevation band is given, an elevation model read over the same extent
#' (windowed; via \code{elev_src} or the optional \pkg{elevatr} package) is
#' resampled to the land-cover grid and used to mask out pixels outside
#' \code{[elev_min, elev_max]}. The remaining suitable area is the AOH. The read
#' is bounded by \code{max_pixels} (the land cover is aggregated with a majority
#' rule beyond that) so a large range does not exhaust memory. This AOH can be
#' passed as the habitat-based upper AOO bound to \code{\link{aoo_bounds}}.
#'
#' Every failure (no DEM, no land cover, an unsupported backend) degrades to
#' \code{NA} with a message rather than an error.
#'
#' @param range An \code{sf}/\code{sfc} range \strong{polygon} (e.g. the EOO hull
#'   from \code{\link{calc_eoo}} or the AOO cell union). Points yield \code{NA}.
#' @param year,collection,initiative,backend,src Land-cover selection, as in
#'   \code{\link{assess_species}} / \code{\link{mb_raster_local}}.
#' @param elev_src Optional DEM path/URL used instead of \pkg{elevatr}.
#' @param elev_min,elev_max Elevation band in metres. \code{NA} (default) on
#'   either side leaves that side open; both \code{NA} skips the elevation mask
#'   and the AOH is the suitable land cover alone.
#' @param z Terrain-tile zoom for \pkg{elevatr} (default \code{9}). Ignored when
#'   \code{elev_src} is given.
#' @param suitable_groups Conservation group(s) counted as suitable habitat
#'   (default \code{"natural"}). See \code{\link{mb_legend}}.
#' @param suitable_codes Optional integer land-cover class codes counted as
#'   suitable, overriding \code{suitable_groups} (e.g. the specific classes that
#'   make ecological sense for the species).
#' @param marginal_codes Optional integer class codes of \emph{marginal} or
#'   unknown-suitability habitat (sRedList's "Marginal_Unknown"). When supplied,
#'   a second, broader AOH (\code{aoh_max_km2}) is returned that adds these to
#'   the suitable classes, giving a strict-to-broad AOH range.
#' @param points Optional \code{sf} of occurrence points; when given, the
#'   \code{point_prevalence} (share of occurrences inside the suitable habitat)
#'   validation metric is returned.
#' @param max_pixels Pixel budget for the land-cover read (default \code{5e7}).
#' @return A list with \code{aoh_km2} (suitable area within the elevation band,
#'   the AOH), \code{aoh_max_km2} (the broader AOH including marginal classes;
#'   equal to \code{aoh_km2} when none), \code{aoo_upper_km2} /
#'   \code{aoo_upper_cells} (the AOH rescaled to the 2 km reference scale - the
#'   IUCN-compliant AOO upper bound - and its cell count), \code{model_prevalence}
#'   (share of the range that is suitable habitat) and \code{point_prevalence}
#'   (share of \code{points} inside the habitat; \code{NA} without \code{points}),
#'   \code{suitable_km2} (suitable area \emph{without} the elevation mask, for
#'   reference), \code{elev_min}, \code{elev_max}, \code{elev_applied},
#'   \code{suitable_groups}, \code{suitable_codes} and \code{marginal_codes}.
#' @references
#' Brooks T.M. \emph{et al.} (2019) Measuring terrestrial Area of Habitat (AOH)
#'   and its utility for the IUCN Red List. \emph{Trends in Ecology & Evolution}
#'   34:977-986. \doi{10.1016/j.tree.2019.06.009}
#'
#' Cazalis V. \emph{et al.} (2024) Accelerating and standardising IUCN Red List
#'   assessments with sRedList. \emph{Biological Conservation} 298:110761.
#'   \doi{10.1016/j.biocon.2024.110761}
#' @seealso \code{\link{aoo_bounds}}, \code{\link{elevation_preferences}},
#'   \code{\link{summarise_conversion}}
#' @examples
#' \donttest{
#' f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
#' occ <- read_occurrences(f)
#' sp1 <- occ[occ$species == occ$species[1], ]
#' hull <- calc_eoo(sp1)$hull
#' # Reads MapBiomas land cover (and a DEM if elevatr is installed):
#' calc_aoh(hull, year = 2024, elev_min = 0, elev_max = 1500)
#' }
#' @export
calc_aoh <- function(range, year = NULL, collection = NULL,
                     initiative = "brazil", backend = "local", src = NULL,
                     elev_src = NULL, elev_min = NA, elev_max = NA, z = 9,
                     suitable_groups = "natural", suitable_codes = NULL,
                     marginal_codes = NULL, points = NULL, max_pixels = 5e7) {
  out <- list(aoh_km2 = NA_real_, aoh_max_km2 = NA_real_, suitable_km2 = NA_real_,
              aoo_upper_km2 = NA_real_, aoo_upper_cells = NA_integer_,
              model_prevalence = NA_real_, point_prevalence = NA_real_,
              elev_min = elev_min, elev_max = elev_max,
              elev_applied = FALSE, suitable_groups = suitable_groups,
              suitable_codes = suitable_codes, marginal_codes = marginal_codes)
  if (!requireNamespace("terra", quietly = TRUE)) {
    message("calc_aoh() needs the 'terra' package."); return(out)
  }
  g <- .as_range_polygon(range)
  if (is.null(g)) {
    message("calc_aoh(): `range` must be a polygon (EOO/AOO); returning NA.")
    return(out)
  }
  coll <- collection
  if (is.null(coll))
    coll <- tryCatch(.mb_resolve_initiative(initiative)$collection,
                     error = function(e) 10L)

  # 1. Land cover over the range (bounded to protect memory).
  lc <- tryCatch(
    mb_raster_local(g, year = year, collection = collection,
                    initiative = initiative, src = src, mask = TRUE),
    error = function(e) NULL)
  if (is.null(lc)) {
    message("calc_aoh(): land cover unavailable for this range; returning NA.")
    return(out)
  }
  npix <- suppressWarnings(prod(dim(lc)[1:2]))
  if (is.finite(npix) && npix > max_pixels) {
    fct <- ceiling(sqrt(npix / max_pixels))
    lc <- tryCatch(terra::aggregate(lc, fact = fct, fun = "modal", na.rm = TRUE),
                   error = function(e) lc)
  }

  # Suitable area without the elevation mask (reference).
  out$suitable_km2 <- .suitable_area(lc, coll, initiative, suitable_groups,
                                     suitable_codes)

  # 2. Optional elevation mask.
  lc_use <- lc
  if (is.finite(elev_min) || is.finite(elev_max)) {
    dem <- .get_dem(g, z = z, src = elev_src)
    if (!is.null(dem)) {
      # project (reproject + align) the DEM onto the land-cover grid, so it works
      # even when the land-cover CRS differs from the DEM's (e.g. Sentinel-2).
      dem2 <- tryCatch(terra::project(dem, lc, method = "bilinear"),
                       error = function(e) NULL)
      if (!is.null(dem2)) {
        lo <- if (is.finite(elev_min)) elev_min else -Inf
        hi <- if (is.finite(elev_max)) elev_max else Inf
        elevok <- tryCatch(terra::ifel(dem2 >= lo & dem2 <= hi, 1, NA),
                           error = function(e) NULL)
        if (!is.null(elevok)) {
          lc_use <- tryCatch(terra::mask(lc, elevok), error = function(e) lc)
          out$elev_applied <- !is.null(elevok)
        }
      }
    }
  }

  out$aoh_km2 <- .suitable_area(lc_use, coll, initiative, suitable_groups,
                                suitable_codes)
  # Maximum AOH: suitable PLUS marginal / unknown-suitability classes (sRedList's
  # Marginal_Unknown), giving an AOH range (strict = aoh_km2, broad = aoh_max_km2).
  upper_codes <- suitable_codes
  if (!is.null(marginal_codes) && length(marginal_codes)) {
    mx <- unique(c(suitable_codes, marginal_codes))
    upper_codes <- mx
    out$aoh_max_km2 <- .suitable_area(lc_use, coll, initiative, suitable_groups, mx)
  } else {
    out$aoh_max_km2 <- out$aoh_km2
  }

  # IUCN 4.10.7 condition (iii): the AOO upper bound must be taken at the
  # reference scale, i.e. the 2 x 2 km cells intersecting the (broad) suitable
  # habitat - not the raw habitat area, which is measured at finer resolution.
  cells <- .aoh_2km_cells(lc_use, suitable_groups, upper_codes, coll, initiative, g)
  if (is.finite(cells)) {
    out$aoo_upper_cells <- as.integer(cells)
    out$aoo_upper_km2 <- cells * 4
  }

  # Validation metrics (Lumbierres et al. 2022): model prevalence = share of the
  # range that is suitable habitat; point prevalence = share of occurrences that
  # fall inside the suitable habitat (within the elevation band).
  range_km2 <- tryCatch(.spheroid_area_km2(g), error = function(e) NA_real_)
  if (is.finite(range_km2) && range_km2 > 0 && is.finite(out$aoh_km2))
    out$model_prevalence <- min(1, out$aoh_km2 / range_km2)
  if (!is.null(points))
    out$point_prevalence <- .point_prevalence(lc_use, suitable_groups,
                                              suitable_codes, coll, initiative,
                                              points)
  out
}

#' Suitable-habitat area (km^2) of a classified land-cover raster: tabulate class
#' areas, group them with the legend (via summarise_conversion), and sum the
#' chosen conservation group(s). Returns \code{NA} on any failure.
#' @keywords internal
#' @noRd
.suitable_area <- function(lc, collection, initiative, groups, codes = NULL) {
  ca <- tryCatch(mb_class_areas_raster(lc), error = function(e) NULL)
  if (is.null(ca) || !nrow(ca)) return(NA_real_)
  # Specific class codes take precedence over the conservation group(s): sum the
  # per-class areas of the chosen codes directly.
  if (!is.null(codes) && length(codes)) {
    codes <- suppressWarnings(as.integer(codes))
    a <- sum(ca$area_km2[ca$code %in% codes], na.rm = TRUE)
    return(if (is.finite(a)) a else NA_real_)
  }
  sc <- tryCatch(summarise_conversion(ca, collection = collection,
                                      initiative = initiative),
                 error = function(e) NULL)
  if (is.null(sc)) return(NA_real_)
  keys <- paste0(groups, "_km2")
  vals <- suppressWarnings(as.numeric(unlist(sc[keys])))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) return(NA_real_)
  sum(vals)
}

#' Resolve the suitable land-cover class codes: explicit \code{codes} if given,
#' otherwise the legend codes of the chosen conservation \code{groups}.
#' @keywords internal
#' @noRd
.resolve_suitable_codes <- function(codes, groups, collection, initiative) {
  if (!is.null(codes) && length(codes))
    return(unique(stats::na.omit(as.integer(codes))))
  leg <- tryCatch(mb_legend(collection, initiative), error = function(e) NULL)
  if (is.null(leg)) return(integer(0))
  unique(stats::na.omit(as.integer(leg$code[leg$group %in% groups])))
}

#' Number of 2 x 2 km reference-scale cells that intersect the suitable habitat
#' (IUCN 4.10.7 condition iii): reproject a binary suitable mask onto a 2 km
#' data-centred equal-area grid (a cell counts as suitable if any underlying
#' habitat pixel falls in it) and count the occupied cells. \code{NA} on failure.
#' @keywords internal
#' @noRd
.aoh_2km_cells <- function(lc, groups, codes, collection, initiative, aoi) {
  if (!requireNamespace("terra", quietly = TRUE)) return(NA_real_)
  scodes <- .resolve_suitable_codes(codes, groups, collection, initiative)
  if (!length(scodes)) return(NA_real_)
  bin <- tryCatch(terra::subst(lc, from = scodes, to = 1L, others = NA),
                  error = function(e) NULL)
  if (is.null(bin)) return(NA_real_)
  laea <- tryCatch(laea_crs(sf::st_geometry(aoi)), error = function(e) NA_character_)
  if (is.na(laea)) return(NA_real_)
  s2k <- tryCatch(terra::project(bin, y = laea, res = 2000, method = "max"),
                  error = function(e) NULL)
  if (is.null(s2k)) return(NA_real_)
  n <- tryCatch(terra::global(s2k, fun = "notNA")[1, 1], error = function(e) NA_real_)
  if (!is.finite(n)) NA_real_ else as.numeric(n)
}

#' Point prevalence (Lumbierres et al. 2022): fraction of occurrence points that
#' fall inside the suitable habitat of a (possibly elevation-masked) land-cover
#' raster. \code{NA} on failure.
#' @keywords internal
#' @noRd
.point_prevalence <- function(lc, groups, codes, collection, initiative, points) {
  if (!requireNamespace("terra", quietly = TRUE) ||
      !requireNamespace("sf", quietly = TRUE)) return(NA_real_)
  scodes <- .resolve_suitable_codes(codes, groups, collection, initiative)
  if (!length(scodes)) return(NA_real_)
  pg <- tryCatch(sf::st_geometry(sf::st_transform(points, terra::crs(lc))),
                 error = function(e) NULL)
  if (is.null(pg) || !length(pg)) return(NA_real_)
  ex <- tryCatch(terra::extract(lc, terra::vect(pg)), error = function(e) NULL)
  if (is.null(ex) || ncol(ex) < 2) return(NA_real_)
  v <- ex[[2]]
  suit <- !is.na(v) & (v %in% scodes)
  if (!length(suit)) return(NA_real_)
  mean(suit)
}

#' Binary suitable-habitat mask over a range (1 = suitable, NA = not), read at a
#' bounded display resolution and optionally restricted to an elevation band.
#' Shared by \code{\link{map_aoh}} and \code{fragment_habitat()} so the mapped
#' AOH and the fragmentation patches use the same definition. Returns a
#' \pkg{terra} \code{SpatRaster}, or \code{NULL} on any failure.
#' @keywords internal
#' @noRd
.suitable_mask_raster <- function(range, year = NULL, collection = NULL,
                                  initiative = "brazil", src = NULL,
                                  suitable_groups = "natural",
                                  suitable_codes = NULL, elev_min = NA,
                                  elev_max = NA, z = 9, elev_src = NULL,
                                  max_pixels = 4000) {
  if (!requireNamespace("terra", quietly = TRUE)) return(NULL)
  g <- .as_range_polygon(range)
  if (is.null(g)) return(NULL)
  coll <- collection
  if (is.null(coll))
    coll <- tryCatch(.mb_resolve_initiative(initiative)$collection,
                     error = function(e) NULL)
  r <- tryCatch(.mb_raster_display(g, year, coll, src, max_pixels = max_pixels,
                                   crs = NULL, initiative = initiative),
                error = function(e) NULL)
  if (is.null(r)) return(NULL)
  leg <- tryCatch(mb_legend(coll, initiative), error = function(e) NULL)
  codes <- suitable_codes
  if ((is.null(codes) || !length(codes)) && !is.null(leg))
    codes <- leg$code[leg$group %in% suitable_groups]
  codes <- unique(stats::na.omit(as.integer(codes)))
  if (!length(codes)) return(NULL)
  vals <- tryCatch(terra::values(r)[, 1], error = function(e) NULL)
  if (is.null(vals)) return(NULL)
  mv <- ifelse(!is.na(vals) & vals %in% codes, 1L, NA_integer_)
  s <- tryCatch(terra::setValues(r, mv), error = function(e) NULL)
  if (is.null(s)) return(NULL)
  if (is.finite(elev_min) || is.finite(elev_max)) {
    dem <- .get_dem(g, z = z, src = elev_src)
    if (!is.null(dem)) {
      dem2 <- tryCatch(terra::project(dem, s, method = "bilinear"),
                       error = function(e) NULL)
      if (!is.null(dem2)) {
        lo <- if (is.finite(elev_min)) elev_min else -Inf
        hi <- if (is.finite(elev_max)) elev_max else Inf
        elevok <- tryCatch(terra::ifel(dem2 >= lo & dem2 <= hi, 1L, NA),
                           error = function(e) NULL)
        if (!is.null(elevok))
          s <- tryCatch(terra::mask(s, elevok), error = function(e) s)
      }
    }
  }
  s
}
