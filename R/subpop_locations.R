# -----------------------------------------------------------------------------
# Estimate of number of subpopulations and number of locations.
#
# The spatial *methods* implemented here are the standard ones used in
# preliminary IUCN Criterion B screening and popularised for R by the ConR
# package (Dauby et al. 2017, Ecology and Evolution 7:11292-11303,
# doi:10.1002/ece3.3704, <https://github.com/gdauby/ConR>):
#   * subpopulations - the "circular buffer" method of Rivers et al. (2010,
#     Biological Conservation 143:2545-2560);
#   * locations - the occupied-grid-cell count of the IUCN guidelines.
#
# The code below is an INDEPENDENT, clean-room implementation written for
# mappingAS. ConR is distributed under the GPL (>= 3); mappingAS is MIT. No
# ConR source code was copied, adapted or translated - only the published,
# non-copyrightable spatial ideas are reused, and ConR is cited so the origin
# of the ideas is properly credited. The grid machinery reuses mappingAS's own
# AOO helper (.aoo_min_grid), which is itself an original implementation.
# -----------------------------------------------------------------------------

#' Estimate of number of subpopulations (circular-buffer method)
#'
#' Estimates the number of subpopulations by the "circular buffer" method: a
#' circle of radius \code{resol_km} is drawn around every occurrence, the
#' circles are dissolved (unioned), and the number of resulting disjoint
#' polygons is the estimated number of subpopulations. Occurrences whose
#' buffers overlap are treated as belonging to the same subpopulation.
#'
#' This is the approach popularised by the \pkg{ConR} package
#' (Dauby \emph{et al.} 2017) and originally proposed by Rivers \emph{et al.}
#' (2010). The buffering is done on a data-centred Lambert Azimuthal Equal-Area
#' projection (see \code{\link{laea_crs}}), consistent with the EOO/AOO
#' computations elsewhere in \pkg{mappingAS}.
#'
#' The buffer radius follows the widely used default of one tenth of the
#' greatest distance separating two occurrences (Rivers \emph{et al.} 2010):
#' when \code{resol_km} is \code{NULL} it is computed from the data. Because the
#' most distant pair of points always lies on the convex hull, the maximum
#' distance is measured over the hull vertices only (great-circle distance on
#' the WGS84 ellipsoid), which keeps the computation fast for large datasets.
#'
#' @param points An \code{sf} of POINT geometries (one species). Use
#'   \code{\link{read_occurrences}} to produce it.
#' @param resol_km Circle radius in kilometres. \code{NULL} (default) uses one
#'   tenth of the maximum distance between two occurrences.
#' @return A list with \code{n_subpop} (integer estimate), \code{resol_km} (the
#'   radius actually used), \code{n_unique} (distinct coordinates),
#'   \code{subpop} (the dissolved subpopulation polygons as an \code{sfc} in
#'   WGS84, or \code{NULL}) and \code{crs_laea} (the equal-area proj string).
#' @references
#' Dauby G. \emph{et al.} (2017) ConR: An R package to assist large-scale
#'   multispecies preliminary conservation assessments using distribution data.
#'   \emph{Ecology and Evolution} 7:11292-11303. \doi{10.1002/ece3.3704}
#'
#' Rivers M.C. \emph{et al.} (2010) How many herbarium specimens are needed to
#'   detect threatened species? \emph{Biological Conservation} 143:2545-2560.
#' @seealso \code{\link{calc_locations}}, \code{\link{calc_aoo}}
#' @examples
#' f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
#' occ <- read_occurrences(f)
#' sp1 <- occ[occ$species == occ$species[1], ]
#' calc_subpop(sp1)$n_subpop
#' @export
calc_subpop <- function(points, resol_km = NULL) {
  .assert_points(points, "points")
  pts <- sf::st_geometry(sf::st_transform(points, 4326))
  co <- unique(round(sf::st_coordinates(pts)[, c("X", "Y"), drop = FALSE], 8))
  n_uni <- nrow(co)

  crs_laea <- tryCatch(laea_crs(pts), error = function(e) NA_character_)
  out <- list(n_subpop = NA_integer_, resol_km = NA_real_, n_unique = n_uni,
              subpop = NULL, crs_laea = crs_laea)
  if (n_uni < 1L) return(out)

  # A single occupied point is, by definition, a single subpopulation.
  if (n_uni == 1L) {
    out$n_subpop <- 1L
    out$resol_km <- if (is.null(resol_km)) NA_real_ else as.numeric(resol_km)
    return(out)
  }

  radius_km <- if (!is.null(resol_km)) as.numeric(resol_km)[1]
               else .subpop_default_radius(co)
  if (!is.finite(radius_km) || radius_km <= 0) {
    message("Subpopulations: non-positive buffer radius; returning NA.")
    return(out)
  }

  upts <- sf::st_as_sf(data.frame(X = co[, 1], Y = co[, 2]),
                       coords = c("X", "Y"), crs = 4326)
  upts_p <- sf::st_transform(sf::st_geometry(upts), crs_laea)

  buf <- sf::st_buffer(upts_p, radius_km * 1000)
  merged <- .st_union_quiet(buf)
  parts <- suppressWarnings(sf::st_cast(merged, "POLYGON"))

  out$n_subpop <- length(sf::st_geometry(parts))
  out$resol_km <- radius_km
  out$subpop <- sf::st_transform(parts, 4326)
  out
}

#' Default subpopulation buffer radius: one tenth of the maximum inter-point
#' distance (Rivers et al. 2010), measured over convex-hull vertices.
#' @keywords internal
#' @noRd
.subpop_default_radius <- function(co) {
  m <- .max_interpoint_km(co)
  if (!is.finite(m) || m <= 0) return(NA_real_)
  m / 10
}

#' Maximum great-circle distance (km) between any two coordinates, measured
#' over the convex-hull vertices (the most distant pair always lies on the
#' hull, so this is exact and fast for large datasets).
#' @keywords internal
#' @noRd
.max_interpoint_km <- function(co) {
  if (nrow(co) < 2L) return(NA_real_)
  idx <- if (nrow(co) >= 3L) grDevices::chull(co[, 1], co[, 2])
         else seq_len(nrow(co))
  hv <- co[idx, , drop = FALSE]
  hp <- sf::st_as_sf(data.frame(X = hv[, 1], Y = hv[, 2]),
                     coords = c("X", "Y"), crs = 4326)
  d <- suppressWarnings(sf::st_distance(hp))
  as.numeric(max(d)) / 1000
}

#' Occupied-cell count for a set of projected coordinates: the minimum over
#' translated grids (reusing mappingAS's own AOO grid search).
#' @keywords internal
#' @noRd
.grid_count <- function(co, size, n_rep) {
  best <- .aoo_min_grid(co, size, max(1L, as.integer(n_rep)))
  cells <- unique(data.frame(ix = best$ix, iy = best$iy))
  list(n = nrow(cells), cells = cells, best = best)
}

#' Build the occupied-cell polygons for a grid result from \code{.grid_count()}.
#' @keywords internal
#' @noRd
.grid_polys <- function(best, cells, size, crs) {
  n <- nrow(cells)
  if (n == 0L) return(sf::st_sfc(crs = crs))
  polys <- vector("list", n)
  for (i in seq_len(n)) {
    x0 <- best$ox + cells$ix[i] * size
    y0 <- best$oy + cells$iy[i] * size
    polys[[i]] <- sf::st_polygon(list(rbind(
      c(x0, y0), c(x0 + size, y0), c(x0 + size, y0 + size),
      c(x0, y0 + size), c(x0, y0)
    )))
  }
  sf::st_sfc(polys, crs = crs)
}

#' Concatenate up to two (possibly NULL) sfc objects, returning an empty sfc
#' with the given CRS when both are absent.
#' @keywords internal
#' @noRd
.sfc_c <- function(a, b, crs) {
  parts <- Filter(function(x) !is.null(x) && length(x) > 0, list(a, b))
  if (!length(parts)) return(sf::st_sfc(crs = crs))
  do.call(c, parts)
}

#' Estimate of number of locations (occupied-grid-cell method, optionally
#' protected-area aware)
#'
#' Estimates the number of "locations" (in the IUCN sense: geographically or
#' ecologically distinct areas in which a single threatening event can rapidly
#' affect all individuals) by overlaying a regular grid and counting the
#' occupied cells, optionally decoupling occurrences that fall inside protected
#' areas from those outside. This mirrors the two complementary approaches used
#' by the \pkg{ConR} package (Dauby \emph{et al.} 2017): the grid method, and
#' the protected-area integration (\code{method_protected}).
#'
#' As for the AOO, the occupied-cell count depends on where the grid is placed,
#' so several randomly translated grids are tested and the \emph{smallest}
#' count is returned (the most conservative, i.e. most threatened, estimate).
#' The grid is built on a data-centred Lambert Azimuthal Equal-Area projection
#' and this function reuses \pkg{mappingAS}'s own AOO grid machinery, so it
#' differs from \code{\link{calc_aoo}} only in the (larger) default cell size.
#'
#' \strong{Cell size.} By default a fixed cell of \code{grid_km} (10 km) is
#' used, representing the scale at which a single threat could affect the whole
#' occupied cell. Alternatively a species-specific "sliding scale"
#' (Rivers \emph{et al.} 2010) is available: set \code{cell_scale} to a fraction
#' (e.g. \code{0.05}) and the cell side becomes that fraction of the maximum
#' distance between two occurrences (\code{cell_scale} then overrides
#' \code{grid_km}).
#'
#' \strong{Protected areas.} When \code{protected} is a protected-area layer
#' (e.g. from \code{\link{protected_areas}}), occurrences are split into those
#' inside and outside protected areas, because a subpopulation inside a
#' protected area is not subject to the same threats as one outside:
#' \itemize{
#'   \item \code{method_protected = "no_more_than_one"} (default): every
#'     protected area holding at least one occurrence counts as exactly one
#'     location (all occurrences within a protected area share a location);
#'     occurrences outside protected areas are gridded as usual. The total is
#'     (occupied cells outside) + (number of protected areas with occurrences).
#'   \item \code{method_protected = "other"}: occurrences inside protected areas
#'     are gridded \emph{separately} from those outside, and the two counts are
#'     added. Two occurrences closer than the cell size are then still counted
#'     as separate locations when one is inside and the other outside a
#'     protected area.
#' }
#' With \code{protected = NULL} (the default) no protected-area information is
#' used and the plain grid count is returned.
#'
#' @param points An \code{sf} of POINT geometries (one species).
#' @param grid_km Grid cell side length in kilometres (default \code{10}).
#'   Ignored when \code{cell_scale} is supplied.
#' @param n_rep Integer; number of randomly translated grids to test (default
#'   \code{30}). The minimum occupied-cell count over the replicates is
#'   returned.
#' @param protected Optional protected-area layer (an \code{sf}/\code{sfc} of
#'   polygons, e.g. from \code{\link{protected_areas}}). \code{NULL} (default)
#'   uses the plain grid method.
#' @param method_protected How protected areas enter the count when
#'   \code{protected} is given: \code{"no_more_than_one"} (default) or
#'   \code{"other"}. See Details.
#' @param cell_scale Optional numeric fraction for the sliding-scale cell size:
#'   the cell side becomes \code{cell_scale} times the maximum distance between
#'   two occurrences. When supplied it overrides \code{grid_km}.
#' @return A list with \code{n_locations} (integer estimate), \code{grid_km}
#'   (the cell size actually used), \code{method} (\code{"grid"},
#'   \code{"no_more_than_one"} or \code{"other"}), \code{n_out} / \code{n_in}
#'   (locations outside / inside protected areas; \code{n_in} is \code{NA} for
#'   the plain grid), \code{n_cells}, \code{cells} (gridded location cells as an
#'   \code{sfc} in WGS84), \code{pa_locations} (the protected areas counted as
#'   locations under \code{"no_more_than_one"}, else \code{NULL}) and
#'   \code{crs_laea}.
#' @references
#' Dauby G. \emph{et al.} (2017) ConR: An R package to assist large-scale
#'   multispecies preliminary conservation assessments using distribution data.
#'   \emph{Ecology and Evolution} 7:11292-11303. \doi{10.1002/ece3.3704}
#'
#' Rivers M.C. \emph{et al.} (2010) How many herbarium specimens are needed to
#'   detect threatened species? \emph{Biological Conservation} 143:2545-2560.
#'
#' IUCN Standards and Petitions Committee. Guidelines for Using the IUCN Red
#'   List Categories and Criteria.
#' @seealso \code{\link{calc_subpop}}, \code{\link{calc_aoo}},
#'   \code{\link{protected_areas}}
#' @examples
#' f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
#' occ <- read_occurrences(f)
#' sp1 <- occ[occ$species == occ$species[1], ]
#' calc_locations(sp1)$n_locations
#' @export
calc_locations <- function(points, grid_km = 10, n_rep = 30,
                           protected = NULL,
                           method_protected = c("no_more_than_one", "other"),
                           cell_scale = NULL) {
  .assert_points(points, "points")
  method_protected <- match.arg(method_protected)
  pts <- sf::st_geometry(sf::st_transform(points, 4326))
  crs_laea <- laea_crs(pts)
  pts_p <- sf::st_transform(pts, crs_laea)
  co <- sf::st_coordinates(pts_p)[, c("X", "Y"), drop = FALSE]

  # Effective cell size: fixed grid_km, or a fraction of the maximum distance
  # between occurrences (sliding scale, Rivers et al. 2010).
  cell_km_eff <- grid_km
  if (!is.null(cell_scale)) {
    co_ll <- unique(round(sf::st_coordinates(pts)[, c("X", "Y"),
                                                  drop = FALSE], 8))
    maxd <- .max_interpoint_km(co_ll)
    if (is.finite(maxd) && maxd > 0) cell_km_eff <- as.numeric(cell_scale) * maxd
  }
  size <- cell_km_eff * 1000

  have_pa <- !is.null(protected) &&
    inherits(protected, c("sf", "sfc")) &&
    length(sf::st_geometry(protected)) > 0

  # --- Plain grid method (no protected areas) ---
  if (!have_pa) {
    g <- .grid_count(co, size, n_rep)
    cells_p <- .grid_polys(g$best, g$cells, size, crs_laea)
    return(list(
      n_locations = as.integer(g$n),
      grid_km = cell_km_eff, method = "grid",
      n_out = as.integer(g$n), n_in = NA_integer_,
      n_cells = as.integer(g$n),
      cells = sf::st_transform(cells_p, 4326),
      pa_locations = NULL, crs_laea = crs_laea))
  }

  # --- Protected-area aware methods ---
  pa <- sf::st_make_valid(sf::st_transform(sf::st_geometry(protected), 4326))
  pa_u <- .st_union_quiet(pa)
  inside <- lengths(suppressMessages(sf::st_intersects(pts, pa_u))) > 0

  co_out <- co[!inside, , drop = FALSE]
  co_in  <- co[inside, , drop = FALSE]

  # Occurrences outside protected areas: gridded as usual.
  n_out <- 0L; cells_out <- NULL
  if (nrow(co_out) > 0L) {
    g_out <- .grid_count(co_out, size, n_rep)
    n_out <- g_out$n
    cells_out <- .grid_polys(g_out$best, g_out$cells, size, crs_laea)
  }

  n_in <- 0L; cells_in <- NULL; pa_loc_geom <- NULL
  if (nrow(co_in) > 0L) {
    if (method_protected == "no_more_than_one") {
      # Each protected area with >= 1 occurrence counts as a single location.
      hit <- suppressMessages(sf::st_intersects(pts[inside], pa))
      pa_idx <- sort(unique(unlist(hit)))
      n_in <- length(pa_idx)
      if (n_in > 0L) pa_loc_geom <- sf::st_transform(pa[pa_idx], crs_laea)
    } else {
      # "other": occurrences inside protected areas gridded separately.
      g_in <- .grid_count(co_in, size, n_rep)
      n_in <- g_in$n
      cells_in <- .grid_polys(g_in$best, g_in$cells, size, crs_laea)
    }
  }

  cells_geom <- .sfc_c(cells_out, cells_in, crs_laea)
  cells_ll <- sf::st_transform(cells_geom, 4326)
  pa_loc_ll <- if (is.null(pa_loc_geom)) NULL
               else sf::st_transform(pa_loc_geom, 4326)

  list(
    n_locations = as.integer(n_out + n_in),
    grid_km = cell_km_eff,
    method = method_protected,
    n_out = as.integer(n_out), n_in = as.integer(n_in),
    n_cells = as.integer(length(sf::st_geometry(cells_ll))),
    cells = cells_ll,
    pa_locations = pa_loc_ll,
    crs_laea = crs_laea
  )
}
