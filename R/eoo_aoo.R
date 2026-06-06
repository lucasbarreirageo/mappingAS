#' Extent of Occurrence (EOO) via minimum convex polygon
#'
#' Computes the EOO as the area of the convex hull (minimum convex polygon)
#' enclosing all occurrence points, following IUCN Red List guidelines and
#' GeoCat. The hull is built and measured on a data-centred equal-area
#' projection (see \code{\link{laea_crs}}) to avoid areal distortion. A polygon
#' requires at least three non-collinear points; with fewer points the EOO is
#' returned as \code{NA} (as in GeoCat, where EOO is undefined / 0 for 1-2
#' points).
#'
#' @param points An \code{sf} of POINT geometries (one species). Use
#'   \code{\link{read_occurrences}} to produce it.
#' @return A list with \code{area_km2} (numeric), \code{n_records} (integer),
#'   \code{n_unique} (distinct coordinates), \code{hull} (the hull as an
#'   \code{sfc} in WGS84, or \code{NULL}) and \code{crs_laea} (the equal-area
#'   proj string used).
#' @examples
#' f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
#' occ <- read_occurrences(f)
#' sp1 <- occ[occ$species == occ$species[1], ]
#' calc_eoo(sp1)$area_km2
#' @export
calc_eoo <- function(points) {
  .assert_points(points, "points")
  pts <- sf::st_geometry(sf::st_transform(points, 4326))
  co <- unique(round(sf::st_coordinates(pts)[, c("X", "Y"), drop = FALSE], 8))
  n_rec <- length(pts)
  n_uni <- nrow(co)

  out <- list(area_km2 = NA_real_, n_records = n_rec, n_unique = n_uni,
              hull = NULL, crs_laea = NA_character_)
  if (n_uni < 3) {
    message("EOO undefined for < 3 unique locations (returning NA).")
    return(out)
  }

  crs_laea <- laea_crs(pts)
  pts_p <- sf::st_transform(pts, crs_laea)
  hull_p <- sf::st_convex_hull(sf::st_union(pts_p))
  area <- as.numeric(sf::st_area(hull_p)) / 1e6  # m^2 -> km^2

  out$area_km2 <- area
  out$hull <- sf::st_transform(hull_p, 4326)
  out$crs_laea <- crs_laea
  out
}

#' Area of Occupancy (AOO) via occupied grid cells
#'
#' Computes the AOO as the number of occupied grid cells multiplied by the cell
#' area, following IUCN Red List guidelines (reference scale: 2 x 2 km cells =
#' 4 km^2 each). Points are snapped to a regular grid defined on a data-centred
#' equal-area projection.
#'
#' Note: AOO depends slightly on grid placement; the IUCN reference scale is the
#' 2 km grid. To explore sensitivity you can change \code{cell_km} or shift the
#' grid via \code{origin}.
#'
#' @param points An \code{sf} of POINT geometries (one species).
#' @param cell_km Grid cell side length in kilometres (default \code{2}).
#' @param origin Optional length-2 numeric \code{c(x0, y0)} grid origin in the
#'   equal-area projection metres (default \code{c(0, 0)}).
#' @return A list with \code{area_km2}, \code{n_cells}, \code{cell_km},
#'   \code{cells} (occupied cells as an \code{sfc} polygon set in WGS84) and
#'   \code{crs_laea}.
#' @examples
#' f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
#' occ <- read_occurrences(f)
#' sp1 <- occ[occ$species == occ$species[1], ]
#' calc_aoo(sp1)$area_km2
#' @export
calc_aoo <- function(points, cell_km = 2, origin = c(0, 0)) {
  .assert_points(points, "points")
  pts <- sf::st_geometry(sf::st_transform(points, 4326))
  crs_laea <- laea_crs(pts)
  pts_p <- sf::st_transform(pts, crs_laea)

  size <- cell_km * 1000
  co <- sf::st_coordinates(pts_p)[, c("X", "Y"), drop = FALSE]
  ix <- floor((co[, "X"] - origin[1]) / size)
  iy <- floor((co[, "Y"] - origin[2]) / size)
  cells <- unique(data.frame(ix = ix, iy = iy))
  n_cells <- nrow(cells)

  # build occupied-cell polygons (for mapping / habitat extraction)
  polys <- vector("list", n_cells)
  for (i in seq_len(n_cells)) {
    x0 <- origin[1] + cells$ix[i] * size
    y0 <- origin[2] + cells$iy[i] * size
    polys[[i]] <- sf::st_polygon(list(rbind(
      c(x0, y0), c(x0 + size, y0), c(x0 + size, y0 + size),
      c(x0, y0 + size), c(x0, y0)
    )))
  }
  cells_p <- sf::st_sfc(polys, crs = crs_laea)

  list(
    area_km2 = n_cells * (cell_km^2),
    n_cells = n_cells,
    cell_km = cell_km,
    cells = sf::st_transform(cells_p, 4326),
    crs_laea = crs_laea
  )
}
