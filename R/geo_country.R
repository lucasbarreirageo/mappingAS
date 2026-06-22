## R/geo_country.R — assign each occurrence point to a MapBiomas country
## ---------------------------------------------------------------------------
## Used by assess_species() to decide, per species, which national raster(s) to
## read. Strategy: a fast bounding-box prefilter narrows the candidate countries
## (no I/O); when an embedded borders layer is available it then runs a proper
## point-in-polygon test for the boundary cases. Everything is offline.

#' @keywords internal
#' @noRd
## Approximate mainland bounding boxes (xmin, ymin, xmax, ymax) in WGS84.
## Generous on purpose: they are only a prefilter, not the final answer.
.country_bbox <- function() list(
  brazil    = c(-74.10, -33.80, -34.70,  5.30),
  argentina = c(-73.60, -55.10, -53.60, -21.70),
  bolivia   = c(-69.70, -22.95, -57.40,  -9.65),
  ecuador   = c(-81.10,  -5.05, -75.15,   1.50),
  peru      = c(-81.40, -18.40, -68.65,  -0.01)
)

#' @keywords internal
#' @noRd
## Locate an optional borders GeoPackage shipped under inst/extdata/.
.country_borders_path <- function() {
  p <- system.file("extdata", "sa_borders.gpkg", package = "mappingAS")
  if (nzchar(p) && file.exists(p)) p else NA_character_
}

#' @keywords internal
#' @noRd
## Lazy, cached read of the borders layer (only the 5 supported countries).
.country_borders <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    path <- .country_borders_path()
    if (is.na(path)) { cache <<- NA; return(NA) }
    b <- tryCatch(sf::st_read(path, quiet = TRUE), error = function(e) NA)
    cache <<- b
    b
  }
})

#' @keywords internal
#' @noRd
.bbox_hits <- function(lon, lat) {
  bx <- .country_bbox()
  hit <- vapply(names(bx), function(k) {
    b <- bx[[k]]
    isTRUE(lon >= b[1] && lon <= b[3] && lat >= b[2] && lat <= b[4])
  }, logical(1))
  names(bx)[hit]
}

#' Identify the MapBiomas country of each occurrence point
#'
#' Returns the country key for every point. When the embedded borders layer is
#' present, ambiguous points (inside more than one bounding box, i.e. near a
#' frontier) are resolved by a point-in-polygon test; otherwise the first
#' bounding-box match is used (with a single message).
#'
#' @param points An \code{sf} of POINT geometries (WGS84 or any CRS).
#' @return A character vector (length = number of points) of country keys; points
#'   outside all supported countries are \code{NA}.
#' @examples
#' \dontrun{
#' occ <- read_occurrences("occ.csv")
#' table(country_of_points(occ))
#' }
#' @export
country_of_points <- function(points) {
  .assert_points(points, "points")
  pts <- sf::st_transform(sf::st_geometry(points), 4326)
  co <- sf::st_coordinates(pts)[, c("X", "Y"), drop = FALSE]

  borders <- .country_borders()
  have_poly <- inherits(borders, "sf") || inherits(borders, "sfc")
  warned_bbox <- FALSE

  out <- character(nrow(co))
  for (i in seq_len(nrow(co))) {
    cand <- .bbox_hits(co[i, 1], co[i, 2])
    if (length(cand) == 0) { out[i] <- NA_character_; next }
    if (length(cand) == 1) { out[i] <- cand; next }

    if (have_poly) {
      out[i] <- .resolve_by_polygon(co[i, 1], co[i, 2], borders, cand)
    } else {
      if (!warned_bbox) {
        message("Borders layer not installed; frontier points resolved by ",
                "bounding box. Install inst/extdata/sa_borders.gpkg for exact ",
                "country assignment.")
        warned_bbox <- TRUE
      }
      out[i] <- cand[1]
    }
  }
  out
}

#' @keywords internal
#' @noRd
.resolve_by_polygon <- function(lon, lat, borders, cand) {
  pt <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
  key_col <- intersect(c("country", "name", "NAME", "admin", "ADMIN"),
                       names(borders))[1]
  sub <- if (!is.na(key_col)) {
    keys <- vapply(borders[[key_col]], .mb_country_aliases, character(1))
    borders[keys %in% cand, , drop = FALSE]
  } else borders
  if (!nrow(sub)) return(cand[1])
  hit <- suppressMessages(sf::st_within(pt, sf::st_geometry(sub), sparse = FALSE))
  if (any(hit)) {
    j <- which(hit)[1]
    if (!is.na(key_col)) .mb_country_aliases(sub[[key_col]][j]) else cand[1]
  } else cand[1]
}

#' Countries spanned by a species' occurrences
#'
#' @param points An \code{sf} of POINT geometries for ONE species.
#' @return A character vector of distinct country keys (\code{NA} dropped),
#'   ordered by number of points (most first).
#' @examples
#' \dontrun{
#' sp1 <- occ[occ$species == "Example atlantica", ]
#' species_countries(sp1)
#' }
#' @export
species_countries <- function(points) {
  ct <- country_of_points(points)
  ct <- ct[!is.na(ct)]
  if (!length(ct)) return(character(0))
  names(sort(table(ct), decreasing = TRUE))
}
