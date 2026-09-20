# -----------------------------------------------------------------------------
# Countries of occurrence and (approximate) biogeographical realms.
#
# The IUCN Red List requires a list of the countries of occurrence and
# recommends the biogeographical realm(s) as supporting information for an
# assessment. sRedList derives these by intersecting the range map with the
# country and realm base maps used by the Red List (Cazalis et al. 2024,
# Biological Conservation 298:110761). mappingAS assimilates the same idea, but
# entirely from data the package already produces (the EOO hull and the
# occurrence points) plus the public Natural Earth country base map, so no new
# heavy data source is introduced.
#
# The country base map is read from the 'rnaturalearth' package when it is
# installed (a Suggests dependency). When it is not, the function degrades
# gracefully to an empty result with a message rather than raising an error, so
# the rest of the assessment is never blocked.
# -----------------------------------------------------------------------------

#' Countries of occurrence and biogeographical realm(s) from a range
#'
#' Derives the list of countries a taxon occurs in - and an approximate
#' biogeographical realm - by intersecting its range with the public Natural
#' Earth country base map, mirroring the "countries of occurrence" and "realms"
#' supporting information that the IUCN Red List requires/recommends and that
#' sRedList extracts automatically (Cazalis \emph{et al.} 2024).
#'
#' Following the Red List convention, every country the range \emph{polygon}
#' overlaps is reported, and each is flagged by presence:
#' \itemize{
#'   \item \strong{Extant} - the country holds at least one occurrence record;
#'   \item \strong{Possibly Extant} - the country overlaps the range but holds
#'     no record in the supplied points.
#' }
#' When no polygon is supplied (fewer than three points, so no EOO hull) the
#' countries are taken from the points alone and are all reported as
#' \strong{Extant}.
#'
#' \strong{Realm.} The biogeographical realm is a \emph{coarse approximation}
#' derived from the continent of each country (Neotropical, Nearctic,
#' Afrotropical, Palearctic, Indomalayan, Australasian, Oceanian or Antarctic);
#' it is exact for the South-American ranges mappingAS is built around but should
#' be checked for taxa spanning continental transitions (e.g. North/Central
#' America, North Africa, South-East Asia), where realm boundaries do not follow
#' country borders.
#'
#' @param range An \code{sf}/\code{sfc} polygon delimiting the range (e.g. the
#'   EOO hull from \code{\link{calc_eoo}}), or an \code{sf} of occurrence points.
#'   Any CRS is accepted (it is transformed to WGS84 internally).
#' @param points Optional \code{sf} of occurrence POINT geometries used to set
#'   the \strong{Extant} / \strong{Possibly Extant} presence flag. \code{NULL}
#'   (default) means the presence is taken from \code{range} itself when it is a
#'   set of points, otherwise every overlapping country is \strong{Possibly
#'   Extant}.
#' @param scale Natural Earth scale passed to \code{rnaturalearth::ne_countries}
#'   (\code{"small"}, \code{"medium"} (default) or \code{"large"}).
#' @return A \code{data.frame} with one row per country: \code{country},
#'   \code{presence} (\code{"Extant"}/\code{"Possibly Extant"}),
#'   \code{n_records} (occurrences in that country), \code{continent} and
#'   \code{realm}. Rows are ordered by presence then country. An empty
#'   \code{data.frame} (with a message) is returned when \pkg{rnaturalearth} is
#'   not installed or no country overlaps.
#' @references
#' Cazalis V. \emph{et al.} (2024) Accelerating and standardising IUCN Red List
#'   assessments with sRedList. \emph{Biological Conservation} 298:110761.
#'   \doi{10.1016/j.biocon.2024.110761}
#' @seealso \code{\link{calc_eoo}}, \code{\link{assess_species}}
#' @examples
#' \donttest{
#' f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
#' occ <- read_occurrences(f)
#' sp1 <- occ[occ$species == occ$species[1], ]
#' # Needs the 'rnaturalearth' package (Suggests):
#' countries_of_occurrence(calc_eoo(sp1)$hull, points = sp1)
#' }
#' @export
countries_of_occurrence <- function(range, points = NULL,
                                    scale = c("medium", "small", "large")) {
  scale <- match.arg(scale)
  empty <- data.frame(country = character(0), presence = character(0),
                      n_records = integer(0), continent = character(0),
                      realm = character(0), stringsAsFactors = FALSE)
  if (!requireNamespace("rnaturalearth", quietly = TRUE) ||
      !requireNamespace("sf", quietly = TRUE)) {
    message("countries_of_occurrence(): the 'rnaturalearth' package is not ",
            "installed; install it to extract countries of occurrence.")
    return(empty)
  }

  # Range polygon (if any) and the points that flag presence.
  rng <- .as_range_polygon(range)
  pts <- .as_points_or_null(points)
  if (is.null(pts)) pts <- .as_points_or_null(range)  # range may itself be points

  load_world <- function(sc) tryCatch(
    sf::st_make_valid(rnaturalearth::ne_countries(scale = sc,
                                                  returnclass = "sf")),
    error = function(e) NULL)
  world <- load_world(scale)
  # scale = "medium"/"large" need the rnaturalearthdata/hires data packages;
  # fall back to the small (1:110m) map bundled in rnaturalearth itself.
  if ((is.null(world) || !nrow(world)) && scale != "small")
    world <- load_world("small")
  if (is.null(world) || !nrow(world)) {
    message("countries_of_occurrence(): could not load the Natural Earth ",
            "country base map.")
    return(empty)
  }
  name_col <- .first_present(names(world),
                             c("name_long", "admin", "name", "sovereignt"))
  cont_col <- .first_present(names(world), c("continent", "region_un"))
  if (is.null(name_col)) return(empty)

  # Planar (GEOS) overlays: the densified EOO hull can trip the S2 engine, and
  # GEOS is tolerant - the same reason assess_species() disables S2.
  old_s2 <- suppressMessages(sf::sf_use_s2())
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  suppressMessages(sf::sf_use_s2(FALSE))
  world <- sf::st_transform(world, 4326)

  # Which countries does the range (polygon, else points) touch?
  overlap_geom <- if (!is.null(rng)) rng else
    if (!is.null(pts)) sf::st_geometry(pts) else NULL
  if (is.null(overlap_geom)) return(empty)
  hit <- suppressMessages(sf::st_intersects(world, .st_union_quiet(overlap_geom)))
  touched <- lengths(hit) > 0
  if (!any(touched)) return(empty)
  sel <- world[touched, , drop = FALSE]

  # Records per country -> presence flag.
  n_rec <- rep(0L, nrow(sel))
  if (!is.null(pts)) {
    inp <- suppressMessages(sf::st_intersects(sf::st_geometry(pts),
                                              sf::st_geometry(sel)))
    tab <- table(unlist(inp))
    if (length(tab)) n_rec[as.integer(names(tab))] <- as.integer(tab)
  }
  # With no points at all, everything is "Possibly Extant"; with points, a
  # country holding >= 1 record is "Extant".
  presence <- ifelse(n_rec > 0L, "Extant", "Possibly Extant")

  cont <- if (!is.null(cont_col)) as.character(sel[[cont_col]]) else NA_character_
  out <- data.frame(
    country   = as.character(sel[[name_col]]),
    presence  = presence,
    n_records = n_rec,
    continent = cont,
    realm     = .continent_to_realm(cont),
    stringsAsFactors = FALSE)
  out <- out[order(out$presence != "Extant", out$country), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Coerce an input to a range POLYGON geometry (sfc), or NULL when it is not
#' polygonal (e.g. points, or fewer than three points so no hull).
#' @keywords internal
#' @noRd
.as_range_polygon <- function(x) {
  if (is.null(x)) return(NULL)
  g <- tryCatch(sf::st_geometry(x), error = function(e) NULL)
  if (is.null(g) || !length(g)) return(NULL)
  ty <- as.character(sf::st_geometry_type(g))
  if (!any(ty %in% c("POLYGON", "MULTIPOLYGON"))) return(NULL)
  g <- g[ty %in% c("POLYGON", "MULTIPOLYGON")]
  if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
  sf::st_transform(g, 4326)
}

#' Coerce an input to POINT geometries as an sf (with a geometry column), or
#' NULL when it holds no points.
#' @keywords internal
#' @noRd
.as_points_or_null <- function(x) {
  if (is.null(x)) return(NULL)
  g <- tryCatch(sf::st_geometry(x), error = function(e) NULL)
  if (is.null(g) || !length(g)) return(NULL)
  ty <- as.character(sf::st_geometry_type(g))
  keep <- ty %in% c("POINT", "MULTIPOINT")
  if (!any(keep)) return(NULL)
  g <- g[keep]
  if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
  sf::st_sf(geometry = sf::st_transform(g, 4326))
}

#' First name in \code{candidates} that appears in \code{have}, or NULL.
#' @keywords internal
#' @noRd
.first_present <- function(have, candidates) {
  m <- candidates[candidates %in% have]
  if (length(m)) m[1] else NULL
}

#' Coarse continent -> biogeographical realm lookup (see the note in
#' \code{countries_of_occurrence}). Exact for South America; approximate at
#' continental transitions.
#' @keywords internal
#' @noRd
.continent_to_realm <- function(continent) {
  map <- c(
    "South America" = "Neotropical",
    "North America" = "Nearctic",
    "Africa"        = "Afrotropical",
    "Europe"        = "Palearctic",
    "Asia"          = "Palearctic",
    "Oceania"       = "Australasian",
    "Antarctica"    = "Antarctic",
    "Seven seas (open ocean)" = NA_character_)
  out <- unname(map[as.character(continent)])
  out[is.na(out) & !is.na(continent)] <- NA_character_
  out
}
