# -----------------------------------------------------------------------------
# Severe fragmentation of the population (Criterion B sub-criterion a).
#
# The Red List guidelines consider a population "severely fragmented" when most
# of its individuals (>= 50 %) are in small and relatively isolated
# subpopulations that may not be viable. sRedList operationalises this (Santini
# et al. 2019; Cazalis et al. 2024) by clustering suitable-habitat patches that
# are within a dispersal / isolation distance of each other, estimating each
# cluster's population, and plotting the proportion of the population that falls
# in "small" subpopulations against the size the assessor considers small.
#
# assess_fragmentation() assimilates that guidance using only data mappingAS
# already has - the occurrence points and a user-supplied isolation distance.
# Subpopulations are the clusters of occurrences no more than the isolation
# distance apart (a circular-buffer clustering, the same family of method as
# calc_subpop()); each cluster's relative size is a weight (the number of
# occurrences by default, or a user weight such as occupied area). The output is
# the sRedList-style fragmentation curve plus the "median subpopulation size",
# which - together with the assessor's definition of a "small" subpopulation -
# guides whether sub-criterion (a) is met. It does NOT flag severe fragmentation
# automatically: that judgement stays with the assessor, as in sRedList.
# -----------------------------------------------------------------------------

#' Assess severe fragmentation from occurrences and an isolation distance
#'
#' Clusters occurrences into subpopulations - groups of occurrences no further
#' apart than an isolation distance - and quantifies how much of the population
#' lies in small subpopulations, to guide the "severely fragmented" judgement of
#' IUCN Criterion B sub-criterion (a). This mirrors the fragmentation step of
#' sRedList (Cazalis \emph{et al.} 2024, after Santini \emph{et al.} 2019) but
#' from occurrence data alone, so no habitat model or density estimate is
#' required.
#'
#' Two occurrences are placed in the same subpopulation when they are within
#' \code{isolation_km} of each other (a circular-buffer clustering with radius
#' \code{isolation_km / 2}, unioned on a data-centred equal-area projection, as
#' in \code{\link{calc_subpop}}). The relative \emph{size} of each subpopulation
#' is the sum of its occurrence \code{weights} (the number of occurrences by
#' default; supply, for example, per-occurrence occupied area for an
#' area-weighted size). The fragmentation \code{curve} then gives, for a range
#' of candidate "small subpopulation" sizes, the proportion of the total
#' population contained in subpopulations no larger than that size; the
#' \code{median_subpop_size} is the size at which that proportion reaches one
#' half (half the population lives in subpopulations smaller than this). If the
#' assessor considers a subpopulation of \code{median_subpop_size} to be
#' "small", the population can be considered severely fragmented.
#'
#' \strong{This is a screening proxy}: occurrence counts are sampling-biased and
#' are not individuals. Use the result only when the mapped clusters make
#' ecological sense as subpopulations, exactly as sRedList advises.
#'
#' @param points An \code{sf} of POINT geometries (one species). Use
#'   \code{\link{read_occurrences}} to produce it.
#' @param isolation_km Isolation distance in km: occurrences further apart than
#'   this are treated as belonging to isolated subpopulations. Required.
#' @param weights Optional numeric vector, one weight per occurrence in
#'   \code{points} (a relative subpopulation-size contribution, e.g. occupied
#'   area). \code{NULL} (default) weights every occurrence equally (1).
#' @param small_size Optional single size the assessor considers "small". When
#'   supplied, \code{prop_in_small} (share of the population in subpopulations
#'   \eqn{\le} this size) and \code{severe_suggested} (\code{TRUE} when that
#'   share \eqn{\ge} 0.5) are returned. \code{NULL} (default) leaves the
#'   judgement to the curve.
#' @param n_thresholds Number of points on the fragmentation \code{curve}
#'   (default \code{50}).
#' @return A list with \code{n_subpop}, \code{isolation_km}, \code{sizes} (a
#'   \code{data.frame} of \code{cluster}, \code{size}, \code{n_points}, ordered
#'   largest first), \code{clusters} (the subpopulation hulls/buffers as an
#'   \code{sfc} in WGS84), \code{curve} (a \code{data.frame} of \code{size} and
#'   \code{prop_pop_le}), \code{median_subpop_size}, \code{largest_subpop_pct}
#'   (percent of the population in the single largest subpopulation),
#'   \code{prop_in_small} / \code{severe_suggested} (when \code{small_size} is
#'   given, else \code{NA}) and \code{crs_laea}.
#' @references
#' Santini L. \emph{et al.} (2019) Applying habitat and population-density models
#'   to land-cover time series to inform IUCN Red List assessments.
#'   \emph{Conservation Biology} 33:1084-1093. \doi{10.1111/cobi.13279}
#'
#' Cazalis V. \emph{et al.} (2024) Accelerating and standardising IUCN Red List
#'   assessments with sRedList. \emph{Biological Conservation} 298:110761.
#'   \doi{10.1016/j.biocon.2024.110761}
#' @seealso \code{\link{calc_subpop}}, \code{\link{iucn_criterion_B}}
#' @examples
#' f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
#' occ <- read_occurrences(f)
#' sp1 <- occ[occ$species == occ$species[1], ]
#' fr <- assess_fragmentation(sp1, isolation_km = 20)
#' fr$n_subpop
#' fr$median_subpop_size
#' @export
assess_fragmentation <- function(points, isolation_km, weights = NULL,
                                 small_size = NULL, n_thresholds = 50) {
  .assert_points(points, "points")
  isolation_km <- suppressWarnings(as.numeric(isolation_km))[1]
  if (!is.finite(isolation_km) || isolation_km <= 0)
    stop("`isolation_km` must be a single positive number.", call. = FALSE)

  pts_all <- sf::st_geometry(sf::st_transform(points, 4326))
  co_all <- sf::st_coordinates(pts_all)[, c("X", "Y"), drop = FALSE]
  n_all <- nrow(co_all)

  # Per-occurrence weights (relative subpopulation-size contribution).
  w_all <- if (is.null(weights)) rep(1, n_all) else {
    w <- suppressWarnings(as.numeric(weights))
    if (length(w) != n_all)
      stop("`weights` must have one value per occurrence in `points`.",
           call. = FALSE)
    w[!is.finite(w)] <- 0
    w
  }

  crs_laea <- tryCatch(laea_crs(pts_all), error = function(e) NA_character_)
  out <- list(n_subpop = NA_integer_, isolation_km = isolation_km,
              sizes = data.frame(cluster = integer(0), size = numeric(0),
                                 n_points = integer(0)),
              clusters = NULL,
              curve = data.frame(size = numeric(0), prop_pop_le = numeric(0)),
              median_subpop_size = NA_real_, largest_subpop_pct = NA_real_,
              prop_in_small = NA_real_, severe_suggested = NA, crs_laea = crs_laea)
  if (n_all < 1L) return(out)
  if (is.na(crs_laea)) return(out)

  # Cluster occurrences within `isolation_km` of each other: buffer by half the
  # isolation distance on an equal-area CRS, dissolve, and split into disjoint
  # subpopulation polygons (same machinery as calc_subpop()).
  pts_p <- sf::st_transform(pts_all, crs_laea)
  buf <- sf::st_buffer(pts_p, isolation_km * 1000 / 2)
  merged <- .st_union_quiet(buf)
  clusters_p <- suppressWarnings(sf::st_cast(merged, "POLYGON"))
  n_sub <- length(sf::st_geometry(clusters_p))

  # Assign each occurrence to its subpopulation and sum weights per cluster.
  hit <- suppressMessages(sf::st_intersects(pts_p, clusters_p))
  cl <- vapply(hit, function(i) if (length(i)) i[1] else NA_integer_, integer(1))
  size <- tapply(w_all, cl, sum)
  npt  <- tapply(rep(1L, n_all), cl, sum)
  ids  <- as.integer(names(size))
  sizes <- data.frame(cluster = ids,
                      size = as.numeric(size),
                      n_points = as.integer(npt[as.character(ids)]),
                      stringsAsFactors = FALSE)
  sizes <- sizes[order(-sizes$size), , drop = FALSE]
  rownames(sizes) <- NULL

  total <- sum(sizes$size)
  out$n_subpop <- as.integer(n_sub)
  out$sizes <- sizes
  out$clusters <- sf::st_transform(clusters_p, 4326)
  out$largest_subpop_pct <- if (total > 0) 100 * max(sizes$size) / total else NA_real_

  # Fragmentation curve: proportion of the population in subpopulations no
  # larger than each candidate "small" size (the sRedList severe-fragmentation
  # plot). The median subpopulation size is where the curve reaches 0.5.
  out$curve <- .fragmentation_curve(sizes$size, n_thresholds)
  out$median_subpop_size <- .weighted_median_subpop(sizes$size)

  if (!is.null(small_size)) {
    ss <- suppressWarnings(as.numeric(small_size))[1]
    if (is.finite(ss) && total > 0) {
      prop <- sum(sizes$size[sizes$size <= ss]) / total
      out$prop_in_small <- prop
      out$severe_suggested <- isTRUE(prop >= 0.5)
    }
  }
  out
}

#' Proportion of the population in subpopulations no larger than each of
#' \code{n} candidate sizes spanning the observed subpopulation sizes.
#' @keywords internal
#' @noRd
.fragmentation_curve <- function(sizes, n = 50) {
  sizes <- sizes[is.finite(sizes) & sizes > 0]
  total <- sum(sizes)
  if (!length(sizes) || total <= 0)
    return(data.frame(size = numeric(0), prop_pop_le = numeric(0)))
  grid <- seq(min(sizes), max(sizes), length.out = max(2L, as.integer(n)))
  prop <- vapply(grid, function(t) sum(sizes[sizes <= t]) / total, numeric(1))
  data.frame(size = grid, prop_pop_le = prop)
}

#' The subpopulation size at which half the population lives in subpopulations
#' smaller than it (i.e. the population-weighted median subpopulation size): sort
#' subpopulations ascending, accumulate their share, and take the size where the
#' cumulative share first reaches one half.
#' @keywords internal
#' @noRd
.weighted_median_subpop <- function(sizes) {
  sizes <- sizes[is.finite(sizes) & sizes > 0]
  total <- sum(sizes)
  if (!length(sizes) || total <= 0) return(NA_real_)
  s <- sort(sizes)
  cum <- cumsum(s) / total
  s[which(cum >= 0.5)[1]]
}

#' Severe fragmentation from suitable-habitat patches and population density
#'
#' Implements the habitat-and-density fragmentation method of the IUCN Red List
#' guidelines (Santini \emph{et al.} 2019), as used by sRedList (Cazalis
#' \emph{et al.} 2024): the suitable habitat (Area of Habitat) within the range
#' is split into \strong{patches}, patches within a \strong{dispersal / isolation
#' distance} of one another are grouped into isolated \strong{subpopulations},
#' and each subpopulation's size in \strong{individuals} is its habitat area
#' times the population \strong{density}. This is the guideline-compliant
#' companion to \code{\link{assess_fragmentation}} (which uses occurrence counts
#' as a rough proxy when no habitat model or density is available).
#'
#' Following Santini \emph{et al.} (2019): contiguous suitable cells form
#' patches; each patch is buffered by half the isolation distance and buffers
#' that touch are merged, so patches closer than the isolation distance share a
#' subpopulation; the number of individuals in a subpopulation is its total
#' suitable area (km^2) times \code{density} (individuals km^-2), and a
#' subpopulation of fewer than two individuals is treated as unoccupied. The
#' \code{curve} then gives the proportion of the population in subpopulations no
#' larger than a given size, whose \code{median_subpop_size} is the size below
#' which half the population lives; if the assessor considers a subpopulation of
#' that size "small", the population can be treated as severely fragmented
#' (Criterion B sub-criterion a). The largest subpopulation's size
#' (\code{max_subpop}, Criterion C2a(i)) and its share of the population
#' (\code{pct_largest}, Criterion C2a(ii)) are also returned.
#'
#' \strong{Use with care}: use the result only when the mapped clusters make
#' ecological sense as subpopulations, exactly as sRedList advises. Barriers
#' (water, mountains, infrastructure) are not accounted for.
#'
#' @param range An \code{sf}/\code{sfc} range \strong{polygon} (e.g. the EOO hull).
#' @param isolation_km Isolation / dispersal distance in km: patches further
#'   apart than this host isolated subpopulations. Required.
#' @param density Population density in mature individuals per km^2 of suitable
#'   habitat. A length-2 vector gives a low/high range (a second population
#'   column \code{pop_high} is added). Required.
#' @param year,collection,initiative,src Land-cover selection, as in
#'   \code{\link{calc_aoh}} / \code{\link{mb_raster_local}}.
#' @param suitable_groups,suitable_codes Which land-cover classes count as
#'   suitable habitat (default: the \code{"natural"} group). See
#'   \code{\link{calc_aoh}}.
#' @param elev_min,elev_max,z,elev_src Optional elevation band (metres) and DEM
#'   source, as in \code{\link{calc_aoh}}.
#' @param max_pixels Display pixel budget for the habitat mask read (default
#'   \code{4000}); larger ranges are read coarser, as sRedList aggregates them.
#' @param small_size Optional size (individuals) the assessor considers "small";
#'   when given, \code{prop_in_small} and \code{severe_suggested} are returned.
#' @return A list with \code{method = "habitat"}, \code{n_subpop} (occupied
#'   subpopulations), \code{isolation_km}, \code{density}, \code{sizes} (a
#'   \code{data.frame} of \code{cluster}, \code{aoh_km2}, \code{pop} and, for a
#'   density range, \code{pop_high}), \code{clusters} (subpopulation polygons as
#'   an \code{sfc} in WGS84), \code{curve}, \code{median_subpop_size},
#'   \code{max_subpop} (C2a(i)), \code{pct_largest} (C2a(ii)), \code{total_pop},
#'   and \code{prop_in_small} / \code{severe_suggested}. Values degrade to
#'   \code{NA} when the land cover cannot be read.
#' @references
#' Santini L. \emph{et al.} (2019) Applying habitat and population-density models
#'   to land-cover time series to inform IUCN Red List assessments.
#'   \emph{Conservation Biology} 33:1084-1093. \doi{10.1111/cobi.13279}
#'
#' Cazalis V. \emph{et al.} (2024) Accelerating and standardising IUCN Red List
#'   assessments with sRedList. \emph{Biological Conservation} 298:110761.
#'   \doi{10.1016/j.biocon.2024.110761}
#' @seealso \code{\link{assess_fragmentation}}, \code{\link{calc_aoh}}
#' @examples
#' \donttest{
#' f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
#' occ <- read_occurrences(f)
#' sp1 <- occ[occ$species == occ$species[1], ]
#' hull <- calc_eoo(sp1)$hull
#' # Reads land cover; density in mature individuals per km^2:
#' fr <- fragment_habitat(hull, isolation_km = 20, density = 5)
#' fr$n_subpop
#' fr$max_subpop      # C2a(i)
#' }
#' @export
fragment_habitat <- function(range, isolation_km, density,
                             year = NULL, collection = NULL,
                             initiative = "brazil", src = NULL,
                             suitable_groups = "natural", suitable_codes = NULL,
                             elev_min = NA, elev_max = NA, z = 9, elev_src = NULL,
                             max_pixels = 4000, small_size = NULL) {
  iso <- suppressWarnings(as.numeric(isolation_km))[1]
  if (!is.finite(iso) || iso <= 0)
    stop("`isolation_km` must be a single positive number.", call. = FALSE)
  dens <- suppressWarnings(as.numeric(density))
  dens <- dens[is.finite(dens) & dens > 0]
  if (!length(dens))
    stop("`density` (individuals per km^2) is required and must be positive.",
         call. = FALSE)

  out <- list(method = "habitat", n_subpop = NA_integer_, isolation_km = iso,
              density = dens,
              sizes = data.frame(cluster = integer(0), aoh_km2 = numeric(0),
                                 pop = numeric(0)),
              clusters = NULL,
              curve = data.frame(size = numeric(0), prop_pop_le = numeric(0)),
              median_subpop_size = NA_real_, max_subpop = NA_real_,
              pct_largest = NA_real_, total_pop = NA_real_,
              prop_in_small = NA_real_, severe_suggested = NA)
  if (!requireNamespace("terra", quietly = TRUE) ||
      !requireNamespace("sf", quietly = TRUE)) return(out)

  mask <- .suitable_mask_raster(range, year = year, collection = collection,
                                initiative = initiative, src = src,
                                suitable_groups = suitable_groups,
                                suitable_codes = suitable_codes,
                                elev_min = elev_min, elev_max = elev_max,
                                z = z, elev_src = elev_src,
                                max_pixels = max_pixels)
  if (is.null(mask)) {
    message("fragment_habitat(): suitable-habitat raster unavailable; NA.")
    return(out)
  }

  g <- .as_range_polygon(range)
  crs_laea <- tryCatch(laea_crs(sf::st_geometry(g)),
                       error = function(e) NA_character_)

  # Contiguous suitable cells -> individual habitat patches (polygons).
  patches <- tryCatch({
    # Defaults dissolve by value and drop NA, so this yields one (multi)polygon
    # of the suitable cells; st_cast then splits it into contiguous patches.
    pv <- terra::as.polygons(mask)
    ps <- sf::st_make_valid(sf::st_as_sf(pv))
    ps <- suppressWarnings(sf::st_cast(sf::st_geometry(ps), "POLYGON"))
    if (!is.na(crs_laea)) ps <- sf::st_transform(ps, crs_laea)
    ps
  }, error = function(e) NULL)
  if (is.null(patches) || !length(patches)) return(out)

  old_s2 <- suppressMessages(sf::sf_use_s2())
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  suppressMessages(sf::sf_use_s2(FALSE))

  area_km2 <- suppressWarnings(as.numeric(sf::st_area(patches)) / 1e6)
  keep <- is.finite(area_km2) & area_km2 > 0
  patches <- patches[keep]; area_km2 <- area_km2[keep]
  if (!length(patches)) return(out)

  # Buffer each patch by half the isolation distance and merge touching buffers:
  # patches within the isolation distance share a subpopulation (Santini 2019).
  buf <- sf::st_buffer(patches, iso * 1000 / 2)
  u <- suppressWarnings(sf::st_cast(.st_union_quiet(buf), "POLYGON"))
  hit <- suppressMessages(sf::st_intersects(buf, u))
  clid <- vapply(hit, function(i) if (length(i)) i[1] else NA_integer_, integer(1))

  ok <- !is.na(clid)
  if (!any(ok)) return(out)
  aoh_by <- tapply(area_km2[ok], clid[ok], sum)
  ids <- as.integer(names(aoh_by))
  aoh_sum <- as.numeric(aoh_by)

  pop <- aoh_sum * dens[1]; pop[pop < 2] <- 0
  sizes <- data.frame(cluster = ids, aoh_km2 = aoh_sum, pop = pop,
                      stringsAsFactors = FALSE)
  if (length(dens) > 1) {
    pop2 <- aoh_sum * dens[2]; pop2[pop2 < 2] <- 0
    sizes$pop_high <- pop2
  }
  sizes <- sizes[order(-sizes$pop), , drop = FALSE]
  rownames(sizes) <- NULL

  # Subpopulation polygons (union of each cluster's buffers) for mapping.
  cl_geom <- tryCatch({
    parts <- lapply(ids, function(k)
      .st_union_quiet(buf[which(clid == k)]))
    sf::st_transform(do.call(c, parts), 4326)
  }, error = function(e) NULL)

  total <- sum(sizes$pop)
  out$sizes <- sizes
  out$clusters <- cl_geom
  out$total_pop <- total
  out$n_subpop <- sum(sizes$pop > 0)
  out$max_subpop <- if (total > 0) max(sizes$pop) else NA_real_        # C2a(i)
  out$pct_largest <- if (total > 0) 100 * max(sizes$pop) / total else NA_real_ # C2a(ii)
  out$curve <- .fragmentation_curve(sizes$pop, 50)
  out$median_subpop_size <- .weighted_median_subpop(sizes$pop)
  if (!is.null(small_size)) {
    ss <- suppressWarnings(as.numeric(small_size))[1]
    if (is.finite(ss) && total > 0) {
      prop <- sum(sizes$pop[sizes$pop <= ss]) / total
      out$prop_in_small <- prop
      out$severe_suggested <- isTRUE(prop >= 0.5)
    }
  }
  out
}
