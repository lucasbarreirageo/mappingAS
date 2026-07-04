#' Build a data-centred equal-area CRS (Lambert Azimuthal Equal Area)
#'
#' EOO and AOO must be measured on an equal-area projection. This helper returns
#' a LAEA proj string centred on the centroid of \code{geom}, which minimises
#' areal distortion for the spatial extent of a single species.
#'
#' @param geom An \code{sf} or \code{sfc} object (any CRS; will be treated as
#'   WGS84 longitude/latitude for centroid computation).
#' @return A character proj4 string suitable for \code{sf::st_transform()}.
#' @keywords internal
#' @export
laea_crs <- function(geom) {
  g <- sf::st_geometry(geom)
  if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
  g <- sf::st_transform(g, 4326)
  # mean coordinate of all vertices/points as the projection centre
  co <- sf::st_coordinates(g)
  lon0 <- mean(co[, "X"], na.rm = TRUE)
  lat0 <- mean(co[, "Y"], na.rm = TRUE)
  sprintf(
    "+proj=laea +lat_0=%.6f +lon_0=%.6f +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
    lat0, lon0
  )
}

#' Case-insensitive lookup of the first matching column name
#' @keywords internal
#' @noRd
.match_col <- function(nms, candidates) {
  nms_l <- tolower(trimws(nms))
  for (cand in candidates) {
    hit <- which(nms_l == tolower(cand))
    if (length(hit)) return(nms[hit[1]])
  }
  NA_character_
}

#' Default candidate column names for occurrence imports
#' @keywords internal
#' @noRd
.coord_candidates <- function() {
  list(
    lon = c("decimalLongitude", "decimal_longitude", "longitude", "long",
            "lon", "lng", "x", "coord_x", "x_coord", "xcoord", "easting"),
    lat = c("decimalLatitude", "decimal_latitude", "latitude", "lat",
            "y", "coord_y", "y_coord", "ycoord", "northing"),
    species = c("species", "scientificName", "scientific_name", "taxon",
                "taxon_name", "binomial", "sp", "especie", "nome_cientifico",
                "name")
  )
}

#' Validate that an object is a point sf in WGS84
#' @keywords internal
#' @noRd
.assert_points <- function(x, arg = "occ") {
  if (!inherits(x, "sf")) {
    stop(sprintf("`%s` must be an sf object (see read_occurrences()).", arg),
         call. = FALSE)
  }
  gt <- as.character(unique(sf::st_geometry_type(x)))
  if (!all(gt %in% c("POINT", "MULTIPOINT"))) {
    stop(sprintf("`%s` must contain POINT geometries, not: %s.",
                 arg, paste(gt, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Union geometries silencing the planar-assumption warning
#' @keywords internal
#' @noRd
.st_union_quiet <- function(x) {
  suppressWarnings(suppressMessages(sf::st_union(x)))
}

#' Map a provisional Criterion B category string to its IUCN colour + code
#' @keywords internal
#' @noRd
.iucn_badge <- function(cat) {
  # cat is like "CR (B1 size)", "EN (B2 size)", "VU ...", "not VU/EN/CR ..." or NA
  code <- if (is.null(cat) || is.na(cat)) "NA"
          else if (grepl("^CR", cat)) "CR"
          else if (grepl("^EN", cat)) "EN"
          else if (grepl("^VU", cat)) "VU"
          else if (grepl("^NT", cat)) "NT"
          else "LC"   # "not VU/EN/CR by size" -> least concern (screening)
  pal <- c(CR = "#d81e05", EN = "#fc7f3f", VU = "#f9e814",
           NT = "#cce226", LC = "#60c659", `NA` = "#bdbdbd")
  fg  <- c(CR = "#ffffff", EN = "#ffffff", VU = "#000000",
           NT = "#000000", LC = "#ffffff", `NA` = "#000000")
  list(code = code, bg = unname(pal[code]), fg = unname(fg[code]))
}


#' Provisional IUCN Red List Criterion B category from range size
#'
#' Returns the category implied by the EOO (B1) and AOO (B2) \emph{size}
#' thresholds only. This is a screening aid, exactly like GeoCat: a full
#' Criterion B listing additionally requires at least two of the sub-criteria
#' (a) severe fragmentation / few locations, (b) continuing decline, and
#' (c) extreme fluctuation. Never report these as final categories.
#'
#' @param eoo_km2,aoo_km2 Numeric range sizes in square kilometres.
#' @return A list with \code{eoo_category}, \code{aoo_category} and
#'   \code{combined} (the more threatened of the two size-based flags).
#' @export
iucn_category_B <- function(eoo_km2 = NA_real_, aoo_km2 = NA_real_) {
  cat_eoo <- if (is.na(eoo_km2)) NA_character_
             else if (eoo_km2 < 100)    "CR (B1 size)"
             else if (eoo_km2 < 5000)   "EN (B1 size)"
             else if (eoo_km2 < 20000)  "VU (B1 size)"
             else "not VU/EN/CR by B1 size"

  cat_aoo <- if (is.na(aoo_km2)) NA_character_
             else if (aoo_km2 < 10)     "CR (B2 size)"
             else if (aoo_km2 < 500)    "EN (B2 size)"
             else if (aoo_km2 < 2000)   "VU (B2 size)"
             else "not VU/EN/CR by B2 size"

  rank <- function(x) {
    if (is.na(x)) return(NA_integer_)
    if (grepl("^CR", x)) 3L else if (grepl("^EN", x)) 2L else
      if (grepl("^VU", x)) 1L else 0L
  }
  rk <- c(rank(cat_eoo), rank(cat_aoo))
  combined <- if (all(is.na(rk))) NA_character_ else {
    best <- which.max(rk)
    c(cat_eoo, cat_aoo)[best]
  }
  list(eoo_category = cat_eoo, aoo_category = cat_aoo, combined = combined)
}

#' Conservation-group labels and colours (single source of truth)
#'
#' Central lookup for the four conservation groups used across the plotting
#' functions, so the same group is never labelled differently in different
#' places (e.g. "Altered" vs "Altered (anthropic)").
#'
#' @param lang \code{"en"} (default) or \code{"pt"}.
#' @return A list with \code{labels} and \code{hex}, both named character
#'   vectors keyed by internal group name.
#' @keywords internal
#' @noRd
.mb_group_labels <- function(lang = c("en", "pt")) {
  lang <- match.arg(lang)
  labels <- if (lang == "en")
    c(natural = "Natural", anthropic = "Altered", water = "Water",
      other = "Other", not_observed = "Not observed")
  else
    c(natural = "Natural", anthropic = "Alterado", water = "Agua",
      other = "Outros", not_observed = "Nao observado")
  hex <- c(natural = "#1f8d49", anthropic = "#d4271e", water = "#2532e4",
           other = "#bdbdbd", not_observed = "#cccccc")
  list(labels = labels, hex = hex)
}