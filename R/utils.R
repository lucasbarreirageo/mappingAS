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
  out <- tryCatch(
    suppressWarnings(suppressMessages(sf::st_union(x))),
    error = function(e) NULL)
  if (!is.null(out)) return(out)
  # The S2 engine can reject a degenerate edge (duplicate vertex) when unioning
  # large or densely-vertexed geographic geometries. Retry off-S2 (planar GEOS),
  # making the inputs valid first, so a wide-ranging species does not abort the
  # whole assessment.
  old <- suppressMessages(sf::sf_use_s2())
  on.exit(suppressMessages(sf::sf_use_s2(old)), add = TRUE)
  suppressMessages(sf::sf_use_s2(FALSE))
  x2 <- tryCatch(sf::st_make_valid(x), error = function(e) x)
  suppressWarnings(suppressMessages(sf::st_union(x2)))
}

#' Geometry a raster is clipped to for a given `clip` choice.
#'
#' `"eoo"` uses the EOO hull, `"aoo"` the union of occupied AOO cells and
#' `"all"` the union of both (so the raster covers the whole range). Falls back
#' to whichever geometry is available when the preferred one is missing.
#' @keywords internal
#' @noRd
.clip_geometry <- function(clip, hull, cells) {
  has_cells <- !is.null(cells) && length(sf::st_geometry(cells)) > 0
  has_hull  <- !is.null(hull)  && length(sf::st_geometry(hull))  > 0
  if (identical(clip, "aoo"))
    return(if (has_cells) .st_union_quiet(cells) else hull)
  if (identical(clip, "all")) {
    parts <- list()
    if (has_hull)  parts <- c(parts, list(sf::st_geometry(hull)))
    if (has_cells) parts <- c(parts, list(sf::st_geometry(cells)))
    if (!length(parts)) return(hull)
    return(.st_union_quiet(do.call(c, parts)))
  }
  hull  # "eoo" (default)
}

#' Split a scientific name into its italic (genus + specific epithet) and its
#' non-italic remainder (naming authority, infraspecific author, etc.).
#'
#' Follows the botanical/zoological convention: only the genus and the specific
#' epithet (the first two words) are italicised; any word that comes after
#' (typically the describing author) is set in normal type.
#' @keywords internal
#' @noRd
.sp_parts <- function(name) {
  name <- if (is.null(name)) "" else trimws(as.character(name)[1])
  if (is.na(name) || !nzchar(name)) return(list(italic = "", rest = ""))
  w <- strsplit(name, "\\s+")[[1]]
  list(italic = paste(utils::head(w, 2), collapse = " "),
       rest   = if (length(w) > 2) paste(w[-(1:2)], collapse = " ") else "")
}

#' HTML rendering of a scientific name: `<i>Genus species</i> Author`.
#' Used in every HTML context (leaflet labels, Shiny UI, plotly titles, the
#' HTML report).
#' @keywords internal
#' @noRd
.sp_html <- function(name) {
  p <- .sp_parts(name)
  if (!nzchar(p$italic)) return(if (nzchar(p$rest)) p$rest else "")
  out <- sprintf("<i>%s</i>", p$italic)
  if (nzchar(p$rest)) out <- paste0(out, " ", p$rest)
  out
}

#' plotmath title for ggplot2: genus + epithet in italic, the remainder (author
#' and any descriptive `suffix`) in normal type. Returns a language object
#' suitable for `ggplot2::labs(title = ...)` / base `main =`.
#' @keywords internal
#' @noRd
.sp_title_expr <- function(name, suffix = "") {
  p <- .sp_parts(name)
  if (is.null(suffix) || is.na(suffix)) suffix <- ""
  tail <- paste0(if (nzchar(p$rest)) paste0(" ", p$rest) else "", suffix)
  if (!nzchar(p$italic)) return(if (nzchar(tail)) tail else "")
  if (nzchar(tail)) bquote(italic(.(p$italic)) * .(tail))
  else bquote(italic(.(p$italic)))
}

#' HTML title for plotly / HTML contexts: the scientific name (italic genus +
#' epithet, normal author) followed by a descriptive `suffix` in normal type.
#' @keywords internal
#' @noRd
.sp_title_html <- function(name, suffix = "") {
  if (is.null(suffix) || is.na(suffix)) suffix <- ""
  paste0(.sp_html(name), suffix)
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

#' Apply IUCN Red List Criterion B (size thresholds plus sub-criteria)
#'
#' Combines the EOO (B1) and AOO (B2) \emph{size} thresholds of Criterion B with
#' the sub-criteria required for a threatened listing, following the IUCN Red
#' List Categories and Criteria (v3.1) and the Guidelines for Using them
#' (Section 6, Criterion B; Section 10, DD/NT/NE), and aligning the category
#' selection with the \pkg{ConR} package (Dauby \emph{et al.} 2017,
#' \code{cat_criterion_b()}). Unlike \code{\link{iucn_category_B}} (size flags
#' only), this returns a category that can actually be applied.
#'
#' A taxon qualifies for a threatened category (CR, EN, VU) only if it meets the
#' size threshold \emph{and} at least two of these three sub-criteria:
#' \itemize{
#'   \item \strong{(a)} severely fragmented \emph{or} number of locations
#'     \eqn{\le} 1 (CR), 5 (EN) or 10 (VU);
#'   \item \strong{(b)} continuing decline (in EOO, AOO, area/extent/quality of
#'     habitat, number of locations/subpopulations, or mature individuals);
#'   \item \strong{(c)} extreme fluctuations.
#' }
#'
#' \strong{Relation to the number of locations.} Sub-criterion (a) is derived
#' from \code{n_locations} (and \code{severe_fragmentation} when supplied), and
#' it \emph{caps} the category exactly as ConR does: the returned category is the
#' \emph{less threatened} of the level implied by range size and the level
#' implied by the number of locations. A range whose EOO/AOO alone would be CR or
#' EN but that is spread over, say, eight locations is therefore returned as
#' \strong{VU} (\eqn{\le 10} locations), not EN - the number of locations, not
#' size alone, sets the category. Equivalently, per level: CR needs a single
#' location, EN \eqn{\le 5}, VU \eqn{\le 10} (unless severely fragmented, which
#' meets condition (a) at any level).
#'
#' \strong{Continuing decline (b), assumed by default.} Sub-criteria (b) and (c)
#' cannot be read from occurrence points. Following ConR - and because
#' \pkg{mappingAS} is built around habitat-conversion data, which is direct
#' evidence of a continuing decline in the area, extent and quality of habitat
#' (sub-criterion b(iii)) - a continuing decline is \strong{assumed to be
#' present} when it is not explicitly documented (\code{decline = NA} with
#' \code{assume_decline = TRUE}, the default). This is what makes the screening
#' track the IUCN thresholds instead of collapsing every undocumented taxon to
#' NT: with the location condition (a) met and decline (b) assumed, a
#' small-range, few-location taxon is returned at its CR/EN/VU size-and-location
#' level. Set \code{assume_decline = FALSE} (or \code{decline = FALSE}) to
#' require a documented decline instead, in which case a size-and-location match
#' with no second sub-criterion falls to NT. Extreme fluctuation (c) is never
#' assumed.
#'
#' Following Section 10: a taxon that meets a size threshold but not two
#' sub-criteria (for example a small range spread over too many locations, or
#' with decline explicitly absent) is returned as \strong{NT} (Near Threatened,
#' it "nearly meets" the requirements); one clearly far from every threshold as
#' \strong{LC}. \strong{DD} and \strong{NE} are \emph{never} assigned
#' automatically - they require the assessor's judgement about data adequacy and
#' are left to the user.
#'
#' @param eoo_km2,aoo_km2 EOO and AOO in km^2 (\code{NA} if undefined).
#' @param n_locations Estimated number of locations (\code{NA} if unknown). This
#'   is the value that caps the category through sub-criterion (a); see Details.
#' @param severe_fragmentation Logical; \code{TRUE} if the taxon is severely
#'   fragmented (meets condition (a) at any level). \code{NA} (default) = not
#'   assessed.
#' @param decline Logical; \code{TRUE} if there is a continuing decline
#'   (sub-criterion b), \code{FALSE} if a decline is known to be absent.
#'   \code{NA} (default) = not documented, resolved by \code{assume_decline}.
#' @param extreme_fluctuation Logical; \code{TRUE} if there are extreme
#'   fluctuations (sub-criterion c). \code{NA} (default) = not documented (never
#'   assumed).
#' @param assume_decline Logical; how an undocumented decline
#'   (\code{decline = NA}) is treated. \code{TRUE} (default) assumes a continuing
#'   decline is present, as ConR does and as the conversion data support;
#'   \code{FALSE} treats it as not met (a stricter, more conservative screening).
#'   Ignored when \code{decline} is \code{TRUE}/\code{FALSE}.
#' @param decline_detail Optional character giving the element(s) of the
#'   continuing decline for the IUCN code notation (v16, Section 6), appended in
#'   parentheses after the \code{b} in \code{code}: \code{"i"} EOO, \code{"ii"}
#'   AOO, \code{"iii"} area/extent/quality of habitat, \code{"iv"} number of
#'   locations/subpopulations, \code{"v"} number of mature individuals (combine
#'   with commas, e.g. \code{"ii,iii"}). \code{NULL} (default) omits it, so the
#'   code reads e.g. \code{"EN B1ab"}; with \code{"iii"} it reads
#'   \code{"EN B1ab(iii)"}.
#' @return A list with \code{category} (one of \code{"CR"}, \code{"EN"},
#'   \code{"VU"}, \code{"NT"}, \code{"LC"}, or \code{NA} when neither EOO nor AOO
#'   is available), \code{code} (e.g. \code{"VU B1ab"}), \code{qualifies_size}
#'   (the highest size level met, or \code{NA}), the evaluated sub-criteria
#'   \code{a}, \code{b}, \code{c}, and \code{decline_assumed} (\code{TRUE} when
#'   sub-criterion (b) was assumed rather than documented).
#' @examples
#' # EN-sized range with few locations: decline assumed by default -> EN B1ab
#' iucn_criterion_B(eoo_km2 = 3000, aoo_km2 = 400, n_locations = 4)$code
#' # Same size but spread over 8 locations -> capped to VU by the location count
#' iucn_criterion_B(eoo_km2 = 3000, aoo_km2 = 400, n_locations = 8)$category
#' # Require a documented decline: with none, a size/location match falls to NT
#' iucn_criterion_B(eoo_km2 = 3000, aoo_km2 = 400, n_locations = 4,
#'                  assume_decline = FALSE)$category
#' @seealso \code{\link{iucn_category_B}}
#' @references
#' Dauby G. \emph{et al.} (2017) ConR: An R package to assist large-scale
#'   multispecies preliminary conservation assessments using distribution data.
#'   \emph{Ecology and Evolution} 7:11292-11303. \doi{10.1002/ece3.3704}
#' @export
iucn_criterion_B <- function(eoo_km2 = NA_real_, aoo_km2 = NA_real_,
                             n_locations = NA_real_,
                             severe_fragmentation = NA,
                             decline = NA, extreme_fluctuation = NA,
                             assume_decline = TRUE, decline_detail = NULL) {
  levels <- list(
    CR = list(eoo = 100,   aoo = 10,   loc = 1),
    EN = list(eoo = 5000,  aoo = 500,  loc = 5),
    VU = list(eoo = 20000, aoo = 2000, loc = 10))
  # Condition (b): a continuing decline is assumed present when undocumented
  # (ConR default), which the habitat-conversion data support; assume_decline =
  # FALSE restores the strict "not documented = not met" behaviour. A NULL or
  # non-scalar decline is treated as undocumented.
  if (length(decline) != 1L) decline <- NA
  b_assumed <- is.na(decline) && isTRUE(assume_decline)
  b    <- isTRUE(decline) || b_assumed
  cc   <- isTRUE(extreme_fluctuation)
  frag <- isTRUE(severe_fragmentation)
  nloc <- suppressWarnings(as.numeric(n_locations))
  eoo  <- suppressWarnings(as.numeric(eoo_km2))
  aoo  <- suppressWarnings(as.numeric(aoo_km2))

  for (lv in names(levels)) {
    th <- levels[[lv]]
    size_b1 <- is.finite(eoo) && eoo < th$eoo
    size_b2 <- is.finite(aoo) && aoo < th$aoo
    if (!size_b1 && !size_b2) next
    # Condition (a): severe fragmentation, or the number of locations within the
    # threshold for THIS level. Because the loop returns the most threatened
    # level whose size AND location count both qualify, the location count caps
    # the category (ConR's max() of the size- and location-implied levels).
    a <- frag || (is.finite(nloc) && nloc <= th$loc)
    if (sum(a, b, cc) >= 2) {
      axes <- paste0(c(if (size_b1) "1", if (size_b2) "2"), collapse = "+")
      # IUCN code notation (v16, sect. 6, e.g. "EN B1ab(iii)"): the letters a/b/c
      # name the conditions met and, for (b), the parenthetical roman numeral(s)
      # name the element(s) in continuing decline (i EOO, ii AOO, iii habitat,
      # iv locations/subpopulations, v mature individuals).
      b_lab <- if (b) paste0("b", if (!is.null(decline_detail) &&
                                        nzchar(decline_detail))
                                    paste0("(", decline_detail, ")") else "")
      lett <- paste0(c(if (a) "a", b_lab, if (cc) "c"), collapse = "")
      return(list(category = lv, code = paste0(lv, " B", axes, lett),
                  qualifies_size = lv, a = a, b = b, c = cc,
                  decline_assumed = b_assumed))
    }
  }

  a_vu <- frag || (is.finite(nloc) && nloc <= levels$VU$loc)
  size_any <- (is.finite(eoo) && eoo < levels$VU$eoo) ||
              (is.finite(aoo) && aoo < levels$VU$aoo)
  if (size_any)
    return(list(category = "NT", code = "NT (meets B size only)",
                qualifies_size = "VU", a = a_vu, b = b, c = cc,
                decline_assumed = b_assumed))
  if (!is.finite(eoo) && !is.finite(aoo))
    return(list(category = NA_character_, code = NA_character_,
                qualifies_size = NA_character_, a = a_vu, b = b, c = cc,
                decline_assumed = b_assumed))
  list(category = "LC", code = "LC", qualifies_size = NA_character_,
       a = a_vu, b = b, c = cc, decline_assumed = b_assumed)
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