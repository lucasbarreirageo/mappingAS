# -----------------------------------------------------------------------------
# Lower and upper bounds of the Area of Occupancy (AOO), and the Criterion B
# category range they imply.
#
# The IUCN Red List guidelines (and sRedList; Cazalis et al. 2024) recommend
# bracketing the AOO between:
#   * a LOWER bound - the occupied 2x2 km cells that actually contain occurrence
#     records (what calc_aoo() already returns); it typically UNDER-estimates
#     AOO because of incomplete sampling; and
#   * an UPPER bound - derived from the Area of Habitat (AOH), i.e. the suitable
#     (natural) habitat within the range; it typically OVER-estimates AOO.
# The true AOO lies between the two (Brooks et al. 2019).
#
# mappingAS already measures the natural (suitable) habitat area inside the EOO
# as part of its land-cover conversion analysis (summarise_conversion()'s
# natural_km2). aoo_bounds() reuses that number as the AOH and turns the pair of
# bounds into a RANGE of Criterion B2 categories, so the assessment reports the
# uncertainty explicitly instead of a single point estimate - exactly the
# "diagnose with more precision" behaviour of the sRedList summary step.
# -----------------------------------------------------------------------------

#' Lower/upper Area of Occupancy bounds and the Criterion B2 category range
#'
#' Turns the occurrence-based AOO (the lower bound) and the Area of Habitat
#' (AOH; the suitable/natural habitat within the range, an upper bound) into a
#' bracketed AOO and the corresponding range of Criterion B2 \emph{size}
#' categories, following the IUCN guidance that the true AOO lies between an
#' occurrence-based minimum and a habitat-based maximum (Brooks \emph{et al.}
#' 2019; IUCN Standards and Petitions Committee 2024), as implemented in
#' sRedList (Cazalis \emph{et al.} 2024).
#'
#' The upper bound is the AOH clamped so it is never below the lower bound and
#' never above the EOO (occupancy cannot exceed the extent of occurrence). When
#' the AOH is not available (\code{aoh_km2 = NA}, e.g. land cover was not
#' computed) the upper bound falls back to the lower bound and the range
#' collapses to a single category.
#'
#' @param aoo_lower_km2 The occurrence-based AOO in km^2 (from
#'   \code{\link{calc_aoo}}); the lower bound.
#' @param aoh_km2 The Area of Habitat (suitable/natural habitat area) within the
#'   range, in km^2 - e.g. the \code{natural_km2} of the EOO conversion from
#'   \code{\link{summarise_conversion}}. \code{NA} (default) if unknown.
#' @param eoo_km2 The EOO in km^2 (from \code{\link{calc_eoo}}), used to cap the
#'   upper bound. \code{NA} (default) leaves the upper bound uncapped.
#' @return A list with \code{aoo_lower}, \code{aoo_upper}, \code{aoh} (the
#'   clamped habitat area used), \code{cat_lower} / \code{cat_upper} (the B2
#'   size category at each bound, e.g. \code{"EN (B2 size)"}) and
#'   \code{cat_range} (a compact string such as \code{"EN-VU"}, or a single
#'   category when both bounds agree).
#' @references
#' Brooks T.M. \emph{et al.} (2019) Measuring terrestrial Area of Habitat (AOH)
#'   and its utility for the IUCN Red List. \emph{Trends in Ecology & Evolution}
#'   34:977-986. \doi{10.1016/j.tree.2019.06.009}
#'
#' Cazalis V. \emph{et al.} (2024) Accelerating and standardising IUCN Red List
#'   assessments with sRedList. \emph{Biological Conservation} 298:110761.
#'   \doi{10.1016/j.biocon.2024.110761}
#' @seealso \code{\link{calc_aoo}}, \code{\link{iucn_category_B}},
#'   \code{\link{iucn_category_B_range}}
#' @examples
#' # Occurrence AOO of 48 km^2, but 900 km^2 of natural habitat within a
#' # 6000 km^2 EOO: AOO is bracketed EN (lower) to VU (upper).
#' aoo_bounds(aoo_lower_km2 = 48, aoh_km2 = 900, eoo_km2 = 6000)
#' @export
aoo_bounds <- function(aoo_lower_km2, aoh_km2 = NA_real_, eoo_km2 = NA_real_) {
  lo  <- suppressWarnings(as.numeric(aoo_lower_km2))[1]
  aoh <- suppressWarnings(as.numeric(aoh_km2))[1]
  eoo <- suppressWarnings(as.numeric(eoo_km2))[1]

  # Upper bound = AOH, but never below the lower bound and never above the EOO.
  up <- if (is.finite(aoh)) aoh else lo
  if (is.finite(up) && is.finite(lo)) up <- max(up, lo)
  if (is.finite(up) && is.finite(eoo)) up <- min(up, eoo)

  cat_lo <- iucn_category_B(aoo_km2 = lo)$aoo_category
  cat_up <- iucn_category_B(aoo_km2 = up)$aoo_category

  list(
    aoo_lower = lo,
    aoo_upper = up,
    aoh       = if (is.finite(aoh)) aoh else NA_real_,
    cat_lower = cat_lo,
    cat_upper = cat_up,
    # Label reads most-threatened (lower AOO) to least-threatened (upper AOO).
    cat_range = .cat_range_label(cat_lo, cat_up)
  )
}

#' Compact category-range label from two size-category strings (passed
#' most-threatened first). Collapses to one label when both resolve equal.
#' @keywords internal
#' @noRd
.cat_range_label <- function(cat_a, cat_b) {
  short <- function(x) {
    if (is.null(x) || is.na(x)) return(NA_character_)
    m <- regmatches(x, regexpr("^(CR|EN|VU|NT|LC)", x))
    if (length(m) && nzchar(m)) m else "not threatened"
  }
  a <- short(cat_a); b <- short(cat_b)
  if (is.na(a) && is.na(b)) return(NA_character_)
  if (is.na(a)) return(b)
  if (is.na(b)) return(a)
  if (identical(a, b)) a else paste0(a, "-", b)
}
