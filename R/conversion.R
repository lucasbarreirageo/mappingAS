#' Summarise habitat conversion from MapBiomas class areas
#'
#' Aggregates a table of per-class areas into conservation groups and computes
#' the percentage of converted (anthropic) versus remaining natural habitat.
#'
#' The headline metrics use a \emph{terrestrial} denominator (natural +
#' anthropic), excluding water, not-observed and ambiguous "other" classes, which
#' is the most defensible measure of habitat conversion. A total-area version
#' (denominator = everything except not-observed) is also returned for
#' transparency.
#'
#' @param class_areas Either a \code{data.frame} with columns \code{code} and
#'   \code{area_km2} (e.g. from \code{\link{mb_class_areas_gee}}) or a
#'   \pkg{terra} \code{SpatRaster} of MapBiomas codes (which is tabulated via
#'   \code{\link{mb_class_areas_raster}}).
#' @param collection Integer collection number (default \code{10}).
#' @param water_in_denominator Logical; if \code{TRUE}, water is treated as
#'   natural and included in the terrestrial denominator (default \code{FALSE}).
#' @return A list with: \code{converted_pct}, \code{natural_pct} (terrestrial,
#'   the headline numbers), \code{converted_pct_total}, \code{natural_pct_total},
#'   the group areas (\code{natural_km2}, \code{anthropic_km2}, \code{water_km2},
#'   \code{other_km2}, \code{not_observed_km2}, \code{total_km2}) and
#'   \code{by_class} (the per-class table joined to the legend).
#' @examples
#' ca <- data.frame(code = c(3, 15, 24, 33), area_km2 = c(60, 30, 10, 5))
#' summarise_conversion(ca)$converted_pct
#' @export
summarise_conversion <- function(class_areas, collection = 10,
                                 water_in_denominator = FALSE) {
  if (inherits(class_areas, "SpatRaster")) {
    class_areas <- mb_class_areas_raster(class_areas)
  }
  stopifnot(all(c("code", "area_km2") %in% names(class_areas)))

  leg <- mb_legend(collection)
  byc <- merge(class_areas, leg, by = "code", all.x = TRUE)
  byc$group[is.na(byc$group)] <- "other"
  byc <- byc[order(-byc$area_km2), , drop = FALSE]

  gsum <- function(g) sum(byc$area_km2[byc$group %in% g], na.rm = TRUE)
  natural        <- gsum("natural")
  anthropic      <- gsum("anthropic")
  water          <- gsum("water")
  other          <- gsum("other")
  not_observed   <- gsum("not_observed")
  total          <- sum(byc$area_km2, na.rm = TRUE)

  terr_denom <- natural + anthropic + if (water_in_denominator) water else 0
  nat_for_pct <- natural + if (water_in_denominator) water else 0
  conv_pct <- if (terr_denom > 0) 100 * anthropic / terr_denom else NA_real_
  nat_pct  <- if (terr_denom > 0) 100 * nat_for_pct / terr_denom else NA_real_

  total_denom <- total - not_observed
  conv_pct_tot <- if (total_denom > 0) 100 * anthropic / total_denom else NA_real_
  nat_pct_tot  <- if (total_denom > 0) 100 * natural / total_denom else NA_real_

  list(
    converted_pct = conv_pct,
    natural_pct = nat_pct,
    converted_pct_total = conv_pct_tot,
    natural_pct_total = nat_pct_tot,
    natural_km2 = natural,
    anthropic_km2 = anthropic,
    water_km2 = water,
    other_km2 = other,
    not_observed_km2 = not_observed,
    total_km2 = total,
    by_class = byc
  )
}
