#' Per-class MapBiomas composition table (every class) for an assessment
#'
#' Returns, for each species, the area and percentage of \emph{every} MapBiomas
#' class found inside the EOO and/or the AOO. This complements the headline
#' natural-versus-converted summary in \code{\link{assess_species}} with the full
#' class-by-class breakdown.
#'
#' @param assessment A \code{geoconv_assessment} from \code{\link{assess_species}}
#'   (run with \code{mapbiomas = TRUE}).
#' @param species Optional species name (or vector). \code{NULL} (default) uses
#'   all assessed species.
#' @param range \code{"both"} (default), \code{"eoo"} or \code{"aoo"}.
#' @return A long \code{data.frame} with columns \code{species}, \code{range}
#'   (\code{"EOO"}/\code{"AOO"}), \code{code}, \code{class_pt}, \code{class_en},
#'   \code{group}, \code{area_km2} and \code{pct} (percentage of the mapped area
#'   within that range). Empty (with a warning) if MapBiomas was not computed.
#' @examples
#' \donttest{
#' occ <- read_occurrences(system.file("extdata", "example_occurrences.csv",
#'                                     package = "mappingAS"))
#' res <- assess_species(occ, year = 2024, verbose = FALSE)  # reads MapBiomas
#' class_table(res)
#' }
#' @export
class_table <- function(assessment, species = NULL,
                        range = c("both", "eoo", "aoo")) {
  stopifnot(inherits(assessment, "geoconv_assessment"))
  range <- match.arg(range)
  d <- assessment$detail
  sp <- if (is.null(species)) names(d) else species

  pull <- function(conv, rng, s) {
    if (is.null(conv) || is.null(conv$by_class) || !nrow(conv$by_class)) return(NULL)
    bc <- conv$by_class
    unk <- is.na(bc$class_pt); bc$class_pt[unk] <- paste0("Classe ", bc$code[unk])
    bc$class_en[is.na(bc$class_en)] <- bc$class_pt[is.na(bc$class_en)]
    bc$group[is.na(bc$group)] <- "other"
    tot <- sum(bc$area_km2, na.rm = TRUE)
    data.frame(
      species  = s,
      range    = rng,
      code     = bc$code,
      class_pt = bc$class_pt,
      class_en = bc$class_en,
      group    = bc$group,
      area_km2 = round(bc$area_km2, 4),
      pct      = if (tot > 0) round(100 * bc$area_km2 / tot, 2) else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  parts <- list(); k <- 0L
  for (s in sp) {
    obj <- d[[s]]; if (is.null(obj)) next
    if (range %in% c("both", "eoo")) { k <- k + 1L; parts[[k]] <- pull(obj$eoo_conversion, "EOO", s) }
    if (range %in% c("both", "aoo")) { k <- k + 1L; parts[[k]] <- pull(obj$aoo_conversion, "AOO", s) }
  }
  res <- do.call(rbind, parts)
  if (is.null(res) || !nrow(res)) {
    warning("No per-class data found (was the assessment run with mapbiomas = TRUE?).",
            call. = FALSE)
    return(data.frame())
  }
  res <- res[order(res$species, res$range, -res$area_km2), , drop = FALSE]
  rownames(res) <- NULL
  res
}
