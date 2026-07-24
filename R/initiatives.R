#' MapBiomas initiatives supported by mappingAS
#'
#' \pkg{mappingAS} can read land-use/land-cover from three public MapBiomas
#' initiatives, all streamed as annual Cloud-Optimized GeoTIFFs from the public
#' \code{storage.googleapis.com/mapbiomas-public} bucket via GDAL's
#' \code{/vsicurl/} driver - no Google Earth Engine account and no Google Drive
#' download required. Each initiative is one coherent product with its own
#' default collection, year span and file layout, but all share the same
#' \emph{standardised} legend (see \code{\link{mb_legend}}), so a species whose
#' range spans more than one product can be assessed with consistent class
#' names, colours and conservation groups.
#'
#' \describe{
#'   \item{\code{"brazil"}}{MapBiomas Brazil, Collection 10 (1985-2024).}
#'   \item{\code{"amazonia"}}{MapBiomas Amazonia / Pan-Amazon (RAISG),
#'     Collection 6 (1986-2023). Covers the whole Amazon basin across countries.}
#'   \item{\code{"colombia"}}{MapBiomas Colombia, Collection 3 (1985-2024).}
#' }
#'
#' @return A named list, one element per initiative, each a list with
#'   \code{label} (human-readable name), \code{collection} (default collection
#'   number), \code{years} (integer vector of available years) and \code{key}.
#' @examples
#' names(mb_initiatives())
#' mb_initiatives()$amazonia$years
#' @seealso \code{\link{mb_source_url}}, \code{\link{mb_legend}},
#'   \code{\link{assess_species}}
#' @export
mb_initiatives <- function() {
  list(
    brazil = list(
      key = "brazil",
      label = "MapBiomas Brazil",
      collection = 10L,
      years = 1985:2024
    ),
    amazonia = list(
      key = "amazonia",
      label = "MapBiomas Amazonia (Pan-Amazon / RAISG)",
      collection = 6L,
      years = 1986:2023
    ),
    colombia = list(
      key = "colombia",
      label = "MapBiomas Colombia",
      collection = 3L,
      years = 1985:2024
    )
  )
}

#' @keywords internal
#' @noRd
.mb_resolve_initiative <- function(initiative = "brazil") {
  if (is.null(initiative) || length(initiative) != 1L || is.na(initiative)) {
    initiative <- "brazil"
  }
  key <- tolower(trimws(as.character(initiative)))
  # a few friendly aliases
  key <- switch(key,
                "brasil" = "brazil",
                "amazon" = "amazonia",
                "pan-amazonia" = "amazonia",
                "panamazonia" = "amazonia",
                "pan_amazonia" = "amazonia",
                "amazônia" = "amazonia",
                key)
  ini <- mb_initiatives()
  if (!key %in% names(ini)) {
    stop("Unknown MapBiomas initiative: '", initiative,
         "'. Use one of: ", paste(names(ini), collapse = ", "), ".",
         call. = FALSE)
  }
  ini[[key]]
}

#' @keywords internal
#' @noRd
.mb_default_collection <- function(initiative = "brazil") {
  .mb_resolve_initiative(initiative)$collection
}
