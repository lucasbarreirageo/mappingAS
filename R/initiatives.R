#' MapBiomas initiatives supported by mappingAS
#'
#' \pkg{mappingAS} can read land-use/land-cover from several public MapBiomas
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
#'     Collection 6 (1986-2023). The whole Amazon basin across countries.}
#'   \item{\code{"colombia"}}{MapBiomas Colombia, Collection 3 (1985-2024).}
#'   \item{\code{"argentina"}}{MapBiomas Argentina, Collection 2 (1985-2024).}
#'   \item{\code{"bolivia"}}{MapBiomas Bolivia, Collection 3 (1985-2024).}
#'   \item{\code{"chile"}}{MapBiomas Chile, Collection 1 (2000-2022).}
#'   \item{\code{"ecuador"}}{MapBiomas Ecuador, Collection 3 (1985-2024).}
#'   \item{\code{"peru"}}{MapBiomas Peru, Collection 3 (1985-2024).}
#'   \item{\code{"venezuela"}}{MapBiomas Venezuela, Collection 2 (1985-2023).}
#'   \item{\code{"paraguay"}}{MapBiomas Paraguay, Collection 2 (1985-2023).}
#'   \item{\code{"uruguay"}}{MapBiomas Uruguay, Collection 1 (1985-2022).}
#' }
#'
#' @return A named list, one element per initiative, each a list with
#'   \code{label} (human-readable name), \code{collection} (default collection
#'   number), \code{years} (integer vector of available years) and \code{key}.
#' @examples
#' names(mb_initiatives())
#' mb_initiatives()$peru$years
#' @seealso \code{\link{mb_source_url}}, \code{\link{mb_legend}},
#'   \code{\link{assess_species}}
#' @export
mb_initiatives <- function() {
  reg <- .mb_registry()
  lapply(reg, function(x) x[c("key", "label", "collection", "years")])
}

#' Internal registry: metadata + per-initiative URL builders
#'
#' Each initiative carries a \code{build(year, collection)} function returning
#' the object path (relative to the public bucket), because the file layout
#' differs between products (\code{coverage/<name>_coverage_YYYY.tif} for most
#' countries, integration-style \code{..._integration_v1-classification_YYYY.tif}
#' for Amazonia/Peru/Venezuela, and Argentina/Chile path quirks).
#' @keywords internal
#' @noRd
.mb_registry <- function() {
  cov <- function(country) function(year, collection)
    sprintf("%s/collection_%d/coverage/%s_coverage_%d.tif",
            country, collection, country, year)
  list(
    brazil = list(
      key = "brazil", label = "MapBiomas Brazil",
      collection = 10L, years = 1985:2024,
      build = function(year, collection)
        sprintf("brasil/collection_%d/lulc/coverage/brazil_coverage_%d.tif",
                collection, year)),
    amazonia = list(
      key = "amazonia", label = "MapBiomas Amazonia (Pan-Amazon / RAISG)",
      collection = 6L, years = 1986:2023,
      build = function(year, collection)
        sprintf(paste0("amazon/lulc/collection_%d/integration/",
                       "mapbiomas_collection%d0_integration_v1-classification_%d.tif"),
                collection, collection, year)),
    colombia = list(
      key = "colombia", label = "MapBiomas Colombia",
      collection = 3L, years = 1985:2024, build = cov("colombia")),
    argentina = list(
      key = "argentina", label = "MapBiomas Argentina",
      collection = 2L, years = 1985:2024,
      build = function(year, collection)
        sprintf("argentina/collection-%d/coverage/argentina_coverage_%d.tif",
                collection, year)),
    bolivia = list(
      key = "bolivia", label = "MapBiomas Bolivia",
      collection = 3L, years = 1985:2024, build = cov("bolivia")),
    chile = list(
      key = "chile", label = "MapBiomas Chile",
      collection = 1L, years = 2000:2022,
      build = function(year, collection)
        sprintf("chile/coverage/chile_coverage_%d.tif", year)),
    ecuador = list(
      key = "ecuador", label = "MapBiomas Ecuador",
      collection = 3L, years = 1985:2024, build = cov("ecuador")),
    peru = list(
      key = "peru", label = "MapBiomas Peru",
      collection = 3L, years = 1985:2024,
      build = function(year, collection)
        sprintf(paste0("peru/collection_%d/LULC/",
                       "peru_collection%d_integration_v1-classification_%d.tif"),
                collection, collection, year)),
    venezuela = list(
      key = "venezuela", label = "MapBiomas Venezuela",
      collection = 2L, years = 1985:2023,
      build = function(year, collection)
        sprintf(paste0("venezuela/collection_%d/lulc/integration/",
                       "mapbiomas_venezuela_collection%d_integration_v1-classification_%d.tif"),
                collection, collection, year)),
    paraguay = list(
      key = "paraguay", label = "MapBiomas Paraguay",
      collection = 2L, years = 1985:2023,
      build = function(year, collection)
        sprintf(paste0("paraguay/collection_%d/",
                       "mapbiomas_paraguay_collection%d_integration_v1-classification_%d.tif"),
                collection, collection, year)),
    uruguay = list(
      key = "uruguay", label = "MapBiomas Uruguay",
      collection = 1L, years = 1985:2022, build = cov("uruguay"))
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
                "equador" = "ecuador",
                "paraguai" = "paraguay",
                "uruguai" = "uruguay",
                key)
  reg <- .mb_registry()
  if (!key %in% names(reg)) {
    stop("Unknown MapBiomas initiative: '", initiative,
         "'. Use one of: ", paste(names(reg), collapse = ", "), ".",
         call. = FALSE)
  }
  reg[[key]]
}

#' @keywords internal
#' @noRd
.mb_default_collection <- function(initiative = "brazil") {
  .mb_resolve_initiative(initiative)$collection
}
