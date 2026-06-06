#' geoConvBR: Range Metrics and MapBiomas Habitat Conversion
#'
#' A GeoCat-style toolkit for IUCN Red List Criterion B screening. It imports
#' species occurrence points from XLSX, CSV, or shapefile inputs, computes the
#' Extent of Occurrence (EOO, minimum convex polygon) and the Area of Occupancy
#' (AOO, 2 km grid), and quantifies the percentage of converted (anthropic)
#' versus remaining natural habitat inside both the EOO and the AOO using the
#' most recent MapBiomas land-use/land-cover collection.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{read_occurrences}}}{Import points from csv/xlsx/shapefile.}
#'   \item{\code{\link{assess_species}}}{Full pipeline (EOO, AOO, conversion).}
#'   \item{\code{\link{calc_eoo}}, \code{\link{calc_aoo}}}{Range metrics only.}
#'   \item{\code{\link{summarise_conversion}}}{Convert class areas to % converted.}
#'   \item{\code{\link{map_species}}, \code{\link{plot_conversion}}}{Visual output.}
#'   \item{\code{\link{run_app}}}{Launch the Shiny GUI.}
#' }
#'
#' @section MapBiomas backends:
#' \code{backend = "local"} (default) reads a windowed crop of the national
#' GeoTIFF via GDAL /vsicurl/ (no Google Earth Engine account needed).
#' \code{backend = "gee"} uses \pkg{rgee} server-side reductions for very large
#' ranges.
#'
#' @keywords internal
"_PACKAGE"
