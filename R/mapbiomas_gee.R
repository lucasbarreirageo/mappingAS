#' MapBiomas class areas via Google Earth Engine (optional backend)
#'
#' Computes the area (km^2) of each MapBiomas pixel class inside \code{aoi}
#' entirely on Google Earth Engine, without downloading any raster. This is the
#' recommended backend for very large ranges (e.g. continental EOOs) where a
#' local windowed read would transfer too much data.
#'
#' Requires the \pkg{rgee} package and an initialised Earth Engine session
#' (\code{rgee::ee_Initialize()}); see the package README for one-time setup.
#'
#' @param aoi An \code{sf}/\code{sfc} polygon (any CRS).
#' @param year Integer year (default \code{2024}).
#' @param collection Integer MapBiomas collection id (default \code{10}).
#' @param version Asset version string for the public LULC collection
#'   (default \code{"v1"}; some mirrors expose \code{"v2"} for Collection 10.1).
#' @param asset Earth Engine \code{ImageCollection} id
#'   (default \code{"projects/mapbiomas-public/assets/brazil/lulc/v1"}).
#' @param scale Nominal pixel scale in metres for the reduction (default
#'   \code{30}).
#' @param max_pixels Maximum pixels for the server-side reduction
#'   (default \code{1e13}).
#' @return A \code{data.frame} with columns \code{code} and \code{area_km2}.
#' @export
mb_class_areas_gee <- function(aoi, year = 2024, collection = 10,
                               version = "v1",
                               asset = "projects/mapbiomas-public/assets/brazil/lulc/v1",
                               scale = 30, max_pixels = 1e13) {
  if (!requireNamespace("rgee", quietly = TRUE)) {
    stop("Package 'rgee' is required for the GEE backend. ",
         "Install it and run rgee::ee_Initialize().", call. = FALSE)
  }
  ee <- rgee::ee

  aoi <- sf::st_geometry(aoi)
  if (is.na(sf::st_crs(aoi))) sf::st_crs(aoi) <- 4326
  aoi <- sf::st_transform(aoi, 4326)
  ee_geom <- rgee::sf_as_ee(sf::st_union(aoi))

  ic <- ee$ImageCollection(asset)$
    filter(ee$Filter$eq("collection_id", as.numeric(collection)))$
    filter(ee$Filter$eq("version", version))$
    filter(ee$Filter$eq("year", as.integer(year)))
  img <- ic$mosaic()$select(0)$rename("class")

  # area (m^2) grouped by class code, summed over the AOI
  area_img <- ee$Image$pixelArea()$addBands(img)
  reduced <- area_img$reduceRegion(
    reducer = ee$Reducer$sum()$group(groupField = 1L, groupName = "code"),
    geometry = ee_geom,
    scale = scale,
    maxPixels = max_pixels,
    bestEffort = TRUE
  )
  groups <- ee$List(reduced$get("groups"))$getInfo()

  if (length(groups) == 0) {
    return(data.frame(code = integer(0), area_km2 = numeric(0)))
  }
  codes <- vapply(groups, function(g) as.integer(g[["code"]]), integer(1))
  areas <- vapply(groups, function(g) as.numeric(g[["sum"]]) / 1e6, numeric(1))
  out <- data.frame(code = codes, area_km2 = areas)
  out[is.finite(out$area_km2) & out$area_km2 > 0, , drop = FALSE]
}
