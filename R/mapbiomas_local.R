#' Build the MapBiomas national GeoTIFF URL for a given year
#'
#' Returns the public Google Cloud Storage URL of the MapBiomas Brazil
#' land-use/land-cover annual mosaic. Collection 10 covers 1985-2024.
#'
#' @param year Integer year (1985-2024 for Collection 10).
#' @param collection Integer collection number (default \code{10}).
#' @return A length-1 character URL.
#' @examples
#' mb_source_url(2024)
#' @export
mb_source_url <- function(year = 2024, collection = 10) {
  sprintf(
    "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_%d/lulc/coverage/brazil_coverage_%d.tif",
    as.integer(collection), as.integer(year)
  )
}

#' Crop a MapBiomas LULC raster to an area of interest (local backend)
#'
#' Reads the MapBiomas national mosaic and crops/masks it to \code{aoi} using
#' \pkg{terra}. By default it streams a \emph{windowed} read of only the AOI's
#' bounding box from the public GeoTIFF via GDAL's \code{/vsicurl/} driver, so
#' there is no Google Earth Engine account and no full-country download. For
#' offline or repeated use, point \code{src} to a local GeoTIFF instead.
#'
#' For very large ranges (e.g. continental EOOs) the windowed read can still
#' transfer a lot of data; in that case prefer \code{\link{mb_class_areas_gee}}.
#'
#' @param aoi An \code{sf}/\code{sfc} polygon (any CRS) defining the area to
#'   extract, e.g. an EOO hull or the union of AOO cells.
#' @param year Integer year (default \code{2024}).
#' @param collection Integer collection number (default \code{10}).
#' @param src Optional path or URL to a MapBiomas GeoTIFF. If \code{NULL}
#'   (default) the public Collection \code{collection} URL for \code{year} is
#'   used through \code{/vsicurl/}.
#' @param mask Logical; if \code{TRUE} (default) pixels outside the polygon are
#'   set to \code{NA}. If \code{FALSE} only a rectangular crop is returned.
#' @return A \pkg{terra} \code{SpatRaster} of MapBiomas pixel codes restricted to
#'   the AOI, in the raster's native CRS.
#' @export
mb_raster_local <- function(aoi, year = 2024, collection = 10,
                            src = NULL, mask = TRUE) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required.", call. = FALSE)
  }
  aoi <- sf::st_geometry(aoi)
  if (is.na(sf::st_crs(aoi))) sf::st_crs(aoi) <- 4326

  # GDAL options to make remote windowed reads robust and cached
  terra::setGDALconfig("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")
  terra::setGDALconfig("CPL_VSIL_CURL_USE_HEAD", "NO")
  terra::setGDALconfig("GDAL_HTTP_MAX_RETRY", "3")
  terra::setGDALconfig("GDAL_HTTP_RETRY_DELAY", "1")
  terra::setGDALconfig("VSI_CACHE", "TRUE")

  if (is.null(src)) src <- mb_source_url(year, collection)
  path <- if (grepl("^https?://", src)) paste0("/vsicurl/", src) else src

  r <- tryCatch(
    terra::rast(path),
    error = function(e) {
      stop("Could not open MapBiomas raster at:\n  ", src,
           "\nIf you are offline or the host is blocked, download the GeoTIFF ",
           "and pass it via `src=`.\nOriginal error: ",
           conditionMessage(e), call. = FALSE)
    }
  )

  aoi_r <- sf::st_transform(aoi, terra::crs(r))
  vect <- terra::vect(aoi_r)
  rc <- terra::crop(r, terra::ext(vect), snap = "out")
  if (mask) rc <- terra::mask(rc, vect)
  names(rc) <- "mapbiomas_class"
  rc
}

#' Tabulate area per MapBiomas class from a cropped raster
#'
#' Computes the true (geodesic, latitude-corrected) area of each MapBiomas pixel
#' class inside a cropped/masked raster. Works for rasters in geographic CRS,
#' where pixel area varies with latitude, by using \code{terra::cellSize()}.
#'
#' @param r A \pkg{terra} \code{SpatRaster} of MapBiomas codes (e.g. from
#'   \code{\link{mb_raster_local}}).
#' @return A \code{data.frame} with columns \code{code} and \code{area_km2}.
#' @export
mb_class_areas_raster <- function(r) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required.", call. = FALSE)
  }
  area <- terra::cellSize(r, unit = "km", mask = TRUE)
  z <- terra::zonal(area, r, fun = "sum", na.rm = TRUE)
  names(z) <- c("code", "area_km2")
  z$code <- as.integer(z$code)
  z[is.finite(z$area_km2) & z$area_km2 > 0, , drop = FALSE]
}
