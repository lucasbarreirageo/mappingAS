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

#' @keywords internal
#' @noRd
.mb_set_gdal <- function() {
  keys <- c("GDAL_DISABLE_READDIR_ON_OPEN", "CPL_VSIL_CURL_USE_HEAD",
            "GDAL_HTTP_MAX_RETRY", "GDAL_HTTP_RETRY_DELAY", "VSI_CACHE")
  vals <- c("EMPTY_DIR", "NO", "3", "1", "TRUE")
  old <- stats::setNames(lapply(keys, terra::getGDALconfig), keys)
  for (i in seq_along(keys)) terra::setGDALconfig(keys[i], vals[i])
  old
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
#' @param cache Logical; if \code{TRUE} (default) the rectangular windowed crop
#'   is cached on disk (keyed by source + bounding box), so reading the same
#'   area again (e.g. the EOO during assessment and later when mapping, or a
#'   re-run) does not re-download. Masking is always applied in memory after the
#'   cached crop is loaded.
#' @param cache_dir Directory for the windowed-crop cache (default a
#'   \code{mappingAS_mb_cache} folder under \code{tempdir()}).
#' @return A \pkg{terra} \code{SpatRaster} of MapBiomas pixel codes restricted to
#'   the AOI, in the raster's native CRS.
#' @export
mb_raster_local <- function(aoi, year = 2024, collection = 10,
                            src = NULL, mask = TRUE,
                            cache = TRUE, cache_dir = NULL) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required.", call. = FALSE)
  }
  aoi <- sf::st_geometry(aoi)
  if (is.na(sf::st_crs(aoi))) sf::st_crs(aoi) <- 4326

  # robust remote reads; restore the user's GDAL settings on exit
  old <- .mb_set_gdal()
  on.exit(for (k in names(old)) terra::setGDALconfig(k, old[[k]]), add = TRUE)

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
  rc <- .mb_window(r, terra::ext(vect), src, year, collection, cache, cache_dir)
  if (mask) rc <- terra::mask(rc, vect)
  names(rc) <- "mapbiomas_class"
  rc
}

#' @keywords internal
#' @noRd
.mb_window <- function(r, e, src, year, collection, cache, cache_dir) {
  rc <- terra::crop(r, e, snap = "out")          # lazy windowed read
  if (!isTRUE(cache)) return(rc)

  if (is.null(cache_dir)) cache_dir <- file.path(tempdir(), "mappingAS_mb_cache")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  key <- paste(basename(src), year, collection,
               paste(round(as.vector(e), 5), collapse = "_"), sep = "__")
  f <- file.path(cache_dir, paste0(gsub("[^A-Za-z0-9_]+", "-", key), ".tif"))
  if (file.exists(f)) return(terra::rast(f))

  terra::writeRaster(rc, f, datatype = "INT1U", overwrite = TRUE,
                     gdal = "COMPRESS=LZW")       # materialise the read once
  terra::rast(f)
}

# Downsampled, optionally reprojected raster for *display* (not area calc).
# Tries a fast decimated read first (GDAL -outsize uses the COG overviews, so a
# large window is not streamed at native 30 m); on any failure it falls back to
# the native windowed read + aggregate, so behaviour is never lost.
#' @keywords internal
#' @noRd
.mb_raster_display <- function(aoi, year, collection, src, max_pixels = 600,
                               crs = NULL, cache = TRUE) {
  r <- tryCatch(
    .mb_display_read(aoi, year, collection, src, max_pixels),
    error = function(e) NULL)
  if (is.null(r)) {
    r <- mb_raster_local(aoi, year = year, collection = collection, src = src,
                         mask = TRUE, cache = cache)
    d <- max(dim(r)[1:2])
    if (d > max_pixels)
      r <- terra::aggregate(r, fact = ceiling(d / max_pixels),
                            fun = "modal", na.rm = TRUE)
  }
  if (!is.null(crs)) r <- terra::project(r, crs, method = "near")
  names(r) <- "mapbiomas_class"
  r
}

# Fast, display-only read: GDAL decimates on read via `-outsize` (using the
# raster overviews when present), avoiding a native-resolution stream over a
# large window. Used only for the map overlay - never for area statistics.
#' @keywords internal
#' @noRd
.mb_display_read <- function(aoi, year, collection, src, max_pixels = 600) {
  if (!requireNamespace("terra", quietly = TRUE) ||
      !requireNamespace("sf", quietly = TRUE))
    stop("Packages 'terra' and 'sf' are required.", call. = FALSE)
  aoi <- sf::st_geometry(aoi)
  if (is.na(sf::st_crs(aoi))) sf::st_crs(aoi) <- 4326
  if (is.null(src)) src <- mb_source_url(year, collection)
  path <- if (grepl("^https?://", src)) paste0("/vsicurl/", src) else src

  old <- .mb_set_gdal()
  on.exit(for (k in names(old)) terra::setGDALconfig(k, old[[k]]), add = TRUE)

  r0    <- terra::rast(path)                       # metadata only (no pixels)
  aoi_r <- sf::st_transform(aoi, terra::crs(r0))
  bb    <- as.numeric(sf::st_bbox(aoi_r))          # xmin ymin xmax ymax
  # cap the output width at the window's native width to avoid upsampling
  win_w <- max(1L, as.integer(ceiling((bb[3] - bb[1]) / terra::xres(r0))))
  out_w <- min(as.integer(max_pixels), win_w)

  tmp <- tempfile(fileext = ".tif")
  on.exit(unlink(tmp), add = TRUE)
  sf::gdal_utils(
    "translate", source = path, destination = tmp,
    options = c("-projwin", sprintf("%.10f", bb[1]), sprintf("%.10f", bb[4]),
                sprintf("%.10f", bb[3]), sprintf("%.10f", bb[2]),
                "-outsize", as.character(out_w), "0", "-r", "nearest"))
  rc <- terra::rast(tmp)
  rc <- terra::mask(rc, terra::vect(aoi_r)) + 0L   # detach from the temp file
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