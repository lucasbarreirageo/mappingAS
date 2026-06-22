#' Build the MapBiomas national GeoTIFF URL for a given year (any country)
#'
#' Returns the public Google Cloud Storage URL of a MapBiomas annual LULC
#' mosaic. Brazil keeps its historical default; the other South-American
#' initiatives (Argentina, Bolivia, Ecuador, Peru) are resolved through the
#' country registry (see \code{\link{mb_country_info}}). Collection 10 covers
#' 1985-2024 for Brazil.
#'
#' @param year Integer year (1985-2024).
#' @param collection Integer collection number. Ignored when \code{country} is a
#'   non-Brazil key (the registry pins each country's collection); kept for
#'   backward compatibility with Brazil calls (default \code{10}).
#' @param country Country name or ISO-2 alias (default \code{"brazil"}).
#' @return A length-1 character URL.
#' @examples
#' mb_source_url(2024)                      # Brazil
#' mb_source_url(2020, country = "peru")    # Peru, Collection 3
#' @export
mb_source_url <- function(year = 2024, collection = 10, country = "brazil") {
  key <- .mb_country_aliases(country)
  if (identical(key, "brazil")) {
    return(sprintf(
      paste0("https://storage.googleapis.com/mapbiomas-public/initiatives/",
             "brasil/collection_%d/lulc/coverage/brazil_coverage_%d.tif"),
      as.integer(collection), as.integer(year)))
  }
  info <- mb_country_info(key)
  .assert_mb_year(year, info)
  sprintf(info$url_template, as.integer(year))
}

#' @keywords internal
#' @noRd
.assert_mb_year <- function(year, info) {
  y <- as.integer(year)
  if (is.na(y) || y < info$year_min || y > info$year_max) {
    stop(sprintf("Year %s is outside the %s range (%d-%d).",
                 year, info$country, info$year_min, info$year_max),
         call. = FALSE)
  }
  invisible(TRUE)
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
#' @keywords internal
#' @noRd
.mb_raster_display <- function(aoi, year, collection, src, max_pixels = 600,
                               crs = NULL, cache = TRUE) {
  r <- mb_raster_local(aoi, year = year, collection = collection, src = src,
                       mask = TRUE, cache = cache)
  d <- max(dim(r)[1:2])
  if (d > max_pixels) {
    r <- terra::aggregate(r, fact = ceiling(d / max_pixels),
                          fun = "modal", na.rm = TRUE)
  }
  if (!is.null(crs)) r <- terra::project(r, crs, method = "near")
  names(r) <- "mapbiomas_class"
  r
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

## ===========================================================================
## Multi-country extensions
## ===========================================================================

#' Crop a MapBiomas LULC raster to an AOI for ONE country (harmonised)
#'
#' Country-aware wrapper over \code{\link{mb_raster_local}}. It streams only the
#' AOI window via \code{/vsicurl/} (reusing the on-disk cache), then reclassifies
#' foreign pixel codes to the Brazil vocabulary (see
#' \code{\link{mb_harmonise_raster}}) so all downstream summaries work unchanged.
#' For \code{country = "brazil"} it is identical to \code{mb_raster_local()}.
#'
#' @inheritParams mb_raster_local
#' @param country Country name/alias (default \code{"brazil"}).
#' @return A \pkg{terra} \code{SpatRaster} of Brazil-harmonised codes over the AOI.
#' @examples
#' \dontrun{
#' r <- mb_raster_country(eoo_hull, year = 2020, country = "peru")
#' }
#' @export
mb_raster_country <- function(aoi, year = 2024, country = "brazil",
                              src = NULL, mask = TRUE,
                              cache = TRUE, cache_dir = NULL) {
  key <- .mb_country_aliases(country)
  info <- mb_country_info(key)
  if (is.null(src)) src <- mb_source_url(year, info$collection, key)
  r <- mb_raster_local(aoi, year = year, collection = info$collection,
                       src = src, mask = mask, cache = cache,
                       cache_dir = cache_dir)
  mb_harmonise_raster(r, key)
}

#' Crop and MOSAIC MapBiomas LULC across one or more countries
#'
#' The multi-country entry point used by \code{\link{assess_species}}. Given an
#' AOI and the set of countries it spans, it reads each country's harmonised
#' window (Brazil vocabulary) and merges them into a single seamless raster
#' covering the whole AOI. Single-country AOIs short-circuit to one read.
#'
#' Per-country failures (e.g. a year missing on one server, or a transient
#' network error) are warned and skipped; the mosaic is built from whatever
#' succeeded, so a transboundary species is not lost because one tile failed.
#'
#' @param aoi An \code{sf}/\code{sfc} polygon (e.g. an EOO hull or AOO union).
#' @param countries Character vector of country keys (from
#'   \code{\link{species_countries}}). If \code{NULL} or empty, defaults to
#'   \code{"brazil"}.
#' @param year Integer year (default \code{2024}).
#' @param src Optional named list of per-country GeoTIFF overrides
#'   (e.g. \code{list(peru = "/path/peru_2020.tif")}); mainly for tests/offline.
#' @param mask,cache,cache_dir Passed through to the per-country reader.
#' @param verbose Logical; print per-country progress (default \code{TRUE}).
#' @return A \pkg{terra} \code{SpatRaster} of Brazil-harmonised codes spanning
#'   the AOI (a mosaic when more than one country contributes).
#' @examples
#' \dontrun{
#' # species spanning Peru and Bolivia
#' r <- mb_raster_multicountry(aoo_union, countries = c("peru", "bolivia"),
#'                             year = 2022)
#' }
#' @export
mb_raster_multicountry <- function(aoi, countries = NULL, year = 2024,
                                   src = NULL, mask = TRUE, cache = TRUE,
                                   cache_dir = NULL, verbose = TRUE) {
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' is required.", call. = FALSE)
  if (is.null(countries) || !length(countries)) countries <- "brazil"
  countries <- unique(vapply(countries, .mb_country_aliases, character(1)))

  read_one <- function(k) {
    s <- if (is.list(src)) src[[k]] else NULL
    tryCatch(
      mb_raster_country(aoi, year = year, country = k, src = s,
                        mask = mask, cache = cache, cache_dir = cache_dir),
      error = function(e) {
        warning(sprintf("MapBiomas %s (%s) skipped: %s",
                        k, year, conditionMessage(e)), call. = FALSE)
        NULL
      })
  }

  if (length(countries) == 1L) {
    if (verbose) message("  country: ", countries)
    r <- read_one(countries)
    if (is.null(r)) stop("No MapBiomas raster could be read for the AOI.",
                         call. = FALSE)
    return(r)
  }

  rasters <- list()
  for (k in countries) {
    if (verbose) message("  country: ", k)
    rk <- read_one(k)
    if (!is.null(rk)) rasters[[k]] <- rk
  }
  if (!length(rasters))
    stop("No MapBiomas raster could be read for any country in the AOI.",
         call. = FALSE)
  if (length(rasters) == 1L) return(rasters[[1]])

  if (verbose) message("  mosaicking ", length(rasters), " countries...")
  .mb_mosaic(rasters)
}

#' @keywords internal
#' @noRd
## Merge harmonised per-country crops into one raster. Countries use different
## native grids/resolutions; align everyone to the first tile before merging so
## terra::merge() has a common geometry. first=TRUE keeps the leading country's
## pixel where footprints overlap at the border (deterministic; no averaging of
## class codes, which would be meaningless for categorical data).
.mb_mosaic <- function(rasters) {
  ref <- rasters[[1]]
  aligned <- vector("list", length(rasters))
  aligned[[1]] <- ref
  for (i in seq_along(rasters)[-1]) {
    ri <- rasters[[i]]
    same <- tryCatch(
      terra::compareGeom(ref, ri, stopOnError = FALSE, messages = FALSE),
      error = function(e) FALSE)
    if (!isTRUE(same)) ri <- terra::project(ri, ref, method = "near")
    aligned[[i]] <- ri
  }
  m <- do.call(terra::merge, c(aligned, list(first = TRUE)))
  names(m) <- "mapbiomas_class"
  m
}