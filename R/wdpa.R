#' Default WDPA ArcGIS FeatureServer query endpoint
#'
#' The public ArcGIS \code{FeatureServer} layer that serves the World Database
#' on Protected Areas (WDPA) polygons, used by [wdpa_areas()] as a global
#' alternative to the Brazil-only ICMBio Conservation Units. This is the layer
#' behind the ArcGIS Living Atlas "WDPA" item and returns GeoJSON on demand, so
#' no Google Earth Engine or bulk download is required.
#'
#' @return A length-1 character URL (the \code{/query} endpoint).
#' @seealso <https://www.protectedplanet.net/en>, [wdpa_areas()]
#' @export
wdpa_query_url <- function() {
  paste0("https://services5.arcgis.com/Mj0hjvkNtV7NRhA7/arcgis/rest/services/",
         "WDPA_v0/FeatureServer/1/query")
}

#' Read World Database on Protected Areas polygons intersecting an area
#'
#' Queries the public WDPA ArcGIS \code{FeatureServer} for the protected-area
#' polygons whose envelope intersects the bounding box of \code{aoi} and returns
#' them as an \code{sf} in WGS84 with the same three standardised columns used by
#' [protected_areas()]: \code{pa_name} (WDPA \code{NAME}), \code{pa_category}
#' (designation / IUCN category) and \code{pa_group} (\emph{Strict protection}
#' for IUCN Ia-III, \emph{Sustainable use} for IUCN IV-VI, else the reported
#' status), plus all original WDPA attributes.
#'
#' Only the AOI's bounding box is requested (server-side envelope filter) and the
#' server's transfer limit is followed with pagination, so this stays light even
#' though WDPA is a global dataset. Results are cached on disk by bounding box.
#' This is the recommended protected-area source for the \code{"amazonia"} and
#' \code{"colombia"} initiatives (and anywhere outside Brazil), where the ICMBio
#' service does not apply.
#'
#' @param aoi An \code{sf}/\code{sfc} polygon or point set (any CRS).
#' @param url WDPA query endpoint (default [wdpa_query_url()]).
#' @param marine One of \code{"all"} (default), \code{"terrestrial"} (drop purely
#'   marine PAs, \code{MARINE = 2}) - useful for terrestrial species screening.
#' @param cache,cache_dir Cache the result on disk (default \code{TRUE}; folder
#'   \code{mappingAS_pa_cache} under \code{tempdir()}).
#' @param quiet Passed to [sf::st_read()] (default \code{TRUE}).
#' @return An \code{sf} of WDPA polygons in EPSG:4326 (possibly zero rows), or
#'   \code{NULL} if the service could not be read.
#' @examples
#' \dontrun{
#' occ <- read_occurrences(system.file("extdata", "example_occurrences.csv",
#'                                     package = "mappingAS"))
#' sp1 <- occ[occ$species == occ$species[1], ]
#' pas <- wdpa_areas(calc_eoo(sp1)$hull)
#' unique(pas$pa_name)
#' }
#' @export
wdpa_areas <- function(aoi, url = wdpa_query_url(),
                       marine = c("all", "terrestrial"),
                       cache = TRUE, cache_dir = NULL, quiet = TRUE) {
  marine <- match.arg(marine)
  g <- sf::st_geometry(aoi)
  if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
  g <- g[!sf::st_is_empty(g)]
  if (length(g) == 0)
    stop("The area of interest has no (non-empty) geometry to query WDPA with.",
         call. = FALSE)
  bb <- sf::st_bbox(sf::st_transform(g, 4326))

  key <- paste0("wdpa__", marine, "__",
                paste(round(as.numeric(bb), 4), collapse = "_"))
  f <- .pa_cache_file(cache, cache_dir, key)
  if (!is.null(f) && file.exists(f))
    return(tryCatch(sf::st_read(f, quiet = TRUE), error = function(e) NULL))

  where <- if (marine == "terrestrial") "MARINE<>2" else "1=1"
  geom <- sprintf("%f,%f,%f,%f",
                  bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])

  pa <- .wdpa_fetch(url, where, geom, quiet)
  if (is.null(pa)) return(NULL)
  if (!nrow(pa)) return(pa)

  # GeoJSON served by the ArcGIS FeatureServer is always WGS84 (EPSG:4326), but
  # some GDAL/sf builds leave the CRS unset on read; assigning it prevents the
  # "cannot transform sfc object with missing crs" failure downstream.
  if (is.na(sf::st_crs(pa))) sf::st_crs(pa) <- 4326
  pa <- sf::st_make_valid(sf::st_transform(pa, 4326))
  pa <- .pa_standardise_wdpa(pa)
  if (nrow(pa) && !is.null(f))
    tryCatch(sf::st_write(pa, f, quiet = TRUE, append = FALSE),
             error = function(e) NULL)
  pa
}

#' Read one WDPA query page as an `sf`.
#'
#' Downloads the query result with R's own HTTP stack (\pkg{curl} when available,
#' otherwise \code{utils::download.file()}) to a temporary file and reads that
#' file. This is deliberate: many GDAL builds - notably several Windows / older
#' installs - lack the \code{/vsicurl} HTTP support that \code{sf::st_read()}
#' needs to open a URL directly, so reading the URL in-place silently returns
#' nothing there while working on Linux/CI. Downloading first makes the query
#' portable across platforms. Direct URL reading is kept as a fallback.
#' @keywords internal
#' @noRd
.wdpa_read_query <- function(qurl, quiet) {
  # Save as ".json" (not ".geojson"): the service returns ESRI JSON, and the
  # ".geojson" extension would force GDAL's GeoJSON driver, which rejects it with
  # "Missing 'features' member". A ".json" file lets the ESRIJSON driver claim it.
  tf <- tempfile(fileext = ".json")
  on.exit(unlink(tf), add = TRUE)
  dl_ok <- tryCatch({
    if (requireNamespace("curl", quietly = TRUE)) {
      curl::curl_download(qurl, tf, quiet = TRUE, mode = "wb")
    } else {
      suppressWarnings(utils::download.file(qurl, tf, quiet = TRUE, mode = "wb"))
    }
    file.exists(tf) && file.size(tf) > 0
  }, error = function(e) FALSE)

  # ESRI JSON uses clockwise outer rings and counter-clockwise holes, so tell
  # GDAL to assume that (ONLY_CCW) instead of running the slow O(n^2) ring-
  # containment analysis that emits "organizePolygons() received a polygon with
  # more than 100 parts. The processing may be really slow." on large protected
  # areas. This is both faster and correct for ESRI-sourced polygons; any residue
  # is cleaned by st_make_valid() upstream.
  old <- Sys.getenv("OGR_ORGANIZE_POLYGONS", unset = NA_character_)
  Sys.setenv(OGR_ORGANIZE_POLYGONS = "ONLY_CCW")
  on.exit(
    if (is.na(old)) Sys.unsetenv("OGR_ORGANIZE_POLYGONS")
    else Sys.setenv(OGR_ORGANIZE_POLYGONS = old),
    add = TRUE)

  read_quiet <- function(x)
    suppressWarnings(tryCatch(sf::st_read(x, quiet = quiet),
                              error = function(e) NULL))

  out <- NULL
  if (isTRUE(dl_ok)) out <- read_quiet(tf)
  # Fallback: let GDAL read the URL directly (works where /vsicurl is available).
  if (is.null(out)) out <- read_quiet(qurl)
  out
}

#' @keywords internal
#' @noRd
.wdpa_fetch <- function(url, where, geom, quiet, page = 1000L, max_pages = 25L) {
  build <- function(offset) {
    q <- c(
      where = utils::URLencode(where, TRUE),
      geometry = utils::URLencode(geom, TRUE),
      geometryType = "esriGeometryEnvelope",
      inSR = "4326", outSR = "4326",
      spatialRel = "esriSpatialRelIntersects",
      outFields = "*", returnGeometry = "true",
      resultOffset = as.character(offset),
      resultRecordCount = as.character(page),
      # This FeatureServer advertises supportedQueryFormats = "JSON" only, so the
      # /query endpoint rejects f=geojson with HTTP 400. Request ESRI JSON (which
      # GDAL's ESRIJSON driver reads) instead.
      f = "json")
    paste0(url, "?", paste(names(q), q, sep = "=", collapse = "&"))
  }

  parts <- list(); i <- 0L; offset <- 0L
  repeat {
    i <- i + 1L
    chunk <- .wdpa_read_query(build(offset), quiet)
    # A non-spatial response (e.g. an ArcGIS error payload parsed as a plain
    # table) has no geometry column: treat it as a read failure, not as data.
    if (!is.null(chunk) &&
        (!inherits(chunk, "sf") || is.null(attr(chunk, "sf_column"))))
      chunk <- NULL
    if (is.null(chunk)) {
      if (i == 1L)
        stop("Could not read the WDPA service. Check the internet connection ",
             "(or GDAL's URL support) or upload a local protected-area file ",
             "instead.", call. = FALSE)
      break
    }
    # GeoJSON is EPSG:4326 by definition; some readers leave the CRS unset.
    if (is.na(sf::st_crs(chunk))) sf::st_crs(chunk) <- 4326
    # Drop features that came back without a geometry so they cannot poison the
    # later bounding-box / transform steps.
    chunk <- chunk[!sf::st_is_empty(sf::st_geometry(chunk)), , drop = FALSE]
    if (nrow(chunk)) parts[[i]] <- chunk
    if (nrow(chunk) < page || i >= max_pages) break
    offset <- offset + page
  }
  if (!length(parts))
    return(sf::st_sf(pa_name = character(0),
                     geometry = sf::st_sfc(crs = 4326)))
  do.call(rbind, parts)
}

#' Standardise WDPA attributes to pa_name / pa_category / pa_group
#' @keywords internal
#' @noRd
.pa_standardise_wdpa <- function(pa) {
  nms <- names(pa)
  nm <- .match_col(nms, c("NAME", "name", "ORIG_NAME"))
  ds <- .match_col(nms, c("DESIG_ENG", "DESIG", "desig_eng", "desig"))
  ic <- .match_col(nms, c("IUCN_CAT", "iucn_cat"))

  pa$pa_name <- if (!is.na(nm)) as.character(pa[[nm]])
                else paste0("WDPA_", seq_len(nrow(pa)))
  cat_desig <- if (!is.na(ds)) as.character(pa[[ds]]) else NA_character_
  iucn <- if (!is.na(ic)) toupper(trimws(as.character(pa[[ic]]))) else NA_character_
  pa$pa_category <- ifelse(is.na(cat_desig) | cat_desig == "",
                           iucn, cat_desig)
  pa$pa_group <- .wdpa_group(iucn)
  pa
}

#' Map WDPA IUCN categories to a strict/sustainable protection group
#' @keywords internal
#' @noRd
.wdpa_group <- function(iucn) {
  strict <- c("IA", "IB", "II", "III")
  sust   <- c("IV", "V", "VI")
  out <- rep("Not reported / applicable", length(iucn))
  out[iucn %in% strict] <- "Strict protection (IUCN Ia-III)"
  out[iucn %in% sust]   <- "Sustainable use (IUCN IV-VI)"
  out
}
