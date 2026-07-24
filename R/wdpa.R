#' Default WDPA ArcGIS FeatureServer query endpoint
#'
#' The public ArcGIS \code{FeatureServer} layer that serves the World Database
#' on Protected Areas (WDPA) polygons, used by [wdpa_areas()] as a global
#' alternative to the Brazil-only ICMBio Conservation Units. This is the layer
#' behind the ArcGIS Living Atlas "WDPA" item and returns GeoJSON on demand, so
#' no Google Earth Engine or bulk download is required.
#'
#' @return A length-1 character URL (the \code{/query} endpoint).
#' @seealso <https://www.protectedplanet.net>, [wdpa_areas()]
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

  pa <- sf::st_make_valid(sf::st_transform(pa, 4326))
  pa <- .pa_standardise_wdpa(pa)
  if (nrow(pa) && !is.null(f))
    tryCatch(sf::st_write(pa, f, quiet = TRUE, append = FALSE),
             error = function(e) NULL)
  pa
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
      f = "geojson")
    paste0(url, "?", paste(names(q), q, sep = "=", collapse = "&"))
  }

  parts <- list(); i <- 0L; offset <- 0L
  repeat {
    i <- i + 1L
    chunk <- tryCatch(sf::st_read(build(offset), quiet = quiet),
                      error = function(e) NULL)
    if (is.null(chunk)) {
      if (i == 1L)
        stop("Could not read the WDPA FeatureServer. Check the connection or ",
             "use pa_source = 'icmbio' / a local `pa_src` file.", call. = FALSE)
      break
    }
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
