#' Export EOO and AOO as spatial files (shapefile or GeoPackage)
#'
#' Writes the Extent of Occurrence (EOO) polygons and the Area of Occupancy
#' (AOO) polygons produced by [assess_species()] to disk, with the area,
#' habitat-conversion and provisional-category attributes attached to each
#' feature (one feature per species by default). Handy for opening the results
#' in QGIS/ArcGIS or sharing them.
#'
#' Because the ESRI Shapefile format limits field names to 10 characters, the
#' attribute fields use compact names (see the Attribute fields section). A
#' GeoPackage (`format = "gpkg"`) stores both ranges as two layers (`eoo`,
#' `aoo`) inside a single file and has no such limitation.
#'
#' @param assessment A `geoconv_assessment` object from [assess_species()].
#' @param dir Output directory (created if it does not exist). Default `"."`.
#' @param layer_prefix File/layer name prefix. Default `"mappingAS"`.
#' @param what Which ranges to export: `"both"` (default), `"eoo"` or `"aoo"`.
#' @param format `"shapefile"` (default; one file set per range, i.e.
#'   `*_EOO.shp` and `*_AOO.shp`) or `"gpkg"` (a single GeoPackage holding both
#'   layers).
#' @param crs Output CRS (default `4326`, WGS84). Anything accepted by
#'   [sf::st_crs()] also works (an EPSG code or a proj string).
#' @param aoo_as `"union"` (default; one dissolved feature per species) or
#'   `"cells"` (one feature per occupied 2-km grid cell).
#' @param class_csv If `TRUE` (default), also writes `<prefix>_classes.csv` with
#'   the full per-class MapBiomas breakdown (every class) for the EOO and AOO of
#'   each species; included in the zip when `zip = TRUE`. Skipped if MapBiomas
#'   was not computed.
#' @param zip If `TRUE`, the written files are bundled into a single `.zip`
#'   inside `dir` and the path to that zip is returned (most useful for the
#'   shapefile format, which has several sidecar files). Default `FALSE`.
#' @param quiet Passed to [sf::st_write()]. Default `TRUE`.
#'
#' @section Attribute fields:
#' `species`; `eoo_km2` or `aoo_km2` (and `n_cells` for AOO); `conv_pct`
#' (percent converted/anthropic within that range); `nat_pct` (percent natural);
#' `cat_B1` or `cat_B2` (provisional size-only category); `prov_cat`; `mb_year`;
#' `mb_coll` (MapBiomas year and collection). Conversion fields are `NA` when
#' MapBiomas was not computed.
#'
#' @return (Invisibly) a character vector with the file paths written, or the
#'   path to the `.zip` when `zip = TRUE`.
#'
#' @examples
#' \dontrun{
#' occ <- read_occurrences(system.file("extdata", "example_occurrences.csv",
#'                                     package = "mappingAS"))
#' res <- assess_species(occ, mapbiomas = FALSE)
#'
#' # Two shapefiles (EOO + AOO) in the current directory:
#' export_ranges(res)
#'
#' # A single GeoPackage with both layers:
#' export_ranges(res, format = "gpkg")
#'
#' # Everything bundled into one .zip (good for downloads):
#' export_ranges(res, zip = TRUE)
#' }
#' @export
export_ranges <- function(assessment, dir = ".", layer_prefix = "mappingAS",
                          what = c("both", "eoo", "aoo"),
                          format = c("shapefile", "gpkg"),
                          crs = 4326, aoo_as = c("union", "cells"),
                          class_csv = TRUE, zip = FALSE, quiet = TRUE) {
  if (!inherits(assessment, "geoconv_assessment"))
    stop("`assessment` must be a geoconv_assessment object (from assess_species()).",
         call. = FALSE)
  what   <- match.arg(what)
  format <- match.arg(format)
  aoo_as <- match.arg(aoo_as)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  eoo_sf <- if (what %in% c("both", "eoo")) .build_eoo_sf(assessment, crs) else NULL
  aoo_sf <- if (what %in% c("both", "aoo")) .build_aoo_sf(assessment, crs, aoo_as) else NULL
  if (is.null(eoo_sf) && is.null(aoo_sf))
    stop("Nothing to export (no EOO/AOO geometries available).", call. = FALSE)

  written <- character(0)

  if (identical(format, "gpkg")) {
    gpkg <- file.path(dir, paste0(layer_prefix, ".gpkg"))
    if (file.exists(gpkg)) file.remove(gpkg)
    if (!is.null(eoo_sf))
      sf::st_write(eoo_sf, gpkg, layer = "eoo", delete_dsn = FALSE, quiet = quiet)
    if (!is.null(aoo_sf))
      sf::st_write(aoo_sf, gpkg, layer = "aoo", append = TRUE, quiet = quiet)
    written <- gpkg
  } else {
    if (!is.null(eoo_sf))
      written <- c(written,
                   .write_shp(eoo_sf, file.path(dir, paste0(layer_prefix, "_EOO.shp")), quiet))
    if (!is.null(aoo_sf))
      written <- c(written,
                   .write_shp(aoo_sf, file.path(dir, paste0(layer_prefix, "_AOO.shp")), quiet))
  }

  if (isTRUE(class_csv)) {
    ct <- tryCatch(suppressWarnings(class_table(assessment, range = "both")),
                   error = function(e) data.frame())
    if (nrow(ct)) {
      csv <- file.path(dir, paste0(layer_prefix, "_classes.csv"))
      utils::write.csv(ct, csv, row.names = FALSE, fileEncoding = "UTF-8")
      written <- c(written, csv)
    }
  }

  if (isTRUE(zip)) {
    zip_path <- file.path(dir, paste0(layer_prefix, "_ranges.zip"))
    if (file.exists(zip_path)) file.remove(zip_path)
    return(invisible(.make_zip(zip_path, written)))
  }
  invisible(written)
}

# ---- internal helpers -------------------------------------------------------

#' @keywords internal
#' @noRd
.write_shp <- function(x, shp, quiet = TRUE) {
  for (f in .shp_siblings(shp)) if (file.exists(f)) file.remove(f)
  sf::st_write(x, shp, append = FALSE, quiet = quiet)
  .shp_siblings(shp)[file.exists(.shp_siblings(shp))]
}

#' @keywords internal
#' @noRd
.shp_siblings <- function(shp) {
  base <- tools::file_path_sans_ext(shp)
  paste0(base, ".", c("shp", "shx", "dbf", "prj", "cpg"))
}

#' @keywords internal
#' @noRd
.same_crs <- function(a, b) {
  ca <- tryCatch(sf::st_crs(a), error = function(e) NA)
  cb <- tryCatch(sf::st_crs(b), error = function(e) NA)
  isTRUE(ca == cb)
}

#' @keywords internal
#' @noRd
.attr_row <- function(assessment, species, range = c("eoo", "aoo")) {
  range <- match.arg(range)
  s <- assessment$summary
  r <- s[s$species == species, , drop = FALSE][1, , drop = FALSE]
  if (identical(range, "eoo")) {
    data.frame(
      species  = species,
      eoo_km2  = r$eoo_km2,
      conv_pct = r$eoo_converted_pct,
      nat_pct  = r$eoo_natural_pct,
      cat_B1   = r$eoo_cat_B1,
      prov_cat = r$provisional_cat,
      mb_year  = r$mapbiomas_year,
      mb_coll  = r$mapbiomas_collection,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      species  = species,
      aoo_km2  = r$aoo_km2,
      n_cells  = r$aoo_cells,
      conv_pct = r$aoo_converted_pct,
      nat_pct  = r$aoo_natural_pct,
      cat_B2   = r$aoo_cat_B2,
      prov_cat = r$provisional_cat,
      mb_year  = r$mapbiomas_year,
      mb_coll  = r$mapbiomas_collection,
      stringsAsFactors = FALSE
    )
  }
}

#' @keywords internal
#' @noRd
.build_eoo_sf <- function(assessment, crs) {
  d <- assessment$detail
  geoms <- list(); rows <- list(); k <- 0L
  for (s in names(d)) {
    h <- d[[s]]$eoo$hull
    if (is.null(h) || length(h) == 0) next
    k <- k + 1L
    geoms[[k]] <- sf::st_geometry(h)[[1]]
    rows[[k]]  <- .attr_row(assessment, s, "eoo")
  }
  if (k == 0L) {
    warning("No EOO polygons to export (species with < 3 unique points).",
            call. = FALSE)
    return(NULL)
  }
  g <- sf::st_sfc(geoms, crs = 4326)
  out <- sf::st_sf(do.call(rbind, rows), geometry = g)
  if (!.same_crs(crs, 4326)) out <- sf::st_transform(out, crs)
  out
}

#' @keywords internal
#' @noRd
.build_aoo_sf <- function(assessment, crs, aoo_as) {
  d <- assessment$detail
  geoms <- list(); rows <- list(); k <- 0L

  if (identical(aoo_as, "cells")) {
    for (s in names(d)) {
      cl <- d[[s]]$aoo$cells
      if (is.null(cl) || length(cl) == 0) next
      for (j in seq_along(cl)) {
        k <- k + 1L
        geoms[[k]] <- cl[[j]]
        rows[[k]]  <- .attr_row(assessment, s, "aoo")
      }
    }
    if (k == 0L) { warning("No AOO cells to export.", call. = FALSE); return(NULL) }
    g <- sf::st_sfc(geoms, crs = 4326)
    out <- sf::st_sf(do.call(rbind, rows), geometry = g)
  } else {
    for (s in names(d)) {
      cl <- d[[s]]$aoo$cells
      if (is.null(cl) || length(cl) == 0) next
      k <- k + 1L
      geoms[[k]] <- sf::st_union(cl)         # one (multi)polygon per species
      rows[[k]]  <- .attr_row(assessment, s, "aoo")
    }
    if (k == 0L) { warning("No AOO geometries to export.", call. = FALSE); return(NULL) }
    g <- do.call(c, geoms)                   # concatenate sfc parts
    g <- sf::st_cast(g, "MULTIPOLYGON")
    out <- sf::st_sf(do.call(rbind, rows), geometry = g)
  }
  if (!.same_crs(crs, 4326)) out <- sf::st_transform(out, crs)
  out
}

#' @keywords internal
#' @noRd
.make_zip <- function(zip_path, files) {
  files <- files[file.exists(files)]
  if (!length(files)) stop("No files available to zip.", call. = FALSE)
  wd <- dirname(zip_path)
  old <- setwd(wd); on.exit(setwd(old), add = TRUE)
  zf <- basename(zip_path); bn <- basename(files)

  ok <- FALSE
  try({ suppressWarnings(utils::zip(zf, bn, flags = "-q")); ok <- file.exists(zf) },
      silent = TRUE)
  if (!ok && requireNamespace("zip", quietly = TRUE))
    try({ zip::zip(zf, bn); ok <- file.exists(zf) }, silent = TRUE)
  if (!ok)
    stop("Could not create the .zip. Install the 'zip' R package, or export as ",
         "GeoPackage (format = 'gpkg').", call. = FALSE)
  normalizePath(file.path(wd, zf))
}
