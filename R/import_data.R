#' Read species occurrence points from XLSX, CSV or vector files
#'
#' Imports occurrence records from a spreadsheet (\code{.xlsx}/\code{.xls}),
#' delimited text (\code{.csv}/\code{.tsv}/\code{.txt}) or a vector file
#' (\code{.shp}, \code{.gpkg}, \code{.geojson}; for shapefiles you may also pass
#' a \code{.zip} containing the .shp/.shx/.dbf/.prj set) and returns a clean
#' point \code{sf} in WGS84 (EPSG:4326) with a standardised \code{species}
#' column.
#'
#' Longitude, latitude and species columns are auto-detected from common header
#' names (Darwin Core \code{decimalLongitude}/\code{decimalLatitude},
#' \code{lon}/\code{lat}, \code{x}/\code{y}, \code{scientificName}, etc.). You can
#' override the detection with the corresponding arguments. Rows with missing or
#' out-of-range coordinates are dropped with a message.
#'
#' For delimited text the field separator and decimal mark are auto-detected, so
#' Brazilian-style CSVs exported from Excel (\code{;} separator, \code{,}
#' decimals, optional UTF-8 BOM) are read correctly without extra arguments.
#'
#' @param path Path to the input file.
#' @param species_col,lon_col,lat_col Optional column names to override
#'   auto-detection. \code{lon_col}/\code{lat_col} are ignored for vector files
#'   that already carry geometry.
#' @param crs Input CRS for tabular coordinates (default \code{4326}). Output is
#'   always reprojected to WGS84.
#' @param sheet For Excel inputs, the sheet name or index (default first sheet).
#' @param sep For delimited text, the field separator. If \code{NULL} (default)
#'   it is auto-detected from the header line (\code{;}, \code{,} or tab).
#' @param dec Decimal mark for delimited text. If \code{NULL} (default) it is
#'   inferred from \code{sep} (\code{,} when \code{sep} is \code{;}, else
#'   \code{.}), matching Brazilian Excel exports.
#' @param encoding File encoding for delimited text (default \code{"UTF-8"};
#'   use e.g. \code{"latin1"} for Windows-1252 files).
#' @param default_species Species label used when no species column is found
#'   (default \code{"sp1"}).
#' @return An \code{sf} of POINT geometries in EPSG:4326 with a \code{species}
#'   column plus all original attributes.
#' @examples
#' f <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
#' occ <- read_occurrences(f)
#' table(occ$species)
#' @export
read_occurrences <- function(path,
                             species_col = NULL,
                             lon_col = NULL,
                             lat_col = NULL,
                             crs = 4326,
                             sheet = NULL,
                             sep = NULL,
                             dec = NULL,
                             encoding = "UTF-8",
                             default_species = "sp1") {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  ext <- tolower(tools::file_ext(path))

  vector_exts <- c("shp", "gpkg", "geojson", "json", "kml", "zip")
  if (ext %in% vector_exts) {
    occ <- .read_vector(path, ext)
    occ <- .standardise_species(occ, species_col, default_species)
    occ <- sf::st_transform(occ, 4326)
  } else {
    df <- .read_table(path, ext, sheet = sheet, sep = sep, dec = dec,
                      file_encoding = encoding)
    occ <- .table_to_sf(df, species_col, lon_col, lat_col, crs, default_species)
  }

  .assert_points(occ)
  occ <- occ[!sf::st_is_empty(occ), , drop = FALSE]
  if (nrow(occ) == 0) stop("No valid point records after cleaning.", call. = FALSE)
  occ
}

#' @keywords internal
#' @noRd
.read_table <- function(path, ext, sheet = NULL, sep = NULL, dec = NULL,
                        file_encoding = "UTF-8") {
  if (ext %in% c("xlsx", "xls")) {
    if (is.null(sheet)) sheet <- 1
    return(as.data.frame(readxl::read_excel(path, sheet = sheet)))
  }

  hdr <- readLines(path, n = 1L, warn = FALSE)
  if (!length(hdr)) hdr <- ""
  hdr <- sub("^\ufeff", "", hdr)            # drop UTF-8 BOM before sniffing

  if (is.null(sep)) {
    count <- function(ch) lengths(regmatches(hdr, gregexpr(ch, hdr, fixed = TRUE)))
    cnt <- c(";" = count(";"), "," = count(","), "\t" = count("\t"))
    sep <- if (max(cnt) == 0) "," else names(cnt)[which.max(cnt)]
  }
  if (is.null(dec)) dec <- if (identical(sep, ";")) "," else "."

  # comment.char = "" reproduces read.csv(): a '#' in a field is data, not a comment
  df <- utils::read.table(path, sep = sep, dec = dec, header = TRUE,
                          stringsAsFactors = FALSE, check.names = FALSE,
                          quote = "\"", fill = TRUE, comment.char = "",
                          fileEncoding = file_encoding)
  names(df) <- sub("^\ufeff", "", names(df))  # strip BOM left on first column
  df
}

#' @keywords internal
#' @noRd
.read_vector <- function(path, ext) {
  if (ext == "zip") {
    tmp <- tempfile("mappingAS_shp_")
    dir.create(tmp)
    utils::unzip(path, exdir = tmp)
    shp <- list.files(tmp, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
    if (!length(shp)) stop("No .shp found inside the zip archive.", call. = FALSE)
    return(sf::st_read(shp[1], quiet = TRUE))
  }
  sf::st_read(path, quiet = TRUE)
}

#' @keywords internal
#' @noRd
.table_to_sf <- function(df, species_col, lon_col, lat_col, crs, default_species) {
  cand <- .coord_candidates()
  nms <- names(df)

  if (is.null(lon_col)) lon_col <- .match_col(nms, cand$lon)
  if (is.null(lat_col)) lat_col <- .match_col(nms, cand$lat)
  if (is.na(lon_col) || is.na(lat_col)) {
    stop("Could not auto-detect longitude/latitude columns. Found columns: ",
         paste(nms, collapse = ", "),
         ". Pass lon_col/lat_col explicitly.", call. = FALSE)
  }

  x <- suppressWarnings(as.numeric(df[[lon_col]]))
  y <- suppressWarnings(as.numeric(df[[lat_col]]))
  ok <- is.finite(x) & is.finite(y)

  # basic sanity filter when the working CRS is geographic
  if (isTRUE(crs == 4326)) {
    ok <- ok & x >= -180 & x <= 180 & y >= -90 & y <= 90 & !(x == 0 & y == 0)
  }
  dropped <- sum(!ok)
  if (dropped > 0) {
    message(sprintf("Dropped %d of %d rows with missing/invalid coordinates.",
                    dropped, length(ok)))
  }
  df <- df[ok, , drop = FALSE]
  x <- x[ok]; y <- y[ok]

  if (is.null(species_col)) species_col <- .match_col(names(df), cand$species)
  df$species <- if (!is.na(species_col)) {
    trimws(as.character(df[[species_col]]))
  } else {
    message("No species column detected; assigning '", default_species, "'.")
    rep(default_species, nrow(df))
  }
  df$species[is.na(df$species) | df$species == ""] <- default_species

  occ <- sf::st_as_sf(
    cbind(df, .lon = x, .lat = y),
    coords = c(".lon", ".lat"), crs = crs, remove = TRUE
  )
  sf::st_transform(occ, 4326)
}

#' @keywords internal
#' @noRd
.standardise_species <- function(occ, species_col, default_species) {
  cand <- .coord_candidates()
  if (is.null(species_col)) species_col <- .match_col(names(occ), cand$species)
  if (!is.na(species_col) && species_col %in% names(occ)) {
    occ$species <- trimws(as.character(occ[[species_col]]))
  } else {
    occ$species <- rep(default_species, nrow(occ))
  }
  occ$species[is.na(occ$species) | occ$species == ""] <- default_species
  occ
}
