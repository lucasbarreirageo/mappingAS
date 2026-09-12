#' Parse one degrees/minutes/seconds coordinate typed as free text
#'
#' Converts a single latitude or longitude written in sexagesimal form into a
#' decimal degree. It accepts the usual spellings, e.g. \code{"23 27 30 S"},
#' \code{"23°27'30\"S"}, \code{"-23 27 30"} or a bare \code{"23.4583"}.
#' The hemisphere letter (N/S/E/W) or a leading minus sign sets the sign; S and
#' W are negative. Minutes and seconds default to zero when absent.
#'
#' @param x A length-one character string (or anything coercible with
#'   \code{as.character}). Empty or non-numeric input returns \code{NA}.
#' @return A single numeric decimal degree, or \code{NA_real_}.
#' @keywords internal
#' @noRd
.parse_dms <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) return(NA_real_)
  hemi <- toupper(gsub("[^NSEWnsew]", "", x))
  neg_sign <- grepl("^\\s*-", x)
  nums <- suppressWarnings(as.numeric(
    regmatches(x, gregexpr("[0-9]+(?:\\.[0-9]+)?", x))[[1]]))
  nums <- nums[is.finite(nums)]
  if (!length(nums)) return(NA_real_)
  deg <- nums[1]
  mn  <- if (length(nums) >= 2) nums[2] else 0
  sec <- if (length(nums) >= 3) nums[3] else 0
  val <- abs(deg) + mn / 60 + sec / 3600
  if ((nzchar(hemi) && hemi %in% c("S", "W")) || neg_sign) val <- -val
  val
}

#' Convert a UTM easting/northing to lon/lat (WGS84)
#'
#' Reprojects a projected UTM coordinate (metres) in a given zone and hemisphere
#' to geographic longitude/latitude on WGS84, using the matching EPSG code
#' (326xx for the northern hemisphere, 327xx for the southern).
#'
#' @param easting,northing UTM coordinates in metres.
#' @param zone Integer UTM zone (1-60).
#' @param hemi Hemisphere, \code{"S"} (default) or \code{"N"}.
#' @return A length-two numeric vector \code{c(lon, lat)}, or \code{c(NA, NA)}
#'   when the inputs are incomplete/invalid or the reprojection fails.
#' @keywords internal
#' @noRd
.utm_to_lonlat <- function(easting, northing, zone, hemi = "S") {
  easting  <- suppressWarnings(as.numeric(easting))
  northing <- suppressWarnings(as.numeric(northing))
  zone     <- suppressWarnings(as.integer(zone))
  if (!is.finite(easting) || !is.finite(northing) ||
      is.na(zone) || zone < 1 || zone > 60) return(c(NA_real_, NA_real_))
  epsg <- (if (identical(toupper(hemi %||% "S"), "S")) 32700L else 32600L) + zone
  tryCatch({
    p  <- sf::st_sfc(sf::st_point(c(easting, northing)), crs = epsg)
    ll <- sf::st_coordinates(sf::st_transform(p, 4326))
    c(ll[1, 1], ll[1, 2])
  }, error = function(e) c(NA_real_, NA_real_))
}
