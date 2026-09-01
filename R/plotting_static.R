#' Publication-ready static map of a species (MapBiomas + EOO + AOO + points)
#'
#' Builds an exportable \pkg{ggplot2} map for one species: the MapBiomas
#' land-cover layer clipped to the EOO (with the official colour legend of the
#' classes actually present, drawn beside the map so it never overlaps the
#' EOO/AOO), the EOO minimum convex polygon, the occupied AOO cells, the
#' occurrence points, plus a north arrow and a scale bar with the geographic
#' reference (datum/projection) printed beneath it. The map is drawn in the
#' species' equal-area projection so the scale bar is meaningful, and is returned
#' as a \code{ggplot} object you can save with \code{ggplot2::ggsave()} to make a
#' usable, georeferenced-looking figure.
#'
#' The MapBiomas layer is read with the cached windowed reader and aggregated to
#' \code{max_pixels} for display, so it is fast and reuses the raster already
#' fetched during \code{\link{assess_species}} (same EOO window).
#'
#' @param assessment A \code{geoconv_assessment} from \code{\link{assess_species}}.
#' @param species Species name (default: first assessed).
#' @param mapbiomas Logical; draw the MapBiomas raster layer (default \code{TRUE}).
#'   Set \code{FALSE} for a quick points/EOO/AOO map with no raster read.
#' @param fire Logical; draw the MapBiomas Fire \emph{frequency} layer (number
#'   of years burned, 1985-2024). With \code{mapbiomas = TRUE} it is overlaid on
#'   the land-cover raster when \pkg{ggnewscale} is installed; otherwise only the
#'   land-cover layer is drawn (default \code{FALSE}).
#' @param src Optional local path / URL to a MapBiomas GeoTIFF (same as in
#'   \code{\link{assess_species}}); \code{NULL} (default) uses the public URL.
#' @param max_pixels Target maximum raster size (longest side, in pixels) for the
#'   display layer (default \code{600}).
#' @param crs Optional CRS for the map (proj string or EPSG code). Defaults to the
#'   species' equal-area projection from the EOO/AOO.
#' @param scalebar,north Logical; add a scale bar / north arrow (default \code{TRUE}).
#' @param title Optional plot title (default: the species name).
#' @param lang Legend language: \code{"pt"} (default) or \code{"en"}. Controls
#'   the language of the MapBiomas class labels, the fire-frequency legend and
#'   the feature labels (EOO/AOO/occurrences).
#' @param clip Geometry the rasters are clipped to: \code{"eoo"} (default, the
#'   EOO hull), \code{"aoo"} (the union of occupied AOO cells) or \code{"all"}
#'   (the union of both). Matches the \code{clip} argument of
#'   \code{\link{map_species}}.
#' @param protected Logical; overlay protected areas on the publishable map
#'   (default \code{FALSE}). Uses the protected-area layer stored by
#'   \code{assess_species(..., protected = TRUE)}, or fetches it on the fly.
#' @param pa_src Optional local protected-area vector file for the overlay
#'   (offline).
#' @param pa_occ_only Logical; when \code{TRUE} (default) draw only the protected
#'   areas that contain occurrences of the species. \code{FALSE} draws every
#'   protected area the range overlaps.
#' @return A \code{ggplot} object.
#' @examples
#' occ <- read_occurrences(system.file("extdata", "example_occurrences.csv",
#'                                     package = "mappingAS"))
#' res <- assess_species(occ, mapbiomas = FALSE, verbose = FALSE)
#' m <- map_static(res, mapbiomas = FALSE)
#' \donttest{
#' # Save it wherever you like (a temporary file here):
#' ggplot2::ggsave(file.path(tempdir(), "eoo_map.png"), m, width = 8, height = 7)
#' }
#' @export
map_static <- function(assessment, species = NULL, mapbiomas = TRUE,
                       fire = FALSE, src = NULL, max_pixels = 600, crs = NULL,
                       scalebar = TRUE, north = TRUE, title = NULL,
                       lang = c("pt", "en"), clip = c("eoo", "aoo", "all"),
                       protected = FALSE, pa_src = NULL, pa_occ_only = TRUE) {
  lang <- match.arg(lang)
  clip <- match.arg(clip)
  stopifnot(inherits(assessment, "geoconv_assessment"))
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for map_static().", call. = FALSE)
  }
  d <- assessment$detail
  if (is.null(species)) species <- names(d)[1]
  obj <- d[[species]]
  if (is.null(obj)) stop("Species not found: ", species, call. = FALSE)
  # Scientific name in the title: genus + epithet italic, author (if any) normal.
  if (is.null(title)) title <- .sp_title_expr(species)

  `%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
  crs <- crs %||% obj$eoo$crs_laea %||% obj$aoo$crs_laea
  if (is.null(crs) || (length(crs) == 1 && is.na(crs))) {
    stop("No projection available (the species has no EOO/AOO); ",
         "cannot draw a scaled map.", call. = FALSE)
  }

  hull  <- if (!is.null(obj$eoo$hull))  sf::st_transform(obj$eoo$hull, crs)  else NULL
  cells <- if (!is.null(obj$aoo$cells)) sf::st_transform(obj$aoo$cells, crs) else NULL
  pts   <- sf::st_transform(sf::st_geometry(obj$points), crs)
  st    <- assessment$settings

  clip_geom <- .clip_geometry(clip, hull, cells)

  g <- ggplot2::ggplot()

  # ggplot2 has a single fill scale: to show LULC *and* fire together we need
  # ggnewscale. Without it, the land-cover layer (the main map) is kept and a
  # warning is emitted, so the publication map never drops MapBiomas silently.
  draw_lulc <- isTRUE(mapbiomas) && !is.null(clip_geom)
  draw_fire <- isTRUE(fire) && !is.null(clip_geom)
  if (draw_lulc && draw_fire && !requireNamespace("ggnewscale", quietly = TRUE)) {
    warning("Both MapBiomas and fire layers requested but 'ggnewscale' is not ",
            "installed: drawing MapBiomas only. Install 'ggnewscale' to overlay ",
            "both.", call. = FALSE)
    draw_fire <- FALSE
  }

  # ---- MapBiomas land-cover raster (clipped to EOO or AOO) ----
  if (draw_lulc) {
    # Use the product actually used for this species (Sentinel-2/Esri fallback
    # when the range fell outside MapBiomas coverage).
    lc_ini  <- obj$initiative %||% st$initiative %||% "brazil"
    lc_year <- obj$year %||% st$year
    lc_coll <- obj$collection %||% st$collection
    rdf <- tryCatch(
      .mb_display_df(clip_geom, lc_year, lc_coll, src, max_pixels, crs, lang,
                     initiative = lc_ini),
      error = function(e) {
        warning("MapBiomas layer skipped: ", conditionMessage(e), call. = FALSE)
        NULL
      })
    if (!is.null(rdf) && nrow(rdf$df) > 0) {
      lbl_clip <- switch(clip, aoo = "AOO", all = "EOO+AOO", "EOO")
      g <- g +
        ggplot2::geom_tile(
          data = rdf$df,
          ggplot2::aes(x = .data[["x"]], y = .data[["y"]], fill = .data[["class"]])) +
        ggplot2::scale_fill_manual(
          values = rdf$cols, drop = FALSE,
          name = sprintf(if (lang == "en") "Land cover %s (in %s)"
                         else "Cobertura %s (no %s)", lc_year, lbl_clip))
    } else {
      draw_lulc <- FALSE
    }
  }

  # ---- MapBiomas Fire frequency raster (clipped to EOO or AOO) ----
  if (draw_fire) {
    fdf <- tryCatch(
      .fire_display_df(clip_geom, st$fire_collection %||% 4, src, max_pixels, crs),
      error = function(e) {
        warning("Fire layer skipped: ", conditionMessage(e), call. = FALSE)
        NULL
      })
    if (!is.null(fdf) && nrow(fdf$df) > 0) {
      if (draw_lulc) g <- g + ggnewscale::new_scale_fill()  # second fill scale
      g <- g +
        ggplot2::geom_tile(
          data = fdf$df,
          ggplot2::aes(x = .data[["x"]], y = .data[["y"]], fill = .data[["class"]])) +
        ggplot2::scale_fill_manual(
          values = fdf$cols, drop = FALSE,
          name = if (lang == "en") "Fire frequency\n(years, 1985-2024)"
                 else "Frequencia de fogo\n(anos, 1985-2024)")
    }
  }

 # ---- vector layers sharing one feature legend (colour aesthetic) ----
  lab_eoo  <- "EOO (MCP)"
  lab_aoo  <- if (lang == "en") "AOO (2 km)" else "AOO (2 km)"
  lab_pts  <- if (lang == "en") "Occurrences" else "Ocorrencias"
  lab_uc   <- if (lang == "en") "Protected areas" else "Areas Protegidas"
  feat <- character(0)
  if (isTRUE(protected)) {
    pa_sf <- obj$pa$layer
    if (is.null(pa_sf) && !is.null(clip_geom))
      pa_sf <- tryCatch(protected_areas(clip_geom, src = pa_src),
                        error = function(e) NULL)
    if (!is.null(pa_sf) && nrow(pa_sf) > 0) {
      pa_sf <- sf::st_transform(pa_sf, 4326)
      if (isTRUE(pa_occ_only) && !is.null(obj$points)) {
        occ <- sf::st_transform(sf::st_geometry(obj$points), 4326)
        pa_sf <- pa_sf[lengths(suppressMessages(
          sf::st_intersects(pa_sf, occ))) > 0, , drop = FALSE]
      }
    }
    if (!is.null(pa_sf) && nrow(pa_sf) > 0) {
      g <- g + ggplot2::geom_sf(data = sf::st_transform(pa_sf, crs),
                                ggplot2::aes(colour = lab_uc),
                                fill = "#1f8d49", alpha = 0.15, linewidth = 0.4)
      feat[lab_uc] <- "#1f8d49"
    }
  }
  if (!is.null(hull)) {
    g <- g + ggplot2::geom_sf(data = hull, ggplot2::aes(colour = lab_eoo),
                              fill = NA, linewidth = 0.7)
    feat[lab_eoo] <- "#1f4e79"
  }
  if (!is.null(cells)) {
    g <- g + ggplot2::geom_sf(data = cells, ggplot2::aes(colour = lab_aoo),
                              fill = "#d4271e", alpha = 0.25, linewidth = 0.25)
    feat[lab_aoo] <- "#9c0027"
  }
  g <- g + ggplot2::geom_sf(data = pts, ggplot2::aes(colour = lab_pts),
                            shape = 21, fill = "#f1c40f", size = 2, stroke = 0.4)
  feat[lab_pts] <- "#111111"
  lw_all <- stats::setNames(c(0.7, 0.25, 0, 0.4),
                            c(lab_eoo, lab_aoo, lab_pts, lab_uc))
  sh_all <- stats::setNames(c(NA, NA, 21, NA),
                            c(lab_eoo, lab_aoo, lab_pts, lab_uc))
  g <- g + ggplot2::scale_colour_manual(
    values = feat, name = NULL,
    guide = ggplot2::guide_legend(order = 1, override.aes = list(
      linewidth = unname(lw_all[names(feat)]),
      shape     = unname(sh_all[names(feat)]))))

  # ---- frame, scale bar, north arrow ----
  bb <- .plot_bbox(list(hull, cells, pts))
  g <- g + ggplot2::coord_sf(crs = crs, xlim = bb[c("xmin", "xmax")],
                             ylim = bb[c("ymin", "ymax")], expand = FALSE)
  if (isTRUE(scalebar)) g <- g + .scalebar_layer(bb, ref = .datum_label(crs))
  if (isTRUE(north))    g <- g + .north_layer(bb)

  g +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "right",
      # No boxes around the legends: transparent legend background and keys.
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.key.size = grid::unit(11, "pt"),
      legend.margin = ggplot2::margin(4, 6, 4, 6),
      panel.grid = ggplot2::element_line(colour = "grey90", linewidth = 0.3)
    )
}

#' @keywords internal
#' @noRd
.mb_display_df <- function(aoi, year, collection, src, max_pixels, crs,
                           lang = "pt", initiative = "brazil") {
  r <- .mb_raster_display(aoi, year, collection, src, max_pixels = max_pixels,
                          crs = crs, initiative = initiative)
  df <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "code"
  leg <- mb_legend(collection, initiative)
  lcol <- if (lang == "en") "class_en" else "class_pt"
  df <- df[df$code %in% leg$code, , drop = FALSE]
  idx <- match(df$code, leg$code)
  df$class <- droplevels(factor(leg[[lcol]][idx], levels = unique(leg[[lcol]])))
  cols <- stats::setNames(leg$hex[match(levels(df$class), leg[[lcol]])],
                          levels(df$class))
  list(df = df, cols = cols)
}

#' @keywords internal
#' @noRd
.fire_display_df <- function(aoi, collection, src, max_pixels, crs) {
  r <- .fire_raster_display(aoi, fire_collection = collection, src = src,
                            max_pixels = max_pixels, crs = crs,
                            product = "frequency")
  df <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "freq"
  b   <- .fire_freq_bins()
  bin <- findInterval(df$freq, b$lower)          # 0 = unburned, 1..6 = classes
  keep <- bin >= 1
  df <- df[keep, , drop = FALSE]; bin <- bin[keep]
  df$class <- droplevels(factor(b$label[bin], levels = b$label))
  cols <- stats::setNames(b$hex[match(levels(df$class), b$label)], levels(df$class))
  list(df = df, cols = cols)
}

#' @keywords internal
#' @noRd
.plot_bbox <- function(geoms) {
  geoms <- geoms[!vapply(geoms, is.null, logical(1))]
  bbs <- lapply(geoms, function(g) sf::st_bbox(sf::st_geometry(g)))
  xmin <- min(vapply(bbs, function(b) b[["xmin"]], numeric(1)))
  xmax <- max(vapply(bbs, function(b) b[["xmax"]], numeric(1)))
  ymin <- min(vapply(bbs, function(b) b[["ymin"]], numeric(1)))
  ymax <- max(vapply(bbs, function(b) b[["ymax"]], numeric(1)))
  pad <- 0.04 * max(xmax - xmin, ymax - ymin)
  c(xmin = xmin - pad, xmax = xmax + pad, ymin = ymin - pad, ymax = ymax + pad)
}

#' @keywords internal
#' @noRd
.nice_len <- function(x) {
  p <- 10^floor(log10(x)); f <- x / p
  (if (f >= 5) 5 else if (f >= 2) 2 else 1) * p
}

#' @keywords internal
#' @noRd
.scalebar_layer <- function(bb, ref = NULL) {
  w <- unname(bb["xmax"] - bb["xmin"]); h <- unname(bb["ymax"] - bb["ymin"])
  len <- .nice_len(w * 0.25)                       # metres (equal-area CRS)
  x0 <- unname(bb["xmin"]) + 0.06 * w
  y0 <- unname(bb["ymin"]) + 0.085 * h
  lab <- if (len >= 1000) sprintf("%g km", len / 1000) else sprintf("%g m", len)
  tick <- 0.012 * h
  has_ref <- !is.null(ref) && nzchar(ref)

  # No backing box behind the scale bar (kept boxless, like the legend).
  out <- list(
    ggplot2::annotate("segment", x = x0, xend = x0 + len, y = y0, yend = y0,
                      linewidth = 1.1, colour = "black"),
    ggplot2::annotate("segment", x = x0, xend = x0, y = y0 - tick, yend = y0 + tick,
                      colour = "black"),
    ggplot2::annotate("segment", x = x0 + len, xend = x0 + len,
                      y = y0 - tick, yend = y0 + tick, colour = "black"),
    ggplot2::annotate("text", x = x0 + len / 2, y = y0 + 0.03 * h,
                      label = lab, size = 3, vjust = 0)
  )
  if (has_ref) out <- c(out, list(
    ggplot2::annotate("text", x = x0, y = y0 - 0.018 * h, label = ref,
                      size = 2.5, vjust = 1, hjust = 0, colour = "grey20")
  ))
  out
}

#' @keywords internal
#' @noRd
.north_layer <- function(bb) {
  w <- unname(bb["xmax"] - bb["xmin"]); h <- unname(bb["ymax"] - bb["ymin"])
  x <- unname(bb["xmin"]) + 0.06 * w; ytop <- unname(bb["ymax"]) - 0.07 * h
  len <- 0.08 * h
  list(
    ggplot2::annotate("segment", x = x, xend = x, y = ytop - len, yend = ytop,
                      arrow = grid::arrow(length = grid::unit(7, "pt"),
                                          type = "closed"),
                      linewidth = 0.9, colour = "black"),
    ggplot2::annotate("text", x = x, y = ytop + 0.03 * h, label = "N",
                      fontface = "bold", size = 3.5, vjust = 0)
  )
}

#' Short datum label for the scale bar, e.g. "DATUM WGS84".
#'
#' The publishable map is drawn in the species' equal-area (LAEA) projection,
#' whose datum is what a reader needs beneath the scale bar. Reads the datum
#' from the CRS when available and falls back to WGS84.
#' @keywords internal
#' @noRd
.datum_label <- function(crs) {
  cr <- tryCatch(sf::st_crs(crs), error = function(e) NULL)
  dat <- NA_character_
  if (!is.null(cr) && !is.na(cr)) {
    p <- cr$proj4string
    if (!is.null(p) && !is.na(p) && nzchar(p)) {
      if (grepl("\\+datum=", p))
        dat <- sub(".*\\+datum=([A-Za-z0-9]+).*", "\\1", p)
      else if (grepl("\\+ellps=", p))
        dat <- sub(".*\\+ellps=([A-Za-z0-9]+).*", "\\1", p)
    }
  }
  if (is.na(dat) || !nzchar(dat)) dat <- "WGS84"
  sprintf("DATUM %s", toupper(dat))
}

#' @keywords internal
#' @noRd
.crs_label <- function(crs) {
  cr <- tryCatch(sf::st_crs(crs), error = function(e) NULL)
  if (is.null(cr) || is.na(cr)) return(NULL)
  if (!is.na(cr$epsg)) {
    nm <- cr$Name
    if (!is.null(nm) && !is.na(nm) && !identical(nm, "unknown"))
      return(sprintf("%s (EPSG:%d)", nm, cr$epsg))
    return(sprintf("EPSG:%d", cr$epsg))
  }
  # No EPSG (e.g. a data-centred LAEA): build a short, readable label from the
  # projection name + datum, not the full proj string (which overflows the bar).
  p <- cr$proj4string
  if (is.null(p) || is.na(p) || !nzchar(p)) return("Custom CRS")
  proj <- if (grepl("\\+proj=([A-Za-z]+)", p))
            toupper(sub(".*\\+proj=([A-Za-z]+).*", "\\1", p)) else NA_character_
  dat  <- if (grepl("\\+datum=", p)) sub(".*\\+datum=([A-Za-z0-9]+).*", "\\1", p)
          else if (grepl("\\+ellps=", p)) sub(".*\\+ellps=([A-Za-z0-9]+).*", "\\1", p)
          else NA_character_
  parts <- c(if (!is.na(proj)) proj, if (!is.na(dat)) paste0("datum ", dat))
  if (!length(parts)) "Custom CRS" else paste(parts, collapse = " - ")
}