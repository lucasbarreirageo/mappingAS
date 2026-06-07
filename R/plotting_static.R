#' Publication-ready static map of a species (MapBiomas + EOO + AOO + points)
#'
#' Builds an exportable \pkg{ggplot2} map for one species: the MapBiomas
#' land-cover layer clipped to the EOO (with the official colour legend of the
#' classes actually present, in the bottom-right corner), the EOO minimum convex
#' polygon, the occupied AOO cells, the occurrence points, plus a north arrow and
#' a scale bar. The map is drawn in the species' equal-area projection so the
#' scale bar is meaningful, and is returned as a \code{ggplot} object you can save
#' with \code{ggplot2::ggsave()} to make a usable, georeferenced-looking figure.
#'
#' The MapBiomas layer is read with the cached windowed reader and aggregated to
#' \code{max_pixels} for display, so it is fast and reuses the raster already
#' fetched during \code{\link{assess_species}} (same EOO window).
#'
#' @param assessment A \code{geoconv_assessment} from \code{\link{assess_species}}.
#' @param species Species name (default: first assessed).
#' @param mapbiomas Logical; draw the MapBiomas raster layer (default \code{TRUE}).
#'   Set \code{FALSE} for a quick points/EOO/AOO map with no raster read.
#' @param src Optional local path / URL to a MapBiomas GeoTIFF (same as in
#'   \code{\link{assess_species}}); \code{NULL} (default) uses the public URL.
#' @param max_pixels Target maximum raster size (longest side, in pixels) for the
#'   display layer (default \code{600}).
#' @param crs Optional CRS for the map (proj string or EPSG code). Defaults to the
#'   species' equal-area projection from the EOO/AOO.
#' @param scalebar,north Logical; add a scale bar / north arrow (default \code{TRUE}).
#' @param title Optional plot title (default: the species name).
#' @return A \code{ggplot} object.
#' @examples
#' \dontrun{
#' occ <- read_occurrences(system.file("extdata", "example_occurrences.csv",
#'                                     package = "mappingAS"))
#' res <- assess_species(occ, year = 2024)
#' m <- map_static(res)
#' ggplot2::ggsave("eoo_map.png", m, width = 8, height = 7, dpi = 300)
#' }
#' @export
map_static <- function(assessment, species = NULL, mapbiomas = TRUE,
                       src = NULL, max_pixels = 600, crs = NULL,
                       scalebar = TRUE, north = TRUE, title = NULL) {
  stopifnot(inherits(assessment, "geoconv_assessment"))
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for map_static().", call. = FALSE)
  }
  d <- assessment$detail
  if (is.null(species)) species <- names(d)[1]
  obj <- d[[species]]
  if (is.null(obj)) stop("Species not found: ", species, call. = FALSE)
  if (is.null(title)) title <- species

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

  g <- ggplot2::ggplot()

  # ---- MapBiomas raster layer (clipped to the EOO) ----
  if (isTRUE(mapbiomas) && !is.null(hull)) {
    rdf <- tryCatch(
      .mb_display_df(hull, st$year, st$collection, src, max_pixels, crs),
      error = function(e) {
        warning("MapBiomas layer skipped: ", conditionMessage(e), call. = FALSE)
        NULL
      })
    if (!is.null(rdf) && nrow(rdf$df) > 0) {
      g <- g +
        ggplot2::geom_tile(
          data = rdf$df,
          ggplot2::aes(x = .data[["x"]], y = .data[["y"]], fill = .data[["class"]])
        ) +
        ggplot2::scale_fill_manual(
          values = rdf$cols, drop = FALSE,
          name = sprintf("MapBiomas %s (no EOO)", st$year))
    }
  }

  # ---- vector layers sharing one feature legend (colour aesthetic) ----
  feat <- character(0)
  if (!is.null(hull)) {
    g <- g + ggplot2::geom_sf(data = hull, ggplot2::aes(colour = "EOO (MCP)"),
                              fill = NA, linewidth = 0.7)
    feat["EOO (MCP)"] <- "#1f4e79"
  }
  if (!is.null(cells)) {
    g <- g + ggplot2::geom_sf(data = cells, ggplot2::aes(colour = "AOO (2 km)"),
                              fill = "#d4271e", alpha = 0.25, linewidth = 0.25)
    feat["AOO (2 km)"] <- "#9c0027"
  }
  g <- g + ggplot2::geom_sf(data = pts, ggplot2::aes(colour = "Ocorrencias"),
                            shape = 21, fill = "#f1c40f", size = 2, stroke = 0.4)
  feat["Ocorrencias"] <- "#111111"
  lw_all <- c("EOO (MCP)" = 0.7, "AOO (2 km)" = 0.25, "Ocorrencias" = 0)
  sh_all <- c("EOO (MCP)" = NA,  "AOO (2 km)" = NA,   "Ocorrencias" = 21)
  g <- g + ggplot2::scale_colour_manual(
    values = feat, name = NULL,
    guide = ggplot2::guide_legend(order = 1, override.aes = list(
      linewidth = unname(lw_all[names(feat)]),
      shape     = unname(sh_all[names(feat)]))))

  # ---- frame, scale bar, north arrow ----
  bb <- .plot_bbox(list(hull, cells, pts))
  g <- g + ggplot2::coord_sf(crs = crs, xlim = bb[c("xmin", "xmax")],
                             ylim = bb[c("ymin", "ymax")], expand = FALSE)
  if (isTRUE(scalebar)) g <- g + .scalebar_layer(bb)
  if (isTRUE(north))    g <- g + .north_layer(bb)

  g +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.background = ggplot2::element_rect(fill = "white", colour = "grey70"),
      legend.key.size = grid::unit(11, "pt"),
      legend.margin = ggplot2::margin(4, 6, 4, 6),
      panel.grid = ggplot2::element_line(colour = "grey90", linewidth = 0.3)
    ) +
    .legend_inside(0.99, 0.01, c(1, 0))
}

#' @keywords internal
#' @noRd
.mb_display_df <- function(aoi, year, collection, src, max_pixels, crs) {
  r <- .mb_raster_display(aoi, year, collection, src, max_pixels = max_pixels,
                          crs = crs)
  df <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "code"
  leg <- mb_legend(collection)
  df <- df[df$code %in% leg$code, , drop = FALSE]
  idx <- match(df$code, leg$code)
  df$class <- droplevels(factor(leg$class_pt[idx], levels = unique(leg$class_pt)))
  cols <- stats::setNames(leg$hex[match(levels(df$class), leg$class_pt)],
                          levels(df$class))
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
.scalebar_layer <- function(bb) {
  w <- unname(bb["xmax"] - bb["xmin"]); h <- unname(bb["ymax"] - bb["ymin"])
  len <- .nice_len(w * 0.25)                       # metres (equal-area CRS)
  x0 <- unname(bb["xmin"]) + 0.06 * w; y0 <- unname(bb["ymin"]) + 0.07 * h
  lab <- if (len >= 1000) sprintf("%g km", len / 1000) else sprintf("%g m", len)
  tick <- 0.012 * h
  list(
    ggplot2::annotate("rect", xmin = x0 - 0.02 * w, xmax = x0 + len + 0.02 * w,
                      ymin = y0 - 0.02 * h, ymax = y0 + 0.05 * h,
                      fill = "white", colour = NA, alpha = 0.7),
    ggplot2::annotate("segment", x = x0, xend = x0 + len, y = y0, yend = y0,
                      linewidth = 1.1, colour = "black"),
    ggplot2::annotate("segment", x = x0, xend = x0, y = y0 - tick, yend = y0 + tick,
                      colour = "black"),
    ggplot2::annotate("segment", x = x0 + len, xend = x0 + len,
                      y = y0 - tick, yend = y0 + tick, colour = "black"),
    ggplot2::annotate("text", x = x0 + len / 2, y = y0 + 0.03 * h,
                      label = lab, size = 3, vjust = 0)
  )
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

#' @keywords internal
#' @noRd
.legend_inside <- function(x, y, just = c(x, y)) {
  if (utils::packageVersion("ggplot2") >= "3.5.0") {
    ggplot2::theme(legend.position = "inside",
                   legend.position.inside = c(x, y),
                   legend.justification.inside = just)
  } else {
    ggplot2::theme(legend.position = c(x, y), legend.justification = just)
  }
}