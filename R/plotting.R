#' Interactive map of a species' points, EOO and AOO (MapBiomas + fire)
#'
#' Builds a \pkg{leaflet} map with the occurrence points, EOO hull and occupied
#' AOO cells. Optionally overlays the MapBiomas land-cover raster and the
#' MapBiomas Fire accumulated layer (fire recurrence: number of years burned),
#' both clipped to the EOO, as toggleable layers with legends.
#'
#' @param assessment A \code{geoconv_assessment} from \code{\link{assess_species}}.
#' @param species Species name to plot (default: first assessed).
#' @param mapbiomas Logical; overlay the MapBiomas raster (default \code{TRUE}).
#' @param fire Logical; overlay the MapBiomas Fire accumulated layer
#'   (default \code{FALSE}).
#' @param src Optional MapBiomas LULC GeoTIFF path/URL.
#' @param fire_src Optional MapBiomas Fire GeoTIFF path/URL.
#' @param max_pixels Target maximum raster size (longest side, pixels) for the
#'   overlays (default \code{800}).
#' @param lang Legend language: \code{"pt"} (default) or \code{"en"}. Controls
#'   the language of the MapBiomas class labels and the fire-frequency legend.
#' @param clip Geometry the rasters are clipped to: \code{"eoo"} (default, the
#'   EOO hull) or \code{"aoo"} (the union of occupied AOO cells).
#' @return A \code{leaflet} widget.
#' @export
map_species <- function(assessment, species = NULL, mapbiomas = TRUE,
                        fire = FALSE, src = NULL, fire_src = NULL,
                        max_pixels = 800, lang = c("pt", "en"),
                        clip = c("eoo", "aoo")) {
  lang <- match.arg(lang)
  clip <- match.arg(clip)
  lulc_col   <- if (lang == "en") "class_en" else "class_pt"
  lbl_mb     <- if (lang == "en") "MapBiomas %s" else "MapBiomas %s"
  lbl_fire   <- if (lang == "en") "Fire frequency<br>(years, 1985-2024)"
                else "Frequencia de fogo<br>(anos, 1985-2024)"
  grp_fire   <- if (lang == "en") "Fire frequency" else "Frequencia de fogo"
  stopifnot(inherits(assessment, "geoconv_assessment"))
  if (!requireNamespace("leaflet", quietly = TRUE))
    stop("Package 'leaflet' is required.", call. = FALSE)
  d <- assessment$detail
  if (is.null(species)) species <- names(d)[1]
  obj <- d[[species]]
  if (is.null(obj)) stop("Species not found in assessment: ", species, call. = FALSE)
  st <- assessment$settings

  # Geometry the rasters are clipped to: the EOO hull or the union of AOO cells.
  clip_geom <- if (clip == "aoo" && !is.null(obj$aoo$cells))
                 .st_union_quiet(obj$aoo$cells) else obj$eoo$hull

  m <- leaflet::leaflet()
  m <- leaflet::addProviderTiles(m, "CartoDB.Positron", group = "Light")
  m <- leaflet::addProviderTiles(m, "Esri.WorldImagery", group = "Satellite")
  
  # --- optional MapBiomas LULC overlay (under everything) ---
  mb_on <- FALSE
  if (isTRUE(mapbiomas) && !is.null(clip_geom) &&
      requireNamespace("terra", quietly = TRUE)) {
    rr <- tryCatch({
      r <- .mb_raster_display(clip_geom, st$year, st$collection, src,
                              max_pixels = max_pixels, crs = NULL)
      leg <- mb_legend(st$collection)
      vals <- terra::unique(r)[, 1]; vals <- vals[!is.na(vals)]
      list(r = r, keep = leg[leg$code %in% vals, , drop = FALSE], year = st$year)
    }, error = function(e) {
      warning("MapBiomas layer skipped: ", conditionMessage(e), call. = FALSE)
      NULL
    })
    if (!is.null(rr) && nrow(rr$keep) > 0) {
      pal <- leaflet::colorFactor(rr$keep$hex, levels = rr$keep$code,
                                  na.color = "transparent")
      m <- leaflet::addRasterImage(m, rr$r, colors = pal, opacity = 0.75,
                                   method = "ngb", project = TRUE,
                                   group = "MapBiomas")
      m <- leaflet::addLegend(m, position = "bottomright",
                              colors = rr$keep$hex, labels = rr$keep[[lulc_col]],
                              opacity = 0.75,
                              title = sprintf(lbl_mb, rr$year),
                              group = "MapBiomas")
      mb_on <- TRUE
    }
  }
  
  # --- optional MapBiomas Fire overlay (over LULC, under vectors) ---
  fire_on <- FALSE
  if (isTRUE(fire) && !is.null(clip_geom) &&
      requireNamespace("terra", quietly = TRUE)) {
    fr <- tryCatch(
      .fire_raster_display(clip_geom,
                           fire_collection = st$fire_collection %||% 4,
                           host_collection = st$fire_host_collection %||% 9,
                           src = fire_src, max_pixels = max_pixels),
      error = function(e) {
        warning("Fire layer skipped: ", conditionMessage(e), call. = FALSE)
        NULL
      })
    fv <- if (!is.null(fr)) {
      v <- terra::values(fr); v[is.finite(v)]
    } else numeric(0)
    if (length(fv) > 0) {
      brks <- c(1, 2, 4, 7, 11, 21, 41)          # fire-frequency classes (years)
      pal  <- leaflet::colorBin(fire_palette(length(brks) - 1), domain = c(1, 40),
                                bins = brks, right = FALSE, na.color = "transparent")
      m <- leaflet::addRasterImage(m, fr, colors = pal, opacity = 0.8,
                                   method = "ngb", project = TRUE,
                                   group = grp_fire)
      m <- leaflet::addLegend(m, position = "bottomleft", pal = pal,
                              values = c(1, 40), opacity = 0.8,
                              title = lbl_fire,
                              group = grp_fire)
      fire_on <- TRUE
    }
  }
  
  if (!is.null(obj$eoo$hull))
    m <- leaflet::addPolygons(m, data = sf::st_transform(obj$eoo$hull, 4326),
                              color = "#1f4e79", weight = 2,
                              fillColor = "#1f4e79", fillOpacity = 0.08,
                              group = "EOO (hull)")
  if (!is.null(obj$aoo$cells))
    m <- leaflet::addPolygons(m, data = sf::st_transform(obj$aoo$cells, 4326),
                              color = "#9c0027", weight = 1,
                              fillColor = "#d4271e", fillOpacity = 0.25,
                              group = "AOO (2 km cells)")
  pts <- sf::st_transform(sf::st_geometry(obj$points), 4326)
  co <- sf::st_coordinates(pts)
  m <- leaflet::addCircleMarkers(m, lng = co[, 1], lat = co[, 2], radius = 4,
                                 color = "#111111", fillColor = "#f1c40f",
                                 fillOpacity = 0.9, weight = 1,
                                 group = "Occurrences")
  
  overlay <- c("Occurrences", "EOO (hull)", "AOO (2 km cells)")
   if (fire_on) overlay <- c(grp_fire, overlay)
  if (mb_on)   overlay <- c("MapBiomas", overlay)
  m <- leaflet::addLayersControl(
    m, baseGroups = c("Light", "Satellite"), overlayGroups = overlay,
    options = leaflet::layersControlOptions(collapsed = FALSE))
  m <- leaflet::addControl(m, html = sprintf("<b>%s</b>", species),
                           position = "topright")
  m
}

#' Habitat composition chart (natural / altered / water / other) for EOO and AOO
#'
#' Draws, for one species, two horizontal stacked bars (EOO and AOO) showing the
#' MapBiomas composition as a percentage of the mapped (observed) area: natural,
#' altered (anthropic), water and other. Segment labels are placed inside each
#' bar and the legend sits below the plot, so nothing overlaps. The headline
#' \emph{terrestrial} converted percentage is shown as a subtitle.
#'
#' @param assessment A \code{geoconv_assessment}.
#' @param species Species name (default: first).
#' @return Invisibly, the plotted percentage matrix.
#' @export
plot_conversion <- function(assessment, species = NULL) {
  stopifnot(inherits(assessment, "geoconv_assessment"))
  d <- assessment$detail
  if (is.null(species)) species <- names(d)[1]
  obj <- d[[species]]
  if (is.null(obj)) stop("Species not found: ", species, call. = FALSE)

  grp  <- c("Natural", "Alterado", "Agua", "Outros")
  cols <- c(Natural = "#1f8d49", Alterado = "#d4271e",
            Agua = "#2532e4", Outros = "#bdbdbd")

  comp <- function(cv) {
    if (is.null(cv)) return(stats::setNames(rep(NA_real_, 4), grp))
    a <- c(Natural = cv$natural_km2, Alterado = cv$anthropic_km2,
           Agua = cv$water_km2, Outros = cv$other_km2)
    tot <- sum(a, na.rm = TRUE)
    if (!is.finite(tot) || tot <= 0) return(stats::setNames(rep(NA_real_, 4), grp))
    100 * a / tot
  }
  M <- rbind(AOO = comp(obj$aoo_conversion), EOO = comp(obj$eoo_conversion))

  op <- graphics::par(mar = c(5, 5, 4, 9), xpd = NA)
  on.exit(graphics::par(op), add = TRUE)
  bp <- graphics::barplot(
    t(M), horiz = TRUE, col = cols[grp], border = "white",
    xlim = c(0, 100), las = 1,
    xlab = "% da area mapeada (observada)",
    main = sprintf("%s - composicao (MapBiomas %s)",
                   species, assessment$settings$year)
  )

  for (r in seq_len(nrow(M))) {
    x <- M[r, ]; cm <- cumsum(c(0, x))
    for (k in seq_along(x)) {
      if (is.finite(x[k]) && x[k] >= 6) {
        graphics::text(cm[k] + x[k] / 2, bp[r], sprintf("%.0f%%", x[k]),
                       cex = 0.8, col = if (grp[k] == "Outros") "black" else "white")
      }
    }
  }

  fmt <- function(cv) {
    v <- if (is.null(cv)) NA_real_ else cv$converted_pct
    if (is.finite(v)) sprintf("%.0f%%", v) else "-"
  }
  graphics::mtext(
    sprintf("Convertido (terrestre): EOO %s  |  AOO %s",
            fmt(obj$eoo_conversion), fmt(obj$aoo_conversion)),
    side = 3, line = 0.2, cex = 0.85)

  usr <- graphics::par("usr")
  graphics::legend(
    x = usr[2] + 0.02 * diff(usr[1:2]), y = mean(usr[3:4]), yjust = 0.5,
    legend = grp, fill = cols[grp], border = "white",
    bty = "n", cex = 0.9, xpd = NA)
  invisible(M)
}