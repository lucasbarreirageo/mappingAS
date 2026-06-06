#' Interactive map of a species' points, EOO and AOO
#'
#' Builds a \pkg{leaflet} map showing the occurrence points, the EOO hull and
#' the occupied AOO cells for one species from an assessment.
#'
#' @param assessment A \code{geoconv_assessment} from
#'   \code{\link{assess_species}}.
#' @param species Species name to plot. If \code{NULL}, the first assessed
#'   species is used.
#' @return A \code{leaflet} widget.
#' @export
map_species <- function(assessment, species = NULL) {
  stopifnot(inherits(assessment, "geoconv_assessment"))
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required.", call. = FALSE)
  }
  d <- assessment$detail
  if (is.null(species)) species <- names(d)[1]
  obj <- d[[species]]
  if (is.null(obj)) stop("Species not found in assessment: ", species, call. = FALSE)

  m <- leaflet::leaflet()
  m <- leaflet::addProviderTiles(m, "CartoDB.Positron", group = "Light")
  m <- leaflet::addProviderTiles(m, "Esri.WorldImagery", group = "Satellite")

  if (!is.null(obj$eoo$hull)) {
    m <- leaflet::addPolygons(
      m, data = sf::st_transform(obj$eoo$hull, 4326),
      color = "#1f4e79", weight = 2, fillColor = "#1f4e79", fillOpacity = 0.08,
      group = "EOO (hull)"
    )
  }
  if (!is.null(obj$aoo$cells)) {
    m <- leaflet::addPolygons(
      m, data = sf::st_transform(obj$aoo$cells, 4326),
      color = "#9c0027", weight = 1, fillColor = "#d4271e", fillOpacity = 0.25,
      group = "AOO (2 km cells)"
    )
  }
  pts <- sf::st_transform(sf::st_geometry(obj$points), 4326)
  co <- sf::st_coordinates(pts)
  m <- leaflet::addCircleMarkers(
    m, lng = co[, 1], lat = co[, 2], radius = 4, color = "#111111",
    fillColor = "#f1c40f", fillOpacity = 0.9, weight = 1,
    group = "Occurrences"
  )
  m <- leaflet::addLayersControl(
    m,
    baseGroups = c("Light", "Satellite"),
    overlayGroups = c("Occurrences", "EOO (hull)", "AOO (2 km cells)"),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )
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
