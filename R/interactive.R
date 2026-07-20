#' Shared ggplot2 theme for mappingAS charts
#'
#' A single, minimal theme applied by every mappingAS chart so the static
#' figures and their interactive (plotly) versions share one visual language:
#' bold title, muted subtitle/axis text, no minor grid and a bottom legend.
#'
#' @param base_size Base font size in points (default \code{12}).
#' @return A \pkg{ggplot2} theme object.
#' @keywords internal
#' @noRd
.mas_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title        = ggplot2::element_text(face = "bold",
                                                 size = base_size + 2),
      plot.subtitle     = ggplot2::element_text(colour = "#7a857b",
                                                 size = base_size - 1),
      plot.title.position = "plot",
      panel.grid.minor  = ggplot2::element_blank(),
      axis.title        = ggplot2::element_text(colour = "#555555"),
      legend.position   = "bottom",
      legend.title      = ggplot2::element_blank(),
      legend.key.size   = ggplot2::unit(12, "pt")
    )
}

# Apply the shared interactive chrome (two-line title, margins that leave room
# for the right-hand legend, trimmed mode bar) to a plotly object.
.mas_finish <- function(gg, title = NULL, subtitle = NULL) {
  has_sub <- !is.null(subtitle) && nzchar(subtitle)
  text <- if (has_sub)
    sprintf("<b>%s</b><br><span style='font-size:12px;color:#7a857b'>%s</span>",
            title %||% "", subtitle)
  else
    sprintf("<b>%s</b>", title %||% "")
  gg <- plotly::layout(
    gg,
    # Anchor the title in the top margin (its bottom on the plot's top edge) so
    # a two-line title/subtitle grows upward into the margin and never clips.
    title  = list(text = text, x = 0.02, xanchor = "left", xref = "paper",
                  y = 1, yanchor = "bottom", yref = "paper",
                  font = list(size = 15), pad = list(b = 6)),
    margin = list(t = if (has_sub) 96 else 66, r = 185, l = 62, b = 52),
    legend = list(x = 1.02, xanchor = "left", y = 1, yanchor = "top",
                  font = list(size = 11)),
    hoverlabel = list(bgcolor = "#233d2c", bordercolor = "#233d2c",
                      font = list(color = "white", size = 13))
  )
  plotly::config(
    gg, displaylogo = FALSE, responsive = TRUE,
    modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d",
                               "hoverClosestCartesian", "hoverCompareCartesian",
                               "toggleSpikelines")
  )
}

# Native plotly horizontal stacked bar (used by plot_conversion / plot_protection).
# One trace per group so the legend toggles and the hover tooltip land on each
# segment, with the % label placed inside its own segment.
.mas_plotly_bar <- function(m) {
  df <- m$df
  ranges <- levels(df$range)
  txtcol <- function(g) { v <- df$lab_col[df$group == g]; if (length(v)) v[1] else "white" }
  p <- plotly::plot_ly()
  for (g in m$levels) {
    sub <- df[df$group == g, , drop = FALSE]
    x   <- sub$pct[match(ranges, as.character(sub$range))]
    lab <- ifelse(is.finite(x) & x >= 6, sprintf("%.0f%%", x), "")
    p <- plotly::add_trace(
      p, x = x, y = ranges, type = "bar", orientation = "h",
      name = g, legendgroup = g,
      marker = list(color = unname(m$palette[[g]]),
                    line = list(color = "white", width = 1)),
      text = lab, textposition = "inside", insidetextanchor = "middle",
      cliponaxis = FALSE, textfont = list(color = txtcol(g), size = 13),
      hovertemplate = paste0("<b>", g, "</b><br>%{y}: %{x:.1f}%<extra></extra>")
    )
  }
  p <- plotly::layout(
    p, barmode = "stack", bargap = 0.32,
    xaxis = list(title = m$xlab, range = c(0, 100), zeroline = FALSE,
                 ticksuffix = "%"),
    yaxis = list(title = "", automargin = TRUE)
  )
  .mas_finish(p, m$title, m$subtitle)
}

# Native plotly stacked-area chart over years (used by plot_timeseries). Uses
# `hovermode = "x unified"` so hovering anywhere over a year shows every class's
# value at that year - which fixes ggplotly reporting a single (wrong) year.
.mas_plotly_area <- function(m) {
  df <- m$df
  if (isTRUE(m$ny < 2)) {           # single year -> simple stacked column
    yr <- as.character(unique(df$year)[1])
    p  <- plotly::plot_ly()
    for (lv in m$levels) {
      v <- df$pct[df$label == lv]; v <- if (length(v)) v[1] else 0
      p <- plotly::add_trace(
        p, x = yr, y = v, type = "bar", name = lv, legendgroup = lv,
        marker = list(color = unname(m$palette[[lv]])),
        hovertemplate = paste0("<b>", lv, "</b><br>%{x}: %{y:.1f}%<extra></extra>"))
    }
    p <- plotly::layout(p, barmode = "stack",
                        xaxis = list(title = m$xlab),
                        yaxis = list(title = m$ylab, range = c(0, 100)))
    return(.mas_finish(p, m$title, NULL))
  }
  p <- plotly::plot_ly()
  for (lv in m$levels) {
    sub <- df[df$label == lv, , drop = FALSE]
    sub <- sub[order(sub$year), , drop = FALSE]
    p <- plotly::add_trace(
      p, x = sub$year, y = sub$pct, type = "scatter", mode = "lines",
      stackgroup = "one", name = lv, legendgroup = lv,
      line = list(width = 0.5, color = unname(m$palette[[lv]])),
      fillcolor = unname(m$palette[[lv]])
    )
  }
  p <- plotly::layout(
    p, hovermode = "x unified",
    xaxis = list(title = m$xlab, zeroline = FALSE),
    yaxis = list(title = m$ylab, range = c(0, 100), hoverformat = ".1f",
                 ticksuffix = "%")
  )
  .mas_finish(p, m$title, NULL)
}

# Native plotly burned-area chart: bars + trend line (used by plot_fire_timeseries).
.mas_plotly_fire <- function(m) {
  df <- m$df
  p  <- plotly::plot_ly()
  p  <- plotly::add_trace(
    p, x = df$year, y = df$burned_pct, type = "bar", name = "Burned area",
    marker = list(color = "#fd8d3c"),
    hovertemplate = "%{x}: %{y:.1f}%<extra></extra>")
  p  <- plotly::add_trace(
    p, x = df$year, y = df$burned_pct, type = "scatter", mode = "lines",
    line = list(color = "#bd0026", width = 1.5),
    showlegend = FALSE, hoverinfo = "skip")
  p  <- plotly::layout(
    p, showlegend = FALSE,
    xaxis = list(title = ""),
    yaxis = list(title = m$ylab, ticksuffix = "%"))
  .mas_finish(p, m$title, NULL)
}

# Native plotly twin donut charts (EOO + AOO) for MapBiomas composition (used by
# plot_conversion_donut). One `add_pie` trace per range placed in its own domain,
# with outside labels + leader lines, MapBiomas slice colours and a fully
# transparent background so the PNG export has no backdrop.
.mas_plotly_donut <- function(m) {
  df <- m$df
  ranges  <- levels(df$range)
  domains <- list(c(0, 0.48), c(0.52, 1))
  p <- plotly::plot_ly()
  anns <- list()
  for (i in seq_along(ranges)) {
    rg  <- ranges[i]
    sub <- df[as.character(df$range) == rg, , drop = FALSE]
    sub <- sub[is.finite(sub$pct) & sub$pct > 0, , drop = FALSE]
    if (!nrow(sub)) next
    dom <- domains[[i]]
    p <- plotly::add_pie(
      p, data = sub, labels = ~label, values = ~pct,
      name = rg, sort = FALSE, direction = "clockwise", hole = 0.55,
      domain = list(x = dom, y = c(0.03, 0.80)),
      marker = list(colors = sub$hex, line = list(color = "white", width = 1.5)),
      textposition = "outside", textinfo = "label+percent",
      insidetextorientation = "radial",
      hovertemplate = "<b>%{label}</b><br>%{percent}<extra></extra>")
    anns[[length(anns) + 1L]] <- list(
      text = paste0("<b>", rg, "</b>"), showarrow = FALSE,
      x = mean(dom), y = 0.5, xref = "paper", yref = "paper",
      font = list(size = 15, color = "#233d2c"))
  }
  p <- plotly::layout(
    p,
    title = list(text = paste0("<b>", m$title %||% "", "</b>"),
                 x = 0.02, xanchor = "left", y = 0.99, yanchor = "top",
                 font = list(size = 15)),
    showlegend = FALSE, annotations = anns,
    margin = list(t = 96, r = 70, l = 70, b = 40),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    hoverlabel = list(bgcolor = "#233d2c", bordercolor = "#233d2c",
                      font = list(color = "white", size = 13)))
  plotly::config(
    p, displaylogo = FALSE, responsive = TRUE,
    toImageButtonOptions = list(format = "png", filename = "mappingAS_donut",
                                scale = 2),
    modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d",
                               "hoverClosestCartesian", "hoverCompareCartesian",
                               "toggleSpikelines"))
}

#' Turn a mappingAS chart into an interactive plotly widget
#'
#' Converts a \pkg{ggplot2} object produced by the mappingAS \code{plot_*}
#' functions (\code{\link{plot_conversion}}, \code{\link{plot_protection}},
#' \code{\link{plot_timeseries}}, \code{\link{plot_fire_timeseries}}) into an
#' interactive \pkg{plotly} widget. Those functions attach the tidy data and
#' palette needed, so the widget is built as \emph{native} plotly (correct
#' per-segment hover tooltips and in-bar labels) rather than through
#' \code{ggplotly()}. Any other ggplot falls back to \code{ggplotly()}. This is
#' the single place that controls the interactive look across the package and
#' the Shiny app.
#'
#' @param p A \pkg{ggplot} object (as returned by the \code{plot_*} functions).
#'   Any non-ggplot is returned unchanged.
#' @param tooltip Aesthetics passed to \code{plotly::ggplotly()} in the generic
#'   fallback (default \code{c("fill", "x", "y")}).
#' @param ... Reserved for future use.
#' @return A \code{plotly} htmlwidget (or \code{p} unchanged when it is not a
#'   ggplot or \pkg{plotly} is not installed).
#' @examples
#' \dontrun{
#' res <- assess_species(read_occurrences("occ.csv"))
#' mas_plotly(plot_conversion(res))
#' }
#' @export
mas_plotly <- function(p, tooltip = c("fill", "x", "y"), ...) {
  if (!inherits(p, "ggplot")) return(p)
  if (!requireNamespace("plotly", quietly = TRUE)) {
    warning("Package 'plotly' is required for interactive charts; ",
            "returning the static ggplot.", call. = FALSE)
    return(p)
  }

  meta <- attr(p, "mas")
  if (!is.null(meta) && !is.null(meta$kind)) {
    out <- switch(meta$kind,
                  bar   = .mas_plotly_bar(meta),
                  area  = .mas_plotly_area(meta),
                  fire  = .mas_plotly_fire(meta),
                  donut = .mas_plotly_donut(meta),
                  NULL)
    if (!is.null(out)) return(out)
  }

  # generic fallback for ggplots without attached mappingAS metadata
  p  <- p + ggplot2::theme(legend.position = "right")
  gg <- plotly::ggplotly(p, tooltip = tooltip)
  .mas_finish(gg, p$labels$title, p$labels$subtitle)
}
