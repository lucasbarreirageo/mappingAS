#' Tidy MapBiomas composition for donut charts
#'
#' Builds the per-range composition used by \code{\link{plot_conversion_donut}},
#' either by individual MapBiomas class or by conservation group, with the
#' official MapBiomas colour attached to every slice.
#'
#' @param assessment A \code{geoconv_assessment} from \code{\link{assess_species}}.
#' @param species Optional species name. \code{NULL} uses the first species.
#' @param by \code{"class"} (default, one slice per MapBiomas class) or
#'   \code{"group"} (Natural / Altered / Water / Other).
#' @param lang \code{"en"} (default) or \code{"pt"}.
#' @return A \code{data.frame} with columns \code{range} (factor
#'   \code{"EOO"}/\code{"AOO"}), \code{label} (factor, ordered by descending
#'   total percentage), \code{pct} and \code{hex}. Empty if no class data.
#' @keywords internal
#' @noRd
.donut_data <- function(assessment, species = NULL,
                        by = c("class", "group"), lang = c("en", "pt")) {
  by   <- match.arg(by)
  lang <- match.arg(lang)
  if (is.null(species)) species <- names(assessment$detail)[1]

  ct <- tryCatch(
    class_table(assessment, species = species, range = "both"),
    error = function(e) data.frame())
  if (!nrow(ct)) return(data.frame())

  # Use the legend of the product actually used for this species (Sentinel-2 /
  # Esri when the range fell back outside MapBiomas coverage).
  obj <- assessment$detail[[species]]
  st  <- assessment$settings
  lc_ini  <- obj$initiative %||% st$initiative %||% "brazil"
  lc_coll <- obj$collection %||% st$collection
  leg <- mb_legend(lc_coll, lc_ini)
  ct$hex <- leg$hex[match(ct$code, leg$code)]
  ct$hex[is.na(ct$hex)] <- "#bdbdbd"

  if (by == "group") {
    gl <- .mb_group_labels(lang)
    ct$label <- gl$labels[ct$group]
    ct$label[is.na(ct$label)] <- gl$labels[["other"]]
    ct$hex   <- unname(gl$hex[ct$group])
    ct$hex[is.na(ct$hex)] <- gl$hex[["other"]]
    agg <- stats::aggregate(pct ~ range + label + hex, data = ct, FUN = sum)
  } else {
    ct$label <- if (lang == "en") ct$class_en else ct$class_pt
    agg <- ct[, c("range", "label", "pct", "hex"), drop = FALSE]
    # lump tiny classes (<1% within a range) into a single grey "Other"
    # slice so the ring stays readable; exact per-class values remain in
    # the Classes tab.
    small <- is.finite(agg$pct) & agg$pct < 1
    if (any(small)) {
      agg$label[small] <- if (lang == "en") "Other (<1%)" else "Outros (<1%)"
      agg$hex[small]   <- "#9e9e9e"
      agg <- stats::aggregate(pct ~ range + label + hex, data = agg, FUN = sum)
    }
  }

  # keep one colour per label (first wins) and order labels by total share
  pal <- tapply(agg$hex, agg$label, function(v) v[1])
  tot <- tapply(agg$pct, agg$label, function(v) sum(v, na.rm = TRUE))
  ord <- names(sort(tot, decreasing = TRUE))

  agg$range <- factor(agg$range, levels = c("EOO", "AOO"))
  agg$label <- factor(agg$label, levels = ord)
  agg$hex   <- unname(pal[as.character(agg$label)])
  agg <- agg[order(agg$range, agg$label), , drop = FALSE]
  rownames(agg) <- NULL
  agg
}

#' MapBiomas composition donut charts (EOO and AOO)
#'
#' Two side-by-side donut (ring) charts summarising the MapBiomas composition of
#' the Extent of Occurrence (EOO) and Area of Occupancy (AOO) of a species, in
#' the official MapBiomas colours. Slices can be individual land-cover classes
#' (\code{by = "class"}) or the four conservation groups
#' (\code{by = "group"}). Designed to be exported as a transparent-background
#' PNG (all panel/plot/legend backgrounds are transparent), and the interactive
#' \pkg{plotly} version (via \code{\link{mas_plotly}}) adds outside labels with
#' leader lines and a transparent export.
#'
#' @param assessment A \code{geoconv_assessment} from \code{\link{assess_species}}
#'   (run with \code{mapbiomas = TRUE}).
#' @param species Optional species name. \code{NULL} (default) uses the first
#'   assessed species.
#' @param by \code{"class"} (default) for one ring slice per MapBiomas class, or
#'   \code{"group"} for the Natural / Altered / Water / Other summary.
#' @param lang \code{"en"} (default) or \code{"pt"} for slice labels.
#' @return A \pkg{ggplot} object (with mappingAS metadata attached in attribute
#'   \code{"mas"} so \code{\link{mas_plotly}} can build the interactive donut).
#'   Stops with an informative message if no class data are available.
#' @examples
#' \donttest{
#' occ <- read_occurrences(system.file("extdata", "example_occurrences.csv",
#'                                     package = "mappingAS"))
#' res <- assess_species(occ, year = 2024, verbose = FALSE)  # reads MapBiomas
#' plot_conversion_donut(res, by = "class")
#' }
#' @export
plot_conversion_donut <- function(assessment, species = NULL,
                                  by = c("class", "group"),
                                  lang = c("en", "pt")) {
  by   <- match.arg(by)
  lang <- match.arg(lang)
  stopifnot(inherits(assessment, "geoconv_assessment"))
  if (is.null(species)) species <- names(assessment$detail)[1]

  df <- .donut_data(assessment, species = species, by = by, lang = lang)
  if (!nrow(df)) {
    stop("No land-cover class data available (run assess_species(mapbiomas = TRUE)).",
         call. = FALSE)
  }

  ttl_suffix <- if (lang == "en")
    sprintf(" - land-cover composition by %s",
            if (by == "group") "group" else "class")
  else
    sprintf(" - composicao da cobertura por %s",
            if (by == "group") "grupo" else "classe")
  ttl      <- .sp_title_expr(species, ttl_suffix)   # ggplot (plotmath)
  ttl_html <- .sp_title_html(species, ttl_suffix)   # plotly / HTML

  pal <- tapply(df$hex, df$label, function(v) v[1])
  pal <- stats::setNames(as.character(pal), names(pal))

  # --- ggplot2 donut (transparent, for PNG export) ---
  df$lab <- ifelse(is.finite(df$pct) & df$pct >= 3,
                   sprintf("%.0f%%", df$pct), "")
  p <- ggplot2::ggplot(
    df, ggplot2::aes(x = 2, y = .data[["pct"]], fill = .data[["label"]])) +
    ggplot2::geom_col(width = 1, colour = "white", linewidth = 0.4) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::xlim(c(0.4, 2.5)) +
    ggplot2::facet_wrap(~ range) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data[["lab"]]),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 3, colour = "#1a1a1a") +
    ggplot2::scale_fill_manual(values = pal, name = NULL) +
    ggplot2::labs(title = ttl) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 14),
      strip.text      = ggplot2::element_text(face = "bold", size = 12),
      legend.position = "right",
      legend.text     = ggplot2::element_text(size = 9),
      plot.background   = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key        = ggplot2::element_rect(fill = "transparent", colour = NA))

  attr(p, "mas") <- list(kind = "donut",
                         df = df[, c("range", "label", "pct", "hex")],
                         title = ttl_html)
  p
}
