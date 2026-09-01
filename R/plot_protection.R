#' Protection chart (inside vs outside Protected areas) for EOO and AOO
#'
#' Two horizontal stacked bars (EOO and AOO) showing the share of the range
#' inside federal Protected areas (Protected areas, UCs) versus outside, from
#' \code{assess_species(..., protected = TRUE)}. When MapBiomas was also computed
#' (\code{mapbiomas = TRUE}), the inside-UC part is split into \emph{natural} and
#' \emph{altered} habitat, so the dark-green segment is the range that is natural
#' \emph{and} protected. Layout mirrors \code{\link{plot_conversion}}: title,
#' one subtitle with the occurrence count, and a legend on the right.
#'
#' @param assessment A \code{geoconv_assessment} run with \code{protected = TRUE}.
#' @param species Species name (default: first).
#' @param lang Label language: \code{"en"} (default) or \code{"pt"}.
#' @return A \code{ggplot} object (ggplot2 available) or, in the base-graphics
#'   fallback, invisibly the plotted percentage matrix (rows \code{EOO}/\code{AOO}).
#' @seealso \code{\link{mas_plotly}} for the interactive version.
#' @examples
#' \donttest{
#' occ <- read_occurrences(system.file("extdata", "example_occurrences.csv",
#'                                     package = "mappingAS"))
#' res <- assess_species(occ, protected = TRUE, verbose = FALSE)  # reads WDPA
#' plot_protection(res)
#' }
#' @export
plot_protection <- function(assessment, species = NULL, lang = c("en", "pt")) {
  lang <- match.arg(lang)
  stopifnot(inherits(assessment, "geoconv_assessment"))
  d <- assessment$detail
  if (is.null(species)) species <- names(d)[1]
  obj <- d[[species]]
  if (is.null(obj)) stop("Species not found: ", species, call. = FALSE)
  pa <- obj$pa
  if (is.null(pa))
    stop("No protected-area data; run assess_species(..., protected = TRUE).",
         call. = FALSE)

  en  <- lang == "en"
  num <- function(x) if (is.null(x) || is.na(x)) 0 else x
  has_nat <- !is.null(pa$eoo_nat_uc_pct) && is.finite(pa$eoo_nat_uc_pct)

  if (has_nat) {
    grp  <- if (en) c("Natural", "Altered", "Outside")
            else    c("Natural", "Alterado", "Fora")
    cols <- stats::setNames(c("#1f8d49", "#f2c14e", "#d9d9d9"), grp)
    dark <- stats::setNames(c(TRUE, FALSE, FALSE), grp)
    seg_na <- function(nat, alt) stats::setNames(
      c(num(nat), num(alt), max(0, 100 - num(nat) - num(alt))), grp)
    M    <- rbind(AOO = seg_na(pa$aoo_nat_uc_pct, pa$aoo_alt_uc_pct),
                  EOO = seg_na(pa$eoo_nat_uc_pct, pa$eoo_alt_uc_pct))
    xlab <- if (en) "% of terrestrial range" else "% da area terrestre"
  } else {
    grp  <- if (en) c("In UC", "Outside") else c("Em UC", "Fora")
    cols <- stats::setNames(c("#1f8d49", "#d9d9d9"), grp)
    dark <- stats::setNames(c(TRUE, FALSE), grp)
    seg_io <- function(p) stats::setNames(c(num(p), 100 - num(p)), grp)
    M    <- rbind(AOO = seg_io(pa$aoo_pct), EOO = seg_io(pa$eoo_pct))
    xlab <- if (en) "% of range" else "% da distribuicao"
  }

  main_suffix <- if (en) " - protection by Protected areas"
                 else    " - protecao por Areas Protegidas"
  main      <- .sp_title_expr(species, main_suffix)   # ggplot (plotmath)
  main_html <- .sp_title_html(species, main_suffix)   # plotly / HTML
  occ  <- if (is.null(pa$occ_pct) || is.na(pa$occ_pct)) "-"
          else sprintf("%.0f%%", pa$occ_pct)
  sub  <- sprintf(if (en) "Occurrences in UCs: %d / %d (%s)  |  UCs: %d"
                  else    "Ocorrencias em UC: %d / %d (%s)  |  UCs: %d",
                  pa$n_occ_in, pa$n_occ, occ, pa$n_uc)

  # --- ggplot2 (interactive-ready) ---
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    df <- data.frame(
      range = factor(rep(c("AOO", "EOO"), each = length(grp)),
                     levels = c("AOO", "EOO")),
      group = factor(rep(grp, 2), levels = grp),
      pct   = c(M["AOO", ], M["EOO", ]),
      stringsAsFactors = FALSE
    )
    df$lab <- ifelse(is.finite(df$pct) & df$pct >= 6,
                     sprintf("%.0f%%", df$pct), "")
    df$lab_col <- ifelse(dark[as.character(df$group)], "white", "black")
    p <- ggplot2::ggplot(
      df, ggplot2::aes(x = .data[["pct"]], y = .data[["range"]],
                       fill = .data[["group"]])) +
      ggplot2::geom_col(width = 0.62, colour = "white", linewidth = 0.3) +
      ggplot2::geom_text(
        ggplot2::aes(label = .data[["lab"]], colour = .data[["lab_col"]]),
        position = ggplot2::position_stack(vjust = 0.5),
        size = 3.4, fontface = "bold", show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = cols[grp], drop = FALSE, name = NULL) +
      ggplot2::scale_colour_identity() +
      ggplot2::scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
      ggplot2::labs(x = xlab, y = NULL, title = main, subtitle = sub) +
      .mas_theme() +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    attr(p, "pct") <- M   # keep the percentage matrix accessible
    attr(p, "mas") <- list(kind = "bar", df = df, palette = as.list(cols[grp]),
                           levels = grp, xlab = xlab,
                           title = main_html, subtitle = sub)
    return(p)
  }

  # --- base-graphics fallback ---
  op <- graphics::par(mar = c(5, 5, 4, 9), xpd = NA)
  on.exit(graphics::par(op), add = TRUE)
  bp <- graphics::barplot(t(M), horiz = TRUE, col = cols[grp], border = "white",
    xlim = c(0, 100), las = 1, xlab = xlab, main = main)

  for (r in seq_len(nrow(M))) {
    x <- M[r, ]; cm <- cumsum(c(0, x))
    for (k in seq_along(x)) if (is.finite(x[k]) && x[k] >= 6)
      graphics::text(cm[k] + x[k] / 2, bp[r], sprintf("%.0f%%", x[k]),
        cex = 0.8, col = if (dark[grp[k]]) "white" else "black")
  }

  graphics::mtext(sub, side = 3, line = 0.2, cex = 0.85)
  usr <- graphics::par("usr")
  graphics::legend(
    x = usr[2] + 0.02 * diff(usr[1:2]), y = mean(usr[3:4]), yjust = 0.5,
    legend = grp, fill = cols[grp], border = "white",
    bty = "n", cex = 0.9, xpd = NA)
  invisible(M)
}