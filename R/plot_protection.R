#' Protection chart (inside vs outside Conservation Units) for EOO and AOO
#'
#' Draws, for one species, two horizontal stacked bars (EOO and AOO) showing the
#' share of the range that falls inside federal Conservation Units (UCs) versus
#' outside them, from \code{assess_species(..., protected = TRUE)}. The share of
#' occurrences inside UCs is shown as a subtitle. Mirrors the layout of
#' \code{\link{plot_conversion}} so the two charts read consistently.
#'
#' @param assessment A \code{geoconv_assessment} run with \code{protected = TRUE}.
#' @param species Species name (default: first).
#' @param lang Label language: \code{"en"} (default) or \code{"pt"}.
#' @return Invisibly, the plotted percentage matrix (rows \code{EOO}/\code{AOO}).
#' @examples
#' \dontrun{
#' res <- assess_species(read_occurrences("occ.csv"), protected = TRUE)
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

  if (lang == "en") {
    grp <- c("In UCs", "Outside UCs")
    xlab <- "% of range"
    main_fmt <- "%s - protection by Conservation Units"
    sub_fmt  <- "Occurrences in UCs: %d / %d (%s)  |  UCs: %d"
  } else {
    grp <- c("Em UC", "Fora de UC")
    xlab <- "% da distribuicao"
    main_fmt <- "%s - protecao por Unidades de Conservacao"
    sub_fmt  <- "Ocorrencias em UC: %d / %d (%s)  |  UCs: %d"
  }
  cols <- stats::setNames(c("#1f8d49", "#d9d9d9"), grp)

  brk <- function(p) {
    p <- if (is.null(p) || is.na(p)) NA_real_ else p
    stats::setNames(c(p, 100 - p), grp)
  }
  M <- rbind(AOO = brk(pa$aoo_pct), EOO = brk(pa$eoo_pct))

  op <- graphics::par(mar = c(5, 5, 4, 9), xpd = NA)
  on.exit(graphics::par(op), add = TRUE)
  bp <- graphics::barplot(t(M), horiz = TRUE, col = cols[grp], border = "white",
    xlim = c(0, 100), las = 1, xlab = xlab, main = sprintf(main_fmt, species))

  for (r in seq_len(nrow(M))) {
    x <- M[r, ]; cm <- cumsum(c(0, x))
    for (k in seq_along(x)) if (is.finite(x[k]) && x[k] >= 6)
      graphics::text(cm[k] + x[k] / 2, bp[r], sprintf("%.0f%%", x[k]),
        cex = 0.8, col = if (k == 1) "white" else "black")
  }

  occ <- if (is.null(pa$occ_pct) || is.na(pa$occ_pct)) "-"
         else sprintf("%.0f%%", pa$occ_pct)
  graphics::mtext(sprintf(sub_fmt, pa$n_occ_in, pa$n_occ, occ, pa$n_uc),
    side = 3, line = 0.2, cex = 0.85)

  usr <- graphics::par("usr")
  graphics::legend(
    x = usr[2] + 0.02 * diff(usr[1:2]), y = mean(usr[3:4]), yjust = 0.5,
    legend = grp, fill = cols[grp], border = "white", bty = "n",
    cex = 0.9, xpd = NA)
  invisible(M)
}