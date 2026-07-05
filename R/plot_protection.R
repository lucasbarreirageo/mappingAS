#' Protection chart (inside vs outside Conservation Units) for EOO and AOO
#'
#' Draws, for one species, two horizontal stacked bars (EOO and AOO) showing the
#' share of the range that falls inside federal Conservation Units (UCs) versus
#' outside them, from \code{assess_species(..., protected = TRUE)}. When the
#' assessment also computed MapBiomas (\code{mapbiomas = TRUE}), the inside-UC
#' segment is split into \emph{natural} and \emph{altered} habitat, so the chart
#' shows how much of the range is natural habitat that is \emph{also} protected
#' (the dark-green segment) - the most conservation-relevant quantity. The share
#' of occurrences inside UCs and the natural fraction \emph{of the UC area} are
#' shown as subtitles. Mirrors the layout of \code{\link{plot_conversion}}.
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

  en  <- lang == "en"
  num <- function(x) if (is.null(x) || is.na(x)) 0 else x
  has_nat <- !is.null(pa$eoo_nat_uc_pct) && is.finite(pa$eoo_nat_uc_pct)

  if (has_nat) {
    grp  <- if (en) c("Natural in UCs", "Altered in UCs", "Outside UCs")
            else    c("Natural em UC", "Alterado em UC", "Fora de UC")
    cols <- stats::setNames(c("#1f8d49", "#f2c14e", "#d9d9d9"), grp)
    txt  <- stats::setNames(c("white", "black", "black"), grp)
    seg3 <- function(nat, alt) {
      nat <- num(nat); alt <- num(alt)
      stats::setNames(c(nat, alt, max(0, 100 - nat - alt)), grp)
    }
    M <- rbind(AOO = seg3(pa$aoo_nat_uc_pct, pa$aoo_alt_uc_pct),
               EOO = seg3(pa$eoo_nat_uc_pct, pa$eoo_alt_uc_pct))
    xlab <- if (en) "% of terrestrial range" else "% da area terrestre"
  } else {
    grp  <- if (en) c("In UCs", "Outside UCs") else c("Em UC", "Fora de UC")
    cols <- stats::setNames(c("#1f8d49", "#d9d9d9"), grp)
    txt  <- stats::setNames(c("white", "black"), grp)
    seg2 <- function(p) { p <- num(p); stats::setNames(c(p, 100 - p), grp) }
    M <- rbind(AOO = seg2(pa$aoo_pct), EOO = seg2(pa$eoo_pct))
    xlab <- if (en) "% of range" else "% da distribuicao"
  }

  main <- sprintf(if (en) "%s - protection by Conservation Units"
                  else    "%s - protecao por Unidades de Conservacao", species)

  op <- graphics::par(mar = c(5, 5, 4, 9), xpd = NA)
  on.exit(graphics::par(op), add = TRUE)
  bp <- graphics::barplot(t(M), horiz = TRUE, col = cols[grp], border = "white",
    xlim = c(0, 100), las = 1, xlab = xlab, main = main)

  for (r in seq_len(nrow(M))) {
    x <- M[r, ]; cm <- cumsum(c(0, x))
    for (k in seq_along(x)) if (is.finite(x[k]) && x[k] >= 6)
      graphics::text(cm[k] + x[k] / 2, bp[r], sprintf("%.0f%%", x[k]),
        cex = 0.8, col = txt[grp[k]])
  }

  occ <- if (is.null(pa$occ_pct) || is.na(pa$occ_pct)) "-"
         else sprintf("%.0f%%", pa$occ_pct)
  graphics::mtext(
    sprintf(if (en) "Occurrences in UCs: %d / %d (%s)  |  UCs: %d"
            else    "Ocorrencias em UC: %d / %d (%s)  |  UCs: %d",
            pa$n_occ_in, pa$n_occ, occ, pa$n_uc),
    side = 3, line = if (has_nat) 0.9 else 0.2, cex = 0.8)

  if (has_nat) {
    pin <- function(x) if (is.null(x) || is.na(x)) "-" else sprintf("%.0f%%", x)
    graphics::mtext(
      sprintf(if (en) "Natural of the UC area: EOO %s | AOO %s"
              else    "Natural da area em UC: EOO %s | AOO %s",
              pin(pa$eoo_nat_uc_pct_in), pin(pa$aoo_nat_uc_pct_in)),
      side = 3, line = 0.1, cex = 0.8)
  }

  usr <- graphics::par("usr")
  graphics::legend(
    x = usr[2] + 0.02 * diff(usr[1:2]), y = mean(usr[3:4]), yjust = 0.5,
    legend = grp, fill = cols[grp], border = "white", bty = "n",
    cex = 0.9, xpd = NA)
  invisible(M)
}