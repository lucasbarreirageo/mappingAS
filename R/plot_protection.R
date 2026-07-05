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
    grp  <- if (en) c("Natural", "Altered", "Outside")
            else    c("Natural", "Alterado", "Fora")
    cols <- stats::setNames(c("#1f8d49", "#f2c14e", "#d9d9d9"), grp)
    dark <- stats::setNames(c(TRUE, FALSE, FALSE), grp)
    seg  <- function(nat, alt) stats::setNames(
      c(num(nat), num(alt), max(0, 100 - num(nat) - num(alt))), grp)
    M    <- rbind(AOO = seg(pa$aoo_nat_uc_pct, pa$aoo_alt_uc_pct),
                  EOO = seg(pa$eoo_nat_uc_pct, pa$eoo_alt_uc_pct))
    xlab <- if (en) "% of terrestrial range" else "% da area terrestre"
  } else {
    grp  <- if (en) c("In UC", "Outside") else c("Em UC", "Fora")
    cols <- stats::setNames(c("#1f8d49", "#d9d9d9"), grp)
    dark <- stats::setNames(c(TRUE, FALSE), grp)
    seg  <- function(p) stats::setNames(c(num(p), 100 - num(p)), grp)
    M    <- rbind(AOO = seg(pa$aoo_pct), EOO = seg(pa$eoo_pct))
    xlab <- if (en) "% of range" else "% da distribuicao"
  }

  main <- sprintf(if (en) "%s - protection by Protected areas"
                  else    "%s - protecao por Areas Protegidas", species)
  occ  <- if (is.null(pa$occ_pct) || is.na(pa$occ_pct)) "-"
          else sprintf("%.0f%%", pa$occ_pct)
  sub  <- sprintf(if (en) "Occurrences in UCs: %d / %d (%s)  |  UCs: %d"
                  else    "Ocorrencias em UC: %d / %d (%s)  |  UCs: %d",
                  pa$n_occ_in, pa$n_occ, occ, pa$n_uc)

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