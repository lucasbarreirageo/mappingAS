#' Build examined-voucher strings from occurrence attributes
#'
#' Derives a character vector of herbarium-style voucher citations from the
#' attribute columns of an occurrence table (or the \code{sf} returned by
#' \code{\link{read_occurrences}}), so the "Examined vouchers" box of the
#' factsheet can be filled automatically instead of by hand.
#'
#' Two strategies are tried, in order:
#' \enumerate{
#'   \item If a ready-made voucher column exists (e.g. \code{voucher},
#'     \code{exsiccata}, \code{material}) its non-empty values are used verbatim.
#'   \item Otherwise a collector column (e.g. \code{collector},
#'     \code{recordedBy}) is combined with a collection-number column (e.g.
#'     \code{collectorNumber}, \code{recordNumber}) into
#'     \code{"Collector Number"}. When a herbarium/institution column is present
#'     (e.g. \code{herbarium}, \code{institutionCode}) its code is appended in
#'     parentheses, giving \code{"Collector Number (RB)"}.
#' }
#'
#' Column names are matched case-insensitively from a set of common Darwin Core
#' and herbarium aliases; pass the \code{*_col} arguments to override the
#' detection. When no relevant column is found, an empty character vector is
#' returned (so the caller simply leaves the box empty).
#'
#' @param occ A data frame or an \code{sf} of occurrence records. Geometry, if
#'   present, is ignored.
#' @param species Optional species name (or vector) to filter the rows before
#'   building the vouchers, matched against the \code{species} column when it
#'   exists. \code{NULL} (default) uses every row.
#' @param voucher_col,collector_col,number_col,herbarium_col Optional column
#'   names overriding the automatic detection. Set a column to \code{NA} to
#'   disable that part of the detection.
#' @param unique Collapse duplicate vouchers to a single entry (default
#'   \code{TRUE}).
#' @param sort Sort the returned vouchers alphabetically (default \code{TRUE}).
#' @return A character vector of voucher strings (possibly empty).
#' @examples
#' df <- data.frame(
#'   species = c("Aus bus", "Aus bus"),
#'   collector = c("Barreira", "Silva"),
#'   collectorNumber = c("123", "456"),
#'   herbarium = c("RB", "R"),
#'   stringsAsFactors = FALSE)
#' vouchers_from_occ(df)
#' @export
vouchers_from_occ <- function(occ, species = NULL,
                              voucher_col = NULL, collector_col = NULL,
                              number_col = NULL, herbarium_col = NULL,
                              unique = TRUE, sort = TRUE) {
  if (inherits(occ, "sf")) occ <- sf::st_drop_geometry(occ)
  if (is.null(occ) || !is.data.frame(occ) || !nrow(occ))
    return(character(0))

  # Optionally restrict to one or more species.
  if (!is.null(species) && "species" %in% names(occ)) {
    keep <- trimws(as.character(occ$species)) %in% trimws(as.character(species))
    occ <- occ[keep, , drop = FALSE]
    if (!nrow(occ)) return(character(0))
  }

  cand <- .voucher_candidates()
  pick <- function(override, aliases) {
    if (!is.null(override)) return(if (isTRUE(is.na(override))) NA_character_ else override)
    .match_col(names(occ), aliases)
  }
  vcol <- pick(voucher_col, cand$voucher)
  ccol <- pick(collector_col, cand$collector)
  ncol <- pick(number_col, cand$number)
  hcol <- pick(herbarium_col, cand$herbarium)

  chr <- function(col) {
    if (is.na(col) || !col %in% names(occ)) return(rep(NA_character_, nrow(occ)))
    x <- trimws(as.character(occ[[col]]))
    x[is.na(x) | x == "" | tolower(x) %in% c("na", "null")] <- NA_character_
    x
  }

  out <- chr(vcol)                                   # strategy 1: ready-made
  if (all(is.na(out))) {                             # strategy 2: build it
    coll <- chr(ccol); num <- chr(ncol); herb <- chr(hcol)
    built <- ifelse(
      is.na(coll) & is.na(num), NA_character_,
      trimws(paste(ifelse(is.na(coll), "", coll),
                   ifelse(is.na(num), "", num))))
    has_h <- !is.na(herb) & !is.na(built)
    built[has_h] <- sprintf("%s (%s)", built[has_h], herb[has_h])
    out <- built
  }

  out <- out[!is.na(out) & nzchar(out)]
  if (isTRUE(unique)) out <- unique(out)
  if (isTRUE(sort)) out <- sort(out)
  out
}

#' Candidate column names for voucher construction
#' @keywords internal
#' @noRd
.voucher_candidates <- function() {
  list(
    voucher = c("voucher", "vouchers", "exsiccata", "exsiccatae",
                "material", "material_examined", "materialexamined",
                "voucher_id", "voucherid", "testemunho"),
    collector = c("collector", "recordedBy", "recorded_by", "recordedby",
                  "coletor", "collectorname", "collector_name", "leg",
                  "legit", "coletores", "collectors"),
    number = c("collectorNumber", "collector_number", "collectornumber",
               "recordNumber", "record_number", "recordnumber",
               "collectionNumber", "collection_number", "collectionnumber",
               "fieldNumber", "field_number", "fieldnumber",
               "numero", "numero_coleta", "numero_de_coleta", "num_coleta"),
    herbarium = c("herbarium", "herbario", "herbaria", "institutionCode",
                  "institution_code", "institutioncode", "acronym", "sigla",
                  "collectionCode", "collection_code")
  )
}
