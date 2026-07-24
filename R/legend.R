#' Standardised MapBiomas legend with conservation groupings
#'
#' Returns the \emph{standardised} MapBiomas land-use/land-cover legend used
#' across all three initiatives supported by \pkg{mappingAS} - MapBiomas Brazil
#' (Collection 10), Pan-Amazon / Amazonia (Collection 6) and Colombia
#' (Collection 3) - with each pixel class assigned to a conservation-relevant
#' group: \code{"natural"}, \code{"anthropic"}, \code{"water"},
#' \code{"not_observed"} or \code{"other"}. The \code{"anthropic"} classes are
#' what counts as \emph{converted} habitat; \code{"natural"} is the remaining
#' (current) natural habitat. By default \code{"water"}, \code{"not_observed"}
#' and \code{"other"} are excluded from the conversion denominator (see
#' \code{\link{summarise_conversion}}).
#'
#' MapBiomas harmonises pixel codes across its initiatives, so a single legend
#' can label the Brazil, Amazonia and Colombia rasters consistently - this is
#' what makes cross-border assessment possible for a species whose range spans
#' more than one product. The table is the union of the classes that occur in
#' any of the three products; a code that does not occur in a given raster
#' simply contributes zero area. Codes 81, 82 (Andinean formations), 34
#' (Glacier), 68 (Other natural non-vegetated) and 74 (Banana) are the
#' Colombia/Amazonia additions to the Brazil Collection 10 set.
#'
#' @param collection Integer collection number. Interpreted together with
#'   \code{initiative}; a value that does not match the initiative's default
#'   collection emits a warning and the standardised legend is still returned.
#' @param initiative One of \code{"brazil"} (default), \code{"amazonia"} or
#'   \code{"colombia"}. Kept for API symmetry and warning logic; the returned
#'   legend is the same standardised table for every initiative.
#' @return A \code{data.frame} with columns \code{code}, \code{class_en},
#'   \code{class_pt}, \code{hex} (standardised MapBiomas colour), \code{level1}
#'   and \code{group}.
#' @examples
#' head(mb_legend())
#' subset(mb_legend(), group == "anthropic")$class_en
#' @export
mb_legend <- function(collection = 10, initiative = "brazil") {
  ini <- .mb_resolve_initiative(initiative)
  if (!identical(as.integer(collection), as.integer(ini$collection))) {
    warning(sprintf(
      "mappingAS ships the standardised legend for %s Collection %d; using it.",
      ini$label, ini$collection))
  }

  leg <- data.frame(
    code = c(
      # Forest (natural)
      1, 3, 4, 5, 6, 49,
      # Herbaceous & shrubby vegetation (natural)
      10, 11, 12, 32, 29, 50, 13, 23, 81, 82,
      # Farming / anthropic
      14, 9, 15, 18, 19, 39, 20, 40, 62, 41,
      36, 46, 47, 35, 48, 21, 74,
      # Non-vegetated anthropic
      24, 30, 75,
      # Ambiguous / natural non-vegetated -> "other" (excluded by default)
      25, 68,
      # Water
      26, 33, 31, 34,
      # Not observed
      27
    ),
    class_en = c(
      "Forest", "Forest Formation", "Savanna Formation", "Mangrove",
      "Floodable Forest", "Wooded Sandbank Vegetation",
      "Herbaceous and Shrubby Vegetation", "Wetland", "Grassland",
      "Hypersaline Tidal Flat", "Rocky Outcrop", "Herbaceous Sandbank Vegetation",
      "Other Non Forest Formations", "Beach, Dune and Sand Spot",
      "Andinean Herbaceous and Shrubby Vegetation",
      "Flooded Andinean Herbaceous and Shrubby Vegetation",
      "Farming", "Forest Plantation", "Pasture", "Agriculture", "Temporary Crop",
      "Soybean", "Sugar cane", "Rice", "Cotton (beta)", "Other Temporary Crops",
      "Perennial Crop", "Coffee", "Citrus", "Palm Oil", "Other Perennial Crops",
      "Mosaic of Uses", "Banana (beta)",
      "Urban Area", "Mining", "Photovoltaic Power Plant (beta)",
      "Other non Vegetated Areas", "Other Natural Non Vegetated Area",
      "Water", "River, Lake and Ocean", "Aquaculture", "Glacier",
      "Not Observed"
    ),
    class_pt = c(
      "Floresta", "Formacao Florestal", "Formacao Savanica", "Mangue",
      "Floresta Alagavel", "Restinga Arborea",
      "Vegetacao Herbacea e Arbustiva", "Campo Alagado e Area Pantanosa",
      "Formacao Campestre", "Apicum", "Afloramento Rochoso", "Restinga Herbacea",
      "Outras Formacoes nao Florestais", "Praia, Duna e Areal",
      "Vegetacao Herbacea e Arbustiva Andina",
      "Vegetacao Herbacea e Arbustiva Andina Alagavel",
      "Agropecuaria", "Silvicultura", "Pastagem", "Agricultura", "Lavoura Temporaria",
      "Soja", "Cana", "Arroz", "Algodao (beta)", "Outras Lavouras Temporarias",
      "Lavoura Perene", "Cafe", "Citrus", "Dende", "Outras Lavouras Perenes",
      "Mosaico de Usos", "Banana (beta)",
      "Area Urbanizada", "Mineracao", "Usina Fotovoltaica (beta)",
      "Outras Areas nao Vegetadas", "Outra Area Natural nao Vegetada",
      "Corpo D'agua", "Rio, Lago e Oceano", "Aquicultura", "Geleira",
      "Nao Observado"
    ),
    hex = c(
      # forest
      "#1f8d49", "#1f8d49", "#7dc975", "#04381d", "#007785", "#02d659",
      # herbaceous / shrubby (+ beach, rocky, Andinean)
      "#d6bc74", "#519799", "#d6bc74", "#fc8114", "#ffaa5f", "#ad5100",
      "#d6bc74", "#ffa07a", "#dfeb62", "#6fc179",
      # farming block (+ banana)
      "#ffefc3", "#7a5900", "#edde8e", "#E974ED", "#C27BA0", "#f5b3c8",
      "#db7093", "#c71585", "#ff69b4", "#f54ca9",
      "#d082de", "#d68fe2", "#9932cc", "#9065d0", "#e6ccff", "#ffefc3",
      "#be83f7",
      # non-vegetated anthropic
      "#d4271e", "#9c0027", "#c12100",
      # other non-vegetated (anthropic-ambiguous + natural)
      "#db4d4f", "#e97a7a",
      # water (+ glacier)
      "#2532e4", "#2532e4", "#091077", "#93dfe6",
      # not observed
      "#ffffff"
    ),
    level1 = c(
      rep("Forest", 6),
      rep("Herbaceous/Shrubby", 10),
      rep("Farming", 17),
      rep("NonVegetated", 3),
      rep("NonVegetated", 2),
      rep("Water", 4),
      "NotObserved"
    ),
    group = c(
      rep("natural", 6),      # forest
      rep("natural", 10),     # herbaceous/shrubby + beach/dune + rocky + Andinean
      rep("anthropic", 17),   # farming block + banana
      rep("anthropic", 3),    # urban, mining, photovoltaic
      "other", "other",       # other non-vegetated (ambiguous + natural bare)
      "water", "water", "anthropic", "water",  # water, river/lake/ocean, aquaculture, glacier
      "not_observed"
    ),
    stringsAsFactors = FALSE
  )

  leg
}

#' Group lookup vectors for MapBiomas conservation groups
#'
#' Convenience wrapper returning the pixel codes that belong to each group.
#'
#' @inheritParams mb_legend
#' @return A named list with elements \code{natural}, \code{anthropic},
#'   \code{water}, \code{other} and \code{not_observed}, each an integer vector
#'   of MapBiomas pixel codes.
#' @export
mb_groups <- function(collection = 10, initiative = "brazil") {
  leg <- mb_legend(collection, initiative)
  split(leg$code, leg$group)
}

#' Official MapBiomas colours for a set of class codes
#'
#' @inheritParams mb_legend
#' @param codes Optional integer vector of MapBiomas pixel codes. If \code{NULL}
#'   (default) colours for the full legend are returned.
#' @param by One of \code{"code"} (names are codes) or \code{"class_pt"} /
#'   \code{"class_en"} (names are class labels). Default \code{"code"}.
#' @return A named character vector of hex colours.
#' @examples
#' mb_palette(c(3, 15, 39, 33))
#' @export
mb_palette <- function(codes = NULL, collection = 10, initiative = "brazil",
                       by = c("code", "class_pt", "class_en")) {
  by <- match.arg(by)
  leg <- mb_legend(collection, initiative)
  if (!is.null(codes)) {
    leg <- leg[match(as.integer(codes), leg$code), , drop = FALSE]
    leg <- leg[!is.na(leg$code), , drop = FALSE]
  }
  stats::setNames(leg$hex, leg[[by]])
}

#' Years available for a MapBiomas initiative/collection
#'
#' @inheritParams mb_legend
#' @return An integer vector of years available for the initiative (Brazil
#'   1985-2024, Amazonia 1986-2023, Colombia 1985-2024).
#' @examples
#' range(mb_years())
#' range(mb_years(initiative = "amazonia"))
#' @export
mb_years <- function(collection = 10, initiative = "brazil") {
  as.integer(.mb_resolve_initiative(initiative)$years)
}
