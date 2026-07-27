#' Read Protected areas intersecting an area of interest
#'
#' Returns the protected-area polygons that fall within the bounding box of
#' `aoi`. By default it reads the global World Database on Protected Areas
#' (WDPA) via [wdpa_areas()] (see there for the source), so the overlap works
#' anywhere in the world. For offline use, point `src` to a local protected-area
#' file (`.shp`, `.gpkg`, `.geojson`) instead; it is then filtered to the AOI
#' bounding box and standardised to the same columns.
#'
#' The result carries three standardised columns: `pa_name`, `pa_category` and
#' `pa_group`, plus all original attributes.
#'
#' @param aoi An `sf`/`sfc` polygon or point set (any CRS) defining the area of
#'   interest, e.g. an EOO hull, the union of AOO cells, or the occurrence
#'   points.
#' @param src Optional path/URL to a local protected-area vector file. When
#'   supplied WDPA is not queried and the file is filtered to the AOI bounding
#'   box.
#' @param cache,cache_dir Cache the WDPA result on disk (default `TRUE`; folder
#'   `mappingAS_pa_cache` under `tempdir()`).
#' @param quiet Passed to [sf::st_read()] (default `TRUE`).
#' @return An `sf` of protected-area polygons in EPSG:4326 (possibly with zero
#'   rows when none intersects the AOI), or `NULL` if the source could not be
#'   read.
#' @seealso [wdpa_areas()]
#' @examples
#' \dontrun{
#' occ <- read_occurrences(system.file("extdata", "example_occurrences.csv",
#'                                     package = "mappingAS"))
#' sp1 <- occ[occ$species == occ$species[1], ]
#' pas <- protected_areas(calc_eoo(sp1)$hull)
#' unique(pas$pa_name)
#' }
#' @export
protected_areas <- function(aoi, src = NULL,
                            cache = TRUE, cache_dir = NULL, quiet = TRUE) {
  if (is.null(src)) {
    return(wdpa_areas(aoi, cache = cache, cache_dir = cache_dir, quiet = quiet))
  }
  g <- sf::st_geometry(aoi)
  if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
  bb <- sf::st_bbox(sf::st_transform(g, 4326))

  pa <- .pa_read_local(src, bb)
  if (is.null(pa)) return(NULL)
  if (nrow(pa) == 0) return(pa)
  pa <- sf::st_make_valid(sf::st_transform(pa, 4326))
  .pa_standardise(pa)
}

#' Overlap between a species' range and federal Protected areas
#'
#' Given a species' occurrence points and (optionally) its EOO and AOO, plus a
#' UC layer from [protected_areas()], computes how much of the range is legally
#' protected: the share of occurrences inside UCs, the percentage/area of the
#' EOO and AOO that fall within UCs, and the list of UCs touched.
#'
#' Percentages are computed on the species' equal-area projection so ratios are
#' exact; the reported `eoo_km2`/`aoo_km2` inside UCs are derived from the
#' headline EOO/AOO so they stay consistent with [assess_species()]. The AOO
#' overlap follows the grid nature of the metric: an occupied 2-km cell counts
#' as protected when it intersects any UC.
#'
#' @param points An `sf` of POINT geometries (one species).
#' @param pa A UC `sf` from [protected_areas()] (standardised columns).
#' @param eoo,aoo The `eoo`/`aoo` sublists produced by [assess_species()] (or
#'   [calc_eoo()]/[calc_aoo()]). Either may be `NULL`.
#' @return A list: `n_occ`, `n_occ_in`, `occ_pct`; `eoo_pct`, `eoo_km2`;
#'   `aoo_pct`, `aoo_km2`, `aoo_cells_in`; `n_uc`; `list` (per-UC data.frame)
#'   and `layer` (the UC `sf`).
#' @export
summarise_protected <- function(points, pa, eoo = NULL, aoo = NULL) {
  .assert_points(points, "points")
  if (is.null(pa) || !inherits(pa, "sf") || nrow(pa) == 0) return(.pa_empty())

  pa   <- sf::st_make_valid(sf::st_transform(pa, 4326))
  if (is.null(pa$pa_name)) pa <- .pa_standardise(pa)
  pa_u <- .st_union_quiet(sf::st_geometry(pa))
  crs  <- .pa_or(if (!is.null(eoo)) eoo$crs_laea,
                 .pa_or(if (!is.null(aoo)) aoo$crs_laea, laea_crs(points)))

  # occurrences inside UCs
  pts    <- sf::st_transform(sf::st_geometry(points), 4326)
  inside <- lengths(suppressMessages(sf::st_intersects(pts, pa_u))) > 0
  n_occ  <- length(pts)
  n_in   <- sum(inside)
  occ_pct <- if (n_occ > 0) 100 * n_in / n_occ else NA_real_

  # EOO in UC (equal-area ratio; km2 derived from headline EOO)
  eoo_pct <- eoo_km2 <- NA_real_
  if (!is.null(eoo) && !is.null(eoo$hull) && isTRUE(is.finite(eoo$area_km2))) {
    h  <- sf::st_transform(eoo$hull, crs)
    u  <- sf::st_transform(pa_u, crs)
    it <- suppressWarnings(sf::st_intersection(h, u))
    a_h <- as.numeric(sum(sf::st_area(h)))
    a_i <- if (length(it)) as.numeric(sum(sf::st_area(it))) else 0
    eoo_pct <- if (a_h > 0) 100 * a_i / a_h else NA_real_
    eoo_km2 <- eoo$area_km2 * eoo_pct / 100
  }

  # AOO in UC (occupied 2-km cells intersecting any UC)
  aoo_pct <- aoo_km2 <- NA_real_
  aoo_cells_in <- NA_integer_
  if (!is.null(aoo) && !is.null(aoo$cells) && length(aoo$cells)) {
    cells <- sf::st_transform(aoo$cells, 4326)
    hit   <- lengths(suppressMessages(sf::st_intersects(cells, pa_u))) > 0
    n_cells <- length(cells)
    aoo_cells_in <- sum(hit)
    aoo_pct <- if (n_cells > 0) 100 * aoo_cells_in / n_cells else NA_real_
    aoo_km2 <- aoo_cells_in * (aoo$cell_km^2)
  }

  lst <- .pa_list(pa, points, eoo, crs)
  list(n_occ = n_occ, n_occ_in = n_in, occ_pct = occ_pct,
       eoo_pct = eoo_pct, eoo_km2 = eoo_km2,
       aoo_pct = aoo_pct, aoo_km2 = aoo_km2, aoo_cells_in = aoo_cells_in,
       n_uc = nrow(lst), list = lst, layer = pa)
}

#' Per-species table of Protected areas overlapping the range
#'
#' Long `data.frame` (one row per species x UC) listing each federal UC that
#' contains occurrences of, or overlaps the EOO of, each assessed species.
#' Requires `assess_species(..., protected = TRUE)`.
#'
#' @param assessment A `geoconv_assessment` from [assess_species()].
#' @param species Optional species name(s); `NULL` (default) uses all.
#' @return A `data.frame` with `species`, `pa_name`, `pa_category`, `pa_group`,
#'   `n_occ` (occurrences of that species inside the UC) and `overlap_eoo_km2`.
#' @export
pa_table <- function(assessment, species = NULL) {
  stopifnot(inherits(assessment, "geoconv_assessment"))
  d <- assessment$detail
  sp <- if (is.null(species)) names(d) else species
  parts <- list(); k <- 0L
  for (s in sp) {
    pa <- d[[s]]$pa
    if (is.null(pa) || is.null(pa$list) || !nrow(pa$list)) next
    k <- k + 1L
    parts[[k]] <- cbind(species = s, pa$list, stringsAsFactors = FALSE)
  }
  res <- do.call(rbind, parts)
  if (is.null(res)) {
    warning("No protected-area data (run assess_species(..., protected = TRUE)).",
            call. = FALSE)
    return(data.frame())
  }
  rownames(res) <- NULL
  res
}

# ---- internal helpers -------------------------------------------------------

#' @keywords internal
#' @noRd
.pa_or <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

#' @keywords internal
#' @noRd
.pa_empty <- function() {
  list(n_occ = 0L, n_occ_in = 0L, occ_pct = NA_real_,
       eoo_pct = NA_real_, eoo_km2 = NA_real_,
       aoo_pct = NA_real_, aoo_km2 = NA_real_, aoo_cells_in = NA_integer_,
       n_uc = 0L, list = .pa_empty_list(), layer = NULL)
}

#' @keywords internal
#' @noRd
.pa_empty_list <- function() {
  data.frame(pa_name = character(0), pa_category = character(0),
             pa_group = character(0), n_occ = integer(0),
             overlap_eoo_km2 = numeric(0), stringsAsFactors = FALSE)
}

#' @keywords internal
#' @noRd
.pa_read_local <- function(src, bb) {
  # Verifica se e o RDS interno ou um shapefile/gpkg externo
  if (grepl("\\.rds$", src, ignore.case = TRUE)) {
    pa <- readRDS(src)
  } else {
    pa <- sf::st_read(src, quiet = TRUE)
  }
  
  pa <- sf::st_make_valid(sf::st_transform(pa, 4326))
  box <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = bb[["xmin"]], ymin = bb[["ymin"]],
      xmax = bb[["xmax"]], ymax = bb[["ymax"]]), crs = 4326))
  
  # Filtra apenas as UCs que tocam a Bounding Box da area de interesse
  pa[lengths(suppressMessages(sf::st_intersects(pa, box))) > 0, , drop = FALSE]
}

#' @keywords internal
#' @noRd
.pa_standardise <- function(pa) {
  nms <- names(pa)
  nm <- .match_col(nms, c("nome_uc", "nome", "nom_unidad", "no_uc", "nomeuc",
                          "denominacao", "nome_uc1", "name"))
  ct <- .match_col(nms, c("categoria", "categori3", "cat_uc", "categoria_uc",
                          "tipo", "classe"))
  gp <- .match_col(nms, c("grupo", "grupo_uc", "gr_uc", "tipo_uc", "protecao",
                          "grupo2"))
  pa$pa_name     <- if (!is.na(nm)) as.character(pa[[nm]])
                    else paste0("UC_", seq_len(nrow(pa)))
  pa$pa_category <- if (!is.na(ct)) as.character(pa[[ct]]) else NA_character_
  pa$pa_group    <- if (!is.na(gp)) as.character(pa[[gp]]) else NA_character_
  pa
}

#' @keywords internal
#' @noRd
.pa_list <- function(pa, points, eoo, crs) {
  if (is.null(pa) || nrow(pa) == 0) return(.pa_empty_list())
  pts   <- sf::st_transform(sf::st_geometry(points), 4326)
  n_occ <- lengths(suppressMessages(sf::st_intersects(sf::st_geometry(pa), pts)))

  ov <- rep(NA_real_, nrow(pa))
  if (!is.null(eoo) && !is.null(eoo$hull)) {
    h  <- sf::st_transform(eoo$hull, crs)
    pg <- sf::st_transform(sf::st_geometry(pa), crs)
    for (i in seq_len(nrow(pa))) {
      it <- suppressWarnings(sf::st_intersection(pg[i], h))
      ov[i] <- if (length(it)) as.numeric(sum(sf::st_area(it))) / 1e6 else 0
    }
  }
  keep <- n_occ > 0 | (is.finite(ov) & ov > 0)
  out <- data.frame(
    pa_name = pa$pa_name, pa_category = pa$pa_category, pa_group = pa$pa_group,
    n_occ = as.integer(n_occ), overlap_eoo_km2 = round(ov, 3),
    stringsAsFactors = FALSE)[keep, , drop = FALSE]
  out[order(-out$n_occ, -out$overlap_eoo_km2), , drop = FALSE]
}

#' @keywords internal
#' @noRd
.pa_cache_file <- function(cache, cache_dir, key) {
  if (!isTRUE(cache)) return(NULL)
  if (is.null(cache_dir)) cache_dir <- file.path(tempdir(), "mappingAS_pa_cache")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  file.path(cache_dir, paste0(substr(key, 1, 180), ".gpkg"))
}

#' Glue used by [assess_species()] to attach protected-area overlap to a species
#' @keywords internal
#' @noRd
.protected_for <- function(points, eoo, aoo, src = NULL, verbose = TRUE) {
  aoi <- if (!is.null(eoo) && !is.null(eoo$hull)) eoo$hull
         else sf::st_as_sfc(sf::st_bbox(sf::st_geometry(points)))
  pa <- tryCatch(
    protected_areas(aoi, src = src),
    error = function(e) {
      warning(sprintf("Protected-area overlap failed: %s", conditionMessage(e)),
              call. = FALSE)
      NULL
    })
  if (is.null(pa)) return(NULL)                      # read failed -> NA metrics
  if (nrow(pa) == 0) {                               # genuinely no PA in range
    if (verbose) message("  PA: no protected area intersects the range.")
    out <- .pa_empty(); out$occ_pct <- 0; out$eoo_pct <- 0; out$aoo_pct <- 0
    out$eoo_km2 <- 0; out$aoo_km2 <- 0; out$aoo_cells_in <- 0L
    out$eoo_nat_uc_pct <- 0; out$aoo_nat_uc_pct <- 0
    out$eoo_alt_uc_pct <- 0; out$aoo_alt_uc_pct <- 0
    out$eoo_nat_uc_pct_in <- NA_real_; out$aoo_nat_uc_pct_in <- NA_real_
    return(out)
  }
  summarise_protected(points, pa, eoo = eoo, aoo = aoo)
}

#' Natural habitat that is also inside UCs, reusing summarise_conversion()
#' @keywords internal
#' @noRd
.nat_in_uc <- function(range_geom, uc_union, conv_range, year, collection,
                       initiative, backend, src, water_in_denominator, verbose,
                       label) {
  na3 <- list(nat_km2 = NA_real_, alt_km2 = NA_real_,
              nat_pct_total = NA_real_, alt_pct_total = NA_real_,
              nat_pct_uc = NA_real_)
  if (is.null(range_geom) || is.null(uc_union) || is.null(conv_range)) return(na3)
  inter <- tryCatch(
    suppressWarnings(sf::st_intersection(sf::st_geometry(range_geom),
                                         sf::st_geometry(uc_union))),
    error = function(e) NULL)
  if (is.null(inter) || !length(inter) || all(sf::st_is_empty(inter)))
    return(utils::modifyList(na3, list(nat_km2 = 0, alt_km2 = 0,
                                       nat_pct_total = 0, alt_pct_total = 0)))
  cu <- .conversion_for(inter, year, collection, initiative, backend, src,
                        water_in_denominator, verbose, paste0(label, "/UC"))
  if (is.null(cu)) return(na3)
  wd   <- isTRUE(water_in_denominator)
  nat  <- cu$natural_km2 + if (wd) cu$water_km2 else 0
  alt  <- cu$anthropic_km2
  terr <- conv_range$natural_km2 + conv_range$anthropic_km2 +
          if (wd) conv_range$water_km2 else 0
  list(
    nat_km2 = nat, alt_km2 = alt,
    nat_pct_total = if (isTRUE(terr > 0)) 100 * nat / terr else NA_real_,
    alt_pct_total = if (isTRUE(terr > 0)) 100 * alt / terr else NA_real_,
    nat_pct_uc = cu$natural_pct
  )
}