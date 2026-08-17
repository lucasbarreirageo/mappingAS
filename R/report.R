#' Narrative conservation-assessment report for one species
#'
#' Builds a written, interpretive summary of a species' preliminary Criterion B
#' screening from an \code{\link{assess_species}} result. Beyond restating the
#' headline numbers, the text \emph{interprets} them: the EOO/AOO composition
#' divergence, the effectiveness (not just the extent) of protected-area
#' overlap, and an honest framing of continuing decline (subcriterion b). When
#' the land-cover and/or fire time series are supplied it also analyses their
#' temporal trend. Habitat conversion, fire and protected-area passages appear
#' only when those modules were run. It can be returned as HTML (on-screen
#' preview), plain text, or a Word \code{.docx} file (via \pkg{officer}) - the
#' format used by the Shiny app's Report tab.
#'
#' @param assessment A \code{geoconv_assessment} from \code{\link{assess_species}}.
#' @param species Species name (default: the first assessed).
#' @param lang Report language: \code{"en"} (default) or \code{"pt"}.
#' @param output One of \code{"html"} (default), \code{"text"} or \code{"docx"}.
#' @param file Target path for \code{output = "docx"} (required in that case).
#' @param cover_series Optional land-cover time series for this species (as from
#'   \code{\link{timeseries_for_species}} / \code{\link{cover_timeseries}}, with
#'   \code{year}, \code{pct} and \code{group} columns). When supplied, a
#'   temporal trend of habitat conversion is added.
#' @param fire_series Optional burned-area time series (as from
#'   \code{\link{fire_timeseries_for_species}} / \code{\link{fire_timeseries}},
#'   with \code{year} and \code{burned_pct}). When supplied, the fire regime is
#'   characterised over time.
#' @param figures Logical; for \code{output = "docx"} only, embed the supporting
#'   figures (composition, protection and the land-cover/fire time-series charts)
#'   in the Word document (default \code{FALSE}). Ignored for other outputs.
#' @return For \code{"html"}/\code{"text"}, a length-one character string. For
#'   \code{"docx"}, the \code{file} path (written as a side effect), invisibly.
#' @examples
#' \dontrun{
#' res <- assess_species(read_occurrences("occ.csv"), fire = TRUE, protected = TRUE)
#' cat(assessment_report(res, output = "text"))
#' ts <- timeseries_for_species(res, range = "eoo", by = "group")
#' assessment_report(res, output = "docx", file = "report.docx", cover_series = ts)
#' }
#' @export
assessment_report <- function(assessment, species = NULL,
                              lang = c("en", "pt"),
                              output = c("html", "text", "docx"),
                              file = NULL,
                              cover_series = NULL, fire_series = NULL,
                              figures = FALSE) {
  lang <- match.arg(lang)
  output <- match.arg(output)
  stopifnot(inherits(assessment, "geoconv_assessment"))
  if (is.null(assessment$summary) || nrow(assessment$summary) == 0)
    stop("The assessment has no results to report.", call. = FALSE)
  if (is.null(species)) species <- assessment$summary$species[1]

  b <- .report_build(assessment, species, lang, cover_series, fire_series,
                     figures = isTRUE(figures) && output == "docx")

  switch(output,
         html = .report_to_html(b),
         text = .report_to_text(b),
         docx = .report_to_docx(b, file))
}

# Normalise a series argument (a single data frame, or a list of them for
# several ranges) to a plain list of data frames.
.as_series_list <- function(x) {
  if (is.null(x)) return(list())
  if (is.data.frame(x)) return(list(x))
  if (is.list(x)) return(Filter(is.data.frame, x))
  list()
}

# Per-year terrestrial converted % and trend summary from a cover time series.
.cover_trend <- function(ts) {
  if (is.null(ts) || !is.data.frame(ts) || !nrow(ts)) return(NULL)
  if (!all(c("year", "pct") %in% names(ts)) || !"group" %in% names(ts)) return(NULL)
  yrs <- sort(unique(ts$year))
  if (length(yrs) < 2) return(NULL)
  conv <- vapply(yrs, function(y) {
    d <- ts[ts$year == y, , drop = FALSE]
    anth <- sum(d$pct[d$group == "anthropic"], na.rm = TRUE)
    nat  <- sum(d$pct[d$group == "natural"], na.rm = TRUE)
    if (!is.finite(anth + nat) || (anth + nat) <= 0) NA_real_ else 100 * anth / (anth + nat)
  }, numeric(1))
  y0 <- yrs[1]; y1 <- yrs[length(yrs)]
  urb <- NULL
  is_urban <- if ("code" %in% names(ts)) ts$code == 24 else
    grepl("urban", ts$label %||% "", ignore.case = TRUE)
  if (any(is_urban, na.rm = TRUE)) {
    u0 <- sum(ts$pct[is_urban & ts$year == y0], na.rm = TRUE)
    u1 <- sum(ts$pct[is_urban & ts$year == y1], na.rm = TRUE)
    urb <- list(u0 = u0, u1 = u1)
  }
  list(range = attr(ts, "range"), y0 = y0, y1 = y1,
       c0 = conv[1], c1 = conv[length(conv)],
       delta = conv[length(conv)] - conv[1], urban = urb)
}

# Per-class linear-regression trends from a cover time series (the textual
# counterpart of plot_class_trendline / ggtrendline). For each class (or group)
# it fits pct ~ year and keeps slope (percentage points per year), R-squared,
# p-value and the first/last values, returning the `top_n` classes with the
# strongest change (largest absolute slope).
.cover_class_trends <- function(ts, top_n = 3L) {
  if (is.null(ts) || !is.data.frame(ts) || !nrow(ts)) return(NULL)
  if (!all(c("year", "pct", "label") %in% names(ts))) return(NULL)
  yrs <- sort(unique(ts$year))
  if (length(yrs) < 3) return(NULL)
  labs <- unique(ts$label)
  rows <- lapply(labs, function(L) {
    d <- ts[ts$label == L, c("year", "pct"), drop = FALSE]
    d <- d[is.finite(d$year) & is.finite(d$pct), , drop = FALSE]
    d <- d[order(d$year), , drop = FALSE]
    if (nrow(d) < 3 || length(unique(d$pct)) < 2) return(NULL)
    fit <- tryCatch(stats::lm(pct ~ year, data = d), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    cf <- stats::coef(fit); sm <- suppressWarnings(summary(fit))
    pv <- tryCatch(stats::pf(sm$fstatistic[1L], sm$fstatistic[2L],
                             sm$fstatistic[3L], lower.tail = FALSE),
                   error = function(e) NA_real_)
    en <- if ("class_en" %in% names(ts)) ts$class_en[ts$label == L][1] else L
    data.frame(label = L, class_en = en, slope = unname(cf[2L]),
               r2 = sm$r.squared, p = as.numeric(pv),
               first = d$pct[1L], last = d$pct[nrow(d)],
               stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(df) || !nrow(df)) return(NULL)
  df <- df[order(-abs(df$slope)), , drop = FALSE]
  list(range = attr(ts, "range"), y0 = min(yrs), y1 = max(yrs),
       top = utils::head(df, top_n))
}

# Fire-regime trend summary from a burned-area time series.
.fire_trend <- function(ts) {
  if (is.null(ts) || !is.data.frame(ts) || !nrow(ts)) return(NULL)
  if (!all(c("year", "burned_pct") %in% names(ts))) return(NULL)
  d <- ts[is.finite(ts$burned_pct), , drop = FALSE]
  if (nrow(d) < 2) return(NULL)
  d <- d[order(d$year), , drop = FALSE]
  top <- utils::head(d[order(d$burned_pct, decreasing = TRUE), , drop = FALSE], 3)
  list(range = attr(ts, "range"), y0 = d$year[1], y1 = d$year[nrow(d)],
       n = nrow(d), mean = mean(d$burned_pct, na.rm = TRUE),
       last_year = d$year[nrow(d)], last_val = d$burned_pct[nrow(d)],
       peak_years = top$year, peak_vals = top$burned_pct)
}

# Assemble the report content (language-aware) as a plain list, so the HTML,
# text and docx renderers all draw from one source.
.report_build <- function(assessment, species, lang,
                          cover_series = NULL, fire_series = NULL,
                          figures = FALSE) {
  L <- function(en, pt) if (lang == "en") en else pt
  s <- assessment$summary
  r <- s[s$species == species, , drop = FALSE]
  if (nrow(r) == 0)
    stop("Species not found in assessment: ", species, call. = FALSE)
  r <- r[1, , drop = FALSE]
  st  <- assessment$settings
  det <- assessment$detail[[species]]
  # Label the land-cover source actually used for this species (Sentinel-2/Esri
  # when the range fell back outside MapBiomas coverage).
  lc_ini <- (if (!is.null(det)) det$initiative else NULL) %||%
    st$initiative %||% "brazil"
  mb_label <- tryCatch(.mb_resolve_initiative(lc_ini)$label,
                       error = function(e) "MapBiomas Brazil")
  lc_is_esri <- identical(as.character(lc_ini), "sentinel2")
  lc_coll <- if (!is.null(r$mapbiomas_collection)) r$mapbiomas_collection
             else st$collection
  lc_year <- if (!is.null(r$mapbiomas_year)) r$mapbiomas_year else st$year
  # Source descriptor for prose: "<product> Collection <n>", or just the
  # product name when there is no collection (the Sentinel-2/Esri layer).
  lc_src_desc <- if (is.null(lc_coll) ||
                     (length(lc_coll) == 1 && is.na(lc_coll)))
    mb_label else paste0(mb_label, L(" Collection ", " Colecao "), lc_coll)
  pa  <- if (!is.null(det)) det$pa else NULL
  # cover_series / fire_series may be a single data frame (one range) or a list
  # of them (e.g. EOO and AOO): normalise to a list of per-range trends.
  cover_list <- .as_series_list(cover_series)
  fire_list  <- .as_series_list(fire_series)
  cover_trs  <- Filter(Negate(is.null), lapply(cover_list, .cover_trend))
  cover_cls  <- Filter(Negate(is.null), lapply(cover_list, .cover_class_trends))
  fire_trs   <- Filter(Negate(is.null), lapply(fire_list,  .fire_trend))
  pick_eoo   <- function(trs) {
    e <- Find(function(t) identical(tolower(t$range %||% ""), "eoo"), trs)
    if (!is.null(e)) e else if (length(trs)) trs[[1]] else NULL
  }

  fmt_km  <- function(x) if (is.null(x) || is.na(x))
    L("not available", "nao disponivel") else
    paste0(formatC(x, format = "f", big.mark = ",", digits = 2), " km<sup>2</sup>")
  fmt_pct <- function(x) if (is.null(x) || is.na(x)) "&mdash;" else sprintf("%.1f%%", x)
  fmt_int <- function(x) if (is.null(x) || is.na(x)) "&mdash;" else
    formatC(x, format = "d", big.mark = ",")
  cat_txt <- function(x) if (is.null(x) || is.na(x)) L("not applicable", "nao aplicavel") else x
  cat_pref <- function(x) {
    if (is.null(x) || is.na(x)) return(NA_character_)
    m <- regmatches(x, regexpr("^(CR|EN|VU|NT|LC)", x)); if (length(m)) m else NA_character_
  }

  sections <- list()
  add <- function(h, p) sections[[length(sections) + 1L]] <<- list(h = h, p = p)

  # --- Overview / executive status ---
  add(
    L("Overview and executive status", "Visao geral e status executivo"),
    c(sprintf(L(
      "This document reports a preliminary, size-based screening of %s against Criterion B of the IUCN Red List Categories and Criteria. It is intended for screening only and does not constitute a formal Red List assessment.",
      "Este documento apresenta uma triagem preliminar, baseada no tamanho da distribuicao, de %s segundo o Criterio B das Categorias e Criterios da Lista Vermelha da IUCN. Destina-se apenas a triagem e nao constitui uma avaliacao formal da Lista Vermelha."),
      .sp_html(species)),
      sprintf(L(
        "It is based on %s occurrence records (%s with unique coordinates). The combined provisional category, from range size alone, is <b>%s</b>.",
        "Baseia-se em %s registros de ocorrencia (%s com coordenadas unicas). A categoria provisoria combinada, apenas pelo tamanho da distribuicao, e <b>%s</b>."),
        fmt_int(r$n_records), fmt_int(r$n_unique), cat_txt(r$provisional_cat)),
      L("The category is driven not by range size alone but by its combination with a spatially concentrated pattern of occupation and active threat vectors (below). The most defensible full-criterion formulation would pair this size-based category with subconditions (a) few locations and (b) continuing decline in habitat quality, contingent on the validation set out in the recommendations.",
        "A categoria decorre nao apenas do tamanho da distribuicao, mas da sua combinacao com um padrao de ocupacao espacialmente concentrado e vetores de ameaca ativos (adiante). A formulacao de criterio mais defensavel associaria essa categoria de tamanho as subcondicoes (a) poucas localidades e (b) declinio continuo na qualidade do habitat, condicionada as validacoes indicadas nas recomendacoes."))
  )

  # --- Range metrics (EOO/AOO) ---
  range_p <- c(sprintf(L(
    "The Extent of Occurrence (EOO), estimated as the area of the minimum convex polygon enclosing all occurrence points, is %s. The Area of Occupancy (AOO), the number of occupied %g&times;%g km cells scaled to the IUCN 2&times;2 km reference, is %s (%s occupied cells).",
    "A Extensao de Ocorrencia (EOO), estimada como a area do poligono convexo minimo que engloba todos os pontos, e %s. A Area de Ocupacao (AOO), o numero de celulas ocupadas de %g&times;%g km na escala de referencia IUCN de 2&times;2 km, e %s (%s celulas ocupadas)."),
    fmt_km(r$eoo_km2), st$cell_km, st$cell_km, fmt_km(r$aoo_km2), fmt_int(r$aoo_cells)),
    L("Both areas were measured on a data-centred Lambert Azimuthal Equal-Area projection, in line with the IUCN guidelines.",
      "Ambas as areas foram medidas em projecao equivalente Lambert Azimutal centrada nos dados, conforme as diretrizes da IUCN."))
  if (isTRUE(st$mapbiomas) && !is.na(r$eoo_natural_pct) && !is.na(r$aoo_natural_pct) &&
      (r$aoo_natural_pct - r$eoo_natural_pct) > 5) {
    range_p <- c(range_p, sprintf(L(
      "The occupied core is better preserved than the range as a whole: natural cover is %s within the AOO against %s within the EOO. This asymmetry indicates that the convex hull incorporates converted land interposed between occurrence nuclei, while the occupied cells concentrate in less accessible, typically higher terrain - a known property of the minimum convex polygon that should temper a literal reading of the EOO composition.",
      "O nucleo ocupado esta mais conservado do que a distribuicao como um todo: a cobertura natural e de %s na AOO contra %s na EOO. Essa assimetria indica que o casco convexo incorpora areas convertidas interpostas entre os nucleos de ocorrencia, enquanto as celulas ocupadas se concentram em terreno menos acessivel, tipicamente mais elevado - propriedade conhecida do poligono convexo minimo que deve moderar a leitura literal da composicao da EOO."),
      fmt_pct(r$aoo_natural_pct), fmt_pct(r$eoo_natural_pct)))
  }
  add(L("Geographic range metrics", "Metricas de distribuicao geografica"), range_p)

  # --- Provisional category ---
  cat_p <- c(sprintf(L(
    "Under the size thresholds of Criterion B, the EOO corresponds to %s (sub-criterion B1) and the AOO to %s (sub-criterion B2). The combined provisional category is <b>%s</b>.",
    "Segundo os limiares de tamanho do Criterio B, a EOO corresponde a %s (subcriterio B1) e a AOO a %s (subcriterio B2). A categoria provisoria combinada e <b>%s</b>."),
    cat_txt(r$eoo_cat_B1), cat_txt(r$aoo_cat_B2), cat_txt(r$provisional_cat)),
    L("Thresholds are CR / EN / VU for EOO &lt; 100 / 5,000 / 20,000 km<sup>2</sup> (B1) and AOO &lt; 10 / 500 / 2,000 km<sup>2</sup> (B2). These reflect range size only and, by themselves, do not qualify a taxon for listing: a formal Criterion B assessment additionally requires at least two of the subconditions (severe fragmentation or few locations, continuing decline, and extreme fluctuation).",
      "Os limiares sao CR / EN / VU para EOO &lt; 100 / 5.000 / 20.000 km<sup>2</sup> (B1) e AOO &lt; 10 / 500 / 2.000 km<sup>2</sup> (B2). Refletem apenas o tamanho da distribuicao e, isoladamente, nao qualificam um taxon: uma avaliacao formal do Criterio B exige ainda pelo menos duas das subcondicoes (fragmentacao severa ou poucas localidades, declinio continuo e flutuacao extrema)."))
  if (!is.na(cat_pref(r$eoo_cat_B1)) && identical(cat_pref(r$eoo_cat_B1), cat_pref(r$aoo_cat_B2))) {
    cat_p <- c(cat_p, L(
      "Both subcriteria converge on the same category, which strengthens the provisional attribution and makes it robust to moderate changes in either metric.",
      "Ambos os subcriterios convergem para a mesma categoria, o que reforca a atribuicao provisoria e a torna robusta a variacoes moderadas de qualquer das metricas."))
  }
  add(L("Provisional Criterion B category", "Categoria provisoria (Criterio B)"), cat_p)

  # --- Habitat conversion (snapshot + trend) ---
  if (isTRUE(st$mapbiomas) &&
      (!is.na(r$eoo_converted_pct) || !is.na(r$aoo_converted_pct))) {
    conv_p <- sprintf(L(
      "Based on %s (%s), converted (anthropic) land cover accounts for %s of the terrestrial EOO and %s of the AOO; the remaining natural habitat is %s (EOO) and %s (AOO). Water and unobserved areas are excluded from the terrestrial denominator.",
      "Com base em %s (%s), a cobertura convertida (antropica) corresponde a %s da EOO terrestre e a %s da AOO; o habitat natural remanescente e de %s (EOO) e %s (AOO). Corpos d'agua e areas nao observadas sao excluidos do denominador terrestre."),
      lc_src_desc, lc_year,
      fmt_pct(r$eoo_converted_pct), fmt_pct(r$aoo_converted_pct),
      fmt_pct(r$eoo_natural_pct), fmt_pct(r$aoo_natural_pct))
    if (length(cover_trs)) {
      for (ct in cover_trs) {
        rlab <- if (is.null(ct$range)) L("range", "distribuicao") else toupper(ct$range)
        conv_p <- c(conv_p, sprintf(L(
          "Over %d-%d, the terrestrial converted fraction of the %s moved from %s to %s (a change of %+.1f percentage points).",
          "Entre %d e %d, a fracao convertida (terrestre) da %s passou de %s para %s (variacao de %+.1f pontos percentuais)."),
          ct$y0, ct$y1, rlab, fmt_pct(ct$c0), fmt_pct(ct$c1), ct$delta))
      }
      main_ct <- pick_eoo(cover_trs)
      if (is.finite(main_ct$delta) && main_ct$delta < -1) {
        conv_p <- c(conv_p, L(
          "This net reduction is consistent with the Atlantic Forest 'forest transition' (pasture abandonment and secondary-forest regrowth). Aggregate natural-cover gain can, however, mask decline in the specific, often azonal habitat on which a narrow endemic depends (for example high-altitude grassland or rocky outcrops), which secondary forest does not recreate; continuing decline should therefore be assessed at the level of habitat quality rather than gross natural area.",
          "Essa reducao liquida e consistente com a 'transicao florestal' da Mata Atlantica (abandono de pastagens e regeneracao de floresta secundaria). O ganho agregado de vegetacao natural pode, contudo, mascarar declinio no habitat especifico, muitas vezes azonal, do qual um endemismo restrito depende (por exemplo, campos de altitude ou afloramentos rochosos), que a floresta secundaria nao recria; o declinio continuo deve, portanto, ser avaliado no nivel da qualidade do habitat, e nao da area natural bruta."))
      } else if (is.finite(main_ct$delta) && main_ct$delta > 1) {
        conv_p <- c(conv_p, L(
          "The net increase indicates ongoing habitat loss over the period, consistent with a continuing decline in the area and quality of habitat (subcriterion b).",
          "O aumento liquido indica perda continua de habitat no periodo, consistente com declinio continuo na area e qualidade do habitat (subcriterio b)."))
      } else {
        conv_p <- c(conv_p, L(
          "The converted fraction was broadly stable over the period; any continuing decline is better evaluated at the level of specific habitat quality than of aggregate natural area.",
          "A fracao convertida manteve-se aproximadamente estavel no periodo; eventual declinio continuo deve ser avaliado no nivel da qualidade do habitat especifico, e nao da area natural agregada."))
      }
      if (!is.null(main_ct$urban) && (main_ct$urban$u1 - main_ct$urban$u0) > 0.1) {
        conv_p <- c(conv_p, sprintf(L(
          "In the same window, urbanized cover rose from %s to %s of the extent - an effectively irreversible form of conversion concentrated near occupied areas.",
          "Na mesma janela, a area urbanizada aumentou de %s para %s da distribuicao - forma de conversao praticamente irreversivel e concentrada proximo as areas ocupadas."),
          fmt_pct(main_ct$urban$u0), fmt_pct(main_ct$urban$u1)))
      }
    } else {
      conv_p <- c(conv_p, L(
        "A land-cover time series was not supplied; computing it would allow the temporal trend of conversion (and any continuing decline) to be assessed.",
        "Nao foi fornecida a serie temporal de cobertura; calcula-la permitiria avaliar a tendencia temporal da conversao (e eventual declinio continuo)."))
    }
    # Per-class regression trends (the textual counterpart of the ggtrendline
    # analysis in the app): the fastest-changing classes per extent.
    if (length(cover_cls)) {
      for (ctr in cover_cls) {
        rlab <- if (is.null(ctr$range)) L("range", "distribuicao") else toupper(ctr$range)
        top <- ctr$top
        parts <- character(nrow(top))
        for (i in seq_len(nrow(top))) {
          nm <- if (lang == "en") top$class_en[i] else top$label[i]
          parts[i] <- sprintf(
            "%s (%+.2f pp/%s, R\u00b2=%.2f, p=%.3g; %s\u2192%s)",
            nm, top$slope[i], L("yr", "ano"), top$r2[i], top$p[i],
            fmt_pct(top$first[i]), fmt_pct(top$last[i]))
        }
        conv_p <- c(conv_p, sprintf(L(
          "Per-class linear trends in the %s (%d-%d) - the classes changing fastest (slope in percentage points per year, with R\u00b2 and p-value from a least-squares fit of class share on year): %s.",
          "Tendencias lineares por classe na %s (%d-%d) - as classes que mais variam (inclinacao em pontos percentuais por ano, com R\u00b2 e p-valor de um ajuste de minimos quadrados da participacao da classe sobre o ano): %s."),
          rlab, ctr$y0, ctr$y1, paste(parts, collapse = "; ")))
      }
    }
    add(L("Habitat conversion", "Conversao de habitat"), conv_p)
  }

  # --- Fire (snapshot + trend) ---
  if (isTRUE(st$fire) &&
      (!is.null(r$eoo_burned_pct) && (!is.na(r$eoo_burned_pct) || !is.na(r$aoo_burned_pct)))) {
    fire_p <- sprintf(L(
      "MapBiomas Fire (Collection %s) indicates that %s of the EOO and %s of the AOO has burned at least once between 1985 and 2024.",
      "O MapBiomas Fogo (Colecao %s) indica que %s da EOO e %s da AOO queimaram ao menos uma vez entre 1985 e 2024."),
      r$fire_collection %||% st$fire_collection,
      fmt_pct(r$eoo_burned_pct), fmt_pct(r$aoo_burned_pct))
    if (length(fire_trs)) {
      for (ft in fire_trs) {
        rlab <- if (is.null(ft$range)) L("range", "distribuicao") else toupper(ft$range)
        fire_p <- c(fire_p, sprintf(L(
          "For the %s, across %d annual layers (%d-%d) the burned fraction averaged %.2f%% per year, with the largest events in %s (up to %.2f%%).",
          "Para a %s, ao longo de %d camadas anuais (%d-%d) a fracao queimada teve media de %.2f%% ao ano, com os maiores eventos em %s (ate %.2f%%)."),
          rlab, ft$n, ft$y0, ft$y1, ft$mean,
          paste(ft$peak_years, collapse = ", "), max(ft$peak_vals, na.rm = TRUE)))
      }
      fire_p <- c(fire_p, L(
        "The regime is episodic rather than monotonic, typically clustered in severe-drought years, and it reaches the occupied core; recurrent fire on a narrow, slow-recovering habitat is a plausible driver of continuing decline in habitat quality (subcriterion b(iii)).",
        "O regime e episodico, e nao monotonico, tipicamente concentrado em anos de seca severa, e alcanca o nucleo ocupado; o fogo recorrente sobre habitat estreito e de recuperacao lenta e vetor plausivel de declinio continuo na qualidade do habitat (subcriterio b(iii))."))
    } else {
      fire_p <- c(fire_p, L(
        "An annual fire series was not supplied; computing it would characterise the temporal regime and its recurrence.",
        "Nao foi fornecida a serie anual de fogo; calcula-la caracterizaria o regime temporal e sua recorrencia."))
    }
    add(L("Fire history", "Historico de fogo"), fire_p)
  }

  # --- Protected areas (snapshot + effectiveness) ---
  if (isTRUE(st$protected) && !is.null(r$occ_in_uc_pct)) {
    pa_p <- sprintf(L(
      "%s of the occurrences fall within protected areas; %s of the EOO and %s of the AOO overlap protected areas, across %s units (source: World Database on Protected Areas, WDPA).",
      "%s das ocorrencias estao dentro de areas protegidas; %s da EOO e %s da AOO sobrepoem areas protegidas, em %s unidades (fonte: World Database on Protected Areas, WDPA)."),
      fmt_pct(r$occ_in_uc_pct), fmt_pct(r$eoo_uc_pct), fmt_pct(r$aoo_uc_pct), fmt_int(r$n_uc))

    lst <- if (!is.null(pa)) pa$list else NULL
    if (!is.null(lst) && is.data.frame(lst) && nrow(lst) > 0 && "n_occ" %in% names(lst)) {
      occ <- lst[!is.na(lst$n_occ) & lst$n_occ > 0, , drop = FALSE]
      if (nrow(occ) > 0) {
        n_int <- if ("pa_group" %in% names(occ))
          sum(grepl("Integral", occ$pa_group, ignore.case = TRUE)) else NA_integer_
        k <- which.max(occ$n_occ)
        dom_name <- occ$pa_name[k]; dom_n <- occ$n_occ[k]
        tot_in <- sum(occ$n_occ, na.rm = TRUE)
        dom_share <- if (tot_in > 0) 100 * dom_n / tot_in else NA_real_
        pa_p <- c(pa_p, sprintf(L(
          "The effective protection rests on far fewer units than the headline count suggests: documented occurrences concentrate in %d unit(s)%s, and a single unit ('%s') holds %s of the in-UC records (%s). This concentration reduces the effective number of locations and exposes the taxon to spatially correlated catastrophic events (a severe fire season or a climatic extreme affecting one massif).",
          "A protecao efetiva repousa em muito menos unidades do que o numero total sugere: as ocorrencias documentadas concentram-se em %d unidade(s)%s, e uma unica unidade ('%s') detem %s dos registros em UC (%s). Essa concentracao reduz o numero efetivo de localidades e expoe o taxon a eventos catastroficos espacialmente correlacionados (uma temporada severa de fogo ou um extremo climatico afetando um mesmo macico)."),
          nrow(occ),
          if (is.na(n_int)) "" else sprintf(L(", %d of them strict-protection", ", %d delas de protecao integral"), n_int),
          dom_name, fmt_int(dom_n), fmt_pct(dom_share)))
      }
    }
    if (!is.null(r$eoo_nat_uc_pct) && !is.na(r$eoo_nat_uc_pct)) {
      pa_p <- c(pa_p, sprintf(L(
        "Protection of the occupied core is substantive: %s of the AOO is both natural and inside UCs, against %s of the EOO. Part of the EOO overlap lies in sustainable-use areas that permit human land use and offer weaker habitat security, so the network is necessary and relevant but not, by itself, sufficient to ensure long-term viability without active fire and visitor management.",
        "A protecao do nucleo ocupado e substantiva: %s da AOO e simultaneamente natural e dentro de UCs, contra %s da EOO. Parte da sobreposicao na EOO recai em areas de uso sustentavel, que admitem ocupacao humana e oferecem menor seguranca de habitat; a rede e, portanto, necessaria e relevante, mas nao suficiente por si so para garantir a viabilidade de longo prazo sem manejo ativo de fogo e da visitacao."),
        fmt_pct(r$aoo_nat_uc_pct), fmt_pct(r$eoo_nat_uc_pct)))
    }
    add(L("Overlap with protected areas and effectiveness", "Sobreposicao com areas protegidas e efetividade"), pa_p)
  }

  # --- Threats and continuing decline (synthesis) ---
  if (isTRUE(st$mapbiomas)) {
    drivers <- if (isTRUE(st$fire))
      L("irreversible urban expansion, a recurrent fire regime that penetrates the occupied core, and pressure on narrow azonal habitat",
        "a expansao urbana irreversivel, um regime de fogo recorrente que penetra o nucleo ocupado e a pressao sobre habitat azonal estreito")
    else
      L("irreversible urban expansion and pressure on narrow azonal habitat",
        "a expansao urbana irreversivel e a pressao sobre habitat azonal estreito")
    add(
      L("Threats and continuing decline", "Ameacas e declinio continuo"),
      sprintf(L(
        "The evidence points to a decline in habitat quality rather than in gross natural area: %s combine to erode the conditions the taxon depends on. Under Criterion B this is best instructed as continuing decline in the area, extent and quality of habitat (subcriterion b(iii)), evaluated at the level of the relevant - frequently azonal - habitat rather than of aggregate natural cover.",
        "As evidencias apontam para declinio na qualidade do habitat, e nao na area natural bruta: %s combinam-se para erodir as condicoes de que o taxon depende. Sob o Criterio B, isso e mais bem instruido como declinio continuo na area, extensao e qualidade do habitat (subcriterio b(iii)), avaliado no nivel do habitat relevante - frequentemente azonal - e nao da cobertura natural agregada."),
        drivers))
  }

  # --- Recommendations for formal assessment ---
  rec <- c(
    L("Delimit the number of locations and assess severe fragmentation (subcondition a), defining 'location' by the most plausible threat (e.g. a fire season or climatic extreme per massif).",
      "Delimitar o numero de localidades e avaliar fragmentacao severa (subcondicao a), definindo 'localidade' pela ameaca mais plausivel (p. ex., uma temporada de fogo ou extremo climatico por macico)."),
    L("Validate and refine the occurrences: confirm peripheral records, which drive the EOO, and expand sampling in undersampled terrain, which may bias the AOO downward.",
      "Validar e refinar as ocorrencias: confirmar registros perifericos, que determinam a EOO, e ampliar o esforco amostral em terreno subamostrado, que pode subestimar a AOO."),
    L("Obtain population size, structure and trend (supports subcriterion b(v) or Criteria C/D), currently absent from this screening.",
      "Levantar tamanho, estrutura e tendencia populacional (subsidia o subcriterio b(v) ou os Criterios C/D), ausentes nesta triagem."))
  if (isTRUE(st$mapbiomas)) {
    rec <- c(rec,
      L("Demonstrate continuing decline in habitat quality (b(iii)) at the level of the specific, often azonal habitat, not of aggregate natural cover.",
        "Demonstrar declinio continuo na qualidade do habitat (b(iii)) no nivel do habitat especifico, muitas vezes azonal, e nao da cobertura natural agregada."),
      L("Treat the land-cover classification uncertainty for grassland and rocky-outcrop mosaics with caution: these azonal classes are under-represented and of lower accuracy, so their figures are low-confidence.",
        "Tratar com cautela a incerteza de classificacao para mosaicos campestres e de afloramento rochoso: essas classes azonais sao subrepresentadas e de menor acuracia, de modo que seus valores sao de baixa confianca."))
  }
  rec <- c(rec,
    L("Submit the taxon to a formal assessment (CNCFlora / IUCN) and integrate this screening into protected-area management (fire and visitor management) where relevant.",
      "Submeter o taxon a avaliacao formal (CNCFlora / IUCN) e integrar esta triagem ao manejo das areas protegidas (gestao de fogo e de visitacao) quando pertinente."))
  add(L("Recommendations for formal assessment", "Recomendacoes para avaliacao formal"), rec)

  # --- Caveats ---
  caveats <- c(
    L("The provisional category reflects range-size thresholds only and must not be reported as a final IUCN category.",
      "A categoria provisoria reflete apenas limiares de tamanho e nao deve ser reportada como categoria final da IUCN."),
    L("The AOO is sensitive to grid origin and to sampling effort; the EOO can be inflated by outlying or erroneous records.",
      "A AOO e sensivel a origem da grade e ao esforco amostral; a EOO pode ser inflada por registros perifericos ou erroneos."))
  if (isTRUE(st$mapbiomas))
    caveats <- c(caveats,
      L("MapBiomas accuracy varies by class, biome and year; the classification of high-altitude grassland and rocky-outcrop mosaics is of lower accuracy and under-represents small azonal patches.",
        "A acuracia do MapBiomas varia por classe, bioma e ano; a classificacao de mosaicos campestres de altitude e de afloramento rochoso e de menor acuracia e subrepresenta pequenas manchas azonais."))
  add(L("Caveats and limitations", "Ressalvas e limitacoes"), caveats)

  # --- References ---
  refs <- c(
    "IUCN Standards and Petitions Committee (2024). Guidelines for Using the IUCN Red List Categories and Criteria, Version 16. Prepared by the Standards and Petitions Committee. https://www.iucnredlist.org/documents/RedListGuidelines.pdf",
    "IUCN (2012). IUCN Red List Categories and Criteria: Version 3.1, Second edition. IUCN, Gland, Switzerland and Cambridge, UK."
  )
  if (isTRUE(st$mapbiomas) && lc_is_esri)
    refs <- c(refs,
      "Karra, K. et al. (2021). Global land use/land cover with Sentinel-2 and deep learning. IGARSS 2021. doi:10.1109/IGARSS47720.2021.9553499",
      "Esri, Impact Observatory & Microsoft. Sentinel-2 10m Land Use/Land Cover Time Series. ArcGIS Living Atlas of the World. https://livingatlas.arcgis.com/landcoverexplorer/")
  else if (isTRUE(st$mapbiomas))
    refs <- c(refs,
      sprintf("Project %s - Collection %s of the Annual Series of Land Use and Land Cover Maps. https://mapbiomas.org", mb_label, lc_coll),
      "Souza, C.M. et al. (2020). Reconstructing Three Decades of Land Use and Land Cover Changes in Brazilian Biomes with Landsat Archive and Earth Engine. Remote Sensing 12(17): 2735. doi:10.3390/rs12172735")
  if (isTRUE(st$fire))
    refs <- c(refs,
      "Alencar, A. et al. (2022). Long-Term Landsat-Based Monthly Burned Area Dataset for the Brazilian Biomes Using Deep Learning. Remote Sensing 14(11): 2510. doi:10.3390/rs14112510")
  if (isTRUE(st$protected))
    refs <- c(refs,
      "UNEP-WCMC and IUCN. Protected Planet: The World Database on Protected Areas (WDPA). Cambridge, UK. https://www.protectedplanet.net/en")
  if (length(cover_cls))
    refs <- c(refs,
      "Mei, W. & Yu, G. (2022). ggtrendline: Add Trendline and Confidence Interval to 'ggplot2'. R package. https://CRAN.R-project.org/package=ggtrendline")

  # --- Support figures (only for docx; static ggplots embedded via officer) ---
  figs <- NULL
  if (isTRUE(figures) && requireNamespace("ggplot2", quietly = TRUE)) {
    figs <- list()
    # `h` is the embed height (inches): shorter for the horizontal bar charts so
    # they are not stretched, taller for the time-series area/line charts.
    add_fig <- function(cap, expr, h = 3.4) {
      g <- tryCatch(expr, error = function(e) NULL)
      if (inherits(g, "ggplot"))
        figs[[length(figs) + 1L]] <<- list(cap = cap, gg = g, h = h)
    }
    if (isTRUE(st$mapbiomas))
      add_fig(L("Habitat composition of the EOO and AOO (land cover).",
                "Composicao do habitat na EOO e na AOO (cobertura)."),
              plot_conversion(assessment, species = species, lang = lang), h = 2.6)
    if (isTRUE(st$protected) && !is.null(pa))
      add_fig(L("Range protection by protected areas (EOO and AOO).",
                "Protecao da distribuicao por areas protegidas (EOO e AOO)."),
              plot_protection(assessment, species = species, lang = lang), h = 2.6)
    for (ts in cover_list)
      add_fig(sprintf(L("Land-cover composition over time - %s.",
                        "Composicao da cobertura ao longo do tempo - %s."),
                      toupper(attr(ts, "range") %||% "")),
              plot_timeseries(ts, lang = lang), h = 3.6)
    for (ts in fire_list)
      add_fig(sprintf(L("Burned area per year - %s.",
                        "Area queimada por ano - %s."),
                      toupper(attr(ts, "range") %||% "")),
              plot_fire_timeseries(ts, lang = lang), h = 3.2)
    if (!length(figs)) figs <- NULL
  }

  list(
    species = species,
    title = L("Preliminary Conservation Assessment", "Avaliacao Preliminar de Conservacao"),
    subtitle = sprintf(L(
      "IUCN Red List Criterion B screening (provisional) - generated on %s with the mappingAS R package.",
      "Triagem pelo Criterio B da Lista Vermelha da IUCN (provisoria) - gerada em %s com o pacote R mappingAS."),
      format(Sys.Date())),
    refs_heading = L("References", "Referencias"),
    figures_heading = L("Support figures", "Figuras de apoio"),
    figures = figs,
    references = refs,
    sections = sections
  )
}

# HTML preview (for the Shiny Report tab).
.report_to_html <- function(b) {
  esc_join <- function(v) paste(sprintf("<p style='margin:.35rem 0;line-height:1.5'>%s</p>", v),
                                collapse = "")
  secs <- vapply(b$sections, function(s) sprintf(
    "<h4 style='margin:1rem 0 .3rem;color:#1f8d49'>%s</h4>%s", s$h, esc_join(s$p)),
    character(1))
  refs <- paste(sprintf(
    "<li style='margin:.25rem 0;font-size:.86rem;line-height:1.4'>%s</li>", b$references),
    collapse = "")
  sprintf(
    "<div style='max-width:820px'>
       <h3 style='margin-bottom:.15rem'>%s</h3>
       <div style='color:#7a857b;margin-bottom:.2rem'>%s</div>
       <div style='color:#7a857b;font-size:.9rem;margin-bottom:.8rem'>%s</div>
       %s
       <h4 style='margin:1rem 0 .3rem;color:#1f8d49'>%s</h4>
       <ol style='padding-left:1.1rem;margin-top:.2rem'>%s</ol>
     </div>",
    b$title, .sp_html(b$species), b$subtitle, paste(secs, collapse = ""),
    b$refs_heading, refs)
}

# Plain-text version (strips the light HTML markup used in the paragraphs).
.report_to_text <- function(b) {
  strip <- function(x) {
    x <- gsub("<sup>2</sup>", "2", x, fixed = TRUE)
    x <- gsub("&mdash;", "-", x, fixed = TRUE)
    x <- gsub("&lt;", "<", x, fixed = TRUE)
    x <- gsub("&times;", "x", x, fixed = TRUE)
    x <- gsub("<[^>]+>", "", x)
    x
  }
  out <- c(b$title, strip(b$subtitle), paste0("Species: ", b$species), "")
  for (s in b$sections)
    out <- c(out, toupper(s$h), strip(s$p), "")
  out <- c(out, toupper(b$refs_heading),
           paste0("- ", strip(b$references)))
  paste(out, collapse = "\n")
}

# Word (.docx) version, built with officer (no pandoc needed).
.report_to_docx <- function(b, file) {
  if (is.null(file) || !nzchar(file))
    stop("`file` is required when output = 'docx'.", call. = FALSE)
  if (!requireNamespace("officer", quietly = TRUE))
    stop("Package 'officer' is required to export a .docx report. ",
         "Install it with install.packages('officer').", call. = FALSE)

  strip <- function(x) {
    x <- gsub("<sup>2</sup>", "\u00b2", x, fixed = TRUE)
    x <- gsub("&mdash;", "\u2014", x, fixed = TRUE)
    x <- gsub("&lt;", "<", x, fixed = TRUE)
    x <- gsub("&times;", "\u00d7", x, fixed = TRUE)
    x <- gsub("<i>|</i>|<b>|</b>", "", x)
    x <- gsub("<[^>]+>", "", x)
    x
  }
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, b$title, style = "heading 1")
  # Species heading: genus + specific epithet in italic, author (if any) normal.
  sp_parts <- .sp_parts(b$species)
  sp_runs  <- list(officer::ftext(
    sp_parts$italic,
    officer::fp_text(italic = TRUE, bold = TRUE, font.size = 14)))
  if (nzchar(sp_parts$rest))
    sp_runs <- c(sp_runs, list(officer::ftext(
      paste0(" ", sp_parts$rest),
      officer::fp_text(italic = FALSE, bold = TRUE, font.size = 14))))
  doc <- officer::body_add_fpar(
    doc, do.call(officer::fpar, sp_runs), style = "heading 2")
  doc <- officer::body_add_par(doc, strip(b$subtitle), style = "Normal")
  doc <- officer::body_add_par(doc, "", style = "Normal")
  for (s in b$sections) {
    doc <- officer::body_add_par(doc, s$h, style = "heading 2")
    for (para in s$p)
      doc <- officer::body_add_par(doc, strip(para), style = "Normal")
  }
  if (!is.null(b$figures) && length(b$figures)) {
    doc <- officer::body_add_par(doc, b$figures_heading %||% "Figures", style = "heading 2")
    for (f in b$figures) {
      doc <- tryCatch({
        d <- officer::body_add_gg(doc, value = f$gg, width = 6.3,
                                  height = f$h %||% 3.4, res = 200,
                                  style = "Normal")
        officer::body_add_par(d, strip(f$cap), style = "Normal")
      }, error = function(e) doc)
    }
  }
  doc <- officer::body_add_par(doc, b$refs_heading, style = "heading 2")
  for (ref in b$references)
    doc <- officer::body_add_par(doc, strip(ref), style = "Normal")
  print(doc, target = file)
  invisible(file)
}
