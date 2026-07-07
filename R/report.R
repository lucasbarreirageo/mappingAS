#' Narrative conservation-assessment report for one species
#'
#' Builds a written, referenced summary of a species' preliminary Criterion B
#' screening from an \code{\link{assess_species}} result. The text adapts to
#' what was actually computed (habitat conversion, fire and protected-area
#' overlap are only described when those modules were run) and always cites the
#' IUCN guidelines and the underlying data sources. It can be returned as HTML
#' (for on-screen preview), as plain text, or written to a Word \code{.docx}
#' file (via \pkg{officer}) - the format used by the Shiny app's Report tab.
#'
#' @param assessment A \code{geoconv_assessment} from \code{\link{assess_species}}.
#' @param species Species name (default: the first assessed).
#' @param lang Report language: \code{"en"} (default) or \code{"pt"}.
#' @param output One of \code{"html"} (default), \code{"text"} or \code{"docx"}.
#' @param file Target path for \code{output = "docx"} (required in that case).
#' @return For \code{"html"}/\code{"text"}, a length-one character string. For
#'   \code{"docx"}, the \code{file} path (written as a side effect), invisibly.
#' @examples
#' \dontrun{
#' res <- assess_species(read_occurrences("occ.csv"), fire = TRUE, protected = TRUE)
#' cat(assessment_report(res, output = "text"))
#' assessment_report(res, output = "docx", file = "report.docx")
#' }
#' @export
assessment_report <- function(assessment, species = NULL,
                              lang = c("en", "pt"),
                              output = c("html", "text", "docx"),
                              file = NULL) {
  lang <- match.arg(lang)
  output <- match.arg(output)
  stopifnot(inherits(assessment, "geoconv_assessment"))
  if (is.null(assessment$summary) || nrow(assessment$summary) == 0)
    stop("The assessment has no results to report.", call. = FALSE)
  if (is.null(species)) species <- assessment$summary$species[1]

  b <- .report_build(assessment, species, lang)

  switch(output,
         html = .report_to_html(b),
         text = .report_to_text(b),
         docx = .report_to_docx(b, file))
}

# Assemble the report content (language-aware) as a plain list, so the HTML,
# text and docx renderers all draw from one source.
.report_build <- function(assessment, species, lang) {
  L <- function(en, pt) if (lang == "en") en else pt
  s <- assessment$summary
  r <- s[s$species == species, , drop = FALSE]
  if (nrow(r) == 0)
    stop("Species not found in assessment: ", species, call. = FALSE)
  r <- r[1, , drop = FALSE]
  st <- assessment$settings

  fmt_km  <- function(x) if (is.null(x) || is.na(x))
    L("not available", "nao disponivel") else
    paste0(formatC(x, format = "f", big.mark = ",", digits = 2), " km<sup>2</sup>")
  fmt_pct <- function(x) if (is.null(x) || is.na(x)) "&mdash;" else sprintf("%.1f%%", x)
  fmt_int <- function(x) if (is.null(x) || is.na(x)) "&mdash;" else
    formatC(x, format = "d", big.mark = ",")
  cat_txt <- function(x) if (is.null(x) || is.na(x)) L("not applicable", "nao aplicavel") else x

  sections <- list()
  add <- function(h, p) sections[[length(sections) + 1L]] <<- list(h = h, p = p)

  # --- Overview ---
  add(
    L("Overview", "Visao geral"),
    c(sprintf(L(
      "This document reports a preliminary, size-based screening of %s against Criterion B of the IUCN Red List Categories and Criteria. It is intended for screening only and does not constitute a formal Red List assessment.",
      "Este documento apresenta uma triagem preliminar, baseada no tamanho da distribuicao, de %s segundo o Criterio B das Categorias e Criterios da Lista Vermelha da IUCN. Destina-se apenas a triagem e nao constitui uma avaliacao formal da Lista Vermelha."),
      sprintf("<i>%s</i>", species)),
      sprintf(L(
        "The assessment is based on %s occurrence records (%s with unique coordinates). The combined provisional category, from range size alone, is <b>%s</b>.",
        "A avaliacao baseia-se em %s registros de ocorrencia (%s com coordenadas unicas). A categoria provisoria combinada, apenas pelo tamanho da distribuicao, e <b>%s</b>."),
        fmt_int(r$n_records), fmt_int(r$n_unique), cat_txt(r$provisional_cat)))
  )

  # --- Range metrics ---
  add(
    L("Geographic range metrics", "Metricas de distribuicao geografica"),
    c(sprintf(L(
      "The Extent of Occurrence (EOO), estimated as the area of the minimum convex polygon enclosing all occurrence points, is %s. The Area of Occupancy (AOO), the number of occupied %g&times;%g km cells scaled to the IUCN 2&times;2 km reference, is %s (%s occupied cells).",
      "A Extensao de Ocorrencia (EOO), estimada como a area do poligono convexo minimo que engloba todos os pontos, e %s. A Area de Ocupacao (AOO), o numero de celulas ocupadas de %g&times;%g km na escala de referencia IUCN de 2&times;2 km, e %s (%s celulas ocupadas)."),
      fmt_km(r$eoo_km2), st$cell_km, st$cell_km, fmt_km(r$aoo_km2), fmt_int(r$aoo_cells)),
      L("Both areas were measured on a data-centred Lambert Azimuthal Equal-Area projection, in line with the IUCN guidelines.",
        "Ambas as areas foram medidas em projecao equivalente Lambert Azimutal centrada nos dados, conforme as diretrizes da IUCN."))
  )

  # --- Provisional category ---
  add(
    L("Provisional Criterion B category", "Categoria provisoria (Criterio B)"),
    c(sprintf(L(
      "Under the size thresholds of Criterion B, the EOO corresponds to %s (sub-criterion B1) and the AOO to %s (sub-criterion B2). The combined provisional category is <b>%s</b>.",
      "Segundo os limiares de tamanho do Criterio B, a EOO corresponde a %s (subcriterio B1) e a AOO a %s (subcriterio B2). A categoria provisoria combinada e <b>%s</b>."),
      cat_txt(r$eoo_cat_B1), cat_txt(r$aoo_cat_B2), cat_txt(r$provisional_cat)),
      L("Thresholds are CR / EN / VU for EOO &lt; 100 / 5,000 / 20,000 km<sup>2</sup> (B1) and AOO &lt; 10 / 500 / 2,000 km<sup>2</sup> (B2). These reflect range size only and, by themselves, do not qualify a taxon for listing: a formal Criterion B assessment additionally requires at least two of the subconditions (severe fragmentation or few locations, continuing decline, and extreme fluctuation).",
        "Os limiares sao CR / EN / VU para EOO &lt; 100 / 5.000 / 20.000 km<sup>2</sup> (B1) e AOO &lt; 10 / 500 / 2.000 km<sup>2</sup> (B2). Refletem apenas o tamanho da distribuicao e, isoladamente, nao qualificam um taxon: uma avaliacao formal do Criterio B exige ainda pelo menos duas das subcondicoes (fragmentacao severa ou poucas localidades, declinio continuo e flutuacao extrema)."))
  )

  # --- Habitat conversion ---
  if (isTRUE(st$mapbiomas) &&
      (!is.na(r$eoo_converted_pct) || !is.na(r$aoo_converted_pct))) {
    add(
      L("Habitat conversion", "Conversao de habitat"),
      sprintf(L(
        "Based on MapBiomas Collection %s (%s), converted (anthropic) land cover accounts for %s of the terrestrial EOO and %s of the AOO; the remaining natural habitat is %s (EOO) and %s (AOO). Water and unobserved areas are excluded from the terrestrial denominator.",
        "Com base na Colecao %s do MapBiomas (%s), a cobertura convertida (antropica) corresponde a %s da EOO terrestre e a %s da AOO; o habitat natural remanescente e de %s (EOO) e %s (AOO). Corpos d'agua e areas nao observadas sao excluidos do denominador terrestre."),
        st$collection, st$year,
        fmt_pct(r$eoo_converted_pct), fmt_pct(r$aoo_converted_pct),
        fmt_pct(r$eoo_natural_pct), fmt_pct(r$aoo_natural_pct))
    )
  }

  # --- Fire ---
  if (isTRUE(st$fire) &&
      (!is.null(r$eoo_burned_pct) && (!is.na(r$eoo_burned_pct) || !is.na(r$aoo_burned_pct)))) {
    add(
      L("Fire history", "Historico de fogo"),
      sprintf(L(
        "MapBiomas Fire (Collection %s) indicates that %s of the EOO and %s of the AOO has burned at least once between 1985 and 2024.",
        "O MapBiomas Fogo (Colecao %s) indica que %s da EOO e %s da AOO queimaram ao menos uma vez entre 1985 e 2024."),
        r$fire_collection %||% st$fire_collection,
        fmt_pct(r$eoo_burned_pct), fmt_pct(r$aoo_burned_pct))
    )
  }

  # --- Protected areas ---
  if (isTRUE(st$protected) && !is.null(r$occ_in_uc_pct)) {
    body <- sprintf(L(
      "%s of the occurrences fall within federal Conservation Units (Unidades de Conservacao, UCs); %s of the EOO and %s of the AOO overlap UCs, across %s units (source: ICMBio / INDE geoservice).",
      "%s das ocorrencias estao dentro de Unidades de Conservacao federais (UCs); %s da EOO e %s da AOO sobrepoem UCs, em %s unidades (fonte: geoservico ICMBio / INDE)."),
      fmt_pct(r$occ_in_uc_pct), fmt_pct(r$eoo_uc_pct), fmt_pct(r$aoo_uc_pct), fmt_int(r$n_uc))
    if (!is.null(r$eoo_nat_uc_pct) && !is.na(r$eoo_nat_uc_pct)) {
      body <- c(body, sprintf(L(
        "Of the terrestrial range, %s of the EOO and %s of the AOO is both natural and inside UCs (effectively protected natural habitat).",
        "Da area terrestre, %s da EOO e %s da AOO e simultaneamente natural e dentro de UCs (habitat natural efetivamente protegido)."),
        fmt_pct(r$eoo_nat_uc_pct), fmt_pct(r$aoo_nat_uc_pct)))
    }
    add(L("Overlap with protected areas", "Sobreposicao com areas protegidas"), body)
  }

  # --- Caveats ---
  caveats <- c(
    L("The provisional category reflects range-size thresholds only and must not be reported as a final IUCN category.",
      "A categoria provisoria reflete apenas limiares de tamanho e nao deve ser reportada como categoria final da IUCN."),
    L("The AOO is sensitive to grid origin and to sampling effort; the EOO can be inflated by outlying or erroneous records.",
      "A AOO e sensivel a origem da grade e ao esforco amostral; a EOO pode ser inflada por registros perifericos ou erroneos."))
  if (isTRUE(st$mapbiomas))
    caveats <- c(caveats,
      L("MapBiomas accuracy varies by class, biome and year; consult the official documentation.",
        "A acuracia do MapBiomas varia por classe, bioma e ano; consulte a documentacao oficial."))
  add(L("Caveats and limitations", "Ressalvas e limitacoes"), caveats)

  # --- References ---
  refs <- c(
    "IUCN Standards and Petitions Committee (2024). Guidelines for Using the IUCN Red List Categories and Criteria, Version 16. Prepared by the Standards and Petitions Committee. https://www.iucnredlist.org/documents/RedListGuidelines.pdf",
    "IUCN (2012). IUCN Red List Categories and Criteria: Version 3.1, Second edition. IUCN, Gland, Switzerland and Cambridge, UK.",
    "Bachman, S., Moat, J., Hill, A.W., de la Torre, J. & Scott, B. (2011). Supporting Red List threat assessments with GeoCAT: geospatial conservation assessment tool. ZooKeys 150: 117-126. doi:10.3897/zookeys.150.2109",
    "Dauby, G. et al. (2017). ConR: An R package to assist large-scale multispecies preliminary conservation assessments using distribution data. Ecology and Evolution 7(24): 11292-11303. doi:10.1002/ece3.3704"
  )
  if (isTRUE(st$mapbiomas))
    refs <- c(refs,
      sprintf("Project MapBiomas - Collection %s of the Annual Series of Land Use and Land Cover Maps of Brazil. https://brasil.mapbiomas.org", st$collection),
      "Souza, C.M. et al. (2020). Reconstructing Three Decades of Land Use and Land Cover Changes in Brazilian Biomes with Landsat Archive and Earth Engine. Remote Sensing 12(17): 2735. doi:10.3390/rs12172735")
  if (isTRUE(st$fire))
    refs <- c(refs,
      "Alencar, A. et al. (2022). Long-Term Landsat-Based Monthly Burned Area Dataset for the Brazilian Biomes Using Deep Learning. Remote Sensing 14(11): 2510. doi:10.3390/rs14112510")
  if (isTRUE(st$protected))
    refs <- c(refs,
      "ICMBio - Instituto Chico Mendes de Conservacao da Biodiversidade. Federal Conservation Units geoservice, Infraestrutura Nacional de Dados Espaciais (INDE). https://www.gov.br/icmbio/pt-br/assuntos/dados_geoespaciais")

  list(
    species = species,
    title = L("Preliminary Conservation Assessment", "Avaliacao Preliminar de Conservacao"),
    subtitle = sprintf(L(
      "IUCN Red List Criterion B screening (provisional) - generated on %s with the mappingAS R package.",
      "Triagem pelo Criterio B da Lista Vermelha da IUCN (provisoria) - gerada em %s com o pacote R mappingAS."),
      format(Sys.Date())),
    refs_heading = L("References", "Referencias"),
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
       <div style='color:#7a857b;margin-bottom:.2rem'><i>%s</i></div>
       <div style='color:#7a857b;font-size:.9rem;margin-bottom:.8rem'>%s</div>
       %s
       <h4 style='margin:1rem 0 .3rem;color:#1f8d49'>%s</h4>
       <ol style='padding-left:1.1rem;margin-top:.2rem'>%s</ol>
     </div>",
    b$title, b$species, b$subtitle, paste(secs, collapse = ""),
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
  doc <- officer::body_add_par(doc, b$species, style = "heading 2")
  doc <- officer::body_add_par(doc, strip(b$subtitle), style = "Normal")
  doc <- officer::body_add_par(doc, "", style = "Normal")
  for (s in b$sections) {
    doc <- officer::body_add_par(doc, s$h, style = "heading 2")
    for (para in s$p)
      doc <- officer::body_add_par(doc, strip(para), style = "Normal")
  }
  doc <- officer::body_add_par(doc, b$refs_heading, style = "heading 2")
  for (ref in b$references)
    doc <- officer::body_add_par(doc, strip(ref), style = "Normal")
  print(doc, target = file)
  invisible(file)
}
