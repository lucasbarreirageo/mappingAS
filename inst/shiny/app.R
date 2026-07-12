## mappingAS Shiny app
## Launch with: mappingAS::run_app()

suppressMessages({
  library(shiny)
  library(bslib)
  library(leaflet)
  library(DT)
  library(plotly)
  if (requireNamespace("mappingAS", quietly = TRUE)) library(mappingAS)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

# Consistent, prettier styling for every DT table in the app: compact striped
# rows, non-integer numerics rounded to 2 dp, and a subtle in-cell colour bar
# on percentage columns so magnitudes read at a glance.
.mas_dt <- function(df, caption = NULL, page = 15) {
  num <- names(df)[vapply(df, is.numeric, logical(1))]
  realish <- num[vapply(df[num], function(x)
    any(is.finite(x) & abs(x - round(x)) > 1e-9), logical(1))]
  pct <- intersect(
    grep("pct|percent|_pc$|perc", names(df), ignore.case = TRUE, value = TRUE),
    num)
  opts <- list(scrollX = TRUE, pageLength = page, dom = "frtip")
  num_idx <- which(names(df) %in% num) - 1
  if (length(num_idx))
    opts$columnDefs <- list(list(className = "dt-center", targets = num_idx))
  dt <- DT::datatable(
    df, rownames = FALSE, caption = caption,
    class = "compact stripe hover row-border order-column",
    options = opts)
  if (length(realish))
    dt <- DT::formatRound(dt, columns = realish, digits = 2)
  if (length(pct))
    dt <- DT::formatStyle(
      dt, pct,
      background = DT::styleColorBar(c(0, 100), "#c9e7d4"),
      backgroundSize = "98% 55%", backgroundRepeat = "no-repeat",
      backgroundPosition = "center")
  dt
}

# Year grid for the MapBiomas collection, thinned by `step` (always keeps the
# last year). Shared by the land-cover and fire time-series reactives.
.year_grid <- function(step, collection = 10L) {
  yy <- mappingAS::mb_years(collection)
  step <- max(1L, as.integer(step))
  sort(unique(c(seq(min(yy), max(yy), by = step), max(yy))))
}

# Run a download's content function, surfacing failures as a notification
# instead of a raw Shiny error. req()/validate() aborts stay silent.
.safe_download <- function(fn) {
  function(file) {
    tryCatch(
      fn(file),
      shiny.silent.error = function(e) invisible(NULL),
      error = function(e) showNotification(
        paste("Download failed:", conditionMessage(e)),
        type = "error", duration = NULL)
    )
  }
}

ui <- bslib::page_sidebar(
  title = "mappingAS | Mapping Area of Species",
  # Non-fillable so tall tabs (chart + table) scroll normally instead of being
  # clipped to the viewport height; the map gets an explicit height below.
  fillable = FALSE,
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#1f8d49"),
  header = bslib::input_dark_mode(id = "dark_mode", mode = "light"),
  sidebar = bslib::sidebar(
    width = 360,
    bslib::input_dark_mode(id = "dark_mode", mode = "light"),
    fileInput(
      "file", "Upload occurrence data (CSV, XLSX, GeoPackage, GeoJSON or shapefile)",
      accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls",
                 ".gpkg", ".geojson", ".json", ".zip")
    ),
    helpText("Tip: for shapefiles, upload a .zip containing .shp/.shx/.dbf/.prj."),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Map columns (optional)",
        textInput("species_col", "Species column", ""),
        textInput("lon_col", "Longitude column", ""),
        textInput("lat_col", "Latitude column", ""),
        helpText("Leave blank for automatic detection.")
      )
    ),
    hr(),
    selectInput("year", "MapBiomas Year", choices = 2024:1985, selected = 2024),
    numericInput("cell_km", "AOO Cell (km)", value = 2, min = 0.5, step = 0.5),
    radioButtons("backend", "MapBiomas Source",
                 choices = c("Local (no GEE account)" = "local",
                             "Google Earth Engine" = "gee"),
                 selected = "local"),
    checkboxInput("water_denom", "Include water as natural in denominator", FALSE),
    checkboxInput("do_mb", "Calculate MapBiomas conversion", TRUE),
    checkboxInput("do_fire", "Calculate fire (MapBiomas burned area)", FALSE),
    checkboxInput("do_pa", "Overlap with Protected areas (ICMBio)", FALSE),
    radioButtons("lang", "Legend language",
                 choices = c("English" = "en", "Portuguese" = "pt"),
                 selected = "en", inline = TRUE),
    actionButton("run", "Assess", class = "btn-primary w-100", icon = icon("calculator")),
    
    hr(),
    radioButtons("export_fmt", "EOO/AOO Maps",
                 choices = c("Shapefile (.zip)" = "shapefile",
                             "GeoPackage (.gpkg)" = "gpkg"),
                 selected = "shapefile", inline = TRUE),
    downloadButton("dl_ranges", "Download EOO/AOO (spatial)", class = "w-100")
  ),
  tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
  bslib::navset_card_tab(
    # TAB 1: MAIN MAP 
    bslib::nav_panel(
      "Map", icon = icon("globe"),
      bslib::layout_columns(
        col_widths = c(9, 3), # 75% for Map, 25% for Controls
        bslib::card(
          full_screen = TRUE,
          leaflet::leafletOutput("map", height = "78vh") # Large map (fullscreen btn available)
        ),
        bslib::card(
          selectInput("map_species", "Species", choices = NULL),
          uiOutput("iucn_panel"),
          radioButtons("map_clip", "Raster clipped to",
                       choices = c("EOO" = "eoo", "AOO" = "aoo"),
                       selected = "eoo", inline = TRUE),
          hr(),
          radioButtons("static_layer", "Publishable map layer (PNG)",
                       choices = c("Land use"  = "lulc",
                                   "Fire"      = "fire",
                                   "Both"      = "both"),
                       selected = "lulc"),
          hr(),
          downloadButton("dl_map_html", "Download map (HTML)", class = "mb-2"),
          downloadButton("dl_map_static", "Publishable map (PNG)", class = "mb-2")
        )
      )
    ),
    bslib::nav_panel(
      "Results", icon = icon("table"),
      div(class = "mb-3",
          downloadButton("dl_csv", "Download results (CSV)")),
      DT::DTOutput("tbl")
    ),
    bslib::nav_panel(
      "Conversion", icon = icon("chart-pie"),
      selectInput("chart_species", "Species", choices = NULL),
      downloadButton("dl_chart_png", "Save image (PNG)", class = "mb-2"),
      div(style = "height:460px; min-height:460px;",
          plotly::plotlyOutput("chart", height = "100%"))
    ),
    bslib::nav_panel(
      "Classes", icon = icon("list"),
      selectInput("class_species", "Species", choices = NULL),
      helpText("Area and % of each MapBiomas class within EOO and AOO."),
      downloadButton("dl_classes", "Download classes (CSV)", class = "mb-3"),
      DT::DTOutput("class_tbl")
    ),
    bslib::nav_panel(
      "Protected areas", icon = icon("tree"),
      selectInput("pa_species", "Species", choices = NULL),
      helpText(htmltools::HTML(
        "Overlap of the range with federal Protected areas (UCs). Source: ",
        "<a href='https://www.gov.br/icmbio/pt-br/assuntos/dados_geoespaciais' ",
        "target='_blank'>ICMBio / INDE geoservice</a> (PDDL public domain).")),
      uiOutput("pa_summary"),
      div(style = "height:460px; min-height:460px;",
          plotly::plotlyOutput("pa_chart", height = "100%")),
      downloadButton("dl_pa_png", "Save chart (PNG)", class = "my-2"),
      downloadButton("dl_pa", "Download UC list (CSV)", class = "my-3"),
      DT::DTOutput("pa_tbl")
    ),
    bslib::nav_panel(
      "Time Series", icon = icon("chart-line"),
      fluidRow(
        column(3, selectInput("ts_species", "Species", choices = NULL)),
        column(3, radioButtons("ts_range", "Area",
                               c("EOO" = "eoo", "AOO" = "aoo"),
                               selected = "eoo", inline = TRUE)),
        column(3, radioButtons("ts_by", "Detail",
                               c("Class" = "class", "Group" = "group"),
                               selected = "class", inline = TRUE)),
        column(3, numericInput("ts_step", "Step (years)", value = 1,
                               min = 1, max = 10, step = 1))
      ),
      div(
        class = "d-flex gap-2 mb-2",
        actionButton("ts_run", "Calculate series", class = "btn-primary"),
        downloadButton("dl_ts", "Download series (CSV)"),
        downloadButton("dl_ts_png", "Save image (PNG)")
      ),
      helpText("Complete MapBiomas history (annual by default). A 1-year step reads all years and may be slow; increase the step to speed up."),
      uiOutput("ts_summary"),
      div(style = "height:480px; min-height:480px;",
          plotly::plotlyOutput("ts_plot", height = "100%")),
      DT::DTOutput("ts_tbl")
    ),

    bslib::nav_panel(
      "Fire", icon = icon("fire"),
      selectInput("fire_species", "Species", choices = NULL),
      uiOutput("fire_summary"),
      fluidRow(
        column(4, radioButtons("fire_ts_range", "Area",
                               c("EOO" = "eoo", "AOO" = "aoo"),
                               selected = "eoo", inline = TRUE)),
        column(4, numericInput("fire_ts_step", "Step (years)", value = 1,
                               min = 1, max = 10, step = 1))
      ),
      div(
        class = "d-flex gap-2 mb-2",
        actionButton("fire_ts_run", "Calculate fire series", class = "btn-primary"),
        downloadButton("dl_fire_ts", "Download series (CSV)"),
        downloadButton("dl_fire_ts_png", "Save image (PNG)")
      ),
      helpText("Burned area per year (MapBiomas Fire, 1985-2024). A 1-year step reads all years and may be slow; increase the step to speed up."),
      div(style = "height:440px; min-height:440px;",
          plotly::plotlyOutput("fire_ts_plot", height = "100%")),
      DT::DTOutput("fire_tbl")
    ),

    bslib::nav_panel(
      "Report", icon = icon("file-word"),
      selectInput("report_species", "Species", choices = NULL),
      helpText("An interpretive, referenced assessment of this species. The EOO/AOO snapshot (metrics, conversion, fire, protection) is always included. The temporal trend accumulates automatically: every Time series and Fire series you calculate for this species (EOO or AOO) is added to the report - no extra step here. The .docx also embeds the support figures."),
      div(class = "mb-3",
          downloadButton("dl_report", "Download report (.docx)",
                         class = "btn-primary")),
      uiOutput("report_status"),
      bslib::card(
        class = "p-3",
        uiOutput("report_preview")
      )
    ),

    bslib::nav_panel(
      "Methods", icon = icon("info-circle"),
      htmltools::HTML(
        "<div style='max-width:820px'>

        <h4>What this tool calculates</h4>
        <ul>
          <li><b>EOO</b> (Extent of Occurrence): area of the minimum convex
              polygon enclosing all occurrence points. The hull is built in
              geographic coordinates, its edges densified along great circles,
              and the area measured on the WGS84 ellipsoid, following the IUCN
              guidelines and the approach of the <i>ConR</i> package.</li>
          <li><b>AOO</b> (Area of Occupancy): number of occupied 2&times;2 km
              cells &times; 4 km<sup>2</sup> (IUCN reference scale). The occupied-cell
              count is the minimum over several randomly translated grids.</li>
          <li><b>% converted</b> = anthropic / (anthropic + natural) within the
              EOO and AOO, from MapBiomas land cover. <b>% natural</b> (current)
              is the complement. Water and unobserved areas are excluded from the
              denominator by default.</li>
          <li><b>Burned area &amp; fire frequency</b>: percentage of the EOO/AOO
              that has burned at least once, and the number of years burned per
              pixel, from MapBiomas Fire (accumulated and frequency layers).</li>
        </ul>

        <h4>Provisional categories</h4>
        <p>The category badges reflect only the <i>size</i> thresholds of
        Criterion B (B1 for EOO, B2 for AOO): CR/EN/VU for EOO &lt;
        100 / 5,000 / 20,000 km<sup>2</sup> and AOO &lt; 10 / 500 / 2,000
        km<sup>2</sup>. A full assessment additionally requires the subcriteria
        (severe fragmentation or few locations, continuing decline, and extreme
        fluctuation) and should never be inferred from size alone. Use these
        results for <b>screening</b> only.</p>

        <h4>Data sources</h4>
        <ul>
          <li><b>MapBiomas Brazil — Land Use and Land Cover</b> (Collection 10,
              1985&ndash;2024). The local backend streams a window of the national
              GeoTIFF via <code>/vsicurl/</code>; the GEE backend computes per-class
              areas server-side.</li>
          <li><b>MapBiomas Fire (Fogo)</b> (Collection 4): annual burned area,
              accumulated burned area and fire-frequency layers
              (1985&ndash;2024).</li>
        </ul>

        <h4>Key references</h4>
        <p style='font-size:.92rem;line-height:1.5'>
          IUCN Standards and Petitions Committee (2024).
          <i>Guidelines for Using the IUCN Red List Categories and Criteria</i>,
          Version 16. <a href='https://www.iucnredlist.org/documents/RedListGuidelines.pdf'
          target='_blank' rel='noopener'>iucnredlist.org</a>.
          <br><br>
          IUCN Red List — Criteria Summary Sheet.
          <a href='https://www.iucnredlist.org/resources/summary-sheet'
          target='_blank' rel='noopener'>iucnredlist.org/resources/summary-sheet</a>.
          <br><br>
          Dauby, G. <i>et al.</i> (2017). ConR: An R package to assist large-scale
          multispecies preliminary conservation assessments using distribution data.
          <i>Ecology and Evolution</i>, 7(24), 11292&ndash;11303.
          <a href='https://doi.org/10.1002/ece3.3704' target='_blank' rel='noopener'>doi:10.1002/ece3.3704</a>.
          <br><br>
          Souza, C. M. <i>et al.</i> (2020). Reconstructing Three Decades of Land
          Use and Land Cover Changes in Brazilian Biomes with Landsat Archive and
          Earth Engine. <i>Remote Sensing</i>, 12(17), 2735.
          <a href='https://doi.org/10.3390/rs12172735' target='_blank' rel='noopener'>doi:10.3390/rs12172735</a>.
          <br><br>
          Alencar, A. <i>et al.</i> (2022). Long-Term Landsat-Based Monthly Burned
          Area Dataset for the Brazilian Biomes Using Deep Learning.
          <i>Remote Sensing</i>, 14(11), 2510.
          <a href='https://doi.org/10.3390/rs14112510' target='_blank' rel='noopener'>doi:10.3390/rs14112510</a>.
          <br><br>
          Project MapBiomas — Collection 10 of the Annual Land Use and Land Cover
          Maps of Brazil, and MapBiomas Fire Collection 4.
          <a href='https://brasil.mapbiomas.org' target='_blank' rel='noopener'>brasil.mapbiomas.org</a>.
        </p>

        <h4>How to cite</h4>
        <p style='font-size:.92rem'>When using results from this application,
        please cite the MapBiomas Land Cover and MapBiomas Fire collections, the
        IUCN Red List guidelines.</p>

        </div>"
      )
    )
  )
)

server <- function(input, output, session) {

  occ <- reactive({
    req(input$file)
    orig <- input$file$name
    ext <- tools::file_ext(orig)
    dest <- file.path(tempdir(), paste0("upload_", as.integer(Sys.time()), ".", ext))
    file.copy(input$file$datapath, dest, overwrite = TRUE)
    mappingAS::read_occurrences(
      dest,
      species_col = input$species_col %||% NULL,
      lon_col = input$lon_col %||% NULL,
      lat_col = input$lat_col %||% NULL
    )
  })

  result <- eventReactive(input$run, {
    o <- tryCatch(occ(), error = function(e) {
      showNotification(paste("Error reading file:", conditionMessage(e)),
                       type = "error", duration = NULL)
      NULL
    })
    req(o)

    if (input$backend == "gee" && !requireNamespace("rgee", quietly = TRUE)) {
      showNotification("rgee is not installed; please use the Local backend.",
                       type = "error", duration = NULL)
      return(NULL)
    }

    withProgress(message = "Assessing species...", value = 0, {
      sp <- unique(o$species); n <- length(sp)
      res <- tryCatch(
        mappingAS::assess_species(
          o,
          year = as.integer(input$year),
          collection = 10L,
          backend = input$backend,
          cell_km = input$cell_km,
          mapbiomas = isTRUE(input$do_mb),
          fire = isTRUE(input$do_fire),
          protected = isTRUE(input$do_pa),
          water_in_denominator = isTRUE(input$water_denom),
          verbose = FALSE
        ),
        error = function(e) {
          showNotification(paste("Error in assessment:", conditionMessage(e)),
                           type = "error", duration = NULL)
          NULL
        }
      )
      incProgress(1)
      res
    })
  })

  observeEvent(result(), {
    req(result())
    sp <- result()$summary$species
    updateSelectInput(session, "map_species", choices = sp, selected = sp[1])
    updateSelectInput(session, "chart_species", choices = sp, selected = sp[1])
    updateSelectInput(session, "class_species", choices = sp, selected = sp[1])
    updateSelectInput(session, "ts_species", choices = sp, selected = sp[1])
    updateSelectInput(session, "fire_species", choices = sp, selected = sp[1])
    updateSelectInput(session, "pa_species", choices = sp, selected = sp[1])
    updateSelectInput(session, "report_species", choices = sp, selected = sp[1])
  })

  output$tbl <- DT::renderDT({
    req(result())
    .mas_dt(result()$summary, page = 25,
            caption = "EOO, AOO and conversion by species")
  })

  # --- Protected areas (UC) tab ---
  output$pa_summary <- renderUI({
    req(result(), input$pa_species)
    d  <- result()$detail[[input$pa_species]]
    pa <- if (!is.null(d)) d$pa else NULL
    if (is.null(pa)) return(htmltools::HTML(
      "<div class='text-muted'>Run the assessment with the Protected areas option enabled.</div>"))
    en  <- (input$lang %||% "en") == "en"
    pct <- function(x) if (is.null(x) || is.na(x)) "&mdash;" else sprintf("%.1f%%", x)
    card <- function(t, v) sprintf(
      "<div style='flex:1;min-width:150px;border:1px solid #e4ddce;border-radius:.6rem;
        padding:12px 14px;background:#fffdf8'>
        <div style='color:#7a857b;font-size:.8rem'>%s</div>
        <div style='font-family:monospace;font-weight:700;font-size:1.25rem'>%s</div></div>",
      t, v)
    htmltools::HTML(sprintf(
      "<div style='display:flex;gap:10px;flex-wrap:wrap;margin-bottom:6px'>%s%s%s%s%s</div>",
      card(if (en) "Occurrences in UCs" else "Ocorrencias em UC",
           sprintf("%d / %d", pa$n_occ_in, pa$n_occ)),
      card(if (en) "% occurrences in UCs" else "% ocorrencias em UC", pct(pa$occ_pct)),
      card(if (en) "% EOO in UCs" else "% EOO em UC", pct(pa$eoo_pct)),
      card(if (en) "% AOO in UCs" else "% AOO em UC", pct(pa$aoo_pct)),
      card(if (en) "UCs touched" else "N. de UCs", as.character(pa$n_uc))))
  })

  output$pa_tbl <- DT::renderDT({
    req(result(), input$pa_species)
    tb <- tryCatch(mappingAS::pa_table(result(), species = input$pa_species),
                   error = function(e) data.frame())
    validate(need(nrow(tb) > 0,
                  "No Protected areas overlap this species' range."))
    .mas_dt(tb, page = 15,
            caption = "Federal Protected areas overlapping the range")
  })

  output$pa_chart <- plotly::renderPlotly({
    req(result(), input$pa_species)
    d <- result()$detail[[input$pa_species]]
    validate(need(!is.null(d) && !is.null(d$pa),
                  "Run the assessment with the Protected areas option enabled."))
    mappingAS::mas_plotly(
      mappingAS::plot_protection(result(), species = input$pa_species,
                                 lang = input$lang %||% "en"))
  })

  output$dl_pa_png <- downloadHandler(
    filename = function() paste0("mappingAS_UC_", input$pa_species, "_", Sys.Date(), ".png"),
    content = .safe_download(function(file) {
      req(result(), input$pa_species)
      p <- mappingAS::plot_protection(result(), species = input$pa_species,
                                      lang = input$lang %||% "en")
      if (inherits(p, "ggplot") && requireNamespace("ggplot2", quietly = TRUE)) {
        ggplot2::ggsave(file, plot = p, width = 8.5, height = 5.2, dpi = 130)
      } else {
        grDevices::png(file, width = 1100, height = 520, res = 130)
        on.exit(grDevices::dev.off(), add = TRUE)
        mappingAS::plot_protection(result(), species = input$pa_species,
                                   lang = input$lang %||% "en")
      }
    })
  )

  output$dl_pa <- downloadHandler(
    filename = function() paste0("mappingAS_UCs_", Sys.Date(), ".csv"),
    content = .safe_download(function(file) {
      req(result())
      tb <- tryCatch(mappingAS::pa_table(result()), error = function(e) data.frame())
      utils::write.csv(tb, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  )
  
  output$map <- leaflet::renderLeaflet({
    req(result(), input$map_species)
    mappingAS::map_species(result(), species = input$map_species,
                           mapbiomas = isTRUE(input$do_mb),
                           fire = isTRUE(input$do_fire),
                           lang = input$lang %||% "en",
                           clip = input$map_clip %||% "eoo",
                           protected = isTRUE(input$do_pa))
  })

  # IUCN badges (EOO/B1 and AOO/B2) + headline metrics, beside the map.
  output$iucn_panel <- renderUI({
    req(result(), input$map_species)
    s <- result()$summary
    r <- s[s$species == input$map_species, , drop = FALSE]
    validate(need(nrow(r) > 0, ""))
    r <- r[1, , drop = FALSE]

    badge <- function(cat) {
      b <- mappingAS:::.iucn_badge(cat)
      sprintf(
        "<div style='display:inline-flex;align-items:center;justify-content:center;
          width:46px;height:46px;border-radius:50%%;background:%s;color:%s;
          font-weight:700;font-size:1rem;border:2px solid rgba(0,0,0,.15)'>%s</div>",
        b$bg, b$fg, b$code)
    }
    fmt_km <- function(x) if (is.na(x)) "&mdash;" else
      formatC(x, format = "f", big.mark = ",", digits = 2)

    en <- (input$lang %||% "en") == "en"
    t_eoo <- if (en) "Extent of Occurrence" else "Extensão de Ocorrência"
    t_aoo <- if (en) "Area of Occupancy"   else "Área de Ocupação"
    t_note <- if (en)
      "Provisional category — Criterion B size thresholds only (screening)."
    else
      "Categoria provisória — apenas limiares de tamanho do Critério B (triagem)."

    htmltools::HTML(sprintf(
      "<div style='border:1px solid #e4ddce;border-radius:.6rem;padding:12px 14px;
        background:#fffdf8;margin-bottom:6px'>
        <div style='display:flex;align-items:center;gap:12px;margin-bottom:10px'>
          %s
          <div style='line-height:1.15'>
            <div style='color:#7a857b;font-size:.8rem'>%s (EOO / B1)</div>
            <div style='font-family:monospace;font-weight:600;font-size:1.05rem'>%s km<sup>2</sup></div>
          </div>
        </div>
        <div style='display:flex;align-items:center;gap:12px'>
          %s
          <div style='line-height:1.15'>
            <div style='color:#7a857b;font-size:.8rem'>%s (AOO / B2)</div>
            <div style='font-family:monospace;font-weight:600;font-size:1.05rem'>%s km<sup>2</sup>
              <span style='color:#7a857b;font-weight:400;font-size:.85rem'>(%s cells)</span></div>
          </div>
        </div>
        <div style='color:#7a857b;font-size:.72rem;margin-top:10px'>%s</div>
       </div>",
      badge(r$eoo_cat_B1), t_eoo, fmt_km(r$eoo_km2),
      badge(r$aoo_cat_B2), t_aoo, fmt_km(r$aoo_km2), r$aoo_cells,
      t_note))
  })

  outputOptions(output, "map", suspendWhenHidden = FALSE)

  output$chart <- plotly::renderPlotly({
    req(result(), input$chart_species)
    mappingAS::mas_plotly(
      mappingAS::plot_conversion(result(), species = input$chart_species,
                                 lang = input$lang %||% "en"))
  })

  output$dl_csv <- downloadHandler(
    filename = function() paste0("mappingAS_results_", Sys.Date(), ".csv"),
    content = .safe_download(function(file) {
      req(result())
      utils::write.csv(result()$summary, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  )

  output$dl_ranges <- downloadHandler(
    filename = function() {
      ext <- if (identical(input$export_fmt, "gpkg")) "gpkg" else "zip"
      paste0("mappingAS_EOO_AOO_", Sys.Date(), ".", ext)
    },
    content = .safe_download(function(file) {
      req(result())
      tmp <- file.path(tempdir(), paste0("ranges_", as.integer(Sys.time())))
      dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
      out <- tryCatch(
        mappingAS::export_ranges(
          result(), dir = tmp, layer_prefix = "mappingAS",
          format = input$export_fmt,
          zip = identical(input$export_fmt, "shapefile")
        ),
        error = function(e) {
          showNotification(paste("Error exporting:", conditionMessage(e)),
                           type = "error", duration = NULL)
          NULL
        }
      )
      req(out)
      file.copy(out, file, overwrite = TRUE)
    })
  )

  output$class_tbl <- DT::renderDT({
    req(result(), input$class_species)
    df <- tryCatch(
      mappingAS::class_table(result(), species = input$class_species, range = "both"),
      error = function(e) data.frame())
    validate(need(nrow(df) > 0,
                  "No class data available. Enable 'Calculate MapBiomas conversion' and reassess."))

    lg <- input$lang %||% "en"
    if (lg == "en") df$class_pt <- NULL else df$class_en <- NULL
    names(df)[names(df) %in% c("class_pt", "class_en")] <- "class"

    .mas_dt(df, page = 25,
            caption = "Area and % by MapBiomas class (EOO and AOO)")
  })

  output$dl_classes <- downloadHandler(
    filename = function() paste0("mappingAS_classes_", Sys.Date(), ".csv"),
    content = .safe_download(function(file) {
      req(result())
      df <- tryCatch(mappingAS::class_table(result(), range = "both"),
                     error = function(e) data.frame())
      utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  )

  ts_data <- eventReactive(input$ts_run, {
    req(result(), input$ts_species)
    yrs <- .year_grid(input$ts_step)
    withProgress(message = "Calculating time series...", value = 0, {
      ts <- tryCatch(
        mappingAS::timeseries_for_species(
          result(), species = input$ts_species, range = input$ts_range,
          years = yrs, by = input$ts_by, verbose = FALSE),
        error = function(e) {
          showNotification(paste("Error in time series:", conditionMessage(e)),
                           type = "error", duration = NULL)
          NULL
        })
      incProgress(1)
      ts
    })
  })

  output$ts_plot <- plotly::renderPlotly({
    ts <- ts_data(); req(ts)
    mappingAS::mas_plotly(
      mappingAS::plot_timeseries(ts, lang = input$lang %||% "en"))
  })

  output$ts_summary <- renderUI({
    ts <- ts_data(); req(ts)
    if (!"group" %in% names(ts)) return(NULL)
    sub <- ts[ts$group == "anthropic", , drop = FALSE]
    if (!nrow(sub)) {
      return(htmltools::HTML(
        "<div style='padding:8px 12px;background:#f6f6f6;border-radius:6px;margin-bottom:8px'>No anthropic classes in this extent.</div>"))
    }
    ya <- stats::aggregate(pct ~ year, data = sub, FUN = sum)
    ya <- ya[order(ya$year), ]
    first <- ya[1, ]; last <- ya[nrow(ya), ]
    delta <- last$pct - first$pct
    htmltools::HTML(sprintf(
      "<div style='padding:10px 14px;background:#f6f6f6;border-radius:6px;margin-bottom:10px'>
        <b>Altered area (anthropic) — %s %s:</b>
        %.1f%% in %d &rarr; %.1f%% in %d
        (<span style='color:%s'><b>%+.1f percentage points</b></span> in the period).
       </div>",
      attr(ts, "species") %||% "", attr(ts, "range") %||% "",
      first$pct, first$year, last$pct, last$year,
      if (delta >= 0) "#d4271e" else "#1f8d49", delta))
  })

  output$ts_tbl <- DT::renderDT({
    ts <- ts_data(); req(ts)
    .mas_dt(ts, page = 15, caption = "Composition (%) by year")
  })

  output$dl_ts <- downloadHandler(
    filename = function() paste0("mappingAS_series_", input$ts_species, "_", Sys.Date(), ".csv"),
    content = .safe_download(function(file) {
      ts <- ts_data(); req(ts)
      utils::write.csv(ts, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  )

  output$fire_summary <- renderUI({
    req(result(), input$fire_species)
    obj <- result()$detail[[input$fire_species]]
    validate(need(!is.null(obj$eoo_fire) || !is.null(obj$aoo_fire),
                  "Enable 'Calculate fire (MapBiomas burned area)' in the side panel and reassess."))
    fmt <- function(f, lab) {
      if (is.null(f)) return(sprintf("<li>%s: no data</li>", lab))
      sprintf("<li><b>%s:</b> %.1f%% of the area has burned at least once (1985-2024).</li>",
              lab, f$burned_pct %||% NA_real_)
    }
    htmltools::HTML(sprintf(
      "<div style='padding:10px 14px;background:#fff3e0;border-radius:6px;margin-bottom:10px'>
        <b>Accumulated Fire — %s</b><ul style='margin-bottom:0'>%s%s</ul></div>",
      input$fire_species, fmt(obj$eoo_fire, "EOO"), fmt(obj$aoo_fire, "AOO")))
  })

  output$fire_tbl <- DT::renderDT({
    req(result(), input$fire_species)
    obj <- result()$detail[[input$fire_species]]
    mk <- function(f, rng) {
      if (is.null(f)) return(NULL)
      data.frame(range = rng,
                 total_area_km2    = round(f$total_km2, 2),
                 burned_area_km2   = round(f$burned_km2, 2),
                 pct_burned        = round(f$burned_pct, 2))
    }
    df <- rbind(mk(obj$eoo_fire, "EOO"), mk(obj$aoo_fire, "AOO"))
    validate(need(!is.null(df) && nrow(df) > 0, "No fire data (or fire was not calculated)."))
    .mas_dt(df, page = 10, caption = "Burned area by extent (EOO and AOO)")
  })

  fire_ts_data <- eventReactive(input$fire_ts_run, {
    req(result(), input$fire_species)
    yrs <- .year_grid(input$fire_ts_step)
    withProgress(message = "Calculating fire series...", value = 0, {
      ts <- tryCatch(
        mappingAS::fire_timeseries_for_species(
          result(), species = input$fire_species, range = input$fire_ts_range,
          years = yrs, verbose = FALSE),
        error = function(e) {
          showNotification(paste("Error in fire series:", conditionMessage(e)),
                           type = "error", duration = NULL)
          NULL
        })
      incProgress(1)
      ts
    })
  })

  output$fire_ts_plot <- plotly::renderPlotly({
    ts <- fire_ts_data(); req(ts)
    mappingAS::mas_plotly(
      mappingAS::plot_fire_timeseries(ts, lang = input$lang %||% "en"))
  })

  output$dl_fire_ts <- downloadHandler(
    filename = function() paste0("mappingAS_fire_", input$fire_species, "_", Sys.Date(), ".csv"),
    content = .safe_download(function(file) {
      ts <- fire_ts_data(); req(ts)
      utils::write.csv(ts, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  )

  output$dl_fire_ts_png <- downloadHandler(
    filename = function() paste0("mappingAS_fire_", input$fire_species, "_", Sys.Date(), ".png"),
    content = .safe_download(function(file) {
      ts <- fire_ts_data(); req(ts)
      p <- mappingAS::plot_fire_timeseries(ts)
      if (inherits(p, "ggplot") && requireNamespace("ggplot2", quietly = TRUE)) {
        ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 130)
      } else {
        grDevices::png(file, width = 1200, height = 720, res = 120)
        on.exit(grDevices::dev.off(), add = TRUE)
        mappingAS::plot_fire_timeseries(ts)
      }
    })
  )

  output$dl_map_html <- downloadHandler(
    filename = function() paste0("mappingAS_map_", input$map_species, "_", Sys.Date(), ".html"),
    content = .safe_download(function(file) {
      req(result(), input$map_species)
      m <- mappingAS::map_species(result(), species = input$map_species,
                           mapbiomas = isTRUE(input$do_mb),
                           fire = isTRUE(input$do_fire),
                           lang = input$lang %||% "en",
                           clip = input$map_clip %||% "eoo",
                           protected = isTRUE(input$do_pa))
      htmlwidgets::saveWidget(m, file, selfcontained = TRUE)
    })
  )

  output$dl_map_static <- downloadHandler(
    filename = function() paste0("mappingAS_map_static_", input$map_species, "_", Sys.Date(), ".png"),
    content = .safe_download(function(file) {
      req(result(), input$map_species)
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        showNotification("The 'ggplot2' package is required for the publishable map.",
                         type = "error", duration = NULL)
        req(FALSE)
      }
      lyr <- input$static_layer %||% "lulc"
      if (lyr == "both" && !requireNamespace("ggnewscale", quietly = TRUE)) {
        showNotification(
          "To overlay land use and fire, install 'ggnewscale'. Exporting land use only.",
          type = "warning", duration = 8)
      }
      m <- withProgress(message = "Generating publishable map...", value = 0, {
        out <- tryCatch(
          mappingAS::map_static(
            result(), species = input$map_species,
            mapbiomas = lyr %in% c("lulc", "both"),
            fire      = lyr %in% c("fire", "both"),
            lang      = input$lang %||% "en",
            clip      = input$map_clip %||% "eoo"),
          error = function(e) {
            showNotification(paste("Error generating the publishable map:",
                                   conditionMessage(e)),
                             type = "error", duration = NULL)
            NULL
          })
        incProgress(1)
        out
      })
      req(inherits(m, "ggplot"))
      ggplot2::ggsave(file, plot = m, width = 9, height = 8, dpi = 300)
    })
  )

  output$dl_chart_png <- downloadHandler(
    filename = function() paste0("mappingAS_conversion_", input$chart_species, "_", Sys.Date(), ".png"),
    content = .safe_download(function(file) {
      req(result(), input$chart_species)
      p <- mappingAS::plot_conversion(result(), species = input$chart_species,
                                      lang = input$lang %||% "en")
      if (inherits(p, "ggplot") && requireNamespace("ggplot2", quietly = TRUE)) {
        ggplot2::ggsave(file, plot = p, width = 8.5, height = 5.8, dpi = 130)
      } else {
        grDevices::png(file, width = 1100, height = 750, res = 130)
        on.exit(grDevices::dev.off(), add = TRUE)
        mappingAS::plot_conversion(result(), species = input$chart_species,
                                   lang = input$lang %||% "en")
      }
    })
  )

  output$dl_ts_png <- downloadHandler(
    filename = function() paste0("mappingAS_series_", input$ts_species, "_", Sys.Date(), ".png"),
    content = .safe_download(function(file) {
      ts <- ts_data(); req(ts)
      p <- mappingAS::plot_timeseries(ts, lang = input$lang %||% "en")
      if (inherits(p, "ggplot") && requireNamespace("ggplot2", quietly = TRUE)) {
        ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 130)
      } else {
        grDevices::png(file, width = 1200, height = 720, res = 120)
        on.exit(grDevices::dev.off(), add = TRUE)
        mappingAS::plot_timeseries(ts)
      }
    })
  )

  # --- Report tab: narrative assessment + Word download ---
  # The report accumulates whatever temporal series the user computes in the
  # Time series and Fire tabs: each calculated series (per species and per
  # EOO/AOO extent) is stored and fed into the report, so the analysis "adds up"
  # without any extra computation on this tab.
  report_cover_store <- reactiveVal(list())
  report_fire_store  <- reactiveVal(list())

  observeEvent(ts_data(), {
    ts <- ts_data()
    if (is.null(ts) || !is.data.frame(ts)) return()
    key <- paste(attr(ts, "species") %||% "", attr(ts, "range") %||% "", sep = "||")
    st <- report_cover_store(); st[[key]] <- ts; report_cover_store(st)
  }, ignoreInit = TRUE)

  observeEvent(fire_ts_data(), {
    ts <- fire_ts_data()
    if (is.null(ts) || !is.data.frame(ts)) return()
    key <- paste(attr(ts, "species") %||% "", attr(ts, "range") %||% "", sep = "||")
    st <- report_fire_store(); st[[key]] <- ts; report_fire_store(st)
  }, ignoreInit = TRUE)

  .report_cover <- function(sp) {
    keep <- Filter(function(ts) identical(attr(ts, "species"), sp), report_cover_store())
    if (length(keep)) unname(keep) else NULL
  }
  .report_fire <- function(sp) {
    keep <- Filter(function(ts) identical(attr(ts, "species"), sp), report_fire_store())
    if (length(keep)) unname(keep) else NULL
  }

  output$report_status <- renderUI({
    req(input$report_species)
    sp <- input$report_species
    ranges <- function(x) if (is.null(x)) "none" else
      paste(toupper(vapply(x, function(t) attr(t, "range") %||% "?", character(1))),
            collapse = ", ")
    cv <- .report_cover(sp); fr <- .report_fire(sp)
    if (is.null(cv) && is.null(fr))
      return(helpText("No temporal series loaded yet for this species. Calculate the Time series and/or Fire series (EOO and/or AOO) in their tabs to add the trend analysis to this report."))
    helpText(sprintf("Temporal analysis loaded - cover: %s | fire: %s. Included in the preview and the .docx.",
                     ranges(cv), ranges(fr)))
  })

  output$report_preview <- renderUI({
    req(result(), input$report_species)
    sp <- input$report_species
    htmltools::HTML(
      mappingAS::assessment_report(
        result(), species = sp, lang = input$lang %||% "en", output = "html",
        cover_series = .report_cover(sp), fire_series = .report_fire(sp)))
  })

  output$dl_report <- downloadHandler(
    filename = function()
      paste0("mappingAS_report_", gsub("[^A-Za-z0-9]+", "_", input$report_species %||% "species"),
             "_", Sys.Date(), ".docx"),
    content = .safe_download(function(file) {
      req(result(), input$report_species)
      if (!requireNamespace("officer", quietly = TRUE)) {
        showNotification(
          "The 'officer' package is required to export the Word report. Install it with install.packages('officer').",
          type = "error", duration = NULL)
        req(FALSE)
      }
      sp <- input$report_species
      mappingAS::assessment_report(
        result(), species = sp, lang = input$lang %||% "en", output = "docx",
        file = file, figures = TRUE,
        cover_series = .report_cover(sp), fire_series = .report_fire(sp))
    })
  )
}

shinyApp(ui, server)