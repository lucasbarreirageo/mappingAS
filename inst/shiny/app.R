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
.year_grid <- function(step, collection = 10L, initiative = "brazil") {
  yy <- mappingAS::mb_years(collection, initiative)
  step <- max(1L, as.integer(step))
  sort(unique(c(seq(min(yy), max(yy), by = step), max(yy))))
}

# Per-class linear-regression trend table for one time series (the tabular
# companion of plot_class_trendline): one row per class with slope (percentage
# points per year), R2, p-value and the first/last/delta values.
.class_trend_df <- function(ts, rng, use_en = TRUE) {
  if (is.null(ts) || !is.data.frame(ts) || !nrow(ts)) return(NULL)
  has_en <- use_en && "class_en" %in% names(ts)
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
    data.frame(
      range       = rng,
      class       = if (has_en) ts$class_en[ts$label == L][1] else L,
      slope_pp_yr = round(unname(cf[2L]), 4),
      r2          = round(sm$r.squared, 4),
      p_value     = signif(as.numeric(pv), 4),
      first_pct   = round(d$pct[1L], 3),
      last_pct    = round(d$pct[nrow(d)], 3),
      delta_pp    = round(d$pct[nrow(d)] - d$pct[1L], 3),
      stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(df) || !nrow(df)) return(NULL)
  df[order(-abs(df$slope_pp_yr)), , drop = FALSE]
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
    selectInput("initiative", "Land-cover product",
                choices = c(
                  "Auto" = "auto",
                  "Sentinel-2 / Esri global 10 m (2017-2023)" = "sentinel2",
                  "Brazil (Collection 10)" = "brazil",
                  "Amazonia / Pan-Amazon (Collection 6)" = "amazonia",
                  "Colombia (Collection 3)" = "colombia",
                  "Argentina (Collection 2)" = "argentina",
                  "Bolivia (Collection 3)" = "bolivia",
                  "Chile (Collection 1, 2000-2022)" = "chile",
                  "Ecuador (Collection 3)" = "ecuador",
                  "Peru (Collection 3)" = "peru",
                  "Venezuela (Collection 2)" = "venezuela",
                  "Paraguay (Collection 2, 1985-2023)" = "paraguay",
                  "Uruguay (Collection 1, 1985-2022)" = "uruguay"),
                selected = "auto"),
    helpText(paste("Auto picks MapBiomas for occurrences in South America",
                   "(by country) and the global Sentinel-2/Esri layer",
                   "elsewhere. Pick a specific product to override.")),
    selectInput("year", "Year (land cover)", choices = 2024:1985, selected = 2024),
    numericInput("cell_km", "AOO Cell (km)", value = 2, min = 0.5, step = 0.5),
    radioButtons("backend", "Land cover source",
                 choices = c("Local (no GEE account)" = "local",
                             "Google Earth Engine" = "gee"),
                 selected = "local"),
    checkboxInput("water_denom", "Include water as natural in denominator", FALSE),
    checkboxInput("do_mb", "Calculate land-cover conversion", TRUE),
    checkboxInput("do_fire", "Calculate fire (burned area, Brazil only)", FALSE),
    checkboxInput("do_pa", "Overlap with Protected areas", FALSE),
    conditionalPanel(
      condition = "input.do_pa == true",
      fileInput(
        "pa_file",
        "Protected-area layer (optional: GeoPackage, GeoJSON or shapefile .zip)",
        accept = c(".gpkg", ".geojson", ".json", ".shp", ".zip")),
      helpText(htmltools::HTML(
        "Leave empty to query the global <b>WDPA</b> service online. ",
        "If it returns nothing (or you are offline), upload a local protected-area ",
        "file - e.g. Brazil's federal Conservation Units from ICMBio ",
        "(<a href='https://www.gov.br/icmbio/pt-br/servicos/geoprocessamento' ",
        "target='_blank'>ICMBio geoprocessamento</a>) or a WDPA extract from ",
        "<a href='https://www.protectedplanet.net' target='_blank'>Protected Planet</a>."))
    ),
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
                       choices = c("EOO" = "eoo", "AOO" = "aoo", "All" = "all"),
                       selected = "eoo", inline = TRUE),
          hr(),
          radioButtons("static_layer", "Downloaded map layer (PNG & HTML)",
                       choices = c("Land use"  = "lulc",
                                   "Fire"      = "fire",
                                   "Both"      = "both"),
                       selected = "lulc"),
          hr(),
          downloadButton("dl_map_html", "Download map (HTML)", class = "mb-2"),
          downloadButton("dl_map_static", "Publishable map (PNG)", class = "mb-2"),
          hr(),
          checkboxGroupInput("raster_layers", "GeoTIFF rasters to export",
                             choices = c("Land cover" = "lulc",
                                         "Fire (accumulated, Brazil)" = "fire"),
                             selected = "lulc"),
          downloadButton("dl_rasters", "Download rasters (GeoTIFF .zip)",
                         class = "mb-1"),
          helpText("For the species and clip (EOO/AOO) selected above.")
        )
      )
    ),
    bslib::nav_panel(
      "Results", icon = icon("table"),
      selectInput("results_species", "Species", choices = NULL),
      uiOutput("results_cards"),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Column glossary - what each field means",
          uiOutput("results_glossary")
        )
      ),
      div(class = "mt-3 mb-2",
          downloadButton("dl_csv", "Download results (CSV)")),
      DT::DTOutput("tbl")
    ),
    bslib::nav_panel(
      "Conversion", icon = icon("chart-pie"),
      selectInput("chart_species", "Species", choices = NULL),
      downloadButton("dl_chart_png", "Save image (PNG)", class = "mb-2"),
      div(style = "height:460px; min-height:460px;",
          plotly::plotlyOutput("chart", height = "100%")),
      tags$hr(),
      h5("Land-cover composition (donut)"),
      helpText("Share of each land-cover class (or conservation group) inside the",
               "EOO and AOO, in the official land-cover colours."),
      fluidRow(
        column(6, radioButtons(
          "donut_by", "Breakdown",
          choices = c("By class" = "class", "By group" = "group"),
          selected = "class", inline = TRUE)),
        column(6, div(class = "text-md-end pt-4",
          downloadButton("dl_donut_png", "Save donut (transparent PNG)")))
      ),
      div(style = "height:470px; min-height:470px;",
          plotly::plotlyOutput("donut", height = "100%"))
    ),
    bslib::nav_panel(
      "Classes", icon = icon("list"),
      selectInput("class_species", "Species", choices = NULL),
      helpText("Area and % of each land-cover class within EOO and AOO."),
      downloadButton("dl_classes", "Download classes (CSV)", class = "mb-3"),
      DT::DTOutput("class_tbl")
    ),
    bslib::nav_panel(
      "Protected areas", icon = icon("tree"),
      selectInput("pa_species", "Species", choices = NULL),
      helpText(htmltools::HTML(
        "Overlap of the range with protected areas. Source: ",
        "<a href='https://www.protectedplanet.net' ",
        "target='_blank'>World Database on Protected Areas (WDPA)</a>.")),
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
        column(4, selectInput("ts_species", "Species", choices = NULL)),
        column(4, radioButtons("ts_by", "Detail",
                               c("Class" = "class", "Group" = "group"),
                               selected = "class", inline = TRUE)),
        column(4, numericInput("ts_step", "Step (years)", value = 1,
                               min = 1, max = 10, step = 1))
      ),
      div(
        class = "d-flex gap-2 mb-2",
        actionButton("ts_run", "Calculate series", class = "btn-primary"),
        downloadButton("dl_ts", "Download series (CSV)"),
        downloadButton("dl_ts_png", "Save image (PNG)")
      ),
      helpText("Complete land-cover history for BOTH extents (EOO and AOO), one chart above the other, each with its own altered-area analysis. A 1-year step reads all years and may be slow; increase the step to speed up."),
      # Each block is a self-contained Bootstrap card so the DataTable reserves
      # its height and the sections below it never overlap it.
      tags$div(
        class = "card mb-3",
        tags$div(
          class = "card-body",
          tags$h5("EOO — Extent of Occurrence", class = "card-title"),
          uiOutput("ts_summary_eoo"),
          plotly::plotlyOutput("ts_plot_eoo", height = "420px"))),
      tags$div(
        class = "card mb-3",
        tags$div(
          class = "card-body",
          tags$h5("AOO — Area of Occupancy", class = "card-title"),
          uiOutput("ts_summary_aoo"),
          plotly::plotlyOutput("ts_plot_aoo", height = "420px"))),
      tags$div(
        class = "card mb-3",
        tags$div(
          class = "card-body",
          tags$h5("Composition (%) by year — EOO and AOO", class = "card-title"),
          DT::DTOutput("ts_tbl"))),
      tags$div(
        class = "card mb-3",
        tags$div(
          class = "card-body",
          tags$h5("Trend analysis (ggtrendline)", class = "card-title"),
          fluidRow(
            column(6, selectInput("ts_trend_class", "Class / group", choices = NULL)),
            column(6, selectInput("ts_trend_model", "Regression model",
                                  c("Linear (line2P)"      = "line2P",
                                    "Quadratic (line3P)"   = "line3P",
                                    "Logarithmic (log2P)"  = "log2P",
                                    "Exponential (exp2P)"  = "exp2P",
                                    "Power (power2P)"       = "power2P"),
                                  selected = "line2P"))),
          helpText("Regression trend of the selected class through time (percentage of area), with the fitted equation, R² and p-value, for each extent. Defaults to the dominant class until you pick one."),
          fluidRow(
            column(6, tags$b("EOO"),
                   plotOutput("ts_trend_eoo", height = "360px")),
            column(6, tags$b("AOO"),
                   plotOutput("ts_trend_aoo", height = "360px"))),
          div(class = "d-flex gap-2 mt-3 mb-2",
              downloadButton("dl_ts_trend", "Download trend table (CSV)")),
          helpText("Linear trend per class for each extent: slope (percentage points per year), R², p-value, and first/last/delta. Needs at least 3 years (lower the step if empty)."),
          DT::DTOutput("ts_trend_tbl")))
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
      ),

      # ---- Factsheet (standalone HTML) ------------------------------------
      tags$hr(),
      tags$h4("Species factsheet (HTML)"),
      helpText(HTML("A single, self-contained HTML page (like a supplementary-website factsheet) combining what the package computes (metrics + charts) with details it cannot know, which you enter below. Up to four photos are embedded, each watermarked in its lower-right corner with the owner name. The page is portable - open it offline or publish it as-is (e.g. on GitHub Pages).")),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Taxonomy and supporting information",
          fluidRow(
            column(4, textInput("fs_family", "Family", "")),
            column(4, textInput("fs_genus", "Genus (defaults to the species' first word)", "")),
            column(4, textInput("fs_authority", "Authority", ""))),
          fluidRow(
            column(4, textInput("fs_countries", "Countries", "")),
            column(4, textInput("fs_system", "System", "")),
            column(4, textInput("fs_biome", "Biome", ""))),
          fluidRow(
            column(6, textInput("fs_habitat", "Habitat", "")),
            column(6, textInput("fs_vegetation", "Vegetation", "")))
        ),
        bslib::accordion_panel(
          "Land use, conservation units and vouchers",
          textAreaInput("fs_land_use", "Land use", "", rows = 2,
                        placeholder = "e.g. surrounded by pasture and urban expansion"),
          textAreaInput("fs_cons_units", "Conservation units", "", rows = 2,
                        placeholder = "e.g. PARNA da Tijuca; APA Petropolis"),
          textAreaInput("fs_vouchers", "Examined vouchers (one per line)", "",
                        rows = 4, placeholder = "Barreira 123 (RB)\nSilva 456 (R)")
        ),
        bslib::accordion_panel(
          "Reference",
          textAreaInput("fs_reference", "Reflora / POWO link, or - for a new species - the article citation",
                        "", rows = 2)
        ),
        bslib::accordion_panel(
          "Photos (up to 4) and watermark",
          fileInput("fs_photos", "Photos (PNG/JPEG, up to 4)",
                    multiple = TRUE,
                    accept = c(".png", ".jpg", ".jpeg", ".gif", ".webp")),
          textInput("fs_photo_credit", "Photo owner (watermark)", "")
        )
      ),
      checkboxInput("fs_map", "Include the distribution map (points + EOO + AOO + land cover)", TRUE),
      radioButtons("fs_map_type", NULL, inline = TRUE,
                   choices = c("Interactive (HTML, with the data)" = "interactive",
                               "Static image" = "static"),
                   selected = "interactive"),
      helpText("The interactive map is the same Leaflet map the Maps tab downloads (Download map (HTML)), embedded inside the factsheet. The land-cover time series and the fire series charts are added automatically once you calculate them (Time series / Fire tabs) for this species - the same series that feed the .docx report."),
      div(class = "my-3 d-flex gap-2",
          downloadButton("dl_factsheet", "Download factsheet (.html)",
                         class = "btn-primary"),
          actionButton("fs_preview", "Preview", icon = icon("eye"),
                       class = "btn-outline-secondary")),
      helpText("The preview and the download render the embedded map and charts, which can take a few seconds. Click Preview after filling in the fields."),
      bslib::card(
        class = "p-2",
        uiOutput("factsheet_preview")
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
              EOO and AOO, from the land-cover product. <b>% natural</b> (current)
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
          <li><b>MapBiomas — Land Use and Land Cover</b> (Brazil, Amazonia and the
              South-American country collections, 1985&ndash;2024). The local
              backend streams a window of the national GeoTIFF via
              <code>/vsicurl/</code>; the GEE backend computes per-class areas
              server-side. Used for occurrences within South America.</li>
          <li><b>Esri / Impact Observatory 10 m Annual Land Use Land Cover</b>
              (Sentinel-2, 9-class, 2017&ndash;2023): the global layer behind the
              ArcGIS Living Atlas Land Cover Explorer, streamed as public COGs via
              <code>/vsicurl/</code>. Used automatically for occurrences outside
              MapBiomas coverage.</li>
          <li><b>MapBiomas Fire (Fogo)</b> (Collection 4): annual burned area,
              accumulated burned area and fire-frequency layers
              (1985&ndash;2024, Brazil only).</li>
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
          Project MapBiomas — Annual Land Use and Land Cover Maps (Brazil and
          South-American collections) and MapBiomas Fire Collection 4.
          <a href='https://brasil.mapbiomas.org' target='_blank' rel='noopener'>brasil.mapbiomas.org</a>.
          <br><br>
          Karra, K. <i>et al.</i> (2021). Global land use/land cover with
          Sentinel-2 and deep learning. <i>IGARSS 2021</i>.
          <a href='https://doi.org/10.1109/IGARSS47720.2021.9553499' target='_blank' rel='noopener'>doi:10.1109/IGARSS47720.2021.9553499</a>.
          Esri, Impact Observatory &amp; Microsoft — Sentinel-2 10m Land Use/Land
          Cover Time Series (ArcGIS Living Atlas).
          <br><br>
          Mei, W. &amp; Yu, G. (2022). <i>ggtrendline: Add Trendline and
          Confidence Interval to 'ggplot2'</i>. R package (used for the
          land-cover class trend analysis in the Time Series tab).
          <a href='https://CRAN.R-project.org/package=ggtrendline'
          target='_blank' rel='noopener'>CRAN.R-project.org/package=ggtrendline</a>.
        </p>

        <h4>How to cite</h4>
        <p style='font-size:.92rem'>When using results from this application,
        please cite the land-cover source used (MapBiomas collections, or the
        Esri/Impact Observatory Sentinel-2 series for areas outside MapBiomas),
        MapBiomas Fire where applicable, and the IUCN Red List guidelines.</p>

        </div>"
      )
    )
  )
)

server <- function(input, output, session) {

  # Keep the Year dropdown in sync with the selected initiative's span
  # (Brazil/Colombia 1985-2024, Amazonia 1986-2023).
  observeEvent(input$initiative, {
    yy <- tryCatch(mappingAS::mb_years(initiative = input$initiative),
                   error = function(e) 1985:2024)
    updateSelectInput(session, "year", choices = rev(yy), selected = max(yy))
  }, ignoreInit = TRUE)

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

  # Optional local protected-area layer: resolve the upload to a path that
  # sf::st_read() can open. A shapefile arrives zipped, so it is unzipped and the
  # contained .shp is returned; other vector files are copied verbatim. Returns
  # NULL when nothing was uploaded (so WDPA is queried online instead).
  pa_src_path <- reactive({
    f <- input$pa_file
    if (is.null(f) || is.null(f$datapath) || !nzchar(f$datapath)) return(NULL)
    ext <- tolower(tools::file_ext(f$name))
    if (ext == "zip") {
      tmp <- file.path(tempdir(), paste0("pa_zip_", as.integer(Sys.time())))
      dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
      utils::unzip(f$datapath, exdir = tmp)
      shp <- list.files(tmp, pattern = "\\.shp$", recursive = TRUE,
                        full.names = TRUE)
      if (!length(shp)) {
        showNotification("No .shp found inside the protected-area .zip.",
                         type = "error", duration = NULL)
        return(NULL)
      }
      return(shp[1])
    }
    dest <- file.path(tempdir(),
                      paste0("pa_upload_", as.integer(Sys.time()), ".", ext))
    file.copy(f$datapath, dest, overwrite = TRUE)
    dest
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
          initiative = input$initiative %||% "brazil",
          year = as.integer(input$year),
          collection = NULL,
          backend = input$backend,
          cell_km = input$cell_km,
          mapbiomas = isTRUE(input$do_mb),
          fire = isTRUE(input$do_fire),
          protected = isTRUE(input$do_pa),
          pa_src = if (isTRUE(input$do_pa)) pa_src_path() else NULL,
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
    updateSelectInput(session, "results_species", choices = sp, selected = sp[1])
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

  # Visual overview cards (with in-cell bars) for the selected species.
  output$results_cards <- renderUI({
    req(result(), input$results_species)
    s <- result()$summary
    r <- s[s$species == input$results_species, , drop = FALSE]
    validate(need(nrow(r) > 0, "Select a species."))
    r  <- r[1, , drop = FALSE]
    st <- result()$settings
    has <- function(c) c %in% names(r) && !is.na(r[[c]])
    v   <- function(c) if (has(c)) r[[c]] else NA_real_
    badge <- function(cat) {
      b <- mappingAS:::.iucn_badge(cat)
      sprintf("<span style='display:inline-flex;align-items:center;justify-content:center;width:34px;height:34px;border-radius:50%%;background:%s;color:%s;font-weight:700;font-size:.8rem;border:2px solid rgba(0,0,0,.15)'>%s</span>", b$bg, b$fg, b$code)
    }
    fkm <- function(x) if (is.na(x)) "&mdash;" else formatC(x, format = "f", big.mark = ",", digits = 2)
    fpc <- function(x) if (is.na(x)) "&mdash;" else sprintf("%.1f%%", x)
    card <- function(inner) sprintf("<div style='flex:1;min-width:180px;border:1px solid #e4ddce;border-radius:.6rem;padding:10px 12px;background:#fffdf8'>%s</div>", inner)
    lab  <- function(t) sprintf("<div style='color:#7a857b;font-size:.78rem;margin-bottom:3px'>%s</div>", t)
    big  <- function(x) sprintf("<div style='font-family:monospace;font-weight:700;font-size:1.1rem'>%s</div>", x)
    bar  <- function(pct, col) { p <- if (is.na(pct)) 0 else max(0, min(100, pct))
      sprintf("<div style='background:#eee;border-radius:5px;height:9px;overflow:hidden;margin-top:4px'><div style='width:%.1f%%;height:100%%;background:%s'></div></div>", p, col) }
    dual <- function(title, e, a, col) card(paste0(lab(title),
      sprintf("<div style='font-size:.82rem'>EOO %s</div>", fpc(e)), bar(e, col),
      sprintf("<div style='font-size:.82rem;margin-top:4px'>AOO %s</div>", fpc(a)), bar(a, col)))

    cards <- c(
      card(paste0(lab(sprintf("EOO / B1 &nbsp; %s", badge(r$eoo_cat_B1))),
                  big(paste0(fkm(v("eoo_km2")), " km<sup>2</sup>")))),
      card(paste0(lab(sprintf("AOO / B2 &nbsp; %s", badge(r$aoo_cat_B2))),
                  big(paste0(fkm(v("aoo_km2")), " km<sup>2</sup>")),
                  sprintf("<div style='color:#7a857b;font-size:.75rem'>%s cells &middot; %s records (%s unique)</div>",
                          v("aoo_cells"), v("n_records"), v("n_unique")))),
      card(paste0(lab("Provisional category (screening)"),
                  sprintf("<div style='display:flex;align-items:center;gap:8px;font-weight:700'>%s<span>%s</span></div>",
                          badge(r$provisional_cat), r$provisional_cat)))
    )
    if (isTRUE(st$mapbiomas)) cards <- c(cards,
      dual("Converted (anthropic)", v("eoo_converted_pct"), v("aoo_converted_pct"), "#d4271e"),
      dual("Natural (remaining)",   v("eoo_natural_pct"),   v("aoo_natural_pct"),   "#1f8d49"))
    if (isTRUE(st$fire) && has("eoo_burned_pct"))
      cards <- c(cards, dual("Burned at least once (1985-2024)",
                             v("eoo_burned_pct"), v("aoo_burned_pct"), "#fd8d3c"))
    if (isTRUE(st$protected) && has("occ_in_uc_pct")) {
      cards <- c(cards, card(paste0(lab("In protected areas (UCs)"),
        sprintf("<div style='font-size:.82rem'>Occurrences %s</div>", fpc(v("occ_in_uc_pct"))),
        bar(v("occ_in_uc_pct"), "#1f8d49"),
        sprintf("<div style='font-size:.82rem;margin-top:4px'>EOO %s &middot; AOO %s</div>",
                fpc(v("eoo_uc_pct")), fpc(v("aoo_uc_pct"))),
        sprintf("<div style='color:#7a857b;font-size:.75rem;margin-top:2px'>%s UCs touched</div>", v("n_uc")))))
      if (has("eoo_nat_uc_pct"))
        cards <- c(cards, dual("Natural AND protected", v("eoo_nat_uc_pct"), v("aoo_nat_uc_pct"), "#04381d"))
    }
    htmltools::HTML(sprintf(
      "<div style='display:flex;gap:10px;flex-wrap:wrap;margin-bottom:12px'>%s</div>",
      paste(cards, collapse = "")))
  })

  # Plain-language glossary for every summary column (language-aware).
  output$results_glossary <- renderUI({
    en <- (input$lang %||% "en") == "en"
    L  <- function(a, b) if (en) a else b
    defs <- list(
      c("species", L("Species name.", "Nome da especie.")),
      c("n_records", L("Number of occurrence records used.", "Numero de registros de ocorrencia usados.")),
      c("n_unique", L("Number of unique coordinates.", "Numero de coordenadas unicas.")),
      c("eoo_km2", L("Extent of Occurrence: area of the minimum convex polygon (km2).", "Extensao de Ocorrencia: area do poligono convexo minimo (km2).")),
      c("aoo_km2", L("Area of Occupancy: occupied 2x2 km cells x 4 km2.", "Area de Ocupacao: celulas ocupadas de 2x2 km x 4 km2.")),
      c("aoo_cells", L("Number of occupied AOO cells.", "Numero de celulas ocupadas da AOO.")),
      c("eoo_converted_pct", L("% of the terrestrial EOO that is converted (anthropic).", "% da EOO terrestre convertida (antropica).")),
      c("eoo_natural_pct", L("% of the terrestrial EOO that is remaining natural habitat.", "% da EOO terrestre de habitat natural remanescente.")),
      c("aoo_converted_pct", L("% of the terrestrial AOO that is converted (anthropic).", "% da AOO terrestre convertida (antropica).")),
      c("aoo_natural_pct", L("% of the terrestrial AOO that is remaining natural habitat.", "% da AOO terrestre de habitat natural remanescente.")),
      c("eoo_cat_B1", L("Provisional category by EOO size (sub-criterion B1).", "Categoria provisoria pelo tamanho da EOO (subcriterio B1).")),
      c("aoo_cat_B2", L("Provisional category by AOO size (sub-criterion B2).", "Categoria provisoria pelo tamanho da AOO (subcriterio B2).")),
      c("provisional_cat", L("Combined provisional category (the more threatened of B1/B2). Screening only.", "Categoria provisoria combinada (a mais ameacada entre B1/B2). Apenas triagem.")),
      c("mapbiomas_initiative", L("Land-cover product used (a MapBiomas country, or 'sentinel2' for the global Esri/Sentinel-2 layer).", "Produto de cobertura usado (um pais do MapBiomas, ou 'sentinel2' para a camada global Esri/Sentinel-2).")),
      c("mapbiomas_year", L("Land-cover year used.", "Ano da cobertura usada.")),
      c("mapbiomas_collection", L("Land-cover collection number (NA for Sentinel-2).", "Numero da colecao de cobertura (NA para Sentinel-2).")),
      c("eoo_burned_pct", L("% of the EOO burned at least once (1985-2024).", "% da EOO queimada ao menos uma vez (1985-2024).")),
      c("aoo_burned_pct", L("% of the AOO burned at least once (1985-2024).", "% da AOO queimada ao menos uma vez (1985-2024).")),
      c("fire_collection", L("MapBiomas Fire collection number.", "Numero da colecao MapBiomas Fogo.")),
      c("occ_in_uc_pct", L("% of occurrences inside protected areas (WDPA).", "% das ocorrencias dentro de areas protegidas.")),
      c("eoo_uc_pct", L("% of the EOO overlapping UCs.", "% da EOO sobreposta a UCs.")),
      c("aoo_uc_pct", L("% of the AOO overlapping UCs.", "% da AOO sobreposta a UCs.")),
      c("n_uc", L("Number of UCs the range touches.", "Numero de UCs que a distribuicao toca.")),
      c("eoo_nat_uc_pct", L("% of the terrestrial EOO that is BOTH natural and inside UCs.", "% da EOO terrestre que e natural E dentro de UCs.")),
      c("eoo_nat_uc_pct_in", L("% of the EOO area inside UCs that is natural.", "% da area da EOO dentro de UCs que e natural.")),
      c("aoo_nat_uc_pct", L("% of the terrestrial AOO that is BOTH natural and inside UCs.", "% da AOO terrestre que e natural E dentro de UCs.")),
      c("aoo_nat_uc_pct_in", L("% of the AOO area inside UCs that is natural.", "% da area da AOO dentro de UCs que e natural."))
    )
    rows <- vapply(defs, function(d) sprintf(
      "<tr><td style='padding:4px 10px;font-family:monospace;white-space:nowrap;vertical-align:top;color:#1f8d49'>%s</td><td style='padding:4px 10px'>%s</td></tr>",
      d[1], d[2]), character(1))
    htmltools::HTML(sprintf(
      "<table style='border-collapse:collapse;font-size:.86rem'><tbody>%s</tbody></table>",
      paste(rows, collapse = "")))
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
            caption = "Protected areas overlapping the range")
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

  # Export the MapBiomas land-use and/or fire GeoTIFF rasters for the selected
  # species and clip (EOO/AOO), bundled into a .zip.
  output$dl_rasters <- downloadHandler(
    filename = function()
      paste0("mappingAS_rasters_",
             gsub("[^A-Za-z0-9]+", "_", input$map_species %||% "species"),
             "_", input$map_clip %||% "eoo", "_", Sys.Date(), ".zip"),
    content = .safe_download(function(file) {
      req(result(), input$map_species)
      layers <- input$raster_layers
      if (is.null(layers) || !length(layers)) {
        showNotification("Select at least one raster layer to export.",
                         type = "warning"); req(FALSE)
      }
      if (!requireNamespace("terra", quietly = TRUE)) {
        showNotification("Package 'terra' is required.", type = "error",
                         duration = NULL); req(FALSE)
      }
      obj  <- result()$detail[[input$map_species]]; req(!is.null(obj))
      st   <- result()$settings
      clip <- input$map_clip %||% "eoo"
      geom <- mappingAS:::.clip_geometry(clip, obj$eoo$hull, obj$aoo$cells)
      validate(need(!is.null(geom) && length(sf::st_geometry(geom)) > 0,
                    "No polygon available for this clip."))
      tmp <- file.path(tempdir(), paste0("rasters_", as.integer(Sys.time())))
      dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
      files <- character(0)
      withProgress(message = "Preparing rasters (reading land cover)...", value = 0, {
        if ("lulc" %in% layers) {
          lc_ini  <- obj$initiative %||% st$initiative %||% "brazil"
          lc_year <- obj$year %||% st$year
          lc_coll <- obj$collection %||% st$collection
          r <- tryCatch(mappingAS::mb_raster_local(geom, year = lc_year,
                          collection = lc_coll,
                          initiative = lc_ini),
                        error = function(e) NULL)
          if (!is.null(r)) {
            f <- file.path(tmp, sprintf("mapbiomas_lulc_%s_%s.tif", clip, lc_year))
            terra::writeRaster(r, f, overwrite = TRUE, datatype = "INT1U",
                               gdal = "COMPRESS=LZW"); files <- c(files, f)
          }
        }
        incProgress(0.5)
        if ("fire" %in% layers) {
          fr <- tryCatch(mappingAS::fire_raster_local(geom, product = "accumulated",
                          fire_collection = st$fire_collection %||% 4,
                          host_collection = st$fire_host_collection %||% 9),
                         error = function(e) NULL)
          if (!is.null(fr)) {
            f <- file.path(tmp, sprintf("mapbiomas_fire_accumulated_%s.tif", clip))
            terra::writeRaster(fr, f, overwrite = TRUE, gdal = "COMPRESS=LZW")
            files <- c(files, f)
          }
        }
        incProgress(0.5)
      })
      validate(need(length(files) > 0,
                    "No raster could be produced (check the network / selection)."))
      owd <- setwd(tmp); on.exit(setwd(owd), add = TRUE)
      if (requireNamespace("zip", quietly = TRUE))
        zip::zipr(file, basename(files))
      else
        utils::zip(file, basename(files))
    })
  )

  output$map <- leaflet::renderLeaflet({
    req(result(), input$map_species)
    mappingAS::map_species(result(), species = input$map_species,
                           mapbiomas = isTRUE(input$do_mb),
                           fire = isTRUE(input$do_fire),
                           lang = input$lang %||% "en",
                           clip = input$map_clip %||% "eoo",
                           protected = isTRUE(input$do_pa),
                           pa_src = if (isTRUE(input$do_pa)) pa_src_path() else NULL)
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

  output$donut <- plotly::renderPlotly({
    req(result(), input$chart_species)
    p <- tryCatch(
      mappingAS::plot_conversion_donut(
        result(), species = input$chart_species,
        by = input$donut_by %||% "class", lang = input$lang %||% "en"),
      error = function(e) e)
    validate(need(!inherits(p, "error"),
                  "No class data available. Enable 'Calculate land-cover conversion' and reassess."))
    mappingAS::mas_plotly(p)
  })

  output$dl_donut_png <- downloadHandler(
    filename = function() paste0("mappingAS_donut_", input$donut_by %||% "class",
                                 "_", input$chart_species, "_", Sys.Date(), ".png"),
    content = .safe_download(function(file) {
      req(result(), input$chart_species)
      p <- mappingAS::plot_conversion_donut(
        result(), species = input$chart_species,
        by = input$donut_by %||% "class", lang = input$lang %||% "en")
      req(inherits(p, "ggplot"))
      ggplot2::ggsave(file, plot = p, width = 10, height = 5.5, dpi = 200,
                      bg = "transparent")
    })
  )

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
      # `export_ranges()` may return more than one path (e.g. a sidecar CSV):
      # copy only the single artefact that matches the requested format so the
      # download never fails with "more 'from' files than 'to' files".
      main <- if (identical(input$export_fmt, "gpkg"))
                out[grepl("\\.gpkg$", out, ignore.case = TRUE)]
              else
                out[grepl("\\.zip$", out, ignore.case = TRUE)]
      if (!length(main)) main <- out[[1]]
      file.copy(main[[1]], file, overwrite = TRUE)
    })
  )

  output$class_tbl <- DT::renderDT({
    req(result(), input$class_species)
    df <- tryCatch(
      mappingAS::class_table(result(), species = input$class_species, range = "both"),
      error = function(e) data.frame())
    validate(need(nrow(df) > 0,
                  "No class data available. Enable 'Calculate land-cover conversion' and reassess."))

    lg <- input$lang %||% "en"
    if (lg == "en") df$class_pt <- NULL else df$class_en <- NULL
    names(df)[names(df) %in% c("class_pt", "class_en")] <- "class"

    .mas_dt(df, page = 25,
            caption = "Area and % by land-cover class (EOO and AOO)")
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

  # Compute the land-cover series for BOTH extents (EOO and AOO) in one run, so
  # the two charts, their analyses, the table, the CSV and the report all cover
  # the two areas at once. Returns a list(eoo = <df|NULL>, aoo = <df|NULL>).
  ts_data <- eventReactive(input$ts_run, {
    req(result(), input$ts_species)
    yrs <- .year_grid(input$ts_step, result()$settings$collection,
                      result()$settings$initiative %||% "brazil")
    one <- function(rng) {
      tryCatch(
        mappingAS::timeseries_for_species(
          result(), species = input$ts_species, range = rng,
          years = yrs, by = input$ts_by, verbose = FALSE),
        error = function(e) {
          showNotification(sprintf("Error in %s time series: %s",
                                   toupper(rng), conditionMessage(e)),
                           type = "error", duration = NULL)
          NULL
        })
    }
    withProgress(message = "Calculating time series (EOO & AOO)...", value = 0, {
      eoo <- one("eoo"); incProgress(0.4)
      # Release memory between the two extents: reading every year for both EOO
      # and AOO is memory-heavy, and on small machines (e.g. a Codespace) the
      # accumulated pressure can get the R process killed ("Terminated").
      invisible(gc(FALSE)); incProgress(0.1)
      aoo <- one("aoo"); incProgress(0.4)
      invisible(gc(FALSE)); incProgress(0.1)
      list(eoo = eoo, aoo = aoo)
    })
  })

  # Shared renderer for the per-extent altered-area (anthropic) analysis box.
  .ts_summary_html <- function(ts) {
    if (is.null(ts) || !"group" %in% names(ts)) return(NULL)
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
  }

  output$ts_plot_eoo <- plotly::renderPlotly({
    d <- ts_data(); req(d$eoo)
    mappingAS::mas_plotly(
      mappingAS::plot_timeseries(d$eoo, lang = input$lang %||% "en"))
  })

  output$ts_plot_aoo <- plotly::renderPlotly({
    d <- ts_data(); req(d$aoo)
    mappingAS::mas_plotly(
      mappingAS::plot_timeseries(d$aoo, lang = input$lang %||% "en"))
  })

  output$ts_summary_eoo <- renderUI({ d <- ts_data(); req(d$eoo); .ts_summary_html(d$eoo) })
  output$ts_summary_aoo <- renderUI({ d <- ts_data(); req(d$aoo); .ts_summary_html(d$aoo) })

  # Combine both extents into one table / CSV, tagged with a `range` column.
  .ts_combined <- function(d) {
    mk <- function(ts, rng) {
      if (is.null(ts) || !is.data.frame(ts)) return(NULL)
      cbind(range = rng, ts, stringsAsFactors = FALSE)
    }
    df <- rbind(mk(d$eoo, "EOO"), mk(d$aoo, "AOO"))
    df
  }

  output$ts_tbl <- DT::renderDT({
    d <- ts_data(); req(!is.null(d$eoo) || !is.null(d$aoo))
    df <- .ts_combined(d)
    validate(need(!is.null(df) && nrow(df) > 0, "No series data."))
    .mas_dt(df, page = 15, caption = "Composition (%) by year — EOO and AOO")
  })

  output$dl_ts <- downloadHandler(
    filename = function() paste0("mappingAS_series_", input$ts_species, "_", Sys.Date(), ".csv"),
    content = .safe_download(function(file) {
      d <- ts_data(); req(!is.null(d$eoo) || !is.null(d$aoo))
      df <- .ts_combined(d); req(!is.null(df) && nrow(df) > 0)
      utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  )

  # Populate the trend-analysis class picker from the computed series, ordered
  # by mean share so the dominant class is offered first.
  observe({
    d <- ts_data(); ts <- d$eoo %||% d$aoo
    if (is.null(ts) || !is.data.frame(ts) || !nrow(ts)) return()
    disp <- if ((input$lang %||% "en") == "en" && "class_en" %in% names(ts))
      ts$class_en else ts$label
    mp <- tapply(ts$pct, disp, mean, na.rm = TRUE)
    labs <- names(sort(mp, decreasing = TRUE))
    if (!length(labs)) return()
    cur <- isolate(input$ts_trend_class)
    sel <- if (!is.null(cur) && nzchar(cur) && cur %in% labs) cur else labs[1]
    updateSelectInput(session, "ts_trend_class", choices = labs, selected = sel)
  })

  .ts_trend_plot <- function(ts) {
    req(ts)
    cl <- input$ts_trend_class
    if (is.null(cl) || !nzchar(cl)) cl <- NULL  # let the function pick the dominant class
    mappingAS::plot_class_trendline(
      ts, class_label = cl,
      model = input$ts_trend_model %||% "line2P",
      lang = input$lang %||% "en")
  }

  output$ts_trend_eoo <- renderPlot({ d <- ts_data(); req(d$eoo); .ts_trend_plot(d$eoo) })
  output$ts_trend_aoo <- renderPlot({ d <- ts_data(); req(d$aoo); .ts_trend_plot(d$aoo) })

  # Per-class regression trend table (both extents), shown and downloadable.
  .ts_trend_table <- reactive({
    d <- ts_data(); req(!is.null(d$eoo) || !is.null(d$aoo))
    use_en <- (input$lang %||% "en") == "en"
    rbind(.class_trend_df(d$eoo, "EOO", use_en),
          .class_trend_df(d$aoo, "AOO", use_en))
  })

  output$ts_trend_tbl <- DT::renderDT({
    df <- .ts_trend_table()
    validate(need(!is.null(df) && nrow(df) > 0,
                  "Trend statistics need at least 3 years - lower the 'Step (years)' and recalculate."))
    .mas_dt(df, page = 15,
            caption = "Per-class linear trend (slope pp/yr, R², p-value) — EOO and AOO")
  })

  output$dl_ts_trend <- downloadHandler(
    filename = function() paste0("mappingAS_trend_", input$ts_species, "_", Sys.Date(), ".csv"),
    content = .safe_download(function(file) {
      df <- .ts_trend_table(); req(!is.null(df) && nrow(df) > 0)
      utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  )

  output$fire_summary <- renderUI({
    req(result(), input$fire_species)
    obj <- result()$detail[[input$fire_species]]
    validate(need(!is.null(obj$eoo_fire) || !is.null(obj$aoo_fire),
                  "Enable 'Calculate fire (burned area)' in the side panel and reassess."))
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
      lyr <- input$static_layer %||% "lulc"
      m <- mappingAS::map_species(result(), species = input$map_species,
                           mapbiomas = lyr %in% c("lulc", "both"),
                           fire      = lyr %in% c("fire", "both"),
                           lang = input$lang %||% "en",
                           clip = input$map_clip %||% "eoo",
                           protected = isTRUE(input$do_pa),
                           pa_src = if (isTRUE(input$do_pa)) pa_src_path() else NULL)
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
      d <- ts_data(); req(!is.null(d$eoo) || !is.null(d$aoo))
      lg <- input$lang %||% "en"
      ps <- Filter(Negate(is.null), list(
        if (!is.null(d$eoo)) mappingAS::plot_timeseries(d$eoo, lang = lg),
        if (!is.null(d$aoo)) mappingAS::plot_timeseries(d$aoo, lang = lg)))
      n <- length(ps)
      grDevices::png(file, width = 1200, height = 420 * max(n, 1), res = 120)
      on.exit(grDevices::dev.off(), add = TRUE)
      if (all(vapply(ps, inherits, logical(1), "ggplot"))) {
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(
          layout = grid::grid.layout(n, 1)))
        for (i in seq_len(n)) {
          print(ps[[i]], vp = grid::viewport(layout.pos.row = i,
                                             layout.pos.col = 1))
        }
      } else {
        # base-graphics fallback: stack the plots
        graphics::par(mfrow = c(n, 1))
        if (!is.null(d$eoo)) mappingAS::plot_timeseries(d$eoo, lang = lg)
        if (!is.null(d$aoo)) mappingAS::plot_timeseries(d$aoo, lang = lg)
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
    d <- ts_data()
    if (is.null(d)) return()
    st <- report_cover_store()
    for (ts in list(d$eoo, d$aoo)) {
      if (is.null(ts) || !is.data.frame(ts)) next
      key <- paste(attr(ts, "species") %||% "", attr(ts, "range") %||% "", sep = "||")
      st[[key]] <- ts
    }
    report_cover_store(st)
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

  # --- Factsheet tab: user metadata + photos -> standalone HTML -----------
  # Copy each uploaded photo to a temp file that keeps its extension, so the
  # image MIME type is detected correctly when it is embedded.
  .fs_photo_paths <- function() {
    pf <- input$fs_photos
    if (is.null(pf) || !nrow(pf)) return(NULL)
    pf <- utils::head(pf, 4L)
    out <- character(0)
    for (i in seq_len(nrow(pf))) {
      ext <- tools::file_ext(pf$name[i]); if (!nzchar(ext)) ext <- "png"
      dst <- tempfile(fileext = paste0(".", ext))
      if (isTRUE(file.copy(pf$datapath[i], dst, overwrite = TRUE)))
        out <- c(out, dst)
    }
    if (length(out)) out else NULL
  }

  # Gather all factsheet arguments (empty text fields become NULL so the
  # corresponding rows are simply omitted from the page).
  .factsheet_args <- function(sp) {
    nz <- function(id) {
      v <- input[[id]]
      if (is.null(v)) return(NULL)
      v <- trimws(v)
      if (nzchar(v)) v else NULL
    }
    list(
      assessment = result(), species = sp, lang = input$lang %||% "en",
      family = nz("fs_family"), genus = nz("fs_genus"),
      authority = nz("fs_authority"),
      countries = nz("fs_countries"), system = nz("fs_system"),
      habitat = nz("fs_habitat"), biome = nz("fs_biome"),
      vegetation = nz("fs_vegetation"),
      land_use = nz("fs_land_use"),
      conservation_units = nz("fs_cons_units"),
      vouchers = nz("fs_vouchers"), reference = nz("fs_reference"),
      photos = .fs_photo_paths(), photo_credit = nz("fs_photo_credit"),
      cover_series = .report_cover(sp), fire_series = .report_fire(sp),
      map = isTRUE(input$fs_map),
      map_interactive = identical(input$fs_map_type %||% "interactive", "interactive"))
  }

  output$factsheet_preview <- renderUI({
    input$fs_preview                 # render only when Preview is clicked
    req(result(), input$report_species, input$fs_preview)
    isolate({
      html <- tryCatch(
        do.call(mappingAS::factsheet_html, .factsheet_args(input$report_species)),
        error = function(e)
          sprintf("<p style='color:#b00'>Preview error: %s</p>",
                  conditionMessage(e)))
      # Embed as an isolated document (its own <html>/<body>) inside an iframe,
      # via a base64 data URI to avoid any attribute-escaping issues.
      b64 <- mappingAS:::.base64_encode(charToRaw(enc2utf8(html)))
      tags$iframe(
        src = paste0("data:text/html;base64,", b64),
        style = "width:100%;height:820px;border:1px solid #e3e8e3;border-radius:10px;background:#fff;")
    })
  })

  output$dl_factsheet <- downloadHandler(
    filename = function()
      paste0("factsheet_",
             gsub("[^A-Za-z0-9]+", "_", input$report_species %||% "species"),
             "_", Sys.Date(), ".html"),
    content = .safe_download(function(file) {
      req(result(), input$report_species)
      args <- .factsheet_args(input$report_species)
      args$file <- file
      do.call(mappingAS::factsheet_html, args)
    })
  )
}

shinyApp(ui, server)