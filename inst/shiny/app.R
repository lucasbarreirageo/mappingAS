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

# Allow large occurrence uploads. Shiny caps a request body at 5 MB by default,
# so a big occurrence table is rejected in the browser ("Maximum upload size
# exceeded") before read_occurrences() ever runs. Raise the cap to a generous
# default; run_app(max_upload_mb=) sets the option before this file is sourced,
# so an explicit caller choice (even a smaller one) is respected here.
if (is.null(getOption("shiny.maxRequestSize")))
  options(shiny.maxRequestSize = 500 * 1024^2)  # 500 MB

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

# Coordinate parsing for the Add points typed-coordinate inputs: DMS free text
# -> decimal degree, and UTM easting/northing -> lon/lat. Both live in the
# package (R/coords.R) so they are unit tested and counted in coverage; alias
# them here (the package is attached above when installed).
.parse_dms     <- mappingAS:::.parse_dms
.utm_to_lonlat <- mappingAS:::.utm_to_lonlat

# Row-bind two point sf objects that may carry different attribute columns:
# the union of columns is kept, missing values filled with NA, so the uploaded
# occurrences and the hand-added points combine without dropping attributes
# (e.g. collector / voucher columns used by the factsheet).
.rbind_sf_fill <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  ga <- attr(a, "sf_column"); gb <- attr(b, "sf_column")
  if (!identical(ga, gb)) { names(b)[names(b) == gb] <- ga; b <- sf::st_set_geometry(b, ga) }
  cols <- union(setdiff(names(a), ga), setdiff(names(b), ga))
  for (cc in setdiff(cols, names(a))) a[[cc]] <- NA
  for (cc in setdiff(cols, names(b))) b[[cc]] <- NA
  a <- a[c(cols, ga)]; b <- b[c(cols, ga)]
  rbind(a, b)
}

# Combine the uploaded occurrences (an sf or NULL) with the hand-added points
# (a data.frame of species/lon/lat or NULL) into a single point sf. Returns
# NULL when there is nothing at all to assess.
.combine_occ <- function(file_occ, manual_df) {
  man_sf <- NULL
  if (!is.null(manual_df) && nrow(manual_df)) {
    ok <- is.finite(manual_df$lon) & is.finite(manual_df$lat)
    manual_df <- manual_df[ok, , drop = FALSE]
    if (nrow(manual_df)) {
      man_sf <- sf::st_as_sf(
        data.frame(species = as.character(manual_df$species),
                   stringsAsFactors = FALSE),
        geometry = sf::st_sfc(
          lapply(seq_len(nrow(manual_df)),
                 function(i) sf::st_point(c(manual_df$lon[i], manual_df$lat[i]))),
          crs = 4326))
      man_sf$.source <- "added"
    }
  }
  if (is.null(file_occ) && is.null(man_sf)) return(NULL)
  if (!is.null(file_occ) && is.null(file_occ[[".source"]]))
    file_occ$.source <- "uploaded"
  .rbind_sf_fill(file_occ, man_sf)
}

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
      "file", "Upload occurrence data (optional: CSV, XLSX, GeoPackage, GeoJSON or shapefile)",
      accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls",
                 ".gpkg", ".geojson", ".json", ".zip")
    ),
    helpText(htmltools::HTML(
      "Tip: for shapefiles, upload a .zip containing .shp/.shx/.dbf/.prj. ",
      "You can also skip the file and drop occurrence points by hand on the ",
      "<b>Add points</b> tab, or add extra points there on top of an uploaded table.")),
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
    numericInput("loc_km", "Locations grid (km)", value = 10, min = 1, step = 1),
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
        "<a href='https://www.protectedplanet.net' target='_blank'>Protected Planet</a>.")),
      selectInput(
        "loc_method", "Locations: protected-area method",
        choices = c("One location per protected area" = "no_more_than_one",
                    "Grid inside/outside separately" = "other"),
        selected = "no_more_than_one"),
      helpText(htmltools::HTML(
        "How occurrences inside protected areas are counted as locations. ",
        "<b>One location per protected area</b>: each protected area with ",
        "occurrences is a single location. <b>Grid inside/outside separately</b>: ",
        "the two groups are gridded apart and summed."))
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
    # TAB: ADD / EDIT OCCURRENCE POINTS BY HAND
    bslib::nav_panel(
      "Add points", icon = icon("map-pin"),
      bslib::layout_columns(
        col_widths = c(8, 4),
        bslib::card(
          full_screen = TRUE,
          helpText(htmltools::HTML(
            "Click on the map to drop an occurrence point for the species selected ",
            "on the right. Only the <b>selected species'</b> existing points are shown: ",
            "uploaded points in grey for reference, the points you add in green. Then ",
            "press <b>Assess</b> in the sidebar - the assessment uses the uploaded table ",
            "<i>plus</i> every point added here (or only these points when no file is ",
            "uploaded).")),
          leaflet::leafletOutput("edit_map", height = "70vh")
        ),
        bslib::card(
          selectizeInput("edit_species", "Species for new points",
                         choices = NULL, selected = NULL,
                         options = list(create = TRUE,
                                        placeholder = "type or pick a species")),
          helpText(htmltools::HTML(
            "Pick a species to work on it: only that species' existing points are ",
            "shown on the map. To start a <b>new species from scratch</b>, type its ",
            "name and press Enter to create it - the map starts empty, then click it ",
            "or type coordinates below to add the points.")),
          radioButtons(
            "edit_coord_fmt", "Coordinate type",
            choices = c("Decimal degrees" = "dd",
                        "Degrees, minutes, seconds" = "dms",
                        "UTM (metres)" = "utm"),
            selected = "dd"),
          uiOutput("edit_coord_help"),
          # Decimal degrees: two plain numbers (e.g. -47.9292, -15.7801).
          conditionalPanel(
            condition = "input.edit_coord_fmt == 'dd'",
            fluidRow(
              column(6, numericInput("edit_lon", "Longitude", value = NA,
                                     step = 0.0001)),
              column(6, numericInput("edit_lat", "Latitude", value = NA,
                                     step = 0.0001)))),
          # Degrees/minutes/seconds: free text so the hemisphere and symbols are
          # allowed (e.g. 15 46 48 S).
          conditionalPanel(
            condition = "input.edit_coord_fmt == 'dms'",
            fluidRow(
              column(6, textInput("edit_lon_dms", "Longitude (D M S + E/W)",
                                  value = "", placeholder = "47 55 45 W")),
              column(6, textInput("edit_lat_dms", "Latitude (D M S + N/S)",
                                  value = "", placeholder = "15 46 48 S")))),
          # UTM: easting/northing in metres plus the zone and hemisphere.
          conditionalPanel(
            condition = "input.edit_coord_fmt == 'utm'",
            fluidRow(
              column(6, numericInput("edit_utm_easting", "Easting (X, m)",
                                     value = NA, step = 1)),
              column(6, numericInput("edit_utm_northing", "Northing (Y, m)",
                                     value = NA, step = 1))),
            fluidRow(
              column(6, numericInput("edit_utm_zone", "UTM zone (1-60)",
                                     value = 23, min = 1, max = 60, step = 1)),
              column(6, radioButtons("edit_utm_hemi", "Hemisphere",
                                     choices = c("South" = "S", "North" = "N"),
                                     selected = "S", inline = TRUE)))),
          div(class = "d-flex gap-2 mb-2",
              actionButton("edit_add_xy", "Add typed point",
                           icon = icon("plus"), class = "btn-outline-primary")),
          hr(),
          div(class = "d-flex gap-2 mb-2 flex-wrap",
              actionButton("edit_undo", "Undo last", icon = icon("rotate-left"),
                           class = "btn-outline-secondary"),
              actionButton("edit_clear", "Clear all", icon = icon("trash"),
                           class = "btn-outline-danger")),
          uiOutput("edit_count"),
          div(class = "mt-2",
              downloadButton("dl_manual_pts", "Download added points (CSV)",
                             class = "btn-sm w-100")),
          hr(),
          DT::DTOutput("edit_tbl")
        )
      )
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
          downloadButton("dl_donut_png", "Save donut (PNG)")))
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
          tags$h5("Trend analysis", class = "card-title"),
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
          div(class = "d-flex gap-2 mt-3 mb-2 flex-wrap",
              downloadButton("dl_ts_trend_eoo_png", "Download EOO trend (PNG)"),
              downloadButton("dl_ts_trend_aoo_png", "Download AOO trend (PNG)"),
              downloadButton("dl_ts_trend", "Download trend table (CSV)")),
          helpText("Linear trend per class for each extent: slope (percentage points per year), R², p-value, and first/last/delta. Needs at least 3 years (lower the step if empty)."),
          DT::DTOutput("ts_trend_tbl")))
    ),

    bslib::nav_panel(
      "Fire", icon = icon("fire"),
      selectInput("fire_species", "Species", choices = NULL),
      uiOutput("fire_summary"),
      fluidRow(
        column(4, numericInput("fire_ts_step", "Step (years)", value = 1,
                               min = 1, max = 10, step = 1))
      ),
      div(
        class = "d-flex gap-2 mb-2",
        actionButton("fire_ts_run", "Calculate fire series", class = "btn-primary"),
        downloadButton("dl_fire_ts", "Download series (CSV)"),
        downloadButton("dl_fire_ts_png", "Save image (PNG)")
      ),
      helpText("Burned area per year (MapBiomas Fire, 1985-2024) for BOTH extents (EOO and AOO), one chart above the other. A 1-year step reads all years and may be slow; increase the step to speed up."),
      tags$div(
        class = "card mb-3",
        tags$div(
          class = "card-body",
          tags$h5("EOO — Extent of Occurrence", class = "card-title"),
          plotly::plotlyOutput("fire_ts_plot_eoo", height = "420px"))),
      tags$div(
        class = "card mb-3",
        tags$div(
          class = "card-body",
          tags$h5("AOO — Area of Occupancy", class = "card-title"),
          plotly::plotlyOutput("fire_ts_plot_aoo", height = "420px"))),
      tags$div(
        class = "card mb-3",
        tags$div(
          class = "card-body",
          tags$h5("Burned area by year — EOO and AOO", class = "card-title"),
          DT::DTOutput("fire_tbl")))
    ),

    # TAB: COUNTRIES OF OCCURRENCE AND BIOGEOGRAPHICAL REALMS (from the range)
    bslib::nav_panel(
      "Countries", icon = icon("earth-americas"),
      selectInput("coo_species", "Species", choices = NULL),
      helpText(htmltools::HTML(
        "Countries of occurrence and biogeographical realm(s), derived from the ",
        "range (EOO) and the occurrence points - the IUCN Red List supporting ",
        "information that sRedList extracts automatically. <b>Extant</b> = the ",
        "country holds at least one record; <b>Possibly Extant</b> = the range ",
        "overlaps the country but no record falls inside. Needs the ",
        "<code>rnaturalearth</code> package; the realm is a coarse ",
        "continent-based approximation (exact for South America).")),
      uiOutput("coo_summary"),
      downloadButton("dl_coo", "Download countries (CSV)", class = "mb-3"),
      DT::DTOutput("coo_tbl")
    ),
    # TAB: AREA OF HABITAT AND LOWER/UPPER AOO BOUNDS (criterion B2 range)
    bslib::nav_panel(
      "Habitat & AOO", icon = icon("layer-group"),
      selectInput("aoh_species", "Species", choices = NULL),
      helpText(htmltools::HTML(
        "<b>Area of Habitat (AOH)</b> and the resulting <b>lower-upper bounds of ",
        "AOO</b> for criterion B2. The <b>lower bound</b> is the occurrence-based ",
        "AOO (occupied 2 km cells); the <b>upper bound</b> is the <b>AOH</b> - the ",
        "suitable habitat within the EOO, capped at the EOO. The true AOO lies ",
        "between them (Brooks et al. 2019), so B2 spans a <b>range of ",
        "categories</b>. Needs land cover (tick 'Calculate land-cover conversion' ",
        "before assessing).")),
      fluidRow(
        column(6, selectizeInput(
          "aoh_classes", "Suitable habitat classes",
          choices = NULL, multiple = TRUE,
          options = list(placeholder = "Click to choose classes (empty = all natural)"))),
        column(3, numericInput("aoh_occupancy",
                               "% of habitat occupied", value = 100,
                               min = 1, max = 100, step = 5)),
        column(3, div(class = "pt-4 d-flex gap-2 flex-wrap",
                      actionButton("aoh_pick_classes", "Choose from land cover",
                                   icon = icon("layer-group"),
                                   class = "btn-outline-primary btn-sm"),
                      actionButton("aoh_classes_reset", "Reset",
                                   class = "btn-outline-secondary btn-sm")))
      ),
      helpText(htmltools::HTML(
        "Pick the land-cover classes that make ecological sense as habitat for ",
        "the species (<b>'Choose from land cover'</b> opens a window listing the ",
        "classes actually present in the range, with their area, where you mark ",
        "each as <b>suitable</b> or <b>marginal</b>). The AOH, the AOO upper bound ",
        "and the map use this selection plus the elevation band. <b>% of habitat ",
        "occupied</b> is the single occupancy correction (IUCN 4.10.7 condition ",
        "ii): it scales the potential habitat down to occupied habitat for the ",
        "AOO upper bound <i>and</i> for the population estimate below. Leave 100 ",
        "if unknown.")),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "How AOH feeds AOO/EOO - IUCN conditions (4.10.7)",
          htmltools::HTML(
            "<div style='font-size:.85rem'>A habitat map shows <i>potential</i> ",
            "habitat and is usually larger than the occupied area, so it is an ",
            "<b>upper bound</b>, valid only when: <b>(i)</b> the habitat classes ",
            "are an accurate, independently justified representation of the ",
            "species' requirements (define habitat in the strict sense - not just ",
            "a land-cover type); <b>(ii)</b> the potential habitat is adjusted by ",
            "the <b>proportion occupied</b> to estimate occupied habitat; and ",
            "<b>(iii)</b> the area is taken at the reference scale - AOO from ",
            "2 km cells intersecting the habitat, EOO from the minimum convex ",
            "polygon around it. A decline in mapped habitat also supports a ",
            "<b>continuing decline</b> under criterion B.</div>"))
      ),
      uiOutput("aoh_cards"),
      tags$div(
        class = "card mb-3",
        tags$div(
          class = "card-body",
          tags$h6("Elevation refinement (optional)", class = "card-title"),
          helpText(htmltools::HTML(
            "Refine the AOH to a species' elevation band, as sRedList does. ",
            "'Suggest from occurrences' reads a DEM at the points (windowed) and ",
            "fills the limits; 'Compute AOH' then intersects the suitable habitat ",
            "with the band and re-derives the AOO upper bound. Needs the ",
            "<code>elevatr</code> package (AWS terrain tiles, no account) or a DEM ",
            "file, plus land cover for the species.")),
          fluidRow(
            column(3, numericInput("aoh_elev_min", "Min elevation (m)",
                                   value = NA, step = 50)),
            column(3, numericInput("aoh_elev_max", "Max elevation (m)",
                                   value = NA, step = 50)),
            column(3, selectInput("aoh_z", "DEM detail (zoom)",
                                  choices = c("Coarse (z7)" = 7, "Medium (z9)" = 9,
                                              "Fine (z10)" = 10), selected = 9)),
            column(3, div(class = "pt-4 d-flex gap-2 flex-wrap",
                          actionButton("aoh_elev_suggest", "Suggest from occurrences",
                                       class = "btn-outline-secondary btn-sm"),
                          actionButton("aoh_run", "Compute AOH",
                                       class = "btn-primary btn-sm")))
          ),
          uiOutput("aoh_elev_summary")
        )
      ),
      tags$div(
        class = "card mb-3",
        tags$div(
          class = "card-body",
          tags$h6("Population size & density (criteria C / D)",
                  class = "card-title"),
          helpText(htmltools::HTML(
            "Population size = <b>occupied AOH &times; density</b>. The occupancy ",
            "correction is the <b>% of habitat occupied</b> field at the top of ",
            "the tab (no need to enter it twice). Enter the density directly, or ",
            "use the calculator: density in occupied habitat &times; % mature. A ",
            "range (e.g. <code>3-8</code>) gives a low&ndash;high estimate. ",
            "Compute the AOH above first.")),
          fluidRow(
            column(4, textInput("pop_density", "Density (mature ind/km2)",
                                value = "", placeholder = "e.g. 5  or  3-8")),
            column(4, numericInput("pop_dens_total",
                                   "or density in occupied habitat", value = NA,
                                   min = 0)),
            column(4, numericInput("pop_pct_mature", "% mature", value = NA,
                                   min = 0, max = 100))
          ),
          uiOutput("aoh_popsize")
        )
      ),
      div(style = "height:430px; min-height:430px;",
          plotly::plotlyOutput("aoh_plot", height = "100%")),
      tags$hr(),
      tags$h6("Area of Habitat map"),
      helpText(htmltools::HTML(
        "Suitable habitat (the selected classes, within the elevation band if ",
        "set) inside the EOO, in green, with the EOO hull and the occurrence ",
        "points. Click <b>'Compute AOH'</b> above to build it. Reads land cover ",
        "(and a DEM when an elevation band is set); may take a few seconds.")),
      leaflet::leafletOutput("aoh_map", height = "70vh")
    ),
    # TAB: SEVERE FRAGMENTATION (criterion B sub-criterion a)
    bslib::nav_panel(
      "Fragmentation", icon = icon("puzzle-piece"),
      selectInput("frag_species", "Species", choices = NULL),
      helpText(htmltools::HTML(
        "<b>Severe fragmentation</b> (criterion B, sub-criterion <b>a</b>): a taxon ",
        "qualifies when <b>more than half</b> of its population lives in ",
        "subpopulations that are <b>both</b> too <b>small</b> to be viable <b>and</b> ",
        "too <b>isolated</b> to be rescued if they blink out. You tick it on the ",
        "Assessment tab - it is never set automatically.")),
      helpText(htmltools::HTML(
        "<b>How this tab estimates it:</b> it takes the <b>Area of Habitat</b> from ",
        "the <i>Habitat &amp; AOO</i> tab (your suitable classes and elevation ",
        "band), then <b>(1)</b> breaks it into patches, <b>(2)</b> merges patches ",
        "closer than the <b>isolation distance</b> into subpopulations, and ",
        "<b>(3)</b> sizes each one. Enter a <b>density</b> to size them in ",
        "individuals (recommended); leave it blank to fall back on a rough count ",
        "of occurrences. The land-cover habitat is an approximation of the area ",
        "truly habitable by the species.")),
      fluidRow(
        column(3, numericInput("frag_iso_km", "Isolation distance (km)",
                               value = 20, min = 0.1, step = 1)),
        column(3, textInput("frag_density",
                            "Density (ind/km2; blank = proxy)",
                            value = "", placeholder = "e.g. 5  or  3-8")),
        column(3, numericInput("frag_small",
                               "'Small' size (individuals; e.g. 100)",
                               value = NA, min = 0, step = 1)),
        column(3, div(class = "pt-4",
                      actionButton("frag_run", "Analyse fragmentation",
                                   class = "btn-primary")))
      ),
      helpText(htmltools::HTML(
        "<b>Isolation distance</b>: set it to several times the species' average ",
        "dispersal distance. <b>'Small' size</b>: the subpopulation size you treat ",
        "as non-viable (often ~100 individuals for vertebrates); leave it blank to ",
        "read it off the <b>median</b> line - the size below which half the ",
        "population lives.")),
      uiOutput("frag_summary"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        div(style = "height:440px; min-height:440px;",
            plotly::plotlyOutput("frag_curve", height = "100%")),
        div(
          tags$div(class = "text-muted small mb-1",
                   "Subpopulations (clusters) and occurrences"),
          leaflet::leafletOutput("frag_map", height = "410px"))
      ),
      downloadButton("dl_frag", "Download subpopulations (CSV)", class = "mt-2"),
      DT::DTOutput("frag_tbl")
    ),
    bslib::nav_panel(
      "Assessment", icon = icon("clipboard-check"),
      selectInput("results_species", "Species", choices = NULL),
      uiOutput("results_cards"),
      bslib::card(
        class = "mb-3",
        bslib::card_header("IUCN Criterion B — applied category"),
        bslib::card_body(
          helpText(htmltools::HTML(
            "The buttons below <b>start out showing the automatic result</b> for ",
            "the selected species: the sub-criteria the assessment found are ",
            "pre-checked, and the category badge reflects them. You keep full ",
            "control - <b>uncheck</b> any sub-criterion to override it (for ",
            "example, drop the taxon to <b>NT</b> or <b>LC</b>), and re-checking ",
            "the automatic state restores the computed category. A threatened ",
            "listing (CR/EN/VU) needs the size threshold plus <b>at least two</b> ",
            "sub-criteria. <b>(a)</b> (few locations / fragmented) is derived from ",
            "the data and <b>caps</b> the category by the number of locations; ",
            "<b>(b)</b> continuing decline is inferred from the habitat loss (or ",
            "assumed when no land cover is available); <b>(c)</b> extreme ",
            "fluctuation cannot be read from occurrence points. A range that meets ",
            "a size threshold but not two sub-criteria (e.g. too many locations, ",
            "or decline unchecked) is <b>NT</b>. DD/NE are left to your judgement.")),
          fluidRow(
            column(4, checkboxInput(
              "cb_decline",
              "(b) Continuing decline (EOO/AOO/habitat/locations/individuals)",
              TRUE)),
            column(4, checkboxInput(
              "cb_fluct", "(c) Extreme fluctuations", FALSE)),
            column(4, checkboxInput(
              "cb_frag", "Severely fragmented (feeds sub-criterion a)", FALSE))
          ),
          uiOutput("results_appliedB")
        )
      ),
      bslib::card(
        class = "mb-3",
        bslib::card_header("Diagnosis under uncertainty - Criterion B category range"),
        bslib::card_body(
          helpText(htmltools::HTML(
            "Brings the analyses together: B1 from the EOO (a single value) and ",
            "B2 from the <b>bracketed AOO</b> - <b>lower</b> = occupied occurrence ",
            "cells, <b>upper</b> = <b>AOH</b> (Area of Habitat within the EOO). ",
            "The plausible screening category runs from the most-threatened ",
            "pairing (B1 with the lower AOO) to the least-threatened (B1 with the ",
            "upper AOO). A single value means the bounds agree. The sub-criteria ",
            "(a/b/c) above still apply on top of these size flags.")),
          uiOutput("assess_uncertainty")
        )
      ),
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
            column(4, textInput("fs_life_form", "Life Form", "")),
            column(4, textInput("fs_substrate", "Substrate", ""))),
          fluidRow(
            column(4, textInput("fs_biome", "Biome", "")),
            column(4, textInput("fs_habitat", "Habitat", "")),
            column(4, textInput("fs_vegetation", "Vegetation", "")))
        ),
        bslib::accordion_panel(
          "Land use, conservation units and vouchers",
          helpText("Land use and conservation units are filled in automatically from the package (the Conversion and Protected areas modules): leave the boxes blank to use them. Type something only to override."),
          textAreaInput("fs_land_use", "Land use (auto from Conversion; optional override)", "", rows = 2,
                        placeholder = "auto: anthropic classes within the EOO"),
          textAreaInput("fs_cons_units", "Conservation units (auto from Protected areas; optional override)", "", rows = 2,
                        placeholder = "auto: protected areas overlapping the range"),
          textAreaInput("fs_vouchers", "Examined vouchers (one per line)", "",
                        rows = 4, placeholder = "Barreira 123 (RB)\nSilva 456 (R)"),
          helpText(htmltools::HTML(
            "Vouchers are loaded automatically from the uploaded table for the ",
            "selected species: from a <b>voucher</b> column if present, otherwise ",
            "built from <b>collector</b> + <b>collectorNumber</b> (a herbarium/",
            "institutionCode column is added in parentheses). Your edits are kept; ",
            "use the button to reload from the table.")),
          actionButton("fs_vouchers_load", "Load vouchers from table",
                       icon = icon("table-list"), class = "btn-outline-secondary btn-sm")
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

        <h4>Full-criteria analyses (assimilated from sRedList)</h4>
        <p>Beyond the size metrics, three analyses - inspired by the sRedList
        platform (Cazalis <i>et al.</i> 2024) and computed entirely from the data
        above - sharpen the Criterion B diagnosis. They appear as dedicated tabs
        just before the <b>Assessment</b> tab, where the results are brought
        together.</p>
        <ul>
          <li><b>Countries of occurrence &amp; realms</b>: the countries the range
              overlaps (flagged <i>Extant</i> where a record falls inside, else
              <i>Possibly Extant</i>) and an approximate biogeographical realm,
              from the Natural Earth base map (needs the <code>rnaturalearth</code>
              package). Required/recommended Red List supporting information.</li>
          <li><b>Area of Habitat &amp; AOO bounds</b>: the AOO is bracketed between
              a <i>lower</i> bound (the occurrence-based occupied cells) and an
              <i>upper</i> bound (the natural/suitable habitat within the EOO, i.e.
              the Area of Habitat, capped at the EOO). The true AOO lies between
              them (Brooks <i>et al.</i> 2019), so B2 spans a <b>range</b> of size
              categories instead of a single value. Optionally, the AOH can be
              <b>refined by elevation</b>: a DEM read over the range (windowed,
              via the <code>elevatr</code> package or a DEM file) restricts the
              suitable habitat to the species' elevation band, and the limits can
              be suggested from the elevation of the occurrence records.</li>
          <li><b>Severe fragmentation</b>: occurrences within a user-set isolation
              distance are clustered into subpopulations; the tool reports the
              share of the population in small subpopulations (a screening proxy
              after Santini <i>et al.</i> 2019), to guide sub-criterion (a). It is
              never ticked automatically - the judgement stays with the
              assessor.</li>
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

  # Hand-added points: a growing data.frame of species / lon / lat, fed by
  # clicks on the "Add points" map (and the typed-coordinate button).
  manual_store <- reactiveVal(
    data.frame(species = character(0), lon = numeric(0), lat = numeric(0),
               stringsAsFactors = FALSE))

  # Per-species Criterion B sub-criteria set by the user (b = continuing
  # decline, c = extreme fluctuation, frag = severely fragmented). Kept in a
  # named list so switching species remembers each one's inputs. Used to
  # re-apply iucn_criterion_B() live, without re-running the assessment.
  subcrit_store <- reactiveVal(list())

  # The automatic (assessment) sub-criteria for a species: the default state the
  # buttons show - what the result WOULD be before any assessor override. (b)
  # comes from the assessment (documented / inferred from habitat loss / assumed)
  # and (c) from the assessment; (frag) is not recoverable from the summary, so
  # it defaults to FALSE (sub-criterion (a) already reflects the locations).
  .computed_subcrit <- function(sp) {
    s <- tryCatch(result()$summary, error = function(e) NULL)
    dflt <- list(b = TRUE, c = FALSE, frag = FALSE)
    if (is.null(s) || !is.data.frame(s) || !nrow(s) ||
        is.null(sp) || !nzchar(sp)) return(dflt)
    r <- s[s$species == sp, , drop = FALSE]
    if (!nrow(r)) return(dflt)
    list(b    = if ("subcrit_b" %in% names(r)) isTRUE(r$subcrit_b[1]) else TRUE,
         c    = if ("subcrit_c" %in% names(r)) isTRUE(r$subcrit_c[1]) else FALSE,
         frag = FALSE)
  }

  # The checkbox state for a species: an explicit assessor override if one exists,
  # otherwise the automatic assessment result (so the buttons start out showing
  # the computed category and can be unchecked to drop the taxon to NT/LC).
  .get_subcrit <- function(sp) {
    v <- subcrit_store()[[sp]]
    if (is.null(v)) .computed_subcrit(sp) else v
  }

  # result()$summary with category_B / criterion_B_code re-applied from the
  # user's per-species sub-criteria inputs (falling back to the stored defaults).
  summary_applied <- reactive({
    req(result())
    s <- result()$summary
    if (is.null(s) || !nrow(s) || !"eoo_km2" %in% names(s)) return(s)
    store <- subcrit_store()
    for (i in seq_len(nrow(s))) {
      v <- store[[s$species[i]]]
      if (is.null(v)) next
      cb <- mappingAS::iucn_criterion_B(
        eoo_km2 = s$eoo_km2[i], aoo_km2 = s$aoo_km2[i],
        n_locations = if ("n_locations" %in% names(s)) s$n_locations[i] else NA,
        severe_fragmentation = isTRUE(v$frag),
        decline = isTRUE(v$b), extreme_fluctuation = isTRUE(v$c))
      s$category_B[i]       <- cb$category
      s$criterion_B_code[i] <- cb$code
      if ("subcrit_a" %in% names(s)) s$subcrit_a[i] <- cb$a
      if ("subcrit_b" %in% names(s)) s$subcrit_b[i] <- cb$b
      if ("subcrit_c" %in% names(s)) s$subcrit_c[i] <- cb$c
      # The checkboxes are an explicit assessor choice, so (b) is documented.
      if ("decline_assumed" %in% names(s)) s$decline_assumed[i] <- FALSE
      if ("decline_basis" %in% names(s))
        s$decline_basis[i] <- if (isTRUE(v$b)) "documented" else "documented (absent)"
    }
    s
  })

  # The assessment with the user's applied Criterion B category folded into its
  # $summary, so the Report and Factsheet reflect the sub-criteria set on the
  # Assessment tab (buttons under "IUCN Criterion B - applied category") instead of
  # the size-only defaults.
  result_applied <- reactive({
    res <- result(); req(res)
    s <- tryCatch(summary_applied(), error = function(e) NULL)
    if (!is.null(s) && is.data.frame(s) && nrow(s)) res$summary <- s
    res
  })

  # Applied Criterion B category + full code for one species (from the live
  # summary), passed on to the report text and the factsheet HTML.
  .applied_B <- function(sp) {
    s <- tryCatch(summary_applied(), error = function(e) NULL)
    if (is.null(s) || !is.data.frame(s) || !nrow(s)) return(NULL)
    r <- s[s$species == sp, , drop = FALSE]
    if (!nrow(r)) return(NULL)
    cat <- if ("category_B" %in% names(r)) r$category_B[1] else NA
    code <- if ("criterion_B_code" %in% names(r)) r$criterion_B_code[1] else NA
    if ((is.null(cat) || is.na(cat)) && (is.null(code) || is.na(code))) return(NULL)
    list(category = cat, code = code)
  }

  # Seed the three checkboxes for the selected species from its current state
  # (assessment result, or an existing override), so the buttons show what the
  # result would be. Runs on species change and whenever a new assessment lands.
  .sync_checkboxes <- function() {
    sp <- input$results_species
    if (is.null(sp) || !nzchar(sp)) return()
    v <- .get_subcrit(sp)
    updateCheckboxInput(session, "cb_decline", value = isTRUE(v$b))
    updateCheckboxInput(session, "cb_fluct",   value = isTRUE(v$c))
    updateCheckboxInput(session, "cb_frag",    value = isTRUE(v$frag))
  }
  observeEvent(input$results_species, .sync_checkboxes(), ignoreInit = FALSE)
  # A fresh assessment invalidates prior per-species overrides; clear them and
  # re-seed the buttons from the new result.
  observeEvent(result(), {
    subcrit_store(list())
    .sync_checkboxes()
  }, ignoreInit = TRUE)

  # Persist checkbox edits. Only a state that DIFFERS from the automatic result
  # is stored as an override; reverting the buttons to the computed default drops
  # the override, so the taxon returns to its assessment category.
  observeEvent(list(input$cb_decline, input$cb_fluct, input$cb_frag), {
    sp <- input$results_species
    if (is.null(sp) || !nzchar(sp)) return()
    new <- list(b = isTRUE(input$cb_decline),
                c = isTRUE(input$cb_fluct),
                frag = isTRUE(input$cb_frag))
    st <- subcrit_store()
    if (identical(new, .computed_subcrit(sp))) st[[sp]] <- NULL else st[[sp]] <- new
    subcrit_store(st)
  }, ignoreInit = TRUE)

  # Applied Criterion B category for the selected species (live).
  output$results_appliedB <- renderUI({
    req(result(), input$results_species)
    s <- summary_applied()
    r <- s[s$species == input$results_species, , drop = FALSE]
    validate(need(nrow(r) > 0, "Select a species."))
    r <- r[1, , drop = FALSE]
    cat_final <- if ("category_B" %in% names(r)) r$category_B else NA
    code <- if ("criterion_B_code" %in% names(r)) r$criterion_B_code else ""
    b <- mappingAS:::.iucn_badge(cat_final)
    badge <- sprintf(
      "<span style='display:inline-flex;align-items:center;justify-content:center;min-width:40px;height:40px;padding:0 10px;border-radius:20px;background:%s;color:%s;font-weight:700;font-size:1rem;border:2px solid rgba(0,0,0,.15)'>%s</span>",
      b$bg, b$fg, if (is.na(cat_final)) "NA" else cat_final)
    sub <- function(ok, lab) sprintf(
      "<span style='margin-right:12px'>%s %s</span>",
      if (isTRUE(ok)) "✅" else "⬜", lab)
    htmltools::HTML(sprintf(
      "<div style='display:flex;align-items:center;gap:12px;flex-wrap:wrap'>%s
       <div><div style='font-family:monospace;font-weight:700;font-size:1.05rem'>%s</div>
       <div style='font-size:.82rem;color:#5a655c;margin-top:2px'>%s%s%s</div></div></div>",
      badge, if (is.na(code) || !nzchar(code)) "&mdash;" else code,
      sub(r$subcrit_a, "(a) few locations / fragmented"),
      sub(r$subcrit_b, "(b) continuing decline"),
      sub(r$subcrit_c, "(c) extreme fluctuation")))
  })

  # ===========================================================================
  # sRedList-assimilated analyses, all computed from the assessment detail
  # (points, EOO, AOO and the natural-habitat area assess_species() already
  # measured) - no new data source and no re-run:
  #   * countries of occurrence & biogeographical realm(s);
  #   * Area of Habitat and the lower/upper AOO bounds (Criterion B2 range);
  #   * severe fragmentation (Criterion B sub-criterion a).
  # ===========================================================================
  .detail_for <- function(sp) {
    d <- tryCatch(result()$detail, error = function(e) NULL)
    if (is.null(d) || is.null(sp) || !nzchar(sp)) return(NULL)
    d[[sp]]
  }

  # ---- Countries of occurrence & biogeographical realms ---------------------
  coo_data <- reactive({
    req(result(), input$coo_species)
    obj <- .detail_for(input$coo_species); req(obj)
    hull <- tryCatch(obj$eoo$hull, error = function(e) NULL)
    rng <- if (!is.null(hull)) hull else obj$points
    tryCatch(
      mappingAS::countries_of_occurrence(rng, points = obj$points),
      error = function(e) NULL)
  })

  output$coo_summary <- renderUI({
    req(result(), input$coo_species)
    df <- coo_data()
    if (is.null(df) || !nrow(df))
      return(helpText(htmltools::HTML(
        "No countries found. This needs the <code>rnaturalearth</code> package ",
        "(install.packages(\"rnaturalearth\")) and at least one occurrence ",
        "over land.")))
    ext <- sum(df$presence == "Extant")
    pos <- sum(df$presence == "Possibly Extant")
    realms <- sort(unique(stats::na.omit(df$realm)))
    card <- function(t, v) sprintf(
      "<div style='flex:1;min-width:150px;border:1px solid #e4ddce;border-radius:.6rem;padding:10px 12px;background:#fffdf8'><div style='color:#7a857b;font-size:.78rem'>%s</div><div style='font-family:monospace;font-weight:700;font-size:1.05rem'>%s</div></div>",
      t, v)
    htmltools::HTML(sprintf(
      "<div style='display:flex;gap:10px;flex-wrap:wrap;margin-bottom:12px'>%s%s%s%s</div>",
      card("Countries (total)", nrow(df)),
      card("Extant", ext),
      card("Possibly Extant", pos),
      card("Realm(s)", if (length(realms)) paste(realms, collapse = ", ") else "&mdash;")))
  })

  output$coo_tbl <- DT::renderDT({
    req(result(), input$coo_species)
    df <- coo_data()
    validate(need(!is.null(df) && nrow(df) > 0,
                  "No countries found (needs the 'rnaturalearth' package)."))
    .mas_dt(df, page = 15, caption = "Countries of occurrence and realm(s)")
  })

  output$dl_coo <- downloadHandler(
    filename = function() paste0("mappingAS_countries_",
                                 input$coo_species, "_", Sys.Date(), ".csv"),
    content = .safe_download(function(file) {
      df <- coo_data()
      utils::write.csv(if (is.null(df)) data.frame() else df, file,
                       row.names = FALSE)
    })
  )

  # ---- Area of Habitat and lower/upper AOO bounds (Criterion B2 range) -------
  # Elevation-refined AOH (button-triggered): reads land cover + a DEM over the
  # EOO and restricts the suitable habitat to the elevation band.
  # Marginal / unknown-suitability class codes (chosen in the modal below).
  aoh_marginal <- reactiveVal(character(0))

  # Land-cover classes actually present in a species' EOO, with area and %.
  .aoh_present_classes <- function(sp) {
    obj <- .detail_for(sp); if (is.null(obj)) return(NULL)
    bc <- tryCatch(obj$eoo_conversion$by_class, error = function(e) NULL)
    if (is.null(bc) || !nrow(bc)) return(NULL)
    labcol <- if ((input$lang %||% "en") == "en") "class_en" else "class_pt"
    lab <- if (labcol %in% names(bc)) bc[[labcol]] else NULL
    if (is.null(lab)) lab <- paste("Class", bc$code)
    lab[is.na(lab)] <- paste("Class", bc$code[is.na(lab)])
    tot <- sum(bc$area_km2, na.rm = TRUE)
    pct <- if (tot > 0) 100 * bc$area_km2 / tot else rep(NA_real_, nrow(bc))
    df <- data.frame(
      code = as.integer(bc$code), group = as.character(bc$group),
      area_km2 = as.numeric(bc$area_km2), pct = pct,
      label = sprintf("%s  [%s] — %s km² (%.1f%%)", lab, bc$group,
                      formatC(bc$area_km2, format = "f", big.mark = ",",
                              digits = 1), pct),
      stringsAsFactors = FALSE)
    df <- df[is.finite(df$area_km2) & df$area_km2 > 0, , drop = FALSE]
    df[order(-df$area_km2), , drop = FALSE]
  }

  # Open a window listing the classes present in the range to mark suitable /
  # marginal (IUCN 4.10.7 condition i: choose habitat from what is actually there).
  observeEvent(input$aoh_pick_classes, {
    req(input$aoh_species)
    df <- .aoh_present_classes(input$aoh_species)
    if (is.null(df) || !nrow(df)) {
      showModal(modalDialog(
        title = "Habitat classes", easyClose = TRUE,
        "No land-cover classes found for this species. Run the assessment with ",
        "'Calculate land-cover conversion' enabled first."))
      return()
    }
    ch <- stats::setNames(as.character(df$code), df$label)
    cur_suit <- isolate(input$aoh_classes)
    cur_marg <- isolate(aoh_marginal())
    showModal(modalDialog(
      title = "Choose habitat classes present in the range", size = "l",
      easyClose = TRUE,
      helpText(htmltools::HTML(
        "Classes found inside the EOO, largest first. Mark each as <b>Suitable</b> ",
        "(counts as habitat / AOH) or <b>Marginal</b> (adds an upper AOH). ",
        "Unmarked = not habitat. Define habitat in the strict sense, not just a ",
        "land-cover type (IUCN 4.10.7).")),
      fluidRow(
        column(6, checkboxGroupInput("aoh_modal_suitable", "Suitable habitat",
                                     choices = ch, selected = cur_suit)),
        column(6, checkboxGroupInput("aoh_modal_marginal", "Marginal / unknown",
                                     choices = ch, selected = cur_marg))
      ),
      footer = tagList(modalButton("Cancel"),
                       actionButton("aoh_modal_apply", "Apply",
                                    class = "btn-primary"))
    ))
  })

  observeEvent(input$aoh_modal_apply, {
    suit <- input$aoh_modal_suitable %||% character(0)
    marg <- input$aoh_modal_marginal %||% character(0)
    marg <- setdiff(marg, suit)   # a class marked both ways counts as suitable
    updateSelectizeInput(session, "aoh_classes", selected = suit)
    aoh_marginal(marg)
    removeModal()
  })

  aoh_refined <- eventReactive(input$aoh_run, {
    req(result(), input$aoh_species)
    obj <- .detail_for(input$aoh_species); req(obj)
    hull <- tryCatch(obj$eoo$hull, error = function(e) NULL)
    validate(need(!is.null(hull),
                  "The EOO polygon is undefined (needs >= 3 unique points)."))
    st <- result()$settings
    r <- withProgress(message = "Reading land cover + elevation for AOH...",
                      value = 0, {
      codes <- if (length(input$aoh_classes))
        as.integer(input$aoh_classes) else NULL
      marg <- aoh_marginal()
      marg <- if (length(marg)) as.integer(marg) else NULL
      emin <- if (is.null(input$aoh_elev_min) || is.na(input$aoh_elev_min))
        NA else input$aoh_elev_min
      emax <- if (is.null(input$aoh_elev_max) || is.na(input$aoh_elev_max))
        NA else input$aoh_elev_max
      zz <- as.integer(input$aoh_z %||% 9)
      out <- tryCatch(mappingAS::calc_aoh(
        hull,
        year = obj$year %||% st$year,
        collection = obj$collection %||% st$collection,
        initiative = obj$initiative %||% st$initiative %||% "brazil",
        elev_min = emin, elev_max = emax, z = zz, suitable_codes = codes,
        marginal_codes = marg, points = obj$points),
        error = function(e) {
          showNotification(paste("AOH:", conditionMessage(e)), type = "error")
          NULL })
      incProgress(1)
      # Keep the exact parameters so the map below matches the computed AOH.
      list(out = out, params = list(codes = codes, marginal = marg,
                                    elev_min = emin, elev_max = emax, z = zz))
    })
    if (is.null(r$out)) return(NULL)
    list(species = input$aoh_species, aoh = r$out, params = r$params)
  })

  # Suggested elevation preferences from the occurrences (button-triggered).
  elev_pref <- eventReactive(input$aoh_elev_suggest, {
    req(result(), input$aoh_species)
    obj <- .detail_for(input$aoh_species); req(obj)
    r <- withProgress(message = "Reading elevation at occurrences...", value = 0, {
      out <- tryCatch(
        mappingAS::elevation_preferences(obj$points,
                                         z = as.integer(input$aoh_z %||% 9)),
        error = function(e) {
          showNotification(paste("Elevation:", conditionMessage(e)),
                           type = "error"); NULL })
      incProgress(1); out
    })
    r
  })

  # Push the suggested limits into the numeric inputs when they arrive.
  observeEvent(elev_pref(), {
    p <- elev_pref()
    if (!is.null(p) && is.finite(p$suggested_min))
      updateNumericInput(session, "aoh_elev_min", value = p$suggested_min)
    if (!is.null(p) && is.finite(p$suggested_max))
      updateNumericInput(session, "aoh_elev_max", value = p$suggested_max)
  }, ignoreInit = TRUE)

  output$aoh_elev_summary <- renderUI({
    p <- tryCatch(elev_pref(), error = function(e) NULL)
    if (is.null(p) || !isTRUE(p$n > 0))
      return(helpText(htmltools::HTML(
        "Click <b>Suggest from occurrences</b> to read the elevation of the ",
        "records (needs the <code>elevatr</code> package or a DEM).")))
    fmt <- function(x) if (is.null(x) || is.na(x)) "&mdash;"
                       else formatC(x, format = "f", digits = 0, big.mark = ",")
    htmltools::HTML(sprintf(
      paste0("<div style='font-size:.85rem'>Elevation at %d occurrences: ",
             "min <b>%s</b>, max <b>%s</b>, median <b>%s</b> m &middot; ",
             "suggested band <b>%s&ndash;%s m</b> (filled above).</div>"),
      as.integer(p$n), fmt(p$min), fmt(p$max), fmt(p$median),
      fmt(p$suggested_min), fmt(p$suggested_max)))
  })

  aoh_data <- reactive({
    req(result(), input$aoh_species)
    obj <- .detail_for(input$aoh_species); req(obj)
    proxy <- tryCatch(as.numeric(obj$eoo_conversion$natural_km2),
                      error = function(e) NA_real_)
    ref <- tryCatch(aoh_refined(), error = function(e) NULL)
    refined <- if (!is.null(ref) && identical(ref$species, input$aoh_species))
      ref$aoh else NULL
    # Upper-bound habitat area, at the 2 km reference scale when available (IUCN
    # 4.10.7 condition iii): prefer the rescaled AOO upper, else the broad AOH
    # area, else the natural-habitat proxy.
    scale2k <- !is.null(refined) && is.finite(refined$aoo_upper_km2)
    aoh_pot <- if (!is.null(refined)) {
      if (scale2k) refined$aoo_upper_km2
      else if (is.finite(refined$aoh_max_km2)) refined$aoh_max_km2
      else if (is.finite(refined$aoh_km2)) refined$aoh_km2 else proxy
    } else proxy
    # Adjust potential habitat to occupied habitat (IUCN 4.10.7 condition ii).
    occ <- suppressWarnings(as.numeric(input$aoh_occupancy))
    if (!is.finite(occ) || occ <= 0 || occ > 100) occ <- 100
    aoh_eff <- if (is.finite(aoh_pot)) aoh_pot * occ / 100 else aoh_pot
    list(bounds = mappingAS::aoo_bounds(
           aoo_lower_km2 = obj$aoo$area_km2, aoh_km2 = aoh_eff,
           eoo_km2 = obj$eoo$area_km2),
         eoo = obj$eoo$area_km2, proxy = proxy, refined = refined,
         occupancy = occ, aoh_potential = aoh_pot, scale2k = scale2k)
  })

  output$aoh_cards <- renderUI({
    req(result(), input$aoh_species)
    a <- aoh_data(); b <- a$bounds
    fkm <- function(x) if (is.null(x) || is.na(x)) "&mdash;"
                       else formatC(x, format = "f", big.mark = ",", digits = 2)
    scat <- function(x) { if (is.null(x) || is.na(x)) return("&mdash;")
      m <- regmatches(x, regexpr("^(CR|EN|VU|NT|LC)", x))
      if (length(m) && nzchar(m)) m else "not VU/EN/CR" }
    card <- function(t, v, sub = "") sprintf(
      "<div style='flex:1;min-width:170px;border:1px solid #e4ddce;border-radius:.6rem;padding:10px 12px;background:#fffdf8'><div style='color:#7a857b;font-size:.78rem'>%s</div><div style='font-family:monospace;font-weight:700;font-size:1.05rem'>%s</div>%s</div>",
      t, v, if (nzchar(sub)) sprintf("<div style='color:#7a857b;font-size:.75rem;margin-top:2px'>%s</div>", sub) else "")
    ref <- a$refined
    # AOH value/label: strict AOH, and the strict-to-broad range when marginal
    # classes were added; note the elevation band and the occupancy adjustment.
    aoh_val <- if (!is.null(ref) && is.finite(ref$aoh_km2)) {
      if (is.finite(ref$aoh_max_km2) && ref$aoh_max_km2 > ref$aoh_km2)
        sprintf("%s&ndash;%s", fkm(ref$aoh_km2), fkm(ref$aoh_max_km2))
      else fkm(ref$aoh_km2)
    } else fkm(a$aoh_potential)
    aoh_note <- if (!is.null(ref) && is.finite(ref$aoh_km2))
                  (if (isTRUE(ref$elev_applied))
                     sprintf("suitable habitat, elevation %s&ndash;%s m",
                             if (is.na(ref$elev_min)) "?" else ref$elev_min,
                             if (is.na(ref$elev_max)) "?" else ref$elev_max)
                   else "suitable habitat (strict&ndash;broad)")
                else if (!is.finite(a$aoh_potential)) "run 'Compute AOH' to estimate"
                else "natural habitat within EOO (proxy)"
    scale_note <- if (isTRUE(a$scale2k)) "2 km reference scale" else "AOH area"
    occ_note <- if (isTRUE(a$occupancy < 100))
      sprintf("%s, %.0f%% occupied", scale_note, a$occupancy)
      else scale_note
    pct <- function(x) if (is.null(x) || is.na(x)) "&mdash;" else sprintf("%.0f%%", 100 * x)
    prev_cards <- ""
    if (!is.null(ref) && (is.finite(ref$model_prevalence) ||
                          is.finite(ref$point_prevalence)))
      prev_cards <- paste0(
        card("Model prevalence", pct(ref$model_prevalence),
             "suitable share of range"),
        card("Point prevalence", pct(ref$point_prevalence),
             "occurrences in habitat"))
    htmltools::HTML(sprintf(
      "<div style='display:flex;gap:10px;flex-wrap:wrap;margin-bottom:12px'>%s%s%s%s%s%s</div>",
      card("AOO lower (occurrence)", paste0(fkm(b$aoo_lower), " km<sup>2</sup>"), scat(b$cat_lower)),
      card("AOO upper (occupied AOH)", paste0(fkm(b$aoo_upper), " km<sup>2</sup>"),
           paste0(scat(b$cat_upper), " &middot; ", occ_note)),
      card("Area of Habitat (AOH)", paste0(aoh_val, " km<sup>2</sup>"), aoh_note),
      card("EOO (cap)", paste0(fkm(a$eoo), " km<sup>2</sup>"), "extent of occurrence"),
      card("B2 category range", if (is.na(b$cat_range)) "&mdash;" else b$cat_range, "size flag, lower&ndash;upper"),
      prev_cards))
  })

  output$aoh_popsize <- renderUI({
    a <- tryCatch(aoh_data(), error = function(e) NULL)
    validate(need(!is.null(a), "Compute the AOH above first."))
    ref <- a$refined
    aoh_lo <- if (!is.null(ref) && is.finite(ref$aoh_km2)) ref$aoh_km2
              else a$aoh_potential
    aoh_hi <- if (!is.null(ref) && is.finite(ref$aoh_max_km2)) ref$aoh_max_km2
              else aoh_lo
    validate(need(is.finite(aoh_lo),
                  "Compute the AOH above (Compute AOH) to estimate population size."))
    # Occupied habitat = potential AOH x the single occupancy correction (the
    # "% of habitat occupied" field at the top of the tab).
    occ <- a$occupancy / 100
    aoh_lo <- aoh_lo * occ; aoh_hi <- aoh_hi * occ
    dens <- .parse_density(input$pop_density)
    if (!length(dens)) {
      dt <- suppressWarnings(as.numeric(input$pop_dens_total))
      if (is.finite(dt) && dt > 0) {
        pm <- suppressWarnings(as.numeric(input$pop_pct_mature))
        dens <- dt * (if (is.finite(pm)) pm / 100 else 1)
      }
    }
    if (!length(dens))
      return(helpText(htmltools::HTML(
        "Enter a <b>density</b> (or the calculator fields) to estimate population ",
        "size.")))
    dlo <- min(dens); dhi <- max(dens)
    pop_lo <- aoh_lo * dlo; pop_hi <- aoh_hi * dhi
    fnum <- function(x) if (is.null(x) || is.na(x)) "&mdash;"
                        else formatC(round(x), format = "d", big.mark = ",")
    fden <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")
    card <- function(t, v, sub = "") sprintf(
      "<div style='flex:1;min-width:170px;border:1px solid #e4ddce;border-radius:.6rem;padding:10px 12px;background:#fffdf8'><div style='color:#7a857b;font-size:.78rem'>%s</div><div style='font-family:monospace;font-weight:700;font-size:1.05rem'>%s</div>%s</div>",
      t, v, if (nzchar(sub)) sprintf("<div style='color:#7a857b;font-size:.75rem;margin-top:2px'>%s</div>", sub) else "")
    dens_lbl <- if (length(dens) > 1) sprintf("%s&ndash;%s", fden(dlo), fden(dhi))
                else fden(dlo)
    pop_lbl <- if (isTRUE(pop_lo != pop_hi))
      sprintf("%s&ndash;%s", fnum(pop_lo), fnum(pop_hi)) else fnum(pop_lo)
    occ_txt <- if (isTRUE(a$occupancy < 100))
      sprintf(", %.0f%% occupied", a$occupancy) else ""
    htmltools::HTML(sprintf(
      "<div style='display:flex;gap:10px;flex-wrap:wrap'>%s%s</div><div style='font-size:.75rem;color:#7a857b;margin-top:6px'>Occupied AOH (potential habitat%s) &times; density; an inferred estimate, use the qualifier 'Inferred'. Informs criteria C/D and the fragmentation density.</div>",
      occ_txt,
      card("Effective density", paste0(dens_lbl, " ind/km<sup>2</sup>"),
           "mature per suitable habitat"),
      card("Population size (est.)", paste0(pop_lbl, " ind."),
           "occupied AOH &times; density")))
  })

  output$aoh_plot <- plotly::renderPlotly({
    req(result(), input$aoh_species)
    b <- aoh_data()$bounds
    validate(need(is.finite(b$aoo_lower) || is.finite(b$aoo_upper),
                  "No AOO available for this species."))
    lo <- b$aoo_lower; hi <- b$aoo_upper
    if (!is.finite(lo)) lo <- hi
    if (!is.finite(hi)) hi <- lo
    # Log ruler padded around the data and the three B2 thresholds.
    xmin <- max(1, min(lo, 10) / 3)
    xmax <- max(hi, 2000) * 1.6
    # IUCN B2 category zones (km2): CR < 10, EN 10-500, VU 500-2000, else not B2.
    zx0 <- c(xmin, 10, 500, 2000); zx1 <- c(10, 500, 2000, xmax)
    zcol <- c("#f4c9c4", "#f6ddc4", "#f6f0c4", "#d8ecd2")
    zlab <- c("CR", "EN", "VU", "not threatened (B2)")
    band <- lapply(seq_along(zx0), function(i) list(
      type = "rect", xref = "x", yref = "paper",
      x0 = zx0[i], x1 = zx1[i], y0 = 0, y1 = 1,
      fillcolor = zcol[i], line = list(width = 0), layer = "below"))
    zone_lab <- lapply(seq_along(zx0), function(i) list(
      x = log10(sqrt(zx0[i] * zx1[i])), y = 1, yref = "paper",
      text = zlab[i], showarrow = FALSE, yanchor = "bottom",
      font = list(size = 11, color = "#5a655b")))
    p <- plotly::plot_ly()
    p <- plotly::add_segments(p, x = lo, xend = hi, y = 1, yend = 1,
      line = list(color = "#4a5a4d", width = 5), showlegend = FALSE,
      hoverinfo = "skip")
    p <- plotly::add_markers(p, x = lo, y = 1, name = "AOO lower (occurrence)",
      marker = list(size = 15, color = "#1f8d49",
                    line = list(color = "white", width = 2)),
      text = sprintf("AOO lower (occupied cells): %s km2",
                     formatC(lo, format = "f", digits = 0, big.mark = ",")),
      hoverinfo = "text")
    p <- plotly::add_markers(p, x = hi, y = 1,
      name = "AOO upper (occupied AOH)",
      marker = list(size = 15, color = "#7bc47f",
                    line = list(color = "white", width = 2)),
      text = sprintf("AOO upper (habitat, capped at EOO): %s km2",
                     formatC(hi, format = "f", digits = 0, big.mark = ",")),
      hoverinfo = "text")
    plotly::layout(p,
      title = list(
        text = "Where the AOO sits among the B2 category thresholds",
        font = list(size = 13)),
      xaxis = list(title = "AOO (km2, log scale)", type = "log",
                   range = c(log10(xmin), log10(xmax)),
                   tickvals = c(10, 500, 2000),
                   ticktext = c("10", "500", "2,000")),
      yaxis = list(title = "", range = c(0.4, 1.6),
                   showticklabels = FALSE, zeroline = FALSE),
      shapes = band, annotations = zone_lab,
      legend = list(orientation = "h", x = 0, y = -0.2),
      margin = list(t = 46))
  })

  # Land-cover legend for the selected species (used to populate the suitable-
  # class picker), keyed to the product actually used for that species.
  .aoh_legend <- function(sp) {
    obj <- .detail_for(sp); if (is.null(obj)) return(NULL)
    st <- tryCatch(result()$settings, error = function(e) NULL)
    ini <- obj$initiative %||% (if (is.null(st)) "brazil" else st$initiative) %||% "brazil"
    coll <- obj$collection %||% (if (is.null(st)) NULL else st$collection)
    tryCatch(mappingAS::mb_legend(coll, ini), error = function(e) NULL)
  }
  .aoh_class_choices <- function(leg, lang) {
    labcol <- if (identical(lang, "en")) "class_en" else "class_pt"
    labs <- if (labcol %in% names(leg)) leg[[labcol]] else NULL
    if (is.null(labs)) labs <- rep(NA_character_, nrow(leg))
    labs[is.na(labs)] <- paste("Class", leg$code[is.na(labs)])
    stats::setNames(as.character(leg$code), sprintf("%s (%s)", labs, leg$group))
  }
  # Populate the suitable-class picker when the species (or result) changes,
  # defaulting to every natural class.
  observeEvent(list(result(), input$aoh_species), {
    req(input$aoh_species)
    leg <- .aoh_legend(input$aoh_species)
    if (is.null(leg) || !nrow(leg)) return()
    ch <- .aoh_class_choices(leg, input$lang %||% "en")
    nat <- as.character(leg$code[leg$group %in% "natural"])
    updateSelectizeInput(session, "aoh_classes", choices = ch,
                         selected = nat, server = FALSE)
    aoh_marginal(character(0))
  }, ignoreInit = TRUE)
  observeEvent(input$aoh_classes_reset, {
    leg <- .aoh_legend(input$aoh_species)
    if (is.null(leg) || !nrow(leg)) return()
    nat <- as.character(leg$code[leg$group %in% "natural"])
    updateSelectizeInput(session, "aoh_classes", selected = nat)
    aoh_marginal(character(0))
  })

  # AOH map: rendered after 'Compute AOH', using exactly the parameters that
  # produced the computed AOH (classes + elevation band).
  output$aoh_map <- leaflet::renderLeaflet({
    ref <- tryCatch(aoh_refined(), error = function(e) NULL)
    validate(need(!is.null(ref),
                  "Click 'Compute AOH' to build the Area of Habitat map."))
    pr <- ref$params
    withProgress(message = "Rendering Area of Habitat map...", value = 0, {
      m <- tryCatch(
        mappingAS::map_aoh(result(), species = ref$species,
                           suitable_codes = pr$codes,
                           elev_min = pr$elev_min, elev_max = pr$elev_max,
                           z = pr$z, lang = input$lang %||% "en"),
        error = function(e) {
          showNotification(paste("AOH map:", conditionMessage(e)),
                           type = "error"); NULL })
      incProgress(1)
      validate(need(!is.null(m), "Could not build the AOH map."))
      m
    })
  })

  # ---- Severe fragmentation (button-triggered) ------------------------------
  # Parse a density string ("5" or a "3-8" range) to a positive numeric vector.
  .parse_density <- function(x) {
    if (is.null(x) || !nzchar(trimws(x))) return(numeric(0))
    parts <- strsplit(trimws(x), "[-/ ]+")[[1]]
    v <- suppressWarnings(as.numeric(parts))
    v[is.finite(v) & v > 0]
  }

  frag_data <- eventReactive(input$frag_run, {
    req(result(), input$frag_species)
    obj <- .detail_for(input$frag_species); req(obj)
    iso <- suppressWarnings(as.numeric(input$frag_iso_km))
    validate(need(is.finite(iso) && iso > 0,
                  "Enter a positive isolation distance."))
    small <- if (is.null(input$frag_small) || is.na(input$frag_small)) NULL
             else input$frag_small
    dens <- .parse_density(input$frag_density)
    st <- result()$settings
    if (length(dens)) {
      # Guideline-compliant method: Area of Habitat patches x density (Santini
      # 2019). Uses the suitable classes / elevation band from the Habitat tab.
      hull <- tryCatch(obj$eoo$hull, error = function(e) NULL)
      validate(need(!is.null(hull),
                    "The EOO polygon is undefined (needs >= 3 points) for the habitat method."))
      codes <- if (length(input$aoh_classes)) as.integer(input$aoh_classes) else NULL
      emin <- if (is.null(input$aoh_elev_min) || is.na(input$aoh_elev_min))
        NA else input$aoh_elev_min
      emax <- if (is.null(input$aoh_elev_max) || is.na(input$aoh_elev_max))
        NA else input$aoh_elev_max
      zz <- as.integer(input$aoh_z %||% 9)
      withProgress(message = "Reading habitat + clustering subpopulations...",
                   value = 0, {
        out <- tryCatch(mappingAS::fragment_habitat(
          hull, isolation_km = iso, density = dens,
          year = obj$year %||% st$year,
          collection = obj$collection %||% st$collection,
          initiative = obj$initiative %||% st$initiative %||% "brazil",
          suitable_codes = codes, elev_min = emin, elev_max = emax, z = zz,
          small_size = small),
          error = function(e) {
            showNotification(paste("Fragmentation:", conditionMessage(e)),
                             type = "error"); NULL })
        incProgress(1); out
      })
    } else {
      out <- tryCatch(
        mappingAS::assess_fragmentation(obj$points, isolation_km = iso,
                                        small_size = small),
        error = function(e) {
          showNotification(paste("Fragmentation:", conditionMessage(e)),
                           type = "error"); NULL })
      if (!is.null(out)) out$method <- "occurrence"
      out
    }
  })

  output$frag_summary <- renderUI({
    fr <- frag_data()
    validate(need(!is.null(fr),
                  "Set the parameters and click 'Analyse fragmentation'."))
    hab <- identical(fr$method, "habitat")
    fmt <- function(x, d = 1) if (is.null(x) || is.na(x)) "&mdash;"
                              else formatC(x, format = "f", digits = d, big.mark = ",")
    card <- function(t, v) sprintf(
      "<div style='flex:1;min-width:150px;border:1px solid #e4ddce;border-radius:.6rem;padding:10px 12px;background:#fffdf8'><div style='color:#7a857b;font-size:.78rem'>%s</div><div style='font-family:monospace;font-weight:700;font-size:1.05rem'>%s</div></div>",
      t, v)
    sev <- if (is.na(fr$severe_suggested)) "read off median line"
           else if (isTRUE(fr$severe_suggested)) "yes (&ge;50% in small subpops)"
           else "no (&lt;50% in small subpops)"
    unit <- if (hab) " ind." else ""
    method_lbl <- if (hab)
      sprintf("Habitat &times; density (%s ind/km<sup>2</sup>)",
              paste(fr$density, collapse = "&ndash;"))
      else "Occurrence-count proxy"
    pct_largest <- if (hab) fr$pct_largest else fr$largest_subpop_pct
    cards <- c(
      card("Method", method_lbl),
      card("Subpopulations", fmt(fr$n_subpop, 0)),
      card(paste0("Median subpop size", unit), fmt(fr$median_subpop_size)),
      card("Largest subpop (% pop) - C2a(ii)", paste0(fmt(pct_largest), "%")),
      card("Severely fragmented?", sev))
    if (hab)
      cards <- append(cards,
                      card("Max in one subpop - C2a(i)", fmt(fr$max_subpop)),
                      after = 3)
    htmltools::HTML(sprintf(
      "<div style='display:flex;gap:10px;flex-wrap:wrap;margin-bottom:12px'>%s</div>",
      paste(cards, collapse = "")))
  })

  output$frag_curve <- plotly::renderPlotly({
    fr <- frag_data()
    validate(need(!is.null(fr) && nrow(fr$curve) > 0,
                  "Set the parameters and click 'Analyse fragmentation'."))
    hab <- identical(fr$method, "habitat")
    cv <- fr$curve
    xlab <- if (hab) "Subpopulation size (mature individuals)"
            else "Subpopulation size (relative, occurrences)"
    p <- plotly::plot_ly(
      x = cv$size, y = cv$prop_pop_le * 100, type = "scatter", mode = "lines",
      line = list(color = "#1f8d49", width = 2),
      hovertemplate = "size &le; %{x:.1f}: %{y:.1f}% of pop<extra></extra>")
    shapes <- list(list(type = "line", x0 = min(cv$size), x1 = max(cv$size),
                        y0 = 50, y1 = 50,
                        line = list(color = "#999", dash = "dash", width = 1)))
    if (is.finite(fr$median_subpop_size))
      shapes <- c(shapes, list(list(
        type = "line", x0 = fr$median_subpop_size, x1 = fr$median_subpop_size,
        y0 = 0, y1 = 100, line = list(color = "#d4271e", dash = "dash", width = 1))))
    plotly::layout(
      p, title = "Population in subpopulations no larger than a given size",
      xaxis = list(title = xlab),
      yaxis = list(title = "% of population", range = c(0, 100)),
      shapes = shapes)
  })

  output$frag_map <- leaflet::renderLeaflet({
    fr <- frag_data()
    validate(need(!is.null(fr) && !is.null(fr$clusters) &&
                    length(sf::st_geometry(fr$clusters)) > 0,
                  "Run the fragmentation analysis to map the subpopulations."))
    obj <- .detail_for(input$frag_species)
    cl <- tryCatch(sf::st_sf(cluster = seq_along(sf::st_geometry(fr$clusters)),
                             geometry = sf::st_transform(fr$clusters, 4326)),
                   error = function(e) NULL)
    m <- leaflet::leaflet()
    m <- leaflet::addProviderTiles(m, "Esri.WorldStreetMap", group = "Light")
    m <- leaflet::addProviderTiles(m, "Esri.WorldImagery", group = "Satellite")
    if (!is.null(cl))
      m <- leaflet::addPolygons(m, data = cl, color = "#1f8d49", weight = 1.5,
                                fillColor = "#7bc47f", fillOpacity = 0.25,
                                label = ~paste("Subpopulation", cluster),
                                group = "Subpopulations")
    if (!is.null(obj)) {
      pts <- sf::st_transform(sf::st_geometry(obj$points), 4326)
      co <- sf::st_coordinates(pts)
      m <- leaflet::addCircleMarkers(m, lng = co[, 1], lat = co[, 2], radius = 4,
                                     color = "#111111", fillColor = "#f1c40f",
                                     fillOpacity = 0.9, weight = 1,
                                     group = "Occurrences")
    }
    leaflet::addLayersControl(
      m, baseGroups = c("Light", "Satellite"),
      overlayGroups = c("Subpopulations", "Occurrences"),
      options = leaflet::layersControlOptions(collapsed = FALSE))
  })

  output$frag_tbl <- DT::renderDT({
    fr <- frag_data()
    validate(need(!is.null(fr) && nrow(fr$sizes) > 0, "No subpopulations yet."))
    .mas_dt(fr$sizes, page = 10, caption = "Subpopulations (largest first)")
  })

  output$dl_frag <- downloadHandler(
    filename = function() paste0("mappingAS_subpops_",
                                 input$frag_species, "_", Sys.Date(), ".csv"),
    content = .safe_download(function(file) {
      fr <- frag_data()
      utils::write.csv(if (is.null(fr)) data.frame() else fr$sizes, file,
                       row.names = FALSE)
    })
  )

  # ---- Assessment tab: Criterion B category range under AOO uncertainty -----
  output$assess_uncertainty <- renderUI({
    req(result(), input$results_species)
    obj <- .detail_for(input$results_species)
    validate(need(!is.null(obj), "Select a species."))
    aoh <- tryCatch(as.numeric(obj$eoo_conversion$natural_km2),
                    error = function(e) NA_real_)
    ab <- mappingAS::aoo_bounds(obj$aoo$area_km2, aoh, obj$eoo$area_km2)
    rng <- mappingAS::iucn_category_B_range(obj$eoo$area_km2,
                                            ab$aoo_lower, ab$aoo_upper)
    fkm <- function(x) if (is.null(x) || is.na(x)) "&mdash;"
                       else formatC(x, format = "f", big.mark = ",", digits = 2)
    sc <- function(x) if (is.null(x) || is.na(x)) "&mdash;" else x
    badge <- function(x) { b <- mappingAS:::.iucn_badge(x)
      sprintf("<span style='display:inline-flex;align-items:center;justify-content:center;min-width:34px;height:28px;padding:0 8px;border-radius:14px;background:%s;color:%s;font-weight:700;font-size:.85rem;border:1px solid rgba(0,0,0,.15)'>%s</span>", b$bg, b$fg, b$code) }
    rng_badges <- if (identical(rng$worst, rng$best)) badge(rng$worst)
                  else paste0(badge(rng$worst),
                              " <span style='color:#7a857b'>to</span> ", badge(rng$best))
    row <- function(t, v) sprintf(
      "<tr><td style='padding:3px 10px 3px 0;color:#5a655c'>%s</td><td style='padding:3px 0;font-family:monospace'>%s</td></tr>", t, v)
    htmltools::HTML(sprintf(
      "<div style='display:flex;align-items:center;gap:14px;flex-wrap:wrap'><div style='font-size:.9rem'>Plausible screening range (size only):</div><div style='display:flex;align-items:center;gap:8px'>%s</div></div><table style='margin-top:10px;font-size:.85rem;border-collapse:collapse'>%s%s%s%s%s</table><div style='font-size:.78rem;color:#7a857b;margin-top:8px'>Most-threatened = B1 with the lower AOO; least-threatened = B1 with the upper AOO. Sub-criteria (a/b/c) apply on top.</div>",
      rng_badges,
      row("B1 (EOO)", sprintf("%s km2 &rarr; %s", fkm(obj$eoo$area_km2), sc(rng$b1_category))),
      row("AOO lower &rarr; upper", sprintf("%s &rarr; %s km2", fkm(ab$aoo_lower), fkm(ab$aoo_upper))),
      row("B2 lower (occurrence)", sc(rng$b2_lower)),
      row("B2 upper (AOH)", sc(rng$b2_upper)),
      row("Area of Habitat", paste0(fkm(ab$aoh), " km2"))))
  })

  # Occurrences read from the uploaded file (NULL when no file is uploaded, so
  # the app can run from hand-added points alone). Read errors propagate to the
  # tryCatch around result().
  occ_file <- reactive({
    if (is.null(input$file)) return(NULL)
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

  # The occurrences everything downstream uses: the uploaded table plus every
  # hand-added point (or only the hand-added points when no file is uploaded).
  occ <- reactive({
    combined <- .combine_occ(occ_file(), manual_store())
    validate(need(
      !is.null(combined) && nrow(combined) > 0,
      "Upload an occurrence file and/or add points on the 'Add points' tab."))
    combined
  })

  # --- Add points tab: interactive occurrence editing --------------------
  # Keep the species picker populated with names already present (uploaded or
  # added), defaulting to the first one; new names can still be typed in.
  observe({
    fo <- tryCatch(occ_file(), error = function(e) NULL)
    sp_file <- if (!is.null(fo) && "species" %in% names(fo)) unique(fo$species) else character(0)
    sp_man  <- unique(manual_store()$species)
    sp <- unique(c(sp_file, sp_man))
    sp <- sp[!is.na(sp) & nzchar(sp)]
    if (!length(sp)) sp <- "sp1"
    cur <- isolate(input$edit_species)
    sel <- if (!is.null(cur) && nzchar(cur)) cur else sp[1]
    updateSelectizeInput(session, "edit_species", choices = sp,
                         selected = sel, server = FALSE)
  })

  .add_point <- function(lon, lat) {
    lon <- suppressWarnings(as.numeric(lon))
    lat <- suppressWarnings(as.numeric(lat))
    if (!is.finite(lon) || !is.finite(lat) ||
        lon < -180 || lon > 180 || lat < -90 || lat > 90) {
      showNotification("Invalid coordinates (need lon in [-180,180], lat in [-90,90]).",
                       type = "warning"); return(invisible())
    }
    sp <- trimws(input$edit_species %||% "")
    if (!nzchar(sp)) sp <- "sp1"
    df <- manual_store()
    manual_store(rbind(df, data.frame(species = sp, lon = lon, lat = lat,
                                      stringsAsFactors = FALSE)))
  }

  observeEvent(input$edit_map_click, {
    cl <- input$edit_map_click
    .add_point(cl$lng, cl$lat)
  })

  # A short description of the coordinate type currently selected, so the user
  # knows exactly what to type in each box.
  output$edit_coord_help <- renderUI({
    txt <- switch(
      input$edit_coord_fmt %||% "dd",
      dd  = paste("Decimal degrees: a single signed number per axis.",
                  "West and South are negative,",
                  "e.g. longitude -47.9292, latitude -15.7801."),
      dms = paste("Degrees, minutes and seconds: type the three numbers and the",
                  "hemisphere letter, e.g. latitude '15 46 48 S',",
                  "longitude '47 55 45 W'. A leading minus works too."),
      utm = paste("UTM: projected easting (X) and northing (Y) in metres, plus",
                  "the zone number (1-60) and hemisphere. Brazil spans zones",
                  "18-25 South. They are converted to decimal degrees (WGS84)."),
      "")
    helpText(txt)
  })

  observeEvent(input$edit_add_xy, {
    fmt <- input$edit_coord_fmt %||% "dd"
    if (fmt == "dd") {
      lon <- input$edit_lon; lat <- input$edit_lat
    } else if (fmt == "dms") {
      lon <- .parse_dms(input$edit_lon_dms)
      lat <- .parse_dms(input$edit_lat_dms)
    } else {
      ll  <- .utm_to_lonlat(input$edit_utm_easting, input$edit_utm_northing,
                            input$edit_utm_zone, input$edit_utm_hemi)
      lon <- ll[1]; lat <- ll[2]
    }
    .add_point(lon, lat)
    updateNumericInput(session, "edit_lon", value = NA)
    updateNumericInput(session, "edit_lat", value = NA)
    updateTextInput(session, "edit_lon_dms", value = "")
    updateTextInput(session, "edit_lat_dms", value = "")
    updateNumericInput(session, "edit_utm_easting", value = NA)
    updateNumericInput(session, "edit_utm_northing", value = NA)
  })

  observeEvent(input$edit_undo, {
    df <- manual_store()
    if (nrow(df)) manual_store(df[-nrow(df), , drop = FALSE])
  })

  observeEvent(input$edit_clear, {
    manual_store(data.frame(species = character(0), lon = numeric(0),
                            lat = numeric(0), stringsAsFactors = FALSE))
  })

  output$edit_count <- renderUI({
    n_man <- nrow(manual_store())
    fo <- tryCatch(occ_file(), error = function(e) NULL)
    n_file <- if (is.null(fo)) 0L else nrow(fo)
    htmltools::HTML(sprintf(
      "<div style='font-size:.85rem'>Uploaded points: <b>%d</b> &middot; Added by hand: <b>%d</b></div>",
      n_file, n_man))
  })

  # Base map for editing. Uploaded points are drawn once here; the hand-added
  # points are refreshed through a proxy so a click gives instant feedback.
  output$edit_map <- leaflet::renderLeaflet({
    # Key-free basemaps: the OpenStreetMap volunteer tile servers now block
    # embedded use ("Access blocked", HTTP 403), so use Esri tiles (no API key).
    leaflet::leaflet() |>
      leaflet::addProviderTiles("Esri.WorldStreetMap", group = "Light") |>
      leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
      leaflet::addLayersControl(
        baseGroups = c("Light", "Satellite"),
        options = leaflet::layersControlOptions(collapsed = TRUE)) |>
      leaflet::setView(lng = -55, lat = -12, zoom = 4)
  })
  outputOptions(output, "edit_map", suspendWhenHidden = FALSE)

  # Uploaded points, filtered to the selected species (grey, for reference).
  # Redrawn when the species selection or the uploaded file changes; the view is
  # fitted to that species so its existing points come into frame. Adding points
  # by hand does not re-run this, so the map is not reset on every click.
  observe({
    sp    <- input$edit_species
    proxy <- leaflet::leafletProxy("edit_map")
    leaflet::clearGroup(proxy, "uploaded")
    fo <- tryCatch(occ_file(), error = function(e) NULL)
    if (is.null(fo) || !nrow(fo) || is.null(sp) || !nzchar(sp)) return()
    keep <- !is.na(fo$species) & fo$species == sp
    if (!any(keep)) return()
    xy <- sf::st_coordinates(fo[keep, ])
    leaflet::addCircleMarkers(
      proxy, lng = xy[, 1], lat = xy[, 2], radius = 4, color = "#666",
      stroke = FALSE, fillOpacity = 0.6, group = "uploaded",
      label = as.character(fo$species[keep]))
    if (nrow(xy) >= 1)
      leaflet::fitBounds(proxy, min(xy[, 1]), min(xy[, 2]),
                         max(xy[, 1]), max(xy[, 2]))
  })

  # Hand-added points, filtered to the selected species (green). Refreshed on
  # every add/undo/clear and on species change, without moving the view.
  observe({
    sp    <- input$edit_species
    df    <- manual_store()
    proxy <- leaflet::leafletProxy("edit_map")
    leaflet::clearGroup(proxy, "added")
    if (!is.null(sp) && nzchar(sp))
      df <- df[!is.na(df$species) & df$species == sp, , drop = FALSE]
    if (nrow(df)) {
      leaflet::addCircleMarkers(
        proxy, lng = df$lon, lat = df$lat, radius = 6, color = "#1f8d49",
        stroke = TRUE, weight = 1, fillOpacity = 0.85, group = "added",
        label = sprintf("%s (%.4f, %.4f)", df$species, df$lat, df$lon))
    }
  })

  output$edit_tbl <- DT::renderDT({
    df <- manual_store()
    validate(need(nrow(df) > 0, "No points added yet. Click the map to add some."))
    DT::datatable(df, rownames = FALSE, class = "compact stripe hover",
                  options = list(scrollX = TRUE, pageLength = 8, dom = "tp"))
  })

  output$dl_manual_pts <- downloadHandler(
    filename = function() paste0("mappingAS_added_points_", Sys.Date(), ".csv"),
    content = .safe_download(function(file) {
      df <- manual_store(); req(nrow(df) > 0)
      utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  )

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
          loc_km = if (is.null(input$loc_km) || is.na(input$loc_km)) 10 else input$loc_km,
          loc_method = input$loc_method %||% "no_more_than_one",
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
    updateSelectInput(session, "coo_species", choices = sp, selected = sp[1])
    updateSelectInput(session, "aoh_species", choices = sp, selected = sp[1])
    updateSelectInput(session, "frag_species", choices = sp, selected = sp[1])
    updateSelectInput(session, "report_species", choices = sp, selected = sp[1])
  })

  output$tbl <- DT::renderDT({
    req(result())
    .mas_dt(summary_applied(), page = 25,
            caption = "EOO, AOO, conversion and applied Criterion B by species")
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
    if (has("n_subpop") || has("n_locations")) {
      fint <- function(x) if (is.na(x)) "&mdash;" else formatC(x, format = "d", big.mark = ",")
      loc_extra <- if (isTRUE(st$protected))
        " <span style='color:#7a857b'>(PA-decoupled)</span>" else ""
      cards <- c(cards, card(paste0(
        lab("Subpopulations / Locations (est.)"),
        sprintf("<div style='font-size:.82rem'>Subpopulations %s</div>", fint(v("n_subpop"))),
        sprintf("<div style='font-size:.82rem;margin-top:4px'>Locations %s%s</div>",
                fint(v("n_locations")), loc_extra))))
    }
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
      c("n_subpop", L("Estimated number of subpopulations (circular-buffer method).", "Numero estimado de subpopulacoes (metodo de buffer circular).")),
      c("n_locations", L("Estimated number of locations (occupied-grid-cell method; decoupled by protected areas when the Protected-areas option is on).", "Numero estimado de localidades (metodo de celulas de grade ocupadas; desacoplado por areas protegidas quando a opcao de Areas protegidas esta ativa).")),
      c("eoo_converted_pct", L("% of the terrestrial EOO that is converted (anthropic).", "% da EOO terrestre convertida (antropica).")),
      c("eoo_natural_pct", L("% of the terrestrial EOO that is remaining natural habitat.", "% da EOO terrestre de habitat natural remanescente.")),
      c("aoo_converted_pct", L("% of the terrestrial AOO that is converted (anthropic).", "% da AOO terrestre convertida (antropica).")),
      c("aoo_natural_pct", L("% of the terrestrial AOO that is remaining natural habitat.", "% da AOO terrestre de habitat natural remanescente.")),
      c("eoo_cat_B1", L("Provisional category by EOO size (sub-criterion B1).", "Categoria provisoria pelo tamanho da EOO (subcriterio B1).")),
      c("aoo_cat_B2", L("Provisional category by AOO size (sub-criterion B2).", "Categoria provisoria pelo tamanho da AOO (subcriterio B2).")),
      c("provisional_cat", L("Combined provisional category (the more threatened of B1/B2). Screening only.", "Categoria provisoria combinada (a mais ameacada entre B1/B2). Apenas triagem.")),
      c("category_B", L("Applied Criterion B category (CR/EN/VU need size AND >=2 sub-criteria; NT if size met but not; LC otherwise). DD/NE are never automatic.", "Categoria aplicada do Criterio B (CR/EN/VU exigem tamanho E >=2 subcriterios; NT se so o tamanho; senao LC). DD/NE nunca automaticos.")),
      c("criterion_B_code", L("Full Criterion B code, e.g. 'VU B1ab'.", "Codigo completo do Criterio B, ex.: 'VU B1ab'.")),
      c("subcrit_a", L("Sub-criterion (a): severely fragmented or few locations; the number of locations caps the category.", "Subcriterio (a): severamente fragmentada ou poucas localidades; o numero de localidades limita a categoria.")),
      c("subcrit_b", L("Sub-criterion (b): continuing decline. Assumed present by default (ConR-style, supported by the conversion data); uncheck to require documentation.", "Subcriterio (b): declinio continuo. Assumido presente por padrao (estilo ConR, apoiado pelos dados de conversao); desmarque para exigir documentacao.")),
      c("subcrit_c", L("Sub-criterion (c): extreme fluctuations (expert input).", "Subcriterio (c): flutuacoes extremas (entrada do especialista).")),
      c("decline_assumed", L("TRUE when sub-criterion (b) was assumed present rather than documented.", "TRUE quando o subcriterio (b) foi assumido presente em vez de documentado.")),
      c("decline_basis", L("How sub-criterion (b) was set: 'documented', 'inferred (habitat loss)' (from measured conversion, per IUCN b(iii) 'rate of habitat loss'), 'assumed' (ConR fallback) or 'not documented'.", "Como o subcriterio (b) foi definido: 'documented', 'inferred (habitat loss)' (da conversao medida, conforme b(iii) 'taxa de perda de habitat' da IUCN), 'assumed' (fallback ConR) ou 'not documented'.")),
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
      utils::write.csv(summary_applied(), file, row.names = FALSE, fileEncoding = "UTF-8")
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

  # Save one trend plot (EOO or AOO) to PNG. Renders the same ggplot shown on
  # screen; a base-graphics fallback covers the (rare) no-ggplot2 case.
  .dl_trend_png <- function(which) .safe_download(function(file) {
    d <- ts_data(); ts <- if (which == "eoo") d$eoo else d$aoo
    req(!is.null(ts))
    p <- .ts_trend_plot(ts)
    if (inherits(p, "ggplot") && requireNamespace("ggplot2", quietly = TRUE)) {
      ggplot2::ggsave(file, plot = p, width = 7.5, height = 5, dpi = 150, bg = "white")
    } else {
      grDevices::png(file, width = 1100, height = 720, res = 150)
      on.exit(grDevices::dev.off(), add = TRUE)
      print(p)
    }
  })
  output$dl_ts_trend_eoo_png <- downloadHandler(
    filename = function() paste0("mappingAS_trend_EOO_", input$ts_species, "_", Sys.Date(), ".png"),
    content = .dl_trend_png("eoo"))
  output$dl_ts_trend_aoo_png <- downloadHandler(
    filename = function() paste0("mappingAS_trend_AOO_", input$ts_species, "_", Sys.Date(), ".png"),
    content = .dl_trend_png("aoo"))

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

  # Fire series for BOTH extents (mirrors the Time series tab). Each calculated
  # series is tagged (species / range) so it also feeds the Report tab.
  fire_ts_data <- eventReactive(input$fire_ts_run, {
    req(result(), input$fire_species)
    yrs <- .year_grid(input$fire_ts_step)
    one <- function(rng) {
      tryCatch(
        mappingAS::fire_timeseries_for_species(
          result(), species = input$fire_species, range = rng,
          years = yrs, verbose = FALSE),
        error = function(e) {
          showNotification(sprintf("Error in %s fire series: %s",
                                   toupper(rng), conditionMessage(e)),
                           type = "error", duration = NULL)
          NULL
        })
    }
    withProgress(message = "Calculating fire series (EOO & AOO)...", value = 0, {
      eoo <- one("eoo"); incProgress(0.4)
      invisible(gc(FALSE)); incProgress(0.1)
      aoo <- one("aoo"); incProgress(0.4)
      invisible(gc(FALSE)); incProgress(0.1)
      list(eoo = eoo, aoo = aoo)
    })
  })

  output$fire_ts_plot_eoo <- plotly::renderPlotly({
    d <- fire_ts_data(); req(!is.null(d$eoo))
    mappingAS::mas_plotly(
      mappingAS::plot_fire_timeseries(d$eoo, lang = input$lang %||% "en"))
  })

  output$fire_ts_plot_aoo <- plotly::renderPlotly({
    d <- fire_ts_data(); req(!is.null(d$aoo))
    mappingAS::mas_plotly(
      mappingAS::plot_fire_timeseries(d$aoo, lang = input$lang %||% "en"))
  })

  # Combine both extents (per year) into one table / CSV, tagged with `range`.
  .fire_ts_combined <- function(d) {
    mk <- function(ts, rng) {
      if (is.null(ts) || !is.data.frame(ts)) return(NULL)
      cbind(range = rng, ts, stringsAsFactors = FALSE)
    }
    rbind(mk(d$eoo, "EOO"), mk(d$aoo, "AOO"))
  }

  output$fire_tbl <- DT::renderDT({
    d <- fire_ts_data(); req(!is.null(d$eoo) || !is.null(d$aoo))
    df <- .fire_ts_combined(d)
    validate(need(!is.null(df) && nrow(df) > 0, "No fire series data."))
    .mas_dt(df, page = 15, caption = "Burned area by year — EOO and AOO")
  })

  output$dl_fire_ts <- downloadHandler(
    filename = function() paste0("mappingAS_fire_", input$fire_species, "_", Sys.Date(), ".csv"),
    content = .safe_download(function(file) {
      d <- fire_ts_data(); req(!is.null(d$eoo) || !is.null(d$aoo))
      df <- .fire_ts_combined(d); req(!is.null(df) && nrow(df) > 0)
      utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  )

  # Stack the EOO and AOO fire charts into one PNG (mirrors the Time series PNG).
  output$dl_fire_ts_png <- downloadHandler(
    filename = function() paste0("mappingAS_fire_", input$fire_species, "_", Sys.Date(), ".png"),
    content = .safe_download(function(file) {
      d <- fire_ts_data(); req(!is.null(d$eoo) || !is.null(d$aoo))
      lg <- input$lang %||% "en"
      ps <- Filter(Negate(is.null), list(
        if (!is.null(d$eoo)) mappingAS::plot_fire_timeseries(d$eoo, lang = lg),
        if (!is.null(d$aoo)) mappingAS::plot_fire_timeseries(d$aoo, lang = lg)))
      n <- length(ps); req(n > 0)
      grDevices::png(file, width = 1200, height = 420 * max(n, 1), res = 120)
      on.exit(grDevices::dev.off(), add = TRUE)
      if (all(vapply(ps, inherits, logical(1), "ggplot"))) {
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, 1)))
        for (i in seq_len(n)) {
          print(ps[[i]], vp = grid::viewport(layout.pos.row = i,
                                             layout.pos.col = 1))
        }
      } else {
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar), add = TRUE)
        graphics::par(mfrow = c(n, 1))
        if (!is.null(d$eoo)) mappingAS::plot_fire_timeseries(d$eoo, lang = lg)
        if (!is.null(d$aoo)) mappingAS::plot_fire_timeseries(d$aoo, lang = lg)
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
        # base-graphics fallback: stack the plots. Reset par() afterwards so the
        # user's graphics state is left untouched (CRAN policy).
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar), add = TRUE)
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
    d <- fire_ts_data()
    if (is.null(d)) return()
    st <- report_fire_store()
    for (ts in list(d$eoo, d$aoo)) {
      if (is.null(ts) || !is.data.frame(ts)) next
      key <- paste(attr(ts, "species") %||% "", attr(ts, "range") %||% "", sep = "||")
      st[[key]] <- ts
    }
    report_fire_store(st)
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
    ab <- .applied_B(sp)
    htmltools::HTML(
      mappingAS::assessment_report(
        result_applied(), species = sp, lang = input$lang %||% "en", output = "html",
        cover_series = .report_cover(sp), fire_series = .report_fire(sp),
        applied_category = ab$category, applied_code = ab$code))
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
      ab <- .applied_B(sp)
      mappingAS::assessment_report(
        result_applied(), species = sp, lang = input$lang %||% "en", output = "docx",
        file = file, figures = TRUE,
        cover_series = .report_cover(sp), fire_series = .report_fire(sp),
        applied_category = ab$category, applied_code = ab$code)
    })
  )

  # --- Vouchers auto-fill from the uploaded table -------------------------
  # Derive the examined-material list for the selected species from the file's
  # attribute columns (voucher, or collector + collectorNumber [+ herbarium]).
  derived_vouchers <- reactive({
    fo <- tryCatch(occ_file(), error = function(e) NULL)
    if (is.null(fo)) return(character(0))
    tryCatch(
      mappingAS::vouchers_from_occ(fo, species = input$report_species),
      error = function(e) character(0))
  })

  # Prefill the vouchers box automatically, but never clobber the user's own
  # edits: refill only while the box is empty or still holds the last value we
  # inserted (so switching species updates it, manual typing freezes it).
  last_auto_vouchers <- reactiveVal("")
  observeEvent(list(input$report_species, occ_file()), {
    dv  <- paste(derived_vouchers(), collapse = "\n")
    cur <- trimws(input$fs_vouchers %||% "")
    if (!nzchar(cur) || identical(cur, trimws(last_auto_vouchers()))) {
      updateTextAreaInput(session, "fs_vouchers", value = dv)
      last_auto_vouchers(dv)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$fs_vouchers_load, {
    dv <- derived_vouchers()
    if (!length(dv)) {
      showNotification(
        "No voucher / collector columns found in the uploaded table.",
        type = "warning")
      return()
    }
    val <- paste(dv, collapse = "\n")
    updateTextAreaInput(session, "fs_vouchers", value = val)
    last_auto_vouchers(val)
  })

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
    ab <- .applied_B(sp)
    list(
      assessment = result_applied(), species = sp, lang = input$lang %||% "en",
      family = nz("fs_family"), genus = nz("fs_genus"),
      authority = nz("fs_authority"),
      countries = nz("fs_countries"),
      life_form = nz("fs_life_form"), substrate = nz("fs_substrate"),
      habitat = nz("fs_habitat"), biome = nz("fs_biome"),
      vegetation = nz("fs_vegetation"),
      land_use = nz("fs_land_use"),
      conservation_units = nz("fs_cons_units"),
      vouchers = nz("fs_vouchers"), reference = nz("fs_reference"),
      photos = .fs_photo_paths(), photo_credit = nz("fs_photo_credit"),
      cover_series = .report_cover(sp), fire_series = .report_fire(sp),
      applied_category = ab$category, applied_code = ab$code,
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