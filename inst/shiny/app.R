## mappingAS Shiny app
## Launch with: mappingAS::run_app()

suppressMessages({
  library(shiny)
  library(bslib)
  library(leaflet)
  library(DT)
  if (requireNamespace("mappingAS", quietly = TRUE)) library(mappingAS)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

ui <- bslib::page_sidebar(
  title = "mappingAS | Mapping Area of Species",
  fillable = TRUE, # Allows the map to stretch and fill the entire screen
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#1f8d49"),
  sidebar = bslib::sidebar(
    width = 360,
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
    selectInput("collection", "Collection", choices = c(10, 9), selected = 10),
    numericInput("cell_km", "AOO Cell (km)", value = 2, min = 0.5, step = 0.5),
    radioButtons("backend", "MapBiomas Source",
                 choices = c("Local (no GEE account)" = "local",
                             "Google Earth Engine" = "gee"),
                 selected = "local"),
    checkboxInput("water_denom", "Include water as natural in denominator", FALSE),
    checkboxInput("do_mb", "Calculate MapBiomas conversion", TRUE),
    checkboxInput("do_fire", "Calculate fire (MapBiomas burned area)", FALSE),
    actionButton("run", "Assess", class = "btn-primary w-100", icon = icon("calculator")),
    hr(),
    downloadButton("dl_csv", "Download results (CSV)", class = "w-100"),
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
          leaflet::leafletOutput("map", height = "100%") # Stretched map
        ),
        bslib::card(
          selectInput("map_species", "Species", choices = NULL),
          radioButtons("static_layer", "Publishable map layer (PNG)",
                       choices = c("Land use"  = "lulc",
                                   "Fire"      = "fire",
                                   "Both"      = "both"),
                       selected = "lulc"),
          hr(),
          downloadButton("dl_map_html", "Download map (HTML)", class = "mb-2"),
          downloadButton("dl_map_png", "Download map (PNG)", class = "mb-2"),
          downloadButton("dl_map_static", "Publishable map (PNG)", class = "mb-2")
        )
      )
    ),
    bslib::nav_panel(
      "Results", icon = icon("table"),
      DT::DTOutput("tbl")
    ),
    bslib::nav_panel(
      "Conversion", icon = icon("chart-pie"),
      selectInput("chart_species", "Species", choices = NULL),
      downloadButton("dl_chart_png", "Save image (PNG)", class = "mb-2"),
      plotOutput("chart", height = 460)
    ),
    bslib::nav_panel(
      "Classes", icon = icon("list"),
      selectInput("class_species", "Species", choices = NULL),
      helpText("Area and % of each MapBiomas class within EOO and AOO."),
      downloadButton("dl_classes", "Download classes (CSV)", class = "mb-3"),
      DT::DTOutput("class_tbl")
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
      plotOutput("ts_plot", height = 480),
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
      plotOutput("fire_ts_plot", height = 420),
      DT::DTOutput("fire_tbl")
    ),

    bslib::nav_panel(
      "Methods", icon = icon("info-circle"),
      htmltools::HTML(
        "<div style='max-width:760px'>
        <h4>What this tool calculates</h4>
        <ul>
          <li><b>EOO</b> (Extent of Occurrence): area of the minimum convex polygon
              encompassing all points, measured in an equal-area projection.</li>
          <li><b>AOO</b> (Area of Occupancy): number of occupied 2x2 km cells
              x 4 km<sup>2</sup> (IUCN reference scale).</li>
          <li><b>% converted</b> = anthropic / (anthropic + natural) within the
              EOO and AOO, based on MapBiomas classes. <b>% natural</b>
              (current) is the complement. Water and unobserved areas are excluded
              from the denominator by default.</li>
        </ul>
        <h4>Provisional categories</h4>
        <p>The category columns reflect only the <i>size</i> thresholds
        of Criterion B (B1 for EOO, B2 for AOO), similar to GeoCAT. A final assessment
        also requires the subcriteria (fragmentation/locations, continuing decline,
        extreme fluctuations) and should not be inferred solely from size.</p>
        <h4>Source</h4>
        <p>MapBiomas Brazil, Collection 10 (1985-2024). The local backend reads a window
        from the national GeoTIFF via <code>/vsicurl/</code>; the GEE backend calculates
        the area by class on the server (requires <code>rgee::ee_Initialize()</code>).</p>
        <p><i>Very large ranges:</i> prefer the GEE backend.</p>
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
          collection = as.integer(input$collection),
          backend = input$backend,
          cell_km = input$cell_km,
          mapbiomas = isTRUE(input$do_mb),
          fire = isTRUE(input$do_fire),
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
  })

  output$tbl <- DT::renderDT({
    req(result())
    DT::datatable(result()$summary, rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 25),
                  caption = "EOO, AOO and conversion by species")
  })
  
  output$map <- leaflet::renderLeaflet({
    req(result(), input$map_species)
    mappingAS::map_species(result(), species = input$map_species,
                           mapbiomas = isTRUE(input$do_mb),
                           fire = isTRUE(input$do_fire))
  })

  outputOptions(output, "map", suspendWhenHidden = FALSE)

  output$chart <- renderPlot({
    req(result(), input$chart_species)
    mappingAS::plot_conversion(result(), species = input$chart_species)
  })

  output$dl_csv <- downloadHandler(
    filename = function() paste0("mappingAS_results_", Sys.Date(), ".csv"),
    content = function(file) {
      req(result())
      utils::write.csv(result()$summary, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$dl_ranges <- downloadHandler(
    filename = function() {
      ext <- if (identical(input$export_fmt, "gpkg")) "gpkg" else "zip"
      paste0("mappingAS_EOO_AOO_", Sys.Date(), ".", ext)
    },
    content = function(file) {
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
    }
  )

  output$class_tbl <- DT::renderDT({
    req(result(), input$class_species)
    df <- tryCatch(
      mappingAS::class_table(result(), species = input$class_species, range = "both"),
      error = function(e) data.frame())
    validate(need(nrow(df) > 0,
                  "No class data available. Enable 'Calculate MapBiomas conversion' and reassess."))
    DT::datatable(df, rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 25),
                  caption = "Area and % by MapBiomas class (EOO and AOO)")
  })

  output$dl_classes <- downloadHandler(
    filename = function() paste0("mappingAS_classes_", Sys.Date(), ".csv"),
    content = function(file) {
      req(result())
      df <- tryCatch(mappingAS::class_table(result(), range = "both"),
                     error = function(e) data.frame())
      utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  ts_data <- eventReactive(input$ts_run, {
    req(result(), input$ts_species)
    coll <- as.integer(input$collection)
    yy <- mappingAS::mb_years(coll)
    step <- max(1L, as.integer(input$ts_step))
    yrs <- sort(unique(c(seq(min(yy), max(yy), by = step), max(yy))))
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

  output$ts_plot <- renderPlot({
    ts <- ts_data(); req(ts)
    mappingAS::plot_timeseries(ts)
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
    DT::datatable(ts, rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 15),
                  caption = "Composition (%) by year")
  })

  output$dl_ts <- downloadHandler(
    filename = function() paste0("mappingAS_series_", input$ts_species, "_", Sys.Date(), ".csv"),
    content = function(file) {
      ts <- ts_data(); req(ts)
      utils::write.csv(ts, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
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
    DT::datatable(df, rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 10),
                  caption = "Burned area by extent (EOO and AOO)")
  })

  fire_ts_data <- eventReactive(input$fire_ts_run, {
    req(result(), input$fire_species)
    yy <- mappingAS::mb_years(as.integer(input$collection))
    step <- max(1L, as.integer(input$fire_ts_step))
    yrs <- sort(unique(c(seq(min(yy), max(yy), by = step), max(yy))))
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

  output$fire_ts_plot <- renderPlot({
    ts <- fire_ts_data(); req(ts)
    mappingAS::plot_fire_timeseries(ts)
  })

  output$dl_fire_ts <- downloadHandler(
    filename = function() paste0("mappingAS_fire_", input$fire_species, "_", Sys.Date(), ".csv"),
    content = function(file) {
      ts <- fire_ts_data(); req(ts)
      utils::write.csv(ts, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$dl_fire_ts_png <- downloadHandler(
    filename = function() paste0("mappingAS_fire_", input$fire_species, "_", Sys.Date(), ".png"),
    content = function(file) {
      ts <- fire_ts_data(); req(ts)
      p <- mappingAS::plot_fire_timeseries(ts)
      if (inherits(p, "ggplot") && requireNamespace("ggplot2", quietly = TRUE)) {
        ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 130)
      } else {
        grDevices::png(file, width = 1200, height = 720, res = 120)
        on.exit(grDevices::dev.off(), add = TRUE)
        mappingAS::plot_fire_timeseries(ts)
      }
    }
  )

  output$dl_map_html <- downloadHandler(
    filename = function() paste0("mappingAS_map_", input$map_species, "_", Sys.Date(), ".html"),
    content = function(file) {
      req(result(), input$map_species)
      m <- mappingAS::map_species(result(), species = input$map_species)
      htmlwidgets::saveWidget(m, file, selfcontained = TRUE)
    }
  )

  output$dl_map_png <- downloadHandler(
    filename = function() paste0("mappingAS_map_", input$map_species, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(result(), input$map_species)
      if (!requireNamespace("webshot2", quietly = TRUE)) {
        showNotification(
          paste("To save the map as a PNG, install the 'webshot2' package",
                "(requires Chrome/Chromium). Meanwhile, use the 'Download map (HTML)' button."),
          type = "warning", duration = NULL)
        req(FALSE)
      }
      m <- mappingAS::map_species(result(), species = input$map_species)
      tmp <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(m, tmp, selfcontained = TRUE)
      webshot2::webshot(tmp, file = file, vwidth = 1100, vheight = 800, delay = 1)
    }
  )

  output$dl_map_static <- downloadHandler(
    filename = function() paste0("mappingAS_mapa_", input$map_species, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(result(), input$map_species)
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        showNotification("Pacote 'ggplot2' necessário para o mapa publicável.",
                         type = "error", duration = NULL)
        req(FALSE)
      }
      lyr <- input$static_layer %||% "lulc"
      if (lyr == "both" && !requireNamespace("ggnewscale", quietly = TRUE)) {
        showNotification(
          "Para sobrepor uso do solo e fogo, instale 'ggnewscale'. Exportando só o uso do solo.",
          type = "warning", duration = 8)
      }
      m <- withProgress(message = "Gerando mapa publicável...", value = 0, {
        out <- tryCatch(
          mappingAS::map_static(
            result(), species = input$map_species,
            mapbiomas = lyr %in% c("lulc", "both"),
            fire      = lyr %in% c("fire", "both")),
          error = function(e) {
            showNotification(paste("Erro ao gerar o mapa publicável:",
                                   conditionMessage(e)),
                             type = "error", duration = NULL)
            NULL
          })
        incProgress(1)
        out
      })
      req(inherits(m, "ggplot"))
      ggplot2::ggsave(file, plot = m, width = 9, height = 8, dpi = 300)
    }
  )

  output$dl_chart_png <- downloadHandler(
    filename = function() paste0("mappingAS_conversion_", input$chart_species, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(result(), input$chart_species)
      grDevices::png(file, width = 1100, height = 750, res = 130)
      on.exit(grDevices::dev.off(), add = TRUE)
      mappingAS::plot_conversion(result(), species = input$chart_species)
    }
  )

  output$dl_ts_png <- downloadHandler(
    filename = function() paste0("mappingAS_series_", input$ts_species, "_", Sys.Date(), ".png"),
    content = function(file) {
      ts <- ts_data(); req(ts)
      p <- mappingAS::plot_timeseries(ts)
      if (inherits(p, "ggplot") && requireNamespace("ggplot2", quietly = TRUE)) {
        ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 130)
      } else {
        grDevices::png(file, width = 1200, height = 720, res = 120)
        on.exit(grDevices::dev.off(), add = TRUE)
        mappingAS::plot_timeseries(ts)
      }
    }
  )
}

shinyApp(ui, server)