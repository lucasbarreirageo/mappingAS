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
  title = "mappingAS — EOO/AOO & conversão de habitat (MapBiomas)",
  theme = bslib::bs_theme(
    version = 5,
    primary = "#1f8d49", secondary = "#0e5c4a", success = "#1f8d49",
    info = "#2a7f8c", warning = "#c8651b", danger = "#d4271e",
    "border-radius" = "0.65rem"
  ),
  sidebar = bslib::sidebar(
    width = 360,
    fileInput(
      "file", "Ocorrências (CSV, XLSX, GeoPackage, GeoJSON ou shapefile .zip)",
      accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls",
                 ".gpkg", ".geojson", ".json", ".zip")
    ),
    helpText("Dica: para shapefile, envie um .zip com .shp/.shx/.dbf/.prj."),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Mapear colunas (opcional)",
        textInput("species_col", "Coluna de espécie", ""),
        textInput("lon_col", "Coluna de longitude", ""),
        textInput("lat_col", "Coluna de latitude", ""),
        helpText("Deixe em branco para detecção automática.")
      )
    ),
    hr(),
    selectInput("year", "Ano MapBiomas", choices = 2024:1985, selected = 2024),
    selectInput("collection", "Coleção", choices = c(10, 9), selected = 10),
    numericInput("cell_km", "Célula AOO (km)", value = 2, min = 0.5, step = 0.5),
    radioButtons("backend", "Fonte do MapBiomas",
                 choices = c("Local (sem conta GEE)" = "local",
                             "Google Earth Engine" = "gee"),
                 selected = "local"),
    checkboxInput("water_denom", "Incluir água como natural no denominador", FALSE),
    checkboxInput("do_mb", "Calcular conversão MapBiomas", TRUE),
    actionButton("run", "Avaliar", class = "btn-primary w-100"),
    hr(),
    downloadButton("dl_csv", "Baixar resultados (CSV)", class = "w-100"),
    hr(),
    radioButtons("export_fmt", "Mapas EOO/AOO",
                 choices = c("Shapefile (.zip)" = "shapefile",
                             "GeoPackage (.gpkg)" = "gpkg"),
                 selected = "shapefile", inline = TRUE),
    downloadButton("dl_ranges", "Baixar EOO/AOO (espacial)", class = "w-100")
  ),
  tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
  bslib::navset_card_tab(
    bslib::nav_panel(
      "Resultados",
      DT::DTOutput("tbl")
    ),
    bslib::nav_panel(
      "Mapa",
      selectInput("map_species", "Espécie", choices = NULL),
      div(
        class = "d-flex gap-2 mb-2",
        downloadButton("dl_map_html", "Baixar mapa (HTML)"),
        downloadButton("dl_map_png", "Baixar mapa (PNG)"),
        downloadButton("dl_map_static", "Mapa publicável (PNG)")
      ),
      leaflet::leafletOutput("map", height = 560)
    ),
    bslib::nav_panel(
      "Conversão",
      selectInput("chart_species", "Espécie", choices = NULL),
      downloadButton("dl_chart_png", "Salvar imagem (PNG)", class = "mb-2"),
      plotOutput("chart", height = 460)
    ),
    bslib::nav_panel(
      "Classes",
      selectInput("class_species", "Especie", choices = NULL),
      helpText("Area e % de cada classe do MapBiomas dentro do EOO e do AOO."),
      downloadButton("dl_classes", "Baixar classes (CSV)", class = "mb-3"),
      DT::DTOutput("class_tbl")
    ),
    bslib::nav_panel(
      "Serie temporal",
      fluidRow(
        column(3, selectInput("ts_species", "Especie", choices = NULL)),
        column(3, radioButtons("ts_range", "Area",
                               c("EOO" = "eoo", "AOO" = "aoo"),
                               selected = "eoo", inline = TRUE)),
        column(3, radioButtons("ts_by", "Detalhe",
                               c("Classe" = "class", "Grupo" = "group"),
                               selected = "class", inline = TRUE)),
        column(3, numericInput("ts_step", "Passo (anos)", value = 1,
                               min = 1, max = 10, step = 1))
      ),
      div(
        class = "d-flex gap-2 mb-2",
        actionButton("ts_run", "Calcular serie", class = "btn-primary"),
        downloadButton("dl_ts", "Baixar serie (CSV)"),
        downloadButton("dl_ts_png", "Salvar imagem (PNG)")
      ),
      helpText("Histórico completo do MapBiomas (anual por padrão). Passo de 1 ano lê todos os anos e pode demorar; aumente o passo para acelerar."),
      uiOutput("ts_summary"),
      plotOutput("ts_plot", height = 480),
      DT::DTOutput("ts_tbl")
    ),
    bslib::nav_panel(
      "Métodos",
      htmltools::HTML(
        "<div style='max-width:760px'>
        <h4>O que esta ferramenta calcula</h4>
        <ul>
          <li><b>EOO</b> (Extensão de Ocorrência): area do poligono convexo minimo
              que envolve todos os pontos, medida em projecao de area igual.</li>
          <li><b>AOO</b> (Area de Ocupacao): numero de celulas de 2x2 km ocupadas
              x 4 km<sup>2</sup> (escala de referencia da IUCN).</li>
          <li><b>% convertida</b> = antropico / (antropico + natural) dentro do
              EOO e do AOO, a partir das classes do MapBiomas. <b>% natural</b>
              (atual) e o complemento. Agua e nao-observado ficam fora do
              denominador por padrao.</li>
        </ul>
        <h4>Categorias provisorias</h4>
        <p>As colunas de categoria refletem apenas os limiares de <i>tamanho</i>
        do Criterio B (B1 para EOO, B2 para AOO), como no GeoCat. Uma avaliacao
        final exige tambem os subcriterios (fragmentacao/locais, declinio
        continuo, flutuacao extrema) e nao deve ser inferida apenas do tamanho.</p>
        <h4>Fonte</h4>
        <p>MapBiomas Brasil, Colecao 10 (1985-2024). Backend local le uma janela
        do GeoTIFF nacional via <code>/vsicurl/</code>; o backend GEE calcula a
        area por classe no servidor (requer <code>rgee::ee_Initialize()</code>).</p>
        <p><i>Ranges muito grandes:</i> prefira o backend GEE.</p>
        </div>"
      )
    )
  )
)

server <- function(input, output, session) {

  occ <- reactive({
    req(input$file)
    # preserve original extension so the reader can detect file type
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
      showNotification(paste("Erro ao ler arquivo:", conditionMessage(e)),
                       type = "error", duration = NULL)
      NULL
    })
    req(o)

    if (input$backend == "gee" && !requireNamespace("rgee", quietly = TRUE)) {
      showNotification("rgee nao instalado; use o backend Local.",
                       type = "error", duration = NULL)
      return(NULL)
    }

    withProgress(message = "Avaliando especies...", value = 0, {
      sp <- unique(o$species); n <- length(sp)
      res <- tryCatch(
        mappingAS::assess_species(
          o,
          year = as.integer(input$year),
          collection = as.integer(input$collection),
          backend = input$backend,
          cell_km = input$cell_km,
          mapbiomas = isTRUE(input$do_mb),
          water_in_denominator = isTRUE(input$water_denom),
          verbose = FALSE
        ),
        error = function(e) {
          showNotification(paste("Erro na avaliacao:", conditionMessage(e)),
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
  })

  output$tbl <- DT::renderDT({
    req(result())
    DT::datatable(result()$summary, rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 25),
                  caption = "EOO, AOO e conversao por especie")
  })
  output$map <- leaflet::renderLeaflet({
    req(result(), input$map_species)
    mappingAS::map_species(result(), species = input$map_species,
                           mapbiomas = isTRUE(input$do_mb))
  })

  outputOptions(output, "map", suspendWhenHidden = FALSE)   #ALTERAÇÃO FEITA PARA VER SE RODA O MAPA

  output$chart <- renderPlot({
    req(result(), input$chart_species)
    mappingAS::plot_conversion(result(), species = input$chart_species)
  })

  output$dl_csv <- downloadHandler(
    filename = function() paste0("mappingAS_resultados_", Sys.Date(), ".csv"),
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
          showNotification(paste("Erro ao exportar:", conditionMessage(e)),
                           type = "error", duration = NULL)
          NULL
        }
      )
      req(out)
      file.copy(out, file, overwrite = TRUE)
    }
  )

  # ---- Classes (per-class MapBiomas breakdown) ----
  output$class_tbl <- DT::renderDT({
    req(result(), input$class_species)
    df <- tryCatch(
      mappingAS::class_table(result(), species = input$class_species, range = "both"),
      error = function(e) data.frame())
    validate(need(nrow(df) > 0,
                  "Sem dados por classe. Ative 'Calcular conversao MapBiomas' e reavalie."))
    DT::datatable(df, rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 25),
                  caption = "Area e % por classe MapBiomas (EOO e AOO)")
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

  # ---- Time series (cover % over years) ----
  ts_data <- eventReactive(input$ts_run, {
    req(result(), input$ts_species)
    coll <- as.integer(input$collection)
    yy <- mappingAS::mb_years(coll)
    step <- max(1L, as.integer(input$ts_step))
    yrs <- sort(unique(c(seq(min(yy), max(yy), by = step), max(yy))))
    withProgress(message = "Calculando serie temporal...", value = 0, {
      ts <- tryCatch(
        mappingAS::timeseries_for_species(
          result(), species = input$ts_species, range = input$ts_range,
          years = yrs, by = input$ts_by, verbose = FALSE),
        error = function(e) {
          showNotification(paste("Erro na serie temporal:", conditionMessage(e)),
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
        "<div style='padding:8px 12px;background:#f6f6f6;border-radius:6px;margin-bottom:8px'>Sem classes antrópicas neste recorte.</div>"))
    }
    ya <- stats::aggregate(pct ~ year, data = sub, FUN = sum)
    ya <- ya[order(ya$year), ]
    first <- ya[1, ]; last <- ya[nrow(ya), ]
    delta <- last$pct - first$pct
    htmltools::HTML(sprintf(
      "<div style='padding:10px 14px;background:#f6f6f6;border-radius:6px;margin-bottom:10px'>
        <b>Área alterada (antrópica) — %s %s:</b>
        %.1f%% em %d &rarr; %.1f%% em %d
        (<span style='color:%s'><b>%+.1f pontos percentuais</b></span> no período).
       </div>",
      attr(ts, "species") %||% "", attr(ts, "range") %||% "",
      first$pct, first$year, last$pct, last$year,
      if (delta >= 0) "#d4271e" else "#1f8d49", delta))
  })

  output$ts_tbl <- DT::renderDT({
    ts <- ts_data(); req(ts)
    DT::datatable(ts, rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 15),
                  caption = "Composicao (%) por ano")
  })

  output$dl_ts <- downloadHandler(
    filename = function() paste0("mappingAS_serie_", input$ts_species, "_", Sys.Date(), ".csv"),
    content = function(file) {
      ts <- ts_data(); req(ts)
      utils::write.csv(ts, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ---- Save-image handlers ----
  output$dl_map_html <- downloadHandler(
    filename = function() paste0("mappingAS_mapa_", input$map_species, "_", Sys.Date(), ".html"),
    content = function(file) {
      req(result(), input$map_species)
      m <- mappingAS::map_species(result(), species = input$map_species)
      htmlwidgets::saveWidget(m, file, selfcontained = TRUE)
    }
  )

  output$dl_map_png <- downloadHandler(
    filename = function() paste0("mappingAS_mapa_", input$map_species, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(result(), input$map_species)
      if (!requireNamespace("webshot2", quietly = TRUE)) {
        showNotification(
          paste("Para salvar o mapa como PNG, instale o pacote 'webshot2'",
                "(requer Chrome/Chromium). Enquanto isso, use o botao 'Baixar mapa (HTML)'."),
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
      withProgress(message = "Gerando mapa publicável...", value = 0, {
        m <- mappingAS::map_static(result(), species = input$map_species,
                                   mapbiomas = isTRUE(input$do_mb))
        ggplot2::ggsave(file, plot = m, width = 9, height = 8, dpi = 300)
        incProgress(1)
      })
    }
  )
  output$dl_chart_png <- downloadHandler(
    filename = function() paste0("mappingAS_conversao_", input$chart_species, "_", Sys.Date(), ".png"),
    content = function(file) {
      req(result(), input$chart_species)
      grDevices::png(file, width = 1100, height = 750, res = 130)
      on.exit(grDevices::dev.off(), add = TRUE)
      mappingAS::plot_conversion(result(), species = input$chart_species)
    }
  )

  output$dl_ts_png <- downloadHandler(
    filename = function() paste0("mappingAS_serie_", input$ts_species, "_", Sys.Date(), ".png"),
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
