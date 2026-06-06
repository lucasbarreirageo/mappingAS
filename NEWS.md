# geoConvBR 0.1.0

* Primeira versão.
* Importação de pontos de ocorrência a partir de `.csv`/`.tsv`/`.txt`, `.xlsx`/`.xls`
  e arquivos vetoriais (`.shp`, `.gpkg`, `.geojson`, `.zip`), com detecção
  automática das colunas de espécie, longitude e latitude.
* Cálculo de EOO (polígono convexo mínimo) e AOO (grade de 2 km) em projeção de
  áreas iguais (LAEA) centrada nos dados.
* Quantificação de habitat convertido (antrópico) versus natural dentro do EOO e
  do AOO usando MapBiomas (Coleção 10), com dois backends: leitura local por
  janela (`/vsicurl/`, sem conta no Google Earth Engine) e `rgee` (opcional).
* Categoria provisória do Critério B (apenas por limiares de tamanho de EOO/AOO),
  sinalizada como triagem.
* Visualizações: mapa interativo (`leaflet`) e gráfico de conversão.
* Detalhamento por **cada classe** do MapBiomas (`class_table()`): área e % de
  todas as classes dentro do EOO e do AOO; também gravado em CSV por
  `export_ranges()` (`class_csv = TRUE`) e exibido na aba "Classes" do app.
* **Série temporal** da cobertura (`cover_timeseries()`,
  `timeseries_for_species()`) e gráfico de área empilhada (% × ano) com as cores
  oficiais do MapBiomas (`plot_timeseries()`); nova aba "Série temporal" no app.
  O app mostra o histórico anual completo por padrão e destaca quanto a área foi
  alterada (antrópica) entre o primeiro e o último ano. A série é
  "retangularizada" (toda classe em todo ano, 0 onde ausente), corrigindo uma
  falha do `geom_area()` quando uma classe não ocorria em alguns anos.
* Gráfico de conversão (`plot_conversion()`) refeito: barras horizontais
  empilhadas (natural/alterado/água/outros) com rótulos internos e legenda fora
  da área do gráfico, evitando sobreposição.
* Cores oficiais da legenda e utilitários `mb_palette()` e `mb_years()`.
* Exportação do EOO e do AOO como **shapefile** ou **GeoPackage** (`export_ranges()`),
  com área, % convertido/natural e categoria provisória na tabela de atributos;
  botão de download correspondente no aplicativo Shiny.
* Aplicativo Shiny (`run_app()`) para uso interativo, com botões para salvar o
  mapa (HTML; PNG via `webshot2`) e os gráficos de conversão e da série temporal
  como imagem (PNG).
* Documentação `man/*.Rd`, vinheta/tutorial (`vignette("geoConvBR")`) e workflow
  de checagem no GitHub Actions (`R CMD check` em Linux/macOS/Windows).
* Testes offline com `testthat`.
