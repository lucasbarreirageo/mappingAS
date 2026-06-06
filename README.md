# geoConvBR <img src="man/figures/logo.png" align="right" height="139" />

**Métricas de distribuição (EOO/AOO) e conversão de habitat (MapBiomas) para avaliação de espécies — no estilo do GeoCat.**

`geoConvBR` é um pacote R com interface Shiny para triagem do **Critério B** da Lista Vermelha da IUCN. Ele:

1. **Importa pontos de ocorrência** a partir de `.xlsx`, `.csv` e **shapefile** (também `.gpkg`, `.geojson`, `.zip`), detectando automaticamente as colunas de espécie, longitude e latitude;
2. Calcula a **Extensão de Ocorrência (EOO**, polígono convexo mínimo) e a **Área de Ocupação (AOO**, grade de 2 km), em projeção de áreas iguais centrada nos dados;
3. Quantifica o **percentual de área convertida (antrópica)** e o **percentual de área natural/atual** dentro **tanto do EOO quanto do AOO**, para **cada espécie**, usando o **MapBiomas mais recente (Coleção 10)**, e detalha a **área de cada classe** do MapBiomas;
4. Monta a **série temporal** da cobertura (**% da área × ano**) dentro do EOO/AOO, no estilo dos gráficos do MapBiomas;
5. Abre um **aplicativo Shiny** *friendly use* com mapa, tabelas, gráficos e download dos resultados (CSV, shapefile/GeoPackage).

> ⚠️ As categorias de risco produzidas são **provisórias e apenas para triagem** (baseadas somente nos limiares de tamanho de EOO/AOO). Não substituem uma avaliação formal da IUCN, que exige também as condições de fragmentação, declínio e flutuação (subcritérios a, b, c).

---

## Instalação

```r
# install.packages("remotes")
remotes::install_github("seu-usuario/geoConvBR")
```

Dependências principais (instaladas automaticamente): `sf`, `terra`, `readxl`, `dplyr`, `leaflet`, `DT`, `shiny`, `bslib`.

O backend **local** do MapBiomas usa o **GDAL com `/vsicurl/`**, que acompanha o `terra`/`sf` — **não é preciso conta no Google Earth Engine nem baixar o mosaico nacional inteiro** (apenas a janela da área de cada espécie é lida pela internet).

O backend opcional via Earth Engine requer `rgee` configurado:

```r
install.packages("rgee")
rgee::ee_install()      # cria o ambiente Python
rgee::ee_Initialize()   # autentica na sua conta Earth Engine
```

---

## Uso rápido (linha de comando)

```r
library(geoConvBR)

# 1. Importar pontos (csv/xlsx/shp) — detecta colunas automaticamente
occ <- read_occurrences("minhas_ocorrencias.xlsx")

# Se a detecção falhar, informe as colunas manualmente:
# occ <- read_occurrences("dados.csv",
#                         species_col = "especie",
#                         lon_col = "longitude",
#                         lat_col = "latitude")

# 2. Rodar a avaliação completa (EOO, AOO e conversão MapBiomas)
res <- assess_species(occ, year = 2024, collection = 10, backend = "local")

# 3. Ver a tabela-resumo (uma linha por espécie)
res$summary

# 4. Mapa interativo e gráfico de conversão
map_species(res)            # leaflet com pontos + EOO + AOO
plot_conversion(res)        # barras natural vs. convertido

# 5. Exportar EOO e AOO como dados espaciais (shapefile ou GeoPackage)
export_ranges(res)                      # cria geoConvBR_EOO.shp e geoConvBR_AOO.shp
export_ranges(res, format = "gpkg")     # um GeoPackage com as camadas eoo e aoo
export_ranges(res, zip = TRUE)          # tudo num único .zip (bom para enviar)
```

### Exemplo incluído

```r
ex <- system.file("extdata", "example_occurrences.csv", package = "geoConvBR")
occ <- read_occurrences(ex)
res <- assess_species(occ, backend = "local")
res
```

---

## Aplicativo Shiny

```r
library(geoConvBR)
run_app()
```

No app você pode: enviar o arquivo (`.xlsx`/`.csv`/`.shp` em `.zip`), mapear colunas se necessário, escolher o **ano** (1985–2024) e a **coleção** do MapBiomas, o **tamanho da célula** do AOO, o **backend** (local/GEE), **baixar os resultados em CSV** e **baixar o EOO e o AOO** como **shapefile (.zip)** ou **GeoPackage (.gpkg)**. Há abas de **Resultados**, **Mapa**, **Conversão** e **Métodos**.

---

## Exportar EOO e AOO (shapefile / GeoPackage)

A função `export_ranges()` grava os polígonos do **EOO** e do **AOO** (um por espécie) já com os atributos de área e conversão na tabela:

```r
export_ranges(res,
              dir = "saida",            # pasta de saída
              format = "shapefile",     # ou "gpkg"
              what = "both",            # "eoo", "aoo" ou "both"
              crs = 4326,               # CRS de saída (padrão WGS84)
              aoo_as = "union",         # "union" (1 feição/espécie) ou "cells"
              zip = FALSE)              # TRUE = empacota tudo num .zip
```

Como o **shapefile limita nomes de campo a 10 caracteres**, a tabela de atributos usa nomes compactos:

| Campo | Significado |
|---|---|
| `species` | Espécie |
| `eoo_km2` / `aoo_km2` | Área do EOO / AOO (km²) |
| `n_cells` | Nº de células do AOO (camada AOO) |
| `conv_pct` | % convertido (antrópico) **naquele polígono** |
| `nat_pct` | % natural **naquele polígono** |
| `cat_B1` / `cat_B2` | Categoria provisória (só tamanho) |
| `prov_cat` | Categoria provisória combinada |
| `mb_year`, `mb_coll` | Ano e coleção do MapBiomas |

O **GeoPackage** (`format = "gpkg"`) guarda as duas camadas (`eoo` e `aoo`) em **um único arquivo** e não tem o limite de 10 caracteres — é a opção mais prática para abrir no QGIS.

> Por padrão, `export_ranges()` também grava `geoConvBR_classes.csv` com a **área e o % de cada classe** do MapBiomas (todas as classes) dentro do EOO e do AOO. Desligue com `class_csv = FALSE`.

---

## Composição por classe (todas as classes do MapBiomas)

Além do resumo natural × alterado, é possível obter a **área e o percentual de cada classe** do MapBiomas dentro do EOO e do AOO:

```r
ct <- class_table(res)            # uma linha por espécie × área (EOO/AOO) × classe
head(ct)
#  species range code class_pt           class_en        group     area_km2  pct
#  sp1     EOO  3    Formacao Florestal  Forest Formation natural  ...        ...
```

Colunas: `species`, `range` (EOO/AOO), `code` (código do pixel), `class_pt`/`class_en`, `group` (natural/anthropic/water/other), `area_km2` e `pct` (% da área mapeada naquele polígono). No app, a aba **Classes** mostra essa tabela e permite baixá-la em CSV.

---

## Série temporal (% da área × ano)

Para ver a **alteração no decorrer do tempo**, o pacote calcula a composição da cobertura para vários anos e desenha um **gráfico de área empilhada** (% × ano), no mesmo estilo das figuras do MapBiomas:

```r
# série da cobertura dentro do EOO da espécie (a cada 5 anos, 1985–2024)
ts <- timeseries_for_species(res, species = "sp1", range = "eoo", by = "class")

plot_timeseries(ts)               # gráfico de área empilhada (cores oficiais)

# anuais (mais lento) e por grupo:
ts_anual <- timeseries_for_species(res, range = "eoo",
                                   years = mb_years(), by = "group")
```

Ou diretamente sobre uma geometria qualquer:

```r
ts <- cover_timeseries(minha_geometria, years = c(1990, 2000, 2010, 2020),
                       by = "class", backend = "local")
```

Cada ano é lido separadamente do MapBiomas (uma janela do GeoTIFF por ano), então séries anuais de toda a coleção podem demorar; o padrão usa passo de 5 anos. No app, a aba **Série temporal** tem o gráfico, a tabela e o download em CSV, com seletor de **EOO/AOO**, **classe/grupo** e **passo (anos)**.

---

## Colunas da tabela de resultados

| Coluna | Significado |
|---|---|
| `species` | Nome da espécie |
| `n_records`, `n_unique` | Nº de registros e de coordenadas únicas |
| `eoo_km2` | EOO em km² (NA se < 3 pontos únicos) |
| `aoo_km2`, `aoo_cells` | AOO em km² e nº de células ocupadas |
| `eoo_converted_pct` / `eoo_natural_pct` | % convertido / % natural **dentro do EOO** |
| `aoo_converted_pct` / `aoo_natural_pct` | % convertido / % natural **dentro do AOO** |
| `eoo_cat_B1`, `aoo_cat_B2`, `provisional_cat` | Categorias provisórias (só por tamanho) |
| `mapbiomas_year`, `mapbiomas_collection` | Fonte de cobertura usada |

---

## Métodos (resumo)

- **EOO** = área do polígono convexo mínimo sobre os pontos (mínimo de 3 coordenadas únicas; caso contrário `NA`).
- **AOO** = nº de células de 2×2 km ocupadas × 4 km² (tamanho da célula configurável via `cell_km`).
- Ambas as áreas são medidas em uma projeção **LAEA de áreas iguais** centrada nos dados, conforme as diretrizes da IUCN.
- **Conversão**: as classes do MapBiomas são agrupadas em *natural*, *antrópico*, *água*, *não observado* e *outros*. O índice principal é
  `converted_pct = antrópico / (antrópico + natural) × 100` (denominador terrestre; água e "não observado" ficam fora por padrão). Use `water_in_denominator = TRUE` para incluir água. As versões `*_total` (sobre toda a área) também ficam disponíveis no detalhamento por espécie (`res$detail`).
- **Categorias do Critério B**: limiares de tamanho — EOO < 100 / 5.000 / 20.000 km² e AOO < 10 / 500 / 2.000 km² para CR / EN / VU, respectivamente. **Somente triagem.**

---

## Limitações importantes

- As **categorias são provisórias** (não incluem fragmentação, declínio ou flutuação).
- O **resultado do AOO é sensível** à origem/posicionamento da grade e ao esforço amostral.
- O backend **local** lê pela rede a janela do GeoTIFF do MapBiomas; para **distribuições muito grandes (continentais)** isso pode ficar lento — nesse caso prefira `backend = "gee"`.
- A **acurácia do MapBiomas** varia por classe, bioma e ano; consulte a documentação oficial.
- A documentação `man/*.Rd` **já acompanha o pacote**; regenere com
  `devtools::document()` após editar os comentários roxygen. O código geoespacial
  **não pôde ser testado no ambiente de build** (sem R/CRAN/rede para os rasters),
  então rode os testes na sua máquina.

---

## Desenvolvimento

```r
# Na raiz do pacote:
devtools::document()   # regenera man/*.Rd e o NAMESPACE a partir do roxygen
devtools::test()       # roda os testes offline (testthat)
devtools::build_vignettes()
devtools::check()      # checagem completa do pacote
```

A documentação (`man/*.Rd`) já acompanha o pacote, mas é regenerada por
`devtools::document()` sempre que você editar os comentários roxygen. A vinheta
(tutorial passo a passo) fica em `vignettes/geoConvBR.Rmd` e pode ser lida com
`vignette("geoConvBR")` depois de instalada.

## Publicar no GitHub (com checagem automática)

O pacote já inclui um workflow de **R CMD check** em
`.github/workflows/R-CMD-check.yaml` (Linux, macOS e Windows). Para publicar:

```bash
git init
git add .
git commit -m "geoConvBR 0.1.0"
git branch -M main
git remote add origin https://github.com/seu-usuario/geoConvBR.git
git push -u origin main
```

A cada push/PR o GitHub Actions roda `R CMD check` automaticamente. Lembre de
atualizar o campo `Authors@R` e as URLs no `DESCRIPTION` com seus dados.

---

## Citação

Ao usar este pacote, cite também as fontes de dados e métodos:

- **MapBiomas** — Projeto MapBiomas, Coleção 10 da Série Anual de Mapas de Cobertura e Uso da Terra do Brasil (https://brasil.mapbiomas.org).
- **IUCN** — IUCN Standards and Petitions Committee. *Guidelines for Using the IUCN Red List Categories and Criteria.*
- **GeoCat** — Bachman, S. et al. (2011). *Supporting Red List threat assessments with GeoCAT.* ZooKeys.

## Licença

MIT. Veja o arquivo `LICENSE`.
