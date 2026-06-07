# mappingAS <img src="man/figures/featured_Resultado.png" align="right" height="139" />

**Métricas de distribuição geográfica (EOO/AOO) e conversão de habitat (MapBiomas) para avaliação de espécies.**

`mappingAS` é um pacote R, acompanhado de interface gráfica em Shiny, voltado à triagem do Critério B da Lista Vermelha da IUCN. O pacote executa as seguintes etapas:

1. **Importação dos dados de ocorrência.** Lê registros a partir de planilhas (`.xlsx`, `.csv`) e de arquivos vetoriais (`.shp`, `.gpkg`, `.geojson`), com detecção automática das colunas de espécie, longitude e latitude.

2. **Cálculo das métricas de distribuição.** Estima a Extensão de Ocorrência (EOO; *minimum convex polygon*) e a Área de Ocupação (AOO; grade de 2 km), ambas computadas em projeção equivalente (de áreas iguais) centrada nos dados, em conformidade com as diretrizes da IUCN.

3. **Quantificação da conversão de habitat.** Calcula, para cada espécie e tanto na EOO quanto na AOO, o percentual de área convertida (antrópica) e o percentual de área natural remanescente, com base na coleção mais recente do MapBiomas (Coleção 10). O índice de conversão adota denominador terrestre — `convertido = antrópico / (antrópico + natural) × 100`, com água e áreas não observadas excluídas por padrão — e o detalhamento por classe do MapBiomas é disponibilizado integralmente.

4. **Série temporal da cobertura.** Gera a evolução da composição da cobertura (percentual de área por ano) no interior da EOO/AOO, em formato análogo aos gráficos de cobertura e uso da terra do MapBiomas.

5. **Aplicação interativa (Shiny).** Disponibiliza uma interface para visualização cartográfica, consulta a tabelas e gráficos e exportação dos resultados (CSV, *shapefile*/GeoPackage).

> **Nota.** As categorias de risco geradas são **provisórias** e destinam-se exclusivamente à triagem, uma vez que se baseiam apenas nos limiares de tamanho de EOO e AOO. Elas **não substituem** uma avaliação formal da IUCN, que requer também a análise das condições de fragmentação, declínio e flutuação (subcritérios a, b e c).
---

## Instalação

```r
# install.packages("remotes")
remotes::install_github("lucasbarreirageo/mappingAS")
```

Dependências principais (instaladas automaticamente): `sf`, `terra`, `readxl`, `dplyr`, `leaflet`, `DT`, `shiny`, `bslib`.

O backend **local** do MapBiomas usa o **GDAL com `/vsicurl/`**, que acompanha o `terra`/`sf` — **não é preciso conta no Google Earth Engine nem baixar o mosaico nacional inteiro** (apenas a janela da área de cada espécie é lida).

O backend opcional via Earth Engine requer `rgee` configurado:

```r
install.packages("rgee")
rgee::ee_install()      # cria o ambiente Python
rgee::ee_Initialize()   # autentica na sua conta Earth Engine
```

---

## Uso rápido (linha de comando)

```r
library(mappingAS)

# 1. Importar pontos (csv/xlsx/shp) — detecta colunas automaticamente
occ <- read_occurrences("minhas_ocorrencias.xlsx")

# Se a detecção falhar, informe as colunas manualmente:
# occ <- read_occurrences("dados.csv",
#                         species_col = "especie",
#                         lon_col = "longitude",
#                         lat_col = "latitude")

# 2. Rodar a avaliação completa (EOO, AOO e conversão MapBiomas)
res <- assess_speci
s(occ, year = 2024, collection = 10, backend = "local")

# 3. Ver a tabela-resumo (uma linha por espécie)
res$summary

# 4. Mapa interativo e gráfico de conversão
map_species(res)            # leaflet com pontos + EOO + AOO
plot_conversion(res)        # barras natural vs. convertido

# 5. Exportar EOO e AOO como dados espaciais (shapefile ou GeoPackage)
export_ranges(res)                      # cria mappingAS_EOO.shp e mappingAS_AOO.shp
export_ranges(res, format = "gpkg")     # um GeoPackage com as camadas eoo e aoo
export_ranges(res, zip = TRUE)          # tudo num único .zip (bom para enviar)
```

### Exemplo incluído

```r
ex <- system.file("extdata", "example_occurrences.csv", package = "mappingAS")
occ <- read_occurrences(ex)
res <- assess_species(occ, backend = "local")
res
```

---

## Aplicativo Shiny

```r
library(mappingAS)
run_app()
```

Na aplicação você pode: enviar o arquivo (`.xlsx`/`.csv`/`.shp` em `.zip`), mapear colunas se necessário, escolher o **ano** (1985–2024) e a **coleção** do MapBiomas, o **tamanho da célula** do AOO, o **backend** (local/GEE), **baixar os resultados em CSV** e **baixar o EOO e o AOO** como **shapefile (.zip)** ou **GeoPackage (.gpkg)**. Há abas de **Resultados**, **Mapa**, **Conversão** e **Métodos**.

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

> Por padrão, `export_ranges()` também grava `mappingAS_classes.csv` com a **área e o % de cada classe** do MapBiomas (todas as classes) dentro do EOO e do AOO. Desligue com `class_csv = FALSE`.

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

## Citação

Ao usar este pacote, cite também as fontes de dados e métodos:

- **MapBiomas** — Projeto MapBiomas, Coleção 10 da Série Anual de Mapas de Cobertura e Uso da Terra do Brasil (https://brasil.mapbiomas.org).
- **IUCN** — IUCN Standards and Petitions Committee. *Guidelines for Using the IUCN Red List Categories and Criteria.*
- **GeoCat** — Bachman, S. et al. (2011). *Supporting Red List threat assessments with GeoCAT.* ZooKeys.

## Licença
[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.20574863-blue.svg)](https://doi.org/10.5281/zenodo.20574863)

MIT. Veja o arquivo `LICENSE`.
