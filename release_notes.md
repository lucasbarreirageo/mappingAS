## Nova metodologia de cálculo: EOO e AOO no padrão ConR

Esta versão substitui o cálculo da Extensão de Ocorrência (EOO) e da Área de
Ocupação (AOO) pelo método usado no pacote **ConR**, alinhando o `mappingAS`
com a abordagem mais consolidada na literatura de avaliação de risco do
Critério B da IUCN.

### O que mudou

**EOO (medição no elipsoide / "spheroid")**
O polígono convexo mínimo passa a ser construído em coordenadas geográficas
(longitude/latitude), com as arestas densificadas via `st_segmentize(20 km)`
para seguir grandes círculos, e a área é medida diretamente sobre o elipsoide
WGS84. Antes, a medição era feita em uma projeção planar de áreas iguais (LAEA).

**AOO (mínimo sobre grades transladadas)**
A contagem de células ocupadas passa a ser calculada testando várias posições
de grade transladadas aleatoriamente (`n_rep = 30` por padrão), retornando a
**menor** contagem — o procedimento recomendado pelas diretrizes da IUCN e
adotado pelo ConR. Antes, era usada uma única grade de origem fixa.

**Tratamento de casos degenerados (sem quebrar)**
- Menos de 3 pontos únicos: EOO retorna `NA`.
- Pontos perfeitamente colineares: é adicionado um pequeno ruído (*jitter*) às
  coordenadas, como no ConR, para que um polígono válido possa ser construído.
- Qualquer outra falha na construção do polígono retorna `NA` em vez de erro.

### Mudanças incompatíveis (BREAKING CHANGES)

- O valor numérico do EOO muda em relação a versões anteriores, por ser agora
  medido no elipsoide.
- Pontos colineares passam a gerar uma área positiva (via *jitter*) em vez de
  `NA`.
- `lwgeom` e `units` passam a ser dependências obrigatórias (`Imports`).

### Outras notas

- A suíte de testes foi atualizada para refletir o novo método.
- `R CMD check`: 0 errors | 0 warnings.
