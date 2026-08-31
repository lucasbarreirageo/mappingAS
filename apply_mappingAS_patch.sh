#!/usr/bin/env bash
# Aplica as duas novidades no pacote mappingAS:
#   1) aba "Add points" (adicionar pontos no mapa, com ou sem tabela)
#   2) vouchers automaticos no factsheet (colunas collector/collectorNumber
#      ou coluna voucher)
#
# Uso, a partir da RAIZ do repositorio mappingAS (no Codespace):
#   bash apply_mappingAS_patch.sh caminho/para/mappingAS_add_points_vouchers.patch
#
# Se nenhum caminho for passado, procura o .patch ao lado deste script.
set -euo pipefail

PATCH="${1:-$(dirname "$0")/mappingAS_add_points_vouchers.patch}"

if [ ! -f DESCRIPTION ] || ! grep -q "^Package: mappingAS" DESCRIPTION; then
  echo "ERRO: rode este script na raiz do repositorio mappingAS." >&2
  exit 1
fi
if [ ! -f "$PATCH" ]; then
  echo "ERRO: patch nao encontrado: $PATCH" >&2
  exit 1
fi

echo ">> Verificando se o patch aplica..."
if git apply --check "$PATCH" 2>/dev/null; then
  echo ">> Aplicando com git apply..."
  git apply "$PATCH"
else
  echo ">> git apply nao encaixou direto; tentando 'git apply --3way'..."
  git apply --3way "$PATCH"
fi

echo ">> Arquivos alterados:"
git status --short

cat <<'EOF'

Pronto. Proximos passos sugeridos (opcionais) no Codespace com R instalado:

  Rscript -e 'devtools::document()'   # regenera NAMESPACE/man (ja incluidos no patch)
  Rscript -e 'devtools::load_all(); testthat::test_local()'
  Rscript -e 'mappingAS::run_app()'   # abre o app e testa a aba "Add points"

Para versionar na main:
  git checkout main
  git add -A && git commit -m "Add hand-drawn points and auto vouchers"
  git push origin main
EOF
