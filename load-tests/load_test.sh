#!/bin/bash

# Diretório do projeto
PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Verifica se wrk está instalado
if ! command -v wrk &> /dev/null; then
    echo "wrk não está instalado. Por favor, instale primeiro."
    echo "No macOS: brew install wrk"
    echo "No Ubuntu: sudo apt-get install wrk"
    exit 1
fi

# Configurações padrão
DURATION=${1:-"30s"}
CONNECTIONS=${2:-"100"}
THREADS=${3:-"4"}
URL=${4:-"http://localhost:8091"}

# Função para exibir ajuda
show_help() {
    echo "Uso: $0 [duração] [conexões] [threads] [url]"
    echo ""
    echo "Argumentos:"
    echo "  duração   Duração do teste (padrão: 30s)"
    echo "  conexões  Número de conexões (padrão: 100)"
    echo "  threads   Número de threads (padrão: 4)"
    echo "  url       URL do servidor (padrão: http://localhost:8091)"
    echo ""
    echo "Exemplo:"
    echo "  $0 1m 1000 8 http://localhost:8091"
}

# Verifica se precisa mostrar ajuda
if [ "$1" == "-h" ] || [ "$1" == "--help" ]; then
    show_help
    exit 0
fi

# Executa o teste
echo "=== Iniciando Teste de Carga ==="
echo "URL: $URL"
echo "Duração: $DURATION"
echo "Conexões: $CONNECTIONS"
echo "Threads: $THREADS"
echo ""

wrk -t$THREADS -c$CONNECTIONS -d$DURATION --latency "$URL"

# Salva resultado em arquivo
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
RESULT_FILE="$PROJECT_DIR/load-tests/results_${TIMESTAMP}.txt"

{
    echo "=== Teste de Carga - $TIMESTAMP ==="
    echo "URL: $URL"
    echo "Duração: $DURATION"
    echo "Conexões: $CONNECTIONS"
    echo "Threads: $THREADS"
    echo ""
    wrk -t$THREADS -c$CONNECTIONS -d$DURATION --latency "$URL"
} > "$RESULT_FILE"

echo ""
echo "Resultados salvos em: $RESULT_FILE" 