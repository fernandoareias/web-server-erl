#!/bin/bash

# Diretório do projeto
PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Verifica se rebar3 está instalado
if ! command -v rebar3 &> /dev/null; then
    echo "rebar3 não está instalado. Por favor, instale primeiro."
    exit 1
fi

# Função para exibir ajuda
show_help() {
    echo "Uso: $0 [opção]"
    echo "Opções:"
    echo "  -h, --help     Mostra esta ajuda"
    echo "  -c, --clean    Limpa documentação existente antes de gerar nova"
}

# Processa argumentos
CLEAN=0
while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--help)
            show_help
            exit 0
            ;;
        -c|--clean)
            CLEAN=1
            shift
            ;;
        *)
            echo "Opção inválida: $1"
            show_help
            exit 1
            ;;
    esac
done

# Limpa documentação existente se solicitado
if [ $CLEAN -eq 1 ]; then
    echo "Limpando documentação existente..."
    rm -rf "$PROJECT_DIR/doc"
fi

# Compila o projeto
echo "Compilando projeto..."
cd "$PROJECT_DIR"
rebar3 compile || {
    echo "Falha na compilação"
    exit 1
}

# Gera documentação
echo "Gerando documentação..."
rebar3 edoc || {
    echo "Falha ao gerar documentação"
    exit 1
}

# Verifica se a documentação foi gerada
if [ -d "$PROJECT_DIR/doc" ]; then
    echo "Documentação gerada com sucesso em: $PROJECT_DIR/doc"
    
    # Abre a documentação no navegador padrão (macOS)
    if [[ "$OSTYPE" == "darwin"* ]]; then
        open "$PROJECT_DIR/doc/index.html"
    # Linux
    elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
        xdg-open "$PROJECT_DIR/doc/index.html"
    fi
else
    echo "Falha ao gerar documentação"
    exit 1
fi 