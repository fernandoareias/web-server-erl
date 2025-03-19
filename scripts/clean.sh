#!/bin/bash

# Diretório do projeto
PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Função para exibir ajuda
show_help() {
    echo "Uso: $0 [opções]"
    echo "Opções:"
    echo "  -h, --help     Mostra esta ajuda"
    echo "  -a, --all      Limpa todos os arquivos (incluindo dependências)"
    echo "  -d, --docs     Limpa apenas a documentação"
    echo "  -t, --tests    Limpa apenas arquivos de teste"
}

# Configurações padrão
CLEAN_ALL=0
CLEAN_DOCS=0
CLEAN_TESTS=0

# Processa argumentos
while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--help)
            show_help
            exit 0
            ;;
        -a|--all)
            CLEAN_ALL=1
            shift
            ;;
        -d|--docs)
            CLEAN_DOCS=1
            shift
            ;;
        -t|--tests)
            CLEAN_TESTS=1
            shift
            ;;
        *)
            echo "Opção inválida: $1"
            show_help
            exit 1
            ;;
    esac
done

# Se nenhuma opção específica foi fornecida, assume limpeza padrão
if [ $CLEAN_ALL -eq 0 ] && [ $CLEAN_DOCS -eq 0 ] && [ $CLEAN_TESTS -eq 0 ]; then
    CLEAN_ALL=1
fi

cd "$PROJECT_DIR"

# Função para limpar arquivos
clean_files() {
    echo "Limpando arquivos..."
    
    # Limpa arquivos compilados
    if [ -d "_build" ]; then 
        rm -rf _build
    fi
    
    if [ -d "ebin" ]; then
        rm -rf ebin
    fi
    
    if [ -f "erl_crash.dump" ]; then
        rm -f erl_crash.dump
    fi
    
    # Limpa arquivos temporários
    find . -type f -name "*~" -delete
    find . -type f -name "*.beam" -delete
    find . -type f -name "*.app" -delete
    find . -type f -name ".rebar3" -delete
    
    echo "Arquivos básicos limpos"
}

# Função para limpar documentação
clean_docs() {
    echo "Limpando documentação..."
    if [ -d "doc" ]; then
        rm -rf doc
        echo "Documentação limpa"
    else
        echo "Diretório de documentação não encontrado"
    fi
}

# Função para limpar arquivos de teste
clean_tests() {
    echo "Limpando arquivos de teste..."
    if [ -d "_build/test" ]; then
        rm -rf _build/test
    fi
    
    if [ -d ".eunit" ]; then
        rm -rf .eunit
    fi
    
    if [ -d "test" ]; then
        find test -name "*.beam" -delete
    fi
    
    echo "Arquivos de teste limpos"
}

# Executa limpeza conforme as opções
if [ $CLEAN_ALL -eq 1 ]; then
    clean_files
    clean_docs
    clean_tests
    echo "Limpeza completa realizada"
else
    if [ $CLEAN_DOCS -eq 1 ]; then
        clean_docs
    fi
    if [ $CLEAN_TESTS -eq 1 ]; then
        clean_tests
    fi
fi

# Verifica se rebar3 está disponível e executa limpeza adicional
if command -v rebar3 &> /dev/null; then
    echo "Executando limpeza via rebar3..."
    rebar3 clean
fi

echo "Limpeza concluída"
exit 0 