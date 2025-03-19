#!/bin/bash

# Diretório do projeto
PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_DIR"

# Função para exibir ajuda
show_help() {
    echo "Uso: $0 [opção]"
    echo "Opções:"
    echo "  -h, --help     Mostra esta ajuda"
    echo "  -u, --unit     Executa apenas testes unitários"
    echo "  -i, --int      Executa apenas testes de integração"
    echo "  -a, --all      Executa todos os testes (padrão)"
    echo "  -v, --verbose  Mostra saída detalhada dos testes"
    echo "  -c, --cover    Gera relatório de cobertura de código"
}

# Configurações padrão
RUN_UNIT=1
RUN_INT=1
VERBOSE=0
COVERAGE=0

# Processa argumentos
while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--help)
            show_help
            exit 0
            ;;
        -u|--unit)
            RUN_UNIT=1
            RUN_INT=0
            shift
            ;;
        -i|--int)
            RUN_UNIT=0
            RUN_INT=1
            shift
            ;;
        -a|--all)
            RUN_UNIT=1
            RUN_INT=1
            shift
            ;;
        -v|--verbose)
            VERBOSE=1
            shift
            ;;
        -c|--cover)
            COVERAGE=1
            shift
            ;;
        *)
            echo "Opção inválida: $1"
            show_help
            exit 1
            ;;
    esac
done

# Configura a saída verbosa e cobertura
OPTS=""
if [ $VERBOSE -eq 1 ]; then
    OPTS="$OPTS verbose"
fi

if [ $COVERAGE -eq 1 ]; then
    OPTS="$OPTS --cover"
fi

# Executa testes unitários
if [ $RUN_UNIT -eq 1 ]; then
    echo "Executando testes unitários..."
    rebar3 eunit $OPTS
    UNIT_RESULT=$?
    
    if [ $UNIT_RESULT -ne 0 ]; then
        echo "Erro: Testes unitários falharam"
        exit 1
    fi
    
    echo "Testes unitários completados com sucesso"
fi

# Executa testes de integração
if [ $RUN_INT -eq 1 ]; then
    echo "Executando testes de integração..."
    rebar3 ct $OPTS
    INT_RESULT=$?
    
    if [ $INT_RESULT -ne 0 ]; then
        echo "Erro: Testes de integração falharam"
        exit 1
    fi
    
    echo "Testes de integração completados com sucesso"
fi

echo "Todos os testes completados com sucesso!"

# Gera relatório de cobertura se solicitado
if [ $COVERAGE -eq 1 ]; then
    echo "Gerando relatório de cobertura..."
    rebar3 cover -v
    
    # Tenta abrir o relatório em um navegador se possível
    COVER_DIR="_build/test/cover"
    if [ -d "$COVER_DIR" ] && [ -f "$COVER_DIR/index.html" ]; then
        echo "Relatório de cobertura gerado em: $COVER_DIR/index.html"
        
        # Tenta abrir o relatório dependendo do SO
        if command -v open &> /dev/null && [[ "$OSTYPE" == "darwin"* ]]; then
            echo "Abrindo relatório no navegador..."
            open "$COVER_DIR/index.html"
        elif command -v xdg-open &> /dev/null && [[ "$OSTYPE" == "linux"* ]]; then
            echo "Abrindo relatório no navegador..."
            xdg-open "$COVER_DIR/index.html"
        else
            echo "Para visualizar o relatório, abra manualmente o arquivo: $COVER_DIR/index.html"
        fi
    else
        echo "Aviso: Relatório de cobertura não encontrado"
    fi
fi

exit 0 