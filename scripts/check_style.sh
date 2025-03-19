#!/bin/bash

# Diretório do projeto
PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Verifica se elvis está instalado
if ! command -v elvis &> /dev/null; then
    echo "elvis não está instalado. Deseja instalar? (y/n)"
    read -r response
    if [[ "$response" =~ ^([yY][eE][sS]|[yY])+$ ]]; then
        if ! command -v rebar3 &> /dev/null; then
            echo "rebar3 é necessário para instalar elvis"
            exit 1
        fi
        echo "Instalando elvis..."
        git clone https://github.com/inaka/elvis.git
        cd elvis
        rebar3 compile
        rebar3 escriptize
        sudo cp _build/default/bin/elvis /usr/local/bin/
        cd ..
        rm -rf elvis
    else
        echo "Instalação cancelada"
        exit 1
    fi
fi

# Função para exibir ajuda
show_help() {
    echo "Uso: $0 [opções]"
    echo "Opções:"
    echo "  -h, --help     Mostra esta ajuda"
    echo "  -f, --fix      Tenta corrigir problemas de estilo automaticamente"
    echo "  -v, --verbose  Mostra saída detalhada"
}

# Configurações padrão
FIX=0
VERBOSE=0

# Processa argumentos
while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--help)
            show_help
            exit 0
            ;;
        -f|--fix)
            FIX=1
            shift
            ;;
        -v|--verbose)
            VERBOSE=1
            shift
            ;;
        *)
            echo "Opção inválida: $1"
            show_help
            exit 1
            ;;
    esac
done

cd "$PROJECT_DIR"

# Cria arquivo de configuração do elvis se não existir
if [ ! -f "elvis.config" ]; then
    cat > elvis.config << 'EOF'
[
    {
        elvis,
        [
            {
                config,
                [
                    #{
                        dirs => ["apps/*/src", "src"],
                        filter => "*.erl",
                        rules => [
                            {line_length, #{limit => 100}},
                            {no_tabs},
                            {no_trailing_whitespace},
                            {macro_names},
                            {macro_module_names},
                            {operator_spaces, #{rules => [{right, ","},
                                                        {right, "++"},
                                                        {left, "++"}]}},
                            {nesting_level, #{level => 3}},
                            {no_if_expression},
                            {used_ignored_variable},
                            {no_behavior_info},
                            {module_naming_convention, #{regex => "^([a-z][a-z0-9]*_?)*$"}},
                            {function_naming_convention, #{regex => "^([a-z][a-z0-9]*_?)*$"}},
                            {variable_naming_convention, #{regex => "^_?([A-Z][0-9a-zA-Z]*)$"}},
                            {state_record_and_type},
                            {no_spec_with_records},
                            {dont_repeat_yourself, #{min_complexity => 10}},
                            {max_function_length, #{max => 30}},
                            {no_debug_call, #{debug_functions => [{ct, pal}, {io, format}]}}
                        ]
                    }
                ]
            }
        ]
    }
].
EOF
    echo "Arquivo de configuração elvis.config criado"
fi

# Executa verificação de estilo
echo "Verificando estilo do código..."

if [ $VERBOSE -eq 1 ]; then
    elvis rock
else
    elvis rock 2>&1 | grep -v "Loading configuration from"
fi

ELVIS_EXIT_CODE=$?

# Se a verificação falhou e a opção de correção está ativada
if [ $ELVIS_EXIT_CODE -ne 0 ] && [ $FIX -eq 1 ]; then
    echo "Tentando corrigir problemas de estilo..."
    
    # Lista de arquivos Erlang
    find . -name "*.erl" -type f | while read -r file; do
        # Remove espaços em branco no final das linhas
        if [[ "$OSTYPE" == "darwin"* ]]; then
            # macOS
            sed -i '' 's/[[:space:]]*$//' "$file"
        else
            # Linux
            sed -i 's/[[:space:]]*$//' "$file"
        fi
        
        # Substitui tabs por espaços
        # Cria um arquivo temporário único para evitar problemas
        temp_file=$(mktemp)
        expand -t 4 "$file" > "$temp_file" && mv "$temp_file" "$file"
    done
    
    echo "Correções básicas aplicadas. Execute o script novamente para verificar."
fi

if [ $ELVIS_EXIT_CODE -eq 0 ]; then
    echo "Verificação de estilo concluída com sucesso"
else
    echo "Foram encontrados problemas de estilo"
fi

exit $ELVIS_EXIT_CODE 