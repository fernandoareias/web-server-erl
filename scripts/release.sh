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
    echo "Uso: $0 [opções] <versão>"
    echo "Opções:"
    echo "  -h, --help     Mostra esta ajuda"
    echo "  -c, --clean    Limpa arquivos antes de gerar a release"
    echo "  -t, --test     Executa testes antes de gerar a release"
    echo ""
    echo "Exemplo:"
    echo "  $0 -c -t 1.0.0"
}

# Configurações padrão
CLEAN=0
TEST=0
VERSION=""

# Processa argumentos
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
        -t|--test)
            TEST=1
            shift
            ;;
        *)
            if [[ -z "$VERSION" ]]; then
                VERSION=$1
            else
                echo "Versão já especificada: $VERSION"
                show_help
                exit 1
            fi
            shift
            ;;
    esac
done

# Verifica se a versão foi fornecida
if [[ -z "$VERSION" ]]; then
    echo "Erro: Versão não especificada"
    show_help
    exit 1
fi

# Verifica se a versão está no formato correto (x.y.z)
if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
    echo "Erro: Versão deve estar no formato x.y.z (exemplo: 1.0.0)"
    exit 1
fi

cd "$PROJECT_DIR"

# Limpa arquivos se solicitado
if [ $CLEAN -eq 1 ]; then
    echo "Limpando arquivos..."
    ./scripts/clean.sh -a
fi

# Executa testes se solicitado
if [ $TEST -eq 1 ]; then
    echo "Executando testes..."
    if [ -f "./scripts/run_tests.sh" ]; then
        ./scripts/run_tests.sh
    else
        rebar3 eunit && rebar3 ct
    fi
    
    if [ $? -ne 0 ]; then
        echo "Erro: Testes falharam"
        exit 1
    fi
fi

# Atualiza a versão no arquivo .app.src
APP_FILE=$(find . -name "*.app.src" -type f)
if [ -f "$APP_FILE" ]; then
    echo "Atualizando versão no arquivo $APP_FILE..."
    
    # Detectar sistema operacional para o comando sed
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS
        sed -i '' "s/{vsn, \".*\"}/{vsn, \"$VERSION\"}/" "$APP_FILE"
    else
        # Linux
        sed -i "s/{vsn, \".*\"}/{vsn, \"$VERSION\"}/" "$APP_FILE"
    fi
fi

# Atualiza a versão no rebar.config
if [ -f "rebar.config" ]; then
    echo "Atualizando versão no arquivo rebar.config..."
    
    # Detectar sistema operacional para o comando sed
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS
        sed -i '' "s/{release, {web_server, \"[0-9.]*\"}/{release, {web_server, \"$VERSION\"}/" "rebar.config"
    else
        # Linux
        sed -i "s/{release, {web_server, \"[0-9.]*\"}/{release, {web_server, \"$VERSION\"}/" "rebar.config"
    fi
fi

# Compila o projeto
echo "Compilando projeto..."
rebar3 compile || {
    echo "Erro: Falha na compilação"
    exit 1
}

# Gera a release
echo "Gerando release $VERSION..."
rebar3 as prod release || {
    echo "Erro: Falha ao gerar release"
    exit 1
}

# Cria o pacote da release
echo "Criando pacote da release..."
RELEASE_DIR="_build/prod/rel/web_server"
RELEASE_NAME="web_server-$VERSION"
RELEASE_TAR="$RELEASE_NAME.tar.gz"

if [ -d "$RELEASE_DIR" ]; then
    cd "$RELEASE_DIR"
    tar czf "../../$RELEASE_TAR" ./*
    cd "$PROJECT_DIR"
    
    echo "Release criada: $RELEASE_TAR"
    
    # Calcula e exibe o hash do arquivo
    if command -v shasum &> /dev/null; then
        echo "SHA256:"
        shasum -a 256 "_build/prod/$RELEASE_TAR"
    fi
else
    echo "Erro: Diretório da release não encontrado"
    exit 1
fi

# Cria tag no git
echo "Criando tag git v$VERSION..."
git tag -a "v$VERSION" -m "Release v$VERSION"
git push origin "v$VERSION"

echo "Release v$VERSION criada com sucesso!"
exit 0 