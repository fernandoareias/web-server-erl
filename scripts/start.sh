#!/bin/bash

# Diretório do projeto
PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Configurar log
LOG_DIR="$PROJECT_DIR/logs"
LOG_DATE=$(date +"%Y-%m-%d-%H-%M-%S")
LOG_FILE="$LOG_DIR/log-$LOG_DATE.log"

# Verificar se diretório de logs existe
if [ ! -d "$LOG_DIR" ]; then
    echo "Erro: Diretório de logs não encontrado: $LOG_DIR"
    exit 1
fi

# Iniciar log
echo "=== Iniciando servidor em $(date) ===" > "$LOG_FILE"

# Configura valores padrão
ENV=${1:-"dev"}
OBSERVER=0
PORT=8091  # Será extraído do arquivo de configuração

# Função para registrar mensagens no log
log() {
    local message="[$(date +"%Y-%m-%d %H:%M:%S")] $1"
    echo "$message" | tee -a "$LOG_FILE"
}

# Detectar a instalação do Erlang
log "Verificando instalação do Erlang..."
ERL_PATH=$(which erl)
log "Caminho do Erlang: $ERL_PATH"

# Obter diretórios Erlang
ERL_ROOT=$(dirname $(dirname "$ERL_PATH"))
if [ -d "$ERL_ROOT/lib/erlang" ]; then
    ERL_ROOT="$ERL_ROOT/lib/erlang"
elif [ -d "/usr/lib/erlang" ]; then
    ERL_ROOT="/usr/lib/erlang"
elif [ -d "/opt/homebrew/Cellar/erlang" ]; then
    # Para instalações Homebrew no macOS
    BREW_ERLANG_VERSION=$(ls -t /opt/homebrew/Cellar/erlang/ | head -1)
    ERL_ROOT="/opt/homebrew/Cellar/erlang/$BREW_ERLANG_VERSION/lib/erlang"
fi
log "Diretório raiz do Erlang: $ERL_ROOT"

# Verificar a existência dos diretórios de biblioteca
if [ ! -d "$ERL_ROOT" ]; then
    log "ERRO: Não foi possível encontrar o diretório raiz do Erlang"
    exit 1
fi

# Gerar timestamp único para o nome do nó
TIMESTAMP=$(date +%s)
NODE_NAME="web_server_${TIMESTAMP}"
log "Nome do nó: $NODE_NAME"

# Processa argumentos adicionais
shift 2>/dev/null || true
while [[ $# -gt 0 ]]; do
    case "$1" in
        --observer|-o)
            OBSERVER=1
            log "Observer habilitado"
            shift
            ;;
        --port|-p)
            PORT="$2"
            log "Porta configurada via linha de comando: $PORT"
            shift 2
            ;;
        *)
            log "Opção desconhecida: $1"
            log "Uso: $0 [dev|prod|test] [--observer|-o] [--port|-p PORTA]"
            exit 1
            ;;
    esac
done

# Verifica o ambiente
if [ "$ENV" != "dev" ] && [ "$ENV" != "prod" ] && [ "$ENV" != "test" ]; then
    log "Ambiente inválido. Use: dev, prod ou test"
    log "Uso: $0 [dev|prod|test] [--observer|-o] [--port|-p PORTA]"
    exit 1
fi

# Configura o ambiente
CONFIG_FILE="$PROJECT_DIR/config/$ENV.config"
if [ ! -f "$CONFIG_FILE" ]; then
    log "Arquivo de configuração não encontrado: $CONFIG_FILE"
    exit 1
fi

# Se a porta não foi especificada via linha de comando, extrai do arquivo de configuração
if [ "$PORT" = "0" ]; then
    # Extrair a porta do arquivo de configuração - versão compatível com macOS
    CONFIG_PORT=$(grep -o '{port, *[0-9]*' "$CONFIG_FILE" | grep -o '[0-9]*' | head -1)
    if [ -n "$CONFIG_PORT" ]; then
        PORT="$CONFIG_PORT"
        log "Porta extraída do arquivo de configuração: $PORT"
    else
        PORT=8091  # Valor padrão se não conseguir extrair
        log "Não foi possível extrair a porta do arquivo de configuração. Usando a porta padrão: $PORT"
    fi
fi

# Verifica se a porta já está em uso
if lsof -i :"$PORT" >/dev/null; then
    log "ERRO: A porta $PORT já está em uso!"
    log "Use --port para especificar uma porta diferente ou encerre o processo usando a porta."
    log "Para verificar processos usando esta porta: lsof -i :$PORT"
    log "Para matar processos usando esta porta: kill \$(lsof -t -i :$PORT)"
    exit 1
fi

# Compila o projeto
cd "$PROJECT_DIR"
log "Compilando o projeto..."
rebar3 compile >> "$LOG_FILE" 2>&1 || {
    log "Falha na compilação. Verifique o log para detalhes."
    exit 1
}
log "Compilação concluída com sucesso."

# Configura opções para o Observer
OBSERVER_OPTS=""
if [ $OBSERVER -eq 1 ]; then
    log "Observer habilitado."
    log "Nome do nó: $NODE_NAME@$(hostname -s)"
    log "Cookie: web_server_cookie"
    
    # Configuração para o Observer
    OBSERVER_OPTS="-sname $NODE_NAME -setcookie web_server_cookie"
fi

# Verificar diretórios de bibliotecas
log "Verificando diretórios de bibliotecas..."
for dir in _build/default/lib/*/ebin; do
    log "  - $dir"
done

# Inicia o servidor
log "Iniciando servidor no ambiente $ENV na porta $PORT..."
if [ $OBSERVER -eq 1 ]; then
    # Com suporte ao Observer - modo interativo
    log "Iniciando em modo interativo com Observer..."
    # Redireciona saída padrão e erro para o arquivo de log, mantendo também no console
    erl -pa _build/default/lib/*/ebin \
        -config "$CONFIG_FILE" \
        $OBSERVER_OPTS \
        -eval "application:set_env(web_server, port, $PORT), application:ensure_all_started(web_server)." \
        2>&1 | tee -a "$LOG_FILE"
else
    # Criar um arquivo temporário para erros
    ERROR_FILE=$(mktemp)
    log "Arquivo temporário para erros: $ERROR_FILE"
    
    # Sem Observer - modo detached com saída redirecionada para arquivos
    log "Iniciando em modo detached..."
    erl -pa _build/default/lib/*/ebin \
        -config "$CONFIG_FILE" \
        -eval "application:set_env(web_server, port, $PORT), io:format(\"~p~n\", [application:ensure_all_started(web_server)])" \
        -noshell \
        -detached \
        > >(tee -a "$LOG_FILE") 2> >(tee "$ERROR_FILE" >&2)
    
    # Salva o código de saída
    ERL_EXIT_CODE=$?
    
    # Verifica se houve erros na inicialização
    if [ $ERL_EXIT_CODE -ne 0 ]; then
        log "ERRO: Falha ao iniciar o Erlang com código $ERL_EXIT_CODE"
        if [ -s "$ERROR_FILE" ]; then
            log "Detalhes do erro:"
            cat "$ERROR_FILE" | tee -a "$LOG_FILE"
        fi
        rm -f "$ERROR_FILE"
        exit 1
    fi
    
    # Verifica se o servidor iniciou
    sleep 2
    if pgrep -f "web_server" > /dev/null; then
        log "Servidor iniciado com sucesso na porta $PORT"
        
        # Verifica se a porta está realmente ouvindo
        if lsof -i :"$PORT" | grep LISTEN > /dev/null; then
            log "Porta $PORT está ouvindo corretamente"
        else
            log "AVISO: A porta $PORT não está ouvindo! Verifique a configuração do servidor."
        fi
        
        # Testa conexão HTTP
        log "Testando conexão HTTP na porta $PORT..."
        if curl -s --head http://localhost:$PORT > /dev/null; then
            log "Conexão HTTP funcionando corretamente"
        else
            CURL_EXIT=$?
            log "AVISO: Não foi possível conectar via HTTP na porta $PORT"
            log "  Código de erro curl: $CURL_EXIT"
        fi
        
        log "Log disponível em: $LOG_FILE"
        rm -f "$ERROR_FILE"
        exit 0
    else
        log "Falha ao iniciar o servidor"
        if [ -s "$ERROR_FILE" ]; then
            log "Detalhes do erro:"
            cat "$ERROR_FILE" | tee -a "$LOG_FILE"
        fi
        
        # Verificar mensagens de erro no arquivo de log do Erlang
        if [ -f "erl_crash.dump" ]; then
            log "Encontrado arquivo erl_crash.dump - indica crash do Erlang"
            log "Últimas 20 linhas do crash dump:"
            tail -20 "erl_crash.dump" | tee -a "$LOG_FILE"
        fi
        
        rm -f "$ERROR_FILE"
        exit 1
    fi
fi

# Limpar arquivo temporário no caso do modo Observer (o modo detached já limpa)
if [ $OBSERVER -eq 1 ]; then
    # O arquivo será apagado quando o script terminar 
    trap 'log "Servidor encerrado em $(date)"; log "Log disponível em: $LOG_FILE"' EXIT
    log "Servidor iniciado em modo interativo. Log disponível em: $LOG_FILE"
fi 