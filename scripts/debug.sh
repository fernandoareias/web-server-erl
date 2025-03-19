#!/bin/bash

# Diretório do projeto
PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Configura a porta
PORT=${1:-8091}

# Compila o projeto
cd "$PROJECT_DIR"
echo "Compilando..."
rebar3 compile

# Exibe um resumo das configurações
echo "=== DEBUG START ==="
echo "Porta: $PORT"
echo "Configuração: $PROJECT_DIR/config/dev.config"
echo "=================="

# Inicia o servidor em modo interativo (não-detached)
erl -pa _build/default/lib/*/ebin \
    -config "$PROJECT_DIR/config/dev.config" \
    -eval "io:format(\"Configurando porta: ~p~n\", [$PORT]), application:set_env(web_server, port, $PORT), {ok, Apps} = application:ensure_all_started(web_server), io:format(\"Apps iniciadas: ~p~n\", [Apps]), timer:sleep(1000), LPort = application:get_env(web_server, port, undefined), io:format(\"Porta configurada (depois): ~p~n\", [LPort])." 