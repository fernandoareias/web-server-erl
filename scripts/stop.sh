#!/bin/bash

# Encontra o PID do processo do servidor
PID=$(pgrep -f "web_server")

if [ -z "$PID" ]; then
    echo "Servidor não está em execução"
    exit 0
fi

# Tenta parar o servidor graciosamente
echo "Parando servidor (PID: $PID)..."
kill -TERM $PID

# Aguarda até 10 segundos pelo encerramento
for i in {1..10}; do
    if ! pgrep -f "web_server" > /dev/null; then
        echo "Servidor parado com sucesso"
        exit 0
    fi
    sleep 1
done

# Se ainda estiver rodando, força o encerramento
if pgrep -f "web_server" > /dev/null; then
    echo "Forçando encerramento do servidor..."
    kill -9 $PID
    sleep 1
    
    if pgrep -f "web_server" > /dev/null; then
        echo "Falha ao parar o servidor"
        exit 1
    else
        echo "Servidor forçadamente encerrado"
        exit 0
    fi
fi 