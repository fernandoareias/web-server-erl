#!/bin/bash

# Diretório do projeto
PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Detectar sistema operacional
OS="$(uname -s)"

# Função para formatar números
format_number() {
    printf "%'d" $1
}

# Função para obter métricas do servidor
get_metrics() {
    # Encontrar o PID do processo web_server
    local pid
    pid=$(pgrep -f "web_server")
    
    if [ -z "$pid" ]; then
        echo "Web server não está em execução."
        return 1
    fi

    # Métricas do processo
    local mem
    mem=$(ps -o rss= -p $pid)
    local cpu
    cpu=$(ps -o %cpu= -p $pid)
    local uptime
    uptime=$(ps -o etime= -p $pid)
    
    # Métricas de rede (assumindo porta 8091)
    local connections
    if [[ "$OS" == "Darwin" ]]; then
        # macOS
        connections=$(netstat -an | grep ":8091" | grep "ESTABLISHED" | wc -l | xargs)
    else
        # Linux
        connections=$(netstat -an | grep ":8091" | wc -l)
    fi
    
    # Métricas do sistema
    local system_mem
    local system_cpu
    
    if [[ "$OS" == "Darwin" ]]; then
        # macOS - Uso de memória
        local total_mem
        total_mem=$(sysctl -n hw.memsize)
        local page_size
        page_size=$(sysctl -n hw.pagesize)
        local free_pages
        free_pages=$(vm_stat | grep "Pages free:" | awk '{print $3}' | tr -d '.')
        local free_mem
        free_mem=$((free_pages * page_size))
        local used_percent
        used_percent=$(( 100 - (free_mem * 100) / total_mem ))
        system_mem=$used_percent
        
        # macOS - Uso de CPU
        system_cpu=$(top -l 1 | grep "CPU usage" | awk '{print $3}' | tr -d '%')
    else
        # Linux
        system_mem=$(free -m | awk '/Mem:/ {print int($3/$2 * 100)}')
        system_cpu=$(top -bn1 | grep "Cpu(s)" | awk '{print int($2)}')
    fi
    
    echo "=== Métricas do Servidor Web ==="
    echo "PID: $pid"
    echo "Uptime: $uptime"
    echo "Memória: $(format_number $mem) KB"
    echo "CPU: $cpu%"
    echo "Conexões: $(format_number $connections)"
    echo ""
    echo "=== Métricas do Sistema ==="
    echo "Uso de Memória: $system_mem%"
    echo "Uso de CPU: $system_cpu%"
}

# Função para exibir ajuda
show_help() {
    echo "Uso: $0 [opção]"
    echo "Opções:"
    echo "  -h, --help     Mostra esta ajuda"
    echo "  -w, --watch    Monitora continuamente (atualiza a cada 2 segundos)"
    echo "  -o, --once     Mostra métricas uma vez e sai"
}

# Processa argumentos
case "$1" in
    -h|--help)
        show_help
        exit 0
        ;;
    -w|--watch)
        # Verificar se o servidor está em execução antes de iniciar o modo watch
        if ! pgrep -f "web_server" > /dev/null; then
            echo "Erro: Web server não está em execução."
            echo "Inicie o servidor primeiro com 'make run' ou 'rebar3 shell'."
            exit 1
        fi
        
        echo "Iniciando monitoramento contínuo (Ctrl+C para sair)..."
        echo ""
        
        trap 'echo "Monitoramento encerrado"; exit 0' INT
        
        while true; do
            clear
            get_metrics || {
                echo "Servidor parou de responder. Monitoramento encerrado."
                exit 1
            }
            echo ""
            echo "Atualizando a cada 2 segundos. Pressione Ctrl+C para sair."
            sleep 2
        done
        ;;
    -o|--once|"")
        get_metrics || {
            echo "Erro ao obter métricas."
            exit 1
        }
        ;;
    *)
        echo "Opção inválida: $1"
        show_help
        exit 1
        ;;
esac

exit 0 