# Arquitetura do Servidor Web

## Visão Geral

O servidor web é construído usando uma arquitetura baseada em processos Erlang, seguindo o padrão de supervisão OTP. A arquitetura é dividida em várias camadas, cada uma com responsabilidades específicas.

## Componentes Principais

### 1. Camada TCP
- **web_server_tcp_listener**: Gerencia conexões TCP e aceita novas conexões
- **web_server_tcp_acceptor**: Processa conexões individuais de forma não-bloqueante
- **web_server_tcp_listener_sup**: Supervisiona os processos de listener e acceptor

### 2. Camada HTTP
- **web_server_http_parser**: Parse de requisições HTTP
- **web_server_http_handler**: Manipulação de requisições HTTP
- **web_server_http_router**: Roteamento de requisições
- **web_server_http_response**: Geração de respostas HTTP

### 3. Camada de Cache
- **web_server_cache**: Cache de arquivos estáticos
- **web_server_cache_sup**: Supervisor do sistema de cache

### 4. Camada de Métricas
- **web_server_metrics**: Coleta e exposição de métricas
- **web_server_metrics_sup**: Supervisor do sistema de métricas

## Fluxo de Dados

1. O `tcp_listener` aceita novas conexões TCP
2. Um novo processo `tcp_acceptor` é criado para cada conexão
3. O `http_parser` processa a requisição HTTP
4. O `http_router` direciona a requisição para o handler apropriado
5. O `http_handler` processa a requisição e gera uma resposta
6. A resposta é enviada de volta através do socket TCP

## Supervisão

```
web_server_sup
├── web_server_tcp_listener_sup
│   ├── web_server_tcp_listener
│   └── web_server_tcp_acceptor_sup
│       └── web_server_tcp_acceptor (dinâmico)
├── web_server_cache_sup
│   └── web_server_cache
└── web_server_metrics_sup
    └── web_server_metrics
```

## Configuração

O sistema usa diferentes configurações para diferentes ambientes:
- `dev.config`: Desenvolvimento local
- `prod.config`: Produção
- `test.config`: Testes automatizados

## Performance

- Processamento não-bloqueante de requisições
- Pool de processos acceptor
- Cache de arquivos estáticos
- Compressão gzip
- Keep-alive connections
- Timeouts configuráveis

## Segurança

- Suporte a SSL/TLS
- Proteção contra buffer overflow
- Timeouts para prevenir ataques DoS
- Validação de headers HTTP
- Sanitização de paths

## Monitoramento

O sistema expõe métricas em tempo real:
- Conexões ativas
- Requisições por segundo
- Latência
- Uso de memória
- Cache hits/misses 