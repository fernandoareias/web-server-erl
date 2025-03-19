# API do Servidor Web

## Módulos Públicos

### web_server
Interface principal do servidor.

```erlang
%% Inicia o servidor web
-spec start() -> {ok, pid()} | {error, term()}.

%% Para o servidor web
-spec stop() -> ok | {error, term()}.

%% Inicia o servidor com opções customizadas
-spec start_link(Options :: map()) -> {ok, pid()} | {error, term()}.
```

### web_server_tcp_listener
Gerenciamento de conexões TCP.

```erlang
%% Inicia o listener TCP
-spec start_link(Port :: integer(), Options :: map()) -> {ok, pid()} | {error, term()}.

%% Para o listener TCP
-spec stop(ServerRef :: pid()) -> ok | {error, term()}.
```

### web_server_http_handler
Manipulação de requisições HTTP.

```erlang
%% Adiciona uma rota
-spec add_route(Method :: http_method(), Path :: string(), Handler :: handler_fun()) -> ok | {error, term()}.

%% Remove uma rota
-spec remove_route(Method :: http_method(), Path :: string()) -> ok | {error, term()}.
```

## Tipos de Dados

### http_method()
```erlang
-type http_method() :: 'GET' | 'POST' | 'PUT' | 'DELETE' | 'HEAD' | 'OPTIONS'.
```

### handler_fun()
```erlang
-type handler_fun() :: fun((Request :: #http_request{}) -> Response :: #http_response{}).
```

### http_request
```erlang
-record(http_request, {
    method :: http_method(),
    path :: string(),
    version :: string(),
    headers :: [{string(), string()}],
    body :: binary()
}).
```

### http_response
```erlang
-record(http_response, {
    status :: integer(),
    headers :: [{string(), string()}],
    body :: binary()
}).
```

## Exemplos de Uso

### Iniciando o Servidor
```erlang
% Inicia com configurações padrão
{ok, Pid} = web_server:start().

% Inicia com opções customizadas
Options = #{
    port => 8080,
    backlog => 1024,
    timeout => 5000
},
{ok, Pid} = web_server:start_link(Options).
```

### Adicionando Rotas
```erlang
% Handler simples
Handler = fun(Request) ->
    #http_response{
        status = 200,
        headers = [{"Content-Type", "text/plain"}],
        body = <<"Hello, World!">>
    }
end,

% Adiciona rota GET
ok = web_server_http_handler:add_route('GET', "/hello", Handler).
```

### Usando o Cache
```erlang
% Adiciona item ao cache
ok = web_server_cache:put(Key, Value, TTL).

% Recupera item do cache
{ok, Value} = web_server_cache:get(Key).
```

### Métricas
```erlang
% Obtém métricas do servidor
Metrics = web_server_metrics:get_metrics().

% Obtém métrica específica
{ok, Value} = web_server_metrics:get_metric(connections_active).
```

## Códigos de Status

O servidor retorna os seguintes códigos de status HTTP:

- 200: OK
- 201: Created
- 204: No Content
- 400: Bad Request
- 401: Unauthorized
- 403: Forbidden
- 404: Not Found
- 405: Method Not Allowed
- 500: Internal Server Error
- 503: Service Unavailable

## Headers Suportados

- Content-Type
- Content-Length
- Connection
- Keep-Alive
- Accept-Encoding
- Content-Encoding
- Cache-Control
- ETag
- Last-Modified
- If-Modified-Since
- If-None-Match

## Limitações

- Tamanho máximo do corpo da requisição: 10MB
- Número máximo de headers: 100
- Tamanho máximo do path: 2048 bytes
- Timeout padrão de conexão: 30 segundos 