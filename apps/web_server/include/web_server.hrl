%%% @doc
%%% Definições comuns para o servidor web.
%%% @end

%% Configurações padrão do servidor
-define(DEFAULT_PORT, 8091).
-define(DEFAULT_BACKLOG, 128).
-define(DEFAULT_TIMEOUT, 30000).  % 30 segundos

%% Opções TCP padrão
-define(TCP_OPTS, [
    binary,                  % Dados são tratados como binários
    {packet, raw},           % Sem processamento automático de pacotes
    {active, false},         % Modo passivo
    {reuseaddr, true},       % Permite reutilizar endereço
    {keepalive, true},       % Mantém conexão ativa
    {backlog, ?DEFAULT_BACKLOG},
    {send_timeout, ?DEFAULT_TIMEOUT},
    {send_timeout_close, true},
    {nodelay, true}          % Desativa algoritmo de Nagle
]).

%% Definições do supervisor
-define(SUP_MAX_RESTART, 5).
-define(SUP_MAX_TIME, 60).
-define(SUP_TIMEOUT, ?DEFAULT_TIMEOUT).

%% Estados do listener
-record(listener_state, {
    server_ref :: atom(),              % Referência do servidor
    module     :: atom(),              % Módulo de callback
    listener   :: port() | undefined,  % Socket de escuta
    acceptor   :: reference() | undefined  % Referência do acceptor
}).

%% Estados do acceptor
-record(acceptor_state, {
    module        :: atom(),              % Módulo de callback
    client_socket :: port() | undefined,  % Socket do cliente
    client_state  :: term()              % Estado do cliente
}).

%% Tipos comuns
-type http_method() :: get | post | put | delete | head | options.
-type http_version() :: {integer(), integer()}.
-type http_header() :: {string(), string()}.
-type http_headers() :: [http_header()].

%% Registro de tipos
-export_type([
    http_method/0,
    http_version/0,
    http_header/0,
    http_headers/0
]). 