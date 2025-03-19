%% DEBUG
%-define(DEBUG, true).
-ifdef(DEBUG).
-define(DEBUGP(Format), error_logger:info_msg("~w ~w (~w) " ++ Format,
                                              [self(), ?MODULE, ?LINE])).
-define(DEBUGP(Format, Args), error_logger:info_msg("~w ~w (~w) " ++ Format,
                                                    [self(), ?MODULE,
                                                     ?LINE|Args])).
-else.
-define(DEBUGP(_Format), true).
-define(DEBUGP(_Format, _Args), true).
-endif.


%% listener state
-record(listener_state, {
    server_ref :: atom(),
    module     :: atom(),
    listener   :: port() | undefined,
    acceptor   :: reference() | undefined
}).

%% listener defaults
-define(DEFAULT_PORT, 8000).
-define(TCP_OPTS, [
    binary,                  % Dados são tratados como binários
    {packet, raw},           % Sem processamento automático de pacotes
    {active, false},         % Modo passivo (não envia dados automaticamente ao processo)
    {reuseaddr, true},       % Permite reutilizar o endereço mesmo se estiver em estado TIME_WAIT
    {keepalive, true},       % Mantém a conexão ativa usando TCP keepalive
    {backlog, 128},          % Número máximo de conexões pendentes na fila
    {exit_on_close, true},   % Encerra o processo quando o socket é fechado
    {send_timeout, 5000}     % Define um timeout de 5 segundos para operações de envio
]).


%% listener types
-type arg() :: {port, pos_integer()}            % listener port
             | {tcp_opts, list(term())}.        % TCP options

-type args() :: list(arg()).


%% supervisor definitions
-define(SUP_MAX_RESTART, 5).
-define(SUP_MAX_TIME,   60).
-define(SUP_TIMEOUT,  2000).
