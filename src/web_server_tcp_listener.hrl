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
    binary,
    {packet, raw},
    {active, false},
    {reuseaddr, true},
    {keepalive, true},   
    {keepalive_idle, 10},   
    {keepalive_interval, 5},
    {backlog, 30},
    {exit_on_close, true},
    {send_timeout, 5000}   
]).


%% listener types
-type arg() :: {port, pos_integer()}            % listener port
             | {tcp_opts, list(term())}.        % TCP options

-type args() :: list(arg()).


%% supervisor definitions
-define(SUP_MAX_RESTART, 5).
-define(SUP_MAX_TIME,   60).
-define(SUP_TIMEOUT,  2000).
