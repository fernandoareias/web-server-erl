-module(metrics).
-behaviour(gen_server).

-export([start_link/0, increment_metric/1, get_metrics/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    io:format("[+][~p][~p] - Starting metrics processor~n", [calendar:local_time(), self()]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

increment_metric(Key) ->
    gen_server:cast(?MODULE, {increment, Key}).

get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

%% =============================================================================
%% GEN_SERVER CALLBACKS
%% =============================================================================

init([]) ->
    % Cria uma tabela ETS para armazenar as métricas
    ets:new(metrics_table, [named_table, public, {write_concurrency, true}]),
    ets:insert(metrics_table, {open_connection, 0}),
    ets:insert(metrics_table, {close_connection, 0}),
    ets:insert(metrics_table, {success_requests, 0}),
    ets:insert(metrics_table, {error_requests, 0}),
    ets:insert(metrics_table, {processing_requests, 0}),
    io:format("[+][~p][~p] - Metrics table initialized~n", [calendar:local_time(), self()]),
    {ok, #state{}}.

handle_cast({open_connection}, State) ->
    io:format("[-][~p][~p] - Enviando metrica 'open_connection'~n", [calendar:local_time(), self()]),
    case ets:lookup(metrics_table, open_connection) of
        [] -> ets:insert(metrics_table, {open_connection, 0});
        _ -> ok
    end,
    ets:update_counter(metrics_table, open_connection, 1),
    {noreply, State};

handle_cast({close_connection}, State) ->
    io:format("[-][~p][~p] - Enviando metrica 'close_connection'~n", [calendar:local_time(), self()]),
    case ets:lookup(metrics_table, open_connection) of
        [] -> ets:insert(metrics_table, {open_connection, 0});
        _ -> ok
    end,
    case ets:lookup(metrics_table, close_connection) of
        [] -> ets:insert(metrics_table, {close_connection, 0});
        _ -> ok
    end,
    CurrentOpen = ets:update_counter(metrics_table, open_connection, {2, -1, 0, 0}),
    if
        CurrentOpen >= 0 ->
            ets:update_counter(metrics_table, close_connection, 1);
        true ->
            io:format("[-][~p][~p] - Attempted to close more connections than open~n", [calendar:local_time(), self()])
    end,
    {noreply, State}.

handle_call(get_metrics, _From, State) ->
    Metrics = ets:tab2list(metrics_table),
    io:format("[+][~p][~p] - Retrieving metrics: ~p~n", [calendar:local_time(), self(), Metrics]),
    {reply, Metrics, State}.

handle_info(_Info, State) ->
    io:format("[-][~p][~p] - Received unknown info: ~p~n", [calendar:local_time(), self(), _Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[+][~p][~p] - Terminating metrics processor~n", [calendar:local_time(), self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:format("[+][~p][~p] - Performing code change...~n", [calendar:local_time(), self()]),
    {ok, State}.