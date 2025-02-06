-module(metrics).
-behaviour(gen_server).
-export([start_link/0, increment_metric/1, get_metrics/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
    io:format("[+][~p] - Startig metrics processor~n", [calendar:local_time()]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

increment_metric(Key) ->
    gen_server:cast(?MODULE, {increment, Key}).

get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

init([]) ->
    % Cria uma tabela ETS para armazenar as métricas
    ets:new(metrics_table, [named_table, public, {write_concurrency, true}]),
    ets:insert(metrics_table, {open_connection, 0}),
    ets:insert(metrics_table, {close_connection, 0}),

    ets:insert(metrics_table, {success_requests, 0}),
    ets:insert(metrics_table, {error_requests, 0}),
    ets:insert(metrics_table, {processing_requests, 0}),
    {ok, #state{}}.

handle_cast({open_connection}, State) ->
    io:format("[-][~p] - Enviando metrica 'open_connection'~n", [calendar:local_time()]),
    case ets:lookup(metrics_table, open_connection) of
        [] -> ets:insert(metrics_table, {open_connection, 0});
        _ -> ok
    end,
    ets:update_counter(metrics_table, open_connection, 1),
    {noreply, State};
    
handle_cast({close_connection}, State) ->
    io:format("[-][~p] - Enviando metrica 'close_connection'~n", [calendar:local_time()]),
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
            io:format("[-][~p] - Attempted to close more connections than open~n", [calendar:local_time()])
    end,
    {noreply, State}.
        
handle_call(get_metrics, _From, State) ->
    Metrics = ets:tab2list(metrics_table),
    {reply, Metrics, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.