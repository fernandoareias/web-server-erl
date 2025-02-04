-module(metrics).
-behaviour(gen_server).
-export([start_link/0, increment_metric/1, get_metrics/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
    io:format("[+][~p] - Iniciando metricas~n", [calendar:local_time()]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

increment_metric(Key) ->
    gen_server:cast(?MODULE, {increment, Key}).

get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

init([]) ->
    % Cria uma tabela ETS para armazenar as métricas
    ets:new(metrics_table, [named_table, public, set]),
    ets:insert(metrics_table, {open_connections, 0}),
    ets:insert(metrics_table, {success_requests, 0}),
    ets:insert(metrics_table, {error_requests, 0}),
    ets:insert(metrics_table, {processing_requests, 0}),
    {ok, #state{}}.

handle_cast({increment, Key}, State) ->
    ets:update_counter(metrics_table, Key, 1),
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