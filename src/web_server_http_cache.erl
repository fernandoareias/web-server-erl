-module(web_server_http_cache).
-behaviour(gen_server).
-author('Fernando Areias <nando.calheirosx@gmail.com>').
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get/1, set/3]).

-record(cache_map, {
    map = #{} :: map()
}).

%%%===================================================================
%% Inicio gen_server
%%%===================================================================

start_link() ->
    io:format("[+][~p][~p] - Starting Cache Processor...~n", [calendar:local_time(), self()]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    io:format("[+][~p][~p] - Initializing cache processor...~n", [calendar:local_time(), self()]),
    {ok, #cache_map{}}.

handle_call({get_cache, Key}, _From, State = #cache_map{map = Map}) ->
    case maps:find(Key, Map) of
        {ok, {ContentType, Data}} -> 
            io:format("[+][~p][~p] - Cache hit for key: ~p~n", [calendar:local_time(), self(), Key]),
            {reply, {ok, {ContentType, Data}}, State};
        error -> 
            io:format("[+][~p][~p] - Cache miss for key: ~p~n", [calendar:local_time(), self(), Key]),
            {reply, {error, not_found}, State}
    end;
handle_call(stop, _From, State) ->
    io:format("[+][~p][~p] - Stopping cache processor...~n", [calendar:local_time(), self()]),
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    io:format("[-][~p][~p] - Received unknown call: ~p~n", [calendar:local_time(), self(), _Request]),
    {reply, ok, State}.

handle_cast({set_cache, Key, ContentType, Data}, State = #cache_map{map = Map}) ->
    UpdatedMap = maps:put(Key, {ContentType, Data}, Map),
    io:format("[+][~p][~p] - Cache updated: ~p => {~p, ~p}~n", [calendar:local_time(), self(), Key, ContentType, Data]),
    {noreply, State#cache_map{map = UpdatedMap}};
handle_cast(_Msg, State) ->
    io:format("[-][~p][~p] - Received unknown cast: ~p~n", [calendar:local_time(), self(), _Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    io:format("[-][~p][~p] - Received unknown info: ~p~n", [calendar:local_time(), self(), _Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[+][~p][~p] - Terminating cache processor...~n", [calendar:local_time(), self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:format("[+][~p][~p] - Performing code change...~n", [calendar:local_time(), self()]),
    {ok, State}.

%%%===================================================================
%% Fim gen_server
%%%===================================================================

%%%===================================================================
%% Interfaces
%%%===================================================================

get(Key) ->
    gen_server:call(?MODULE, {get_cache, Key}).

set(Key, ContentType, Data) ->
    gen_server:cast(?MODULE, {set_cache, Key, ContentType, Data}).