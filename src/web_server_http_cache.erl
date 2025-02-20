-module(web_server_http_cache).
-behaviour(gen_server).

%% API
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
    io:format("[+][~p] - Starting Cache Processor...~n", [calendar:local_time()]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, #cache_map{}}.

handle_call({get_cache, Key}, _From, State = #cache_map{map = Map}) ->
    case maps:find(Key, Map) of
        {ok, {ContentType, Data}} -> 
            {reply, {ok, {ContentType, Data}}, State};
        error -> 
            {reply, {error, not_found}, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({set_cache, Key, ContentType, Data}, State = #cache_map{map = Map}) ->
    UpdatedMap = maps:put(Key, {ContentType, Data}, Map),
    io:format("[+][~p] - Cache updated: ~p => {~p, ~p}~n", [calendar:local_time(), Key, ContentType, Data]),
    {noreply, State#cache_map{map = UpdatedMap}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
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