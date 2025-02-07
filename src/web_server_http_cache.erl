-module(web_server_http_cache).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([get/1, put/2]).

-record(state, {data = #{} :: map()}).

start_link() ->
    io:format("[+][~p] - Starting Caching processor...~n", [calendar:local_time()]),    
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).   

init([]) ->
    io:format("[+][~p] - Initing Caching processor...~n", [calendar:local_time()]),
    {ok, #state{}}.


handle_call({get, Key}, _From, #state{data = Data} = State) ->
    Response = maps:get(Key, Data, not_found),
    {reply, Response, State}.
    

handle_cast({put, Key, Value}, #state{data = Data} = State) ->
    io:format("[+][~p] - Puting cache, key: ~p~n", [calendar:local_time(), Key]),
    NewData = maps:put(Key, Value, Data),
    {noreply, State#state{data = NewData}}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

put(Key, Value) ->
    gen_server:cast(?MODULE, {put, Key, Value}).
