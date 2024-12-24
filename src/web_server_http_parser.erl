-module(web_server_http_parser).

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

start_link() ->
    io:format("Starting http parser...~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> 
    gen_server:stop(?MODULE).   

init(_Args) -> 
    io:format("Request message consumer...~n"),
    gen_server:cast(web_server_request_queue, {subscribe, self()}),
    {ok, []}.

handle_call(alloc, _From, State) ->
    {reply, ok, State}.

handle_cast({consume, {Data, Connection}}, State) ->
    io:format("Consumed message: ~p from connection ~p~n", [Data, Connection]),
    {noreply, State};
handle_cast(_UnknownMessage, State) ->
    io:format("Unknown message in http parser: ~p~n", [_UnknownMessage]),
    {noreply, State}.
    

terminate(_Reason, _State) ->
    io:format("Http parser terminated.~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
