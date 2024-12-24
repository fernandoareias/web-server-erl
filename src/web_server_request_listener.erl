-module(web_server_request_listener).

-behaviour(gen_server).

%%%===================================================================
%% API
%%%===================================================================
-export([start_link/0, stop/0]).
-export([init/2]). 
-export([init/1, handle_call/3, handle_cast/2]).
-define(SERVER, ?MODULE).

init(_GroupId, _Arg) -> {ok, []}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    io:format("Stopping web server...~n"),
    gen_server:stop(?SERVER).

init([]) ->
    io:format("Web server request processor listening...~n"),
    Result = gen_tcp:listen(8091, [{active, true}, binary]),
    start_request_consumer(Result).

start_request_consumer({ok, ListenSocket}) when is_port(ListenSocket) -> 
    io:format("Web server accepts messages now!~n"),
    loop(ListenSocket);

start_request_consumer({error, Reason}) when Reason =:= eaddrinuse ->
    io:format("Error: port already in use~n");

start_request_consumer({error, Reason}) when is_atom(Reason) ->
    io:format("Error starting web server: ~p~n", [Reason]),
    error;

start_request_consumer(_) ->
    io:format("Unhandled error while starting web server~n"),
    error.

loop(ListenSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    io:format("Connection accepted: ~p at ~p~n", [AcceptSocket, calendar:local_time()]),
    receive
        {tcp, AcceptSocket, Data} ->
            io:format("Received message: ~s~n", [Data]),
            io:format("Send data to event queue~n~n"),
            gen_server:cast(web_server_request_queue, {request_message, Data, AcceptSocket}),
            loop(ListenSocket);
        {tcp_closed, AcceptSocket} ->
            io:format("Connection closed: ~p~n", [AcceptSocket]),
            loop(ListenSocket);
        {tcp_error, AcceptSocket, Reason} ->
            io:format("Connection error: ~p~n", [Reason]),
            loop(ListenSocket)
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
    
handle_cast(_Request, State) ->
    {noreply, State}.
