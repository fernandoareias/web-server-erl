-module(web_server_request_listener).

-behaviour(gen_server).



%%%===================================================================
%% API
%%%===================================================================
-export([start_link/0, stop/0]).
-export([init/2]). 
-export([init/1, handle_call/3, handle_cast/2]).
-define(SERVER, ?MODULE).

% -export([init/2, handle_message/4,handle_call/3, handle_cast/2]). 


init(_GroupId, _Arg) -> {ok, []}.

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    io:format("Stoping web server..."),
    gen_server:stop(?SERVER).   

init([]) ->
    io:format("Web server request processor listening...~n"),

    case gen_tcp:listen(8091, [{active, true}, binary]) of 
        {ok, ListenSocket} -> 
            io:format("Web server accepts messages now!~n"),
            loop(ListenSocket);
        {error, eaddrinuse} -> 
            io:format("Error port already in use~n");
        _ ->
            io:format("Error when start web server~n"),
            error
    end.
    

loop(ListenSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    io:format("Connection accepted: ~p at ~p~n", [AcceptSocket, calendar:local_time()]),

    receive
        {tcp, AcceptSocket, Data} ->
            io:format("Received message: ~s~n", [Data]),
            io:format("Send data to event queue~n~n"),
            gen_server:cast(web_server_request_queue, {Data, AcceptSocket}),
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
    