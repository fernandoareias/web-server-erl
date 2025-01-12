-module(web_server_request_listener).

-behaviour(gen_server).

%%%===================================================================
%% API
%%%===================================================================
-export([start_link/0, stop/0]).
-export([init/2]). 
-export([init/1, handle_call/3, handle_cast/2]).
-define(SERVER, ?MODULE).
-define(PORT, 8091).

init(_GroupId, _Arg) -> {ok, []}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    io:format("[~p] - Stopping web server...~n", [calendar:local_time()]),
    gen_server:stop(?SERVER).

init([]) ->
    io:format("[~p] - Web server request processor listening in port ~p ...~n", [calendar:local_time(), ?PORT]),
    Result = gen_tcp:listen(?PORT, [{active, true}, binary]),
    start_request_consumer(Result).

start_request_consumer({ok, ListenSocket}) when is_port(ListenSocket) -> 
    io:format("[~p] - Web server accepts messages now!~n", [calendar:local_time()]),
    loop(ListenSocket);

start_request_consumer({error, Reason}) when Reason =:= eaddrinuse ->
    io:format("[~p] - Error: port already in use~n", [calendar:local_time()]);

start_request_consumer({error, Reason}) when is_atom(Reason) ->
    io:format("[~p] - Error starting web server: ~p~n", [calendar:local_time(), Reason]),
    error;

start_request_consumer(_) ->
    io:format("[~p] - Unhandled error while starting web server~n", [calendar:local_time()]),
    error.

loop(ListenSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    io:format("[~p] - Connection accepted: ~p ~n", [calendar:local_time(), AcceptSocket]),
    receive
        {tcp, AcceptSocket, Data} ->
            io:format("[~p] - Received message: ~s~n", [calendar:local_time(), Data]),
            io:format("[~p] - Send data to event queue~n~n", [calendar:local_time()]),
            gen_server:cast(web_server_request_queue, {request_message, Data, AcceptSocket}),
            loop(ListenSocket);
        {tcp_closed, AcceptSocket} ->
            io:format("[~p] - Connection closed: ~p~n", [calendar:local_time(), AcceptSocket]),
            loop(ListenSocket);
        {tcp_error, AcceptSocket, Reason} ->
            io:format("[~p] - Connection error: ~p~n", [calendar:local_time(), Reason]),
            loop(ListenSocket)
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
    
handle_cast(_Request, State) ->
    {noreply, State}.
