-module(web_server_tcp_acceptor).
-author('Fernando Areias <nando.calheirosx@gmail.com>').
-behaviour(gen_server).
-include("web_server_tcp_listener.hrl").

%% API
-export([behaviour_info/1]).
-export([start_link/0, start_link/3, accept/2]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record('$gen_tcp_acceptor_state', {
    module,
    client_socket,
    client_state
}).

%% =============================================================================
%% API
%% =============================================================================

%% -----------------------------------------------------------------------------
-spec behaviour_info(callbacks) -> list(tuple());
                  (_) -> undefined.
-spec start_link() -> {ok, pid()}.
-spec start_link(Mod :: atom(), Args :: args(),
           Options :: list(term())) -> {ok, pid()}.
-spec accept(ServerRef :: {local, string()} | {global, string()} | pid(),
    ClientSocket :: port()) -> ok.
-spec init(Args :: list(term())) ->
    Result :: {ok, #'$gen_tcp_acceptor_state'{}}.
-spec handle_cast({accept, ClientSocket :: port()},
    State :: #'$gen_tcp_acceptor_state'{}) ->
    {noreply, #'$gen_tcp_acceptor_state'{}}
    | {noreply, #'$gen_tcp_acceptor_state'{}, Timeout :: pos_integer()}
    | {noreply, #'$gen_tcp_acceptor_state'{}, hibernate}
    | {stop, Reason :: term(), #'$gen_tcp_acceptor_state'{}}.
-spec terminate(_, #'$gen_tcp_acceptor_state'{}) -> ok.
-spec code_change(_, State :: #'$gen_tcp_acceptor_state'{}, _) ->
    {ok, State :: #'$gen_tcp_acceptor_state'{}}.
-spec handle_info(Info :: term(), State :: #'$gen_tcp_acceptor_state'{}) ->
    Result :: {stop, {unknown_info, Info :: term()},
                State :: #'$gen_tcp_acceptor_state'{}}.
-spec handle_call(Request :: term(), _,
        State :: #'$gen_tcp_acceptor_state'{}) ->
        Result :: {stop, {unknown_call, Request :: term()},
        State :: #'$gen_tcp_acceptor_state'{}}.

%% @doc
%% The gen_tcp_acceptor behaviour.
%% @end
%% -----------------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{start_link, 0},
     {init, 1},
     {handle_accept, 2}];
behaviour_info(_) ->
    undefined.

%% -----------------------------------------------------------------------------
start_link() ->
    io:format("[+][~p][~p] - Starting tcp acceptor with default arguments...~n", [calendar:local_time(), self()]),
    start_link(web_server_tcp_acceptor, [], []).

%% @doc
%% Starts a new gen_tcp_acceptor as part of a supervisor tree.
%%
%% Mod is the behaviour implementation module, Args and Opts are passed to the
%% behaviour.
%% @end
%% -----------------------------------------------------------------------------
start_link(Mod, Args, Opts) ->
    io:format("[+][~p][~p] - Starting tcp acceptor...~n", [calendar:local_time(), self()]),          
    gen_server:start_link(?MODULE,
                          [{'$gen_tcp_acceptor_module', Mod} | Args], Opts).

%% -----------------------------------------------------------------------------
%% @doc
%% Makes the gen_tcp_acceptor accept incoming connections on ClientSocket.
%% @end
%% -----------------------------------------------------------------------------
accept(ServerRef, ClientSocket) ->
    io:format("[+][~p][~p] - Accept/2 | Server: ~p | Connection: ~p~n", [calendar:local_time(), self(), ServerRef, ClientSocket]),
    gen_server:cast(ServerRef, {accept, ClientSocket}).

%% =============================================================================
%% GEN_SERVER EXPORTS
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the gen_tcp_acceptor behaviour.
%% @end
%% -----------------------------------------------------------------------------
init(Args) ->
    process_flag(trap_exit, true),
    io:format("[+][~p][~p] - Initializing tcp acceptor, args = ~p~n", [calendar:local_time(), self(), Args]),
    {ok, #'$gen_tcp_acceptor_state'{module = web_server_http_parser}}.

%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Handles the asynchronous accept by calling the behaviour implementation
%% module's handle_accept function.
%% @end
%% -----------------------------------------------------------------------------
handle_cast({accept, ClientSocket},
State0 = #'$gen_tcp_acceptor_state'{module = _,
                                        client_state = ClientState}) ->    
    io:format("[+][~p][~p] - Handle_cast/2 accept tcp acceptor~n", [calendar:local_time(), self()]),
    State1 = State0#'$gen_tcp_acceptor_state'{client_socket = ClientSocket},
    try
        case inet:setopts(ClientSocket, [{active, once}]) of
            ok ->
                receive      
                    {tcp, _, Data} ->
                        io:format("[+][~p][~p] - Received message: ~s~n", [calendar:local_time(), self(), Data]),
                        gen_server:cast(metrics, {open_connection}),
                        web_server_http_parser:parse(Data, ClientSocket, self()),
                        {noreply, State1#'$gen_tcp_acceptor_state'{client_state = ClientState}}; 
                    {tcp_error, _, Reason} ->
                        io:format("[-][~p][~p] - TCP error: ~p~n", [calendar:local_time(), self(), Reason]),
                        gen_server:cast(metrics, {close_connection}),
                        {stop, {tcp_error, Reason}, State1};
                    {tcp_closed, _} ->
                        io:format("[-][~p][~p] - TCP connection closed by peer~n", [calendar:local_time(), self()]),
                        gen_server:cast(metrics, {close_connection}),
                        {stop, normal, State1}
                after 101000 -> % 101 second timeout
                    io:format("[-][~p][~p] - Connection timeout~n", [calendar:local_time(), self()]),
                    gen_server:cast(metrics, {close_connection}),
                    gen_tcp:close(ClientSocket),
                    {stop, normal, State1}
                end;
            {error, SetOptsError} ->
                io:format("[-][~p][~p] - Failed to set socket options: ~p~n", [calendar:local_time(), self(), SetOptsError]),
                gen_tcp:close(ClientSocket),
                {stop, {socket_error, SetOptsError}, State1}
        end
    catch
        _:Error:Stacktrace ->
            io:format("[-][~p][~p] - Unexpected error: ~p~nStacktrace: ~p~n", [calendar:local_time(), self(), Error, Stacktrace]),
            gen_server:cast(metrics, {close_connection}),
            gen_tcp:close(ClientSocket),
            {stop, {unexpected_error, Error}, State1}
    end.

%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Shutdown the behaviour and cleanup sockets that might not have been closed.
%% @end
%% -----------------------------------------------------------------------------
terminate(_, #'$gen_tcp_acceptor_state'{client_socket = ClientSocket}) ->
    io:format("[+][~p][~p] - Terminating tcp acceptor and closing client socket: ~p~n", [calendar:local_time(), self(), ClientSocket]),
    (catch gen_tcp:close(ClientSocket)),
    ok.

%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Handles upgrade of internal state upon code change.
%% @end
%% -----------------------------------------------------------------------------
code_change(_, State, _) -> 
    io:format("[+][~p][~p] - Performing code change...~n", [calendar:local_time(), self()]),
    {ok, State}.

%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Unhandled call.
%% @end
%% -----------------------------------------------------------------------------
handle_call(Request, _, State) ->
    io:format("[-][~p][~p] - Unhandled call: ~p~n", [calendar:local_time(), self(), Request]),
    {stop, {unknown_call, Request}, State}.

%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Unhandled info.
%% @end
%% -----------------------------------------------------------------------------
handle_info({'EXIT', Port, normal}, State = #'$gen_tcp_acceptor_state'{client_socket = Port}) ->
    io:format("[+][~p][~p] - Socket ~p closed normally.~n", [calendar:local_time(), self(), Port]),
    {stop, normal, State};
handle_info({'EXIT', Port, Reason}, State = #'$gen_tcp_acceptor_state'{client_socket = Port}) ->
    io:format("[-][~p][~p] - Socket ~p closed with reason: ~p~n", [calendar:local_time(), self(), Port, Reason]),
    {stop, {socket_closed, Reason}, State};
handle_info({connection_closed, Port}, State = #'$gen_tcp_acceptor_state'{client_socket = Port}) ->
    io:format("[+][~p][~p] - Connection closed by writer. Stopping acceptor.~n", [calendar:local_time(), self()]),
    {stop, normal, State}; 
handle_info(Info, State) ->
    io:format("[-][~p][~p] - Unhandled info: ~p~n", [calendar:local_time(), self(), Info]),
    {stop, {unknown_info, Info}, State}.