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

-spec start_link() -> {ok, pid()}.
start_link() ->
    io:format("[+][~p] - Starting tcp acceptor with default arguments...~n", [calendar:local_time()]),
    start_link(web_server_tcp_acceptor, [], []).

-spec start_link(Mod :: atom(), Args :: args(),
           Options :: list(term())) -> {ok, pid()}.
%% @doc
%% Starts a new gen_tcp_acceptor as part of a supervisor tree.
%%
%% Mod is the behaviour implementation module, Args and Opts are passed to the
%% behaviour.
%% @end
%% -----------------------------------------------------------------------------
start_link(Mod, Args, Opts) ->
    io:format("[+][~p] - Starting tcp acceptor...~n", [calendar:local_time()]),          
    gen_server:start_link(?MODULE,
                          [{'$gen_tcp_acceptor_module', Mod} | Args], Opts).

%% -----------------------------------------------------------------------------
-spec accept(ServerRef :: {local, string()} | {global, string()} | pid(),
       ClientSocket :: port()) -> ok.
%% @doc
%% Makes the gen_tcp_acceptor accept incoming connections on ClientSocket.
%% @end
%% -----------------------------------------------------------------------------
accept(ServerRef, ClientSocket) ->
    io:format("[+][~p] - Accept/2 | Server: ~p | Connection: ~p~n", [calendar:local_time(), ServerRef, ClientSocket]),
    gen_server:cast(ServerRef, {accept, ClientSocket}).

%% =============================================================================
%% GEN_SERVER EXPORTS
%% =============================================================================
%% -----------------------------------------------------------------------------
-spec init(Args :: list(term())) ->
    Result :: {ok, #'$gen_tcp_acceptor_state'{}}.
%% @private
%% @doc
%% Initializes the gen_tcp_acceptor behaviour.
%% @end
%% -----------------------------------------------------------------------------
init(Args) ->
    process_flag(trap_exit, true),
    % io:format("[+][~p] - Init tcp acceptor~n", [calendar:local_time()]),
    % Module = proplists:get_value('$gen_tcp_acceptor_module', Args),
    % {ok, ClientState} = Module:init(Args),
    {ok, #'$gen_tcp_acceptor_state'{module = web_server_http_parser}}.

%% -----------------------------------------------------------------------------
-spec handle_cast({accept, ClientSocket :: port()},
                  State :: #'$gen_tcp_acceptor_state'{}) ->
    {noreply, #'$gen_tcp_acceptor_state'{}}
    | {noreply, #'$gen_tcp_acceptor_state'{}, Timeout :: pos_integer()}
    | {noreply, #'$gen_tcp_acceptor_state'{}, hibernate}
    | {stop, Reason :: term(), #'$gen_tcp_acceptor_state'{}}.

%% @private
%% @doc
%% Handles the asynchronous accept by calling the behaviour implementation
%% module's handle_accept function.
%% @end
%% -----------------------------------------------------------------------------
handle_cast({accept, ClientSocket},
State0 = #'$gen_tcp_acceptor_state'{module = Module,
                                        client_state = ClientState}) ->    
    io:format("[+][~p] - Handle_cast/2 accept tcp acceptor~n", [calendar:local_time()]),

    %% Atualiza o estado com o novo socket do cliente
    State1 = State0#'$gen_tcp_acceptor_state'{client_socket = ClientSocket},

    %% Configura o socket para modo ativo uma vez
    inet:setopts(ClientSocket, [{active, once}]),

    %% Processa a requisição recebida
    case receive_data(ClientSocket) of
        {ok, Data} ->
            io:format("[+][~p] - Received message: ~s~n", [calendar:local_time(), Data]),
            gen_server:cast(metrics, {open_connection}),
            web_server_http_parser:parse(Data, ClientSocket, self()),
            {noreply, State1#'$gen_tcp_acceptor_state'{client_state = ClientState}};
        {error, closed} ->
            io:format("[+][~p] - Connection closed: ~p~n", [calendar:local_time(), ClientSocket]),
            gen_server:cast(metrics, {close_connection}),
            {stop, normal, State1};
        {error, Reason} ->
            io:format("[-][~p] - Connection error: ~p~n", [calendar:local_time(), Reason]),
            gen_server:cast(metrics, {close_connection}),
            {stop, Reason, State1}
    end.

%% Função auxiliar para receber dados do socket
receive_data(Socket) ->
    receive
    {tcp, Socket, Data} ->
        {ok, Data};
    {tcp_closed, Socket} ->
        io:format("[+][~p] - Connection closed: ~p~n", [calendar:local_time(), Socket]),
        gen_server:cast(metrics, {close_connection}),
        {error, closed};
    {tcp_error, Socket, Reason} ->
        io:format("[-][~p] - Connection error: ~p~n", [calendar:local_time(), Reason]),
        gen_server:cast(metrics, {close_connection}),
        {error, Reason}
    after 5000 -> % Timeout de 5 segundos
        io:format("[-][~p] - Timeout while waiting for data~n", [calendar:local_time()]),
        {error, timeout}
    end.

%% -----------------------------------------------------------------------------
-spec terminate(_, #'$gen_tcp_acceptor_state'{}) -> ok.
%% @private
%% @doc
%% Shutdown the behaviour and cleanup sockets that might not have been closed.
%% @end
%% -----------------------------------------------------------------------------
terminate(_, #'$gen_tcp_acceptor_state'{client_socket = ClientSocket}) ->
    (catch gen_tcp:close(ClientSocket)),
    ok.

%% -----------------------------------------------------------------------------
-spec code_change(_, State :: #'$gen_tcp_acceptor_state'{}, _) ->
    {ok, State :: #'$gen_tcp_acceptor_state'{}}.
%% @private
%% @doc
%% Handles upgrade of internal state upon code change.
%% @end
%% -----------------------------------------------------------------------------
code_change(_, State, _) -> {ok, State}.

%% -----------------------------------------------------------------------------
-spec handle_call(Request :: term(), _,
            State :: #'$gen_tcp_acceptor_state'{}) ->
    Result :: {stop, {unknown_call, Request :: term()},
               State :: #'$gen_tcp_acceptor_state'{}}.
%% @private
%% @doc
%% Unhandled call.
%% @end
%% -----------------------------------------------------------------------------
handle_call(Request, _, State) ->
    {stop, {unknown_call, Request}, State}.

%% -----------------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: #'$gen_tcp_acceptor_state'{}) ->
    Result :: {stop, {unknown_info, Info :: term()},
               State :: #'$gen_tcp_acceptor_state'{}}.
%% @private
%% @doc
%% Unhandled info.
%% @end
%% -----------------------------------------------------------------------------
handle_info({'EXIT', Port, normal}, State = #'$gen_tcp_acceptor_state'{client_socket = Port}) ->
    io:format("[+][~p] - Socket ~p closed normally.~n", [calendar:local_time(), Port]),
    {stop, normal, State};
handle_info({'EXIT', Port, Reason}, State = #'$gen_tcp_acceptor_state'{client_socket = Port}) ->
    io:format("[-][~p] - Socket ~p closed with reason: ~p~n", [calendar:local_time(), Port, Reason]),
    {stop, {socket_closed, Reason}, State};
handle_info({connection_closed, Port}, State = #'$gen_tcp_acceptor_state'{client_socket = Port}) ->
    io:format("[+][~p] - Connection closed by writer. Stopping acceptor.~n", [calendar:local_time()]),
    gen_server:cast(metrics, {close_connection}),
    {stop, normal, State};
handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.