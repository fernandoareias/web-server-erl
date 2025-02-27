-module(web_server_tcp_listener).

-author('Fernando Areias <nando.calheirosx@gmail.com>').


-behaviour(gen_server).

-include("web_server_tcp_listener.hrl").



%% API
-export([start_link/3, start_link/4]).

%% gen_server exports
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).



%% =============================================================================
%% API
%% =============================================================================

%% -----------------------------------------------------------------------------
-spec start_link(Mod :: atom(), Args :: args(),
                 Options :: list(term())) -> {ok, pid()}.
-spec start_link(Name :: atom(), Mod :: atom(), Args :: args(),
                 Options :: list(term())) -> {ok, pid()}.

-spec init(Args :: args()) -> {ok, #listener_state{}} | {stop, term()}.

%% @doc
%% Starts the tcp_listener as part of a supervisor tree.
%%
%% Parses options from Args and uses defaults if an argument is not supplied.
%% @end
%% -----------------------------------------------------------------------------
start_link(Mod, Args, Opts) ->
    start_link({local, Mod}, Mod, Args, Opts).

start_link(Name, Mod, Args, Opts) ->
    io:format("[+][~p] - start_link/4 tcp listener...~n", [calendar:local_time()]),          
    supervisor:start_link(Name, web_server_tcp_listener_sup,
                          [{'$tcp_listener_server_ref', Name},
                           {'$tcp_listener_opts', Opts},
                           {'$tcp_listener_module', Mod}|Args]).



%% =============================================================================
%% GEN_SERVER 
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Creates listen socket and starts an asynchronous accept.
%% @end
%% -----------------------------------------------------------------------------
init(Args) ->
    %% Habilita a captura de sinais de saída para tratamento de erros.
    process_flag(trap_exit, true),
    
    %% Extrai os argumentos da lista de inicialização.
    Port         = proplists:get_value(port, Args),
    ListenSocket = proplists:get_value(listen_socket, Args),
    TcpOpts      = proplists:get_value(tcp_opts, Args, ?TCP_OPTS),
    Module       = proplists:get_value('$tcp_listener_module', Args),
    ServerRef    = proplists:get_value('$tcp_listener_server_ref', Args),

    %% Log de início da inicialização.
    io:format("[+][~p] - init/1 tcp listener...~n", [calendar:local_time()]),  
    io:format("[+][~p] - Listener configuration:~n", [calendar:local_time()]),
    io:format("         Port: ~p~n", [Port]),
    io:format("         ListenSocket: ~p~n", [ListenSocket]),
    io:format("         TcpOpts: ~p~n", [TcpOpts]),
    io:format("         Module: ~p~n", [Module]),
    io:format("         ServerRef: ~p~n", [ServerRef]), 

    %% Cria o estado inicial do listener.
    State0 = #listener_state{server_ref = ServerRef,
                             module     = Module},

    %% Log para indicar a criação do estado inicial.
    io:format("[+][~p] - Initial listener state created.~n", [calendar:local_time()]),

    %% Processa o caso baseado nos valores de Port e ListenSocket.
    case {Port, ListenSocket} of
        %% Caso 1: Um novo socket de escuta deve ser criado usando a porta fornecida.
        {Port, undefined} when is_integer(Port) ->
            io:format("[+][~p] - Creating new listen socket on port ~p with options ~p...~n", 
                      [calendar:local_time(), Port, TcpOpts]),
            do_listen(Port, TcpOpts, State0);

        %% Caso 2: Usa o socket de escuta fornecido.
        {undefined, ListenSocket} when is_port(ListenSocket) ->
            io:format("[+][~p] - Using supplied listen socket: ~p~n", 
                      [calendar:local_time(), ListenSocket]),
            State1 = State0#listener_state{listener = ListenSocket},
            io:format("[+][~p] - Asynchronous acceptor created for supplied listen socket.~n", 
                      [calendar:local_time()]),
            {ok, create_async_acceptor(State1)};

        %% Caso 3: Erro - Ambos Port e ListenSocket foram fornecidos.
        {Port, ListenSocket} when is_integer(Port), is_port(ListenSocket) ->
            io:format("[-][~p] - Error: Both port (~p) and listen socket (~p) were supplied.~n", 
                      [calendar:local_time(), Port, ListenSocket]),
            {stop, ambiguos_listen_socket};

        %% Caso 4: Nenhum Port ou ListenSocket foi fornecido. Usa a porta padrão.
        {undefined, undefined} ->
            io:format("[+][~p] - No port or listen socket supplied. Using default port ~p...~n", 
                      [calendar:local_time(), ?DEFAULT_PORT]),
            do_listen(?DEFAULT_PORT, TcpOpts, State0)
    end.



%% ----------------------------------------------------------------------------
-spec terminate(_, #listener_state{}) -> ok.
%% @private
%% @doc
%% Shutdown the server, close the listening socket.
%% @end
%% ----------------------------------------------------------------------------
terminate(_Reason, #listener_state{listener = ListenSocket}) ->
    (catch gen_tcp:close(ListenSocket)),
    ok.



%% ----------------------------------------------------------------------------
-spec code_change(_, State :: #listener_state{}, _) ->
          Result :: {ok, #listener_state{}}.
%% @private
%% @doc
%% Transforms servers internal state upon code change.
%% @end
%% ----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.



%% ----------------------------------------------------------------------------
-spec handle_cast({shutdown, Reason :: term()},
                  State :: #listener_state{}) -> ok.
%% @private
%% @doc
%% Handles asynchronous calls to tcp_listener.
%%
%% Messages handled are shutdown.
%% @end
%% ----------------------------------------------------------------------------
handle_cast({shutdown, Reason}, State) ->
    gen_server:terminate({shutdown, Reason}, State).



%% ----------------------------------------------------------------------------
-spec handle_info({inet_async, ListenSocket :: port(), Ref :: port(),
                   {ok, ClientSocket :: port()}},
                  State :: #listener_state{}) ->
          Result :: {noreply, #listener_state{}};
       ({inet_async, ListenSocket :: port(), Ref :: port(), Error :: term()},
                  State :: #listener_state{}) ->
          Result :: {stop, Error :: term(), State :: #listener_state{}};
      ({'EXIT', _Pid :: pid(), normal}, State :: #listener_state{}) ->
          Result :: {stop, normal, State :: #listener_state{}};
      (Info :: term(), State :: #listener_state{}) ->
          Result :: {stop, {unknown_info, Info :: term()},
                     State :: #listener_state{}}.
%% @private
%% @doc
%% Spawns a new connection handler (server) for accepted asynchronous
%% connection.
%% @end
%% ----------------------------------------------------------------------------
handle_info({inet_async, ListenSocket, Ref, {ok, ClientSocket}},
            State = #listener_state{listener   = ListenSocket,
                                    acceptor   = Ref,
                                    server_ref = ServerRef}) ->
    io:format("[+][~p] - handle_info/2: Received inet_async message with successful connection.~n", [calendar:local_time()]),
    try
        io:format("[+][~p] - Transferring socket options from ListenSocket to ClientSocket...~n", [calendar:local_time()]),
        case transfer_sockopt(ListenSocket, ClientSocket) of
            ok ->
                io:format("[+][~p] - Socket options transferred successfully.~n", [calendar:local_time()]);
            {error, Reason} ->
                io:format("[-][~p] - Failed to transfer socket options: ~p~n", [calendar:local_time(), Reason]),
                exit({transfer_sockopt, Reason})
        end,
        ?DEBUGP("handle (hand over) incoming connection~n"),
        io:format("[+][~p] - Starting new acceptor process under supervisor ~p...~n", [calendar:local_time(), ServerRef]),
        {ok, Pid} = web_server_tcp_listener_sup:start_acceptor(ServerRef),
        io:format("[+][~p] - New acceptor process started with PID: ~p~n", [calendar:local_time(), Pid]),
        io:format("[+][~p] - Accepting incoming connection on acceptor PID: ~p~n", [calendar:local_time(), Pid]),
        web_server_tcp_acceptor:accept(Pid, ClientSocket),
        io:format("[+][~p] - Setting controlling process for ClientSocket to PID: ~p~n", [calendar:local_time(), Pid]),
        ok = gen_tcp:controlling_process(ClientSocket, Pid),
        io:format("[+][~p] - Creating asynchronous acceptor for next connection...~n", [calendar:local_time()]),
        {noreply, create_async_acceptor(State)}
    catch
        exit:Error ->
            io:format("[-][~p] - Error in async accept: ~p~n", [calendar:local_time(), Error]),
            error_logger:error_msg("Error in async accept: ~p.~n", [Error]),
            {stop, Error, State}
    end;

handle_info({inet_async, ListenSocket, Ref, Error},
            State = #listener_state{listener = ListenSocket, acceptor = Ref}) ->
    io:format("[-][~p] - handle_info/2: Received inet_async message with error: ~p~n", [calendar:local_time(), Error]),
    error_logger:error_msg("Error in socket acceptor: ~p.~n", [Error]),
    {stop, Error, State};

handle_info({'EXIT', _Pid, normal}, State) ->
    io:format("[+][~p] - handle_info/2: Received normal EXIT signal. Stopping listener gracefully.~n", [calendar:local_time()]),
    {stop, normal, State};

handle_info(Info, State) ->
    io:format("[-][~p] - handle_info/2: Received unknown message: ~p~n", [calendar:local_time(), Info]),
    {stop, {unknown_info, Info}, State}.



%% ----------------------------------------------------------------------------
-spec handle_call(Request :: term(), _, State :: #listener_state{}) ->
          Result :: {stop,
                      {unknown_call, Request :: term()},
                     State :: #listener_state{}}.
%% @private
%% @doc
%% Unused gen_server function for handling synchronous calls.
%% @end
%% ----------------------------------------------------------------------------
handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.



%% ----------------------------------------------------------------------------
%% INTERNAL
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
-spec do_listen(Port :: integer, TcpOpts :: [atom() | {atom(), term()}],
                State0 :: #listener_state{}) ->
            Result :: {ok, #listener_state{}} | {stop, term()}.
%% @private
%% @doc
%% Create listen on port with TCP options.
%%
%% Chainloads from init/1 when port is supplied.
%% @end
%% ----------------------------------------------------------------------------
do_listen(Port, TcpOpts, State0) ->
    io:format("[+][~p] - Attempting to listen on port ~p with options: ~p~n", 
              [calendar:local_time(), Port, TcpOpts]),
    case gen_tcp:listen(Port, TcpOpts) of
        {ok, NewListenSocket} ->
            io:format("[+][~p] - Successfully created listen socket: ~p~n", 
                      [calendar:local_time(), NewListenSocket]),
            State1 = State0#listener_state{listener = NewListenSocket},
            io:format("[+][~p] - Updated listener state with new listen socket.~n", 
                      [calendar:local_time()]),
            io:format("[+][~p] - Creating asynchronous acceptor for new connections...~n", 
                      [calendar:local_time()]),
            {ok, create_async_acceptor(State1)};
        {error, Reason} ->
            io:format("[-][~p] - Failed to listen on port ~p. Reason: ~p~n", 
                      [calendar:local_time(), Port, Reason]),
            {stop, Reason}
    end.



%% ----------------------------------------------------------------------------
-spec create_async_acceptor(State :: #listener_state{}) ->
          Result :: #listener_state{}.
%% @private
%% @doc
%% Create asynchrounous acceptor.
%% @end
%% ----------------------------------------------------------------------------
create_async_acceptor(State = #listener_state{listener = ListenSocket}) ->
    io:format("[+][~p] - Creating new async acceptor...~n", [calendar:local_time()]),
    case prim_inet:async_accept(ListenSocket, -1) of
        {ok, NewRef} ->
            io:format("[+][~p] - Async acceptor created successfully with Ref: ~p~n", [calendar:local_time(), NewRef]),
            State#listener_state{acceptor = NewRef};
        {error, Reason} ->
            io:format("[-][~p] - Failed to create async acceptor: ~p~n", [calendar:local_time(), inet:format_error(Reason)]),
            exit({async_accept, inet:format_error(Reason)})
    end.



%% ----------------------------------------------------------------------------
-spec transfer_sockopt(ListenSocket :: port(), ClientSocket :: port()) ->
    ok | term().
%% @private
%% @doc
%% Transfer socket options from listen socket to client socket.
%% @end
%% ----------------------------------------------------------------------------
transfer_sockopt(ListenSocket, ClientSocket) ->
    io:format("[+][~p] - Transferring socket options from ListenSocket to ClientSocket...~n", [calendar:local_time()]),
    true = inet_db:register_socket(ClientSocket, inet_tcp),
    case prim_inet:getopts(ListenSocket, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, TcpOpts} ->
            io:format("[+][~p] - Retrieved socket options: ~p~n", [calendar:local_time(), TcpOpts]),
            case prim_inet:setopts(ClientSocket, TcpOpts) of
                ok ->
                    io:format("[+][~p] - Socket options transferred successfully.~n", [calendar:local_time()]),
                    ok;
                {error, SetOptsError} ->
                    io:format("[-][~p] - Failed to set socket options: ~p~n", [calendar:local_time(), SetOptsError]),
                    gen_tcp:close(ClientSocket),
                    {error, {setopts_failed, SetOptsError}}
            end;
        {error, GetOptsError} ->
            io:format("[-][~p] - Failed to retrieve socket options: ~p~n", [calendar:local_time(), GetOptsError]),
            gen_tcp:close(ClientSocket),
            {error, {getopts_failed, GetOptsError}}
    end.