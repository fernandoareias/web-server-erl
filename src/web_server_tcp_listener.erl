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
    io:format("[+][~p][~p] - start_link/4 tcp listener...~n", [calendar:local_time(), self()]),          
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
    process_flag(trap_exit, true),
    Port         = proplists:get_value(port, Args),
    ListenSocket = proplists:get_value(listen_socket, Args),
    TcpOpts      = proplists:get_value(tcp_opts, Args, ?TCP_OPTS),
    Module       = proplists:get_value('$tcp_listener_module', Args),
    ServerRef    = proplists:get_value('$tcp_listener_server_ref', Args),

    io:format("[+][~p][~p] - Listener configuration:~n", [calendar:local_time(), self()]),
    io:format("[+][~p][~p] - Port: ~p~n", [calendar:local_time(), self(), Port]),
    io:format("[+][~p][~p] - ListenSocket: ~p~n", [calendar:local_time(), self(), ListenSocket]),
    io:format("[+][~p][~p] - TcpOpts: ~p~n", [calendar:local_time(), self(), TcpOpts]),
    io:format("[+][~p][~p] - Module: ~p~n", [calendar:local_time(), self(), Module]),
    io:format("[+][~p][~p] - ServerRef: ~p~n", [calendar:local_time(), self(), ServerRef]),

    State0 = #listener_state{server_ref = ServerRef, module = Module},
    io:format("[+][~p][~p] - Initial listener state created.~n", [calendar:local_time(), self()]),

    case {Port, ListenSocket} of
        {Port, undefined} when is_integer(Port) ->
            io:format("[+][~p][~p] - Creating new listen socket on port ~p with options ~p...~n", 
                      [calendar:local_time(), self(), Port, TcpOpts]),
            do_listen(Port, TcpOpts, State0);
        {undefined, ListenSocket} when is_port(ListenSocket) ->
            io:format("[+][~p][~p] - Using supplied listen socket: ~p~n", 
                      [calendar:local_time(), self(), ListenSocket]),
            State1 = State0#listener_state{listener = ListenSocket},
            io:format("[+][~p][~p] - Asynchronous acceptor created for supplied listen socket.~n", 
                      [calendar:local_time(), self()]),
            {ok, create_async_acceptor(State1)};
        {Port, ListenSocket} when is_integer(Port), is_port(ListenSocket) ->
            io:format("[-][~p][~p] - Error: Both port (~p) and listen socket (~p) were supplied.~n", 
                      [calendar:local_time(), self(), Port, ListenSocket]),
            {stop, ambiguos_listen_socket};
        {undefined, undefined} ->
            io:format("[+][~p][~p] - No port or listen socket supplied. Using default port ~p...~n", 
                      [calendar:local_time(), self(), ?DEFAULT_PORT]),
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
            State = #listener_state{listener = ListenSocket,
                                     acceptor = Ref,
                                     server_ref = ServerRef}) ->
    io:format("[+][~p][~p] - Received inet_async message with successful connection.~n", [calendar:local_time(), self()]),
    try
        io:format("[+][~p][~p] - Transferring socket options from ListenSocket to ClientSocket...~n", [calendar:local_time(), self()]),
        case transfer_sockopt(ListenSocket, ClientSocket) of
            ok ->
                io:format("[+][~p][~p] - Socket options transferred successfully.~n", [calendar:local_time(), self()]);
            {error, Reason} ->
                io:format("[-][~p][~p] - Failed to transfer socket options: ~p~n", [calendar:local_time(), self(), Reason]),
                exit({transfer_sockopt, Reason})
        end,
        io:format("[+][~p][~p] - Starting new acceptor process under supervisor ~p...~n", [calendar:local_time(), self(), ServerRef]),
        case web_server_tcp_listener_sup:start_acceptor(ServerRef) of
            {ok, Pid} ->
                io:format("[+][~p][~p] - New acceptor process started with PID: ~p~n", [calendar:local_time(), self(), Pid]),
                io:format("[+][~p][~p] - Accepting incoming connection on acceptor PID: ~p~n", [calendar:local_time(), self(), Pid]),
                web_server_tcp_acceptor:accept(Pid, ClientSocket),
                io:format("[+][~p][~p] - Setting controlling process for ClientSocket to PID: ~p~n", [calendar:local_time(), self(), Pid]),
                case gen_tcp:controlling_process(ClientSocket, Pid) of
                    ok ->
                        io:format("[+][~p][~p] - Creating asynchronous acceptor for next connection...~n", [calendar:local_time(), self()]),
                        {noreply, create_async_acceptor(State)};
                    {error, ControllingProcessError} ->
                        io:format("[-][~p][~p] - Failed to set controlling process: ~p~n", [calendar:local_time(), self(), ControllingProcessError]),
                        error_logger:error_msg("Failed to set controlling process: ~p.~n", [ControllingProcessError]),
                        {stop, ControllingProcessError, State}
                end;
            {error, StartAcceptorError} ->
                io:format("[-][~p][~p] - Failed to start acceptor process: ~p~n", [calendar:local_time(), self(), StartAcceptorError]),
                error_logger:error_msg("Failed to start acceptor process: ~p.~n", [StartAcceptorError]),
                {stop, StartAcceptorError, State};
            UnexpectedReturn ->
                io:format("[-][~p][~p] - Unexpected return from start_acceptor: ~p~n", [calendar:local_time(), self(), UnexpectedReturn]),
                error_logger:error_msg("Unexpected return from start_acceptor: ~p.~n", [UnexpectedReturn]),
                {stop, {unexpected_return, UnexpectedReturn}, State}
        end
    catch
        exit:Error ->
            io:format("[-][~p][~p] - Error in async accept: ~p~n", [calendar:local_time(), self(), Error]),
            error_logger:error_msg("Error in async accept: ~p.~n", [Error]),
            {stop, Error, State}
    end;

handle_info({inet_async, ListenSocket, Ref, Error},
            State = #listener_state{listener = ListenSocket, acceptor = Ref}) ->
    io:format("[-][~p][~p] - Received inet_async message with error: ~p~n", [calendar:local_time(), self(), Error]),
    error_logger:error_msg("Error in socket acceptor: ~p.~n", [Error]),
    {stop, Error, State};

handle_info({'EXIT', _Pid, normal}, State) ->
    io:format("[+][~p][~p] - Received normal EXIT signal. Stopping listener gracefully.~n", [calendar:local_time(), self()]),
    {stop, normal, State};

handle_info(Info, State) ->
    io:format("[-][~p][~p] - Received unknown message: ~p~n", [calendar:local_time(), self(), Info]),
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
    io:format("[+][~p][~p] - Attempting to listen on port ~p with options: ~p~n", 
              [calendar:local_time(), self(), Port, TcpOpts]),
    case gen_tcp:listen(Port, TcpOpts) of
        {ok, NewListenSocket} ->
            io:format("[+][~p][~p] - Successfully created listen socket: ~p~n", 
                      [calendar:local_time(), self(), NewListenSocket]),
            State1 = State0#listener_state{listener = NewListenSocket},
            io:format("[+][~p][~p] - Updated listener state with new listen socket.~n", 
                      [calendar:local_time(), self()]),
            io:format("[+][~p][~p] - Creating asynchronous acceptor for new connections...~n", 
                      [calendar:local_time(), self()]),
            {ok, create_async_acceptor(State1)};
        {error, Reason} ->
            io:format("[-][~p][~p] - Failed to listen on port ~p. Reason: ~p~n", 
                      [calendar:local_time(), self(), Port, Reason]),
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
    io:format("[+][~p][~p] - Creating new async acceptor...~n", [calendar:local_time(), self()]),
    case prim_inet:async_accept(ListenSocket, -1) of
        {ok, NewRef} ->
            io:format("[+][~p][~p] - Async acceptor created successfully with Ref: ~p~n", [calendar:local_time(), self(), NewRef]),
            State#listener_state{acceptor = NewRef};
        {error, Reason} ->
            io:format("[-][~p][~p] - Failed to create async acceptor: ~p~n", [calendar:local_time(), self(), inet:format_error(Reason)]),
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
    io:format("[+][~p][~p] - Transferring socket options from ListenSocket to ClientSocket...~n", [calendar:local_time(), self()]),
    true = inet_db:register_socket(ClientSocket, inet_tcp),
    case prim_inet:getopts(ListenSocket, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, TcpOpts} ->
            io:format("[+][~p][~p] - Retrieved socket options: ~p~n", [calendar:local_time(), self(), TcpOpts]),
            case prim_inet:setopts(ClientSocket, TcpOpts) of
                ok ->
                    io:format("[+][~p][~p] - Socket options transferred successfully.~n", [calendar:local_time(), self()]),
                    ok;
                {error, SetOptsError} ->
                    io:format("[-][~p][~p] - Failed to set socket options: ~p~n", [calendar:local_time(), self(), SetOptsError]),
                    gen_tcp:close(ClientSocket),
                    {error, {setopts_failed, SetOptsError}}
            end;
        {error, GetOptsError} ->
            io:format("[-][~p][~p] - Failed to retrieve socket options: ~p~n", [calendar:local_time(), self(), GetOptsError]),
            gen_tcp:close(ClientSocket),
            {error, {getopts_failed, GetOptsError}}
    end.