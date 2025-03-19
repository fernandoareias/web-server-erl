%%%-------------------------------------------------------------------
%%% @doc
%%% Módulo responsável por gerenciar conexões TCP do servidor web.
%%% Implementa um servidor gen_server que aceita conexões TCP e delega
%%% o processamento para processos acceptor individuais.
%%% @end
%%%-------------------------------------------------------------------
-module(web_server_tcp_listener).
-author('Fernando Areias <nando.calheirosx@gmail.com>').

-behaviour(gen_server).

-include("web_server.hrl").

%% API Exports
-export([
    start_link/3,
    start_link/4,
    stop/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%====================================================================
%% Tipos e Registros
%%====================================================================

-type start_options() :: [
    {port, pos_integer()} |
    {tcp_opts, list()} |
    {listen_socket, port()}
].

-export_type([start_options/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc Inicia o listener TCP como parte da árvore de supervisão.
%% @equiv start_link({local, Mod}, Mod, Args, Opts)
-spec start_link(Mod :: atom(), Args :: list(), Opts :: start_options()) ->
    {ok, pid()} | {error, term()}.
start_link(Mod, Args, Opts) ->
    start_link({local, Mod}, Mod, Args, Opts).

%% @doc Inicia o listener TCP com um nome registrado.
%% @param Name Nome para registro do processo
%% @param Mod Módulo de callback
%% @param Args Argumentos de inicialização
%% @param Opts Opções de configuração
-spec start_link(Name :: atom(), Mod :: atom(), Args :: list(), Opts :: start_options()) ->
    {ok, pid()} | {error, term()}.
start_link(Name, Mod, Args, Opts) ->
    io:format("[+][~p][~p] - Iniciando TCP listener...~n", [calendar:local_time(), self()]),
    supervisor:start_link(Name, web_server_tcp_listener_sup, [
        {'$tcp_listener_server_ref', Name},
        {'$tcp_listener_opts', Opts},
        {'$tcp_listener_module', Mod} | Args
    ]).

%% @doc Para o listener TCP.
-spec stop(ServerRef :: atom()) -> ok.
stop(ServerRef) ->
    gen_server:cast(ServerRef, stop).

%%====================================================================
%% Callbacks gen_server
%%====================================================================

%% @private
%% @doc Inicializa o listener TCP.
init(Args) ->
    process_flag(trap_exit, true),
    Port = proplists:get_value(port, Args),
    ListenSocket = proplists:get_value(listen_socket, Args),
    TcpOpts = proplists:get_value(tcp_opts, Args, ?TCP_OPTS),
    Module = proplists:get_value('$tcp_listener_module', Args),
    ServerRef = proplists:get_value('$tcp_listener_server_ref', Args),

    log_init_info(Port, ListenSocket, TcpOpts, Module, ServerRef),
    
    State0 = #listener_state{server_ref = ServerRef, module = Module},
    
    case initialize_socket(Port, ListenSocket, TcpOpts, State0) of
        {ok, State1} -> {ok, create_async_acceptor(State1)};
        {error, Reason} -> {stop, Reason}
    end.

%% @private
%% @doc Manipula chamadas síncronas ao listener.
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
%% @doc Manipula chamadas assíncronas ao listener.
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc Manipula mensagens assíncronas do socket.
handle_info({inet_async, ListenSocket, Ref, {ok, ClientSocket}},
           State = #listener_state{listener = ListenSocket,
                                 acceptor = Ref,
                                 server_ref = ServerRef}) ->
    handle_client_connection(ClientSocket, ServerRef, State);

handle_info({inet_async, ListenSocket, Ref, Error},
           State = #listener_state{listener = ListenSocket, acceptor = Ref}) ->
    error_logger:error_msg("Erro no acceptor: ~p~n", [Error]),
    {stop, Error, State};

handle_info({'EXIT', _Pid, normal}, State) ->
    {stop, normal, State};

handle_info(Info, State) ->
    error_logger:warning_msg("Mensagem desconhecida: ~p~n", [Info]),
    {noreply, State}.

%% @private
%% @doc Limpa recursos ao parar o listener.
terminate(_Reason, #listener_state{listener = ListenSocket}) ->
    (catch gen_tcp:close(ListenSocket)),
    ok.

%% @private
%% @doc Atualiza o estado durante mudança de código.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Funções Internas
%%====================================================================

%% @private
%% @doc Inicializa o socket TCP com as opções apropriadas.
-spec initialize_socket(Port :: integer() | undefined,
                       ListenSocket :: port() | undefined,
                       TcpOpts :: list(),
                       State :: #listener_state{}) ->
    {ok, #listener_state{}} | {error, term()}.
initialize_socket(Port, undefined, TcpOpts, State) when is_integer(Port) ->
    case gen_tcp:listen(Port, TcpOpts) of
        {ok, LSock} -> {ok, State#listener_state{listener = LSock}};
        {error, Reason} -> {error, {listen_error, Reason}}
    end;
initialize_socket(undefined, LSock, _TcpOpts, State) when is_port(LSock) ->
    {ok, State#listener_state{listener = LSock}};
initialize_socket(undefined, undefined, TcpOpts, State) ->
    initialize_socket(?DEFAULT_PORT, undefined, TcpOpts, State);
initialize_socket(_, _, _, _) ->
    {error, ambiguous_socket_config}.

%% @private
%% @doc Cria um acceptor assíncrono para novas conexões.
-spec create_async_acceptor(State :: #listener_state{}) -> #listener_state{}.
create_async_acceptor(State = #listener_state{listener = LSock}) ->
    case prim_inet:async_accept(LSock, -1) of
        {ok, Ref} -> State#listener_state{acceptor = Ref};
        {error, Reason} -> exit({async_accept_error, Reason})
    end.

%% @private
%% @doc Manipula uma nova conexão de cliente.
-spec handle_client_connection(ClientSocket :: port(),
                             ServerRef :: atom(),
                             State :: #listener_state{}) ->
    {noreply, #listener_state{}}.
handle_client_connection(ClientSocket, ServerRef, State) ->
    case transfer_socket_control(ClientSocket, ServerRef) of
        ok -> {noreply, create_async_acceptor(State)};
        {error, Reason} -> {stop, Reason, State}
    end.

%% @private
%% @doc Transfere o controle do socket para um novo processo acceptor.
-spec transfer_socket_control(ClientSocket :: port(), ServerRef :: atom()) ->
    ok | {error, term()}.
transfer_socket_control(ClientSocket, ServerRef) ->
    case start_acceptor_process(ServerRef) of
        {ok, Pid} -> set_socket_control(ClientSocket, Pid);
        Error -> Error
    end.

%% @private
%% @doc Inicia um novo processo acceptor.
-spec start_acceptor_process(ServerRef :: atom()) ->
    {ok, pid()} | {error, term()}.
start_acceptor_process(ServerRef) ->
    SupName = acceptor_sup_name(ServerRef),
    supervisor:start_child(SupName, []).

%% @private
%% @doc Define o processo controlador do socket.
-spec set_socket_control(Socket :: port(), Pid :: pid()) ->
    ok | {error, term()}.
set_socket_control(Socket, Pid) ->
    case gen_tcp:controlling_process(Socket, Pid) of
        ok ->
            web_server_tcp_acceptor:accept(Pid, Socket);
        Error ->
            Error
    end.

%% @private
%% @doc Gera o nome do supervisor de acceptors.
-spec acceptor_sup_name(ServerRef :: atom() | {local, atom()}) -> atom().
acceptor_sup_name({local, Name}) when is_atom(Name) ->
    acceptor_sup_name(Name);
acceptor_sup_name(ServerRef) when is_atom(ServerRef) ->
    list_to_atom("web_server_tcp_listener_" ++
                 atom_to_list(ServerRef) ++
                 "_connection_sup").

%% @private
%% @doc Registra informações de inicialização.
-spec log_init_info(Port :: integer() | undefined,
                    ListenSocket :: port() | undefined,
                    TcpOpts :: list(),
                    Module :: atom(),
                    ServerRef :: atom()) -> ok.
log_init_info(Port, ListenSocket, TcpOpts, Module, ServerRef) ->
    io:format("[+][~p][~p] - Configuração do Listener:~n", [calendar:local_time(), self()]),
    io:format("[+][~p][~p] - Porta: ~p~n", [calendar:local_time(), self(), Port]),
    io:format("[+][~p][~p] - Socket: ~p~n", [calendar:local_time(), self(), ListenSocket]),
    io:format("[+][~p][~p] - Opções TCP: ~p~n", [calendar:local_time(), self(), TcpOpts]),
    io:format("[+][~p][~p] - Módulo: ~p~n", [calendar:local_time(), self(), Module]),
    io:format("[+][~p][~p] - Ref Servidor: ~p~n", [calendar:local_time(), self(), ServerRef]).