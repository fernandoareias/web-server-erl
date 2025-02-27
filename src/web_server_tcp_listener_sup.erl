-module(web_server_tcp_listener_sup).
-author('Fernando Areias <nando.calheirosx@gmail.com>').

-behaviour(supervisor).
-include("web_server_tcp_listener.hrl").

%% supervisor export
-export([init/1]).
%% external exports
-export([start_acceptor/1]).

%% types
-type child() :: pid() | undefined.
-type error() :: already_present | {already_started, child()} | term().

%% ----------------------------------------------------------------------------
%%
%% SUPERVISOR EXPORTS
%%
%% ----------------------------------------------------------------------------
-spec init(Args :: args()) -> Result :: term().
%% @doc
%% Initializes the supervisor with child specifications.
%% @end
%% ----------------------------------------------------------------------------
init(Args) ->
    io:format("[+][~p] - Initing supervisor tcp listener, args = ~p~n", [calendar:local_time(), Args]),
    Supervisor =
        proplists:get_value('$tcp_listener_supervisor', Args,
                            web_server_tcp_listener_sup),
    io:format("[+][~p] - Supervisor type: ~p~n", [calendar:local_time(), Supervisor]),
    init(Supervisor, Args).

%% ----------------------------------------------------------------------------
-spec init(tcp_listener | web_server_tcp_listener_connection_sup,
           Args :: args()) -> Result :: term();
      (web_server_tcp_listener_connection_sup, Args :: args()) ->
            Result :: term().
%% @doc
%% Supervisor definitions.
%%
%% The tcp_listener is supervised by a one_for_one supervisor.
%%
%% Each connection is started dynamically by a simple_one_for_one supervisor.
%% @end
%% ----------------------------------------------------------------------------
init(web_server_tcp_listener_sup, Args) ->
    io:format("[+][~p] - Init/2 supervisor tcp listener...~n", [calendar:local_time()]),
    
    %% Build tcp_listener start arguments
    {Registry, SuppliedName} =
        proplists:get_value('$tcp_listener_server_ref', Args),
    Name = list_to_atom("web_server_tcp_listener_" ++ atom_to_list(SuppliedName)),
    ServerName = {Registry, Name},
    Opts = proplists:get_value('$tcp_listener_opts', Args, []),
    io:format("[+][~p] - Listener server name: ~p, options: ~p~n", [calendar:local_time(), ServerName, Opts]),
    
    %% If Name is empty start_link/3 is called, otherwise start_link/4
    ListenerArgs = [ServerName, web_server_tcp_listener, Args, Opts],
    ListenerSupName = list_to_atom("web_server_tcp_listener_" ++
                                   atom_to_list(SuppliedName) ++ "_sup"),
    ConnectionSupName = list_to_atom("web_server_tcp_listener_" ++
                                     atom_to_list(SuppliedName) ++
                                     "_connection_sup"),
    io:format("[+][~p] - Listener supervisor name: ~p~n", [calendar:local_time(), ListenerSupName]),
    io:format("[+][~p] - Connection supervisor name: ~p~n", [calendar:local_time(), ConnectionSupName]),
    
    {ok, {{one_for_one, ?SUP_MAX_RESTART, ?SUP_MAX_TIME},
          %% TCP listener supervisor
          [{ListenerSupName,
            {gen_server, start_link, ListenerArgs},
            permanent,
            ?SUP_TIMEOUT,
            worker,
            [web_server_tcp_listener]},
          %% TCP connections supervisor
           {ConnectionSupName,
            {supervisor, start_link,
             [{local, ConnectionSupName}, ?MODULE,
              [{'$tcp_listener_supervisor',
                web_server_tcp_listener_connection_sup}|Args]]},
            permanent,
            infinity,
            supervisor,
            []}
          ]}};

init(web_server_tcp_listener_connection_sup, Args) ->
    Module = proplists:get_value('$tcp_listener_module', Args),
    io:format("[+][~p] - Tcp listener connection sup module ~p...~n", [calendar:local_time(), Module]),
    io:format("[+][~p] - Initializing simple_one_for_one supervisor for connections...~n", [calendar:local_time()]),
    {ok, {{simple_one_for_one, ?SUP_MAX_RESTART, ?SUP_MAX_TIME},
          [{undefined,
            {Module, start_link, []},
            temporary,
            ?SUP_TIMEOUT,
            worker,
            [Module]}
          ]}}.

%% ----------------------------------------------------------------------------
%%
%% EXTERNAL EXPORTS
%%
%% ----------------------------------------------------------------------------
-spec start_acceptor(Args :: args()) -> Result :: {ok, child()} | error().
%% @doc
%% Starts an acceptor for a new connection.
%% @end
%% ----------------------------------------------------------------------------
start_acceptor({_Registry, Name}) ->
    io:format("[+][~p] - Start client ~p...~n", [calendar:local_time(), Name]),
    ConnectionSupName = list_to_atom("web_server_tcp_listener_" ++
                                     atom_to_list(Name) ++
                                     "_connection_sup"),
    io:format("[+][~p] - Starting child under connection supervisor: ~p~n", [calendar:local_time(), ConnectionSupName]),
    {ok, _Pid} = supervisor:start_child(ConnectionSupName, []).