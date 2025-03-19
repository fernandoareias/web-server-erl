-module(web_server_sup).
-author('Fernando Areias <nando.calheirosx@gmail.com>').
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    io:format("[+][~p][~p] - Starting supervisor...~n", [calendar:local_time(), self()]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % Obter porta da configuração usando get_env/2 e então manipular a resposta
    Port = case application:get_env(web_server, port) of
        {ok, ConfigPort} -> ConfigPort;
        undefined -> 8091  % Valor padrão se não estiver configurado
    end,
    io:format("[+][~p][~p] - Using port: ~p~n", [calendar:local_time(), self(), Port]),
    
    % Log detalhado sobre o ambiente
    io:format("[+][~p][~p] - DEBUG: Environment variables:~n", [calendar:local_time(), self()]),
    Env = application:get_all_env(web_server),
    [io:format("[+][~p][~p] -   ~p: ~p~n", [calendar:local_time(), self(), K, V]) || {K, V} <- Env],
    
    SupFlags = #{strategy => one_for_one, intensity => 100, period => 3600},
    ChildSpecs = [
        #{
            id => web_server_http_socket_writer,
            start => {web_server_http_socket_writer, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [web_server_http_socket_writer]
        }, 
        #{  
            id => web_server_http_parser,
            start => {web_server_http_parser, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [web_server_http_parser]
        },
        #{  
            id => metrics,
            start => {metrics, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [metrics]
        },
        #{  
            id => web_server_http_cache,
            start => {web_server_http_cache, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [web_server_http_cache]
        },
        #{  
            id => web_server_http_io,
            start => {web_server_http_io, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [web_server_http_io]
        },
        #{  
            id => web_server_http_get,
            start => {web_server_http_get, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [web_server_http_get]
        },
        #{
            id => web_server_tcp_listener,
            start => {web_server_tcp_listener, start_link,
                    [{local, http_server},  %% Name
                    web_server_tcp_acceptor,          %% Module -- acceptor
                    [{port, Port}],                   %% Args -- porta de escuta explícita
                    []                                %% Opts -- geralmente não usado
                    ]},
            restart => temporary,
            shutdown => 5000,
            type => worker,
            modules => [web_server_tcp_listener]
        }
        
    ],
    io:format("[+][~p][~p] - Supervisor initialized with ~p child processes~n", [calendar:local_time(), self(), length(ChildSpecs)]),
    {ok, {SupFlags, ChildSpecs}}.
