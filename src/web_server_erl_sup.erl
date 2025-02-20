-module(web_server_erl_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    io:format("[+][~p] - Starting supervisor...~n", [calendar:local_time()]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 100, period => 3600},
    ChildSpecs = [
        #{
            id => metrics, 
            start => {metrics, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [metrics]
        },
        #{
            id => http_parser,
            start => {web_server_http_parser, start_link, []},
            shutdown => 5000,
            type => worker,
            modules => [web_server_http_parser]
        },     
        #{
            id => request_processor,
            start => {web_server_request_listener, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [web_server_request_listener]
        },
        #{
            id => http_get,
            start => {web_server_http_get, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [web_server_http_get]
        },
        #{
            id => http_cache,
            start => {web_server_http_cache, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [web_server_http_cache]
        },
        #{
            id => http_io,
            start => {web_server_http_io, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [web_server_http_io]
        },
        #{
            id => socket_writer,
            start => {web_server_http_socket_writer, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [web_server_http_socket_writer]
        }        
    ],
    io:format("[+][~p] - Supervisor initialized with ~p child processes~n", [calendar:local_time(), length(ChildSpecs)]),
    {ok, {SupFlags, ChildSpecs}}.
