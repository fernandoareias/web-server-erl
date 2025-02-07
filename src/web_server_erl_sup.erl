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
            id => http_cachex, 
            start => {web_server_http_cache, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [web_server_http_cache]
        },
        #{
            id => http_log, 
            start => {web_server_log, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [web_server_log]
        },
        #{
            id => request_queue, % E um topic, tenho que atualizar o nome kkkkk
            start => {web_server_request_queue, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [web_server_request_queue]
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
        } 
        
    ],
    {ok, {SupFlags, ChildSpecs}}.
