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
            id => http_parser_queue,  
            start => {queue_processor, start_link, [http_parser_queue]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [queue_processor]
        },
        #{
            id => http_parser,
            start => {web_server_http_parser, start_link, [processor1]},
            shutdown => 5000,
            type => worker,
            modules => [web_server_http_parser]
        },    
        #{
            id => http_parser2,
            start => {web_server_http_parser, start_link, [processor2]},
            shutdown => 5000,
            type => worker,
            modules => [web_server_http_parser]
        },   
        #{
            id => http_parser3,
            start => {web_server_http_parser, start_link, [processor3]},
            shutdown => 5000,
            type => worker,
            modules => [web_server_http_parser]
        },   
        #{
            id => http_parser4,
            start => {web_server_http_parser, start_link, [processor4]},
            shutdown => 5000,
            type => worker,
            modules => [web_server_http_parser]
        }, 
        #{
            id => http_parser5,
            start => {web_server_http_parser, start_link, [processor5]},
            shutdown => 5000,
            type => worker,
            modules => [web_server_http_parser]
        },     
        #{
            id => request_listener,
            start => {web_server_request_listener, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [web_server_request_listener]
        } 
        
    ],
    {ok, {SupFlags, ChildSpecs}}.
