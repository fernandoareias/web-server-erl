-module(web_server_erl_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    io:fwrite("Starting supervisor...~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => rest_for_one, intensity => 100, period => 3600},
    ChildSpecs = [
        #{
            id => request_queue,
            start => {web_server_request_queue, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [web_server_request_queue]
        },
        #{
            id => http_parser,
            start => {web_server_http_parser, start_link, []},
            restart => permanent,
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
