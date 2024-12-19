-module(web_server_erl_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    io:fwrite("Starting supervisor...~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all, intensity => 1, period => 5},
    ChildSpecs = [
        % #{
        %     id => request_processor,
        %     start => {web_server_request_listener, start_link, []},
        %     restart => permanent,
        %     shutdown => 5000,
        %     type => worker,
        %     modules => [web_server_request_listener]
        % },
        #{
            id => request_queue,
            start => {web_server_request_queue, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [web_server_request_queue]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
