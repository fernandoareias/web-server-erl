%%%-------------------------------------------------------------------
%% @doc web-server-erl public API
%% @end
%%%-------------------------------------------------------------------

-module(web_server_erl_app).

-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
     io:format("[+][~p] - Initing web server.~n", [calendar:local_time()]),
    web_server_erl_sup:start_link().

stop(_State) ->
    io:format("[+][~p] - Stoping web server...~n", [calendar:local_time()]),
    ok.
 