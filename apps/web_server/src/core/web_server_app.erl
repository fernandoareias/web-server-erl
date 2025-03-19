%%%-------------------------------------------------------------------
%% @doc web_server public API
%% @end
%%%-------------------------------------------------------------------

-module(web_server_app).
-author('Fernando Areias <nando.calheirosx@gmail.com>').
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
     io:format("[+][~p][~p] - Initing web server.~n", [calendar:local_time(), self()]),
    web_server_sup:start_link().

stop(_State) ->
    io:format("[+][~p][~p] - Stoping web server...~n", [calendar:local_time(), self()]),
    ok.
 