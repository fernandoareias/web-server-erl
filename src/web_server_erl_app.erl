%%%-------------------------------------------------------------------
%% @doc web-server-erl public API
%% @end
%%%-------------------------------------------------------------------

-module(web_server_erl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    web_server_erl_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
