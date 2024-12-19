-module(web_server_request_processor).


-behaviour(gen_server).



%%%===================================================================
%% API
%%%===================================================================
-export([start_link/0]).
-export([init/2]). 
-export([init/1, handle_call/3, handle_cast/2]).
-define(SERVER, ?MODULE).

% -export([init/2, handle_message/4,handle_call/3, handle_cast/2]). 


init(_GroupId, _Arg) -> {ok, []}.

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    io:write("Web server request processor listenig.").


handle_call(_Request, _From, State) ->
    {reply, ok, State}.
    
    handle_cast(_Request, State) ->
    {noreply, State}.
    