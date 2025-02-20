-module(web_server_http_get).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([handle_request/2]).

%%%===================================================================
%% Inicio gen_server
%%%===================================================================

start_link() ->
    io:format("[+][~p] - Starting Get Processor..~n", [calendar:local_time()]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    io:format("[+][~p] - Initialize Get Processor..~n", [calendar:local_time()]),
    {ok, []}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({handle_request, {Path, Connection}}, State) ->
    io:format("[+][~p] - Handling GET request: ~p~n", [calendar:local_time(), Path]),
    process_request(Path, Connection),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Fim gen_server
%%%===================================================================

%%%===================================================================
%% Interfaces
%%%===================================================================

handle_request(Path, Connection) ->
    gen_server:cast(?MODULE, {handle_request, {Path, Connection}}).

%%%===================================================================
%% Funcoes privadas
%%%===================================================================

process_request(Path, Connection) ->
    io:format("[+][~p] - Processing request: ~p with connection: ~p~n", [calendar:local_time(), Path, Connection]),
    case web_server_http_cache:get(Path) of
        {ok, {ContentType, Data}} ->
            io:format("[+][~p] - Cache hit for key: ~p~n", [calendar:local_time(), Path]),
            web_server_http_socket_writer:write_success_ok(Connection, ContentType, Data);
        {error, not_found} ->
            io:format("[+][~p] - Cache miss for key: ~p~n", [calendar:local_time(), Path]),
            web_server_http_io:read(Path, Connection)
    end.