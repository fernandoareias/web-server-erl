-module(web_server_http_get).
-behaviour(gen_server).
-author('Fernando Areias <nando.calheirosx@gmail.com>').
%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([handle_request/3]).

%%%===================================================================
%% Inicio gen_server
%%%===================================================================

start_link() ->
    io:format("[+][~p][~p] - Starting Get Processor..~n", [calendar:local_time(), self()]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    io:format("[+][~p][~p] - Initialize Get Processor..~n", [calendar:local_time(), self()]),
    {ok, []}.

handle_call(stop, _From, State) ->
    io:format("[+][~p][~p] - Stopping Get Processor...~n", [calendar:local_time(), self()]),
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    io:format("[-][~p][~p] - Received unknown call: ~p~n", [calendar:local_time(), self(), _Request]),
    {reply, ok, State}.

handle_cast({handle_request, {Path, Connection, AcceptorPid}}, State) ->
    io:format("[+][~p][~p] - Handling GET request: ~p~n", [calendar:local_time(), self(), Path]),
    process_request(Path, Connection, AcceptorPid),
    {noreply, State};
handle_cast(_Msg, State) ->
    io:format("[-][~p][~p] - Received unknown cast: ~p~n", [calendar:local_time(), self(), _Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    io:format("[-][~p][~p] - Received unknown info: ~p~n", [calendar:local_time(), self(), _Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[+][~p][~p] - Terminating Get Processor...~n", [calendar:local_time(), self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:format("[+][~p][~p] - Performing code change...~n", [calendar:local_time(), self()]),
    {ok, State}.

%%%===================================================================
%% Fim gen_server
%%%===================================================================

%%%===================================================================
%% Interfaces
%%%===================================================================

handle_request(Path, Connection, AcceptorPid) ->
    gen_server:cast(?MODULE, {handle_request, {Path, Connection, AcceptorPid}}).

%%%===================================================================
%% Funcoes privadas
%%%===================================================================

process_request(Path, Connection, AcceptorPid) ->
    io:format("[+][~p][~p] - Processing request: ~p with connection: ~p~n", [calendar:local_time(), self(), Path, Connection]),
    case web_server_http_cache:get(Path) of
        {ok, {ContentType, Data}} ->
            io:format("[+][~p][~p] - Cache hit for path: ~p~n", [calendar:local_time(), self(), Path]),
            web_server_http_socket_writer:write_success_ok(Connection, ContentType, Data, AcceptorPid);
        {error, not_found} ->
            io:format("[+][~p][~p] - Cache miss for path: ~p~n", [calendar:local_time(), self(), Path]),
            web_server_http_io:read(Path, Connection, AcceptorPid)
    end.