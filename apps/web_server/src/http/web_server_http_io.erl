-module(web_server_http_io).
-behaviour(gen_server).
-author('Fernando Areias <nando.calheirosx@gmail.com>').
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([read/3]).

%%%===================================================================
%% Inicio gen_server
%%%===================================================================

start_link() ->
    io:format("[+][~p][~p] - Starting File IO...~n", [calendar:local_time(), self()]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->     
    {ok, []}.

handle_call(stop, _From, State) ->
    io:format("[+][~p][~p] - Stopping File IO...~n", [calendar:local_time(), self()]),
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    io:format("[-][~p][~p] - Received unknown call: ~p~n", [calendar:local_time(), self(), _Request]),
    {reply, ok, State}.

handle_cast({Path, Connection, AcceptorPid}, State) ->
    io:format("[+][~p][~p] - Handling file read request for path: ~p~n", [calendar:local_time(), self(), Path]),
    read_file(Path, Connection, AcceptorPid),
    {noreply, State}.

handle_info(_Info, State) ->
    io:format("[-][~p][~p] - Received unknown info: ~p~n", [calendar:local_time(), self(), _Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[+][~p][~p] - Terminating File IO...~n", [calendar:local_time(), self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:format("[+][~p][~p] - Performing code change...~n", [calendar:local_time(), self()]),
    {ok, State}.

%%%===================================================================
%% Interface
%%%===================================================================

read(Path, Connection, AcceptorPid) ->
    gen_server:cast(?MODULE, {Path, Connection, AcceptorPid}).

%%%===================================================================
%% Funcao privada
%%%===================================================================

read_file(Path, Connection, AcceptorPid) -> 
    io:format("[+][~p][~p] - Reading file: ~p~n", [calendar:local_time(), self(), Path]),
    % FilePath = "priv/www" ++ binary_to_list(Path),
    FilePath = "/Users/fernandoareias/Documents/dev/web-server-erl/apps/web_server/priv/www/index.html",
    io:format("[+][~p][~p] - Full file path: ~p~n", [calendar:local_time(), self(), FilePath]),
    handle_read(file:read_file(FilePath), content_type(Path), Path, Connection, AcceptorPid).

handle_read({ok, Content}, ContentType, Path, Connection, AcceptorPid) -> 
    io:format("[+][~p][~p] - File read success~n", [calendar:local_time(), self()]),
    web_server_http_cache:set(Path, ContentType, Content),
    web_server_http_socket_writer:write_success_ok(Connection, ContentType, Content, AcceptorPid);
handle_read({error, Reason}, _, Path, Connection, AcceptorPid) ->     
    io:format("[-][~p][~p] - File not found: ~p, Reason: ~p~n", [calendar:local_time(), self(), Path, Reason]),
    web_server_http_socket_writer:write_not_found(Connection, AcceptorPid).

content_type(Path) ->
    StringPath = binary_to_list(Path),
    FileExtension = filename:extension(StringPath),
    case FileExtension of
        ".html" -> "text/html";
        ".css"  -> "text/css";   
        ".js"   -> "application/javascript"; 
        ".jpg"  -> "image/jpeg";
        ".jpeg" -> "image/jpeg";
        ".png"  -> "image/png";  
        ".gif"  -> "image/gif";
        _       -> "text/plain"  
    end.