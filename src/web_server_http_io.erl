-module(web_server_http_io).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([read/3]).

%%%===================================================================
%% Inicio gen_server
%%%===================================================================

start_link() ->
    io:format("[+][~p] - Starting File IO...~n", [calendar:local_time()]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->     
    {ok, []}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({Path, Connection, AcceptorPid}, State) ->
    read_file(Path, Connection, AcceptorPid),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
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
    io:format("[+][~p] - Reading file: ~p~n", [calendar:local_time(), Path]),
    FilePath = "./http" ++ binary_to_list(Path),
    handle_read(file:read_file(FilePath), content_type(Path), Path, Connection, AcceptorPid).

handle_read({ok, Content}, ContentType, Path, Connection, AcceptorPid) -> 
    io:format("[+][~p] - File read success~n", [calendar:local_time()]),
    web_server_http_cache:set(Path, ContentType, Content),
    web_server_http_socket_writer:write_success_ok(Connection, ContentType, Content, AcceptorPid);
handle_read({error, _}, _, _, Connection, AcceptorPid) ->     
    io:format("[-][~p] - File not found~n", [calendar:local_time()]),
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