-module(web_server_http_parser).

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

-define(CRLF, "\r\n").
-define(AUTH_REALM, "RESTRITO").
-define(AUTH_CREDENTIALS, "admin:admin").

start_link() ->
    io:format("[~p] - Starting HTTP parser...~n", [calendar:local_time()]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> 
    gen_server:stop(?MODULE).   

init(_Args) -> 
    io:format("[~p] - Request message consumer...~n", [calendar:local_time()]),
    gen_server:cast(web_server_request_queue, {subscribe, self()}),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({consume, {Data, Connection}}, State) ->    
    io:format("[~p] - Consumed message: ~p from connection ~p~n", [calendar:local_time(), Data, Connection]),
    process_request(Data, Connection),
    {noreply, State};

handle_cast(_UnknownMessage, State) ->
    io:format("[~p] - Unknown message in HTTP parser: ~p~n", [calendar:local_time(), _UnknownMessage]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[~p] - HTTP parser terminated.~n", [calendar:local_time()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Funções privadas
%%%===================================================================

process_request(Data, Connection) ->
    io:format("[~p] - Processing request...", [calendar:local_time()]),
    {Method, Path, Headers} = parse_request(Data),
    io:format("[~p] - Method: ~p | Path: ~p | Headers: ~p", [calendar:local_time(), Method, Path, Headers]),
    Authenticated = check_authentication(Headers),
    case handle_path(Path, Authenticated) of
        {ok, Content, ContentType} ->
            send_response(Connection, "200 OK", ContentType, Content);
        {error, unauthorized} ->
            send_response(Connection, "401 Unauthorized", 
                          "text/html", unauthorized_response());
        {error, not_found} ->
            send_response(Connection, "404 Not Found", 
                          "text/html", not_found_response())
    end,
    gen_tcp:close(Connection).

parse_request(Data) ->
    Lines = string:split(Data, ?CRLF, all),
    case Lines of
        [RequestLine | HeaderLines] ->
            case parse_request_line(RequestLine) of
                {error, _} ->
                    {error, "/", []};
                {Method, Path} ->
                    Headers = parse_headers(HeaderLines),
                    {Method, Path, Headers}
            end;
        _ -> 
            {error, "/", []}
    end.

parse_request_line(RequestLine) ->
    case string:split(RequestLine, " ", all) of
        [Method, Path | _] -> {Method, Path};
        _ -> {error, "/"}
    end.
        

parse_headers(Lines) ->
    lists:foldl(fun(Line, Acc) ->
        case string:find(Line, ": ") of
            {ok, _} ->
                [Key, Value] = string:split(Line, ": ", parts),
                [{Key, Value} | Acc];
            _ ->
                io:format("[~p] - Ignoring invalid header line: ~p~n", [calendar:local_time(), Line]),
                Acc
        end
    end, [], Lines).
        

check_authentication(Headers) ->
    %% Check for Authorization header
    case lists:keyfind("Authorization", 1, Headers) of
        {_, Value} ->
            ["Basic", Encoded] = string:split(Value, " ", parts),
            Decoded = base64:decode(Encoded),
            Decoded == ?AUTH_CREDENTIALS;
        _ -> false
    end.

handle_path("/RESTRITO", false) ->
    {error, unauthorized};
handle_path(Path, _) ->
    case file:read_file("." ++ Path) of
        {ok, Content} ->
            {ok, Content, content_type(Path)};
        {error, _} ->
            {error, not_found}
    end.

send_response(Connection, Status, ContentType, Body) ->
    Headers = io_lib:format("[~p] - HTTP/1.1 ~s~nContent-Type: ~s~n~n", 
                            [calendar:local_time(), Status, ContentType]),
    gen_tcp:send(Connection, Headers ++ Body).

content_type(Path) ->
    case filename:extension(Path) of
        ".html" -> "text/html";
        ".jpg" -> "image/jpeg";
        ".gif" -> "image/gif";
        _ -> "application/octet-stream"
    end.

unauthorized_response() ->
    "<html><head><title>Unauthorized</title></head><body>Unauthorized</body></html>".

not_found_response() ->
    "<html><head><title>Not Found</title></head><body>Not Found</body></html>".
