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
    io:format("[~p] - Method: ~p | Path: ~p ", [calendar:local_time(), Method, Path]),
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
        case binary:split(Line, <<": ">>) of
            [Key, Value] ->
                [{binary_to_list(Key), binary_to_list(Value)} | Acc];
            _ ->
                io:format("[~p] - Ignoring invalid header line: ~p~n", [calendar:local_time(), Line]),
                Acc
        end
    end, [], Lines).
        

check_authentication(Headers) ->
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
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
    Date = io_lib:format("~s, ~2..0w ~s ~4..0w ~2..0w:~2..0w:~2..0w GMT",
                            [day_of_week(Year, Month, Day), Day, month(Month), Year, Hour, Minute, Second]),

    BinaryBody = iolist_to_binary(Body),

    ContentLength = integer_to_list(byte_size(BinaryBody)),

    Headers = [
        "HTTP/1.1 ", Status, ?CRLF,
        "Date: ", Date, ?CRLF,
        "Server: MyErlangServer", ?CRLF,
        "Content-Type: ", ContentType, ?CRLF,
        "Content-Length: ", ContentLength, ?CRLF,
        "Connection: close", ?CRLF,
        ?CRLF
    ],

    Response = list_to_binary([Headers, BinaryBody]),
    io:format("Sending response body ~p ~n", [BinaryBody]),
    gen_tcp:send(Connection, Response).

content_type(Path) ->
    case filename:extension(Path) of
        ".html" -> "text/html";
        ".jpg" -> "image/jpeg";
        ".gif" -> "image/gif";
        _ -> "application/octet-stream"
    end.

not_found_response() ->
    <<"<html><head><title>Not Found</title></head><body>Not Found</body></html>">>.

unauthorized_response() ->
    <<"<html><head><title>Unauthorized</title></head><body>Unauthorized</body></html>">>.

day_of_week(Y, M, D) ->
    Days = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"],
    DayNum = calendar:day_of_the_week(Y, M, D),
    lists:nth(DayNum, Days).

month(M) ->
    Months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
    lists:nth(M, Months).