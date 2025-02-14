-module(web_server_http_parser).
-behaviour(gen_server).

%%%===================================================================
%% API Functions
%%%===================================================================
-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(state, {name = "" :: string()}).
-define(DEFAULT_INTERVAL, 1000). % 1 segundo
-define(CRLF, "\r\n").
-define(AUTH_REALM, "RESTRITO").
-define(AUTH_CREDENTIALS, "admin:admin").

%%%===================================================================
%% API Callbacks
%%%===================================================================
start_link(Name) ->
    io:format("[+][Processor ~p][~p] - Starting HTTP parser...~n", [Name, calendar:local_time()]),
    gen_server:start_link({local, Name}, ?MODULE, Name, []).

stop(Name) -> 
    gen_server:stop(Name).

%%%===================================================================
%% gen_server Callbacks
%%%===================================================================
init(Name) -> 
    io:format("[+][Processor ~p][~p] - Request message consumer...~n", [Name, calendar:local_time()]),
    erlang:send_after(?DEFAULT_INTERVAL, self(), receive_message),
    {ok, #state{name = Name}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[+][~p] - HTTP parser terminated.~n", [calendar:local_time()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(receive_message, State) ->
    case whereis(http_parser_queue) of
        undefined ->
            io:format("[!][~p] Queue processor is not running~n", [calendar:local_time()]),
            {noreply, State};
        _Pid ->
            case queue_processor:dequeue(http_parser_queue) of
                empty ->
                    erlang:send_after(?DEFAULT_INTERVAL, self(), receive_message),
                    {noreply, State};
                {Data, Connection} when is_binary(Data), is_port(Connection) ->
                    io:format("[+][Processor ~p][~p] - Received message: ~p~n", [State#state.name, calendar:local_time(), Data]),
                    process_request(Data, Connection),
                    erlang:send_after(?DEFAULT_INTERVAL, self(), receive_message),
                    {noreply, State};
                _InvalidMessage ->
                    io:format("[!][Processor ~p][~p] Invalid message format in queue~n", [State#state.name, calendar:local_time()]),
                    erlang:send_after(?DEFAULT_INTERVAL, self(), receive_message),
                    {noreply, State}
            end
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%%%===================================================================
%% Funções privadas
%%%===================================================================
process_request(Data, Connection) when is_binary(Data), is_port(Connection) ->
    io:format("[+][~p] - Processing request...~n", [calendar:local_time()]),
    try
        {Method, Path, Headers} = parse_request(Data),
        io:format("[+][~p] - Method: ~p | Path: ~p ~n", [calendar:local_time(), Method, Path]),
        Authenticated = true,%check_authentication(Headers),
        Response = route_request(Method, Path, Authenticated, Data),
        send_response(Connection, Response),
        gen_server:cast(metrics, {close_connection}),
        io:format("[+][~p] - Connection closed ~n", [calendar:local_time()]),
        gen_tcp:close(Connection)
    catch
        error:Reason ->
            io:format("[!] Error processing request: ~p~n", [Reason]),
            send_response(Connection, {error, internal_server_error}),
            gen_tcp:close(Connection)
    end;
process_request(_InvalidData, _InvalidConnection) ->
    io:format("[!] Invalid data or connection in process_request~n").

route_request(<<"GET">>, Path, Authenticated, _Data) ->
    handle_get(Path, Authenticated);
route_request(<<"POST">>, Path, Authenticated, Data) ->
    handle_post(Path, Authenticated, Data);
route_request(<<"PUT">>, Path, Authenticated, Data) ->
    handle_put(Path, Authenticated, Data);
route_request(<<"DELETE">>, Path, Authenticated, _Data) ->
    handle_delete(Path, Authenticated);
route_request(_, _, _, _) ->
    {error, method_not_allowed}.

handle_get(Path, Authenticated) ->
    io:format("[+][~p] - Handling GET request for path ~p~n", [calendar:local_time(), Path]),
    case Authenticated of
        true ->
            FilePath = "./http" ++ binary_to_list(Path),
            case file:read_file(FilePath) of
                {ok, Content} ->
                    {ok, Content, content_type(Path)};
                {error, _} ->
                    {error, not_found}
            end;
        false ->
            {error, unauthorized}
    end.

handle_post(Path, Authenticated, Data) ->
    io:format("[+][~p] - Handling POST request for path ~p with data ~p~n", [calendar:local_time(), Path, Data]),
    case Authenticated of
        true ->
            % Process POST data here
            {ok, <<"POST request processed">>, "text/plain"};
        false ->
            {error, unauthorized}
    end.

handle_put(Path, Authenticated, Data) ->
    io:format("[+][~p] - Handling PUT request for path ~p with data ~p~n", [calendar:local_time(), Path, Data]),
    case Authenticated of
        true ->
            % Process PUT data here
            {ok, <<"PUT request processed">>, "text/plain"};
        false ->
            {error, unauthorized}
    end.

handle_delete(Path, Authenticated) ->
    io:format("[+][~p] - Handling DELETE request for path ~p~n", [calendar:local_time(), Path]),
    case Authenticated of
        true ->
            % Process DELETE logic here
            {ok, <<"DELETE request processed">>, "text/plain"};
        false ->
            {error, unauthorized}
    end.

check_authentication(Headers) ->
    case lists:keyfind("Authorization", 1, Headers) of
        {_, Value} ->
            ["Basic", Encoded] = string:split(Value, " ", parts),
            Decoded = base64:decode(Encoded),
            Decoded == ?AUTH_CREDENTIALS;
        _ -> false
    end.

parse_request(Data) when is_binary(Data) ->
    Lines = binary:split(Data, <<?CRLF>>, [global]),
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
            io:format("[-][~p] - Invalid request format ~n", [calendar:local_time()]),
            {error, "/", []}
    end;
parse_request(_InvalidData) ->
    io:format("[-][~p] - Invalid request data ~n", [calendar:local_time()]),
    {error, "/", []}.

parse_request_line(RequestLine) ->
    case string:split(RequestLine, " ", all) of
        [Method, Path | _] -> {Method, Path};
        _ -> 
            io:format("[-][~p] - Error parsing request line ~n", [calendar:local_time()]),
            {error, "/"}
    end.

parse_headers(Lines) ->
    lists:foldl(fun(Line, Acc) ->
        case binary:split(Line, <<": ">>) of
            [Key, Value] ->
                [{binary_to_list(Key), binary_to_list(Value)} | Acc];
            _ ->
                io:format("[+][~p] - Ignoring invalid header line: ~p~n", [calendar:local_time(), Line]),
                Acc
        end
    end, [], Lines).

send_response(Connection, {ok, Content, ContentType}) ->
    send_response(Connection, "200 OK", ContentType, Content);
send_response(Connection, {error, unauthorized}) ->
    send_response(Connection, "401 Unauthorized", "text/html", unauthorized_response());
send_response(Connection, {error, not_found}) ->
    send_response(Connection, "404 Not Found", "text/html", not_found_response());
send_response(Connection, {error, internal_server_error}) ->
    send_response(Connection, "500 Internal Server Error", "text/html", internal_server_error_response());
send_response(Connection, {error, method_not_allowed}) ->
    send_response(Connection, "405 Method Not Allowed", "text/html", method_not_allowed_response()).

send_response(Connection, Status, ContentType, Body) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
    Date = io_lib:format("[+][~p] - ~s, ~2..0w ~s ~4..0w ~2..0w:~2..0w:~2..0w GMT",
                         [calendar:local_time(), day_of_week(Year, Month, Day), Day, month(Month), Year, Hour, Minute, Second]),
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
    io:format("[*][~p] - Connection ~p | Status response ~p ~n", [calendar:local_time(), Connection, Status]),
    Response = list_to_binary([Headers, BinaryBody]),
    gen_tcp:send(Connection, Response).

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

not_found_response() ->
    <<"<html><head><title>Not Found</title></head><body>Not Found</body></html>">>.

unauthorized_response() ->
    <<"<html><head><title>Unauthorized</title></head><body>Unauthorized</body></html>">>.

internal_server_error_response() ->
    <<"<html><head><title>Internal Server Error</title></head><body>Internal Server Error</body></html>">>.

method_not_allowed_response() ->
    <<"<html><head><title>Method Not Allowed</title></head><body>Method Not Allowed</body></html>">>.

day_of_week(Y, M, D) ->
    Days = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"],
    DayNum = calendar:day_of_the_week(Y, M, D),
    lists:nth(DayNum, Days).

month(M) ->
    Months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
    lists:nth(M, Months).