-module(web_server_http_parser).

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).
-export([parse/2]).

-define(CRLF, "\r\n").
-define(AUTH_REALM, "RESTRITO").
-define(AUTH_CREDENTIALS, "admin:admin"). 
%%%===================================================================
%% Funções públicas
%%%===================================================================

%%%===================================================================
%% Inicio gen_server
%%%===================================================================
start_link() ->
    io:format("[+][~p] - Starting HTTP parser...~n", [calendar:local_time()]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> 
    gen_server:stop(?MODULE).   

init(_Args) ->     
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({request_message, {Data, Connection}}, State) ->        
    process_request(Data, Connection),
    {noreply, State};

handle_cast(_UnknownMessage, State) ->
    io:format("[+][~p] - Unknown message in HTTP parser: ~p~n", [calendar:local_time(), _UnknownMessage]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[+][~p] - HTTP parser terminated.~n", [calendar:local_time()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Fim gen_server
%%%===================================================================

parse(Data, AcceptSocket) -> 
    io:format("[+][~p] - Send data to http parser~n~n", [calendar:local_time()]),
    gen_server:cast(?MODULE, {request_message, {Data, AcceptSocket}}).

%%%===================================================================
%% Funções privadas
%%%===================================================================
process_request(Data, Connection) when is_binary(Data), is_port(Connection) ->
    io:format("[+][~p] - Processing request...~n", [calendar:local_time()]),
    try
        {Method, Path, Headers} = parse_request(Data),
        io:format("[+][~p] - Method: ~p | Path: ~p ~n", [calendar:local_time(), Method, Path]),
        Authenticated = true,%check_authentication(Headers),
        route_request(Method, Path, Authenticated, Data, Connection) 
    catch
        error:Reason ->
            io:format("[!] Error processing request: ~p~n", [Reason]),            
            gen_tcp:close(Connection)
    end;
process_request(_InvalidData, _InvalidConnection) ->
    io:format("[!] Invalid data or connection in process_request~n").

route_request(<<"GET">>, Path, Headers, Data, Connection) ->
    io:format("[+][~p] - Send data to GET Process...~n", [calendar:local_time()]),
    web_server_http_get:handle_request(Path, Connection).
 
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

      