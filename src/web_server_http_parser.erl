-module(web_server_http_parser).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).
-export([parse/3]).

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
    io:format("[+][~p][~p] - Starting HTTP parser...~n", [calendar:local_time(), self()]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> 
    gen_server:stop(?MODULE).   

init(_Args) ->     
    {ok, []}.

handle_call(_Request, _From, State) ->
    io:format("[-][~p][~p] - Received unknown call: ~p~n", [calendar:local_time(), self(), _Request]),
    {reply, ok, State}.

handle_cast({request_message, {Data, Connection, AcceptorPid}}, State) ->        
    process_request(Data, Connection, AcceptorPid),
    {noreply, State};
handle_cast(_UnknownMessage, State) ->
    io:format("[+][~p][~p] - Unknown message in HTTP parser: ~p~n", [calendar:local_time(), self(), _UnknownMessage]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[+][~p][~p] - HTTP parser terminated.~n", [calendar:local_time(), self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:format("[+][~p][~p] - Performing code change...~n", [calendar:local_time(), self()]),
    {ok, State}.

%%%===================================================================
%% Fim gen_server
%%%===================================================================

parse(Data, AcceptSocket, AcceptorPid) -> 
    io:format("[+][~p][~p] - Send data to http parser~n~n", [calendar:local_time(), self()]),
    gen_server:cast(?MODULE, {request_message, {Data, AcceptSocket, AcceptorPid}}).

%%%===================================================================
%% Funções privadas
%%%===================================================================

process_request(Data, Connection, AcceptorPid) when is_binary(Data), is_port(Connection) ->
    io:format("[+][~p][~p] - Processing request...~n", [calendar:local_time(), self()]),
    try
        {Method, Path, _} = parse_request(Data),
        io:format("[+][~p][~p] - Method: ~p | Path: ~p ~n", [calendar:local_time(), self(), Method, Path]),
        Authenticated = true,%check_authentication(Headers),
        route_request(Method, Path, Authenticated, Data, Connection, AcceptorPid) 
    catch
        error:Reason ->
            io:format("[!][~p][~p] - Error processing request: ~p~n", [calendar:local_time(), self(), Reason]),            
            gen_tcp:close(Connection)
    end;
process_request(_InvalidData, _InvalidConnection, _InvalidAcceptorPid) ->
    io:format("[!][~p][~p] - Invalid data or connection in process_request~n", [calendar:local_time(), self()]).

route_request(<<"GET">>, Path, _, _, Connection, AcceptorPid) ->
    io:format("[+][~p][~p] - Send data to GET Process...~n", [calendar:local_time(), self()]),
    web_server_http_get:handle_request(Path, Connection, AcceptorPid).


parse_request(Data) when is_binary(Data) ->
    Lines = binary:split(Data, <<?CRLF>>, [global]),
    case Lines of
        [RequestLine | HeaderLines] ->
            case parse_request_line(RequestLine) of
                {error, _} ->
                    io:format("[-][~p][~p] - Error parsing request line~n", [calendar:local_time(), self()]),
                    {error, "/", []};
                {Method, Path} ->
                    Headers = parse_headers(HeaderLines),
                    {Method, Path, Headers}
            end;
        _ ->
            io:format("[-][~p][~p] - Invalid request format~n", [calendar:local_time(), self()]),
            {error, "/", []}
    end;
parse_request(_InvalidData) ->
    io:format("[-][~p][~p] - Invalid request data~n", [calendar:local_time(), self()]),
    {error, "/", []}.

parse_request_line(RequestLine) ->
    case string:split(RequestLine, " ", all) of
        [Method, Path | _] -> {Method, Path};
        _ -> 
            io:format("[-][~p][~p] - Error parsing request line~n", [calendar:local_time(), self()]),
            {error, "/"}
    end.

parse_headers(Lines) ->
    lists:foldl(fun(Line, Acc) ->
        case binary:split(Line, <<": ">>) of
            [Key, Value] ->
                [{binary_to_list(Key), binary_to_list(Value)} | Acc];
            _ ->
                io:format("[+][~p][~p] - Ignoring invalid header line: ~p~n", [calendar:local_time(), self(), Line]),
                Acc
        end
    end, [], Lines).