-module(web_server_http_socket_writer).
-behaviour(gen_server).
-author('Fernando Areias <nando.calheirosx@gmail.com>').
%% API
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([write_success_ok/4, write_not_found/2, write_unauthorized/2, write_method_not_allowed/2, write_internal_server_error/2]).

-define(CRLF, "\r\n").
-define(AUTH_REALM, "RESTRITO").
-define(AUTH_CREDENTIALS, "admin:admin").

%%%===================================================================
%% Inicio gen_server
%%%===================================================================

start_link() ->
    io:format("[+][~p][~p] - Starting Socket Writer...~n", [calendar:local_time(), self()]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> 
    gen_server:stop(?MODULE).   

init(_Args) ->
    io:format("[+][~p][~p] - Initing Socket Writer...~n", [calendar:local_time(), self()]),
    {ok, []}.

handle_call(_Request, _From, State) ->
    io:format("[-][~p][~p] - Received unknown call: ~p~n", [calendar:local_time(), self(), _Request]),
    {reply, ok, State}.

handle_cast({success_ok, {Connection, ContentType, Content, AcceptorPid}}, State) -> 
    write_response(Connection, "200 OK", ContentType, Content, AcceptorPid),
    {noreply, State};
handle_cast({not_found, Connection, AcceptorPid}, State) -> 
    write_response(Connection, "404 Not Found", "text/html", not_found_response(), AcceptorPid),
    {noreply, State};
handle_cast({unauthorized, Connection, AcceptorPid}, State) -> 
    write_response(Connection, "401 Unauthorized", "text/html", unauthorized_response(), AcceptorPid),
    {noreply, State};
handle_cast({method_not_allowed, Connection, AcceptorPid}, State) -> 
    write_response(Connection, "405 Method Not Allowed", "text/html", method_not_allowed_response(), AcceptorPid),
    {noreply, State};
handle_cast({internal_server_error, Connection, AcceptorPid}, State) -> 
    write_response(Connection, "500 Internal Server Error", "text/html", internal_server_error_response(), AcceptorPid),
    {noreply, State};
handle_cast(_Msg, State) ->
    io:format("[-][~p][~p] - Received unknown cast: ~p~n", [calendar:local_time(), self(), _Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    io:format("[-][~p][~p] - Received unknown info: ~p~n", [calendar:local_time(), self(), _Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[+][~p][~p] - HTTP parser terminated.~n", [calendar:local_time(), self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:format("[+][~p][~p] - Performing code change...~n", [calendar:local_time(), self()]),
    {ok, State}.

%%%===================================================================
%% Interface
%%%===================================================================

write_success_ok(Connection, ContentType, Content, AcceptorPid) ->
    gen_server:cast(?MODULE, {success_ok, {Connection, ContentType, Content, AcceptorPid}}).

write_not_found(Connection, AcceptorPid) ->
    gen_server:cast(?MODULE, {not_found, Connection, AcceptorPid}).

write_unauthorized(Connection, AcceptorPid) ->
    gen_server:cast(?MODULE, {unauthorized, Connection, AcceptorPid}).

write_method_not_allowed(Connection, AcceptorPid) ->
    gen_server:cast(?MODULE, {method_not_allowed, Connection, AcceptorPid}).

write_internal_server_error(Connection, AcceptorPid) ->
    gen_server:cast(?MODULE, {internal_server_error, Connection, AcceptorPid}).

%%%===================================================================
%% Funcoes privadas
%%%===================================================================

write_response(Connection, Status, ContentType, Body, AcceptorPid) ->
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
    io:format("[*][~p][~p] - Connection ~p | Status response ~p ~n", [calendar:local_time(), self(), Connection, Status]),
    Response = list_to_binary([Headers, BinaryBody]),
    case gen_tcp:send(Connection, Response) of
        ok ->
            io:format("[+][~p][~p] - Response sent successfully~n", [calendar:local_time(), self()]);
        {error, Reason} ->
            io:format("[-][~p][~p] - Failed to send response: ~p~n", [calendar:local_time(), self(), Reason])
    end,
    gen_server:cast(metrics, {close_connection}),
    io:format("[+][~p][~p] - Send message connection_closed to process: ~p ~n", [calendar:local_time(), self(), AcceptorPid]),
    AcceptorPid ! {connection_closed, Connection},
    gen_tcp:close(Connection),
    io:format("[+][~p][~p] - Connection closed~n", [calendar:local_time(), self()]).

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