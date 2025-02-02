-module(web_server_request_listener_tests).
-include_lib("eunit/include/eunit.hrl").

%% Testa a inicialização do gen_server
start_link_test_() ->
    {setup,
     fun setup/0,          % executada antes dos testes
     fun cleanup/1,        % executada após os testes
     fun tests/1}.         % executa o teste

setup() ->
    {ok, Pid} = web_server_request_listener:start_link(),
    io:format("Starting gen_server PID %s", Pid),
    Pid.

%% Função de cleanup: para o gen_server
cleanup(Pid) ->
    web_server_request_listener:stop(),
    io:format("Ending the gen_server PID %s", Pid),
    ok.

%% Testes
tests(Pid) ->
    [?_assert(erlang:is_process_alive(Pid)),  
     ?_test(test_handle_call(Pid)),           
     ?_test(test_handle_cast(Pid))].          

%% Testa uma chamada síncrona (handle_call/3)
test_handle_call(Pid) ->
    {reply, ok, _State} = gen_server:call(Pid, test_call),
    ?assertEqual(ok, ok).

%% Testa uma mensagem assíncrona (handle_cast/2)
test_handle_cast(Pid) ->
    gen_server:cast(Pid, test_cast),
    ?assertEqual(ok, ok).