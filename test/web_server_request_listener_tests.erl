-module(web_server_request_listener_tests).
-include_lib("eunit/include/eunit.hrl").

start_link_test_() ->
    {setup,
     fun setup/0,          
     fun cleanup/1,        
     fun tests/1}.         

setup() ->
    {ok, Pid} = web_server_request_listener:start_link(),
    io:format("Starting gen_server PID %s", Pid),
    Pid.

cleanup(Pid) ->
    web_server_request_listener:stop(),
    io:format("Ending the gen_server PID %s", Pid),
    ok.

tests(Pid) ->
    [?_assert(erlang:is_process_alive(Pid)),  
     ?_test(test_handle_call(Pid)),           
     ?_test(test_handle_cast(Pid))].          

test_handle_call(Pid) ->
    {reply, ok, _State} = gen_server:call(Pid, test_call),
    ?assertEqual(ok, ok).

test_handle_cast(Pid) ->
    gen_server:cast(Pid, test_cast),
    ?assertEqual(ok, ok).