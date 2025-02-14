-module(queue_processor).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([dequeue/1]).

-record(state, {
    queue = queue:new() :: queue:queue(),
    size = 0 :: non_neg_integer(),
    name = "" 
}).

start_link(Name) ->    
    io:format("[+][~p] - Starting queue ~p~n", [calendar:local_time(), Name]),
    gen_server:start_link({local, Name}, ?MODULE, Name, []).

stop(Name) ->
    gen_server:stop(Name).

init(Name) ->
    {ok, #state{queue = queue:new(), size = 0, name = Name}}.

 
handle_cast({enqueue, Data, Connection}, State) when is_binary(Data), is_port(Connection) ->
    io:format("[+][Queue ~p][~p] - Enqueue: ~p~n", [State#state.name, calendar:local_time(), Data]),
    NewQueue = queue:in({Data, Connection}, State#state.queue),
    {noreply, State#state{queue = NewQueue, size = State#state.size + 1}};
handle_cast(_InvalidRequest, State) ->
    logger:error("Invalid enqueue request"),
    {noreply, State}.
 
dequeue(Name) ->
    gen_server:call(Name, dequeue).

handle_call(dequeue, _From, State) ->
    case queue:out(State#state.queue) of
        {empty, _} ->
            {reply, empty, State};
        {{value, Msg}, NewQueue} ->
            io:format("[+][Queue ~p][~p] - Dequeued: ~p~n", [State#state.name, calendar:local_time(), Msg]),
            {reply, Msg, State#state{queue = NewQueue, size = State#state.size - 1}}
    end.