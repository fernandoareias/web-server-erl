-module(web_server_request_queue).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {
    queue = queue:new() :: queue:queue(),
    size = 0 :: non_neg_integer(),
    subscribers = [] :: [pid()]  
}).

start_link() ->    
    io:format("Starting request queue FSM...~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

init(_Args) ->
    InitialState = #state{queue = queue:new(), size = 0, subscribers = []},
    {ok, InitialState}.

%% Inscrição de consumidores
handle_cast({subscribe, ConsumerPid}, State) ->
    io:format("Subscriber added: ~p~n", [ConsumerPid]),
    UpdatedSubscribers = lists:usort([ConsumerPid | State#state.subscribers]),
    {noreply, State#state{subscribers = UpdatedSubscribers}};

%% Cancelamento de inscrição (opcional)
handle_cast({unsubscribe, ConsumerPid}, State) ->
    io:format("Subscriber removed: ~p~n", [ConsumerPid]),
    UpdatedSubscribers = lists:delete(ConsumerPid, State#state.subscribers),
    {noreply, State#state{subscribers = UpdatedSubscribers}};

%% Mensagem recebida
handle_cast({request_message, Data, Connection}, State) ->
    io:format("Receiving a request message...~n"),
    io:format("Message received: Data=~p | Connection=~p~n", [Data, Connection]),
    UpdatedState = add_queue(State, {Data, Connection}),
    notify_subscribers({Data, Connection}, UpdatedState#state.subscribers),
    {noreply, UpdatedState};

%% Mensagem desconhecida
handle_cast(_UnknownMessage, State) ->
    io:format("Mensagem desconhecida recebida: ~p~n", [_UnknownMessage]),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.


%%%===================================================================
%% Funções privadas
%%%===================================================================

add_queue(State, Item) ->
    io:format("Adding message to queue: ~p~n", [Item]),
    NewQueue = queue:in(Item, State#state.queue),
    NewSize = State#state.size + 1,
    State#state{queue = NewQueue, size = NewSize}.

notify_subscribers(Item, Subscribers) ->
    lists:foreach(
        fun(ConsumerPid) ->
            io:format("Notifying subscriber ~p with item ~p~n", [ConsumerPid, Item]),
            gen_server:cast(ConsumerPid, {consume, Item})
        end,
        Subscribers).

