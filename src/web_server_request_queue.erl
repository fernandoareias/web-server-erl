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
    io:format("[+][~p] - Starting request queue~n", [calendar:local_time()]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

init(_Args) ->
    InitialState = #state{queue = queue:new(), size = 0, subscribers = []},
    {ok, InitialState}.

%% Inscrição de consumidores
handle_cast({subscribe, ConsumerPid}, State) ->
    io:format("[+][~p] - Subscriber added: ~p~n", [calendar:local_time(), ConsumerPid]),
    UpdatedSubscribers = lists:usort([ConsumerPid | State#state.subscribers]),
    {noreply, State#state{subscribers = UpdatedSubscribers}};

%% Cancelamento de inscrição (opcional)
handle_cast({unsubscribe, ConsumerPid}, State) ->
    io:format("[+][~p] - Subscriber removed: ~p~n", [calendar:local_time(), ConsumerPid]),
    UpdatedSubscribers = lists:delete(ConsumerPid, State#state.subscribers),
    {noreply, State#state{subscribers = UpdatedSubscribers}};

%% Mensagem recebida
handle_cast({request_message, Data, Connection}, State) ->
    io:format("[+][~p] - Receiving a request message...~n", [calendar:local_time()]),
    % io:format("Message received: Data=~p | Connection=~p~n", [Data, Connection]),
    UpdatedState = add_queue(State, {Data, Connection}),
    notify_subscribers({Data, Connection}, UpdatedState#state.subscribers),
    {noreply, UpdatedState};

%% Mensagem desconhecida
handle_cast(_UnknownMessage, State) ->
    io:format("[+][~p] - Mensagem desconhecida recebida: ~p~n", [calendar:local_time(), _UnknownMessage]),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.


%%%===================================================================
%% Funções privadas
%%%===================================================================

add_queue(State, Item) ->
    % io:format("[~p] - Adding message to queue: ~p~n", [calendar:local_time(), Item]),
    NewQueue = queue:in(Item, State#state.queue),
    NewSize = State#state.size + 1,
    State#state{queue = NewQueue, size = NewSize}.

notify_subscribers(Item, Subscribers) ->
    lists:foreach(
        fun(ConsumerPid) ->
            % io:format("[~p] - Notifying subscriber ~p with item ~p~n", [calendar:local_time(), ConsumerPid, Item]),
            gen_server:cast(ConsumerPid, {consume, Item})
        end,
        Subscribers).

