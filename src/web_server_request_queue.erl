-module(web_server_request_queue).
-behaviour(gen_statem).

-export([start_link/0, stop/0, callback_mode/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([idle/3]).

-record(state, {
    queue = queue:new() :: queue:queue(),
    size = 0 :: non_neg_integer()
}).

%% Inicia a FSM
start_link() ->
    io:format("Starting request queue FSM...~n"),
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_statem:stop(?MODULE).

callback_mode() ->
    state_functions.

init(_Args) ->
    %% Inicializa o estado com uma fila vazia e tamanho zero
    InitialState = #state{queue = queue:new(), size = 0},
    {ok, idle, InitialState}.

%% Estado 'idle'
idle({call, From, alloc}, State) ->
    {reply, ok, State};

idle({cast, {request_message, Data, Connection}}, State) ->
    processing_message({request_message, Data, Connection}, State);

idle(_Event, State) ->
    {next_state, idle, State}.

%%%===================================================================
%% private functions
%%%===================================================================

processing_message({request_message, Data, Connection}, State) ->
    io:format("Receiving a request message...~n"),
    io:format("Message request received: Data=~p | Connection=~p ~n", [Data, Connection]),
    %% Adiciona o Data à fila usando add_queue
    UpdatedState = add_queue(State, {Data, Connection}),
    {next_state, idle, UpdatedState}.

add_queue(State, Item) ->
    %% Adiciona o item à fila existente
    NewQueue = queue:in(Item, State#state.queue),
    %% Incrementa o tamanho
    NewSize = State#state.size + 1,
    %% Retorna um novo estado com os valores atualizados
    State#state{queue = NewQueue, size = NewSize}.
