-module(web_server_log).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([write/1]).

%% Função para iniciar o servidor
start_link() ->
    io:format("[+][~p] - Starting Logging processor...~n", [calendar:local_time()]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Inicialização do estado
init([]) ->
    io:format("[+][~p] - Initializing Logging processor...~n", [calendar:local_time()]),
    {ok, #{}}.  % Estado inicial vazio

%% Manipulação de chamadas assíncronas
handle_cast({write, Msg}, State) ->
    io:format("[+][~p] - ~s~n", [calendar:local_time(), Msg]),
    {noreply, State};  % Não retorna resposta ao cliente
handle_cast(_Msg, State) ->
    {noreply, State}.  % Resposta padrão para outras mensagens

%% Manipulação de mensagens genéricas
handle_info(_Info, State) ->
    {noreply, State}.

%% Encerramento do servidor
terminate(_Reason, _State) ->
    ok.

%% Atualização de código
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Funções públicas

%% Escreve uma mensagem no log (usando cast)
write(Msg) ->
    gen_server:cast(?MODULE, {write, Msg}).