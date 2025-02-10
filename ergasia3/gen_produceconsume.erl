-module(gen_produceconsume).
-behaviour(gen_server).
-export([start/2, stop/1, produce/2, consume/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-callback handle_produce(Input :: term()) -> {ok, term()}.
-callback handle_consume(Task :: term()) -> ok.

start(Callback, MaxSize) when is_atom(Callback) ->
    case erlang:function_exported(Callback, handle_produce, 1) andalso
         erlang:function_exported(Callback, handle_consume, 1) of
        true ->
            gen_server:start_link(?MODULE, {Callback, MaxSize}, []);
        false ->
            {error, invalid_callback}
    end.

stop(Pid) ->
    gen_server:cast(Pid, stop).

produce(Pid, T) ->
    gen_server:call(Pid, {produce, T}).

consume(Pid) ->
    gen_server:call(Pid, consume).

init({Callback, MaxSize}) ->
    {ok, #{callback => Callback, max_size => MaxSize, queue => []}}.

handle_call({produce, T}, _From, State = #{callback := Callback, queue := Queue, max_size := MaxSize}) ->
    case length(Queue) < MaxSize of
        true ->
            {ok, Task} = Callback:handle_produce(T),
            {reply, {ok, Task}, State#{queue := Queue ++ [Task]}};
        false ->
            {reply, full, State}
    end;
handle_call(consume, _From, State = #{callback := Callback, queue := [Task | Rest]}) ->
    Callback:handle_consume(Task),
    {reply, ok, State#{queue := Rest}};
handle_call(consume, _From, State = #{queue := []}) ->
    {reply, empty, State};
handle_call(_, _, State) -> 
    {reply, {error, unknown_call}, State}.

handle_cast(stop, _State) ->
    {stop, normal, ok};
handle_cast(_, State) -> 
    {noreply, State}.
