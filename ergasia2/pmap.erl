-module(pmap).
-export([unordered/2,unordered/3,ordered/3,worker/2]).

worker(Parent, Fun) ->
    receive
        {Index, Value} -> Parent ! {Index, Fun(Value)}, worker(Parent, Fun); % Ordered execution
        Value -> Parent ! Fun(Value), worker(Parent, Fun) % Unordered execution
    end.
%--------------------------------
unordered(Fun, List) ->
    unordered(Fun, List, length(List)).

unordered(Fun, List, MaxWorkers) ->
    ActualWorkers = min(length(List), MaxWorkers),
    WorkerPids = [spawn(?MODULE, worker, [self(), Fun])|| _ <- lists:seq(1, ActualWorkers)],
    collect_unordered(List, WorkerPids, []).

collect_unordered([], WorkerPids, Acc) ->
    [Pid ! stop || Pid <- WorkerPids],
    Acc;  

collect_unordered([H | T], [Pid | Rest], Acc) ->
    Pid ! H,
    receive
        Result ->
            collect_unordered(T, [Pid | Rest], [Result | Acc])
    end.
%----------------------------------------------------------------------------
ordered(Fun, List, MaxWorkers) ->
    ActualWorkers = min(length(List), MaxWorkers),
    WorkerPids = [spawn(?MODULE, worker, [self(), Fun])||_ <- lists:seq(1, ActualWorkers)],
    collect_ordered(List, WorkerPids, 0, maps:new()).

collect_ordered([], WorkerPids, _Index, Map) ->
    [Pid ! stop || Pid <- WorkerPids],
    [maps:get(I, Map) || I <- lists:seq(0, maps:size(Map) - 1)];

collect_ordered([H | T], [Pid | Rest], Index, Map) ->
    Pid ! {Index, H},
    receive
        {Idx, R} ->
            NewMap = maps:put(Idx, R, Map),
            collect_ordered(T, [Pid | Rest], Index + 1, NewMap)
    end.

