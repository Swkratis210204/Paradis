-module(pmap).

%% Export the main public functions only
-export([
    unordered/2,
    unordered/3,
    ordered/3,
    worker/2
]).

%%%%============================================================
%%%% UNORDERED IMPLEMENTATION
%%%%============================================================

%% Default to as many workers as items in the list
unordered(Fun, List) ->
    unordered(Fun, List, length(List)).

%% Spawn 'MaxWorkers' worker processes and collect results in no particular order
unordered(Fun, List, MaxWorkers) ->
    ActualWorkers = min(length(List), MaxWorkers),
    %% Spawn the limited number of workers
    WorkerPids = [spawn(?MODULE, worker, [self(), Fun])
                  || _ <- lists:seq(1, ActualWorkers)],
    %% Collect the results in an unordered fashion
    collect_unordered(List, WorkerPids, []).

%% Collect results: send each item to a worker, receive the result, repeat
collect_unordered([], WorkerPids, Acc) ->
    %% No more items, stop all workers
    [Pid ! stop || Pid <- WorkerPids],
    Acc;  %% Return accumulated results

collect_unordered([H | T], [Pid | Rest], Acc) ->
    %% Send one item (H) to a worker (Pid)
    Pid ! H,
    %% Wait for a result from that worker
    receive
        Result ->
            %% Store it in Acc, then reuse the SAME worker (Pid) for the next item
            collect_unordered(T, [Pid | Rest], [Result | Acc])
    end.

%%%%============================================================
%%%% ORDERED IMPLEMENTATION
%%%%============================================================

%% Ordered version with a worker limit
ordered(Fun, List, MaxWorkers) ->
    ActualWorkers = min(length(List), MaxWorkers),
    %% Spawn the limited number of workers
    WorkerPids = [spawn(?MODULE, worker, [self(), Fun])
                  || _ <- lists:seq(1, ActualWorkers)],
    %% Collect results in the original order
    collect_ordered(List, WorkerPids, 0, maps:new()).

%% Collect results in order: store each result by index in a map
collect_ordered([], WorkerPids, _Index, Map) ->
    %% No more items, stop all workers
    [Pid ! stop || Pid <- WorkerPids],
    %% Reconstruct ordered list from the map
    [maps:get(I, Map) || I <- lists:seq(0, maps:size(Map) - 1)];

collect_ordered([H | T], [Pid | Rest], Index, Map) ->
    %% Send {Index, Item} to differentiate it from an unordered task
    Pid ! {Index, H},
    %% Wait for the worker's reply {Index, Result}
    receive
        {Idx, R} ->
            NewMap = maps:put(Idx, R, Map),
            collect_ordered(T, [Pid | Rest], Index + 1, NewMap)
    end.

%%%%============================================================
%%%% WORKER FUNCTION
%%%%============================================================

worker(Parent, Fun) ->
    receive
        stop -> ok; % Stop signal for cleanup
        {Index, Value} -> Parent ! {Index, Fun(Value)}, worker(Parent, Fun); % Ordered execution
        Value -> Parent ! Fun(Value), worker(Parent, Fun) % Unordered execution
    end.
