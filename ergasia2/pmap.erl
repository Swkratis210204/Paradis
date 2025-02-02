-module(pmap).
-export([unordered/2, unordered/3, worker/2, collect_results/3]).

unordered(Fun, List) ->
    unordered(Fun, List, length(List)). % Default to spawning length(List) workers

unordered(Fun, List, MaxWorkers) ->
    ActualWorkers = min(length(List), MaxWorkers), % Ensure MaxWorkers does not exceed List length
    PidList = [spawn(?MODULE, worker, [self(), Fun]) || _ <- lists:seq(1, ActualWorkers)], % Spawn limited workers
    collect_results(List, PidList, []). % Start processing

worker(Parent, Fun) ->
    receive
        X -> Parent ! Fun(X), worker(Parent, Fun) % Compute and continue
    end.

collect_results([], _, Results) -> Results; % When all tasks are processed, return results
collect_results([H | T], [Pid | PidList], Acc) -> % Take a worker without keeping Rest
    Pid ! H, % Send it a task
    receive
        Result -> collect_results(T, [Pid | PidList], [Result | Acc]) % Collect result, continue
    end.
