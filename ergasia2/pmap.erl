-module(pmap).
-export([unordered/2, unordered/3, worker/3, collect_results/2, process_tasks/3]).

% Unordered processing without worker limit
unordered(Fun, List) ->
    [spawn(?MODULE, worker, [self(), Fun, X]) || X <- List], % Spawn all workers
    collect_results(length(List), []). % Collect results asynchronously in an unordered manner

% Unordered processing with a worker limit (MaxWorkers)
unordered(Fun, List, MaxWorkers) ->
    ActualWorkers = min(length(List), MaxWorkers), % Ensure MaxWorkers does not exceed List length
    process_tasks(Fun, List, ActualWorkers). % Process tasks with limited parallelism

% Helper function to process the list with a limited number of workers
process_tasks(_Fun, [], _MaxWorkers) -> 
    collect_results(0, []); % No tasks, return an empty list

process_tasks(Fun, List, MaxWorkers) ->
    WorkersToSpawn = min(length(List), MaxWorkers), % Ensure we don’t split more than the available elements
    {Batch, Remaining} = lists:split(WorkersToSpawn, List), % Safely split the list
    [spawn(?MODULE, worker, [self(), Fun, X]) || X <- Batch], % Spawn WorkersToSpawn processes
    Results = collect_results(length(Batch), []), % Collect results of these workers
    Results ++ process_tasks(Fun, Remaining, MaxWorkers). % Process remaining items

% Worker function: applies Fun to X and sends the result to the parent process
worker(Parent, Fun, X) ->
    Parent ! Fun(X).

% Collects results from worker processes
collect_results(0, Results) -> 
    Results; % Return results once all have been received

collect_results(N, Acc) ->
    receive
        Result -> collect_results(N - 1, [Result | Acc]) % Accumulate results in arbitrary order
    end.
