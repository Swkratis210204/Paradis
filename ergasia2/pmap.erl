-module(pmap).
-export([unordered/2, worker/3, collect_results/2]).

unordered(Fun, List) ->
    [spawn(?MODULE, worker, [self(), Fun, X]) || X <- List], % Spawn all workers
    collect_results(length(List), []). % Collect results asynchronously in an unordered manner

worker(Parent, Fun, X) ->
    Parent ! Fun(X). % Send the result to the parent process

collect_results(0, Results) -> 
    Results; % Return results when all are received
collect_results(N, Acc) ->
    receive
        Result -> collect_results(N - 1, [Result | Acc]) % Accumulate results in arbitrary order
    end.
