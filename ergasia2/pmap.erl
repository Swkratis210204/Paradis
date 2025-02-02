-module(pmap).
-export([unordered/2, worker/3]).

unordered(Fun, List) ->
    unordered(Fun, List, []).

unordered(_Fun, [], Results) -> lists:reverse(Results);

unordered(Fun, [H | T], Acc) ->
    spawn(?MODULE, worker, [self(), Fun, H]),  
    receive
        Result -> unordered(Fun, T, [Result | Acc]) 
    end.

worker(Parent, Fun, X) ->
    Parent ! Fun(X).
