-module(double).
-export([start/0, double/0]).

start() ->
    register(double, spawn(fun double/0)).

double() ->
    receive
        {Pid, Ref, N} ->  
            Result = 2 * N,  %% This line will crash if N is not a number
            Pid ! {Ref, Result},
            double()
    end.

double(N) when is_integer(N) ->
    receive
        {Pid, Ref} when is_integer(N) -> 
            Pid ! {Ref, 2 * N},
            double()
    end.
