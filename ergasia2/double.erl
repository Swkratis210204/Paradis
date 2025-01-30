-module(double).
-export([start/0, double/0]).

start() ->
    register(double, spawn(fun double/0)).

double() ->
    receive
        {Pid, Ref, N} -> 
            Pid ! {Ref, 2 * N},
            double() 
    end.
