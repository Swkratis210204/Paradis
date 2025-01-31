-module(monitor).
-export([start/0, monitor/0]).

start() ->
    double:start(),
    spawn(fun monitor/0).

monitor() ->
    receive
        {Pid, Ref, N} when is_integer(N) ->  
            double ! {Pid, Ref, N},
            monitor()
    end.
