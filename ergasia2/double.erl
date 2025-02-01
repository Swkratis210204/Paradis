-module(double).
-export([start/0, double/0, double/1]).

start() -> register(double, spawn(fun double/0)).

double() ->
    receive
        {Pid, Ref, N} when is_number(N) ->  
            Pid ! {Ref, 2 * N},
            double();
        _ -> exit(badarith)
    end.

double(N) ->
    Ref = make_ref(),
    case whereis(double) of
        undefined -> 
            timer:sleep(100),
            double(N);
        Pid -> 
            Pid ! {self(), Ref, N},
            receive {Ref, Result} -> Result after 1000 -> double(N) end
    end.
