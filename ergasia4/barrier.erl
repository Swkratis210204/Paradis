-module(barrier).
-export([start/1, wait/2]).

start(Expected) ->
    spawn(fun() -> barrier(Expected, []) end).

wait(Barrier, Pid) ->
    Barrier ! {arrive, Pid},
    receive
        continue -> ok
    end.

barrier(Expected, Arrived) ->
    receive
        {arrive, Pid} ->
            NewArrived = [Pid | Arrived],
            case NewArrived == Expected of
                true -> notify_clients(NewArrived);
                false -> barrier(Expected, NewArrived)
            end
    end.

notify_clients([]) -> done;
notify_clients([Pid | Rest]) ->
    Pid ! continue,
    notify_clients(Rest).
