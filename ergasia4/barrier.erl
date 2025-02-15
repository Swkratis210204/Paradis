-module(barrier).
-export([start/1, wait/2]).

start(Refs) ->
    spawn(fun() -> barrier(Refs, [], maps:new()) end).

wait(Barrier, Ref) ->
    Barrier ! {arrive, Ref, self()},
    receive
        {continue, Ref} -> ok
    after 0 -> ok
    end.

barrier(Expected, Arrived, Waiting) ->
    receive
        {arrive, Ref, Pid} ->
            case lists:member(Ref, Expected) of
                true ->
                    NewArrived = [Ref | Arrived],
                    NewWaiting = maps:put(Pid, Ref, Waiting),
                    SortedExpected = lists:sort(Expected),
                    SortedArrived = lists:sort(NewArrived),
                    case SortedArrived == SortedExpected of
                        true -> notify_clients(maps:to_list(NewWaiting));
                        false -> barrier(Expected, NewArrived, NewWaiting)
                    end;
                false -> 
                    Pid ! {continue, Ref},
                    barrier(Expected, Arrived, Waiting)
            end
    end.


notify_clients([]) ->
    done;
notify_clients([{Pid, Ref} | Rest]) ->
    Pid ! {continue, Ref},
    notify_clients(Rest).
