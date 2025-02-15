-module(barrier).
-export([start/1, wait/1, test/0]).

start(N) ->
    spawn(fun() -> barrier(N, []) end).
wait(Barrier) ->
    Ref = make_ref(),
    MRef = monitor(process, Barrier),
    Barrier ! {arrive, Ref, self()},
    receive
        {continue, Ref} -> ok;
        {'DOWN', MRef, _, _, _} -> ok
    end.
notify_clients([]) ->
    done;
notify_clients([{Pid, Ref} | Rest]) ->
    Pid ! {continue, Ref},
    notify_clients(Rest).
barrier(0, Arrived) ->
    notify_clients(Arrived);
barrier(N, Arrived) ->
    receive
        {arrive, Ref, Pid} ->
            barrier(N - 1, [{Pid, Ref} | Arrived])
    end.

test() ->
    Barrier = barrier:start(3),
    lists:foreach(
        fun(I) ->
            spawn(fun() ->
                io:format("Process ~p started..~n", [I]),
                receive
                after I * 1000 -> ok
                end,
                io:format("Process ~p waiting..~n", [I]),
                barrier:wait(Barrier),
                io:format("Process ~p resumed..~n", [I])
            end)
        end,
        lists:seq(1, 4)
    ).
