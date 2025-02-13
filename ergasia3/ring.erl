-module(ring).
-export([start/2, process_loop/3]).

start(N, M) when N >= 1, M > 0 ->
    Parent = self(), 
    FirstPid = spawn(?MODULE, process_loop, [Parent, undefined, N]),  
    LastPid = spawn_ring(N - 1, Parent, FirstPid, N),  
    FirstPid ! {init, LastPid},  
    FirstPid ! {start, 0, M}, 
    receive {final_result, Result} -> Result end;
start(_, _) -> {error, invalid_parameters}.

spawn_ring(0, _, PrevPid, _) -> PrevPid;
spawn_ring(N, Parent, PrevPid, TotalN) ->
    Pid = spawn(?MODULE, process_loop, [Parent, PrevPid, TotalN]),
    spawn_ring(N - 1, Parent, Pid, TotalN).

process_loop(Parent, NextPid, N) ->
    receive
        {init, NewNextPid} -> process_loop(Parent, NewNextPid, N);
        {start, Value, Target} ->
            case Value of
                Target -> Parent ! {final_result, Target * N}, exit(normal);
                _ -> NextPid ! {start, Value + 1, Target}, process_loop(Parent, NextPid, N)
            end
    end.
