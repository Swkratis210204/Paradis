-module(ring).
-export([start/2, process_loop/1]).

start(N, M) when N > 1, M > 0 ->
    Sum = N * M,  % Compute the target sum at the start
    FirstPid = spawn(?MODULE, process_loop, [undefined]),
    LastPid = spawn_ring(N - 1, FirstPid, FirstPid),
    FirstPid ! {init, LastPid},  
    FirstPid ! {start, Sum, M},  
    ok.

spawn_ring(1, PrevPid, FirstPid) ->
    Pid = spawn(?MODULE, process_loop, [PrevPid]),
    Pid ! {init, FirstPid},
    Pid;
spawn_ring(N, PrevPid, FirstPid) when N > 1 ->
    Pid = spawn(?MODULE, process_loop, [PrevPid]),
    spawn_ring(N - 1, Pid, FirstPid).

process_loop(NextPid) ->
    receive
        {init, NewNextPid} ->
            process_loop(NewNextPid);
        {start, _, 0} ->  
            exit(normal);
        {start, Sum, M} when M > 0 -> 
            NextPid ! {start, Sum, M - 1}, 
            process_loop(NextPid)
    end.
