-module(ring).
-export([start/2, process_loop/2]).

start(N, M) when N >= 1, M > 0 ->
    Parent = self(),  % Store parent process to send the final result
    FirstPid = spawn(?MODULE, process_loop, [Parent, undefined]),  
    LastPid = spawn_ring(N - 1, Parent, FirstPid, FirstPid),  
    FirstPid ! {init, LastPid},  
    FirstPid ! {start, 0, N * M},  % Start message with 0, expecting final result N * M

    receive
        {final_result, Result} ->
            Result  % Return the final sum
    end;
start(_, _) ->
    {error, invalid_parameters}.

spawn_ring(1, Parent, PrevPid, FirstPid) ->
    Pid = spawn(?MODULE, process_loop, [Parent, PrevPid]),
    Pid ! {init, FirstPid},
    Pid;
spawn_ring(N, Parent, PrevPid, FirstPid) when N > 1 ->
    Pid = spawn(?MODULE, process_loop, [Parent, PrevPid]),
    spawn_ring(N - 1, Parent, Pid, FirstPid).

process_loop(Parent, NextPid) ->
    receive
        {init, NewNextPid} ->
            process_loop(Parent, NewNextPid);
        {start, Value, Target} when Value < Target ->
            NextPid ! {start, Value + 1, Target},  % Increment value
            process_loop(Parent, NextPid);
        {start, Target, Target} ->  % Final value reached
            Parent ! {final_result, Target},  % Send final result to parent
            exit(normal)
    end.
