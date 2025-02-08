-module(ring).
-export([start/2, loop/1]).

start(N, M) ->
    Pids = create_processes(N),
    % Start the message passing with the first process
    hd(Pids) ! {message, 0, M * N, Pids},
    receive
        {done, Result} ->
            Result
    end.

create_processes(N) ->
    create_processes(N, []).

create_processes(0, Pids) ->
    lists:reverse(Pids);
create_processes(N, Pids) ->
    Pid = spawn(ring, loop, [self()]),
    create_processes(N - 1, [Pid | Pids]).

loop(Parent) ->
    receive
        {message, Value, 0, _} ->
            Parent ! {done, Value};
        {message, Value, M, [NextPid | Rest]} ->
            NewValue = Value + 1,
            NextPid ! {message, NewValue, M - 1, Rest ++ [self()]},
            loop(Parent)
    end.