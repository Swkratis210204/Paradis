-module(monitor).
-export([start/0, monitor_loop/1, start_double/0]).

start() ->
    spawn(fun() ->
        process_flag(trap_exit, true), %% Suppress crash reports
        DoublePid = start_double(),
        monitor_loop(DoublePid)
    end).

start_double() ->
    {Pid, _MonitorRef} = spawn_monitor(fun double:double/0), %% Monitor instead of linking
    register(double, Pid),
    Pid.

monitor_loop(DoublePid) ->
    receive
        {'DOWN', _MonitorRef, process, DoublePid, _Reason} -> %% Silent exit detection
            io:format("Restarting double process due to crash~n"),
            NewPid = start_double(),
            monitor_loop(NewPid)
    end.
