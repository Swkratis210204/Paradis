-module(monitor).
-export([start/0, monitor_loop/1, start_double/0]).

start() -> spawn(fun() -> process_flag(trap_exit, true), monitor_loop(start_double()) end).

start_double() -> 
    {Pid, _} = spawn_monitor(fun double:double/0),
    register(double, Pid),
    Pid.

monitor_loop(Pid) ->
    receive {'DOWN', _, process, Pid, _} -> monitor_loop(start_double()) end.
