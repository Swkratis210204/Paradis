-module(gen_worker).
-export([start/2, stop/1, async/2, await/1, await_all/1]).

-callback handle_work(Value :: term()) -> {result, term()} | no_result.

start(Callback, Max) ->
    spawn(fun() -> server_loop(Callback, spawn_workers(Callback, Max), #{}) end).

stop(Pid) ->
    Pid ! stop.

async(Pid, Work) ->
    Ref = make_ref(),
    Pid ! {async, Work, Ref, self()},
    Ref.

await(Ref) ->
    receive
        {Ref, {result, Result}} -> {result, Result};
        {Ref, no_result} -> no_result;
        {Ref, error} -> error
    end.

await_all(Refs) ->
    Results = [await(Ref) || Ref <- Refs],
    [Result || {result, Result} <- Results]. 

spawn_workers(Callback, Max) ->
    [spawn(fun() -> worker_loop(Callback) end) || _ <- lists:seq(1, Max)].

server_loop(Callback, Workers, Pending) ->
    receive
        {async, Work, Ref, Caller} ->
            Worker = lists:nth(rand:uniform(length(Workers)), Workers),
            Worker ! {process, Work, Ref, self()},
            server_loop(Callback, Workers, Pending#{Ref => Caller});
        
        {Ref, Result} ->
            case maps:find(Ref, Pending) of
                {ok, Caller} -> Caller ! {Ref, Result};
                error -> ok 
            end,
            server_loop(Callback, Workers, maps:remove(Ref, Pending));
        stop ->
            [Worker ! stop || Worker <- Workers],
            ok
    end.

worker_loop(Callback) ->
    receive
        {process, Work, Ref, Master} ->
            Result =
                case Callback:handle_work(Work) of
                    {result, _} = Res -> Res;
                    no_result -> no_result;
                    _ -> error
                end,
            Master ! {Ref, Result},
            worker_loop(Callback);
        stop -> ok
    end.

