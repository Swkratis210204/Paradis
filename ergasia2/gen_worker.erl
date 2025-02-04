-module(gen_worker).
-export([start/2, stop/1, async/2, await/1, await_all/1]).

-callback handle_work(Value :: term()) -> {result, term()} | no_result.

%% Start a work-pool with Callback and Max workers
start(Callback, Max) ->
    spawn(fun() -> server_loop(Callback, spawn_workers(Callback, Max), #{}) end).

%% Stop the work-pool and all workers
stop(Pid) ->
    Pid ! stop.

%% Schedule a task for processing, returning a unique reference
async(Pid, Work) ->
    Ref = make_ref(),
    Pid ! {async, Work, Ref, self()},
    Ref.

%% Wait for a single result based on a reference
await(Ref) ->
    receive
        {Ref, {result, Result}} -> {result, Result};
        {Ref, no_result} -> no_result;
        {Ref, error} -> error
    after 5000 -> timeout % Prevent indefinite blocking
    end.

%% Wait for multiple results and return only successful ones
await_all(Refs) ->
    Results = [await(Ref) || Ref <- Refs],
    [Result || {result, Result} <- Results]. % Filter out no_result and errors

%% Spawn worker processes
spawn_workers(Callback, Max) ->
    [spawn(fun() -> worker_loop(Callback) end) || _ <- lists:seq(1, Max)].

%% Server process: Distributes work to workers and stores pending results
server_loop(Callback, Workers, Pending) ->
    receive
        {async, Work, Ref, Caller} ->
            Worker = lists:nth(rand:uniform(length(Workers)), Workers),
            Worker ! {process, Work, Ref, self()},
            server_loop(Callback, Workers, Pending#{Ref => Caller});
        
        {Ref, Result} ->
            case maps:find(Ref, Pending) of
                {ok, Caller} -> Caller ! {Ref, Result};
                error -> ok % Ignore if no caller exists
            end,
            server_loop(Callback, Workers, maps:remove(Ref, Pending));
        
        stop ->
            [Worker ! stop || Worker <- Workers],
            ok
    end.

%% Worker process: Executes tasks and sends results back
worker_loop(Callback) ->
    receive
        {process, Work, Ref, Master} ->
            Result = try Callback:handle_work(Work) of
                {result, _} = Res -> Res;
                no_result -> no_result
            catch
                _:_ -> error
            end,
            Master ! {Ref, Result},
            worker_loop(Callback);
        
        stop -> ok
    end.
