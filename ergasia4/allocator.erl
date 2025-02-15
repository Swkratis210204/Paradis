-module(allocator).
-export([start/1, request/2, release/2]).

start(Resources) ->
    spawn(fun() -> allocator(Resources) end).

request(Allocator, ReqResources) ->
    Allocator ! {request, self(), ReqResources},
    receive
        {granted, Allocated} -> Allocated
    end.

release(Allocator, ReleasedResources) ->
    Allocator ! {release, ReleasedResources},
    ok.

allocator(Resources) ->
    receive
        {request, From, ReqResources} ->
            case can_allocate(Resources, ReqResources) of
                true ->
                    NewResources = allocate(Resources, ReqResources),
                    From ! {granted, ReqResources},
                    allocator(NewResources);
                false ->
                    receive after 100 -> ok end, % Avoid busy waiting
                    allocator(Resources) % Keep waiting
            end;
        {release, ReleasedResources} ->
            NewResources = release_resources(Resources, ReleasedResources),
            allocator(NewResources)
    end.

can_allocate(Resources, ReqResources) ->
    lists:all(fun({Key, ReqAmount}) ->
        maps:get(Key, Resources, 0) >= ReqAmount
    end, maps:to_list(ReqResources)).

allocate(Resources, ReqResources) ->
    maps:fold(fun(Key, ReqAmount, Acc) ->
        maps:update_with(Key, fun(Val) -> Val - ReqAmount end, 0, Acc)
    end, Resources, ReqResources).

release_resources(Resources, ReleasedResources) ->
    maps:fold(fun(Key, RelAmount, Acc) ->
        maps:update_with(Key, fun(Val) -> Val + RelAmount end, 0, Acc)
    end, Resources, ReleasedResources).
