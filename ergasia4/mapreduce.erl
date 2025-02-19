-module(mapreduce).
-export([test/0, test_distributed/0,spawn_mappers/5,spawn_mapper/4,spawn_reducer/2,mapreduce/5,mapreduce/6]).

test() ->
    Mapper = fun (_Key, Text) -> [{Word, 1} || Word <- Text] end,
    Reducer = fun (Word, Counts) -> [{Word, lists:sum(Counts)}] end,
    mapreduce(Mapper, 2, Reducer, 10, [{a, ["hello", "world", "hello", "text"]}, {b, ["world", "a", "b", "text"]}]).

test_distributed() ->
    Nodes = [node() | nodes()],
    Mapper = fun (_Key, Text) -> [{Word, 1} || Word <- Text] end,
    Reducer = fun (Word, Counts) -> [{Word, lists:sum(Counts)}] end,
    mapreduce(Nodes, Mapper, 2, Reducer, 10, [{a, ["hello", "world", "hello", "text"]}, {b, ["world", "a", "b", "text"]}]).

mapreduce(Mapper, Mappers, Reducer, Reducers, Input) ->
    mapreduce([node()], Mapper, Mappers, Reducer, Reducers, Input).

mapreduce(Nodes, Mapper, Mappers, Reducer, Reducers, Input) ->
    Self = self(),
    ReducerPids = spawn_reducers(Nodes, Self, Reducer, Reducers),
    MapperPids = spawn_mappers(Nodes, Self, Mapper, ReducerPids, partition(Mappers, Input)),
    wait_for_mappers(length(MapperPids)),
    [ReducerPid ! {start_reduce} || ReducerPid <- ReducerPids],
    lists:sort(lists:flatten([receive {reduce_done, _Pid, Data} -> Data end || _ <- ReducerPids])).

spawn_mappers(Nodes, Master, Mapper, Reducers, Partitions) ->
    [rpc:call(lists:nth(((I - 1) rem length(Nodes)) + 1, Nodes), mapreduce, spawn_mapper, [Master, Mapper, Reducers, Part]) || {I, Part} <- lists:enumerate(Partitions)].

spawn_mapper(Master, Mapper, Reducers, Data) ->
    spawn_link(fun () ->
        lists:foreach(fun ({DataKey, DataValue}) ->
            lists:foreach(fun ({MapKey, MapValue}) ->
                lists:nth(erlang:phash2(MapKey, length(Reducers)) + 1, Reducers) ! {map_result, MapKey, MapValue}
            end, Mapper(DataKey, DataValue))
        end, Data),
        Master ! {mapper_done, self()}
    end).

spawn_reducers(Nodes, Master, Reducer, Reducers) ->
    [rpc:call(lists:nth(((I - 1) rem length(Nodes)) + 1, Nodes), mapreduce, spawn_reducer, [Master, Reducer]) || I <- lists:seq(1, Reducers)].

spawn_reducer(Master, Reducer) ->
    spawn_link(fun () ->
        receive {start_reduce} ->
            Master ! {reduce_done, self(), [KV || {K, Vs} <- groupkeys(collect_map_data([])), KV <- Reducer(K, Vs)]}
        end
    end).

collect_map_data(Data) ->
    receive {map_result, Key, Value} -> collect_map_data([{Key, Value} | Data]) after 500 -> Data end.

wait_for_mappers(0) -> ok;
wait_for_mappers(N) -> receive {mapper_done, _Pid} -> wait_for_mappers(N - 1) end.

groupkeys([]) -> [];
groupkeys([{K, V} | Rest]) -> groupkeys(K, [V], Rest).

groupkeys(K, Vs, [{K, V} | Rest]) -> groupkeys(K, [V | Vs], Rest);
groupkeys(K, Vs, Rest) -> [{K, lists:reverse(Vs)} | groupkeys(Rest)].

partition(N, L) -> lists:splitwith(fun (_, I) -> I =< length(L) div N end, lists:enumerate(L)).
