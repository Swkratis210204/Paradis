-module(hello).
-export([add/2, sub/2, mul/2, divi/2, eval/1, eval/2,map/2,filter/2,split/2,groupby/2]).
%-------------------------------1------------------------------------------
add(A, B) -> A + B.
sub(A, B) -> A - B.
mul(A, B) -> A * B.
divi(A, B) -> A div B.

eval({Fun, A, B}) when is_integer(A), is_integer(B) ->
    {ok, Fun(A,B)};
eval({Fun, A, B}) when is_tuple(A) ->
    case eval(A) of
        {ok, ResA} -> eval({Fun, ResA, B});
        Error -> Error
    end;
eval({Fun, A, B}) when is_tuple(B) ->
    case eval(B) of
        {ok, ResB} -> eval({Fun, A, ResB});
        Error -> Error
    end;
eval(_) -> {error, variable_not_found}.

%---------------------------------2----------------------------------------
eval(_, Map) when not is_map(Map) -> 
    {error, there_is_no_map};

eval(Fun, _) when not is_function(Fun) -> 
    {error, there_is_no_function};

eval({Fun, A, B}, Map) when is_map(Map) ->
    ResolvedA = resolve(A, Map),
    ResolvedB = resolve(B, Map),
    case {ResolvedA, ResolvedB} of
        {{ok, ValA}, {ok, ValB}} -> eval({Fun, ValA, ValB});
        {{error, _} = Error, _} -> Error;
        {_, {error, _} = Error} -> Error
    end.

resolve(Value, Map) when is_tuple(Value) ->
    eval(Value, Map);
resolve(Value, Map) ->
    case maps:is_key(Value, Map) of
        true -> {ok, maps:get(Value, Map)};
        false -> {error, variable_not_found}
    end.

%--------------------------------3A-----------------------------------------
map(Fun, _) when not is_function(Fun) ->
    {error, not_function};

map(_,L) when not is_list(L)->
    {error, not_List};
map(_, []) ->  
    [];

map(Fun, [H|T]) ->
    NH = Fun(H),      
    NL = map(Fun, T),    
    [NH | NL].
%--------------------------------3B-----------------------------------------
filter(Fun, _) when not is_function(Fun) ->
    {error, not_function};

filter(_, L) when not is_list(L) ->
    {error, not_List};
filter(_, []) ->  
    [];

filter(Fun, [H|T]) ->
    NH = Fun(H),
    NL=filter(Fun,T),
    case NH of
        true->[H | NL];
        false->filter(Fun, T)
    end.
%--------------------------------3C-----------------------------------------
split(Fun, _) when not is_function(Fun) ->
    {error, not_function};

split(_, L) when not is_list(L) ->
    {error, not_list};

split(_, []) ->
    {[], []};  

split(Fun, [H|T]) ->
    {FalseList, TrueList} = split(Fun, T),  
    case Fun(H) of
        true -> {[H | FalseList], TrueList};  
        false -> {FalseList, [H | TrueList]}  
    end.
%--------------------------------3D-----------------------------------------
groupby(Fun, _) when not is_function(Fun) ->
    {error, not_function};

groupby(_, L) when not is_list(L) ->
    {error, not_list};

groupby(Fun, List) ->
    groupby(Fun, List, 1, #{negative => [], positive => [], zero => []}).

groupby(_, [], _, Acc) ->
    Acc; 

groupby(Fun, [H | T], Index, Acc) ->
    Key = Fun(H),
    UpdatedAcc = maps:update_with(Key, fun(L) -> [Index | L] end, [Index], Acc),
    groupby(Fun, T, Index + 1, UpdatedAcc).
