-module(task1).
-export([add/2, sub/2, mul/2, divi/2, eval/1, eval/2,map/2,filter/2,split/2,groupby/2]).
%-------------------------------1------------------------------------------
add(A, B) -> A + B.
sub(A, B) -> A - B.
mul(A, B) -> A * B.
divi(A, B) -> A div B.

eval({Fun, A, B}) when is_atom(Fun), is_integer(A), is_integer(B) ->
    {ok, apply(task1, Fun, [A, B])};
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
eval(_) ->
    {error, invalid_input}.

%---------------------------------2----------------------------------------
eval({Fun, A, B}, Map) ->
    case {resolve(A, Map), resolve(B, Map)} of
        {{ok, ValA}, {ok, ValB}} -> {ok, apply(task1, Fun, [ValA, ValB])};
        {Error = {error, _}, _} -> Error;
        {_, Error = {error, _}} -> Error
    end;
eval(_, _) -> {error, invalid_arguments}.

resolve(Value, _) when is_number(Value) -> {ok, Value};
resolve({Fun, A, B}, Map) -> eval({Fun, A, B}, Map);
resolve(Value, Map) ->
    case maps:get(Value, Map, undefined) of
        undefined -> {error, variable_not_found};
        Val -> {ok, Val}
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
    UpdatedAcc = maps:update_with(Key, fun(L) -> L ++ [Index] end, [Index], Acc),
    groupby(Fun, T, Index + 1, UpdatedAcc).

