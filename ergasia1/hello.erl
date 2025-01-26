-module(hello).
-export([add/2, sub/2, mul/2, divi/2, eval/1, eval/2,map/2,filter/2]).
%-------------------------------1------------------------------------------
% Basic arithmetic functions
add(A, B) -> A + B.
sub(A, B) -> A - B.
mul(A, B) -> A * B.
divi(A, B) -> A div B.

% Evaluate without variables
eval({Fun, A, B}) when is_integer(A), is_integer(B) ->
    {ok, apply(hello, Fun, [A, B])};
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
eval({Fun, A, B}, Map) when is_map(Map) ->
    ResolvedA = resolve(A, Map),
    ResolvedB = resolve(B, Map),
    case {ResolvedA, ResolvedB} of
        {{ok, ValA}, {ok, ValB}} -> eval({Fun, ValA, ValB});
        {{error, _} = Error, _} -> Error;
        {_, {error, _} = Error} -> Error
    end;
eval(_, _) -> {error, variable_not_found}.

resolve(Value, Map) when is_tuple(Value) ->
    eval(Value, Map);
resolve(Value, Map) ->
    case maps:is_key(Value, Map) of
        true -> {ok, maps:get(Value, Map)};
        false when is_integer(Value) -> {ok, Value};
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