-module(hello).
-export([add/2, sub/2, mul/2, divi/2, eval/1,eval/2]).

add(A, B) -> A + B.
sub(A, B) -> A - B.
mul(A, B) -> A * B.
divi(A, B) -> A div B.

%---------------------------------------------------------------
%1o zitoumeno
eval({Fun, A, B}) when is_integer(A), is_integer(B) ->
    {ok, apply(hello, Fun, [A, B])};
%3o zitoumeno
eval({Fun, A, B}) when is_tuple(A) ->
    {ok, ResultA} = eval(A),
    eval({Fun, ResultA, B});
eval({Fun, A, B}) when is_tuple(B) ->
    {ok, ResultB} = eval(B),
    eval({Fun, A, ResultB});
%2o zitoumeno
eval(_) ->
    error.

%--------------------------------------------------------

eval(_, B) when not is_map(B) ->
    {error, variable_not_found};
eval(A, B) when is_map(B) ->
    Size = maps:size(B),
    case Size of
        0 -> eval(A);
        _ -> 
            {First, Second, Third} = A,
            case {maps:is_key(Second, B), maps:is_key(Third, B)} of
                {true, true} ->
                    ValueSecond = maps:get(Second, B),
                    ValueThird = maps:get(Third, B),
                    UpdatedA = {First, ValueSecond, ValueThird},
                    eval(UpdatedA);
                {true, false} ->
                    {error, variable_not_found};
                {false,true} ->
                    {error,variable_not_found}
            end
    end.






