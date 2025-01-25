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
eval(A, B) when is_map(B) ->
    Size = maps:size(B),
    case Size of
        0 -> eval(A);
        _ ->
            case A of
                {Fun, Arg1, Arg2} ->
                    ResolvedArg1 =
                        case is_tuple(Arg1) of
                            true ->
                                case eval(Arg1, B) of
                                    {ok, EvaluatedArg1} -> EvaluatedArg1;
                                    _ -> Arg1
                                end;
                            false ->
                                case maps:is_key(Arg1, B) of
                                    true -> maps:get(Arg1, B);
                                    false -> Arg1
                                end
                        end,
                    ResolvedArg2 =
                        case is_tuple(Arg2) of
                            true ->
                                case eval(Arg2, B) of
                                    {ok, EvaluatedArg2} -> EvaluatedArg2;
                                    _ -> Arg2
                                end;
                            false ->
                                case maps:is_key(Arg2, B) of
                                    true -> maps:get(Arg2, B);
                                    false -> Arg2
                                end
                        end,
                    eval({Fun, ResolvedArg1, ResolvedArg2});
                _ ->
                    {error, variable_not_found}
            end
    end;
eval(_, _) ->
    {error, variable_not_found}.

