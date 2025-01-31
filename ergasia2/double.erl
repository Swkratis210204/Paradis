-module(double).
-export([start/0, double/0]).

start() ->
    register(double, spawn(fun double/0)).

double() ->
    receive
        {Pid, Ref, N} ->  % Accept any input and let badarith occur naturally
            Pid ! {Ref, 2 * N},
            double()
    end.

% double(N) when is_integer(N) ->
%     receive
%         {Pid, Ref} when is_integer(N) -> 
%             Pid ! {Ref, 2 * N},
%             double()
%     end.
