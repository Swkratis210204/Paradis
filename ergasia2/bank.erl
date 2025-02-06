-module(bank).
-export([start/0, balance/2, deposit/3, withdraw/3, lend/4]).

start() -> spawn(fun() -> server(#{}) end).

server(Balances) ->
    receive
        {From, Ref, {balance, Acc}} ->
            Response = case maps:find(Acc, Balances) of
                {ok, Amt} -> {ok, Amt};
                error -> no_account
            end,
            From ! {Ref, Response},
            server(Balances);
        {From, Ref, {deposit, Acc, Amt}} when Amt >= 0 ->
            NewBalances = Balances#{Acc => maps:get(Acc, Balances, 0) + Amt},
            From ! {Ref, {ok, maps:get(Acc, NewBalances)}},
            server(NewBalances);
        {From, Ref, {withdraw, Acc, Amt}} when Amt > 0 ->
            case maps:find(Acc, Balances) of
                {ok, Bal} when Bal >= Amt ->
                    NewBalances = Balances#{Acc => Bal - Amt},
                    From ! {Ref, {ok, Bal - Amt}},
                    server(NewBalances);
                error -> From ! {Ref, no_account}, server(Balances);
                _ -> From ! {Ref, insufficient_funds}, server(Balances)
            end;
        {From, Ref, {lend, FromAcc, ToAcc, Amt}} when Amt > 0 ->
            case {maps:find(FromAcc, Balances), maps:find(ToAcc, Balances)} of
                {error, error} -> From ! {Ref, {no_account, both}}, server(Balances);
                {error, _} -> From ! {Ref, {no_account, FromAcc}}, server(Balances);
                {_, error} -> From ! {Ref, {no_account, ToAcc}}, server(Balances);
                {{ok, FromBal}, {ok, ToBal}} when FromBal >= Amt ->
                    NewBalances = Balances#{FromAcc => FromBal - Amt, ToAcc => ToBal + Amt},
                    From ! {Ref, ok},
                    server(NewBalances);
                _ -> From ! {Ref, insufficient_funds}, server(Balances)
            end;
        _ -> server(Balances)
    end.

call(Pid, Msg) ->
    Ref = make_ref(),
    monitor(process, Pid),
    Pid ! {self(), Ref, Msg},
    receive {Ref, Response} -> Response end.

balance(Pid, Acc) -> call(Pid, {balance, Acc}).
deposit(Pid, Acc, Amt) -> call(Pid, {deposit, Acc, Amt}).
withdraw(Pid, Acc, Amt) -> call(Pid, {withdraw, Acc, Amt}).
lend(Pid, FromAcc, ToAcc, Amt) -> call(Pid, {lend, FromAcc, ToAcc, Amt}).
