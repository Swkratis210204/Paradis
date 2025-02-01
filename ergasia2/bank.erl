-module(bank).
-export([start/0, balance/2, deposit/3, withdraw/3, lend/4]).

%% Start the bank server and return its PID
start() ->
    spawn(fun() -> server(#{}) end).

%% Server loop that manages accounts as a map
server(Balances) ->
    receive
        {From, {balance, Account}} ->
            Response = case maps:find(Account, Balances) of
                error -> no_account;
                {ok, Amount} -> {ok, Amount}
            end,
            From ! Response,
            server(Balances);

        {From, {deposit, Account, Amount}} when is_number(Amount), Amount >= 0 ->
            NewBalances = Balances#{Account => maps:get(Account, Balances, 0) + Amount},
            From ! {ok, maps:get(Account, NewBalances)},
            server(NewBalances);

        {From, {withdraw, Account, Amount}} when is_number(Amount), Amount > 0 ->
            case maps:find(Account, Balances) of
                error -> From ! no_account, server(Balances);
                {ok, CurrentBalance} when CurrentBalance >= Amount ->
                    NewBalances = Balances#{Account => CurrentBalance - Amount},
                    From ! {ok, CurrentBalance - Amount},
                    server(NewBalances);
                _ -> From ! insufficient_funds, server(Balances)
            end;

        {From, {lend, FromAcc, ToAcc, Amount}} when is_number(Amount), Amount > 0 ->
            case {maps:find(FromAcc, Balances), maps:find(ToAcc, Balances)} of
                {error, error} -> From ! {no_account, both}, server(Balances);
                {error, _} -> From ! {no_account, FromAcc}, server(Balances);
                {_, error} -> From ! {no_account, ToAcc}, server(Balances);
                {{ok, FromBal}, {ok, ToBal}} when FromBal >= Amount ->
                    NewBalances = Balances#{FromAcc => FromBal - Amount, ToAcc => ToBal + Amount},
                    From ! ok,
                    server(NewBalances);
                _ -> From ! insufficient_funds, server(Balances)
            end;

        _ -> server(Balances) % Ignore invalid messages
    end.

%% Client-side functions with monitoring
balance(Pid, Account) ->
    Ref = make_ref(),
    Monitor = erlang:monitor(process, Pid),
    Pid ! {self(), {balance, Account}},
    receive
        Response ->
            erlang:demonitor(Monitor, [flush]),
            Response
    after 1000 ->
        erlang:demonitor(Monitor, [flush]),
        no_bank
    end.

deposit(Pid, Account, Amount) ->
    Ref = make_ref(),
    Monitor = erlang:monitor(process, Pid),
    Pid ! {self(), {deposit, Account, Amount}},
    receive
        Response ->
            erlang:demonitor(Monitor, [flush]),
            Response
    after 1000 ->
        erlang:demonitor(Monitor, [flush]),
        no_bank
    end.

withdraw(Pid, Account, Amount) ->
    Ref = make_ref(),
    Monitor = erlang:monitor(process, Pid),
    Pid ! {self(), {withdraw, Account, Amount}},
    receive
        Response ->
            erlang:demonitor(Monitor, [flush]),
            Response
    after 1000 ->
        erlang:demonitor(Monitor, [flush]),
        no_bank
    end.

lend(Pid, FromAcc, ToAcc, Amount) ->
    Ref = make_ref(),
    Monitor = erlang:monitor(process, Pid),
    Pid ! {self(), {lend, FromAcc, ToAcc, Amount}},
    receive
        Response ->
            erlang:demonitor(Monitor, [flush]),
            Response
    after 1000 ->
        erlang:demonitor(Monitor, [flush]),
        no_bank
    end.
