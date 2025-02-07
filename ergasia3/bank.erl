-module(bank).
-behaviour(gen_server).

-export([start/0, balance/2, deposit/3, withdraw/3, lend/4]).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

start() ->
    case gen_server:start_link({local, bank_server}, ?MODULE, #{}, []) of
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end.

balance(Pid, Acc) ->
    case is_process_alive(Pid) of
        true -> gen_server:call(Pid, {balance, Acc});
        false -> no_bank
    end.

deposit(Pid, Acc, Amt) when Amt >= 0 ->
    case is_process_alive(Pid) of
        true -> gen_server:call(Pid, {deposit, Acc, Amt});
        false -> no_bank
    end.

withdraw(Pid, Acc, Amt) when Amt > 0 ->
    case is_process_alive(Pid) of
        true -> gen_server:call(Pid, {withdraw, Acc, Amt});
        false -> no_bank
    end.

lend(Pid, FromAcc, ToAcc, Amt) when Amt > 0 ->
    case is_process_alive(Pid) of
        true -> gen_server:call(Pid, {lend, FromAcc, ToAcc, Amt});
        false -> no_bank
    end.

init(_) ->
    {ok, #{}}.  % Initial empty state

handle_call({balance, Acc}, _From, Balances) ->
    Response = case maps:find(Acc, Balances) of
        {ok, Amt} -> {ok, Amt};
        error -> no_account
    end,
    {reply, Response, Balances};

handle_call({deposit, Acc, Amt}, _From, Balances) when Amt >= 0 ->
    NewBalances = Balances#{Acc => maps:get(Acc, Balances, 0) + Amt},
    {reply, {ok, maps:get(Acc, NewBalances)}, NewBalances};

handle_call({withdraw, Acc, Amt}, _From, Balances) when Amt > 0 ->
    case maps:find(Acc, Balances) of
        {ok, Bal} when Bal >= Amt ->
            NewBalances = Balances#{Acc => Bal - Amt},
            {reply, {ok, Bal - Amt}, NewBalances};
        error -> {reply, no_account, Balances};
        _ -> {reply, insufficient_funds, Balances}
    end;

handle_call({lend, FromAcc, ToAcc, Amt}, _From, Balances) when Amt > 0 ->
    case {maps:find(FromAcc, Balances), maps:find(ToAcc, Balances)} of
        {error, error} -> {reply, {no_account, both}, Balances};
        {error, _} -> {reply, {no_account, FromAcc}, Balances};
        {_, error} -> {reply, {no_account, ToAcc}, Balances};
        {{ok, FromBal}, {ok, ToBal}} when FromBal >= Amt ->
            NewBalances = Balances#{FromAcc => FromBal - Amt, ToAcc => ToBal + Amt},
            {reply, ok, NewBalances};
        _ -> {reply, insufficient_funds, Balances}
    end.

handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
