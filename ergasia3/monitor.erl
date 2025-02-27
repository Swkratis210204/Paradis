-module(monitor).
-behavior(supervisor). 
-export([start/0, init/1]).

start() ->
    supervisor:start(?MODULE, []).

init(_) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 5},
    ChildSpec = [
        #{id => double_id, 
          start => {double, start, []}, 
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [double]}
    ],

    {ok, {SupFlags, ChildSpec}}.
