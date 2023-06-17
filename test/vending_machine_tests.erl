-module(vending_machine_tests).
-include_lib("eunit/include/eunit.hrl").

add_coin_test() ->
    ?assertEqual(vending_machine:add_coin(dime, maps:new()), #{dime => 1}).

optimize_coins_nothing_to_optimize_should_work_test() ->
    {optimized, Coins, Optimized} = vending_machine:optimize_coins(100, maps:new(), maps:new()),
    0 = maps:size(Coins),
    0 = maps:size(Optimized).

optimize_coins_simple_optimization_should_work_test() ->
    Coins = #{quarter => 1},
    true = true.
