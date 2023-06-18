-module(vending_machine_tests).
-include_lib("eunit/include/eunit.hrl").

add_coin_test() ->
    ?assertEqual(vending_machine:add_coin(dime, maps:new()), #{dime => 1}).

optimize_coins_nothing_to_optimize_should_work_test() ->
    {optimized, Coins, Optimized} = vending_machine:optimize_coins(100, maps:new(), maps:new()),
    0 = maps:size(Coins),
    0 = maps:size(Optimized).

optimize_coins_remove_quarter_should_work_test() ->
    Coins = #{quarter => 1},
    {optimized, NewCoins, Optimized} = vending_machine:optimize_coins(25, Coins, maps:new()),
    0 = maps:size(NewCoins),
    ?assertEqual(Optimized, Coins).

optimize_coins_remove_quarters_should_work_test() ->
    Coins = #{quarter => 4},
    {optimized, NewCoins, Optimized} = vending_machine:optimize_coins(100, Coins, maps:new()),
    0 = maps:size(NewCoins),
    ?assertEqual(Optimized, Coins).

optimize_coins_one_fifty_from_quarters_dimes_test() ->
    Coins = #{quarter => 4, dime => 5},
    {optimized, NewCoins, Optimized} = vending_machine:optimize_coins(150, Coins, maps:new()),
    0 = maps:size(NewCoins),
    ?assertEqual(Optimized, Coins).

optimize_coins_select_smallest_test() ->
    Coins = #{nickel => 1, dime => 1},
    {optimized, NewCoins, Optimized} = vending_machine:optimize_coins(5, Coins, maps:new()),
    ?assertEqual(#{dime => 1}, NewCoins),
    ?assertEqual(#{nickel => 1}, Optimized).

%% Goes over 150 because we have no nickels
optimize_coins_one_fifty_from_quarters_dimes_over_test() ->
    Coins = #{quarter => 3, dime => 8},
    {optimized, NewCoins, Optimized} = vending_machine:optimize_coins(150, Coins, maps:new()),
    0 = maps:size(NewCoins),
    ?assertEqual(Coins, Optimized).
