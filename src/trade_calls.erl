%% TODO: This doesn't work yet, we need to fix the trade_statem module
-module(trade_calls).
-export([works/0]).

works() ->
    S = self(),
    %% spawn Carl, and recieved the Pid to send to Jim
    PidCliA = spawn(fun() -> carl(S) end),
    receive
        PidA -> PidA
    end,
    spawn(fun() -> jim(PidA, PidCliA) end).

carl(Parent) ->
    {ok, Pid} = trade_statem:start_link("Carl"),
    Parent ! Pid,
    io:format("Spawned Carl: ~p~n", [Pid]),
    timer:sleep(800),
    trade_statem:accept_trade(Pid),
    timer:sleep(400),
    io:format("~p~n", [trade_statem:ready(Pid)]),
    timer:sleep(1000),
    trade_statem:make_offer(Pid, "horse"),
    trade_statem:make_offer(Pid, "sword"),
    timer:sleep(1000),
    synchronize(),
    trade_statem:ready(Pid),
    timer:sleep(200),
    trade_statem:ready(Pid),
    timer:sleep(1000).

jim(PidCarl, PidCliA) ->
    {ok, Pid} = trade_statem:start_link("Jim"),
    io:format("Spawned Jim: ~p~n", [Pid]),
    timer:sleep(500),
    trade_statem:trade(Pid, PidCarl),
    trade_statem:make_offer(Pid, "boots"),
    timer:sleep(200),
    trade_statem:retract_offer(Pid, "boots"),
    timer:sleep(500),
    trade_statem:make_offer(Pid, "shotgun"),
    timer:sleep(1000),
    synchronize(PidCliA),
    trade_statem:make_offer(Pid, "horse"),
    trade_statem:ready(Pid),
    timer:sleep(200),
    timer:sleep(1000).

synchronize(Pid) ->
    Pid ! self(),
    receive
        acknowledge -> ok
    end.

synchronize() ->
    receive
        From -> From ! acknowledge
    end.
