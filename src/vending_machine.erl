-module(vending_machine).
-behaviour(gen_statem).

-export([
    start_link/1,
    flip_switch/1,
    request_coins/1,
    request_soda/1,
    insert_coin/2
]).

%% Mandatory callbacks
-export([
    init/1,
    callback_mode/0,
    terminate/3,
    code_change/4,
    on/3,
    off/3
]).

-type coin() :: nickel | dime | quarter.

-record(state, {
    inserted_coins = [] :: list(coin()),
    bank = [] :: #{coin() => pos_integer()}
}).

%% I don't know why this doesn't work...
%% -spec start_link(maps:iterator(coin(), pos_integer())) -> gen_statem:start_ret().
start_link(Bank) ->
    gen_statem:start_link(?MODULE, [], [Bank]).

init([Bank]) ->
    {ok, off, #state{inserted_coins = [], bank = Bank}}.

flip_switch(Pid) ->
    gen_statem:call(Pid, flip_switch).

request_coins(Pid) ->
    gen_statem:call(Pid, request_coins).

request_soda(Pid) ->
    gen_statem:call(Pid, request_soda).

-spec insert_coin(pid(), coin()) -> any().
insert_coin(Pid, Coin) ->
    gen_statem:call(Pid, {insert_coin, Coin}).

on({call, From}, flip_switch, S = #state{}) ->
    {next_state, off, S, [
        {reply, From, off}
    ]}.

off({call, From}, flip_switch, S = #state{}) ->
    {next_state, off, S, [
        {reply, From, off}
    ]}.

%% Mandatory callbacks

terminate(_Reason, _State, _Data) ->
    void.

code_change(_Version, State, Data, _Extra) ->
    {ok, State, Data}.

callback_mode() ->
    state_functions.
