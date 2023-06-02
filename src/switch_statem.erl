-module(switch_statem).
-behaviour(gen_statem).

%% API
-export([
    start_link/0,
    flip/1,
    how_many/1
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

-record(state, {
    flipped = 0 :: non_neg_integer()
}).

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

flip(Pid) ->
    gen_statem:call(Pid, flip).

how_many(Pid) ->
    gen_statem:call(Pid, how_many).

init([]) ->
    {ok, off, #state{}}.

on({call, From}, flip, S = #state{}) ->
    {next_state, off, S#state{flipped = S#state.flipped + 1}, [
        {reply, From, off}
    ]};
on({call, From}, how_many, S = #state{}) ->
    {next_state, on, S, [
        {reply, From, S#state.flipped}
    ]}.

off({call, From}, flip, S = #state{}) ->
    {next_state, on, S#state{flipped = S#state.flipped + 1}, [
        {reply, From, on}
    ]};
off({call, From}, how_many, S = #state{}) ->
    {next_state, off, S, [
        {reply, From, S#state.flipped}
    ]}.

%% Mandatory callbacks...

terminate(_Reason, _State, _Data) ->
    void.

code_change(_Version, State, Data, _Extra) ->
    {ok, State, Data}.

callback_mode() ->
    state_functions.
