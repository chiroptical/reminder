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
    flip/3
]).

-type switch_state() :: on | off.

-record(state, {
    switch_is = on :: switch_state(),
    flipped = 0 :: non_neg_integer()
}).

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

flip(Pid) ->
    gen_statem:call(Pid, flip).

how_many(Pid) ->
    gen_statem:call(Pid, how_many).

init([]) ->
    {ok, flip, #state{}}.

flip({call, From}, flip, S = #state{}) ->
    Next =
        case S#state.switch_is of
            off -> on;
            on -> off
        end,
    {next_state, flip, S#state{switch_is = Next, flipped = S#state.flipped + 1}, [
        {reply, From, Next}
    ]};
flip({call, From}, how_many, S = #state{}) ->
    {next_state, flip, S, [
        {reply, From, S#state.flipped}
    ]}.

%% Mandatory callbacks...

terminate(_Reason, _State, _Data) ->
    void.

code_change(_Version, State, Data, _Extra) ->
    {ok, State, Data}.

callback_mode() ->
    state_functions.
