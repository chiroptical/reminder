-module(trade_statem).
-behaviour(gen_statem).

%% Public API
-export([
    start/1,
    start_link/1,
    trade/2,
    accept_trade/1,
    make_offer/2,
    retract_offer/2,
    ready/1,
    cancel/1
]).

%% gen_state callbacks
-export([
    init/1,
    callback_mode/0,
    % terminate/3,
    % code_change/4,

    idle/2,
    idle/3
    % idle_wait/2,
    % idle_wait/3,
    % negotiate/2,
    % negotiate/3,
    % wait/2,
    % ready/2,
    % ready/3
]).

%% A player's name
start(Name) ->
    gen_start:start(?MODULE, [Name], []).

start_link(Name) ->
    gen_statem:start_link(?MODULE, [Name], []).

callback_mode() ->
    state_functions.

trade(OwnPid, OtherPid) ->
    gen_statem:call(OwnPid, {negotiate, OtherPid}, 30000).

accept_trade(OwnPid) ->
    gen_statem:call(OwnPid, accept_negotiate).

make_offer(OwnPid, Item) ->
    gen_statem:cast(OwnPid, {make_offer, Item}).

retract_offer(OwnPid, Item) ->
    gen_statem:cast(OwnPid, {retract_offer, Item}).

ready(OwnPid) ->
    gen_statem:call(OwnPid, ready, infinity).

cancel(OwnPid) ->
    gen_statem:call(OwnPid, cancel).

ask_negotiate(OtherPid, OwnPid) ->
    gen_statem:cast(OtherPid, {ask_negotiate, OwnPid}).

accept_negotiate(OtherPid, OwnPid) ->
    gen_statem:cast(OtherPid, {accept_negotiate, OwnPid}).

do_offer(OtherPid, Item) ->
    gen_statem:cast(OtherPid, {do_offer, Item}).

undo_offer(OtherPid, Item) ->
    gen_statem:cast(OtherPid, {undo_offer, Item}).

are_you_ready(OtherPid) ->
    gen_statem:cast(OtherPid, are_you_ready).

not_yet(OtherPid) ->
    gen_statem:cast(OtherPid, not_yet).

am_ready(OtherPid) ->
    gen_statem:cast(OtherPid, 'ready!').

acknowledge_transfer(OtherPid) ->
    gen_statem:cast(OtherPid, acknowledge).

ask_commit(OtherPid) ->
    gen_statem:call(OtherPid, ask_commit).

do_commit(OtherPid) ->
    gen_statem:call(OtherPid, do_commit).

notify_cancel(OtherPid) ->
    gen_statem:call(OtherPid, cancel).

-record(state, {
    name = "",
    other,
    own_items = [],
    other_items = [],
    monitor,
    from
}).

init(Name) ->
    {ok, idle, #state{name = Name}}.

notice(#state{name = Name}, Str, Args) ->
    io:format("~s: " ++ Str ++ "~n", [Name | Args]).

unexpected(Msg, State) ->
    io:format(
        "~p recieved unknown event ~p while in state ~p~n",
        [self(), Msg, State]
    ).

idle({ask_negotiate, OtherPid}, S = #state{}) ->
    Ref = monitor(process, OtherPid),
    notice(S, "~p asked for a trade negotiation", [OtherPid]),
    {next_state, idle_wait, S#state{other = OtherPid, monitor = Ref}};
idle(Event, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.

idle({negotiate, OtherPid}, From, S = #state{}) ->
    ask_negotiate(OtherPid, self()),
    notice(S, "asking user ~p for a trade", [OtherPid]),
    Ref = monitor(process, OtherPid),
    {next_state, idle_wait, S#state{other = OtherPid, monitor = Ref, from = From}};
idle(Event, _From, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.
