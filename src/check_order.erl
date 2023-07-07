%% This is a random module I generated to check the behavior of state when you
%% have an async call within a sync one. Essentially, trying to answer this
%% question I posted on Twitter:
%% https://twitter.com/chiroptical/status/1672544356417372160
-module(check_order).
-behaviour(gen_server).
-export([
    start_link/0,
    thing/1,

    init/1,
    handle_call/3,
    handle_cast/2
]).

-record(state, {text :: string()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

thing(Pid) ->
    gen_server:call(Pid, thing).

async_thing(Pid) ->
    gen_server:cast(Pid, async_thing).

init(_Args) ->
    {ok, #state{text = "init"}}.

handle_call(thing, _From, _State = #state{text = CurrentText}) ->
    io:format("Starting async thing...~n"),
    async_thing(self()),
    io:format("Sleep 1 second...~n"),
    ok = timer:sleep(1000),
    {reply, {thing, CurrentText}, #state{text = "async_thing"}}.

handle_cast(async_thing, State = #state{text = CurrentText}) ->
    io:format("Async thing says ~p...~n", [CurrentText]),
    {noreply, State}.
