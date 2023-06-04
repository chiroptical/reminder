-module(musicians).
-behaviour(gen_server).

-export([
    start_link/2,
    stop/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-type role() :: guitar | drummer | bass | keyboard.
-type skill() :: good | bad.

-record(state, {
    name = "" :: string(),
    role :: role(),
    skill :: skill()
}).

-define(DELAY, 750).

-spec start_link(role(), skill()) -> any().
start_link(Role, Skill) ->
    gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).

-spec stop(role()) -> any().
stop(Role) ->
    gen_server:call(Role, stop).

init([Role, Skill]) ->
    %% know when the parent shuts down
    process_flag(trap_exit, true),
    TimeToPlay = rand:uniform(3000),
    Name = pick_name(),
    io:format(
        "Musician ~s, playing the ~s entered the room~n",
        [Name, Role]
    ),
    {ok, #state{name = Name, role = Role, skill = Skill}, TimeToPlay}.

pick_name() ->
    lists:nth(rand:uniform(10), first_names()) ++
        " " ++
        lists:nth(rand:uniform(10), last_names()).

first_names() ->
    [
        "Valerie",
        "Arnold",
        "Carlos",
        "Dorothy",
        "Keesha",
        "Phoebe",
        "Ralphie",
        "Tim",
        "Wanda",
        "Janet"
    ].

last_names() ->
    [
        "Frizzle",
        "Perlstein",
        "Ramon",
        "Ann",
        "Franklin",
        "Terese",
        "Tennelli",
        "Jamal",
        "Li",
        "Perlstein"
    ].

handle_call(stop, _From, S = #state{}) ->
    {stop, normal, ok, S};
handle_call(_Message, _From, S) ->
    {noreply, S, ?DELAY}.

handle_cast(_Message, S) ->
    {noreply, S, ?DELAY}.

handle_info(timeout, S = #state{name = Name, skill = good}) ->
    io:format("~s produced sound!~n", [Name]),
    {noreply, S, ?DELAY};
handle_info(timeout, S = #state{name = Name, skill = bad}) ->
    case rand:uniform(5) of
        1 ->
            io:format("~s played a false note. Ooof.~n", [Name]),
            {stop, bad_note, S};
        _ ->
            io:format("~s produced sound!~n", [Name]),
            {noreply, S, ?DELAY}
    end;
handle_info(_Message, S) ->
    {noreply, S, ?DELAY}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(normal, S) ->
    io:format("~s left the room (~s)~n", [S#state.name, S#state.role]);
terminate(bad_note, S) ->
    io:format("~s is trash at ~s, get them out~n", [S#state.name, S#state.role]);
terminate(shutdown, _S) ->
    io:format("The manager is pissed, you are all out~n");
terminate(_Reason, S) ->
    io:format("~s left the room (~s)~n", [S#state.name, S#state.role]).
