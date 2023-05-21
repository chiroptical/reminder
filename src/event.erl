-module(event).
-export([
    loop/1,
    start/2,
    start_link/2,
    init/3,
    cancel/1
]).
-record(state, {
    server,
    name = "",
    to_go = 0
}).

start(EventName, Delay) ->
    spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
    spawn_link(?MODULE, init, [self(), EventName, Delay]).

init(Server, EventName, DateTime) ->
    loop(#state{
        server = Server,
        name = EventName,
        to_go = time_to_go(DateTime)
    }).

% Erlang has a timeout limit of about 49 days, so we can split
% long reminders into sections of delays each 49 days long.
% N - seconds
normalize(N) ->
    % 49 days in seconds
    Limit = 49 * 24 * 60 * 60,
    [N rem Limit | lists:duplicate(N div Limit, Limit)].

loop(S = #state{server = Server, to_go = [T | Next]}) ->
    receive
        {_, Ref, cancel} ->
            Server ! {Ref, ok}
    after T * 1000 ->
        case Next of
            [] -> Server ! {done, S#state.name};
            _ -> loop(S#state{to_go = Next})
        end
    end.

cancel(Pid) ->
    % Monitor in case the process is already dead
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, cancel},
    receive
        {_Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', _Ref, process, _Pid, _Reason} ->
            ok
    end.

%% Erlang's DateTime {{Year, Month, Day}, {Hour, Minute, Second}}
time_to_go(TimeOut = {{_, _, _}, {_, _, _}}) ->
    Now = calendar:local_time(),
    ToGo =
        calendar:datetime_to_gregorian_seconds(TimeOut) -
            calendar:datetime_to_gregorian_seconds(Now),
    Secs =
        if
            ToGo > 0 -> ToGo;
            ToGo =< 0 -> 0
        end,
    normalize(Secs).
