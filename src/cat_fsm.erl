-module(cat_fsm).
-export([
    start/0,
    event/2
]).

start() ->
    spawn(fun() -> dont_care() end).

dont_care() ->
    receive
        {Pid, Ref, _Msg} ->
            Pid ! {Ref, meh};
        _ ->
            ok
    end,
    io:format("Switching to 'dont_care' state~n"),
    dont_care().

event(Pid, Event) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, Event},
    receive
        {Ref, Msg} -> {ok, Msg}
    after 5000 ->
        {error, timeout}
    end.
