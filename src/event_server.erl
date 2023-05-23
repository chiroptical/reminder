-module(event_server).
-export([
    init/0,
    subscribe/1,
    add_event/3,
    cancel/1,
    listen/1,
    start/0,
    start_link/0,
    terminate/0,
    add_minute/1
]).

% list of events and clients
-record(state, {events, clients}).

-record(event, {
    name = "",
    description = "",
    pid,
    timeout = {{1970, 1, 1}, {0, 0, 0}}
}).

add_minute({Date, {H, M, S}}) -> {Date, {H, M + 1, S}}.

subscribe(Pid) ->
    Ref = erlang:monitor(process, whereis(?MODULE)),
    ?MODULE ! {self(), Ref, {subscribe, Pid}},
    receive
        {Ref, ok} ->
            {ok, Ref};
        {'DOWN', Ref, process, _Pid, Reason} ->
            {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

add_event(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
        {Ref, Msg} -> Msg
    after 5000 ->
        {error, timeout}
    end.

cancel(Name) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {cancel, Name}},
    receive
        {Ref, ok} ->
            {ok, Ref}
    after 5000 ->
        {error, timeout}
    end.

listen(Delay) ->
    receive
        M = {done, _Name, _Description} ->
            [M | listen(0)]
    after Delay * 1000 ->
        []
    end.

init() ->
    loop(#state{events = orddict:new(), clients = orddict:new()}).

start() ->
    register(?MODULE, Pid = spawn(?MODULE, init, [])),
    Pid.

start_link() ->
    register(?MODULE, Pid = spawn_link(?MODULE, init, [])),
    Pid.

terminate() ->
    ?MODULE ! shutdown.

loop(S = #state{}) ->
    receive
        {Pid, MsgRef, {subscribe, Client}} ->
            % create a monitor for the client subscriber
            Ref = erlang:monitor(process, Client),
            % store the client subscriber under the monitor reference
            NewClients = orddict:store(Ref, Client, S#state.clients),
            % signal that the subscription worked
            Pid ! {MsgRef, ok},
            % continue with our new client
            loop(S#state{clients = NewClients});
        {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
            case valid_datetime(TimeOut) of
                true ->
                    EventPid = event:start_link(Name, TimeOut),
                    NewEvents = orddict:store(
                        Name,
                        #event{
                            name = Name,
                            description = Description,
                            pid = EventPid,
                            timeout = TimeOut
                        },
                        S#state.events
                    ),
                    Pid ! {MsgRef, ok},
                    loop(S#state{events = NewEvents});
                false ->
                    Pid ! {MsgRef, {error, bad_timeout}},
                    loop(S)
            end;
        {Pid, MsgRef, {cancel, Name}} ->
            Events =
                case orddict:find(Name, S#state.events) of
                    {ok, Ev} ->
                        event:cancel(Ev#event.pid),
                        orddict:erase(Name, S#state.events);
                    error ->
                        S#state.events
                end,
            Pid ! {MsgRef, ok},
            loop(S#state{events = Events});
        {done, Name} ->
            case orddict:find(Name, S#state.events) of
                {ok, Event} ->
                    send_to_clients(
                        {done, Event#event.name, Event#event.description},
                        S#state.clients
                    ),
                    NewEvents = orddict:erase(Name, S#state.events),
                    loop(S#state{events = NewEvents});
                error ->
                    loop(S)
            end;
        shutdown ->
            exit(shutdown);
        %% Means the client shutdown, we can remove it
        {'DOWN', Ref, process, _Pid, _Reason} ->
            loop(S#state{clients = orddict:erase(Ref, S#state.clients)});
        code_change ->
            ?MODULE:loop(S);
        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(S)
    end.

send_to_clients(Msg, ClientDict) ->
    orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).

%% andalso short-circuits, orelse also exists
valid_datetime({Date, Time}) ->
    try
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
        error:function_clause -> false
    end.

valid_time({H, M, S}) ->
    (H >= 0) andalso (H =< 24) andalso valid_minute_or_second(M) andalso valid_minute_or_second(S);
valid_time(_) ->
    false.

valid_minute_or_second(X) ->
    (X >= 0) andalso (X =< 60).
