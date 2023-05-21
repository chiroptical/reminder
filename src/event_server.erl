-module(event_server).
-export([
    init/0,
    valid_time/1,
    valid_datetime/1
]).

% list of events and clients
-record(state, {events, clients}).

-record(event, {
    name = "",
    description = "",
    pid,
    timeout = {{1970, 1, 1}, {0, 0, 0}}
}).

init() ->
    loop(#state{events = orddict:new(), clients = orddict:new()}).

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
            io:format("Got add~n");
        {Pid, MsgRef, {cancel, Name}} ->
            io:format("Got cancel~n");
        {done, Name} ->
            io:format("Got done~n");
        shutdown ->
            io:format("Got shutdown~n");
        {'DOWN', Ref, process, _Pid, _Reason} ->
            io:format("Got DOWN~n");
        code_change ->
            io:format("Got code_change~n");
        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown])
    end.

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
