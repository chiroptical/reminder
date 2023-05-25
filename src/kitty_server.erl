-module(kitty_server).
-export([
    start_link/0,
    order_cat/4,
    return_cat/2,
    close_shop/1
]).

-type color() :: red | orange | yellow | green | blue | indigo | violet.

-record(cat, {
    name :: string(),
    color = green :: color(),
    description :: string()
}).
-type cat() :: #cat{}.

%% Client API
start_link() ->
    spawn_link(fun init/0).

%% Synchronous call
-spec order_cat(pid(), string(), color(), string()) -> any().
order_cat(Pid, Name, Color, Description) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, {order, Name, Color, Description}},
    receive
        {Ref, Cat} ->
            erlang:demonitor(Ref, [flush]),
            Cat;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.

%% Asynchronous call
-spec return_cat(pid(), cat()) -> any().
return_cat(Pid, Cat = #cat{}) ->
    Pid ! {return, Cat},
    ok.

-spec close_shop(pid()) -> any().
close_shop(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, terminate},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.

init() ->
    loop(queue:new()).

-spec loop(queue:queue(cat())) -> any().
loop(Cats) ->
    receive
        {Pid, Ref, {order, Name, Color, Description}} ->
            %% Try to find the oldest cat in the shelter
            case queue:out(Cats) of
                %% No cats in the shelter, find the cat they want
                {empty, _} ->
                    Pid ! {Ref, make_cat(Name, Color, Description)},
                    loop(Cats);
                %% Cats in the shelter, they adopt the one present in the
                %% shelter longest.
                {{value, Cat}, Next} ->
                    Pid ! {Ref, Cat},
                    loop(Next)
            end;
        {return, Cat = #cat{}} ->
            loop(queue:in(Cat, Cats));
        {Pid, Ref, terminate} ->
            Pid ! {Ref, ok},
            terminate(Cats);
        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(Cats)
    end.

-spec make_cat(string(), color(), string()) -> cat().
make_cat(Name, Color, Description) ->
    #cat{name = Name, color = Color, description = Description}.

-spec terminate(queue:queue(cat())) -> any().
terminate(Cats) ->
    [io:format("~p was set free!~n", [C#cat.name]) || C <- queue:to_list(Cats)],
    ok.
