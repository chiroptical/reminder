%% Changing the example a bit
%%
%% This is an adoption center
%%
%% You can:
%% - drop off a new cat
%% - adopt a cat
%% - return a cat
-module(kitty).
-behaviour(gen_server).
-export([
    start_link/0,
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    terminate/2,
    code_change/3,

    adopt_cat/4,
    drop_off/2,
    start_sanctuary/1
]).

-include("kitty.hrl").

-spec cat(string(), color(), string()) -> cat().
cat(Name, Color, Description) ->
    #cat{name = Name, color = Color, description = Description}.

-spec adopt_cat(pid(), string(), color(), string()) -> any().
adopt_cat(Pid, Name, Color, Description) ->
    gen_server:call(Pid, {adopt, Name, Color, Description}).

-spec drop_off(pid(), cat()) -> any().
drop_off(Pid, Cat = #cat{}) ->
    gen_server:cast(Pid, {drop_off, Cat}).

-spec start_sanctuary(pid()) -> any().
start_sanctuary(Pid) ->
    gen_server:call(Pid, sanctuary).

%% Arguments to gen_server:start_link
%% 1. the callback module
%% 2. parameter to init/1, i.e. an empty FIFO queue
%% 3. debugging options
-spec start_link() -> {atom(), pid()}.
start_link() ->
    gen_server:start_link(?MODULE, queue:new(), []).

init(Queue) ->
    {ok, Queue}.

handle_call({adopt, Name, Color, Description}, _From, Cats) ->
    case queue:out(Cats) of
        %% No cats in the shelter, find the cat they want
        {empty, _} ->
            {reply, cat(Name, Color, Description), Cats};
        %% Cats in the shelter, they adopt the one present in the
        %% shelter longest.
        {{value, Cat}, Next} ->
            {reply, Cat, Next}
    end;
handle_call(sanctuary, _From, Cats) ->
    {stop, normal, ok, Cats}.

handle_cast({drop_off, Cat = #cat{}}, Cats) ->
    {noreply, queue:in(Cat, Cats)}.

handle_info(Msg, Cats) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, Cats}.

terminate(normal, Cats) ->
    [io:format("~p was set free!~n", [C#cat.name]) || C <- queue:to_list(Cats)],
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
