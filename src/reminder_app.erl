%%%-------------------------------------------------------------------
%% @doc reminder public API
%% @end
%%%-------------------------------------------------------------------

-module(reminder_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    reminder_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
