-module(ppool_supervisor).
-export([
    start_link/3,
    init/1
]).
-behaviour(supervisor).

start_link(Name, Limit, MFA) ->
    supervisor:start_link(?MODULE, {Name, Limit, MFA}).

init({Name, Limit, MFA}) ->
    MaxRestart = 1,
    MaxTime = 3600,
    {ok,
        {{one_for_all, MaxRestart, MaxTime}, [
            {serv, {ppool_server, start_link, [Name, Limit, self(), MFA]}, permanent,
                % shutdown time
                5000, worker, [ppool_server]}
        ]}}.
