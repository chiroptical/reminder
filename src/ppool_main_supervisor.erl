-module(ppool_main_supervisor).
-behavior(supervisor).

-export([
    start_link/0,
    stop/0,
    start_pool/3,
    stop_pool/1,
    init/1
]).

%% Top level supervisor to hold pools in memory and supervise them

start_link() ->
    supervisor:start_link({local, ppool}, ?MODULE, []).

stop() ->
    case whereis(ppool) of
        P when is_pid(P) ->
            exit(P, kill);
        _ ->
            ok
    end.

init([]) ->
    MaxRestart = 6,
    MaxTime = 3600,
    {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.

start_pool(Name, Limit, MFA) ->
    MaxShutdown = 10500,
    ChildSpec =
        {Name, {ppool_sup, start_link, [Name, Limit, MFA]}, permanent, MaxShutdown, supervisor, [
            ppool_sup
        ]},
    supervisor:start_child(ppool, ChildSpec).

stop_pool(Name) ->
    supervisor:terminate_child(ppool, Name),
    supervisor:delete_child(ppool, Name).
