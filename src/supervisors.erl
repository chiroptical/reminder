-module(supervisors).
-export([start/2, start_link/2, init/1, loop/1]).

%% Given a module and some initialization arguments, spawn a process which
%% supervises the module and arguments
start(Mod, Args) ->
    spawn(?MODULE, init, [{Mod, Args}]).

%% Same as above but provides us the Pid
start_link(Mod, Args) ->
    spawn_link(?MODULE, init, [{Mod, Args}]).

%% Start our process with `start_link` and arguments.
%% i.e. supervise the process
init({Mod, Args}) ->
    % what does this do?
    process_flag(trap_exit, true),
    loop({Mod, start_link, Args}).

%% Start the link, handles `EXIT` messages and either shutdown or restart on
%% these failures.
loop({Mod, From, Args}) ->
    %% I guess apply with literally does like mod:from(...args)
    Pid = apply(Mod, From, Args),
    receive
        {'EXIT', _From, shutdown} ->
            exit(shutdown);
        {'EXIT', Pid, Reason} ->
            io:format("Process ~p exited for reason ~p~n", [Pid, Reason]),
            loop({Mod, From, Args})
    end.
