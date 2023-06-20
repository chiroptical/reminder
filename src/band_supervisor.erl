-module(band_supervisor).
-behaviour(supervisor).

-export([
    start_link/1,
    init/1
]).

start_link(Kind) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Kind).

-type kind() :: lenient | angry | jerk | jamband.

-spec init(kind()) -> any().
init(lenient) ->
    init({one_for_one, 3, 60});
init(angry) ->
    init({one_for_one, 2, 60});
init(jerk) ->
    init({one_for_one, 1, 60});
init(jamband) ->
    {ok,
        {{simple_one_for_one, 3, 60}, [
            {jam_musician, {musicians, start_link, []}, temporary, 1000, worker, [musicians]}
        ]}};
init({RestartStrategy, MaxRestart, MaxTime}) ->
    {ok,
        {{RestartStrategy, MaxRestart, MaxTime}, [
            {guitar, {musicians, start_link, [singer, good]}, permanent, 1000, worker, [musicians]},
            {bass, {musicians, start_link, [bass, good]}, temporary, 1000, worker, [musicians]},
            {drummer, {musicians, start_link, [drummer, bad]}, transient, 1000, worker, [musicians]},
            {keyboard, {musicians, start_link, [keyboard, good]}, transient, 1000, worker, [
                musicians
            ]}
        ]}}.
