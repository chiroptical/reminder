%% To define a custom behaviour you'll define a `behaviour_info` with the
%% requisite functions and their arity.
-module(custom_behaviour).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init, 1},
     {some_fun, 0},
     {other, 3}
    ];
behaviour_info(_) -> undefined.
