-module(dog_fsm).
-export([
    start/0,
    squirrel/1,
    pet/1
]).

squirrel(Pid) ->
    Pid ! squirrel.

pet(Pid) ->
    Pid ! pet.

start() ->
    spawn(fun() -> bark() end).

bark() ->
    io:format("Dog says: bark!~n"),
    receive
        pet ->
            wag_tail();
        _ ->
            io:format("Dog is confused~n"),
            bark()
    after 2000 ->
        bark()
    end.

wag_tail() ->
    io:format("Dog wags it's tail~n"),
    receive
        pet ->
            sit();
        _ ->
            io:format("Dog is confused~n"),
            wag_tail()
    after 3000 ->
        bark()
    end.

sit() ->
    io:format("Dog is sitting, good dog!~n"),
    receive
        squirrel ->
            bark();
        _ ->
            io:format("Dog is confused~n"),
            sit()
    end.
