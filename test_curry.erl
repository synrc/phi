-module(test_curry).
-export([run/0]).

run() ->
    F = fun(X, Y) -> X + Y end,
    A = element(2, erlang:fun_info(F, arity)),
    io:format("Arity: ~p~n", [A]).
