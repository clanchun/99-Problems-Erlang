-module(e99_arith).

-export([is_prime/1]).

%% 2.01 Determine whether a given integer number is prime.
is_prime(N) when N =< 1 ->
    false;
is_prime(N) ->
    is_prime(N, N div 2).

is_prime(_, 1) ->
    true;
is_prime(N, M) ->
    if
        N rem M == 0 ->
            false;
        true ->
            is_prime(N, M - 1)
    end.
