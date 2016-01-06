-module(e99_arith).

-import(e99_list, [length_encode/1]).

-export([is_prime/1,
         prime_factors/1,
         prime_factors2/1,
         prime_numbers/2
        ]).

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

%% 2.02 Determine the prime factors of a given positive integer. 
prime_factors(N) ->
    prime_factors(N, 1).

prime_factors(N, M) when M >= N ->
    case is_prime(M) of
        true ->
            [M];
        false ->
            []
    end;
prime_factors(N, M) ->
    case N rem M == 0 andalso is_prime(M) of
        true ->
            [M | prime_factors(N div M, M)];
        false ->
            prime_factors(N, M + 1)
    end.

%% 2.03 Determine the prime factors of a given positive integer (2).
prime_factors2(N) ->
    length_encode(prime_factors(N)).

%% 2.04 A list of prime numbers.
prime_numbers(H, H) ->
    case is_prime(H) of
        true ->
            [H];
        false ->
            []
    end;
prime_numbers(L, H) ->
    case is_prime(L) of
        true ->
            [L | prime_numbers(L + 1, H)];
        false ->
            prime_numbers(L + 1, H)
    end.
