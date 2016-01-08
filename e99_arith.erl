-module(e99_arith).

-import(e99_list, [length_encode/1]).

-export([is_prime/1,
         prime_factors/1,
         prime_factors2/1,
         prime_numbers/2,
         goldbach/1,
         goldbach_list/2,
         gcd/2,
         coprime/2,
         phi/1
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

%% 2.05 Goldbach's conjecture.
goldbach(N) ->
    goldbach(1, N).

goldbach(M, N) when M > N div 2 ->
    [];
goldbach(M, N) ->
    case is_prime(M) andalso is_prime(N - M) of
        true ->
            [M, N - M];
        false ->
            goldbach(M + 1, N)
    end.

%% 2.06 A list of Goldbach compositions.
goldbach_list(H, H) ->
    case goldbach(H) of 
        [] ->
            [];
        [A, B] ->
            [[H, A, B]]
    end;
goldbach_list(L, H) ->
    case goldbach(L) of
        [] ->
            goldbach_list(L + 1, H);
        [A, B] ->
            [[L, A, B] | goldbach_list(L + 1, H)]
    end.

%% 2.07 Determine the greatest common divisor of two positive integer numbers. 
gcd(H, L) when H < L ->
    gcd(L, H);
gcd(H, L) when H rem L == 0 ->
    L;
gcd(H, L) ->
    gcd(L, H - L).
    
%% 2.08 Determine whether two positive integer numbers are coprime.
coprime(A, B) ->
    gcd(A, B) == 1.

%% 2.09 Calculate Euler's totient function phi(m).
phi(1) ->
    1;
phi(N) ->
    phi(1, N).

phi(N, N) ->
    0;
phi(L, N) ->
    case coprime(L, N) of
        true ->
            1 + phi(L + 1, N);
        false ->
            phi(L + 1, N)
    end.
