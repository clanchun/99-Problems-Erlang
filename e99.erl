-module(e99).

-export([last/1,
         last_but_one/1,
         kth/2,
         len/1,
         reverse/1, reverse2/1
        ]).

%%% Part 1: Lists

%% 1.01: Find the last element of a list.
last([]) ->
    throw(none);
last([X]) ->
    X;
last([_ | T]) ->
    last(T).

%% 1.02: Find the last but one element of a list.
last_but_one([]) ->
    throw(none);
last_but_one([_]) ->
    throw(none);
last_but_one([X, _]) ->
    X;
last_but_one([_ | T]) ->
    last_but_one(T).

%% 1.03: Find the K'th element of a list.
kth([], _) ->
    throw(none);
kth([X | _], 1) ->
    X;
kth([_ | T], N) ->
    kth(T, N - 1).

%% 1.04: Find the number of elements of a list.
len([]) ->
    0;
len([_ | T]) ->
    1 + len(T).

%% 1.05: Reverse a list.
reverse([]) ->
    [];
reverse([X | T]) ->
    reverse(T) ++ [X].

reverse2(L) ->
    reverse2(L, []).

reverse2([], Acc) ->
    Acc;
reverse2([X | T], Acc) ->
    reverse2(T, [X | Acc]).
