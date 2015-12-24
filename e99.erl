-module(e99).

-export([last/1,
         last_but_one/1,
         kth/2,
         len/1,
         reverse/1, reverse2/1,
         is_palindrome/1,
         flatten/1,
         compress/1,
         pack/1,
         length_encode/1,
         length_encode2/1
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

%% 1.06: Find out whether a list is a palindrome.
is_palindrome(L) ->
    is_palindrome(L, reverse2(L)).

is_palindrome([], []) ->
    true;
is_palindrome([X | T1], [X | T2]) ->
    is_palindrome(T1, T2);
is_palindrome(_, _) ->
    false.

%% 1.07: Flatten a list
flatten([]) ->
    [];
flatten([X  | T]) when not is_list(X) ->
    [X | flatten(T)];
flatten([X | T]) ->
    flatten(X) ++ flatten(T).

%% 1.08: Eliminate consecutive duplicates of list elements
compress([]) ->
    [];
compress([X]) ->
    [X];
compress([X, X | T]) ->
    compress([X | T]);
compress([X, Y | T]) ->
    [X, Y | compress(T)].

%% 1.09 Pack consecutive duplicates of list elements into sublists.
pack(L) ->
    pack(L, []).

pack([], _) ->
    [];
pack([X], [X | _] = Acc) ->
    [[X | Acc]];
pack([X], _) ->
    [[X]];
pack([X, X | T], Acc) ->
    pack([X | T], [X | Acc]);
pack([X, Y | T], Acc) ->
    [[X | Acc] | pack([Y | T], [])].

%% 1.10: Run-length encoding of a list.
length_encode(L) ->
    L1 = pack(L),
    lencode(L1).

lencode([]) ->
    [];
lencode([[X | _] = T1 | T2]) ->
    [{len(T1), X} | lencode(T2)].

%% 1.11: Modified run-length encoding.
length_encode2(L) ->
    L1 = pack(L),
    lencode2(L1).

lencode2([]) ->
    [];
lencode2([[X] | T2]) ->
    [X | lencode2(T2)];
lencode2([[X | _] = T1 | T2]) ->
    [{len(T1), X} | lencode2(T2)].
